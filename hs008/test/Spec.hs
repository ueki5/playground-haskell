module Main where

import Calc
import Control.Monad (ap)
import Control.Monad.Except
import Control.Monad.State
import Data.Functor.Identity
import Test.Hspec
import Util

main :: IO ()
main = hspec $ do
  describe "calc（正常系）" $ do
    it "足し算と掛け算" $
      calc "3 4 + 2 *" `shouldBe` Right 14
    it "引き算と割り算はオペランドの順序を守る" $
      calc "10 3 - 2 /" `shouldBe` Right 3

  describe "calc（エラー系）" $ do
    it "0 で割ると DivByZero" $
      calc "1 0 /" `shouldBe` Left DivByZero
    it "値が足りないと StackUnderflow" $
      calc "1 +" `shouldBe` Left StackUnderflow
    it "不明なトークンは UnknownToken" $
      calc "1 2 foo" `shouldBe` Left (UnknownToken "foo")
    it "値が残りすぎると InvalidResult" $
      calc "1 2" `shouldBe` Left (InvalidResult [2, 1])
    it "空の式は InvalidResult []" $
      calc "" `shouldBe` Left (InvalidResult [])

  describe "ExceptT e (State s) の性質" $ do
    it "最初のエラーで処理が打ち切られる" $
      calc "1 0 / foo" `shouldBe` Left DivByZero
    it "エラーで中断してもそれまでのスタックは残る" $
      runCalcM (evalRPN "5 1 0 /") `shouldBe` (Left DivByZero, [5])

  describe "catchError" $ do
    it "エラーを捕まえて別の値で続行できる" $
      runCalcM (safeDiv 1 0 `catchError` \_ -> pure 0)
        `shouldBe` (Right 0, [])
    it "捕まえたエラーの種類で分岐できる" $
      fst (runCalcM (pop `catchError` \e -> pure (if e == StackUnderflow then -1 else -2)))
        `shouldBe` Right (-1)

  describe "ExceptT' の fmap" $ do
    it "Right の中身に関数を適用する" $
      runIdentity (runExceptT' (fmap (+ 1) (ExceptT' (Identity (Right 1)) :: ExceptT' String Identity Int)))
        `shouldBe` Right 2
    it "Left はそのまま（関数は適用されない）" $
      runIdentity (runExceptT' (fmap (+ 1) (ExceptT' (Identity (Left "err")) :: ExceptT' String Identity Int)))
        `shouldBe` Left "err"
    it "内側のモナドの効果はそのまま残る（Left でも消えない）" $
      runState (runExceptT' (fmap (+ 1) (ExceptT' (modify (+ 10) >> pure (Left "err")) :: ExceptT' String (State Int) Int))) 0
        `shouldBe` (Left "err", 10)
    it "恒等則：fmap id = id" $
      let m = ExceptT' (Identity (Right 1)) :: ExceptT' String Identity Int
       in runIdentity (runExceptT' (fmap id m)) `shouldBe` runIdentity (runExceptT' m)
    it "合成則：fmap (f . g) = fmap f . fmap g" $
      let m = ExceptT' (Identity (Right 3)) :: ExceptT' String Identity Int
          f = (* 2)
          g = (+ 1)
       in runIdentity (runExceptT' (fmap (f . g) m))
            `shouldBe` runIdentity (runExceptT' ((fmap f . fmap g) m))

  describe "ExceptT' の Applicative" $ do
    it "pure は Right に包む" $
      runIdentity (runExceptT' (pure 1 :: ExceptT' String Identity Int))
        `shouldBe` Right 1
    it "両方 Right なら関数を適用する" $
      runIdentity (runExceptT' (ExceptT' (Identity (Right (+ 1))) <*> ExceptT' (Identity (Right 1)) :: ExceptT' String Identity Int))
        `shouldBe` Right 2
    it "左が Left なら Left（右側の Left より優先される）" $
      runIdentity (runExceptT' (ExceptT' (Identity (Left "left")) <*> ExceptT' (Identity (Left "right")) :: ExceptT' String Identity Int))
        `shouldBe` Left "left"
    it "右が Left なら Left" $
      runIdentity (runExceptT' (ExceptT' (Identity (Right (+ 1))) <*> ExceptT' (Identity (Left "right")) :: ExceptT' String Identity Int))
        `shouldBe` Left "right"
    it "左が Left なら右側の効果は実行されない（短絡）" $
      runState (runExceptT' (ExceptT' (modify (+ 1) >> pure (Left "err")) <*> ExceptT' (modify (+ 10) >> pure (Right 1)) :: ExceptT' String (State Int) Int)) 0
        `shouldBe` (Left "err", 1)
    it "効果は左から右の順に実行される" $
      runState (runExceptT' (ExceptT' (modify (++ "L") >> pure (Right (+ 1))) <*> ExceptT' (modify (++ "R") >> pure (Right 1)) :: ExceptT' String (State String) Int)) ""
        `shouldBe` (Right 2, "LR")
    it "恒等則：pure id <*> v = v" $
      let v = ExceptT' (Identity (Right 1)) :: ExceptT' String Identity Int
       in runIdentity (runExceptT' (pure id <*> v)) `shouldBe` runIdentity (runExceptT' v)
    it "準同型則：pure f <*> pure x = pure (f x)" $
      runIdentity (runExceptT' (pure (+ 1) <*> pure 1 :: ExceptT' String Identity Int))
        `shouldBe` runIdentity (runExceptT' (pure (1 + 1) :: ExceptT' String Identity Int))
    it "(<*>) = ap（Monad と同じ結果になる）" $
      let mf = ExceptT' (modify (+ 1) >> pure (Right (* 2))) :: ExceptT' String (State Int) (Int -> Int)
          mx = ExceptT' (modify (* 10) >> pure (Right 3)) :: ExceptT' String (State Int) Int
       in runState (runExceptT' (mf <*> mx)) 1 `shouldBe` runState (runExceptT' (mf `ap` mx)) 1

  describe "ExceptT' の Monad" $ do
    it "Right の値を次の処理に渡す" $
      runIdentity (runExceptT' (ExceptT' (Identity (Right 1)) >>= \x -> pure (x + 1) :: ExceptT' String Identity Int))
        `shouldBe` Right 2
    it "Left なら後続の関数は呼ばれず Left のまま" $
      runIdentity (runExceptT' (ExceptT' (Identity (Left "err")) >>= \x -> pure (x + 1) :: ExceptT' String Identity Int))
        `shouldBe` Left "err"
    it "Left なら後続の効果は実行されない（短絡）" $
      runState (runExceptT' (ExceptT' (modify (+ 1) >> pure (Left "err")) >>= \x -> ExceptT' (modify (+ 10) >> pure (Right x)) :: ExceptT' String (State Int) Int)) 0
        `shouldBe` (Left "err", 1)
    it "前の結果を見て次の処理を選べる（Applicative ではできない）" $
      let check x = if x > 0 then pure x else throwE' "not positive"
          m n = (pure n :: ExceptT' String Identity Int) >>= check
       in (runIdentity (runExceptT' (m 1)), runIdentity (runExceptT' (m 0)))
            `shouldBe` (Right 1, Left "not positive")
    it "do 記法で途中の Left 以降は実行されない" $
      runState
        ( runExceptT' $ do
            x <- ExceptT' (modify (++ "1") >> pure (Right 1)) :: ExceptT' String (State String) Int
            _ <- ExceptT' (modify (++ "2") >> pure (Left "stop")) :: ExceptT' String (State String) Int
            ExceptT' (modify (++ "3") >> pure (Right x))
        )
        ""
        `shouldBe` (Left "stop", "12")
    it "左単位則：pure a >>= k = k a" $
      let k x = if x > 0 then pure (x * 2) else throwE' "neg" :: ExceptT' String Identity Int
       in runIdentity (runExceptT' (pure 3 >>= k)) `shouldBe` runIdentity (runExceptT' (k 3))
    it "右単位則：m >>= pure = m" $
      let m = ExceptT' (modify (+ 1) >> pure (Right 1)) :: ExceptT' String (State Int) Int
       in runState (runExceptT' (m >>= pure)) 0 `shouldBe` runState (runExceptT' m) 0
    it "結合則：(m >>= f) >>= g = m >>= (\\x -> f x >>= g)" $
      let m = ExceptT' (modify (+ 1) >> pure (Right 1)) :: ExceptT' String (State Int) Int
          f x = ExceptT' (modify (* 10) >> pure (Right (x + 1)))
          g x = ExceptT' (modify (+ 5) >> pure (if x > 1 then Left "big" else Right x))
       in runState (runExceptT' ((m >>= f) >>= g)) 0
            `shouldBe` runState (runExceptT' (m >>= (\x -> f x >>= g))) 0
