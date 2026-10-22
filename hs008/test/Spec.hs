module Main where

import Calc
import Control.Monad.Except
import Test.Hspec

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
