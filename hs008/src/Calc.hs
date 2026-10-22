{-# LANGUAGE FlexibleContexts #-}

module Calc (
  CalcError (..),
  CalcM,
  push,
  pop,
  safeDiv,
  evalToken,
  evalRPN,
  runCalcM,
  calc,
) where

import Control.Monad.Except
import Control.Monad.State
import Text.Read (readMaybe)

-- 計算中に起こりうるエラー
data CalcError
  = DivByZero -- 0 で割った
  | StackUnderflow -- 値が足りないのに pop した
  | UnknownToken String -- 数値でも演算子でもないトークン
  | InvalidResult [Int] -- 最後にスタックに値がちょうど1つ残らなかった
  deriving (Show, Eq)

-- ExceptT でエラーを、その内側の State でスタックを扱う。
-- ExceptT e (State s) の順に重ねると、エラーで中断しても
-- その時点までのスタック（状態）は失われずに残る。
type CalcM = ExceptT CalcError (State [Int])

push :: MonadState [Int] m => Int -> m ()
push x = modify (x :)

-- 空スタックからの pop は Maybe ではなく throwError で失敗させる
pop :: (MonadState [Int] m, MonadError CalcError m) => m Int
pop = do
  s <- get
  case s of
    [] -> throwError StackUnderflow
    (x : xs) -> put xs >> pure x

safeDiv :: MonadError CalcError m => Int -> Int -> m Int
safeDiv _ 0 = throwError DivByZero
safeDiv x y = pure (x `div` y)

-- 逆ポーランド記法のトークンを1つ処理する
evalToken :: (MonadState [Int] m, MonadError CalcError m) => String -> m ()
evalToken "+" = binOp (\x y -> pure (x + y))
evalToken "-" = binOp (\x y -> pure (x - y))
evalToken "*" = binOp (\x y -> pure (x * y))
evalToken "/" = binOp safeDiv
evalToken tok = case readMaybe tok of
  Just n -> push n
  Nothing -> throwError (UnknownToken tok)

binOp :: (MonadState [Int] m, MonadError CalcError m) => (Int -> Int -> m Int) -> m ()
binOp f = do
  y <- pop
  x <- pop
  r <- f x y
  push r

-- "3 4 + 2 *" のような式を評価する
evalRPN :: (MonadState [Int] m, MonadError CalcError m) => String -> m Int
evalRPN src = do
  mapM_ evalToken (words src)
  s <- get
  case s of
    [x] -> pure x
    _ -> throwError (InvalidResult s)

-- 結果（成功 or エラー）と最終スタックの両方を返す
runCalcM :: CalcM a -> (Either CalcError a, [Int])
runCalcM m = runState (runExceptT m) []

-- 結果だけ欲しいとき
calc :: String -> Either CalcError Int
calc = fst . runCalcM . evalRPN
