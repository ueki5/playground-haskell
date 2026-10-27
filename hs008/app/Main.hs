{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad (when)
import Control.Monad.Except
import Util

step1 :: Int -> IO (Either String Int)
step1 x
  | x >= 0 = pure (Right (x - 1))
  | otherwise = pure (Left "minus value!")

step2 :: Int -> IO (Either String Int)
step2 x
  | x >= 0 = pure (Right (x - 2))
  | otherwise = pure (Left "minus value!")

-- ExceptT なし：段数が増えるほど case が深くなる
run1 :: Int -> IO (Either String Int)
run1 i = do
  r1 <- step1 i
  case r1 of
    Left e -> pure (Left e)
    Right x -> do
      r2 <- step2 x
      case r2 of
        Left e -> pure (Left e)
        Right y -> pure (Right (x + y))

-- ExceptT あり：段数が増えても１行追加するだけ
run2 :: Int -> ExceptT String IO Int
run2 i = do
  x <- ExceptT (step1 i)
  y <- ExceptT (step2 x)
  pure (x + y)

-- 自作の ExceptT'：run2 とコンストラクタ名が違うだけ
run3 :: Int -> ExceptT' String IO Int
run3 i = do
  x <- ExceptT' (step1 i)
  y <- ExceptT' (step2 x)
  pure (x + y)

-- mtl スタイル：具体的な型ではなく「エラーを投げられること」だけを要求する
step1' :: MonadError String m => Int -> m Int
step1' x
  | x >= 0 = pure (x - 1)
  | otherwise = throwError "minus value!"

step2' :: MonadError String m => Int -> m Int
step2' x
  | x >= 0 = pure (x - 2)
  | otherwise = throwError "minus value!"

-- ExceptT で包む必要がない。どのモナドで動かすかは呼び出し側が決める
run4 :: MonadError String m => Int -> m Int
run4 i = do
  x <- step1' i
  y <- step2' x
  pure (x + y)

main :: IO ()
main = do
  r1 <- run1 3
  r2 <- runExceptT (run2 0)
  r3 <- runExceptT' (run3 0)
  print (r2, r3)
  -- 同じ run4 を、型注釈だけで別のモナドとして実行する
  print (run4 0 :: Either String Int)
  r4 <- runExceptT (run4 0 :: ExceptT String IO Int)
  r4' <- runExceptT' (run4 0 :: ExceptT' String IO Int)
  print (r4, r4')
  print $ case (r1, r2) of
    (Right n1, Right n2) -> Right (n1 + n2)
    (Right _, Left e2) -> Left e2
    (Left e1, Right _) -> Left e1
    (Left e1, Left e2) -> Left (e1 ++ "," ++ e2)
