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

main :: IO ()
main = do
  r1 <- run1 3
  r2 <- runExceptT (run2 0)
  r3 <- runExceptT' (run3 0)
  print (r2, r3)
  print $ case (r1, r2) of
    (Right n1, Right n2) -> Right (n1 + n2)
    (Right _, Left e2) -> Left e2
    (Left e1, Right _) -> Left e1
    (Left e1, Left e2) -> Left (e1 ++ "," ++ e2)
