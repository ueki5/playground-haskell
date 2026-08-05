module Main where

import Control.Monad.State
import Stack

program :: StackM (Maybe Int, Maybe Int)
program = do
  push 1
  a <- pop
  b <- pop
  pure (a, b)

-- StateT [Int] IO の例。push/pop は Stack.hs で
-- MonadState [Int] m => m a と汎用化してあるので、
-- 純粋な State のときと同じ関数がそのまま使える。
-- lift は IO のような内側のモナドのアクションを
-- StateT のブロックに持ち上げるために使う。
program2 :: StateT [Int] IO (Maybe Int, Maybe Int)
program2 = do
  push 1
  lift $ putStrLn "pushed 1"
  push 2
  lift $ putStrLn "pushed 2"
  push 3
  lift $ putStrLn "pushed 3"
  a <- pop
  lift $ putStrLn ("popped: " ++ show a)
  b <- pop
  lift $ putStrLn ("popped: " ++ show b)
  pure (a, b)

-- program = do
--   push 1
--   push 2
--   push 3
--   a <- pop
--   b <- pop
--   push 100
--   pure (a, b)

-- program =
--   push 1
--     >> push 2
--     >> push 3
--     >> pop
--     >>= \a ->
--       pop >>= \b ->
--         push 100
--           >> pure (a, b)

main :: IO ()
main = do
  let (result, finalStack) = runStackM program
  putStrLn ("Result: " ++ show result)
  putStrLn ("Final stack: " ++ show finalStack)

  (result2, finalStack2) <- runStateT program2 []
  putStrLn ("Result2: " ++ show result2)
  putStrLn ("Final stack2: " ++ show finalStack2)
