module Main where

import Stack

program :: StackM (Maybe Int, Maybe Int)
program = do
  push 1
  push 2
  push 3
  a <- pop
  b <- pop
  push 100
  pure (a, b)

main :: IO ()
main = do
  let (result, finalStack) = runStackM program
  putStrLn ("Result: " ++ show result)
  putStrLn ("Final stack: " ++ show finalStack)
