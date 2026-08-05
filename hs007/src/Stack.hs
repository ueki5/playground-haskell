{-# LANGUAGE FlexibleContexts #-}

module Stack (
  StackM,
  push,
  pop,
  peek,
  isEmpty,
  runStackM,
  evalStackM,
  execStackM,
) where

import Control.Monad.State

-- type StackM a = State [Int] a
type StackM = State [Int]

push :: MonadState [Int] m => Int -> m ()
push x = modify (x :)

pop :: MonadState [Int] m => m (Maybe Int)
pop = do
  s <- get
  case s of
    [] -> pure Nothing
    (x : xs) -> put xs >> pure (Just x)

peek :: MonadState [Int] m => m (Maybe Int)
peek = gets safeHead
 where
  safeHead [] = Nothing
  safeHead (x : _) = Just x

isEmpty :: MonadState [Int] m => m Bool
isEmpty = gets null

runStackM :: StackM a -> (a, [Int])
runStackM m = runState m []

evalStackM :: StackM a -> a
evalStackM m = evalState m []

execStackM :: StackM a -> [Int]
execStackM m = execState m []
