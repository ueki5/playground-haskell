module Stack
  ( StackM
  , push
  , pop
  , peek
  , isEmpty
  , runStackM
  , evalStackM
  , execStackM
  ) where

import Control.Monad.State

type StackM a = State [Int] a

push :: Int -> StackM ()
push x = modify (x :)

pop :: StackM (Maybe Int)
pop = do
  s <- get
  case s of
    []       -> pure Nothing
    (x : xs) -> put xs >> pure (Just x)

peek :: StackM (Maybe Int)
peek = gets safeHead
  where
    safeHead []      = Nothing
    safeHead (x : _) = Just x

isEmpty :: StackM Bool
isEmpty = gets null

runStackM :: StackM a -> (a, [Int])
runStackM m = runState m []

evalStackM :: StackM a -> a
evalStackM m = evalState m []

execStackM :: StackM a -> [Int]
execStackM m = execState m []
