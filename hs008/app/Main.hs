module Main where

import Calc
import Control.Monad.Except
import Control.Monad.State

-- 純粋な CalcM（ExceptT CalcError (State [Int])）の例
showCalc :: String -> IO ()
showCalc src = do
  let (result, finalStack) = runCalcM (evalRPN src)
  putStrLn (src ++ " => " ++ show result ++ "  stack: " ++ show finalStack)

-- ExceptT CalcError (StateT [Int] IO) の例。
-- push/pop/evalToken は MonadState/MonadError で汎用化してあるので、
-- 内側に IO があってもそのまま使える。
-- liftIO で IO アクションを持ち上げ、catchError でエラーから回復する。
program :: ExceptT CalcError (StateT [Int] IO) Int
program = do
  mapM_ step (words "10 2 / 0 /")
  pop
 where
  step :: String -> ExceptT CalcError (StateT [Int] IO) ()
  step tok = do
    evalToken tok `catchError` recover
    s <- get
    liftIO $ putStrLn ("  " ++ tok ++ " -> " ++ show s)
  -- 0 除算だけは握りつぶして 0 を積み直し、それ以外は再送出する
  recover :: CalcError -> ExceptT CalcError (StateT [Int] IO) ()
  recover DivByZero = do
    liftIO $ putStrLn "  (DivByZero を捕まえたので 0 を積む)"
    push 0
  recover e = throwError e

main :: IO ()
main = do
  putStrLn "--- CalcM ---"
  mapM_
    showCalc
    [ "3 4 + 2 *"
    , "1 0 /"
    , "1 +"
    , "1 2 foo"
    , "1 2"
    ]

  putStrLn "--- ExceptT over StateT IO ---"
  (result, finalStack) <- runStateT (runExceptT program) []
  putStrLn ("Result: " ++ show result)
  putStrLn ("Final stack: " ++ show finalStack)
