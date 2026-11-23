{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, Reader, ReaderT, asks, runReader, runReaderT)
import Control.Monad.Writer (MonadWriter, Writer, WriterT, runWriter, runWriterT, tell)
import Data.Char (isDigit, isSpace)
import Data.Functor.Identity (Identity, runIdentity)

------------------------------------------------------------
-- 1. 自作の型クラス：操作の名前と型だけを決める
------------------------------------------------------------
class (Monad m) => MonadLogger m where
  logMsg :: String -> m ()

------------------------------------------------------------
-- 2. 本番用の具体的な型 App
------------------------------------------------------------
data Env = Env
  { envVerbose :: Bool, -- False ならログを出さない
    envPrefix :: String
  }

newtype App a = App (ReaderT Env (ExceptT String IO) a)
  deriving newtype
    (Functor, Applicative, Monad, MonadReader Env, MonadError String, MonadIO)

runApp :: Env -> App a -> IO (Either String a)
runApp env (App m) = runExceptT (runReaderT m env)

-- 自作クラスは App に直接実装する。ReaderT・ExceptT 用のインスタンスは書かない
instance MonadLogger App where
  logMsg s = do
    verbose <- asks envVerbose
    prefix <- asks envPrefix
    if verbose then liftIO (putStrLn (prefix ++ s)) else pure ()

------------------------------------------------------------
-- 2'. 実験用の具体的な型 App'（ExceptTとReaderTを逆にしたもの）
------------------------------------------------------------
newtype App' a = App' (ExceptT String (ReaderT Env IO) a)
  deriving newtype
    (Functor, Applicative, Monad, MonadReader Env, MonadError String, MonadIO)

runApp' :: Env -> App' a -> IO (Either String a)
runApp' env (App' m) = runReaderT (runExceptT m) env

-- 自作クラスは App' に直接実装する。ExceptT・ReaderT 用のインスタンスは書かない
instance MonadLogger App' where
  logMsg s = do
    verbose <- asks envVerbose
    prefix <- asks envPrefix
    if verbose then liftIO (putStrLn (prefix ++ s)) else pure ()

------------------------------------------------------------
-- 3. テスト用の具体的な型 TestM：IO を使わず、ログをリストに集める
------------------------------------------------------------
newtype TestM a = TestM (ReaderT Env (ExceptT String (WriterT [String] Identity)) a)
  deriving newtype
    (Functor, Applicative, Monad, MonadReader Env, MonadError String, MonadWriter [String])

runTestM :: Env -> TestM a -> (Either String a, [String])
runTestM env (TestM m) = runIdentity (runWriterT (runExceptT (runReaderT m env)))

instance MonadLogger TestM where
  logMsg s = do
    verbose <- asks envVerbose
    prefix <- asks envPrefix
    if verbose then tell [prefix ++ s] else pure ()

------------------------------------------------------------
-- 3'. テスト実験用の具体的な型 TestM'：TestMのReader <-> Writer版
------------------------------------------------------------
newtype TestM' a = TestM' (WriterT [String] (ExceptT String (ReaderT Env Identity)) a)
  deriving newtype
    (Functor, Applicative, Monad, MonadReader Env, MonadError String, MonadWriter [String])

runTestM' :: Env -> TestM' a -> (Either String (a, [String]))
runTestM' env (TestM' m) = runIdentity (runReaderT (runExceptT (runWriterT m)) env)

instance MonadLogger TestM' where
  logMsg s = do
    verbose <- asks envVerbose
    prefix <- asks envPrefix
    if verbose then tell [prefix ++ s] else pure ()

------------------------------------------------------------
-- 4. 本体：具体的な型を知らない（制約だけを書く）
------------------------------------------------------------
data Token = TInt Int | TPlus deriving (Show)

data Expr = Lit Int | Add Expr Expr deriving (Show)

data Instr = Push Int | IAdd deriving (Show)

-- Lexer
tokenize :: (MonadError String m) => String -> m [Token]
tokenize [] = pure []
tokenize (c : cs)
  | isSpace c = tokenize cs
  | isDigit c =
      let (digits, rest) = span isDigit (c : cs)
       in (TInt (read digits) :) <$> tokenize rest
  | c == '+' = (TPlus :) <$> tokenize cs
  | otherwise = throwError $ "unexpected character = " ++ [c]

-- Parser
parse :: (MonadError String m) => [Token] -> m Expr
parse (TInt n : TPlus : [TInt m]) = pure $ Add (Lit n) (Lit m)
parse tokens = throwError $ "unexpected tokens = " ++ show tokens

-- Code generator
compile :: Expr -> [Instr]
compile (Lit n) = [Push n]
compile (Add l r) = compile l ++ compile r ++ [IAdd]

-- Virtual machine
run :: (MonadError String m) => [Instr] -> m Int
run instrs = go instrs []
  where
    go [] [v] = pure v
    go [] _ = throwError "invalid stack state after execution"
    go (Push n : rest) stack = go rest (n : stack)
    go (IAdd : rest) (b : a : stack) = go rest ((a + b) : stack)
    go _ _ = throwError "stack underflow"

-- MonadIO を要求しないので、IO が無い TestM でも動く
pipeline :: (MonadError String m, MonadLogger m) => String -> m Int
pipeline input = do
  tokens <- tokenize input
  logMsg ("tokens: " ++ show tokens)
  expr <- parse tokens
  logMsg ("expr:   " ++ show expr)
  let instrs = compile expr
  logMsg ("instrs: " ++ show instrs)
  run instrs

------------------------------------------------------------
-- 5. 呼び出し側で具体的な型を選ぶ
------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "== App（本番）"
  runApp (Env True "[log] ") (pipeline "1+1") >>= print
  runApp (Env True "[log] ") (pipeline "1+") >>= print
  runApp (Env False "") (pipeline "2+3") >>= print

  putStrLn "== App'（実験）"
  runApp' (Env True "[log] ") (pipeline "1+1") >>= print
  runApp' (Env True "[log] ") (pipeline "1+") >>= print
  runApp' (Env False "") (pipeline "2+3") >>= print

  putStrLn "== TestM（テスト）"
  print (runTestM (Env True "[log] ") (pipeline "1+1"))
  print (runTestM (Env True "[log] ") (pipeline "1+"))
  print (runTestM (Env False "") (pipeline "2+3"))

  putStrLn "== TestM'（テスト実験）"
  print (runTestM' (Env True "[log] ") (pipeline "1+1"))
  print (runTestM' (Env True "[log] ") (pipeline "1+"))
  print (runTestM' (Env False "") (pipeline "2+3"))
