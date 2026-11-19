module Main where

import Control.Monad.Error.Class
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, get, modify, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Char (isDigit, isSpace)
import Data.IORef
import Text.Printf (printf)

data Token
  = TInt Int
  | TPlus
  deriving (Show, Eq)

data Expr
  = Lit Int
  | Add Expr Expr
  deriving (Show, Eq)

data Instr
  = Push Int
  | IAdd
  deriving (Show, Eq)

-- Lexer
-- 通常版
tokenize :: String -> Either String [Token]
tokenize [] = Right []
tokenize (c : cs)
  | isSpace c = tokenize cs
  | isDigit c =
      let (digits, rest) = span isDigit (c : cs)
       in (TInt (read digits) :) <$> tokenize rest
  | c == '+' = (TPlus :) <$> tokenize cs
  | otherwise = Left $ "unexpected character = " ++ [c]

-- mtl版
tokenize' :: (MonadError String m) => String -> m [Token]
tokenize' [] = pure []
tokenize' (c : cs)
  | isSpace c = tokenize' cs
  | isDigit c =
      let (digits, rest) = span isDigit (c : cs)
       in (TInt (read digits) :) <$> tokenize' rest
  | c == '+' = (TPlus :) <$> tokenize' cs
  | otherwise = throwError $ "unexpected character = " ++ [c]

-- Parser
-- 通常版
parse :: [Token] -> Either String Expr
parse (TInt n : TPlus : [TInt m]) = Right $ Add (Lit n) (Lit m)
parse tokens = Left $ "unexpected tokens = " ++ (show tokens)

-- mtl版
parse' :: (MonadError String m) => [Token] -> m Expr
parse' (TInt n : TPlus : [TInt m]) = pure $ Add (Lit n) (Lit m)
parse' tokens = throwError $ "unexpected tokens = " ++ (show tokens)

-- Code generator
compile :: Expr -> [Instr]
compile (Lit n) = [Push n]
compile (Add l r) = compile l ++ compile r ++ [IAdd]

-- Virtual machine
-- 通常版
run :: [Instr] -> Either String Int
run instrs = go instrs []
  where
    go [] [v] = Right v
    go [] _ = Left "invalid stack state after execution"
    go (Push n : rest) stack = go rest (n : stack)
    go (IAdd : rest) (b : a : stack) = go rest ((a + b) : stack)
    go _ _ = Left "stack underflow"

-- mtl版
run' :: (MonadError String m) => [Instr] -> m Int
run' instrs = go instrs []
  where
    go [] [v] = pure v
    go [] _ = throwError "invalid stack state after execution"
    go (Push n : rest) stack = go rest (n : stack)
    go (IAdd : rest) (b : a : stack) = go rest ((a + b) : stack)
    go _ _ = throwError "stack underflow"

-- 通常版
pipeline :: String -> Either String Int
pipeline input = do
  tokens <- tokenize input
  exprs <- parse tokens
  run $ compile exprs

-- mtl版
pipeline' :: (MonadError String m, MonadIO m) => String -> m Int
pipeline' input = do
  tokenize' input >>= \tokens ->
    liftIO (putStrLn (show tokens))
      >> parse' tokens
      >>= \exprs ->
        liftIO (putStrLn (show exprs))
          >> run' (compile exprs)

-- tokens <- tokenize' input
-- liftIO $ putStrLn (show tokens)
-- exprs <- parse' tokens
-- run' $ compile exprs

main :: IO ()
main = do
  let input = "1+1"
  -- runExceptTの型: IO (Either String Int)
  r <- runExceptT (pipeline' input)
  -- either putStrLn print r
  case r of
    Right n -> print n
    Left err -> putStrLn err

-- let input = "1+1"
--     result = pipeline' input
-- case result of
--   Left err -> putStrLn err
--   Right n -> print n

-- type App = ReaderT String (StateT Int (ExceptT String IO))
--
-- -- lift で1段ずつ上げる：段数と同じ3回の lift が要る
-- logLift :: String -> App ()
-- logLift msg = lift (lift (lift (putStrLn msg)))
--
-- -- liftIO なら何段重ねでも1回
-- logIO :: String -> App ()
-- logIO msg = liftIO (putStrLn msg)
--
-- -- mtl スタイル：何段重ねかを知らなくてよい
-- logM :: (MonadIO m) => String -> m ()
-- logM msg = liftIO (putStrLn msg)
--
-- step :: App Int
-- step = do
--   name <- ask
--   s1 <- get
--   logLift (printf "lift     : name = '%s', status = %d" name s1)
--   logIO (printf "liftIO   : name = '%s', status = %d" name s1)
--   logM (printf "MonadIO  : name = '%s', status = %d" name s1)
--   modify (+ 1)
--   s2 <- get
--   line <- liftIO (pure "入力の代わり") -- 結果を受け取る IO も同じ書き方
--   logIO ("got: " ++ line)
--   if s2 > 10 then throwError "too big" else pure s2
--
-- main :: IO ()
-- main = do
--   r <- runExceptT (runStateT (runReaderT step "alice") 0)
--   print r
