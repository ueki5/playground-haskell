module Main where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, get, modify, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.IORef
import Text.Printf (printf)

data Status = First | Second | Third

type OperateM = StateT Status (Either String)

first :: Status
first = Third

next :: Status -> Either String Status
next s = case s of
  First -> Right Second
  Second -> Right Third
  _ -> Left "invalid operation"

main :: IO ()
main = do
  let status = first
      count = 1 :: Int
  putStrLn "!"

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
