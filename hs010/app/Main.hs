module Main where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, get, modify, runStateT)
import Control.Monad.Trans.Class (lift)

-- 上から ReaderT → StateT → ExceptT、一番下が IO
type App = ReaderT String (StateT Int (ExceptT String IO))

-- lift で1段ずつ上げる：段数と同じ3回の lift が要る
logLift :: String -> App ()
logLift msg = lift (lift (lift (putStrLn msg)))

-- liftIO なら何段重ねでも1回
logIO :: String -> App ()
logIO msg = liftIO (putStrLn msg)

-- mtl スタイル：何段重ねかを知らなくてよい
logM :: (MonadIO m) => String -> m ()
logM msg = liftIO (putStrLn msg)

step :: App Int
step = do
  name <- ask
  logLift ("lift x3: " ++ name)
  logIO "liftIO x1"
  logM "MonadIO m => ..."
  modify (+ 1)
  n <- get
  line <- liftIO (pure "入力の代わり") -- 結果を受け取る IO も同じ書き方
  logIO ("got: " ++ line)
  if n > 10 then throwError "too big" else pure n

main :: IO ()
main = do
  r <- runExceptT (runStateT (runReaderT step "alice") 0)
  print r
