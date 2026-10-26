-- | ExceptT の仕組みを理解するための自前実装。
-- mtl / transformers の ExceptT と同じ構造を最小限で書いている。
module Util
  ( ExceptT' (..)
  , throwE'
  , catchE'
  ) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans (MonadTrans (..))

-- ExceptT' の正体は「m (Either e a)」を包んだだけの newtype。
newtype ExceptT' e m a = ExceptT' { runExceptT' :: m (Either e a) }

instance Functor m => Functor (ExceptT' e m) where
  fmap f (ExceptT' m) = ExceptT' (fmap (fmap f) m)

instance Monad m => Applicative (ExceptT' e m) where
  pure a = ExceptT' (pure (Right a))
  ExceptT' mf <*> ExceptT' ma = ExceptT' $ do
    ef <- mf
    case ef of
      Left e -> pure (Left e) -- 失敗したら右側は実行しない
      Right f -> fmap (fmap f) ma

-- 肝はここ：Main.hs の run1 で手書きしていた case を >>= が肩代わりする
instance Monad m => Monad (ExceptT' e m) where
  ExceptT' m >>= k = ExceptT' $ do
    ea <- m
    case ea of
      Left e -> pure (Left e)
      Right a -> runExceptT' (k a)

-- 内側のモナドの処理を持ち上げる（成功扱いで Right に包む）
instance MonadTrans (ExceptT' e) where
  lift m = ExceptT' (fmap Right m)

instance MonadIO m => MonadIO (ExceptT' e m) where
  liftIO = lift . liftIO

-- 失敗させる：Left を返すだけ
throwE' :: Monad m => e -> ExceptT' e m a
throwE' e = ExceptT' (pure (Left e))

-- 失敗を捕まえる：Left のときだけハンドラに処理を渡す
catchE' :: Monad m => ExceptT' e m a -> (e -> ExceptT' e' m a) -> ExceptT' e' m a
catchE' (ExceptT' m) h = ExceptT' $ do
  ea <- m
  case ea of
    Left e -> runExceptT' (h e)
    Right a -> pure (Right a)
