-- | ExceptT と IORef を組み合わせた、銀行口座の最小の例（mtl スタイル）。
-- 残高は IORef（書き換えられる参照）で持ち、失敗しうる操作は MonadError で表す。
-- 各操作は具体的なモナドを決めず、必要な能力（MonadError・MonadIO）だけを制約で書く。
-- 具体的なモナド（BankM = ExceptT BankError IO）は、runBankM で実行するときに決まる。
module Bank
  ( BankError (..),
    BankM,
    MonadBank,
    Account,
    newAccount,
    balance,
    deposit,
    withdraw,
    transfer,
    payAll,
    withRollback,
    runBankM,
  )
where

import Control.Monad (when, zipWithM_)
import Control.Monad.Except (ExceptT, MonadError, catchError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef

-- 口座の操作で起こりうるエラー
data BankError
  = InvalidAmount Int -- 0 以下の金額を指定した
  | InsufficientFunds Int Int -- 残高が足りない（残高, 引き出そうとした額）
  deriving (Show, Eq)

-- 口座の残高。IORef なので、書き換えると同じ口座を参照しているすべての場所から見える
type Account = IORef Int

-- 口座の操作に必要な能力。
-- MonadError BankError で throwError・catchError を、MonadIO で liftIO（IORef の読み書き）を使う。
-- 制約に別名をつけるには ConstraintKinds が必要（GHC2024 では最初から有効）。
type MonadBank m = (MonadError BankError m, MonadIO m)

-- MonadBank を満たす具体的なモナドの1つ。runBankM で実行するときに使う
type BankM = ExceptT BankError IO

newAccount :: (MonadIO m) => Int -> m Account
newAccount = liftIO . newIORef

balance :: (MonadIO m) => Account -> m Int
balance acc = liftIO (readIORef acc)

-- エラーを投げるだけで IO は使わないので、MonadError だけを要求する
checkAmount :: (MonadError BankError m) => Int -> m ()
checkAmount n = when (n <= 0) (throwError (InvalidAmount n))

deposit :: (MonadBank m) => Account -> Int -> m ()
deposit acc n = do
  checkAmount n
  liftIO (modifyIORef' acc (+ n))

-- 残高が足りなければ、書き換える前に throwError で中断する
withdraw :: (MonadBank m) => Account -> Int -> m ()
withdraw acc n = do
  checkAmount n
  b <- balance acc
  when (b < n) (throwError (InsufficientFunds b n))
  liftIO (writeIORef acc (b - n))

transfer :: (MonadBank m) => Account -> Account -> Int -> m ()
transfer from to n = do
  withdraw from n
  deposit to n

-- 複数の相手に順に支払う。
-- 途中で失敗すると以降の支払いは実行されないが、それまでの支払いは取り消されない。
-- IORef の書き換えは IO の効果としてすでに起きており、エラーの仕組みはそれを巻き戻さないため。
payAll :: (MonadBank m) => Account -> [(Account, Int)] -> m ()
payAll from = mapM_ (\(to, n) -> transfer from to n)

-- 失敗したときは、指定した口座の残高を実行前の値に戻してから、同じエラーを投げ直す
withRollback :: (MonadBank m) => [Account] -> m a -> m a
withRollback accs m = do
  saved <- liftIO (mapM readIORef accs)
  m `catchError` \e -> do
    liftIO (zipWithM_ writeIORef accs saved)
    throwError e

-- ここで初めて m = ExceptT BankError IO に決まる
runBankM :: BankM a -> IO (Either BankError a)
runBankM = runExceptT
