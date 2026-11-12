-- | ExceptT と IORef を組み合わせた、銀行口座の最小の例。
-- 残高は IORef（書き換えられる参照）で持ち、失敗しうる操作は ExceptT で表す。
module Bank
  ( BankError (..),
    BankM,
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

data Account = Account {amount :: Int}

type BankM = Either BankError

-- 口座の操作で起こりうるエラー
data BankError
  = InvalidAmount Int -- 0 以下の金額を指定した
  | InsufficientFunds Int Int -- 残高が足りない（残高, 引き出そうとした額）
  deriving (Show, Eq)

newAccount :: Int -> Account
newAccount = Account

balance :: Account -> BankM Int
balance acc = Right $ amount acc

checkAmount :: Int -> BankM ()
checkAmount n
  | n <= 0 = Left $ InvalidAmount n
  | otherwise = Right ()

deposit :: Account -> Int -> BankM ()
deposit acc n = do
  checkAmount n
  Right ()

-- 残高が足りなければ、書き換える前に throwError で中断する
withdraw :: Account -> Int -> BankM ()
withdraw acc n = do
  checkAmount n
  b <- balance acc
  if b < n
    then
      Left (InsufficientFunds b n)
    else
      Right () -- ダミー処理（値更新できない為）

transfer :: Account -> Account -> Int -> BankM ()
transfer from to n = do
  withdraw from n
  deposit to n

-- 複数の相手に順に支払う。
-- 途中で失敗すると以降の支払いは実行されないが、それまでの支払いは取り消されない。
-- IORef の書き換えは IO の効果としてすでに起きており、ExceptT はそれを巻き戻さないため。
payAll :: Account -> [(Account, Int)] -> BankM ()
payAll from = mapM_ (\(to, n) -> transfer from to n)

-- 失敗したときは、指定した口座の残高を実行前の値に戻してから、同じエラーを投げ直す
withRollback :: [Account] -> BankM a -> BankM a
withRollback accs m = do
  -- saved <- liftIO (mapM readIORef accs) -- ダミー処理（値更新できない為）
  m

runBankM :: BankM a -> IO (Either BankError a)
runBankM m = pure m

------------------------------------------------------------------------------------------
-- -- | ExceptT と IORef を組み合わせた、銀行口座の最小の例。
-- -- 残高は IORef（書き換えられる参照）で持ち、失敗しうる操作は ExceptT で表す。
-- module Bank
--   ( BankError (..),
--     BankM,
--     Account,
--     newAccount,
--     balance,
--     deposit,
--     withdraw,
--     transfer,
--     payAll,
--     withRollback,
--     runBankM,
--   )
-- where
--
-- import Control.Monad (when, zipWithM_)
-- import Control.Monad.Except (ExceptT, catchError, runExceptT, throwError)
-- import Control.Monad.IO.Class (liftIO)
-- import Data.IORef
--
-- -- 口座の操作で起こりうるエラー
-- data BankError
--   = InvalidAmount Int -- 0 以下の金額を指定した
--   | InsufficientFunds Int Int -- 残高が足りない（残高, 引き出そうとした額）
--   deriving (Show, Eq)
--
-- -- 口座の残高。IORef なので、書き換えると同じ口座を参照しているすべての場所から見える
-- type Account = IORef Int
--
-- -- ExceptT でエラーを、その内側の IO で IORef の読み書きを扱う。
-- -- IORef の操作は IO なので、ExceptT の中では liftIO で持ち上げて使う。
-- type BankM = ExceptT BankError IO
--
-- newAccount :: Int -> IO Account
-- newAccount = newIORef
--
-- balance :: Account -> BankM Int
-- balance acc = liftIO (readIORef acc)
--
-- checkAmount :: Int -> BankM ()
-- checkAmount n = when (n <= 0) (throwError (InvalidAmount n))
--
-- deposit :: Account -> Int -> BankM ()
-- deposit acc n = do
--   checkAmount n
--   liftIO (modifyIORef' acc (+ n))
--
-- -- 残高が足りなければ、書き換える前に throwError で中断する
-- withdraw :: Account -> Int -> BankM ()
-- withdraw acc n = do
--   checkAmount n
--   b <- balance acc
--   when (b < n) (throwError (InsufficientFunds b n))
--   liftIO (writeIORef acc (b - n))
--
-- transfer :: Account -> Account -> Int -> BankM ()
-- transfer from to n = do
--   withdraw from n
--   deposit to n
--
-- -- 複数の相手に順に支払う。
-- -- 途中で失敗すると以降の支払いは実行されないが、それまでの支払いは取り消されない。
-- -- IORef の書き換えは IO の効果としてすでに起きており、ExceptT はそれを巻き戻さないため。
-- payAll :: Account -> [(Account, Int)] -> BankM ()
-- payAll from = mapM_ (\(to, n) -> transfer from to n)
--
-- -- 失敗したときは、指定した口座の残高を実行前の値に戻してから、同じエラーを投げ直す
-- withRollback :: [Account] -> BankM a -> BankM a
-- withRollback accs m = do
--   saved <- liftIO (mapM readIORef accs)
--   m `catchError` \e -> do
--     liftIO (zipWithM_ writeIORef accs saved)
--     throwError e
--
-- runBankM :: BankM a -> IO (Either BankError a)
-- runBankM = runExceptT
