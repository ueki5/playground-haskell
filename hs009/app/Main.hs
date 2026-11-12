module Main where

import Bank

-- import Data.IORef (readIORef)
-- import GHC.Base (VecElem (Int32ElemRep))

-- 3 つの口座の残高を表示する
showBalances :: Account -> Account -> Account -> IO ()
showBalances alice bob carol =
  let a = balance alice
      b = balance bob
      c = balance carol
   in case (a, b, c) of
        (Right na, Right nb, Right nc) -> putStrLn ("  alice=" ++ show na ++ " bob=" ++ show nb ++ " carol=" ++ show nc)
        (Right _, Right _, Left ec) -> putStrLn ("  carol=" ++ show ec)
        (Right _, Left eb, Right _) -> putStrLn ("  bob=" ++ show eb)
        (Right _, Left eb, Left ec) -> putStrLn ("  bob=" ++ show eb ++ " carol=" ++ show ec)
        (Left ea, Right _, Right _) -> putStrLn ("  alice=" ++ show ea)
        (Left ea, Right _, Left ec) -> putStrLn ("  alice=" ++ show ea ++ " carol=" ++ show ec)
        (Left ea, Left eb, Right _) -> putStrLn ("  alice=" ++ show ea ++ " bob=" ++ show eb)
        (Left ea, Left eb, Left ec) -> putStrLn ("  alice=" ++ show ea ++ " bob=" ++ show eb ++ " carol=" ++ show ec)

main :: IO ()
main =
  let alice = newAccount 100
      bob = newAccount 0
      carol = newAccount 0
   in do
        putStrLn "--- initial ---"
        showBalances alice bob carol
        putStrLn "--- transfer alice -> bob 30 ---"
        runBankM (transfer alice bob 30) >>= print
        showBalances alice bob carol

        putStrLn "--- withdraw bob 50（残高不足） ---"
        runBankM (withdraw bob 50) >>= print
        showBalances alice bob carol

        putStrLn "--- payAll alice [bob 50, carol 101]（2 件目で失敗。1 件目は残る） ---"
        runBankM (payAll alice [(bob, 50), (carol, 101)]) >>= print
        showBalances alice bob carol

        putStrLn "--- withRollback で包む [carol 50, bob 102]（1 件目は成功、2 件目で失敗。全部戻す） ---"
        runBankM (withRollback [alice, bob, carol] (payAll alice [(carol, 50), (bob, 102)])) >>= print
        showBalances alice bob carol

-------------------------------------------------------------------------------
-- -- 3 つの口座の残高を表示する
-- showBalances :: Account -> Account -> Account -> IO ()
-- showBalances alice bob carol = do
--   a <- readIORef alice
--   b <- readIORef bob
--   c <- readIORef carol
--   putStrLn ("  alice=" ++ show a ++ " bob=" ++ show b ++ " carol=" ++ show c)
--
-- main :: IO ()
-- main = do
--   alice <- newAccount 100
--   bob <- newAccount 0
--   carol <- newAccount 0
--
--   putStrLn "--- initial ---"
--   showBalances alice bob carol
--   putStrLn "--- transfer alice -> bob 30 ---"
--   runBankM (transfer alice bob 30) >>= print
--   showBalances alice bob carol
--
--   putStrLn "--- withdraw bob 50（残高不足） ---"
--   runBankM (withdraw bob 50) >>= print
--   showBalances alice bob carol
--
--   putStrLn "--- payAll alice [bob 50, carol 50]（2 件目で失敗。1 件目は残る） ---"
--   runBankM (payAll alice [(bob, 50), (carol, 50)]) >>= print
--   showBalances alice bob carol
--
--   putStrLn "--- withRollback で包む [carol 10, bob 50]（1 件目は成功、2 件目で失敗。全部戻す） ---"
--   runBankM (withRollback [alice, bob, carol] (payAll alice [(carol, 10), (bob, 50)])) >>= print
--   showBalances alice bob carol
