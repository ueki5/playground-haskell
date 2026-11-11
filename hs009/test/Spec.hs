module Main where

import Bank
import Control.Monad.Except (catchError)
import Data.IORef (readIORef)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "deposit" $ do
    it "残高が増える" $ do
      acc <- newAccount 10
      runBankM (deposit acc 5) `shouldReturn` Right ()
      readIORef acc `shouldReturn` 15
    it "0 以下の金額は InvalidAmount で、残高は変わらない" $ do
      acc <- newAccount 10
      runBankM (deposit acc 0) `shouldReturn` Left (InvalidAmount 0)
      readIORef acc `shouldReturn` 10

  describe "withdraw" $ do
    it "残高が減る" $ do
      acc <- newAccount 10
      runBankM (withdraw acc 3) `shouldReturn` Right ()
      readIORef acc `shouldReturn` 7
    it "残高不足は InsufficientFunds で、残高は変わらない" $ do
      acc <- newAccount 10
      runBankM (withdraw acc 30) `shouldReturn` Left (InsufficientFunds 10 30)
      readIORef acc `shouldReturn` 10

  describe "transfer" $ do
    it "送り元が減り、送り先が増える" $ do
      from <- newAccount 100
      to <- newAccount 0
      runBankM (transfer from to 30) `shouldReturn` Right ()
      readIORef from `shouldReturn` 70
      readIORef to `shouldReturn` 30
    it "引き出しで失敗すると、送り先への入金は実行されない" $ do
      from <- newAccount 10
      to <- newAccount 0
      runBankM (transfer from to 30) `shouldReturn` Left (InsufficientFunds 10 30)
      readIORef to `shouldReturn` 0

  describe "ExceptT と IORef の性質" $ do
    it "途中で失敗しても、それまでの IORef の書き換えは取り消されない" $ do
      alice <- newAccount 100
      bob <- newAccount 0
      carol <- newAccount 0
      runBankM (payAll alice [(bob, 50), (carol, 80)])
        `shouldReturn` Left (InsufficientFunds 50 80)
      readIORef alice `shouldReturn` 50
      readIORef bob `shouldReturn` 50
      readIORef carol `shouldReturn` 0
    it "最初のエラーで処理が打ち切られ、以降の操作は実行されない" $ do
      alice <- newAccount 100
      bob <- newAccount 0
      runBankM (payAll alice [(bob, 0), (bob, 10)])
        `shouldReturn` Left (InvalidAmount 0)
      readIORef bob `shouldReturn` 0

  describe "catchError" $ do
    it "エラーを捕まえて別の値で続行できる" $ do
      acc <- newAccount 10
      runBankM ((withdraw acc 30 >> pure "ok") `catchError` \e -> pure (show e))
        `shouldReturn` Right "InsufficientFunds 10 30"
    it "withRollback は失敗したとき残高を元に戻し、同じエラーを返す" $ do
      alice <- newAccount 100
      bob <- newAccount 0
      carol <- newAccount 0
      runBankM (withRollback [alice, bob, carol] (payAll alice [(bob, 50), (carol, 80)]))
        `shouldReturn` Left (InsufficientFunds 50 80)
      readIORef alice `shouldReturn` 100
      readIORef bob `shouldReturn` 0
      readIORef carol `shouldReturn` 0
    it "withRollback は成功したときは何もしない" $ do
      alice <- newAccount 100
      bob <- newAccount 0
      runBankM (withRollback [alice, bob] (transfer alice bob 30)) `shouldReturn` Right ()
      readIORef alice `shouldReturn` 70
      readIORef bob `shouldReturn` 30
