module Main where

import Stack
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "push / pop" $ do
    it "popはLIFO順で値を返す" $
      evalStackM (do
        push 1
        push 2
        push 3
        a <- pop
        b <- pop
        c <- pop
        pure [a, b, c])
        `shouldBe` [Just 3, Just 2, Just 1]
    it "pushは状態（スタック）の先頭に値を積む" $
      execStackM (push 1 >> push 2) `shouldBe` [2, 1]

  describe "空スタックへの操作" $ do
    it "popはNothingを返す" $
      evalStackM pop `shouldBe` Nothing
    it "peekはNothingを返す" $
      evalStackM peek `shouldBe` Nothing
    it "isEmptyはTrueを返す" $
      evalStackM isEmpty `shouldBe` True

  describe "peek / isEmpty" $ do
    it "peekは先頭を覗くだけで取り除かない" $
      runStackM (do
        push 1
        push 2
        a <- peek
        b <- peek
        pure (a, b))
        `shouldBe` ((Just 2, Just 2), [2, 1])
    it "isEmptyはpush後にFalseを返す" $
      evalStackM (push 1 >> isEmpty) `shouldBe` False

  describe "runStackM / evalStackM / execStackM の使い分け" $ do
    let sample = push 1 >> push 2 >> pop
    it "runStackMは結果と最終状態の両方を返す" $
      runStackM sample `shouldBe` (Just 2, [1])
    it "evalStackMは結果だけを返す" $
      evalStackM sample `shouldBe` Just 2
    it "execStackMは最終状態だけを返す" $
      execStackM sample `shouldBe` [1]
