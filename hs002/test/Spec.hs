module Main where

import Lib (greeting)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "greeting" $ do
    it "正しい挨拶文を返す" $
      greeting `shouldBe` "Hello, Haskell!"
