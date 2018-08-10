module Main where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import SemanticCheck
import System.Directory

main :: IO ()
main = hspec $ do
   describe "Counters" $ do
      it "test counter3" $ do
         (fileCheck "tests/src/acounter3.txt")  `shouldReturn` [True, False]
      it "test counter5" $ do
         (fileCheck "tests/src/acounter5.txt")  `shouldReturn` [True, True]
      it "test counter 10" $ do
         (fileCheck "tests/src/acounter10.txt")  `shouldReturn` [False, True]

   describe "Shift registers" $ do
      it "test counter3" $ do
         (fileCheck "tests/src/acounter3.txt")  `shouldReturn` [True, False]
   