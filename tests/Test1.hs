module Main where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import SemanticCheck
import System.Directory

main :: IO ()
main = hspec $ do
   {-
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
   -}

   describe "Interleave" $ do
      {-
      it "test interleave2" $ do
         (fileCheckOutput "tests/src/interleave2.txt")  `shouldReturn` [True]
      it "test interleave3" $ do
         (fileCheckOutput "tests/src/interleave3.txt")  `shouldReturn` [True]
      it "test interleave6" $ do
         (fileCheck "tests/src/interleave6.txt")  `shouldReturn` [True]
      it "test interleave10" $ do
         (fileCheck "tests/src/interleave10.txt")  `shouldReturn` [True]
      it "test interleave12" $ do
         (fileCheck "tests/src/interleave12.txt")  `shouldReturn` [True]
      it "test interleave13" $ do
         (fileCheck "tests/src/interleave13.txt")  `shouldReturn` [True]
      it "test interleave14" $ do
         (fileCheck "tests/src/interleave14.txt")  `shouldReturn` [True]
      it "test interleave15" $ do
         (fileCheck "tests/src/interleave15.txt")  `shouldReturn` [True]
      it "test interleave16" $ do
         (fileCheck "tests/src/interleave16.txt")  `shouldReturn` [True]
      it "test interleave17" $ do
         (fileCheck "tests/src/interleave17.txt")  `shouldReturn` [True]-}
      it "test interleave18" $ do
         (fileCheck "tests/src/interleave18.txt")  `shouldReturn` [True]