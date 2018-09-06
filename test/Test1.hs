module Main where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import SemanticCheck
import System.Directory

main :: IO ()
main = hspec $ do
   describe (title "CounterOutput") $ do
      it "test counter 3" $ do
         (fileCheckOutput "test/testcodes/counter/H/counterH3.txt") `shouldReturn` [True]
      it "test counter 4" $ do
         (fileCheckOutput "test/testcodes/counter/H/counterH4.txt") `shouldReturn` [True]
      it "test counter 7" $ do
         (fileCheckOutput "test/testcodes/counter/H/counterH7.txt") `shouldReturn` [True]
   
   describe (title "CounterSilent") $ do     
      it "test counter 3" $ do
         (fileCheck "test/testcodes/counter/H/counterH3.txt") `shouldReturn` [True]
      it "test counter 4" $ do
         (fileCheck "test/testcodes/counter/H/counterH4.txt") `shouldReturn` [True]
      it "test counter 7" $ do
         (fileCheck "test/testcodes/counter/H/counterH7.txt") `shouldReturn` [True]
   
   describe (title "FairOutput") $ do
      it "test fair 3" $ do
         (fileCheckOutput "test/testcodes/fair/H/fairH3.txt") `shouldReturn` [True]
      it "test fair 4" $ do
         (fileCheckOutput "test/testcodes/fair/H/fairH4.txt") `shouldReturn` [True]
      it "test fair 7" $ do
         (fileCheckOutput "test/testcodes/fair/H/fairH7.txt") `shouldReturn` [True]
   
   describe (title "FairSilent") $ do     
      it "test fair 3" $ do
         (fileCheck "test/testcodes/fair/H/fairH3.txt") `shouldReturn` [True]
      it "test fair 4" $ do
         (fileCheck "test/testcodes/fair/H/fairH4.txt") `shouldReturn` [True]
      it "test counter 7" $ do
         (fileCheck "test/testcodes/fair/H/fairH7.txt") `shouldReturn` [True]

   describe (title "InterleaveOutput") $ do
      it "test interleave 3" $ do
         (fileCheckOutput "test/testcodes/interleave/H/interleaveH3.txt") `shouldReturn` [True]
      it "test interleave 4" $ do
         (fileCheckOutput "test/testcodes/interleave/H/interleaveH4.txt") `shouldReturn` [True]
      it "test interleave 7" $ do
         (fileCheckOutput "test/testcodes/interleave/H/interleaveH7.txt") `shouldReturn` [True]
   
   describe (title "InterleaveSilent") $ do     
      it "test interleave 3" $ do
         (fileCheck "test/testcodes/interleave/H/interleaveH3.txt") `shouldReturn` [True]
      it "test interleave 4" $ do
         (fileCheck "test/testcodes/interleave/H/interleaveH4.txt") `shouldReturn` [True]
      it "test interleave 7" $ do
         (fileCheck "test/testcodes/interleave/H/interleaveH7.txt") `shouldReturn` [True]

   describe (title "ShiftOutput") $ do
      it "test interleave 3" $ do
         (fileCheckOutput "test/testcodes/shift/H/shiftH3.txt") `shouldReturn` [True,True,True,True,True]
      it "test interleave 4" $ do
         (fileCheckOutput "test/testcodes/shift/H/shiftH4.txt") `shouldReturn` [True,True,True,True,True]
      it "test interleave 7" $ do
         (fileCheckOutput "test/testcodes/shift/H/shiftH7.txt") `shouldReturn` [True,True,True,True,True]
   
   describe (title "ShiftSilent") $ do     
      it "test interleave 3" $ do
         (fileCheck "test/testcodes/shift/H/shiftH3.txt") `shouldReturn` [True,True,True,True,True]
      it "test interleave 4" $ do
         (fileCheck "test/testcodes/shift/H/shiftH4.txt") `shouldReturn` [True,True,True,True,True]
      it "test interleave 7" $ do
         (fileCheck "test/testcodes/shift/H/shiftH7.txt") `shouldReturn` [True,True,True,True,True]



title :: String -> String
title tit = let
               deco = "\n" ++ concat (replicate  100 "-") ++ "\n"
            in 
               "\n" ++ deco ++ tit ++ deco ++ "\n"