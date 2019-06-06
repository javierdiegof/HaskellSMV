module Main where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import ModelCheck
import System.Directory

main :: IO ()
main = hspec $ do
   describe (title "CounterOutput") $ do
      it "test counter 3" $
         fileCheckOutput "test/testcodes/counter/H/counterH3.txt" `shouldReturn` [True,True,True]
      it "test counter 4" $ 
         fileCheckOutput "test/testcodes/counter/H/counterH4.txt" `shouldReturn` [True,True,True]
      it "test counter 7" $ 
         fileCheckOutput "test/testcodes/counter/H/counterH7.txt" `shouldReturn` [True,True,True]
      it "test counter 9" $ 
         fileCheckOutput "test/testcodes/counter/H/counterH9.txt" `shouldReturn` [True,True,True]
      it "test counter 10" $ 
         fileCheckOutput "test/testcodes/counter/H/counterH10.txt" `shouldReturn` [True,True,True]
    
   describe (title "CounterSilent") $ do     
      it "test counter 3" $ 
         fileCheck "test/testcodes/counter/H/counterH3.txt" `shouldReturn` [True,True,True]
      it "test counter 4" $ 
         fileCheck "test/testcodes/counter/H/counterH4.txt" `shouldReturn` [True,True,True]
      it "test counter 7" $ 
         fileCheck "test/testcodes/counter/H/counterH7.txt" `shouldReturn` [True,True,True]
      it "test counter 9" $ 
         fileCheck "test/testcodes/counter/H/counterH9.txt" `shouldReturn` [True,True,True]
      it "test counter 10" $ 
         fileCheck "test/testcodes/counter/H/counterH10.txt" `shouldReturn` [True,True,True]
   
   describe (title "FairOutput") $ do
      it "test fair 3" $ 
         fileCheckOutput "test/testcodes/fair/H/fairH3.txt" `shouldReturn` [True]
      it "test fair 4" $ 
         fileCheckOutput "test/testcodes/fair/H/fairH4.txt" `shouldReturn` [True]
      it "test fair 7" $ 
         fileCheckOutput "test/testcodes/fair/H/fairH7.txt" `shouldReturn` [True]
   
   describe (title "FairSilent") $ do     
      it "test fair 3" $ 
         fileCheck "test/testcodes/fair/H/fairH3.txt" `shouldReturn` [True]
      it "test fair 4" $ 
         fileCheck "test/testcodes/fair/H/fairH4.txt" `shouldReturn` [True]
      it "test counter 7" $ 
         fileCheck "test/testcodes/fair/H/fairH7.txt" `shouldReturn` [True]

   describe (title "InterleaveOutput") $ do
      it "test interleave 3" $ 
         fileCheckOutput "test/testcodes/interleave/H/interleaveH3.txt" `shouldReturn` [True]
      it "test interleave 4" $ 
         fileCheckOutput "test/testcodes/interleave/H/interleaveH4.txt" `shouldReturn` [True]
      it "test interleave 7" $ 
         fileCheckOutput "test/testcodes/interleave/H/interleaveH7.txt" `shouldReturn` [True]
   
   describe (title "InterleaveSilent") $ do     
      it "test interleave 3" $ 
         fileCheck "test/testcodes/interleave/H/interleaveH3.txt" `shouldReturn` [True]
      it "test interleave 4" $ 
         fileCheck "test/testcodes/interleave/H/interleaveH4.txt" `shouldReturn` [True]
      it "test interleave 7" $ 
         fileCheck "test/testcodes/interleave/H/interleaveH7.txt" `shouldReturn` [True]

   describe (title "ShiftOutput") $ do
      it "test interleave 3" $ 
         fileCheckOutput "test/testcodes/shift/H/shiftH3.txt" `shouldReturn` [True,True,True,True,True]
      it "test interleave 4" $ 
         fileCheckOutput "test/testcodes/shift/H/shiftH4.txt" `shouldReturn` [True,True,True,True,True]
      it "test interleave 7" $ 
         fileCheckOutput "test/testcodes/shift/H/shiftH7.txt" `shouldReturn` [True,True,True,True,True]
   
   describe (title "ShiftSilent") $ do     
      it "test interleave 3" $ 
         fileCheck "test/testcodes/shift/H/shiftH3.txt" `shouldReturn` [True,True,True,True,True]
      it "test interleave 4" $ 
         fileCheck "test/testcodes/shift/H/shiftH4.txt" `shouldReturn` [True,True,True,True,True]
      it "test interleave 7" $ 
         fileCheck "test/testcodes/shift/H/shiftH7.txt" `shouldReturn` [True,True,True,True,True]



title :: String -> String
title tit = let
               deco = "\n" ++ concat (replicate  100 "-") ++ "\n"
            in 
               "\n" ++ deco ++ tit ++ deco ++ "\n"