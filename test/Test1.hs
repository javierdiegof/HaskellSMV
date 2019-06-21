module Main where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import ModelCheck
import System.Directory

main :: IO ()
main = hspec $ do
   describe (title "ThesisCheck") $ do
      it "Counter3" $
         fileCheck "test/testcodes/thesis/counter.txt" `shouldReturn` [True, True, True, True]
      it "Shift3" $
         fileCheck "test/testcodes/thesis/shift.txt" `shouldReturn` [True, True, True]
      it "Dining2" $
         fileCheck "test/testcodes/thesis/dining.txt" `shouldReturn` [True, True, True, True, False, False]


title :: String -> String
title tit = let
               deco = "\n" ++ concat (replicate  100 "-") ++ "\n"
            in 
               "\n" ++ deco ++ tit ++ deco ++ "\n"