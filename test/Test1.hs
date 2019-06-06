module Main where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import ModelCheck
import System.Directory

main :: IO ()
main = hspec $ do
   describe (title "Dining") $ do
      it "dining 2" $
         fileCheckOutput "test/testcodes/dining/dining2.txt" `shouldReturn` [True, True]


title :: String -> String
title tit = let
               deco = "\n" ++ concat (replicate  100 "-") ++ "\n"
            in 
               "\n" ++ deco ++ tit ++ deco ++ "\n"