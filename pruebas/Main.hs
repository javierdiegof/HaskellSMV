module Main 
where

import SemanticCheck
import Data.HasCacBDD
import Criterion.Main
import CounterGenerator



outputF :: String -> IO()
outputF str = fileCheckOutput str


genPrograms :: [Int] -> IO ()
genPrograms [] = return ()
genPrograms (x:xs) = do
                        genProgram x  
                        genPrograms xs

genProgramSeq :: Int -> IO ()
genProgramSeq x = genPrograms ([y | y <- [1 .. x]])

main = defaultMain [
         bgroup "counter" [
            bench "3" $ whnf fileCheck "acounter3.txt",
            bench "4" $ whnf fileCheck "acounter4.txt",
            bench "5" $ whnf fileCheck "acounter5.txt",
            bench "6" $ whnf fileCheck "acounter6.txt",
            bench "7" $ whnf fileCheck "acounter7.txt",
            bench "8" $ whnf fileCheck "acounter8.txt",
            bench "9" $ whnf fileCheck "acounter9.txt",
            bench "10" $ whnf fileCheck "acounter10.txt",
            bench "30" $ whnf fileCheck "acounter30.txt"
         ]
      ]
