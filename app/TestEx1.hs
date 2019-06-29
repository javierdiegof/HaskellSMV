module Main where
import ModelCheck
import System.Environment
import Text.Printf
import System.Directory

main :: IO [Bool]
main = do
            args <- getArgs
            d <- getCurrentDirectory
            fileCheck $ d ++  "/test/testcodes/counter/T/counter" ++ head args ++ ".txt"