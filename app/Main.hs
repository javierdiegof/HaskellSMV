module Main where
import SemanticCheck

main :: IO [Bool]
main = fileCheck "test/testcodes/counter/H/counterH3.txt"
   