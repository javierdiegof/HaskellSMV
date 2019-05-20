module Main where
import ModelCheck

main :: IO [Bool]
main = fileCheck "test/testcodes/counter/H/counterH3.txt"
   