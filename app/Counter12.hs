module Main where
import ModelCheck

-- Absolute paths needed for performace testing, will delete afterwards
main :: IO [Bool]
main = fileCheck "/home/javier/Desktop/Escuela/Carrera/Tesis/Codigos/SMV-Haskell/test/testcodes/counter/H/counterH12.txt"
   