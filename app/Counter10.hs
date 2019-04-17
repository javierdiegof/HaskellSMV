module Main where
import SemanticCheck

-- Absolute paths needed for performace testing, will delete afterwards
main :: IO [Bool]
main = fileCheck "/home/javier/Desktop/Escuela/Carrera/Tesis/Codigos/SMV-Haskell/test/testcodes/counter/H/counterH10.txt"
   