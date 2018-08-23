import DataTypes
import SMVParser
import DataTypesOps
import SemanticCheck
import Data.List
import Data.HasCacBDD
import Criterion.Main

fib :: Int -> Int
fib m | m < 0        = error "Numero negativo"
      | otherwise    = go m
      where
         go 0 = 0
         go 1 = 1
         go n = go (n-1) + go (n-2)
      
main = defaultMain [
  bgroup "fib" [ bench "1"  $ whnf fib 1
               , bench "5"  $ whnf fib 5
               , bench "9"  $ whnf fib 9
               , bench "11" $ whnf fib 11
               , bench "11" $ whnf fib 50
               ]
  ]



