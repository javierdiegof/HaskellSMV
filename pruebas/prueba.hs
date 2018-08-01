import SemanticCheck
import Data.HasCacBDD
import Criterion.Main
import CounterGenerator

outputF :: String -> IO()
outputF str = fileCheckOutput str

main = defaultMain [
         bgroup "counter" [
            bench "3" $ whnf fileCheck "counter3.txt",
            bench "4" $ whnf fileCheck "counter4.txt",
            bench "5" $ whnf fileCheck "counter5.txt",
            bench "6" $ whnf fileCheck "counter6.txt",
            bench "7" $ whnf fileCheck "counter7.txt",
            bench "8" $ whnf fileCheck "counter8.txt",
            bench "9" $ whnf fileCheck "counter9.txt",
            bench "10" $ whnf fileCheck "counter10.txt",
            bench "11" $ whnf fileCheck "counter11.txt",
            bench "12" $ whnf fileCheck "counter12.txt",
            bench "13" $ whnf fileCheck "counter13.txt"
         ]
      ]
