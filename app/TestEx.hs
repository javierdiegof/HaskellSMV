import System.Environment
import Text.Printf

main :: IO ()
main = do
  print $ foo 0

foo :: Int -> Int
foo x = fooSub (x+1)
  where
    fooSub x = bar (x+1)

bar :: Int -> Int
bar x = barSub (x+1)
  where
    barSub x = barSubSub (x+1)
      where
        barSubSub x = x+1