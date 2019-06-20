module Main where
    import ModelCheck
    import System.Directory
    import System.Environment
    
    main :: IO [Bool]
    main = do
                args <- getArgs
                d <- getCurrentDirectory
                fileCheck $ d ++  "/test/testcodes/fair/H/fairH" ++ head args ++ ".txt"