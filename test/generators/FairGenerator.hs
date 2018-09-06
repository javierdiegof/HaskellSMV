import Data.Char
import Data.List

genProgramH :: Int -> IO ()
genProgramH val =  let
                     var   = genVarH val
                     def   = genDefine val
                     init  = genInit val
                     trans = genTrans val
                     ctls  = genCTLS val
                     fair  = genFairness val
                     strf  = var ++ def ++ init ++ trans ++ ctls ++ fair
                   in 
                     writeFile ("../testcodes/fair/H/fairH" ++ show(val) ++ ".txt") strf

                  
genProgramS :: Int -> IO ()
genProgramS val =  let
                     main  = "MODULE main\n"
                     var   = genVarS val
                     def   = genDefine val
                     init  = genInit val
                     trans = genTrans val
                     ctls  = genCTLS val
                     fair  = genFairness val
                     strf  = main ++ var ++ def ++ init ++ trans ++ ctls ++ fair
                   in 
                     writeFile ("../testcodes/fair/S/fairS" ++ show(val) ++ ".smv") strf


genVarS :: Int -> String
genVarS val =  let
                  genl    = genList (val)
                  defl1   = map ("   " ++) genl
                  defl2   = map (++ ": boolean;\n") defl1
                  str     = concat defl2 
                in
                  "VAR\n" ++ str

genVarH :: Int -> String
genVarH val =  let
                  genl    = genList (val)
                  defl1   = map ("   " ++ ) genl
                  defl2   = map ( ++ ";\n") defl1
                  str     = concat defl2
                in
                  "VAR\n" ++ str
         
genDefine :: Int -> String
genDefine val =   let
                     vars  = genList val
                     props = take val vars
                     cont  = drop val vars
                     zero  = genZero vars 
                     varsD  = genVars props cont
                   in
                     "DEFINE\n   " ++ zero  ++(intercalate "   " varsD)

genInit :: Int -> String
genInit val =  "INIT\n   z;\n"


genTrans :: Int -> String
genTrans val = let
                  vars     = genList val
                  props    = take val vars
                  ztrans   = genZTrans props
                  vartrans = genVarTrans props
               in 
                  "TRANS\n   " ++ ztrans ++ vartrans

genCTLS :: Int -> String
genCTLS val =  let
                  vars = genList val
                  props = take val vars
                  propsp = map (printParen) props
                  propsaf = map ("AF" ++ ) propsp
                in 
                  "CTLSPEC\n   " ++ intercalate (" & ") propsaf  ++ ";\n"

genFairness :: Int -> String
genFairness val = let 
                     vars = genList val
                     props = take val vars
                     propss = map (++ ";\n") props
                     propsf = map ("FAIRNESS\n   " ++ ) propss
                  in
                     concat propsf

genZTrans :: [String] -> String
genZTrans vars =  let
                     lsttrans = [(head vars) ++ show(i) | i <- [0 .. length(vars) - 1]]
                     nextm =  map (printNext) (lsttrans)
                     edge  = map ("z & "++) nextm
                     edgeparen = map printParen edge
                  in 
                     intercalate (" | ") edgeparen ++ " |\n   "

genVarTrans :: [String] -> String
genVarTrans vars =   let
                        patharr = genPathArrs vars
                        paths = [genPathStringsU x | x<- patharr]
                     in 
                        intercalate " |\n   " paths ++ ";\n"

genPathStringsU :: [String] -> String
genPathStringsU vars = let
                           paths = genPathStrings vars
                        in 
                           intercalate " | " paths



genPathStrings :: [String] -> [String]
genPathStrings (x:[]) = []
genPathStrings vars =   let  
                           vals = take 2 vars
                           now  = head vals
                           next = head (drop 1 vals)
                           nextn = printNext next
                           and   = now ++  " & " ++  nextn
                           andp  = printParen and
                        in 
                           andp : (genPathStrings (tail vars))
                           


genPathArrs :: [String] -> [[String]]
genPathArrs vars = [(genPathArr vars x) | x <- [0 .. (length vars - 1)]]


genPathArr :: [String] -> Int -> [String]
genPathArr vars val =   let
                           patharr = genPathArrR vars val
                        in 
                           patharr ++ (take 1 patharr)

genPathArrR :: [String] -> Int -> [String]
genPathArrR vars (-1) = []
genPathArrR vars x  = ((head vars) ++ show(x)) : genPathArrR (tail vars) (x-1)


genZero :: [String] -> String
genZero vars = let
                  ant  = "z := "
                  neg  = map ("!" ++ ) vars
                in
                  ant ++ intercalate " & " neg  ++ ";\n   "

genVars :: [String] -> [String] -> [String]
genVars vars cont =  let
                        n = length vars
                     in 
                        [(genVarD vars cont x y) | x <- [0 .. n-1], y <- [0 .. n - x -1]]

genVarD :: [String] -> [String] -> Int -> Int -> String
genVarD vars cont pos it =  let
                              cad_cont = genCont cont it 
                              elem     = head (drop pos vars)
                              varsneg  = map ("!" ++ ) vars
                              negvarsant = take pos varsneg
                              negvarspos = drop (pos + 1) varsneg
                              totvars    = negvarsant ++ [elem] ++ negvarspos ++ [cad_cont]
                           in 
                               elem ++ show(it) ++ " := " ++ (intercalate " & " totvars) ++ ";\n"
                        

genCont :: [String] -> Int -> String
genCont cont val =   let  
                        binrep = toBinaryU val cont
                        contstr = reverse(genCont2 (reverse cont) (reverse binrep))
                     in 
                        intercalate " & " contstr

genCont2 :: [String] -> [Int] -> [String]
genCont2 _ [] = []
genCont2 (x:xs) (y:ys) =   if y == 0
                           then ("!" ++ x) : (genCont2 xs ys)
                           else (" " ++ x) : (genCont2 xs ys)

toBinaryU :: Int -> [String] -> [Int]
toBinaryU val cont = let
                        tob = toBinary val
                      in 
                        (replicate (length(cont) - val) 0) ++ tob
                        

toBinary :: Int -> [Int]
toBinary 0 = []
toBinary n = toBinary ( n `quot` 2 ) ++ [ n `rem` 2 ]

printNext :: String -> String
printNext str = "next(" ++ str ++ ")"

printParen :: String -> String
printParen str = "(" ++ str ++ ")"


------------------------------------------------------------------------------
genList :: Int -> [String]
genList valmax =  let
                     numvars = valmax + myceil valmax
                     numdig = numDigits (numvars)
                  in
                     [genDig val numdig | val <- [0 .. numvars-1]]

myceil :: Int -> Int
myceil x =  let 
               a = (fromIntegral x :: Float)   
            in 
               ceiling (logBase 2 a)

genDig :: Int -> Int -> String
genDig val numdig =  let
                        dec = decomposeVal val numdig
                        init = initStr numdig
                      in
                        addString dec init


numDigits :: Int -> Int
numDigits num = numDigitsR num 1 

numDigitsR :: Int -> Int -> Int
numDigitsR num act = let
                        x = num `div` 26
                      in
                        if(x == 0)
                        then act
                        else numDigitsR x (act+1)


initStr :: Int -> String
initStr dig = replicate dig 'a' 

lastStr :: Int -> String
lastStr dig = replicate dig 'z' 


addString :: [Int] -> String -> String
addString dec ini = zipWith addChar dec ini


addChar :: Int -> Char -> Char
addChar inc char = chr (ord(char) + inc)



decArr :: Int -> Int -> [[Int]] 
decArr valmax size = [(decomposeVal x size) | x <- [0 .. valmax]] 

decomposeVal :: Int -> Int -> [Int]
decomposeVal val size = let
                           dec = decompose val
                           res = decomposeDigits size dec
                        in
                           res

decomposeDigits :: Int -> [Int] -> [Int]
decomposeDigits size arr = replicate (size - (length arr)) 0 ++ arr 


decompose :: Int -> [Int]
decompose val =   if val == 0
                  then []
                  else            
                     let
                        res = val `mod` 26
                        nuval = (val-res) `div` 26
                      in 
                        if(nuval >= 0)
                        then  (decompose nuval) ++ [res]
                        else []