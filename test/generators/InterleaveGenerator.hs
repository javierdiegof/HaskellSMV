import Data.Char
import Data.List

genProgram :: Int -> Bool -> IO ()
genProgram val has = let
                        main     = "MODULE main\n"
                        varH     = genVarH val
                        varS     = genVarS val
                        init     = genInit val
                        trans    = genTrans val
                        spec     = genSpec val
                        programH = varH ++ init ++ trans ++ spec
                        programS = main ++ varS ++ init ++ trans ++ spec
                     in
                        if has
                        then writeFile ("../testcodes/interleave/H/interleaveH" ++ show(val) ++ ".txt") programH
                        else writeFile ("../testcodes/interleave/S/interleaveS" ++ show(val) ++ ".smv") programS

-- Este código genera programas de interleave arbitrariamente largos
genProgramH :: Int -> IO()
genProgramH val = let
                     var   = genVarH val
                     init  = genInit val
                     trans = genTrans val
                     spec  = genSpec val
                     strf  = var ++ init ++ trans ++ spec
                   in 
                     writeFile ("../testcodes/interleave/H/interleaveH" ++ show(val) ++ ".txt") strf

genProgramS :: Int -> IO()
genProgramS val = let
                     mod  = "MODULE main\n"
                     var   = genVarS val
                     init  = genInit val
                     trans = genTrans val
                     spec  = genSpec val
                     strf = mod ++ var ++ init ++ trans ++ spec
                   in 
                     writeFile ("../testcodes/interleave/S/interleaveS" ++ show(val) ++ ".smv") strf

genVarH :: Int -> String
genVarH val =  let
                  genl = genList val
                  defl1 = map ("   " ++ ) genl
                  defl2 = map (++ ";\n") defl1
                  defl3 = concat defl2
                in 
                  "VAR\n" ++ defl3

genVarS :: Int -> String
genVarS val =  let
                  genl = genList val
                  defl1 = map ("   " ++ ) genl
                  defl2 = map (++ ": boolean;\n") defl1
                  defl3 = concat defl2
               in 
                  "VAR\n" ++ defl3

genInit :: Int -> String
genInit val =  let
                  genl = genList (val)
                  negl = map ("!" ++) (init genl)
                  negp = intercalate " & " negl
                in
                  "INIT\n   " ++ negp ++ " & " ++ (last genl) ++ ";\n"

genTrans :: Int -> String 
genTrans val = let
                  term = printTerm val
                  par  = printParity val
                in
                  "TRANS\n" ++ printAnd ("   " ++ printParen term) ("\n   " ++ par ++ ";\n")

genSpec :: Int -> String
genSpec val =  let
                  parity = last (genList(val))
                  next = "AX" ++ printParen (printNeg parity)
                  paren = "AG" ++ printParen (printDImply parity next)
                in 
                  "CTLSPEC\n   " ++ paren ++ ";"


printParity :: Int -> String
printParity val = let
                     genl = genList (val)
                     lval = last genl
                     xorl = printXORL (tail (reverse genl))
                   in 
                     printParen(printDImply (printNext lval) xorl)


printXORL :: [String] -> String
printXORL []  = ""
printXORL (x:y:[]) =  printParen (printXOR y x)
printXORL (x:xs) =  printParen (printXOR (printXORL xs) x)


                  
printTerm :: Int -> String
printTerm val =   let
                     genl = genList (val-1)
                     tot = [(intercalate " & "(printTermR y genl)) | y <- [0 .. val-1]]
                     totPar = map (printParen) tot
                   in
                     intercalate " | \n    " totPar



printTermR :: Int -> [String] -> [String]
printTermR val []     = []
printTermR val (x:xs) =  if val == 0
                        then printParen(printUnit x False) : (printTermR (val-1)(xs))
                        else printParen(printUnit x True)  : (printTermR (val-1)(xs))


         
printUnit :: String -> Bool -> String
printUnit str True = printDImply (printNext str) (" " ++ str)
printUnit str False = printDImply (printNext str) (printNeg str)


printNext :: String -> String
printNext str = "next(" ++ str ++ ")"

printNeg :: String -> String
printNeg str = "!" ++ str 

printParen :: String -> String
printParen str = "(" ++ str ++ ")"

printDImply :: String -> String -> String
printDImply str1 str2 = str1 ++ " <-> " ++ str2

printXOR :: String -> String -> String
printXOR str1 str2 = str1 ++ " xor " ++ str2

printAnd :: String -> String -> String
printAnd str1 str2 = str1 ++ " & " ++ str2








------ Generacion de la lista de valores ---------------------------------
genList :: Int -> [String]
genList valmax =  let
                     numdig = numDigits valmax
                  in
                     if valmax >= 221
                     then [(genDig val numdig) | val <- [0 .. valmax+1], val /= 221]
                     else [(genDig val numdig) | val <- [0 .. valmax]]


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
                                          
                    

                

checkGreater :: [String] -> Bool
checkGreater []         = True
checkGreater [x]        = True
checkGreater (x:y:xs)   = (x <= y) && checkGreater (y:xs)
------ Generacion de la lista de valores ---------------------------------