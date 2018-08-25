import Data.Char
import Data.List

-- Este código genera programas de registro de corrimiento universal arbitrariamente largos
genProgram :: Int -> IO()
genProgram val =  let
                     mod  = "MODULE main\n"
                     var   = genVar val
                     ivar  = genIVar val
                     init  = genInit val
                     trans = genTrans val
                     spec  = genSpec val
                     strf = mod ++ var ++ ivar ++ init ++ trans ++ spec
                   in 
                     writeFile ("shiftG/shiftG" ++ show(val) ++ ".smv") strf


genVar :: Int -> String
genVar val =   let
                  genl = genList (val)
                  varl = take val genl
                  s0var   = head $ drop (2*val) genl
                  s1var   = head $ drop (2*val + 1) genl
                  clrvar  = last genl
                  vardl = varl ++ [s0var] ++ [s1var] ++ [clrvar]
                in
                  "VAR\n   " ++ intercalate "   : boolean;\n   " vardl ++ "   : boolean;\n"


genIVar :: Int -> String
genIVar val =   let
                  genl = genList (val)
                  ivarl = drop val (take (2*val) genl)
                  shiftlv  = head $ drop (2*val + 2) genl
                  shiftrv  = head $ drop (2*val + 3) genl
                  ivardl = ivarl ++ [shiftlv] ++ [shiftrv]
                in
                  "IVAR\n   " ++ intercalate "   : boolean;\n   " ivardl ++ "   : boolean;\n"
               
genInit :: Int -> String
genInit val =  let
                  genl = genList (val)
                  varl = take val genl
                  negl = map ("!" ++) (varl)
                  negp = intercalate " & " negl
                in
                  "INIT\n   " ++ negp ++ ";\n"

genTrans :: Int -> String 
genTrans val = let
                  stay     = genStay val     
                  shiftr   = genShiftR val   
                  shiftl   = genShiftL val   
                  loadp    = genLoadP val    
                  clear    = genClr val      
                in
                  "TRANS\n   " ++ stay ++ " &\n   " ++ shiftr ++ "&\n   " ++ shiftl ++ " &\n   " ++ loadp ++ " &\n   " ++ clear ++ "\n"

genSpec :: Int -> String
genSpec val =  let
                  clear = genClearSpec val
                  stay  = genStaySpec val
                  rspec = genSRSpec val
                  lspec = genSLSpec val 
                  parSpec= genParSpec val
                in
                  stay ++ rspec ++ lspec ++ parSpec ++ clear 

------------------------------ Funciones utilizadas para generar CTLSPEC -------------------------
genClearSpec :: Int -> String
genClearSpec val =   let
                        genl     = genList val
                        varl     = take val genl
                        negvar   = map printNeg varl
                        negand   = "AX" ++ printParen (intercalate " & " negvar)
                        clrvar   = last genl
                        implication = printParen (printImply clrvar negand)
                      in
                        "CTLSPEC\n   " ++ "AG" ++ implication ++ ";\n"

genStaySpec :: Int -> String
genStaySpec val = let
                     genl     = genList val
                     varl     = take val genl
                     clrvarn  = printNeg (last genl)
                     s0varn   = printNeg (head $ drop (2*val) genl)
                     s1varn   = printNeg (head $ drop (2*val + 1) genl)
                     axvarl   = map ("AX" ++ ) (map printParen varl)
                     future   = genEqual varl axvarl
                     anteced  = printParen $ s1varn ++ " & " ++ s0varn ++ " & " ++ clrvarn
                   in
                     "CTLSPEC\n   " ++ "AG" ++ printParen (printImply anteced future) ++ ";\n"

genSRSpec :: Int -> String
genSRSpec val =   let
                     genl     = genList val
                     varl     = take val genl
                     clrvarn  = printNeg (last genl)
                     firstvar = head genl
                     firstvarn= printNeg $ head genl
                     s0var    = " " ++ (head $ drop (2*val) genl)
                     s1varn   = printNeg (head $ drop (2*val + 1) genl)
                     anteced  = printParen (s1varn ++ " & " ++ s0var ++ " & " ++ clrvarn)
                     consec   = printParen ("EX" ++ (printParen firstvar) ++ " & " ++ "EX" ++ (printParen firstvarn))
                     impl     = printImply anteced consec
                   in
                     "CTLSPEC\n   " ++ "AG" ++ printParen (impl) ++ ";\n"

genSLSpec :: Int -> String
genSLSpec val =   let
                     genl     = genList val
                     varl     = take val genl
                     clrvarn  = printNeg (last genl)
                     lastvar  = head $ drop (val - 1) genl
                     lastvarn = printNeg lastvar
                     s0varn   = printNeg(head $ drop (2*val) genl)
                     s1var    = head $ drop (2*val + 1) genl
                     anteced  = printParen (s1var ++ " & " ++ s0varn ++ " & " ++ clrvarn)
                     consec   = printParen ("EX" ++ (printParen lastvar) ++ " & " ++ "EX" ++ (printParen lastvarn))
                     impl     = printImply anteced consec
                   in
                     "CTLSPEC\n   " ++ "AG" ++ printParen (impl) ++ ";\n"

genParSpec :: Int -> String
genParSpec val =   let
                     genl     = genList val
                     varl     = take val genl
                     nvarl    = map ("!" ++) varl
                     exvar    = map ("EX" ++) (map (printParen) (varl ++ nvarl))
                     consec   = printParen (intercalate " & " exvar)
                     clrvarn  = printNeg (last genl)
                     s0var    = head $ drop (2*val) genl
                     s1var    = head $ drop (2*val + 1) genl
                     anteced  = printParen (s1var ++ " & " ++ s0var ++ " & " ++ clrvarn)
                     impl     = printImply anteced consec
                   in
                     "CTLSPEC\n   " ++ "AG" ++ printParen (impl) ++ ";\n"
--------------------------------------------------------------------------------------------------





------------------------------ Funciones utilizadas para generar TRANS -------------------------
genStay :: Int -> String
genStay val =  let
                  genl     = genList val
                  varl     = take val genl
                  clrvarn  = printNeg (last genl)
                  s0varn   = printNeg (head $ drop (2*val) genl)
                  s1varn   = printNeg (head $ drop (2*val + 1) genl)
                  future   = genFuture varl varl 
                in
                  printParen (printImply (printParen $  s1varn ++ " & " ++  s0varn ++ " & " ++ clrvarn) future)

genShiftR :: Int -> String
genShiftR val = let
                  genl     = genList val
                  varl     = take val genl
                  clrvarn  = printNeg (last genl)
                  s0var    = " " ++ (head $ drop (2*val) genl)
                  s1varn   = printNeg (head $ drop (2*val + 1) genl)
                  shiftlv  = head $ drop (2*val + 2) genl
                  varlleft = shiftlv : (take (val-1) varl)
                  future   = genFuture varl varlleft
                 in 
                  printParen (printImply (printParen (s1varn ++ " & " ++  s0var ++ " & " ++ clrvarn)) future)

genShiftL :: Int -> String
genShiftL val = let
                  genl     = genList val
                  varl     = take val genl
                  clrvarn  = printNeg (last genl)
                  s0varn   = printNeg (head $ drop (2*val) genl)
                  s1var    = " " ++ (head $ drop (2*val + 1) genl)
                  shiftrv  = head $ drop (2*val + 3) genl
                  varlleft = (drop 1 varl) ++ [shiftrv]
                  future   = genFuture varl varlleft
                 in 
                  printParen (printImply (printParen (s1var ++ " & " ++ s0varn  ++ " & " ++ clrvarn)) future)

   
genLoadP :: Int -> String
genLoadP val = let
                  genl      = genList val
                  varl     = take val genl
                  pvarl    = drop val (take (val*2) genl)
                  clrvarn  = printNeg (last genl)
                  s0var    =" " ++ (head $ drop (2*val) genl)
                  s1var    = " " ++ (head $ drop (2*val + 1) genl)
                  future   = genFuture varl pvarl
                 in 
                  printParen (printImply (printParen (s1var ++ " & " ++ s0var  ++ " & " ++ clrvarn)) future)
                  


genClr :: Int -> String
genClr val =   let
                  genl     = genList val
                  varl     = take val genl
                  clrvar   = last genl
                  falsel   = replicate val "FALSE" 
                  future   = genFuture varl falsel
                in
                  printParen (printImply clrvar future)



-- ["a", "b", "c"], ["li", "a", "b"] -> ((next(a) <-> li) & (next(b) <-> a) & (next(c) <-> b))
genFuture :: [String] -> [String] -> String
genFuture next vals =   let 
                           nextn     = map printNext next
                           dimplies  = zipWith (printDImply) nextn vals
                           dimpparen = map printParen dimplies 
                         in
                           printParen (intercalate " & " dimpparen)

genEqual :: [String] -> [String] -> String
genEqual next vals =   let 
                           dimplies  = zipWith (printDImply) next vals
                           dimpparen = map printParen dimplies 
                        in
                           printParen (intercalate " & " dimpparen)

-----------------------------------------------------------------------------------------------
printNext :: String -> String
printNext str = "next(" ++ str ++ ")"

printNeg :: String -> String
printNeg str = "!" ++ str 

printParen :: String -> String
printParen str = "(" ++ str ++ ")"

printDImply :: String -> String -> String
printDImply str1 str2 = str1 ++ " <-> " ++ str2

printImply :: String -> String -> String
printImply str1 str2 = str1 ++ " -> " ++ str2

printXOR :: String -> String -> String
printXOR str1 str2 = str1 ++ " xor " ++ str2

printAnd :: String -> String -> String
printAnd str1 str2 = str1 ++ " & " ++ str2




------------------------------------------------------------------------------
genList :: Int -> [String]
genList valmax =  let
                     numdig = numDigits valmax
                  in
                     [genDig val numdig | val <- [0 .. 2*valmax + 4]]


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