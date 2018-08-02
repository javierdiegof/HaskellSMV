module CounterGenerator(
   genProgram
)
where
import Data.Char

genProgram :: Int -> IO()
genProgram nvars =   let
                        var   = genVar      (nvars - 1)
                        init  = genInit     (nvars - 1)
                        trans = genTrans    (nvars - 1)
                        ctls  = genCTLSpec  (nvars - 1)
                      in
                        writeFile ("acounter" ++ show(nvars) ++ ".txt") (var ++ init ++ trans ++ ctls) 






----------------------------------- Funciones para generar los elementos del programa (Inicio) --------------------
genVar :: Int -> String
genVar x =  let 
               var = concat (map addSemi (genStringList x x))  
             in 
               "VAR" ++ var ++ "\n"
genInit :: Int -> String
genInit x =   let
               expr = andList (map addNeg (genStringList x x))
             in
               "INIT\n   " ++ expr ++ ";\n"

genTrans :: Int -> String
genTrans x = "TRANS\n   " ++ andList [paren (genTrans1 y x) | y <- [0 .. x]] ++ ";\n"


genCTLSpec :: Int -> String
genCTLSpec x = "CTLSPEC\n   " ++ "AG" ++ paren("AF" ++ paren (andList (genStringList x x))) ++ ";\n"

genTrans1 :: Int -> Int -> String
genTrans1 0 max = let
                     code = genString 0 max 
                   in
                     (nextExpr code) `binXOR` code 
         
genTrans1 x max = let
                     code = genString x max
                   in 
                     (nextExpr code) `binIFF` paren (code `binXOR` paren (andList (genStringList (x-1) max)))
----------------------------------- Funciones para generar los elementos del programa (Fin) ----------------------------------------------                     
--------------------------------------- Funciones Auxiliares para la generacion de la secuencia (Inicio) ---------------------------------
genStringList :: Int -> Int -> [String]
genStringList val max = [genString y max | y <- [0 .. val]]


genString :: Int -> Int -> String
genString val max =  let
                        arr      = decompose val
                        digits   = numDigits max
                        norm     = normalize arr digits
                      in
                        convertString norm


normalize :: [Int] -> Int -> [Int]
normalize code digits = let
                           nd = length code
                         in
                           if (digits > nd)
                           then (replicate (digits-nd) (0)) ++ code 
                           else code
                     
convertString :: [Int] -> [Char]
convertString xs = foldr numString "" xs


numString :: Int -> String -> String
numString val str =  let
                        char = chr(ord('a') + val)
                      in
                        char : str

decompose :: Int -> [Int]
decompose num = let 
                  x = num `div` 26
                in
                  if (x == 0)
                  then [num `mod` 26] 
                  else decompose x ++ [num `mod` 26] 


numDigits :: Int -> Int 
numDigits num = numDigits1 num 1

numDigits1 :: Int -> Int -> Int
numDigits1 num dig = if (num >= 26^dig)
                        then numDigits1 num (dig+1)
                        else dig
--------------------------------------- Funciones Auxiliares para la generacion de la secuencia (Fin) -----------------------------------
--------------------------------------- Funciones Auxiliares para generar el codigo (Inicio)          -----------------------------------




--------------------------------------- Funciones Auxiliares para generar el codigo (Fin)             -----------------------------------
addSemi :: String -> String
addSemi str = "\n   " ++ str ++ ";"

addNeg :: String -> String
addNeg str = "!" ++ str

andList :: [String] -> String
andList [x] = x
andList (x:xs) = x ++ " & " ++ andList xs

binXOR :: String -> String -> String
binXOR s1 s2 = s1 ++ " xor " ++ s2

binIFF :: String -> String -> String
binIFF s1 s2 = s1 ++ " <-> " ++ s2 

nextExpr :: String -> String
nextExpr str = "next(" ++ str ++ ")"

paren :: String -> String
paren x = "(" ++ x ++ ")"