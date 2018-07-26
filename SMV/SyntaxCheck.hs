module SyntaxCheck(
   syntaxChecking,
   extractVarSet
)
where 
import SMVParser
import DataTypes
import Data.Set as Set
import Debug.Trace as Debug

syntaxChecking :: String -> IO(Program)
syntaxChecking program = do 
                           x <- parseFile program
                           putStrLn $ show $ varCheck x
                           let 
                              transform = transformPENF x
                            in  
                              return transform


-- Toma un programa y transforma la formula CTL dentro de el a Forma Normal Existencial (ENF, en ingles)
transformPENF :: Program -> Program
transformPENF (Program a b c (CTLS ctlf) fair) =  let
                                                enf =  transformFNEF ctlf
                                             in
                                                Program a b c (CTLS enf) fair

-- Transforma una formula CTL a su Forma Normal Existencial
-- Teorema: Para toda formula CTL existe una formula CTL en ENF.
transformFNEF :: CTLF -> CTLF
transformFNEF (CCUnary EF f)           = CCBinary EU (CConst TRUE) (transformFNEF f)                              -- EF(F) = E[TRUE U F]
transformFNEF (CCUnary AG f)           = CBUnary Not (CCBinary EU (CConst TRUE) (CBUnary Not (transformFNEF f)))  -- AG(F) = !E[TRUE U !F]
transformFNEF (CCUnary AX f)           = CBUnary Not (CCUnary  EX (CBUnary Not (transformFNEF f)))                -- AX(F) = !EX(!F)
transformFNEF (CCUnary AF f)           = CBUnary Not (CCUnary  EG (CBUnary Not (transformFNEF f)))                -- AF(W) = !EG(!W)
transformFNEF (CCBinary AU f1 f2)      =  let
                                             not1  = CBUnary Not (transformFNEF f1)
                                             not2  = CBUnary Not (transformFNEF f2)
                                             lr    = CBBinary And not1 not2
                                             l     = CBUnary Not (CCBinary EU not2 lr)
                                             r     = CBUnary Not (CCUnary EG not2)
                                          in
                                             CBBinary And l r                          -- A[X U Y] = !E[!Y U (!X & !Y)] & !EG(!Y)

transformFNEF (CCUnary op f)           = CCUnary op (transformFNEF f)                  -- Operador basico, se analiza la subexpresion (EG,EX)
transformFNEF (CVariable var)          = CVariable var                                 -- Las variables no se transforman
transformFNEF (CConst const)           = CConst const                                  -- Las constantes no se transforman
transformFNEF (CBUnary bunop f)        = CBUnary bunop (transformFNEF f)               -- En NOT, se analiza la subexpresion
transformFNEF (CBBinary bbinop f1 f2)  =  let                                          -- En booleanas, solamente se analizan las subexpresiones 
                                             tf1 = transformFNEF f1             
                                             tf2 = transformFNEF f2
                                          in
                                             CBBinary bbinop tf1 tf2

transformFNEF (CCBinary bop f1 f2)     =  let                                          -- Operador basico, se analizan las subexpresiones (EU)
                                             tf1 = transformFNEF f1
                                             tf2 = transformFNEF f2
                                          in
                                             CCBinary bop tf1 tf2

varCheck :: Program -> String 
varCheck (Program vars init trans ctls fair) =  let 
                                                   varset   = extractVarSet vars -- Check
                                                   initc    = checkInitU init varset
                                                   transc   = checkTransU trans varset
                                                   ctlc     = checkCTLU ctls varset
                                                   fairc    = checkFairU fair varset
                                                 in
                                                   if initc == False then
                                                      error "Error en las variables de INIT"
                                                   else
                                                      if transc == False then
                                                         error "Error en las variables de TRANS"
                                                      else
                                                         if ctlc == False then
                                                            error "Error en las variables de CTLSPEC"
                                                         else
                                                            if fairc == False then
                                                               error "Error en las variables de FAIRNESS"
                                                            else
                                                               "Primer chequeo exitoso"

                                                         
                                                

-- Toma un programa y extrae las variables declaradas, las guarda en un Set
-- Data.Sequence permite realizar operaciones de busqueda en tiempo logaritmico
extractVarSet :: VarS -> Set Variable
extractVarSet (VarS x) =  case checkVarS x (Just Set.empty) of
                              Just x -> x
                              Nothing -> error "Error en declaracion de variables"

checkVarS :: [Variable] -> Maybe (Set Variable) -> Maybe (Set Variable)
checkVarS [] s = s
checkVarS (x:xs) Nothing   = Nothing
checkVarS (x:xs) (Just s)  = case member x s of
                                 True -> Nothing
                                 False -> checkVarS xs (Just(insert x s))

checkCTLU :: CTLS -> Set Variable -> Bool
checkCTLU (CTLS ctlf) set = case (checkCTLS ctlf set) of
                                 True -> True
                                 False -> False

checkCTLS :: CTLF -> Set Variable -> Bool
checkCTLS (CConst _) _                 = True
checkCTLS (CVariable var) set          = member var set
checkCTLS (CBUnary _ exp) set          = checkCTLS exp set
checkCTLS (CCUnary _ exp) set          = checkCTLS exp set
checkCTLS (CBBinary _ exp1 exp2) set   = (checkCTLS exp1 set) && (checkCTLS exp2 set)
checkCTLS (CCBinary _ exp1 exp2) set   = (checkCTLS exp1 set) && (checkCTLS exp2 set)


-- Desenvuelve el programa y extrae la lista de funciones en las transiciones
checkTransU :: Trans -> Set Variable -> Bool
checkTransU (Trans xs) set = checkTransM xs set

-- Verifica que todas las variables proposicionales que se encuentran en las formulas de TRANS hayan sido declaradas
checkTransM :: [BNext] -> Set Variable -> Bool
checkTransM []     set  = True
checkTransM (x:xs) set  = case checkTransS x set of
                              True ->  checkTransM xs set
                              False -> False

-- Analiza que las proposiciones de las expresiones Next se hayan analizado
checkTransS :: BNext -> Set Variable -> Bool
checkTransS (NConst _) _                     = True
checkTransS (NSVariable var)        set      = member var set
checkTransS (NNVariable var)        set      = member var set
checkTransS (NUnary _ exp)          set      = checkTransS exp set
checkTransS (NBinary _ exp1 exp2)   set      = (checkTransS exp1 set) && (checkTransS exp2 set)


-- Verifica que todas las variables proposicionales que se encuentran en las formulas de INIT hayan sido declaradas
checkInitU :: Init -> Set Variable -> Bool
checkInitU (Init xs) set =  checkSimpleL xs set

checkSimpleL :: [BSimple] -> Set Variable -> Bool
checkSimpleL [] set        = True
checkSimpleL (x:xs) set    = case checkSimpleE x set of
                              True -> checkSimpleL xs set
                              False -> False

-- Verifica que todas las variables proposicionales en una sola formula hayan sido declaradas   
checkSimpleE :: BSimple -> Set Variable -> Bool
checkSimpleE (SConst   _) _             = True
checkSimpleE (SVariable var)  set       = member var set              -- Se verifica directamente si se encuentra en el set
checkSimpleE (SUnary Not exp)  set      = checkSimpleE exp set           -- Se quita el Not y se analiza la subexpresiones
checkSimpleE (SBinary _ exp1 exp2)  set = (checkSimpleE exp1 set) && (checkSimpleE exp2 set)     -- Se quita el Not y se analiza la subexpresiones



checkFairU :: Maybe Fair -> Set Variable -> Bool 
checkFairU Nothing   _           = True
checkFairU (Just (Fair xs)) set  = checkSimpleL xs set

