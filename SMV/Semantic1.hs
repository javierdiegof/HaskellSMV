import SyntaxCheck
import DataTypes
import Data.List
import Data.HasCacBDD
import Data.Maybe

semantic11 file = do
                     x <- syntaxChecking file
                     putStrLn $ "El programa es:  " ++ show x
                     let   
                        vars     = extractVarsList x
                        transBDD = transSynthesis x vars
                        fBDD     = formulaSynthesis x transBDD vars
                     print transBDD
                     return ()


     

{-
   Nota: El ordenamiento de las variables será por orden lexicográfico, y de manera intercalada.
   - Primer variable convencional, primer variable next, segunda variable convencional, segunda variable next, ...
   - Unicamente se guardan las listas con las variables convencionales, los indices next se calculan.
   - pos(variable_normal) = pos(arreglo)*2
   - pos(variable_next) = (pos(arreglo)*2)+1
   - Comenzando desde cero.
-}

----------------- Funciones de sintesis la relacion de transicion ------------------------------------------------
-- Convierte la lista de expresiones en TRANS a un BDD que representa a la funcion de transicion
transSynthesis :: Program -> [Variable] -> Bdd
transSynthesis (Program _ _ (Trans list) _) varL = synthTransList list varL

-- Hace la conjuncion de las expresiones en la lista
synthTransList :: [BNext] -> [Variable] -> Bdd
synthTransList [] _ = top
synthTransList (x:xs) varL = con (synthOneTrans x varL) (synthTransList xs varL) -- Se consideran restricciones conjuntas 

-- Convierte uno de los renglones en TRANS a un bdd
synthOneTrans :: BNext -> [Variable] -> Bdd
synthOneTrans (NConst const)      _     = case const of
                                             TRUE        ->  top
                                             FALSE       ->  bot
synthOneTrans (NSVariable cur)   vars  = case (cur `elemIndex` vars) of
                                             Just num    -> var $ num*2   
                                             Nothing     -> error "Variable no encontrada"

synthOneTrans (NNVariable next)   vars  = case (next `elemIndex` vars) of
                                             Just num    -> var $ num*2+1   
                                             Nothing     -> error "Variable no encontrada"

synthOneTrans (NUnary Not f)  vars  = neg $ synthOneTrans f vars

synthOneTrans (NBinary op f1 f2)  vars  =   case op of
                                             And        ->  con (synthOneTrans f1 vars) (synthOneTrans f2 vars)
                                             Or         ->  dis (synthOneTrans f1 vars) (synthOneTrans f2 vars)
                                             If         ->  imp (synthOneTrans f1 vars) (synthOneTrans f2 vars)
                                             Iff        ->  equ (synthOneTrans f1 vars) (synthOneTrans f2 vars)
----------------- Funciones de sintesis la relacion de transicion (fin)--------------------------------------------

----------------- Funciones de sintesis de la formula CTL ------------------------------------------------------------
formulaSynthesis :: Program -> Bdd -> [Variable] -> Bdd
formulaSynthesis (Program _ _ _ (CTLS ctlf)) trans  vars = subfSynthesis ctlf trans vars

subfSynthesis :: CTLF -> Bdd -> [Variable] -> Bdd
subfSynthesis (CConst const) _  _                  =  case const of
                                                      TRUE        -> top
                                                      FALSE       -> bot
subfSynthesis (CVariable cur) _ vars               =  case (cur `elemIndex` vars) of
                                                      Just num    -> var $ num*2
                                                      Nothing     -> error "Variable no encontrada"
subfSynthesis (CBUnary Not ctlf) trans vars        =  neg $ subfSynthesis ctlf trans vars
subfSynthesis (CBBinary bbinop f1 f2) trans vars   =  case bbinop of
                                                      And      -> con (subfSynthesis f1 trans vars) (subfSynthesis f2 trans vars)
                                                      Or       -> dis (subfSynthesis f1 trans vars) (subfSynthesis f2 trans vars)
                                                      If       -> imp (subfSynthesis f1 trans vars) (subfSynthesis f2 trans vars)
                                                      Iff      -> equ (subfSynthesis f1 trans vars) (subfSynthesis f2 trans vars)
subfSynthesis (CCUnary cunop cltf)  trans vars     =  let 
                                                      sub = subfSynthesis cltf trans vars
                                                       in
                                                         case cunop of
                                                            EX    -> satEX sub trans vars
                                                            EG    -> satEG sub trans vars
                                                            _     -> error "Formula a verificar no esta en ENF"


satEX :: Bdd -> Bdd -> [Variable] -> Bdd
satEX x y z = bot

satEG :: Bdd -> Bdd -> [Variable] -> Bdd
satEG x y z = top



----------------- Funciones de sintesis de la formula CTL (Fin) ------------------------------------------------------

extractVarsList :: Program -> [Variable]
extractVarsList (Program (VarS vars) _ _ _) = sort vars


caller = semantic11 "prueba.txt"
