import SyntaxCheck
import DataTypes
import Data.List
import Data.HasCacBDD
import Data.Maybe
import Debug.Trace

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
                                                         And      -> (subfSynthesis f1 trans vars) `con` (subfSynthesis f2 trans vars)
                                                         Or       -> (subfSynthesis f1 trans vars) `dis` (subfSynthesis f2 trans vars)
                                                         If       -> (subfSynthesis f1 trans vars) `imp` (subfSynthesis f2 trans vars)
                                                         Iff      -> (subfSynthesis f1 trans vars) `equ` (subfSynthesis f2 trans vars)
subfSynthesis (CCUnary cunop cltf)  trans vars     =  let 
                                                         sub = subfSynthesis cltf trans vars
                                                       in
                                                         case cunop of
                                                            EX    -> satEX sub trans 
                                                            EG    -> satEG sub trans 
                                                            _     -> error "Formula a verificar no esta en ENF"
subfSynthesis (CCBinary cbinop f1 f2) trans vars   =  let
                                                         sub1 = subfSynthesis f1 trans vars
                                                         sub2 = subfSynthesis f2 trans vars
                                                       in
                                                         case cbinop of
                                                            EU    -> satEU sub1 sub2 trans
                                                            _     -> error "Formula a verificar no esta en ENF"


{-
   Funcion que obtiene, dado un conjunto de estados, el conjunto de estados que se encuentran a un paso de este:
   Dado B, se obtiene EX(B)                                                  
-}
satEX :: Bdd -> Bdd -> Bdd
satEX subf trans  = let
                           nextsubf    = renameNextBdd subf -- X_B{x' <- x}
                           transnext   = con trans nextsubf -- Delta & nextsubf 
                           nextvars    = [a | a <- (allVarsOfSorted transnext), a `mod` 2 == 1] -- Todas las variables next en transnext
                         in
                           existsSet nextvars transnext



satEG :: Bdd -> Bdd -> Bdd
satEG fj trans =   let
                        aux      = satEX fj trans
                        fjp1     = aux `con` fj
                      in
                        if (fj `equ` fjp1) == top 
                           then fjp1
                           else satEG fjp1 trans

                           

satEU :: Bdd -> Bdd -> Bdd -> Bdd
satEU fj fc trans =  let
                        postb = satEX fj trans
                        term  = fc `con` postb
                        fjp1  = fj `dis` term
                      in
                        if(fj `equ` fjp1) == top
                           then fjp1
                           else satEU fjp1 fc trans


-- Recibe un Bdd y cambia todas sus variables por la siguiente f{x'<-x}
renameNextBdd :: Bdd -> Bdd
renameNextBdd orig      =  let 
                              varbdd = allVarsOfSorted orig
                              mapping = renameNextList varbdd
                            in
                              relabel mapping orig

renameNextList :: [Int] -> [(Int, Int)]
renameNextList vars = [(a,b) | a <- vars, let b = a+1]

----------------- Funciones de sintesis de la formula CTL (Fin) ------------------------------------------------------

extractVarsList :: Program -> [Variable]
extractVarsList (Program (VarS vars) _ _ _) = sort vars


caller = semantic11 "prueba.txt"

{-
   Algunas pruebas:
   cltf = CCUnary EG(CBBinary And (CVariable(Variable "req")) (CBUnary Not (CVariable(Variable "status"))))
   bdd = var 0 `con` neg(var 2)
   l = [Variable "req", Variable "status"]

-}