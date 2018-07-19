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
                        vars      = extractVarsList x
                        initBDD   = initSynthesis x vars
                        transBDD  = transSynthesis x vars
                        res = case (checkFair x) of
                                 False  -> uFormulaCheck x transBDD vars   -- Verificador sin Fairness
                                 True   -> fFormulaCheck x transBDD vars   -- Verificador con Fairness
                     print transBDD
                     return ()


     
initSynthesis :: Program -> [Variable] -> Bdd
initSynthesis (Program _ (Init xs) _ _ _) vars = synthSimpleList xs vars

synthSimpleList :: [BSimple] -> [Variable] -> Bdd
synthSimpleList [] _          = top
synthSimpleList (x:xs) vars   = con (synthOneSimple x vars) (synthSimpleList xs vars)

synthOneSimple :: BSimple -> [Variable] -> Bdd
synthOneSimple (SConst const) _  =  case const of
                                       TRUE        ->  top
                                       FALSE       ->  bot

synthOneSimple (SVariable cur) vars	= case (cur `elemIndex` vars) of
                                          Just num	-> var $ num*2
                                          Nothing	-> error "Variable no encontrada"

synthOneSimple (SUnary Not f) vars	= neg $ synthOneSimple f vars

synthOneSimple (SBinary bop f1 f2) vars = case bop of
                                             And 	-> (synthOneSimple f1 vars) `con` (synthOneSimple f2 vars)
                                             Or 	-> (synthOneSimple f1 vars) `dis` (synthOneSimple f2 vars)
                                             If 	-> (synthOneSimple f1 vars) `imp` (synthOneSimple f2 vars)
                                             Iff 	-> (synthOneSimple f1 vars) `equ` (synthOneSimple f2 vars)

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
transSynthesis (Program _ _ (Trans list) _ _) varL = synthTransList list varL

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

synthOneTrans (NBinary bop f1 f2)  vars  =   case bop of
                                                And        -> (synthOneTrans f1 vars) `con` (synthOneTrans f2 vars)
                                                Or         -> (synthOneTrans f1 vars) `dis` (synthOneTrans f2 vars)
                                                If         -> (synthOneTrans f1 vars) `imp` (synthOneTrans f2 vars)
                                                Iff        -> (synthOneTrans f1 vars) `equ` (synthOneTrans f2 vars)
----------------- Funciones de sintesis la relacion de transicion (fin)--------------------------------------------

----------------- Funciones de verificacion de la formula CTL sin Fairness -----------------------------------------
uFormulaCheck :: Program -> Bdd -> [Variable] -> Bdd
uFormulaCheck (Program _ _ _ (CTLS ctlf) _) trans vars = uSubCheck ctlf trans vars

uSubCheck :: CTLF -> Bdd -> [Variable] -> Bdd
uSubCheck  (CConst const) _  _                  =  case const of
                                                      TRUE        -> top
                                                      FALSE       -> bot
uSubCheck  (CVariable cur) _ vars               =  case (cur `elemIndex` vars) of
                                                      Just num    -> var $ num*2
                                                      Nothing     -> error "Variable no encontrada"

uSubCheck (CBUnary Not ctlf) trans vars        =  neg $ uSubCheck ctlf trans vars

uSubCheck (CBBinary bbinop f1 f2) trans vars   =  case bbinop of
                                                         And      -> (uSubCheck f1 trans vars) `con` (uSubCheck f2 trans vars)
                                                         Or       -> (uSubCheck f1 trans vars) `dis` (uSubCheck f2 trans vars)
                                                         If       -> (uSubCheck f1 trans vars) `imp` (uSubCheck f2 trans vars)
                                                         Iff      -> (uSubCheck f1 trans vars) `equ` (uSubCheck f2 trans vars)
uSubCheck (CCUnary cunop cltf)  trans vars     =      let 
                                                         sub = uSubCheck cltf trans vars
                                                       in
                                                         case cunop of
                                                            EX    -> satEX sub trans 
                                                            EG    -> satEG sub trans 
                                                            _     -> error "Formula a verificar no esta en ENF"
uSubCheck (CCBinary cbinop f1 f2) trans vars   =      let
                                                         sub1 = uSubCheck f1 trans vars
                                                         sub2 = uSubCheck f2 trans vars
                                                       in
                                                         case cbinop of
                                                            EU    -> satEU sub1 sub2 trans
                                                            _     -> error "Formula a verificar no esta en ENF"
----------------- Funciones de verificacion de la formula CTL sin Fairness (fin)-----------------------------------------
----------------- Funciones de verificacion de la formula CTL con Fairness -----------------------------------------
fFormulaCheck :: Program -> Bdd -> [Variable] -> Bdd
fFormulaCheck (Program _ _ _ (CTLS ctlf) (Just fair)) trans vars =   let
                                                                        fairs = synthFairness fair vars
                                                                      in
                                                                        fSubCheck ctlf trans fairs vars
                                                                        

fSubCheck :: CTLF -> Bdd -> [Bdd]-> [Variable] -> Bdd
fSubCheck (CConst const) _ _ _      =  case const of
                                          TRUE  -> top
                                          FALSE -> bot
fSubCheck (CVariable cur) _ _ vars  =  case (cur `elemIndex` vars) of
                                          Just num -> var $ num*2
                                          Nothing  -> error "Variable no encontrada"

fSubCheck (CBUnary Not ctlf) trans fairs vars = neg $ fSubCheck ctlf trans fairs vars

fSubCheck (CBBinary bbinop f1 f2) trans fairs vars = case bbinop of
                                                         And   -> (fSubCheck f1 trans fairs vars) `con` (fSubCheck f2 trans fairs vars)
                                                         Or    -> (fSubCheck f1 trans fairs vars) `dis` (fSubCheck f2 trans fairs vars)
                                                         If    -> (fSubCheck f1 trans fairs vars) `imp` (fSubCheck f2 trans fairs vars)
                                                         Iff   -> (fSubCheck f1 trans fairs vars) `equ` (fSubCheck f2 trans fairs vars)
fSubCheck (CCUnary EX ctlf) trans fairs vars =  let
                                                   sub = fSubCheck ctlf trans fairs vars
                                                   statefair = fairStates trans fairs
                                                 in
                                                   satEX (sub `con` statefair) trans
fSubCheck (CCBinary cbinop f1 f2) trans fairs vars =  let
                                                         sub1 = fSubCheck f1 trans fairs vars
                                                         sub2 = fSubCheck f2 trans fairs vars
                                                         statefair = fairStates trans fairs
                                                       in
                                                         satEU sub1 (sub2 `con` statefair) trans

fSubCheck (CCUnary EG ctlf) trans fairs vars =  let
                                                   sub = fSubCheck ctlf trans fairs vars
                                                 in
                                                   satEG sub trans 




fairStates :: Bdd -> [Bdd] -> Bdd
fairStates trans xs = satEGFair top trans xs 

satEGFair :: Bdd -> Bdd -> [Bdd] -> Bdd
satEGFair subf trans xs = satEGFairA subf subf trans xs


satEGFairA :: Bdd -> Bdd -> Bdd -> [Bdd] -> Bdd
satEGFairA subf fixed trans xs =  let
                                    bigconj = satEGFairCon subf trans fixed xs
                                    fixedp = subf `con` bigconj
                                  in
                                    if (fixedp `equ` fixed) == top
                                       then fixed
                                       else satEGFairA subf fixedp trans xs


-- Realiza la conjuncion iterada de 
satEGFairCon :: Bdd -> Bdd -> Bdd -> [Bdd]  -> Bdd
satEGFairCon subf trans fixed []  = top
satEGFairCon subf trans fixed (x:xs)  = let
                                          eu = satEU subf (fixed `con` x) trans
                                          ex = satEX eu trans
                                        in
                                          ex `con` (satEGFairCon subf trans fixed xs)

synthFairness :: Fair -> [Variable] -> [Bdd]
synthFairness (Fair xs) vars = synthFairList xs vars


synthFairList :: [BSimple] -> [Variable] -> [Bdd]
synthFairList [] _ = []
synthFairList (x:xs) vars = (synthOneSimple x vars) : synthFairList xs vars








----------------- Funciones de verificacion de la formula CTL con Fairness (fin) -----------------------------------------




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
extractVarsList (Program (VarS vars) _ _ _ _) = sort vars

-- Funcion que analiza si hay una especificacion de Fairness en el programa o no
checkFair :: Program -> Bool
checkFair (Program _ _ _ _ Nothing) = False
checkFair (Program _ _ _ _ (Just _))  = True


caller = semantic11 "prueba.txt"

{-
   Algunas pruebas:
   cltf = CCUnary EG(CBBinary And (CVariable(Variable "req")) (CBUnary Not (CVariable(Variable "status"))))
   bdd = var 0 `con` neg(var 2)
   l = [Variable "req", Variable "status"]

-}