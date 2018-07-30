module DataTypesOps(
   syntaxCheck
) where
   import DataTypes
   import SMVParser
   import qualified Data.Set as Set
   import qualified Data.Map.Strict as DMS

   -----------------------------------------------------------------------------------------------------
   -- Funciones de conversiones de modulos
   -- Desordenado a ordenado
   -- Ordenado a P
   -- Inicio
   -----------------------------------------------------------------------------------------------------
   syntaxCheck :: String -> IO (PModule)
   syntaxCheck file =  do
                           umod <- parseFile file
                           let omod    = convertModuleUO umod
                           let pmod    = convertModuleOP omod
                           let pmodE   = pModEmptyCheck pmod
                           let pmodc   = pModSyntaxCheck pmodE
                           let pmodENF = transforMPENF pmodc
                           return pmodENF 



   convertModuleUO :: UModule -> OModule
   convertModuleUO umodule = let
                              vardec      = joinVarDec      umodule
                              mdefinedec  = joinDefineDec   umodule
                              initcons    = joinInitCons    umodule
                              transcons   = joinTransCons   umodule
                              ctlspecs    = joinCTLSpecs    umodule
                              mfaircons   = joinFairCons    umodule
                            in
                              OModule vardec mdefinedec initcons transcons ctlspecs mfaircons

   
                  
   -- Debido a los problemas con define, por el momento ignoraremos la construccion
   convertModuleOP :: OModule -> PModule
   convertModuleOP (OModule vdc Nothing ics tcs ctls fcs) = PModule vdc ics tcs ctls fcs -- Si no tiene expresion DEFINE, unicamente se elimina
   convertModuleOP (OModule vdc (Just dfd) ics tcs ctls fcs) = let
                                                                  (lck, rck) = checkDef vdc dfd
                                                                in
                                                                  if not lck
                                                                  then error "macros previamente declaradas"
                                                                  else if not rck
                                                                       then error "variables no declaradas en DEFINE"
                                                                       else subsituteDef (OModule vdc (Just dfd) ics tcs ctls fcs)

   -- Funcion que se encarga de hacer la sustitucion de todas las expresiones define a los largo del programa
   subsituteDef :: OModule -> PModule
   subsituteDef (OModule vdc (Just dfd) ics tcs ctls fcs) = let
                                                               map      = defMap dfd 
                                                               newics   = subsInit ics map
                                                               newtcs   = subsTrans tcs map
                                                               newctls  = subsCTLSS ctls map
                                                            in 
                                                               case fcs of
                                                                  Nothing  -> (PModule vdc newics newtcs newctls Nothing)
                                                                  Just fs -> (PModule vdc newics newtcs newctls (Just (subsFairs fs map)))

   defMap :: DefineDec -> DMS.Map Variable BSimple
   defMap (DefineDec defs) =  let
                                 tuples = foldr defTuple [] defs
                               in
                                 DMS.fromList tuples

   defTuple :: DefineExp -> [(Variable, BSimple)] -> [(Variable, BSimple)]
   defTuple (DefineExp var simple) tuples = (var, simple) : tuples
   -- Crea un mapa cuya llave es 
   checkDef :: VarDec -> DefineDec -> (Bool, Bool)
   checkDef vdc dfd =   let
                           varset = extractVarsDec vdc
                           lck    = checkVarsDef varset dfd
                           rck    = checkSimpleDef varset dfd
                         in
                           (lck, rck) 


   -- Funcion que verifica que las variables en el lado izquierdo de las definiciones no hayan sido declaradas previamente
   checkVarsDef :: Set.Set Variable -> DefineDec -> Bool
   checkVarsDef varset dfd =  let
                              defVarSet = extractVarsDef dfd
                            in
                              if Set.null $ Set.intersection varset defVarSet
                              then True
                              else False

   -- Funcion que verifica que las variables del lado derecho de las definiciones hayan sido declaradas
   checkSimpleDef :: Set.Set Variable -> DefineDec -> Bool
   checkSimpleDef varset dfd = checkDefVars dfd varset
                              
                                                            


   pModEmptyCheck :: PModule -> PModule
   pModEmptyCheck (PModule vdc ics tcs ctls fcs) = let 
                                                      vdcE  = varDecEmpty vdc
                                                      icsE  = initConsEmpty ics
                                                      tcsE  = transConsEmpty tcs
                                                      ctlsE = ctlSpecEmpty ctls
                                                    in
                                                      if vdcE
                                                      then error "VAR esta vacio"
                                                      else if icsE 
                                                           then error "INIT vacio"
                                                           else if tcsE
                                                                then error "TRANS vacio"
                                                                else if ctlsE
                                                                     then error "CTLSPEC vacio"
                                                                     else (PModule vdc ics tcs ctls fcs)
                                                   

   pModSyntaxCheck :: PModule -> PModule
   pModSyntaxCheck (PModule vdc ics tcs ctls fcs) =   let
                                                         varset      = extractVarsDec vdc
                                                         checkinit   = checkInitVars ics varset
                                                         checktrans  = checkTransVars tcs varset
                                                         checkctl    = checkCTLSVars ctls varset
                                                         checkfair   = checkFairVars fcs varset
                                                       in
                                                         if (checkinit == False)
                                                         then error "Error en las variables de INIT"
                                                         else if (checktrans == False)
                                                               then error "Error en las variables de TRANS"
                                                               else if (checkctl == False)
                                                                     then error "Error en las variables de CTLSPEC"
                                                                     else if (checkfair == False)
                                                                        then error "Error en las variables de FAIRNESS"
                                                                        else (PModule vdc ics tcs ctls fcs)
   -----------------------------------------------------------------------------------------------------
   -- Funciones de conversiones de modulos
   -- Desordenado a ordenado
   -- Ordenado a P
   -- Fin
   -----------------------------------------------------------------------------------------------------

   -----------------------------------------------------------------------------------------------------
   -- Funciones que sustituyen las definiciones de DEFINE en el cuerpo del programa
   -- Inicio
   -----------------------------------------------------------------------------------------------------
   subsInit :: InitCons -> DMS.Map Variable BSimple -> InitCons
   subsInit (InitCons bsimple) map = InitCons $ subsSimple bsimple map

   subsTrans :: TransCons -> DMS.Map Variable BSimple -> TransCons
   subsTrans (TransCons bnext) map = TransCons $ subsNext bnext map

   subsCTLSS :: [CTLSpec] -> DMS.Map Variable BSimple -> [CTLSpec]
   subsCTLSS [] _ = []
   subsCTLSS (x:xs) map = (subsCTLSP x map) : subsCTLSS xs map

   subsFairs ::[FairCons] -> DMS.Map Variable BSimple -> [FairCons]
   subsFairs [] _ = []
   subsFairs (x:xs) map = (subsFair x map) : subsFairs xs map

   subsFair :: FairCons -> DMS.Map Variable BSimple -> FairCons
   subsFair (FairCons bsimple) map = FairCons $ subsSimple bsimple map



   subsCTLSP :: CTLSpec -> DMS.Map Variable BSimple -> CTLSpec
   subsCTLSP (CTLSpec ctlf) map = CTLSpec $ subsCTLF ctlf map

   subsCTLF :: CTLF -> DMS.Map Variable BSimple -> CTLF
   subsCTLF (CConst bconst) _             = CConst bconst 
   subsCTLF (CVariable var) map           =   case (DMS.lookup var map) of
                                                Nothing  -> CVariable var
                                                Just def -> simpleToCTLF def
   subsCTLF (CBUnary bunop ctlf) map      = CBUnary bunop (subsCTLF ctlf map)
   subsCTLF (CCUnary cunop ctlf ) map     = CCUnary cunop (subsCTLF ctlf map)
   subsCTLF (CBBinary bbinop c1 c2) map   = CBBinary bbinop (subsCTLF c1 map) (subsCTLF c2 map)
   subsCTLF (CCBinary cbinop c1 c2) map   = CCBinary cbinop (subsCTLF c1 map) (subsCTLF c2 map)  
   
   subsNext :: BNext -> DMS.Map Variable BSimple -> BNext
   subsNext (NConst bconstant) map  = NConst bconstant
   subsNext (NSVariable var)   map  =  case (DMS.lookup var map) of
                                          Nothing -> NSVariable var
                                          Just def -> simpleToNext def False
   subsNext (NNVariable var)  map   =  case (DMS.lookup var map) of
                                          Nothing -> NNVariable var
                                          Just def -> simpleToNext def True
   subsNext (NUnary bunop bnext)  map   = NUnary bunop (subsNext bnext map)
   subsNext (NBinary bbinop n1 n2) map  = NBinary bbinop (subsNext n1 map) (subsNext n2 map)


   subsSimple :: BSimple -> DMS.Map Variable BSimple -> BSimple
   subsSimple (SConst bconstant) map      = SConst bconstant 
   subsSimple (SVariable var) map         = case (DMS.lookup var map) of
                                             Nothing  -> SVariable var
                                             Just def -> def
   subsSimple (SUnary bunop bsimple) map   = SUnary bunop (subsSimple bsimple map)
   subsSimple (SBinary bbinop s1 s2) map   = SBinary bbinop (subsSimple s1 map) (subsSimple s2 map)

   -- Funcion que convierte de una expresion simple a una next
   -- Si el segundo argumento es True, todas las variables adentro de la expresion se convierten a next
   -- Si el segundo argumento es False, todas las variables adentro se mantienen sin ser next
   simpleToNext :: BSimple -> Bool -> BNext
   simpleToNext (SConst bconstant) _            = NConst bconstant
   simpleToNext (SVariable var) False           = NSVariable var
   simpleToNext (SVariable var) True            = NNVariable var
   simpleToNext (SUnary bunop bsimple) change   = NUnary bunop (simpleToNext bsimple change)
   simpleToNext (SBinary bbinop s1 s2) change   = NBinary bbinop (simpleToNext s1 change) (simpleToNext s2 change) 

   simpleToCTLF :: BSimple -> CTLF
   simpleToCTLF (SConst bconst) = CConst bconst
   simpleToCTLF (SVariable var) = CVariable var 
   simpleToCTLF (SUnary bunop simple) = CBUnary bunop (simpleToCTLF simple)
   simpleToCTLF (SBinary bbinop s1 s2) = CBBinary bbinop (simpleToCTLF s1) (simpleToCTLF s2)
   -----------------------------------------------------------------------------------------------------
   -- Funciones que sustituyen las definiciones de DEFINE en el cuerpo del programa
   -- Fin
   -----------------------------------------------------------------------------------------------------
   

   --------------------------------------------------------------------------------------------------
   --   Funciones que realizan el chequeo sintactico de los diferentes elementos en un PModule
   --   Verifican que todas las variables usadas hayan sido declaradas en Vars
   --   Inicio
   --------------------------------------------------------------------------------------------------
   checkInitVars :: InitCons  -> Set.Set Variable -> Bool
   checkInitVars (InitCons bsimple) varset = checkSimpleVars bsimple varset

   checkTransVars :: TransCons -> Set.Set Variable -> Bool
   checkTransVars (TransCons bnext) varset = checkNextVars bnext varset

   checkCTLSVars :: [CTLSpec] -> Set.Set Variable -> Bool
   checkCTLSVars [] _     = True
   checkCTLSVars ((CTLSpec x):xs) set = (checkCTLSVars1 x set) && checkCTLSVars xs set 

   checkFairVars :: (Maybe [FairCons]) -> Set.Set Variable -> Bool
   checkFairVars Nothing _    = True
   checkFairVars (Just []) _  = True
   checkFairVars (Just (x:xs)) vars =  (checkFairVars1 x vars) && (checkFairVars (Just xs) vars)

   checkDefVars :: DefineDec -> Set.Set Variable -> Bool
   checkDefVars (DefineDec [])      _  = True
   checkDefVars (DefineDec (x:xs)) vars = (checkDefExpVars x vars) && (checkDefVars (DefineDec xs) vars) 
   --------------------------------------------------------------------------------------------------
   --   Funciones que realizan el chequeo sintactico de los diferentes elementos en un PModule
   --   Verifican que todas las variables usadas hayan sido declaradas en Vars
   --   Fin
   --------------------------------------------------------------------------------------------------


   --------------------------------------------------------------------------------------------------
   --   Funciones que sustituyen expresiones DEFINE en el cuerpo de la funcion
   --   Inicio
   --------------------------------------------------------------------------------------------------

   --------------------------------------------------------------------------------------------------
   --   Funciones que sustituyen expresiones DEFINE en el cuerpo de la funcion
   --   Fin
   --------------------------------------------------------------------------------------------------

   --------------------------------------------------------------------------------------------------
   --   Funciones que verifican si alguno de los elementos en un PModule estan vacios
   --   Verifican que el modelo se pueda verificar
   --   Inicio
   --------------------------------------------------------------------------------------------------
   varDecEmpty :: VarDec -> Bool
   varDecEmpty (VarDec []) = True
   varDecEmpty _           = False


   
   initConsEmpty :: InitCons -> Bool
   initConsEmpty (InitCons (SConst TRUE)) = True
   initConsEmpty _                        = False

   transConsEmpty :: TransCons -> Bool
   transConsEmpty (TransCons (NConst TRUE))  = True
   transConsEmpty _                          = False

   ctlSpecEmpty :: [CTLSpec] -> Bool
   ctlSpecEmpty [] = True
   ctlSpecEmpty _  = False

   --------------------------------------------------------------------------------------------------
   --   Funciones que verifican si alguno de los elementos en un PModule estan vacios
   --   Verifican que el modelo se pueda verificar
   --   Fin
   --------------------------------------------------------------------------------------------------




   
   --------------------------------------------------------------------------------------------------
   --   Funciones auxiliares para la verificacion sintactica de las formulas
   --   Inicio
   --------------------------------------------------------------------------------------------------                          
   extractVarsDec :: VarDec -> Set.Set Variable
   extractVarsDec (VarDec vars) =   case checkVarS vars (Just Set.empty) of
                                       Just x -> x
                                       Nothing -> error "Declaracion de variable repetida"

   extractVarsDef :: DefineDec -> Set.Set Variable
   extractVarsDef defDec = let 
                              vars = defVarList defDec
                            in
                              case checkVarS vars (Just Set.empty) of
                                 Just x -> x
                                 Nothing -> error "Variables en DEFINITION repetidas"
   
   -- Verifica que las variables que va agregando no se hayan declarado con anterioridad, si es asi, reporta un error
   checkVarS :: [Variable] -> Maybe (Set.Set Variable) -> Maybe (Set.Set Variable)
   checkVarS [] s = s
   checkVarS (x:xs) Nothing  = Nothing
   checkVarS (x:xs) (Just s) = case Set.member x s of  
                                 True  -> Nothing
                                 False -> checkVarS xs (Just (Set.insert x s))

   
   
   checkSimpleVars :: BSimple -> Set.Set Variable -> Bool
   checkSimpleVars (SConst _) _            = True
   checkSimpleVars (SVariable var) set     = Set.member var set
   checkSimpleVars (SUnary _ exp) set    = checkSimpleVars exp set
   checkSimpleVars (SBinary _ e1 e2) set   = (checkSimpleVars e1 set) && (checkSimpleVars e2 set) 

   

   checkNextVars :: BNext -> Set.Set Variable -> Bool
   checkNextVars (NConst _)  set = True
   checkNextVars (NSVariable var) set = Set.member var set
   checkNextVars (NNVariable var) set = Set.member var set
   checkNextVars (NUnary _ exp)  set  = checkNextVars exp set 
   checkNextVars (NBinary _ e1 e2) set = (checkNextVars e1 set) && (checkNextVars e2 set)


   
   
   checkCTLSVars1 :: CTLF -> Set.Set Variable -> Bool
   checkCTLSVars1 (CConst _) _                  = True
   checkCTLSVars1 (CVariable var) set           = Set.member var set
   checkCTLSVars1 (CBUnary _ exp) set           = checkCTLSVars1 exp set
   checkCTLSVars1 (CCUnary _ exp) set           = checkCTLSVars1 exp set
   checkCTLSVars1 (CBBinary _ exp1 exp2) set    = (checkCTLSVars1 exp1 set) && (checkCTLSVars1 exp2 set)
   checkCTLSVars1 (CCBinary _ exp1 exp2) set    = (checkCTLSVars1 exp1 set) && (checkCTLSVars1 exp2 set)


   

   checkFairVars1 :: FairCons -> Set.Set Variable -> Bool
   checkFairVars1 (FairCons bsimple) vars = checkSimpleVars bsimple vars

   checkDefExpVars :: DefineExp -> Set.Set Variable -> Bool
   checkDefExpVars (DefineExp _ bsimple) vars = checkSimpleVars bsimple vars

   defVarList :: DefineDec -> [Variable]
   defVarList (DefineDec decs) = foldr addDefVar [] decs 

   addDefVar :: DefineExp -> [Variable] -> [Variable]
   addDefVar (DefineExp var _) vars = var:vars
   --------------------------------------------------------------------------------------------------
   --   Funciones auxiliares para la verificacion sintactica de las formulas
   --   Fin
   --------------------------------------------------------------------------------------------------                          






   --------------------------------------------------------------------------------------------------
   -- Funciones auxiliares para la transformacion de programas desordenados a ordenados
   -- Inicio
   --------------------------------------------------------------------------------------------------
   joinVarDec :: UModule -> VarDec
   joinVarDec (UModule xs) = VarDec $ foldr joinVarDec1 [] xs

   joinVarDec1 :: ModuleElem -> [Variable] -> [Variable]
   joinVarDec1 modelem vars = case modelem of
                                    (ModuleVar (VarDec ys)) -> ys ++ vars
                                    _                       -> vars     

   -- Junta los init InitCons, hace la conjuncion de todos los presentes
   joinInitCons :: UModule -> InitCons
   joinInitCons (UModule xs) = InitCons $ foldr joinInitCons1 (SConst TRUE) xs

   joinInitCons1 :: ModuleElem -> BSimple -> BSimple
   joinInitCons1 modelem form =  case modelem of
                                       (ModuleInit (InitCons bsimple)) -> SBinary And bsimple form
                                       _                               -> form
   
   -- Junta todas las DefineDec, las une en una sola lista                                       
   joinDefineDec :: UModule -> Maybe DefineDec
   joinDefineDec (UModule xs) =  case (foldr joinDefineDec1 [] xs) of
                                       [] -> Nothing
                                       xs -> Just (DefineDec xs)

   joinDefineDec1 :: ModuleElem -> [DefineExp] -> [DefineExp]
   joinDefineDec1 modelem defs = case modelem of
                                       (ModuleDefine (DefineDec ys)) -> ys ++ defs
                                       _                             -> defs 
   
   -- Junta todas las TransCons, hace la conjuncion de todas las presentes
   joinTransCons :: UModule -> TransCons
   joinTransCons (UModule xs) = TransCons $ foldr joinTransCons1 (NConst TRUE) xs

   joinTransCons1 :: ModuleElem -> BNext -> BNext
   joinTransCons1 modelem form = case modelem of
                                    (ModuleTrans (TransCons bnext)) -> NBinary And bnext form
                                    _                               -> form 

   -- Junta todas las CTLSpec, las une en una lista y las verifica de manera independiente
   joinCTLSpecs :: UModule -> [CTLSpec]
   joinCTLSpecs (UModule xs) = foldr joinCTLSpecs1 [] xs

   joinCTLSpecs1 :: ModuleElem -> [CTLSpec] -> [CTLSpec]
   joinCTLSpecs1 modelem ctls =  case modelem of
                                       (ModuleCTL ctlspec) -> ctlspec : ctls
                                       _                   -> ctls

   -- Junta todas las fair constraints, las une en una lista y las verifica independientemente
   joinFairCons :: UModule -> Maybe [FairCons]
   joinFairCons (UModule xs) =   case (foldr joinFairCons1 [] xs) of
                                       [] -> Nothing
                                       xs -> Just xs



   joinFairCons1 :: ModuleElem -> [FairCons] -> [FairCons]
   joinFairCons1 modelem fairs = case modelem of
                                       (ModuleFair faircons) -> faircons : fairs
                                       _                     -> fairs                  
   --------------------------------------------------------------------------------------------------
   -- Funciones auxiliares para la transformacion de programas desordenados a ordenados
   -- Fin
   --------------------------------------------------------------------------------------------------



   --------------------------------------------------------------------------------------------------
   -- Funcion que hace la transformacion de una funcion CTL a su forma normal existencial
   -- Inicio
   --------------------------------------------------------------------------------------------------
   -- Toma un programa y transforma la formula CTL dentro de el a Forma Normal Existencial (ENF, en ingles)
   transforMPENF :: PModule -> PModule
   transforMPENF (PModule vdc ics tcs ctls fcs) =  let
                                                      enf = map transformSENF ctls
                                                    in
                                                      PModule vdc ics tcs enf fcs

   transformSENF :: CTLSpec -> CTLSpec
   transformSENF (CTLSpec ctlf) = CTLSpec (transformFNEF ctlf)
   
   
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
   
   --------------------------------------------------------------------------------------------------
   -- Funcion que hace la transformacion de una funcion CTL a su forma normal existencial
   -- Fin
   --------------------------------------------------------------------------------------------------

   



   -- Algunas pruebas de los elementos que se van creando
   -------------------------------------------------------------------------------------------------------------------------------
   -- extractVarsDec (VarDec [Variable "a",Variable "b",Variable "b",Variable "c"])
   -- extractVarsDef (DefineDec [DefineExp (Variable "d") (NSVariable (Variable "a"))])




   -------------------------------------------------------------------------------------------------------------------------------
