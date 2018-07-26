module DataTypesOps(
   syntaxCheck
) where
   import DataTypes
   import SMVParser
   import qualified Data.Set as Set

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
                           return pmodc  



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
   convertModuleOP (OModule vdc _ ics tcs ctls fcs) = PModule vdc ics tcs ctls fcs

   pModEmptyCheck :: PModule -> PModule
   pModEmptyCheck (PModule vdc ics tcs ctls fcs) = let 
                                                      vdcE  = varDecEmpty vdc
                                                      icsE  = initConsEmpty ics
                                                      tcsE  = transConsEmpty tcs
                                                      ctlsE = ctlSpecEmpty ctls
                                                    in
                                                      if(vdcE == True)
                                                      then error "VAR esta vacio"
                                                      else if (icsE == True)
                                                               then error "INIT vacio"
                                                               else if (tcsE == True)
                                                                        then error "TRANS vacio"
                                                                        else if (ctlsE == True)
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
   --------------------------------------------------------------------------------------------------
   --   Funciones que realizan el chequeo sintactico de los diferentes elementos en un PModule
   --   Verifican que todas las variables usadas hayan sido declaradas en Vars
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

   extractVarsDec :: VarDec -> Set.Set Variable
   extractVarsDec (VarDec vars) =   case checkVarS vars (Just Set.empty) of
                                       Just x -> x
                                       Nothing -> error "Declaracion de variable repetida"
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
