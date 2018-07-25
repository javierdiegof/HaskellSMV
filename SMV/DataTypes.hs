module DataTypes(
   BSimple(..),
   BNext(..),
   Variable(..),
   BConstant(..),
   BUnOp(..),
   BBinOp(..),
   CTLF(..),
   CUnOp(..),
   CBinOp(..),
   VarS(..),
   Init(..),
   Trans(..),
   CTLS(..),
   Fair(..),
   --Program(..),
   --UProgram(..),
   VarDec(..),
   InitCons(..),
   DefineExp(..),
   DefineDec(..),
   TransCons(..),
   CTLSpec(..),
   FairCons(..),
   ModuleElem(..),
   UModule(..),
   Define(..)
) where
   import Data.Map.Strict

   -- Expresion simple, sin next
   data BSimple =    SConst BConstant
                  |  SVariable Variable
                  |  SUnary BUnOp BSimple
                  |  SBinary BBinOp BSimple BSimple
                  deriving (Show)
   
   -- Expresion con next
   data BNext = NConst BConstant
                  |  NSVariable Variable -- variable sin next
                  |  NNVariable Variable -- variable con next
                  |  NUnary BUnOp BNext
                  |  NBinary BBinOp BNext BNext
                  deriving (Show)
   
   -- Variables simples, sin posibilidad de next
   data Variable = Variable String
                  deriving(Show)

   -- Misma sintaxis que NuSMV
   data BConstant = TRUE | FALSE deriving(Show)
      
   data BUnOp = Not deriving (Show)

   data DefineExp = DefineExp Variable BNext deriving (Show)

   -- Misma sintaxis que NuSMV
   data BBinOp =   And 
               |   Or 
               |   If 
               |   Iff 
               deriving(Show)
  
   
   data CTLF  =   CConst BConstant
               |  CVariable Variable
               |  CBUnary  BUnOp    CTLF        -- Operadores booleanos unarios (Not)
               |  CCUnary  CUnOp    CTLF        -- Operadores CTL unarios (AX, EX, AF, EF, AG, EG)
               |  CBBinary BBinOp   CTLF  CTLF  -- Operadores booleanos binarios (&, |, ->, <->)
               |  CCBinary CBinOp   CTLF  CTLF  -- Operadores CTL binarios (AU, EU)
               deriving(Show)
   
   data CUnOp =   EG
               |  EX
               |  EF
               |  AG 
               |  AX
               |  AF  
               deriving(Show)

   data CBinOp =      EU
                  |   AU
                  deriving(Show)

   data VarDec     = VarDec    [Variable]     deriving (Show)
   data InitCons   = InitCons   BSimple       deriving (Show)  
   data DefineDec  = DefineDec [DefineExp]    deriving (Show)  
   data TransCons  = TransCons  BNext         deriving (Show)
   data CTLSpec    = CTLSpec   CTLF           deriving (Show) 
   data FairCons   = FairCons  BSimple        deriving (Show) 

   data ModuleElem =   ModuleVar       VarDec
                     | ModuleInit      InitCons
                     | ModuleDefine    DefineDec
                     | ModuleTrans     TransCons
                     | ModuleCTL       CTLSpec
                     | ModuleFair      FairCons
                     deriving (Show)
               
   data UModule   = UModule [ModuleElem]   deriving (Show)

   data OModule   = OModule VarDec InitCons (Maybe DefineDec) TransCons [CTLSpec] (Maybe [FairCons]) deriving (Show)
   




   -- Conjunto de asignaciones, uso en INIT y en TRANS
   data VarS      = VarS [Variable]                deriving (Show)
   data Init      = Init [BSimple]                 deriving (Show)
   data Define    = Define [DefineExp]             deriving (Show)
   data Trans     = Trans [BNext]                  deriving (Show)
   data CTLS      = CTLS CTLF                      deriving (Show)
   data Fair      = Fair [BSimple]                 deriving (Show)

   {-
   data Program   = Program VarS Init Trans CTLS (Maybe Fair)
                     deriving(Show)
   data UProgram  = UProgram VarS (Maybe Define) Init Trans CTLS (Maybe Fair)
                     deriving(Show)
   -}

   {-
      Operaciones utiles para los tipos que se  han creado
   -}

   -- Dos variables son iguales si sus nombres son iguales
   instance Eq Variable where
      -- (==)  :: a -> a -> Bool
      Variable x == Variable y      =  x == y
      -- (/=)  :: a -> a -> Bool
      Variable x /= Variable y      =  x /= y

   instance Ord Variable where
      -- (<=) :: a -> a -> Bool
      Variable x <= Variable y      = x <= y

   instance Eq BConstant where   
      -- (==) :: a -> a -> Bool
      TRUE == TRUE                  = True
      FALSE == FALSE                = True
      _  == _                       = False

   {-
      Esta funcion convierte un modulo desordenado a un modulo ordenado.
      El modulo desordenado proviene del parser.
      El modulo ordenado es con el que van a trabajar los algoritmos de verificacion de modelos
   -}

   convertModule :: UModule -> OModule
   convertModule umodule = let
                              vardec      = extractVarDec      umodule
                              initcons    = extractInitCons    umodule
                              mdefinedec  = extractDefineDec   umodule
                              transcons   = extractTransCons   umodule
                              ctlspecs    = extractCTLSpecs    umodule
                              mfaircons   = extractFairCons    umodule
                            in
                              OModule vardec initcons mdefinedec transcons ctlspecs mfaircons



   -- Funciones auxiliares para convertir un modulo desordenado a ordenado
   
   -- Obtiene un unico VarDec 
   extractVarDec :: UModule -> VarDec
   extractVarDec umodule = VarDec $ extractVarDec1 umodule
   
   extractVarDec1 :: UModule -> [Variable]
   extractVarDec1 (UModule (x:xs)) = case x of 
                                       (ModuleVar (VarDec ys))    -> ys ++ extractVarDec1(UModule xs)
                                       _                          -> extractVarDec1(UModule xs)
   
   -- Obtiene un unico InitCons
   extractInitCons :: UModule -> InitCons
   extractInitCons umodule = InitCons $ extractInitCons1 (umodule)

   extractInitCons1 :: UModule -> BSimple
   extractInitCons1 (UModule (x:xs)) = case x of
                                          (ModuleInit (InitCons bsimple))  -> (SBinary And bsimple (extractInitCons1 (UModule xs)))
                                          _                                -> extractInitCons1 (UModule xs)
   


   extractDefineDec :: UModule -> Maybe DefineDec
   extractDefineDec umodule =   case (extractDefineDec1 umodule) of
                                    []    -> Nothing
                                    xs    -> Just (DefineDec xs)

   extractDefineDec1 :: UModule -> [DefineExp]
   extractDefineDec1 (UModule (x:xs)) = case x of
                                          (ModuleDefine (DefineDec ys)) -> ys ++ extractDefineDec1(UModule (xs))
                                          _                             -> extractDefineDec1 (UModule (xs))

   -- Obtiene un unico transcons
   extractTransCons :: UModule -> TransCons
   extractTransCons umodule = TransCons $ extractTransCons1 umodule

   extractTransCons1 :: UModule -> BNext
   extractTransCons1 (UModule (x:xs)) =   case x of
                                             (ModuleTrans (TransCons bnext)) -> (NBinary And bnext (extractTransCons1 (UModule xs)))
                                             _                               -> extractTransCons1 (UModule xs)
   
   extractCTLSpecs :: UModule -> [CTLSpec]
   extractCTLSpecs (UModule (x:xs)) =  case x of
                                          (ModuleCTL ctlspec)  -> ctlspec : extractCTLSpecs(UModule xs)
                                          _                    -> extractCTLSpecs(UModule xs)    
                                       
   extractFairCons :: UModule -> Maybe [FairCons]
   extractFairCons umodule =  case (extractFairCons1 umodule) of 
                              []    -> Nothing
                              xs    -> Just xs

   extractFairCons1 :: UModule -> [FairCons]
   extractFairCons1 (UModule (x:xs)) =  case x of
                                          (ModuleFair faircons) -> faircons : extractFairCons1 (UModule xs)
                                          _                     -> extractFairCons1 (UModule xs)