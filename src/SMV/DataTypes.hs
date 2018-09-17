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
   VarDec(..),
   IVarDec(..),
   InitCons(..),
   DefineExp(..),
   DefineDec(..),
   TransCons(..),
   CTLSpec(..),
   FairCons(..),
   ModuleElem(..),
   UModule(..),
   OModule(..),
   PModule(..)
) where
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
   newtype Variable = Variable String
                  deriving(Show)

   -- Misma sintaxis que NuSMV
   data BConstant = TRUE | FALSE deriving(Show)
      
   data BUnOp = Not deriving (Show)

   data DefineExp = DefineExp Variable BSimple deriving (Show)

   -- Misma sintaxis que NuSMV
   data BBinOp =   And 
               |   Or 
               |   Xor
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

   newtype VarDec    = VarDec    [Variable]     deriving (Show)
   newtype IVarDec   = IVarDec   [Variable]     deriving (Show)
   newtype InitCons  = InitCons   BSimple       deriving (Show)  
   newtype DefineDec = DefineDec [DefineExp]    deriving (Show)  
   newtype TransCons = TransCons  BNext         deriving (Show)
   newtype CTLSpec   = CTLSpec   CTLF           deriving (Show) 
   newtype FairCons  = FairCons  BSimple        deriving (Show) 

   data ModuleElem =   ModuleVar       VarDec
                     | ModuleIVar      IVarDec
                     | ModuleInit      InitCons
                     | ModuleDefine    DefineDec
                     | ModuleTrans     TransCons
                     | ModuleCTL       CTLSpec
                     | ModuleFair      FairCons
                     deriving (Show)
               
   newtype UModule   = UModule [ModuleElem]   deriving (Show)

   data OModule      = OModule VarDec (Maybe IVarDec) (Maybe DefineDec) InitCons TransCons [CTLSpec] (Maybe [FairCons]) deriving (Show)

   data PModule      = PModule VarDec (Maybe IVarDec) InitCons TransCons [CTLSpec] (Maybe [FairCons]) deriving (Show)


   
   



   {-
   -- Conjunto de asignaciones, uso en INIT y en TRANS
   data VarS      = VarS [Variable]                deriving (Show)
   data Init      = Init [BSimple]                 deriving (Show)
   data Define    = Define [DefineExp]             deriving (Show)
   data Trans     = Trans [BNext]                  deriving (Show)
   data CTLS      = CTLS CTLF                      deriving (Show)
   data Fair      = Fair [BSimple]                 deriving (Show)
   -}
   {-
   data Program   = Program VarS Init Trans CTLS (Maybe Fair)
                     deriving(Show)
   data UProgram  = UProgram VarS (Maybe Define) Init Trans CTLS (Maybe Fair)
                     deriving(Show)
   -}

   {-
      Convierte de OModule, que aun tiene las expresiones Define a PModule, con el que trabajaran los algoritmos semanticos
      Esta funcion elimina la construccion "Maybe Define" del modulo ordenado.
      Es decir, sustituye las variables definidas a lo largo del programa.
      Despues del procedimiento, ya no habra definiciones porque ya se habran sus  
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
   
                        
