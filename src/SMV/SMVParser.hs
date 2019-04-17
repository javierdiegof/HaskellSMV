module SMVParser(
   parseFile
) where
   import DataTypes
   import LanguageDef
   import System.IO
   import Control.Monad
   import Data.Maybe
   import Text.ParserCombinators.Parsec
   import Text.ParserCombinators.Parsec.Expr
   


   --------------------------------------------------------------------------------------------------
   -- Parser del modulo desordenado (inicio)                                                     ----
   --------------------------------------------------------------------------------------------------
   uModuleParser :: Parser UModule
   uModuleParser = do 
                     list <- manyTill moduleElemParser eof
                     return $ UModule list 
   --------------------------------------------------------------------------------------------------
   -- Parser del modulo desordenado (fin)                                                        ----
   --------------------------------------------------------------------------------------------------

   --------------------------------------------------------------------------------------------------
   -- Parser de los elementos del modulo y sus wrapper (inicio)                                 -----
   --------------------------------------------------------------------------------------------------
   moduleElemParser :: Parser ModuleElem
   moduleElemParser =    mVarDecParser
                     <|> mIVarDecParser 
                     <|> mInitConsParser 
                     <|> mDefineParser
                     <|> mTransConsParser
                     <|> mCtlSpecParser
                     <|> mFairConsParser 

   mVarDecParser :: Parser ModuleElem
   mVarDecParser = do
                     varDec <- varDecParser
                     return $ ModuleVar varDec 

   mIVarDecParser :: Parser ModuleElem
   mIVarDecParser =  do 
                        iVarDec <- iVarDecParser
                        return $ ModuleIVar iVarDec

   
   mInitConsParser :: Parser ModuleElem
   mInitConsParser = do 
                        initcons <- initConsParser
                        return $ ModuleInit initcons
               
   mDefineParser :: Parser ModuleElem
   mDefineParser = do
                     definedec <- defineDecParser
                     return $ ModuleDefine definedec

   mTransConsParser :: Parser ModuleElem
   mTransConsParser = do
                        transcons <- transConsParser
                        return $ ModuleTrans transcons
            
   mCtlSpecParser :: Parser ModuleElem
   mCtlSpecParser =  do
                        ctlspec <- ctlSpecParser
                        return $ ModuleCTL ctlspec

   mFairConsParser :: Parser ModuleElem
   mFairConsParser = do
                        faircons <- fairConsParser
                        return $ ModuleFair faircons
   --------------------------------------------------------------------------------------------------
   -- Parser de los elementos del modulo y sus wrapper (fin)                                    -----
   --------------------------------------------------------------------------------------------------




   --------------------------------------------------------------------------------------------------
   -- Parser de los elementos del modulo (module_element) inicio                                -----
   --------------------------------------------------------------------------------------------------
   varDecParser :: Parser VarDec
   varDecParser = do
                     reserved "VAR"
                     list <- endBy1 variableParser semi -- separados y finalizados por punto y coma (semicolon)
                     return $ VarDec list

               
   iVarDecParser :: Parser IVarDec
   iVarDecParser = do
                     reserved "IVAR"
                     list <- endBy1 variableParser semi -- separados y finalizados por punto y coma (semicolon)
                     return $ IVarDec list


   initConsParser :: Parser InitCons
   initConsParser = do
                     reserved "INIT"
                     bsimple <- bSimpleParser
                     optional semi
                     return $ InitCons bsimple 


   defineDecParser :: Parser DefineDec
   defineDecParser = do
                        reserved "DEFINE"
                        list <- endBy1 defineExpParser semi -- separados y finalizados por un punto y coma (semicolon)
                        return $ DefineDec list

   
   transConsParser :: Parser TransCons
   transConsParser =  do
                        reserved "TRANS"
                        bnext <- bNextParser
                        optional semi
                        return $ TransCons bnext

   ctlSpecParser :: Parser CTLSpec
   ctlSpecParser = do
                     reserved "CTLSPEC"
                     ctlf <- ctlFParser
                     optional semi
                     return $ CTLSpec ctlf

   fairConsParser :: Parser FairCons
   fairConsParser =  do  
                        reserved "FAIRNESS"
                        bsimple <- bSimpleParser
                        optional semi
                        return $ FairCons bsimple
   --------------------------------------------------------------------------------------------------
   -- Parser de los elementos del modulo (module_element) fin                                   -----
   --------------------------------------------------------------------------------------------------
   

   
   
   
   
   --------------------------------------------------------------------------------------------------
   --- Parser de expresiones simples y sus wrappers (inicio)                                 --------
   --------------------------------------------------------------------------------------------------
   -- Crea las operaciones a partir de los operadores
   bSimpleParser :: Parser BSimple
   bSimpleParser = buildExpressionParser sOperators sTerm

   sTerm =      parens bSimpleParser    -- Entre parentesis
            <|> sConstParser            -- Una constante booleana
            <|> sVariableParser         -- Una variable simple

   -- Parsea constantes booleanas y las envuelve en una expresion simple                   
   sConstParser :: Parser BSimple
   sConstParser = do
                     bconstant <- bConstantParser
                     return $ SConst bconstant
            
   -- Parsea variables (booleanas) y las envuelve en expresiones simples
   sVariableParser :: Parser BSimple
   sVariableParser = do
                        variable <- variableParser
                        return $ SVariable variable
   --------------------------------------------------------------------------------------------------
   --- Parser de expresiones simples y sus wrappers (fin)                                    --------
   --------------------------------------------------------------------------------------------------
   
   
   
   
   --------------------------------------------------------------------------------------------------
   --- Parser de expresiones next y sus wrappers (inicio)                                    --------
   --------------------------------------------------------------------------------------------------
   -- Parsea expresiones next
   bNextParser :: Parser BNext
   bNextParser = buildExpressionParser nOperators nTerm

   nTerm =      parens bNextParser
            <|> nConstParser 
            <|> nNVariableParser
            <|> nSVariableParser

   -- Parsea constantes booleanas dentro de expresiones next                   
   nConstParser :: Parser BNext
   nConstParser = do
                    bconstant <- bConstantParser
                    return $ NConst bconstant

   -- Parsea variables (booleanas) dentro de expresiones next    
   nSVariableParser :: Parser BNext
   nSVariableParser = do 
                        variable <- variableParser
                        return $ NSVariable variable

   -- Parsea variables next (booleanas) dentro de expresiones next         
   nNVariableParser :: Parser BNext
   nNVariableParser  = do 
                           reserved "next"
                           nvariable <- parens variableParser
                           return $ NNVariable nvariable
   --------------------------------------------------------------------------------------------------
   --- Parser de expresiones next y sus wrappers (fin)                                    --------
   --------------------------------------------------------------------------------------------------
   




   --------------------------------------------------------------------------------------------------
   --- Parser de expresiones CTL y sus wrappers (inicio)                                    --------
   --------------------------------------------------------------------------------------------------
   ctlFParser :: Parser CTLF
   ctlFParser = buildExpressionParser cOperators cTerm

   cTerm =     parens ctlFParser
         <|>  cConstParser
         <|>  cVariableParser

   -- Parsea constantes booleanas dentro de expresiones CTL
   cConstParser :: Parser CTLF
   cConstParser = do
                     bconstant <- bConstantParser
                     return $ CConst bconstant

   cVariableParser :: Parser CTLF
   cVariableParser = do 
                        variable <- variableParser
                        return $ CVariable variable
   --------------------------------------------------------------------------------------------------
   --- Parser de expresiones CTL y sus wrappers (fin)                                        --------
   --------------------------------------------------------------------------------------------------






   
   
   
   
   
   --------------------------------------------------------------------------------------------------
   --- Parser de expresiones define y sus wrappers (inicio)                                    --------
   --------------------------------------------------------------------------------------------------
   -- Parsea expresiones define
   defineExpParser :: Parser DefineExp
   defineExpParser = do 
                        var <- variableParser
                        reservedOp ":="
                        nextexp <- bSimpleParser
                        return $ DefineExp var nextexp
   --------------------------------------------------------------------------------------------------
   --- Parser de expresiones define y sus wrappers (fin)                                    --------
   --------------------------------------------------------------------------------------------------






   --------------------------------------------------------------------------------------------------
   --- Parsers primitivos que utilizan los elementos del modulo (inicio)                     --------
   --------------------------------------------------------------------------------------------------
   -- Parsea a los identificadores individualmente
   variableParser :: Parser Variable
   variableParser = fmap Variable identifier -- Conformados por un identificador

   -- Parsea constantes booleanas, usadas para formar expresiones simples y complejas
   bConstantParser :: Parser BConstant
   bConstantParser =       (reserved "TRUE" >> return TRUE)
                     <|>   (reserved "FALSE" >> return FALSE)
   --------------------------------------------------------------------------------------------------
   --- Parsers primitivos que utilizan los elementos del modulo (fin)                        --------
   --------------------------------------------------------------------------------------------------




   --------------------------------------------------------------------------------------------------
   --- Pruebas de parsers varios (inicio)                                                    --------
   --------------------------------------------------------------------------------------------------
   parseVarDec :: String -> VarDec
   parseVarDec string = case parse varDecParser "" string of 
                           Left e   -> error $ show e
                           Right r  -> r

   parseInitCons :: String -> InitCons
   parseInitCons string =   case parse initConsParser "" string of
                              Left e   -> error $ show e
                              Right r  -> r

   parseDefineDec :: String -> DefineDec
   parseDefineDec string = case parse defineDecParser "" string of
                              Left e   -> error $ show e
                              Right r  -> r
   
   parseTransCons :: String -> TransCons
   parseTransCons string =  case parse transConsParser "" string of
                              Left e   -> error $ show e
                              Right r  -> r 
                        
   parseCTLSpec :: String -> CTLSpec
   parseCTLSpec string =  case parse ctlSpecParser "" string of
                              Left e   -> error $ show e
                              Right r  -> r 

   parseFairCons :: String -> FairCons
   parseFairCons string =  case parse fairConsParser "" string of
                              Left e   -> error $ show e
                              Right r  -> r

   parseFile :: String -> IO UModule
   parseFile file =  do
                        program <- readFile file
                        case parse uModuleParser "" program of
                           Left  e -> print e >> fail "parse error"
                           Right r -> return r
   
   

   
   --------------------------------------------------------------------------------------------------
   --- Pruebas de parsers varios (fin)                                                       --------
   --------------------------------------------------------------------------------------------------






   --------------------------------------------------------------------------------------------------
   --- Cadenas para probar parsers (inicio)                                                  --------
   --------------------------------------------------------------------------------------------------
   -- parseVarDec "VAR a; b;"
   -- parseInitCons "INIT a & b & c; a & b & c;"
   -- parseDefineDec "DEFINE sx := next(ax) & bx; sy := ay & next(by);" 
   -- parseTransCons  "TRANS next(a) & next(b); next(a) & next(b);"
   -- parseCTLSpec "CTLSPEC EX(a) & AG(b);"
   -- parseFairCons "FAIRNESS a & b;"
   -- parseFile "pruebanewparser.txt"
   --------------------------------------------------------------------------------------------------
   --- Cadenas para probar parsers  (fin)                                                    --------
   --------------------------------------------------------------------------------------------------

