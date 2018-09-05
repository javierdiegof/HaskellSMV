module LanguageDef(
   languageDef,
   lexer,
   identifier,
   reserved,
   reservedOp,
   parens,
   brackets,
   semi,
   whiteSpace,
   sOperators,
   nOperators,
   cOperators
)where
   import DataTypes
   import Text.ParserCombinators.Parsec
   import Text.ParserCombinators.Parsec.Expr
   import Text.ParserCombinators.Parsec.Language
   import qualified Text.ParserCombinators.Parsec.Token as Token


   languageDef = 
      emptyDef{
               Token.commentLine          = "--",
               Token.identStart           = lower,
               Token.identLetter          = lower <|> digit,
               Token.reservedNames        = [
                                             "next",
                                             "MODULE",
                                             "DEFINE",
                                             "INIT",
                                             "VAR",
                                             "IVAR",
                                             "TRANS",
                                             "TRUE",
                                             "FALSE",
                                             "CTLSPEC",
                                             "FAIRNESS"
                                          ],
               Token.reservedOpNames = [
                                          ":=",
                                          "!",
                                          "&",
                                          "|",
                                          "xor",
                                          "->",
                                          "<->",
                                          "EG",
                                          "EX",
                                          "EF",
                                          "EU",
                                          "AG",
                                          "AX",
                                          "AF",
                                          "AU"
                                       ]
      }

   lexer       = Token.makeTokenParser languageDef
   
   identifier  = Token.identifier      lexer
   reserved    = Token.reserved        lexer
   reservedOp  = Token.reservedOp      lexer
   parens      = Token.parens          lexer
   brackets    = Token.brackets        lexer
   semi        = Token.semi            lexer
   whiteSpace  = Token.whiteSpace      lexer


   -- Se deben de definir operadores distintos para las expresiones Simple y Next, semantica distinta tambien 
   sOperators = [
                    [Prefix  (reservedOp "!"   >> return (SUnary  Not  ))           ],
                    [Infix   (reservedOp "&"   >> return (SBinary And  )) AssocLeft ],
                    [Infix   (reservedOp "|"   >> return (SBinary Or   )) AssocLeft ],
                    [Infix   (reservedOp "xor" >> return (SBinary Xor  )) AssocLeft ],
                    [Infix   (reservedOp "->"  >> return (SBinary If   )) AssocRight],
                    [Infix   (reservedOp "<->" >> return (SBinary Iff  )) AssocLeft ]
                  ]
   
   nOperators = [
                    [Prefix  (reservedOp "!"   >> return (NUnary  Not  ))           ],
                    [Infix   (reservedOp "&"   >> return (NBinary And  )) AssocLeft ],
                    [Infix   (reservedOp "|"   >> return (NBinary Or   )) AssocLeft ],
                    [Infix   (reservedOp "xor" >> return (NBinary Xor  )) AssocLeft ],
                    [Infix   (reservedOp "->"  >> return (NBinary If   )) AssocRight],
                    [Infix   (reservedOp "<->" >> return (NBinary Iff  )) AssocLeft ]
                  ]
                  
   cOperators = [
                     [Prefix  (reservedOp "!"   >> return (CBUnary   Not   ))           ],
                     [Prefix  (reservedOp "EG"  >> return (CCUnary   EG    ))           ,
                      Prefix  (reservedOp "EX"  >> return (CCUnary   EX    ))           ,
                      Prefix  (reservedOp "EF"  >> return (CCUnary   EF    ))           , 
                      Prefix  (reservedOp "AG"  >> return (CCUnary   AG    ))           , 
                      Prefix  (reservedOp "AX"  >> return (CCUnary   AX    ))           , 
                      Prefix  (reservedOp "AF"  >> return (CCUnary   AF    ))            
                     ],
                     [Infix   (reservedOp "&"   >> return (CBBinary  And   )) AssocLeft ],
                     [Infix   (reservedOp "|"   >> return (CBBinary  Or    )) AssocLeft ],
                     [Infix   (reservedOp "xor" >> return (CBBinary Xor     )) AssocLeft ],
                     [Infix   (reservedOp "->"  >> return (CBBinary  If    )) AssocRight],
                     [Infix   (reservedOp "<->" >> return (CBBinary  Iff   )) AssocLeft ],
                     [Infix   (reservedOp "<->" >> return (CBBinary  Iff   )) AssocLeft ],
                     [Infix   (reservedOp "EU"  >> return (CCBinary  EU    )) AssocLeft ,
                      Infix   (reservedOp "AU"  >> return (CCBinary  AU    )) AssocLeft 
                     ]
                   ]