module Parser where

import CommonParserUtil
import Token
import Expr

-- | Utility
rule prodRule action              = (prodRule, action, Nothing  )
ruleWithPrec prodRule action prec = (prodRule, action, Just prec)
ruleWithNoAction prodRule         = (prodRule, noAction, Nothing)

noAction = \rhs -> ()

-- | 
parserSpec :: ParserSpec Token AST
parserSpec = ParserSpec
  {
    startSymbol = "Start",
    
    tokenPrecAssoc = [],
    
    parserSpecList =
    [
      ruleWithNoAction "Start -> AdditiveExpr",

      ruleWithNoAction "AdditiveExpr -> AdditiveExpr + PrimaryExpr",

      ruleWithNoAction "AdditiveExpr -> AdditiveExpr - PrimaryExpr",

      ruleWithNoAction "AdditiveExpr -> PrimaryExpr",

      ruleWithNoAction "PrimaryExpr -> integer_number",

      ruleWithNoAction "PrimaryExpr -> ( AdditiveExpr )"
    ],
    
    baseDir = "./",
    actionTblFile = "action_table.txt",  
    gotoTblFile = "goto_table.txt",
    grammarFile = "prod_rules.txt",
    parserSpecFile = "mygrammar.grm",
    genparserexe = "yapb-exe"
  }


parserSpec1 :: ParserSpec Token AST
parserSpec1 = ParserSpec
  {
    startSymbol = "Start",
    
    tokenPrecAssoc = [],
    
    parserSpecList =
    [
      ruleWithNoAction "Start -> MultiplicativeExpr",   -- Changed

      ruleWithNoAction "MultiplicativeExpr -> MultiplicativeExpr * AdditiveExpr",   -- New

      ruleWithNoAction "MultiplicativeExpr -> MultiplicativeExpr / AdditiveExpr",   -- New

      ruleWithNoAction "MultiplicativeExpr -> AdditiveExpr",            -- New
      
      ruleWithNoAction "AdditiveExpr -> AdditiveExpr + PrimaryExpr",

      ruleWithNoAction "AdditiveExpr -> AdditiveExpr - PrimaryExpr",

      ruleWithNoAction "AdditiveExpr -> PrimaryExpr",

      ruleWithNoAction "PrimaryExpr -> integer_number",

      ruleWithNoAction "PrimaryExpr -> ( MultiplicativeExpr )" -- Changed: MultiplicativeExpr
    ],
    
    baseDir = "./",
    actionTblFile = "action_table.txt",  
    gotoTblFile = "goto_table.txt",
    grammarFile = "prod_rules.txt",
    parserSpecFile = "mygrammar.grm",
    genparserexe = "yapb-exe"
  }


