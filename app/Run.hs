module Run where

import CommonParserUtil

import TokenInterface
import Lexer
import Terminal
import Parser
import Expr
import Context
import CPP

import Control.Monad (when)
import System.IO

doProcess verbose fileName = do
  text_ <- readFile fileName
  let text = removeAttributes text_
  -- when (verbose) $ putStrLn "Lexing..."
  -- terminalList <- lexing lexerSpec text
  -- mapM_ (putStrLn . terminalToString) terminalList
  when verbose $ putStrLn "Remove GCC extensions"
  
  when verbose $ putStrLn "Parsing..."
  -- when verbose $ mapM_ (\(a,_,_)-> putStrLn $ show a) $ parserSpecList parserSpec
  -- putStrLn text
  
  exprSeqAst <-
    parsing False parserSpec
      (LPS {lexer_state=init_c_lexer_state, name_set=emptyContext},1,1,text)
         c_lexer (fromToken (endOfToken lexerSpec))
  when (verbose) $ putStrLn "Done."
  -- when (verbose) $ putStrLn "Pretty Printing..."
  -- when (verbose) $ putStrLn (pprintAst exprSeqAst)
  -- return (pprintAst exprSeqAst)
