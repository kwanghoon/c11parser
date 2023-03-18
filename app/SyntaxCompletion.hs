module SyntaxCompletion where

import CommonParserUtil

import TokenInterface
import Token
import Expr
import Lexer
import Parser
import Context
import SynCompAlgorithm

import EmacsServer
import SynCompInterface
import Control.Exception
import Data.Typeable

import System.IO

import Run(doProcess)
--import ParserSpec (spec)

import System.IO
import System.Environment (getArgs, withArgs)


-- Todo: The following part should be moved to the library.
--       Arguments: lexerSpec, parserSpec
--                  isSimpleMode
--                  programTextUptoCursor, programTextAfterCursor

maxLevel = 10000

-- | computeCand
computeCand :: Bool -> String -> String -> Bool -> IO [EmacsDataItem]
computeCand debug programTextUptoCursor programTextAfterCursor isSimpleMode =
  ((do ast <- parsing False parserSpec
                (LPS {lexer_state=init_c_lexer_state,
                        name_set=emptyContext},1,1,programTextUptoCursor)
                  c_lexer (fromToken (endOfToken lexerSpec))
       successfullyParsed)
    `catch` \e ->
      case e :: ParseError Token AST LPS of
        _ ->
          do compCandidates <- chooseCompCandidatesFn
            
             handleParseError
               compCandidates
               (defaultHandleParseError lexerSpec parserSpec) {
                   debugFlag=debug,
                   searchMaxLevel=maxLevel,
                   simpleOrNested=isSimpleMode,
                   postTerminalList=[],              -- terminalListAfterCursor is set to []!
                   nonterminalToStringMaybe=Nothing
                   } e
  )
  `catch` \e -> case e :: LexError of _ -> handleLexError
