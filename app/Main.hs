module Main where

import CommonParserUtil

import TokenInterface
import Token
import Expr
import Lexer
import Parser

import EmacsServer
import SynCompInterface
import Control.Exception
import Data.Typeable

import System.IO

import Run(doProcess)
--import ParserSpec (spec)

import System.IO
import System.Environment (getArgs, withArgs)

main :: IO ()
main = do
  args <- getArgs
  _main args

-- Todo: Can I fix to have "test" as a command in stack exec?

_main [] = return ()
_main (fileName:args) = 
  case fileName of
--    "test" -> withArgs [] spec
    "emacs" -> emacsServer (computeCand True)
    _ -> do _ <- doProcess True fileName
            _main args

-- Todo: The following part should be moved to the library.
--       Arguments: lexerSpec, parserSpec
--                  isSimpleMode
--                  programTextUptoCursor, programTextAfterCursor

maxLevel = 10000

-- | computeCand
data ParseErrorWithLineCol token ast = ParseErrorWithLineCol Int Int (ParseError token ast)
  deriving (Typeable, Show)

instance (TokenInterface token, Typeable token, Show token, Typeable ast, Show ast)
  => Exception (ParseErrorWithLineCol token ast)

computeCand :: Bool -> String -> String -> Bool -> IO [EmacsDataItem]
computeCand debug programTextUptoCursor programTextAfterCursor isSimpleMode = do
  ((computeCand_ isSimpleMode programTextUptoCursor programTextAfterCursor
    `catch` \e -> case e :: LexError of _ -> handleLexError)
    `catch` \e -> case e :: ParseErrorWithLineCol Token AST of ParseErrorWithLineCol line column e -> do {
        (_, _, terminalListAfterCursor) <- lexingWithLineColumn lexerSpec line column programTextAfterCursor;
        handleParseError (
          defaultHandleParseError {
              debugFlag=debug,
              searchMaxLevel=maxLevel,
              simpleOrNested=isSimpleMode,
              postTerminalList=terminalListAfterCursor,
              nonterminalToStringMaybe=Nothing
              }) e
          -- HandleParseError {
          --     debugFlag=debug,
          --     searchMaxLevel=maxLevel,
          --     simpleOrNested=isSimpleMode,
          --     postTerminalList=terminalListAfterCursor,
          --     nonterminalToStringMaybe=Nothing}) e
        })
    
computeCand_ :: Bool -> String -> String -> IO [EmacsDataItem]
computeCand_ isSimpleMode programTextUptoCursor programTextAfterCursor = do
  (line, column, terminalListUptoCursor)  <- lexingWithLineColumn lexerSpec 1 1 programTextUptoCursor
  
  ast <-
    (parsing True parserSpec1 terminalListUptoCursor
      `catch` \e -> case e :: ParseError Token AST of  _ -> throw (ParseErrorWithLineCol line column e))

  successfullyParsed

