module Main where

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
import SyntaxCompletion(computeCand)

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
    "emacs" -> emacsServer (computeCand False)
    _ -> do _ <- doProcess True fileName
            _main args

