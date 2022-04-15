module Main where

-- import ParserBidiLinksTest

-- main :: IO ()
-- main = runTestTT parserTests

import SyntaxCompletion (computeCand)
import SynCompInterface
import Test.Hspec

import Config

import Data.Maybe (isJust)
import System.IO (readFile)

spec = hspec $ do
  describe "Testing examples/exp/benchmarks" $ do
    let benchmark1 = "int main() {\n  if ( x "          -- examples/exp/benchmark1.c
    let benchmark2 = "int main() {\n  if ( x < 123 ) "  -- examples/exp/benchmark2.c

    let config_simple = True
    let max_gs_level  = 9
    
    let config =
          Configuration
            {
              config_SIMPLE       = config_simple,
              config_R_LEVEL      = 1,
              config_GS_LEVEL     = 1,
              config_DEBUG        = False,
              config_DISPLAY      = False,
              config_PRESENTATION = 0,
              config_ALGORITHM    = 3
            }
    
    it ("[Benchmark1] " ++ benchmark1) $
      do results <- computeCand False benchmark1 "" config_simple
         mapM_ (item benchmark1 config) [1..max_gs_level] 

    it ("[Benchmark2] " ++ benchmark2) $
      do results <- computeCand False benchmark2 "" config_simple
         mapM_ (item benchmark2 config) [1..max_gs_level] 

item benchmark init_config gslevel = 
      do let test_config = init_config{config_GS_LEVEL=gslevel}
         putStrLn (show test_config)
         
         configMaybe <- readConfig
         case configMaybe of
           Just config ->
             do writeConfig test_config  -- set
                results <- computeCand False benchmark "" (config_SIMPLE test_config)
                writeConfig init_config       -- restore
                
           Nothing -> isJust configMaybe `shouldBe` True

main :: IO ()
main = spec

