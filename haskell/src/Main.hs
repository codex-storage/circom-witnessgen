
module Main where

--------------------------------------------------------------------------------

import Data.Map (Map)
import qualified Data.Map as Map

import Witness
import Parser
import Graph
import JSON
import Export

--------------------------------------------------------------------------------
{-

(~>) :: String -> a -> (String, a)
(~>) = (,)

infix 2 ~>

testInputs :: Map String [Integer]
testInputs = Map.fromList 
  [ "a" ~> [0xff01] 
  , "b" ~> [0xff02]
  ]

-}
--------------------------------------------------------------------------------

graphFile = "../../tmp/graph4.bin"
inputFile = "../../tmp/input4.json"
wtnsFile  = "../../tmp/my4.wtns"

main :: IO ()
main = do
  Right graph <- parseGraphFile graphFile
  putStrLn ""
  inputs <- loadInputJsonFile inputFile
  print (inputSignals $ graphMeta graph)
  let wtns = witnessCalc inputs graph
  -- putStrLn ""
  -- print wtns
  exportWitness wtnsFile wtns
