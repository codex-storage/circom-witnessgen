
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
fullFile  = "../../tmp/my4_full.bin"

main :: IO ()
main = do
  Right graph <- parseGraphFile    graphFile
  inputs      <- loadInputJsonFile inputFile

  -- putStrLn ""
  -- print (inputSignals $ graphMeta graph)

  -- let full = fullComputation graph inputs
  -- exportFeltSequence fullFile full

  let wtns = witnessCalc graph inputs 
  exportWitness wtnsFile wtns

