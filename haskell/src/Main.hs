
module Main where

--------------------------------------------------------------------------------

import Data.Map (Map)
import qualified Data.Map as Map

import Witness
import Parser
import Graph
import JSON

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

main :: IO ()
main = do
  Right graph <- parseGraphFile "../../tmp/graph4.bin"
  putStrLn ""
  inputs <- loadInputJsonFile "../../tmp/input4.json"
  print (inputSignals $ graphMeta graph)
  let wtns = witnessCalc inputs graph
  putStrLn ""
  print wtns