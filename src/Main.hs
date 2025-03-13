
module Main where

--------------------------------------------------------------------------------

import Data.Map (Map)
import qualified Data.Map as Map

import Witness
import Parser

--------------------------------------------------------------------------------

(~>) :: String -> a -> (String, a)
(~>) = (,)

infix 2 ~>

testInputs :: Map String [Integer]
testInputs = Map.fromList 
  [ "a" ~> [0xff01] 
  , "b" ~> [0xff02]
  ]

--------------------------------------------------------------------------------

main :: IO ()
main = do
  Right graph <- parseGraphFile "../tmp/graph2.bin"
  putStrLn ""
  print graph
  let wtns = witnessCalc testInputs graph
  putStrLn ""
  print wtns