
{-# LANGUAGE StrictData #-}
module Witness where

--------------------------------------------------------------------------------

import Data.Array
import Data.Word

import qualified Data.Map    as Map    ; import Data.Map    (Map   )
import qualified Data.IntMap as IntMap ; import Data.IntMap (IntMap)

import BN254

import qualified Semantics as S ; import Semantics ( PrimOp , evalPrimOp )
import qualified Graph     as G ; import Graph     ( Graph(..) , Node(..) , UnoOpNode(..) , DuoOpNode(..) , TresOpNode(..) , SignalDescription(..) ) 

import Debug.Trace
debug msg x y = trace (">>> " ++ msg ++ " ~> " ++ show x) y

--------------------------------------------------------------------------------

newtype Witness = MkWitness (Array Int F)

type Inputs = Map String [Integer]

-- | This includes all temporary values (one per graph node), not all of which is
-- present in the final witness
fullComputation :: Graph -> Inputs -> Array Int F 
fullComputation (Graph nodes meta) inputs = rawWitness where
  nodesArr   = listArray (0,length nodes-1) nodes
  rawWitness = evaluateNodes rawInputs nodesArr
  rawInputs  = convertInputs (G.inputSignals meta) inputs 

fullLogToWitness :: Graph -> Array Int F -> Witness
fullLogToWitness (Graph nodes meta) fullLog = MkWitness witness where
  mapping_   = G.fromWitnessMapping (G.witnessMapping meta)
  wtnslen    = length mapping_
  mapping    = listArray (0,wtnslen-1) mapping_
  witness    = listArray (0,wtnslen-1) $ [ fullLog!(fromIntegral (mapping!i)) | i<-[0..wtnslen-1] ]

witnessCalc :: Graph -> Inputs -> Witness
witnessCalc graph inputs = fullLogToWitness graph (fullComputation graph inputs)

convertInputs :: [(String,SignalDescription)] -> Map String [Integer] -> IntMap F
convertInputs descTable inputTable = IntMap.fromList $ (0,1) : concatMap f descTable where
  f :: (String,SignalDescription) -> [(Int,F)]
  f (name,desc) = case Map.lookup name inputTable of
    Nothing     -> error $ "input signal `" ++ name ++ "` not found in the given inputs!"
    Just values -> if length values /= fromIntegral (signalLength desc)
      then error $ "input signal `" ++ name ++ "` has incorrect size"
      else let ofs = fromIntegral (signalOffset desc) :: Int
           in  zip [ofs..] (map toF values)

--------------------------------------------------------------------------------

type RawInputs = IntMap F

evaluateNodes :: RawInputs -> Array Int Node -> Array Int F
evaluateNodes inputs nodes = witness where

  (a,b) = bounds nodes
  witness = array (0,b-a) $ [ (i-a, worker i) | i<-[a..b] ] 

  lkp :: Word32 -> F
  lkp i = witness!(fromIntegral i)

  worker i = case nodes!i of
    AnInputNode   (G.InputNode    idx)  -> getInputValue idx
    AConstantNode (G.ConstantNode big)  -> fromBigUInt big
    AnUnoOpNode   unoOpNode             -> evalPrimOp (fmap lkp $ unoToPrimOp  unoOpNode )
    ADuoOpNode    duoOpNode             -> evalPrimOp (fmap lkp $ duoToPrimOp  duoOpNode )
    ATresOpNode   tresOpNode            -> evalPrimOp (fmap lkp $ tresToPrimOp tresOpNode)

  getInputValue j = case IntMap.lookup (fromIntegral j) inputs of
    Just y  -> y
    Nothing -> error ("input value not found at index " ++ show j)

  fromBigUInt (G.BigUInt bs) = fromBytesLE bs

--------------------------------------------------------------------------------

unoToPrimOp :: UnoOpNode -> PrimOp Word32
unoToPrimOp (UnoOpNode op arg1) = case op of
  G.Neg -> S.Neg arg1
  G.Id  -> S.Id  arg1

duoToPrimOp :: DuoOpNode -> PrimOp Word32
duoToPrimOp (DuoOpNode op arg1 arg2) = case op of
  G.Mul  -> S.Mul  arg1 arg2
  G.Div  -> S.Div  arg1 arg2
  G.Add  -> S.Add  arg1 arg2
  G.Sub  -> S.Sub  arg1 arg2     -- debug "sub " (op,arg1,arg2) $ 
  G.Pow  -> S.Pow  arg1 arg2
  G.Idiv -> S.Idiv arg1 arg2
  G.Mod  -> S.Mod  arg1 arg2
  G.Eq_  -> S.Eq_  arg1 arg2
  G.Neq  -> S.Neq  arg1 arg2
  G.Lt   -> S.Lt   arg1 arg2
  G.Gt   -> S.Gt   arg1 arg2
  G.Leq  -> S.Leq  arg1 arg2
  G.Geq  -> S.Geq  arg1 arg2
  G.Land -> S.Land arg1 arg2
  G.Lor  -> S.Lor  arg1 arg2
  G.Shl  -> S.Shl  arg1 arg2
  G.Shr  -> S.Shr  arg1 arg2
  G.Bor  -> S.Bor  arg1 arg2
  G.Band -> S.Band arg1 arg2
  G.Bxor -> S.Bxor arg1 arg2

tresToPrimOp :: TresOpNode -> PrimOp Word32
tresToPrimOp (TresOpNode op arg1 arg2 arg3) = case op of
  G.TernCond -> S.Cond arg1 arg2 arg3

--------------------------------------------------------------------------------
