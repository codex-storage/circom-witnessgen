
{-# LANGUAGE StrictData #-}
module Graph where

--------------------------------------------------------------------------------

import Text.Printf

--------------------------------------------------------------------------------

import Data.Word

data Graph = Graph
  { graphNodes :: [Node]
  , graphMeta  :: GraphMetaData
  }
  deriving Show

--------------------------------------------------------------------------------

-- | Unary operations
data UnoOp
  = Neg       -- ^ @= 0@
  | Id        -- ^ @= 1@
  deriving (Eq,Enum,Bounded,Show)

data DuoOp 
  = Mul       -- ^ @=  0@
  | Div       -- ^ @=  1@
  | Add       -- ^ @=  2@
  | Sub       -- ^ @=  3@
  | Pow       -- ^ @=  4@
  | Idiv      -- ^ @=  5@
  | Mod       -- ^ @=  6@
  | Eq_       -- ^ @=  7@
  | Neq       -- ^ @=  8@
  | Lt        -- ^ @=  9@
  | Gt        -- ^ @= 10@
  | Leq       -- ^ @= 11@
  | Geq       -- ^ @= 12@
  | Land      -- ^ @= 13@
  | Lor       -- ^ @= 14@
  | Shl       -- ^ @= 15@
  | Shr       -- ^ @= 16@
  | Bor       -- ^ @= 17@
  | Band      -- ^ @= 18@
  | Bxor      -- ^ @= 19@
  deriving (Eq,Enum,Bounded,Show)

data TresOp
  = TernCond  -- ^ @= 0@
  deriving (Eq,Enum,Bounded,Show)

--------------------------------------------------------------------------------

newtype BigUInt 
  = BigUInt [Word8]      -- ^ little endian

showBigUInt :: BigUInt -> String
showBigUInt (BigUInt bytes) = "0x" ++ concatMap f (reverse bytes) where
  f :: Word8 -> String
  f = printf "%02x"

instance Show BigUInt where show = showBigUInt

newtype InputNode 
  = InputNode Word32
  deriving (Show)
 
newtype ConstantNode 
  = ConstantNode BigUInt
  deriving (Show)

data UnoOpNode  = UnoOpNode  !UnoOp  !Word32                 deriving (Show)
data DuoOpNode  = DuoOpNode  !DuoOp  !Word32 !Word32         deriving (Show)
data TresOpNode = TresOpNode !TresOp !Word32 !Word32 !Word32 deriving (Show)

data Node 
  = AnInputNode    InputNode         -- @= 1@
  | AConstantNode  ConstantNode      -- @= 2@
  | AnUnoOpNode    UnoOpNode         -- @= 3@
  | ADuoOpNode     DuoOpNode         -- @= 4@
  | ATresOpNode    TresOpNode        -- @= 5@
  deriving (Show)

data SignalDescription = SignalDescription
  { signalOffset :: !Word32
  , signalLength :: !Word32
  }
  deriving (Show)

newtype WitnessMapping 
  = WitnessMapping { fromWitnessMapping :: [Word32] }
  deriving (Show)

type CircuitInputs = [(String, SignalDescription)]

data GraphMetaData = GraphMetaData 
  { witnessMapping :: WitnessMapping
  , inputSignals   :: CircuitInputs  
  }
  deriving (Show)

--------------------------------------------------------------------------------
