
{-# LANGUAGE Strict #-}
module Misc where

--------------------------------------------------------------------------------

import Data.Bits

--------------------------------------------------------------------------------

class ShowHex a where
  showHex  :: a -> String

printHex :: ShowHex a => a -> IO ()
printHex x = putStrLn (showHex x)

--------------------------------------------------------------------------------
-- Integer logarithm

-- | Largest integer @k@ such that @2^k@ is smaller or equal to @n@
integerLog2 :: Integer -> Int
integerLog2 n = go n where
  go 0 = -1
  go k = 1 + go (shiftR k 1)

-- | Smallest integer @k@ such that @2^k@ is larger or equal to @n@
ceilingLog2 :: Integer -> Int
ceilingLog2 0 = 0
ceilingLog2 n = 1 + go (n-1) where
  go 0 = -1
  go k = 1 + go (shiftR k 1)
  
--------------------------------------------------------------------------------
