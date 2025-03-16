
-- | Exporting the witness

{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
module Export where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Word
import Data.Array

import Control.Monad
import Data.Binary.Put

import qualified Data.ByteString.Lazy as L

import BN254 ( F , fromF , fieldPrime )
import Witness

--------------------------------------------------------------------------------

exportWitness :: FilePath -> Witness -> IO ()
exportWitness fpath (MkWitness ws) = do
  let bs = runPut (putWitness ws)
  L.writeFile fpath bs

exportFeltSequence :: FilePath -> Array Int F -> IO ()
exportFeltSequence fpath arr = do
  let bs = runPut $ putRawWitness (elems arr)
  L.writeFile fpath bs

--------------------------------------------------------------------------------

putHeader :: Int -> Put
putHeader witnessLen = do
  -- global header
  putWord32be 0x_77_74_6e_73                   -- magic word @"wtns"@
  putWord32le 2                                -- version
  putWord32le 2                                -- number of sections

  -- section 1
  putWord32le 1                                -- section id
  putWord64le 0x28                             -- section length
  putWord32le 32                               -- 32 bytes per field element
  putInteger32LE fieldPrime                    -- the field prime
  putWord32le (fromIntegral witnessLen)        -- number of witness elements

  -- section 2
  putWord32le 2                                -- section id
  putWord64le (32 * fromIntegral witnessLen)   -- section length

--------------------------------------------------------------------------------

arrayLength :: Array Int a -> Int
arrayLength arr = let (a,b) = bounds arr in (b-a+1)

putWitness :: Array Int F -> Put
putWitness xs = do 
  putHeader (arrayLength xs)
  putRawWitness (elems xs)

putRawWitness :: [F] -> Put
putRawWitness list = mapM_ putF list

putF :: F -> Put
putF x = putInteger32LE (fromF x)

putInteger32LE :: Integer -> Put
putInteger32LE x = forM_ [0..31] $ \k -> putWord8 (fromIntegral (shiftR x (k*8) .&. 0xff))

--------------------------------------------------------------------------------
