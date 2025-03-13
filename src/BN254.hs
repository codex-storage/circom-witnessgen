
-- | The BN254 scalar field

{-# LANGUAGE Strict, BangPatterns #-} 
module BN254 where

--------------------------------------------------------------------------------

import Prelude hiding (div)
import qualified Prelude

import Data.Bits
import Data.Word
import Data.Ratio

import System.Random
import Text.Printf

import Misc

--------------------------------------------------------------------------------

fieldPrime :: Integer
fieldPrime = 21888242871839275222246405745257275088548364400416034343698204186575808495617

modP :: Integer -> Integer
modP x = mod x fieldPrime

halfPrimePlus1 :: Integer
halfPrimePlus1 = 1 + Prelude.div fieldPrime 2

--------------------------------------------------------------------------------

newtype F 
  = MkF Integer 
  deriving (Eq,Show)

fromF :: F -> Integer
fromF (MkF x) = x

-- from the circom docs: @val(z) = z-p  if p/2 +1 <= z < p@
signedFromF :: F -> Integer
signedFromF (MkF x) = if x >= halfPrimePlus1 then x - fieldPrime else x

toF :: Integer -> F
toF = MkF . modP

isZero :: F -> Bool
isZero (MkF x) = (x == 0)

fromBool :: Bool -> F
fromBool b = if b then 1 else 0

toBool :: F -> Bool
toBool = not . isZero

--------------------------------------------------------------------------------

neg :: F -> F
neg (MkF x) = toF (negate x)

add :: F -> F -> F
add (MkF x) (MkF y) = toF (x+y)

sub :: F -> F -> F
sub (MkF x) (MkF y) = toF (x-y)

mul :: F -> F -> F
mul (MkF x) (MkF y) = toF (x*y)

instance Num F where
  fromInteger = toF 
  negate = neg
  (+) = add
  (-) = sub
  (*) = mul
  abs = id
  signum _ = toF 1

square :: F -> F
square x = x*x

rndF :: IO F
rndF = MkF <$> randomRIO (0,fieldPrime-1)

--------------------------------------------------------------------------------

pow :: F -> Integer -> F
pow x0 exponent
  | exponent < 0  = error "power: expecting positive exponent"
  | otherwise     = go 1 x0 exponent
  where
    go !acc _ 0 = acc
    go !acc s e = go acc' s' (shiftR e 1) where
      s'   = s*s
      acc' = if e .&. 1 == 0 then acc else acc*s

invNaive :: F -> F
invNaive x = pow x (fieldPrime - 2)

divNaive :: F -> F -> F
divNaive x y = x * invNaive y

--------------------------------------------------------------------------------

instance Fractional F where
  fromRational q = fromInteger (numerator q) / fromInteger (denominator q)
  recip = inv
  (/)   = div

--------------------------------------------------------------------------------

fromBytesLE :: [Word8] -> F
fromBytesLE = toF . integerFromBytesLE

integerFromBytesLE :: [Word8] -> Integer
integerFromBytesLE = go where
  go []     = 0
  go (b:bs) = fromIntegral b + (shiftL (go bs) 8)

--------------------------------------------------------------------------------

instance ShowHex Integer where showHex = printf "0x%x"
instance ShowHex F       where showHex (MkF x) = showHex x

--------------------------------------------------------------------------------

-- | Inversion (using Euclid's algorithm)
inv :: F -> F
inv (MkF a) 
  | a == 0    = 0 -- error "field inverse of zero (generic prime)"
  | otherwise = MkF (euclid 1 0 a fieldPrime) 

-- | Division via Euclid's algorithm
div :: F -> F -> F
div (MkF a) (MkF b)
  | b == 0    = 0 -- error "field division by zero (generic prime)"
  | otherwise = MkF (euclid a 0 b fieldPrime) 

--------------------------------------------------------------------------------
-- * Euclidean algorithm

-- | Extended binary Euclidean algorithm
euclid :: Integer -> Integer -> Integer -> Integer -> Integer 
euclid !x1 !x2 !u !v = go x1 x2 u v where

  p = fieldPrime

  halfp1 = shiftR (p+1) 1

  modp :: Integer -> Integer
  modp n = mod n p

  -- Inverse using the binary Euclidean algorithm 
  euclid :: Integer -> Integer
  euclid a 
    | a == 0     = 0
    | otherwise  = go 1 0 a p
  
  go :: Integer -> Integer -> Integer -> Integer -> Integer
  go !x1 !x2 !u !v 
    | u==1       = x1
    | v==1       = x2
    | otherwise  = stepU x1 x2 u v

  stepU :: Integer -> Integer -> Integer -> Integer -> Integer
  stepU !x1 !x2 !u !v = if even u 
    then let u'  = shiftR u 1
             x1' = if even x1 then shiftR x1 1 else shiftR x1 1 + halfp1
         in  stepU x1' x2 u' v
    else     stepV x1  x2 u  v

  stepV :: Integer -> Integer -> Integer -> Integer -> Integer
  stepV !x1 !x2 !u !v = if even v
    then let v'  = shiftR v 1
             x2' = if even x2 then shiftR x2 1 else shiftR x2 1 + halfp1
         in  stepV x1 x2' u v' 
    else     final x1 x2  u v

  final :: Integer -> Integer -> Integer -> Integer -> Integer
  final !x1 !x2 !u !v = if u>=v

    then let u'  = u-v
             x1' = if x1 >= x2 then modp (x1-x2) else modp (x1+p-x2)
         in  go x1' x2  u' v 

    else let v'  = v-u
             x2' = if x2 >= x1 then modp (x2-x1) else modp (x2+p-x1)
         in  go x1  x2' u  v'

--------------------------------------------------------------------------------
