
-- | See <https://docs.circom.io/circom-language/basic-operators> for the official
-- semantics of the operations

{-# LANGUAGE StrictData, DeriveFunctor #-}
module Semantics where

--------------------------------------------------------------------------------

import Data.Bits

import BN254
import Misc

--------------------------------------------------------------------------------

data PrimOp a
  -- unary
  = Neg  a
  | Id   a
  | Lnot a
  | Bnot a
  -- binary
  | Mul  a a
  | Div  a a
  | Add  a a
  | Sub  a a
  | Pow  a a
  | Idiv a a
  | Mod  a a
  | Eq_  a a
  | Neq  a a
  | Lt   a a
  | Gt   a a
  | Leq  a a
  | Geq  a a
  | Land a a
  | Lor  a a
  | Shl  a a
  | Shr  a a
  | Bor  a a
  | Band a a
  | Bxor a a
  -- ternary
  | Cond a a a
  deriving (Show,Functor)

--------------------------------------------------------------------------------

evalPrimOp :: PrimOp F -> F
evalPrimOp prim = case prim of
  Neg  x     -> neg x
  Id   x     -> x
  Lnot x     -> fromBool (not (toBool x))
  Bnot x     -> toF (fieldMask .&. (negate (fromF x) - 1)) 
  Mul  x y   -> mul x y
  Div  x y   -> BN254.div x y
  Add  x y   -> add x y
  Sub  x y   -> sub x y
  Pow  x y   -> pow x (fromF y)
  Idiv x y   -> if isZero y then 0 else toF $ Prelude.div (fromF x) (fromF y)
  Mod  x y   -> if isZero y then 0 else toF $ Prelude.mod (fromF x) (fromF y)
  Eq_  x y   -> fromBool (x == y)
  Neq  x y   -> fromBool (x /= y)
  Lt   x y   -> fromBool (signedFromF x <  signedFromF y)
  Gt   x y   -> fromBool (signedFromF x >  signedFromF y)
  Leq  x y   -> fromBool (signedFromF x <= signedFromF y)
  Geq  x y   -> fromBool (signedFromF x >= signedFromF y)
  Land x y   -> fromBool (toBool x && toBool y)
  Lor  x y   -> fromBool (toBool x || toBool y)
  Shl  x y   -> shiftLeft  x (fromF y)
  Shr  x y   -> shiftRight x (fromF y)
  Bor  x y   -> toF (fromF x  .|.  fromF y)
  Band x y   -> toF (fromF x  .&.  fromF y)
  Bxor x y   -> toF (fromF x `xor` fromF y) 
  Cond b x y -> if toBool b then x else y

--------------------------------------------------------------------------------

fieldMask :: Integer
fieldMask = shiftL 1 fieldBits - 1

fieldBits :: Int
fieldBits = ceilingLog2 fieldPrime

shiftRight :: F -> Integer -> F
shiftRight (MkF x) k = if k < halfPrimePlus1
  then if k > fromIntegral fieldBits 
    then 0 
    else MkF (shiftR x (fromInteger k))
  else shiftLeft (MkF x) (fieldPrime - k)

shiftLeft :: F -> Integer -> F
shiftLeft (MkF x) k = if k < halfPrimePlus1
  then if k > fromIntegral fieldBits
    then 0
    else toF (shiftL x (fromIntegral k) .&. fieldMask)
  else shiftRight (MkF x) (fieldPrime - k)

--------------------------------------------------------------------------------

{-
notYet :: a
notYet = error "not yet implemented"
-}
