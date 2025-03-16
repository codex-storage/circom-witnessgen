
import pkg/constantine/math/arithmetic
import pkg/constantine/math/io/io_bigints
import pkg/constantine/math/io/io_fields
import pkg/constantine/named/properties_fields

#-------------------------------------------------------------------------------

type 
  B* = BigInt[254]
  F* = Fr[BN254Snarks]

const zeroB* : B = fromHex( BigInt[254], "0x00" )
const oneB*  : B = fromHex( BigInt[254], "0x01" )

const zeroF* : F = fromHex( Fr[BN254Snarks], "0x00" )
const oneF*  : F = fromHex( Fr[BN254Snarks], "0x01" )

func isZeroB*   (x: B )    : bool = bool(isZero(x))
func isZeroF*   (x: F )    : bool = bool(isZero(x))
func isNonZeroF*(x: F )    : bool = not isZeroF(x)
func isEqualF*  (x, y: F ) : bool = bool(x == y)
func `===`*     (x, y: F ) : bool = isEqualF(x,y)

const fieldMask*            : B = fromHex( BigInt[254] , "0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff", bigEndian )
const fieldPrime*           : B = fromHex( BigInt[254] , "0x30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000001", bigEndian )
const halfPrimePlus1*       : B = fromHex( BigInt[254] , "0x183227397098d014dc2822db40c0ac2e9419f4243cdcb848a1f0fac9f8000001", bigEndian )
const numberOfBitsAsBigInt* : B = fromHex( BigInt[254] , "0xfe", bigEndian )

#-------------------------------------------------------------------------------

func uintToB*(a: uint): B =
  var y : B
  y.fromUint(a)
  return y

func intToF*(a: int): F =
  var y : F
  y.fromInt(a)
  return y

func int64ToF*(a: int64): F =
  var y : F
  y.fromInt(a)
  return y

func boolToF*(b: bool): F = 
  return (if b: oneF else: zeroF)

func fToBool*(x: F): bool = 
  return isNonZeroF(x)

func bigToF*(big: B): F = 
  var x : F
  x.fromBig( big )
  return x

func fToBig*(x: F): B = 
  return x.toBig()

proc decimalToB*(s: string): B =
  var y: B
  let ok = y.fromDecimal(s)
  return y

proc decimalToF*(s: string): F = 
  return bigToF(decimalToB(s))
  # var y: F
  # let ok = y.fromDecimal(s)     # wtf nim
  # return y

func bigToDecimal*(x: B): string = 
  return toDecimal(x)

func fToDecimal*(x: F): string = 
  return toDecimal(x)

#-------------------------------------------------------------------------------

func negB* (y: B  ): B  =  ( var z : B = zeroB ; z -= y ; return z )
func negF* (y: F  ): F  =  ( var z : F = zeroF ; z -= y ; return z )
func invF* (y: F  ): F  =  ( var z : F = y ; if isNonZeroF(y): z.inv() ; return z )

func `+`*[n](x, y: BigInt[n] ): BigInt[n] = ( var z : BigInt[n] = x ; z += y ; return z )
func `-`*[n](x, y: BigInt[n] ): BigInt[n] = ( var z : BigInt[n] = x ; z -= y ; return z )

func `+`*(x, y: F ): F  =  ( var z : F = x ; z += y ; return z )
func `-`*(x, y: F ): F  =  ( var z : F = x ; z -= y ; return z )
func `*`*(x, y: F ): F  =  ( var z : F = x ; z *= y ; return z )
func `/`*(x, y: F ): F  =  ( var z : F = x ; z *= invF(y) ; return z )

#-------------------------------------------------------------------------------
