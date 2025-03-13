
-- | Parsing the graph binary format

{-# LANGUAGE Strict, PackageImports, BangPatterns #-}
module Parser where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Word
import Data.List
import Data.Ord

import Control.Monad
import Control.Applicative
import System.IO

import Data.ByteString.Lazy (ByteString) 
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as LC

import "binary" Data.Binary.Get
import "binary" Data.Binary.Builder as Builder

import Graph

--------------------------------------------------------------------------------

{-
test :: IO ()
test = do
  Right graph <- parseGraphFile "../tmp/graph.bin"
  print graph
-}

{-
nodeEx1  = 0x06 : nodeEx1'            :: [Word8]
nodeEx2  = 0x08 : nodeEx2'            :: [Word8]

nodeEx1' = 0x22 : 0x04 : duoNodeEx1   :: [Word8]
nodeEx2' = 0x22 : 0x06 : duoNodeEx2   :: [Word8]

duoNodeEx1 = [ 0x10 , 0x05 , 0x18 , 0x05 ]                 :: [Word8]
duoNodeEx2 = [ 0x08 , 0x02 , 0x10 , 0x03 , 0x18 , 0x04 ]   :: [Word8]
-}

--------------------------------------------------------------------------------

type Msg = String

parseGraphFile :: FilePath -> IO (Either Msg Graph)
parseGraphFile fname = do
  h    <- openBinaryFile fname ReadMode 
  ei   <- readGraphFile h
  hClose h
  return ei

hGetBytes :: Handle -> Int -> IO ByteString
hGetBytes h n = L.hGet h (fromIntegral n)

hSeekInt :: Handle -> Int -> IO ()
hSeekInt h ofs = hSeek h AbsoluteSeek (fromIntegral ofs)

readGraphFile :: Handle -> IO (Either Msg Graph)
readGraphFile h = do
  flen  <- (fromIntegral :: Integer -> Int) <$> hFileSize h 
  magic <- hGetBytes h (length magicHeader)
  if magic /= LC.pack magicHeader
    then return $ Left "magic header not found or invalid"
    else do
      hSeekInt h (flen - 8)
      offset <- (fromIntegral . runGet getWord64le) <$> hGetBytes h 8
      putStrLn $ "metadata offset = " ++ show offset
      if (offset >= flen) || (offset <= 18)
        then return $ Left "invalid final `graphMetaData` offset bytes"
        else do
          hSeekInt h (length magicHeader)
          part1 <- hGetBytes h (offset - length magicHeader)
          part2 <- hGetBytes h (flen - offset - 8)
          return (Right $ Graph (parseNodes part1) (parseMeta part2))

magicHeader :: String
magicHeader = "wtns.graph.001"

parseNodes :: ByteString -> [Node]
parseNodes = runGet getNodes

parseMeta :: ByteString -> GraphMetaData
parseMeta = runGet getMetaData

--------------------------------------------------------------------------------

varInt' :: Get Word64
varInt' = go 0 where
  go !cnt = if cnt >= 8 then return 0 else do
    w <- getWord8
    if (w < 128) 
      then return (fromIntegral w)
      else do
        let x = fromIntegral (w .&. 127) 
        y <- go (cnt+1)
        return (x + 128*y)

varInt :: Get Int
varInt = fromIntegral <$> varInt'

varUInt :: Get Word32
varUInt = fromIntegral <$> varInt'

--------------------------------------------------------------------------------

getNodes :: Get [Node]
getNodes = do
  n <- getWord64le 
  replicateM (fromIntegral n) getNode

-- | with varint length prefix
getNode :: Get Node
getNode = do
  len <- varInt
  bs  <- getLazyByteString (fromIntegral len)
  return (runGet getNode' bs)

-- | without varint length prefix
getNode' :: Get Node
getNode' = do
  nodetype <- getFieldId LEN
  case nodetype of
    1 -> AnInputNode   <$> getInputNode   
    2 -> AConstantNode <$> getConstantNode
    3 -> AnUnoOpNode   <$> getUnoOpNode   
    4 -> ADuoOpNode    <$> getDuoOpNode   
    5 -> ATresOpNode   <$> getTresOpNode  
    _ -> error "unexpected node type"

getInputNode :: Get InputNode
getInputNode = do
  SomeNode idx _ _ _ <- getSomeNode 
  return (InputNode idx)

getConstantNode :: Get ConstantNode
getConstantNode = do
  len  <- varInt
  bs   <- getLazyByteString (fromIntegral len)
  return $ ConstantNode (runGet getBigUInt bs)

getBigUInt :: Get BigUInt
getBigUInt = do
  fld <- getFieldId LEN
  if fld /= 1 
    then error "getBigUInt"
    else do
      len <- varInt
      bs  <- getLazyByteString (fromIntegral len)
      return $ BigUInt (runGet getByteList bs)

getByteList :: Get [Word8]
getByteList = do
  fld <- getFieldId LEN
  if fld /= 1 
    then error "getByteList"
    else do
      len <- varInt
      bs  <- getLazyByteString (fromIntegral len)
      return (L.unpack bs)

getUnoOpNode :: Get UnoOpNode
getUnoOpNode = do
  SomeNode op arg1 _ _ <- getSomeNode 
  return (UnoOpNode (wordToEnum op) arg1)

getDuoOpNode :: Get DuoOpNode
getDuoOpNode = do
  SomeNode op arg1 arg2 _ <- getSomeNode 
  return (DuoOpNode (wordToEnum op) arg1 arg2)

getTresOpNode :: Get TresOpNode
getTresOpNode = do
  SomeNode op arg1 arg2 arg3 <- getSomeNode 
  return (TresOpNode (wordToEnum op) arg1 arg2 arg3)

wordToEnum :: Enum a => Word32 -> a
wordToEnum w = toEnum (fromIntegral w)

--------------------------------------------------------------------------------

data SomeNode = SomeNode
  { field1 :: Word32
  , field2 :: Word32
  , field3 :: Word32
  , field4 :: Word32
  }
  deriving Show

defaultSomeNode :: SomeNode
defaultSomeNode = SomeNode 0 0 0 0

insert1 :: (Int,Word32) -> SomeNode -> SomeNode
insert1 (idx,val) old = case idx of
  1 -> old { field1 = val }
  2 -> old { field2 = val } 
  3 -> old { field3 = val } 
  4 -> old { field4 = val }

insertMany :: [(Int,Word32)] -> SomeNode -> SomeNode
insertMany list old = foldl' (flip insert1) old list

getSomeNode :: Get SomeNode
getSomeNode = do
  len  <- varInt
  bs   <- getLazyByteString (fromIntegral len)
  let list = runGet getRecord bs
  return $ insertMany list defaultSomeNode 

--------------------------------------------------------------------------------
-- TODO: refactor this mess

getMetaData :: Get GraphMetaData
getMetaData = do
  len <- varInt
  mapping <- getWitnessMapping
  inputs  <- getCircuitInputs
  return $ GraphMetaData mapping inputs

getWitnessMapping :: Get WitnessMapping
getWitnessMapping = do
  fld <- getFieldId LEN 
  if fld /= 1 
    then error "getWitnessMapping: expecting field 1"
    else do
      len <- varInt
      bs  <- getLazyByteString (fromIntegral len)
      return $ WitnessMapping (runGet worker bs)
  where
    worker :: Get [Word32]
    worker = isEmpty >>= \b -> if b 
      then return [] 
      else (:) <$> varUInt <*> worker

getCircuitInputs :: Get CircuitInputs
getCircuitInputs = worker where

  worker :: Get [(String, SignalDescription)]
  worker = isEmpty >>= \b -> if b 
    then return [] 
    else (:) <$> getSingleInput <*> worker

  getSingleInput :: Get (String, SignalDescription)
  getSingleInput = do
    fld <- getFieldId LEN 
    if fld /= 2
      then error "getCircuitInputs: expecting field 2"
      else do
        len <- varInt
        bs  <- getLazyByteString (fromIntegral len)
        return $ runGet inputHelper bs

  inputHelper = do
    name   <- getName
    signal <- getSignal
    return (name,signal)

  getName :: Get String
  getName = do
    fld <- getFieldId LEN 
    if fld /= 1
      then error "getCircuitInputs/getName: expecting field 1"
      else do
        len <- varInt
        bs  <- getLazyByteString (fromIntegral len)
        return (LC.unpack bs)

  getSignal :: Get SignalDescription
  getSignal = do
    fld <- getFieldId LEN 
    if fld /= 2
      then error "getCircuitInputs/getSignal: expecting field 2"
      else do
        len <- varInt
        bs  <- getLazyByteString (fromIntegral len)
        return $ runGet signalHelper bs

  signalHelper = do
    ofs <- getSignalOffset
    len <- getSignalLength
    return $ SignalDescription { signalOffset = ofs , signalLength = len }

  getSignalOffset = do
    fld <- getFieldId VARINT
    if fld /= 1
      then error "getCircuitInputs/getSignalOffset: expecting field 1"
      else varUInt

  getSignalLength = do
    fld <- getFieldId VARINT
    if fld /= 2
      then error "getCircuitInputs/getSignalLength: expecting field 2"
      else varUInt

--------------------------------------------------------------------------------
-- * protobuf stuff

-- | There are six wire types: VARINT, I64, LEN, SGROUP, EGROUP, and I32
data WireType
  = VARINT    -- ^ used for: int32, int64, uint32, uint64, sint32, sint64, bool, enum
  | I64       -- ^ used for: fixed64, sfixed64, double
  | LEN       -- ^ used for: string, bytes, embedded messages, packed repeated fields
  | SGROUP    -- ^ used for: group start (deprecated)
  | EGROUP    -- ^ used for: group end (deprecated)
  | I32       -- ^ fixed32, sfixed32, float
  deriving (Eq,Show,Enum,Bounded)

type FieldId = Int

getFieldId :: WireType -> Get FieldId
getFieldId wty = do
  tag <- getWord8
  let (fld,wty') = decodeTag tag
  if wty == wty' 
    then return fld
    else error "getFieldId: unexpected protobuf wire type"

decodeTag_ :: Word8 -> FieldId
decodeTag_ = fst . decodeTag

decodeTag :: Word8 -> (FieldId, WireType)
decodeTag w = (fld , wty) where
  fld = fromIntegral (shiftR w 3) 
  wty = toEnum (fromIntegral (w .&. 7))

-- (index, value) pair
getEntry :: Get (Int,Word32)
getEntry = do
  idx <- getFieldId VARINT
  val <- varUInt
  return (idx,val)

-- list of (index, value) pairs
getRecord :: Get [(Int,Word32)]
getRecord = sort <$> go where
  go = isEmpty >>= \b -> if b
    then return []
    else (:) <$> getEntry <*> go

--------------------------------------------------------------------------------
