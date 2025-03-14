
-- | Parse JSON input files

module JSON where

--------------------------------------------------------------------------------

import Data.List
import Data.Foldable           as F
import Data.Foldable.WithIndex as FI

import Data.Map (Map)
import qualified Data.Map as Map 

import qualified Data.Text       as T
import qualified Data.ByteString as B

import Data.Aeson
import Data.Aeson.Key as Key
import Data.Scientific

--------------------------------------------------------------------------------

parseInputJsonValue :: Value -> Map String [Integer]
parseInputJsonValue val =
  case val of 
    Object obj -> Map.fromList (map worker (FI.itoList obj))
    _ -> error "parseInputJsonValue: expecting a JSON object"
  where

    worker (key,value) = (Key.toString key, flatten value)

    flatten :: Value -> [Integer]
    flatten val = case val of
      Number num -> numberToInteger num
      Array  arr -> F.concatMap flatten arr  
      String txt -> [read (T.unpack txt)]
      _          -> error "parseInputJsonValue/flatten: expecting a number or an array"

    numberToInteger x = case floatingOrInteger x of
      Right y -> [y]
      Left  _ -> error "parseInputJsonValue/numberToInteger: expecting an integer"

--------------------------------------------------------------------------------

loadInputJsonFile :: FilePath -> IO (Map String [Integer])
loadInputJsonFile fpath = do
  text <- B.readFile fpath
  let Just value = decodeStrict text
  return $ parseInputJsonValue value

--------------------------------------------------------------------------------
