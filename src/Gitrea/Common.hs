module Gitrea.Common (
  toHex
  , isMsbSet
  , pktLine
  , flushPkt
  , eitherToMaybe
  , fromOctets
  , GitRepository(..)
  , Ref(..)
  , ObjectId
  , WithRepository
) where

import Control.Monad.Reader
import Data.Bits (Bits, shiftL, (.&.), (.|.))
import qualified Data.ByteString.Char8 as C
import Numeric (showHex)
import Data.Bits (Bits, (.&.), (.|.), shiftL)
import Data.Word
import Text.Printf (printf)

type ObjectId = String

type WithRepository = ReaderT GitRepository IO

data GitRepository = GitRepository {
  getName :: String
} deriving (Show, Eq)

data Ref = Ref {
  getObjId :: C.ByteString
  , getRefName :: C.ByteString
} deriving (Show, Eq)

toHex :: (Integral a, Show a) => a -> String
toHex x = showHex x ""

pktLine :: String -> String
pktLine = printf "%04s%s" =<< toHex . (4 +) . length

flushPkt :: String
flushPkt = "0000"

isMsbSet :: (Bits a, Num a) => a -> Bool
isMsbSet x = (x .&. 0x80) /= 0

fromOctets :: [Word8] -> Word32
fromOctets = foldl (\acc octect -> (acc `shiftL` 8) .|. fromIntegral octect) 0

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Right x) = Just x
eitherToMaybe (Left _) = Nothing