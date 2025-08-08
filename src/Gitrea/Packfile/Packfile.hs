{-# LANGUAGE OverloadedStrings, DoAndIfThenElse, BangPatterns #-}

module Gitrea.Packfile.Packfile (
  packRead
  , Packfile(..)
  , PackfileObject(..)
  , PackObjectType(..)
) where

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.Iteratee as It
import Data.Iteratee.Binary
import Data.Iteratee.ZLib
import Control.Monad (replicateM)
import Data.Maybe
import Data.Char
import Data.Word (Word8, Word32)
import Data.Bits
import Gitrea.Common (isMsbSet, fromOctets)

type Content = ByteString

data Packfile = Packfile {
  version :: Word32
  , numObjects :: Word32
  , objects :: [PackfileObject]
} | InvalidPackfile deriving (Show, Eq)

data PackfileObject = PackfileObject {
  objectType :: PackfileObjectType
  , size :: Int
  , objectData :: Content
} deriving (Show, Eq)

data PackfileObjectType =
  OBJ_COMMIT
  | OBJ_TREE
  | OBJ_NONE
  | OBJ_BLOB
  | OBJ_TAG
  | OBJ_OFS_DELTA Int
  | OBJ_REF_DELTA [Word8]
  | OBJ_ANY
  | OBJ_MAX deriving (Eq, Show, Ord)

packRead :: FilePath -> IO Packfile
packRead path = It.fileDriverRandom parsePackfile

parsePackfile :: I.Iteratee ByteString IO Packfile
parsePackfile = do
  magic <- endianRead4 MSB -- 4 bytes, big-endian
  version' <- endianRead4 MSB
  numObjects' <- endianRead4 MSB

  if packMagic == magic
    then parseObjects version' numObjects'
    else return InvalidPackFile

  where
    packMagic = fromOctets $ map (fromIntegral . ord) "PACK"

parsePackObject :: It.Iteratee ByteString IO (Maybe PackfileObject)
parsePackObject = do
  byte <- It.head

  let objectType' = byte `shiftR` 4 .&. 0b111
      initial = fromIntegral $ byte .&. 15

  size' <- if isMsbSet byte then parseObjectSize initial 0 else return initial
  normalizedObjectType <- toPackObjectType objectType'

  !content <- It.joinI $ enumInflate Zlib defaultDecompressParams It.stream2stream
  return $ (\t -> PackfileObject t size' content) <$> normalizedObjectType

isMsbSet :: Word8 -> Bool
isMsbSet x = (x .&. 0b10000000) /= 0

parseObjectSize size' iterations = do
  nextByte <- I.head
  
  let add = (coerce (nextByte .&. 127) :: Int) `shiftL` (4 + iterations * 7)
      acc = size' + fromIntegral add

  if isMsbSet nextByte then
    parseObjectSize acc (iterations + 1)
  else
    return acc

  where coerce = toEnum . fromEnum

toPackObjectType :: (Show a, Integral a) => a -> I.Iteratee ByteString IO (Maybe PackObjectType)
toPackObjectType 1 = return $ Just OBJ_COMMIT
toPackObjectType 2 = return $ Just OBJ_TREE
toPackObjectType 3 = return $ Just OBJ_BLOB
toPackObjectType 4 = return $ Just OBJ_TAG
toPackObjectType 6 = do
  offset <- readOffset 0 0
  return $ Just (OBJ_OFS_DELTA offset)
toPackObjectType 7 = do
  baseObject <- replicateM 20 I.head
  return $ Just (OBJ_REF_DELTA baseObject)
toPackObjectType _ = return Nothing

-- FIXME: Isso está usando little-endian, mas deveria usar big-endian.
-- https://github.com/git/git/blob/2c2ba49d55ff26c1082b8137b1ec5eeccb4337d1/packfile.c#L1245-L1258
readOffset :: Int -> Int -> It.Iteratee ByteString IO Int
readOffset shift acc = do
  x <- It.head

  let bs = acc + ((coerce (x .&. 127) :: Int) `shiftL` shift)

  if isMsbSet x
    then readOffset (shift + 7) (bs + 1)
    else return bs
  where coerce = toEnum . fromEnum