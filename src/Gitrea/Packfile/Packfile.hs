{-# LANGUAGE OverloadedStrings, DoAndIfThenElse, BangPatterns #-}

module Gitrea.Packfile.Packfile where

import qualified Data.Iteratee as I

parsePackfile :: I.Iterate ByteString IO Packfile
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
  return $ (\t -> PackfileObject t siz' content) <$> normalizedObjectType

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