{-# LANGUAGE DoAndIfThenElse #-}

module Gitrea.Packfile.Delta
  ( patch,
  )
where

import Control.Monad (foldM, liftM)
import Data.Binary.Strict.Get
import Data.Bits (Bits, shiftL, (.&.), (.|.))
import qualified Data.ByteString as B
import Data.Word
import Gitrea.Common (isMsbSet)
import System.Environment (getArgs)

data DeltaHeader = DeltaHeader
  { sourceLength :: Int,
    targetLength :: Int,
    getOffset :: Int
  }
  deriving (Show)

main = do
  (sourceFile : deltaFile : _) <- getArgs
  source <- B.readFile sourceFile
  delta <- B.readFile deltaFile
  header <- decodeDeltaHeader delta
  print header
  print $ B.length source
  either putStrLn (B.writeFile "target.file") $ patch source delta

patch :: B.ByteString -> B.ByteString -> Either String B.ByteString
patch base delta = do
  header <- decodeDeltaHeader delta

  if B.length base == sourceLength header
    then
      fst $ runGet (run (getOffset header) base delta) delta
    else
      Left "Source length does not match base length"

run :: Int -> B.ByteString -> B.ByteString -> Get B.ByteString
run offset source delta = do
  skip offset
  command <- getWord8
  runCommand command B.empty source delta

decodeDeltaHeader :: (Monad m) => B.ByteString -> m DeltaHeader
decodeDeltaHeader delta = do
  let res1 = runGet (decodeSize 0) delta
      (sourceBufferSize, offset) = either (const (0, 0)) id $ fst res1
      res2 = runGet (decodeSize offset) delta
      (targetBufferSize, offset') = either (const (0, 0)) id $ fst res2

  return (DeltaHeader sourceBufferSize targetBufferSize offset')
  where
    decodeSize offset = do
      skip offset
      byte <- getWord8
      next (maskMsb byte) 7 byte $ succ offset

    next base shift byte' count | isMsbSet byte' = do
      b <- getWord8
      let len = base .|. ((maskMsb b) `shiftL` shift)
      next len (shift + 7) b $ succ count
    next finalLen _ _ count = return (finalLen, count)

    maskMsb byte = fromIntegral $ byte .&. 0x7f

runCommand :: Word8 -> B.ByteString -> B.ByteString -> t -> Get B.ByteString
runCommand command acc source delta = do
  result <- choose command
  finished <- isEmpty

  let acc' = B.append acc result

  if finished
    then
      return acc'
    else do
      command' <- getWord8
      runCommand command' acc' source delta
  where
    choose opcode | isMsbSet opcode = copyCommand opcode source
    choose opcode = insertCommand opcode

insertCommand :: (Integral a) => a -> Get B.ByteString
insertCommand = getByteString . fromIntegral

copyCommand :: Word8 -> B.ByteString -> Get B.ByteString
copyCommand opcode source = do
  (offset, length) <- readCopyInstruction opcode
  return $ copy length offset source
  where
    copy len' offset' = B.take len' . B.drop offset'

readCopyInstruction :: (Integral a) => Word8 -> Get (a, a)
readCopyInstruction opcode = do
  offset <- foldM readIfBitSet 0 $ zip [0x01, 0x02, 0x04, 0x08] [0, 8 ..]
  len' <- foldM readIfBitSet 0 $ zip [0x10, 0x20, 0x40] [0, 8 ..]
  let len = if len' == 0 then 0x10000 else len'
  return $ (fromIntegral offset, fromIntegral len)
  where
    calculateVal off shift = (\x -> off .|. (x `shiftL` shift) :: Int) . fromIntegral
    readIfBitSet off (test, shift) = if opcode .&. test /= 0 then liftM (calculateVal off shift) getWord8 else return off