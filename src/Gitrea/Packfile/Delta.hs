{-# LANGUAGE DoAndIfThenElse #-}

module Gitrea.Packfile.Delta (
  patch
) where

import qualified Data.ByteString as B
import Data.Binary.Strict.Get
import Control.Monad (liftM, foldM)
import Data.Bits (Bits, (.&.), (.|.), shiftL)
import Gitrea.Common (isMsbSet)
import System.Environment (getArgs)
import Data.Word

data DeltaHeader = {
    sourceLength :: Int
  , targetLength :: Int
  , getOffset    :: Int 
} deriving (Show)

patch :: B.ByteString -> B.ByteString -> Either String B.ByteString
patch base delta = do
  header <- decodeDeltaHeader delta
  
  if B.length base == sourceLength header then
    fst $ runGet (run (getOffset header) base delta) delta
  else
    Left "Source length does not match base length"

run :: Int -> B.ByteString -> B.ByteString -> Get B.ByteString
run offset source delta = do
  skip offset
  command <- getWord8
  runCommand command B.empty source delta

runCommand :: Word8 -> B.ByteString -> B.ByteString -> t -> Get B.ByteString
runCommand command acc source delta = do
  result <- choose cmd
  finished <- isEmpty

  let acc' = B.append acc result

  if finished then
    return acc'
  else do
    command' <- getWord8
    runCommand command' acc' source delta

  where
    choose opcode | isMsbSet opcode = copyCommand opcode source
    choose opcode = insertCommand opcode

insertCommand :: Integral a => a -> Get B.ByteString
insertCommand = getByteString . fromIntegral

copyCommand :: Word8 -> B.ByteString -> Get B.ByteString
copyCommand opcode source = do
  (offset, length) <- readCopyInstruction opcode
  return $ copy length offset source

readCopyInstruction :: (Integral a) => Word8 -> Get (a, a)
readCopyInstruction opcode = do
  offset <- foldM readIfBitSet 0 $ zip [0x01, 0x02, 0x04, 0x08] [0,8..]
  len' <- foldM readIfBitSet 0 $ zip [0x10, 0x20, 0x40] [0,8..]
  let len = if coerce len' == 0 then 0x10000 else len'
  return $ (coerce offset, coerce len)

  where
    calculateVal off shift = if shift /= 0 then (\x -> off .|. (x `shiftL` shift)::Int) . fromIntegral else fromIntegral
    readIfBitSet off (test, shift) = if opcode .&. test /= 0 then liftM (calculateVal off shift) getWord8 else return off
    coerce = toEnum . fromEnum