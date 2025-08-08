module Gitrea.Common (isMsbSet) where

import Control.Monad.Reader (ReaderT)
import Data.Bits (Bits, shiftL, (.&.), (.|.))
import qualified Data.ByteString.Char8 as C

type WithRepository = ReaderT GitRepository IO

data GitRepository = GitRepository {
  getName :: String
} deriving (Show, Eq)

data Ref = Ref {
  getObjId :: C.ByteString
  , getRefName :: C.ByteString
} deriving (Show, Eq)

isMsbSet :: (Bits a, Num a) => a -> Bool
isMsbSet x = (x .&. 0x80) /= 0

fromOctets :: [Word8] -> Word32
fromOctets = foldl (\acc octect -> (acc `shiftL` 8) .|. fromIntegral octect) 0