module Gitrea.Common (isMsbSet) where

import qualified Data.ByteString.Char8 as C
import Data.Bits (Bits, (.&.), (.|.), shiftL)

data GitRepository = GitRepository {
  getName :: String
} deriving (Show, Eq)

data Ref = Ref {
  getObjId :: C.ByteString
  , getRefName :: C.ByteString
} deriving (Show, Eq)

type WithRepository = ReaderT GitRepository IO

isMsbSet :: (Bits a, Num a) => a -> Bool
isMsbSet x = (x .&. 0x80) /= 0