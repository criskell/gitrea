{-# LANGUAGE OverloadedStrings, BangPatterns, NoMonomorphismRestriction, RecordWildCards #-}

module Gitrea.Store.Index (
  writeIndex
  , readIndex
  , indexEntryFor
  , GitFileMode(..)
  , IndexEntry(path)
) where

import Prelude hiding (take, takeWhile)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Codec.Binary.UTF8.String as CS (encode)
import qualified Crypto.Hash.SHA1 as SHA1
import Data.Char (ord)
import Data.Function (on)
import Data.List (sortBy)
import Gitrea.Store.ObjectStore (getGitDirectory)
import Gitrea.Common
import System.FilePath
import Data.Word
import Data.Int
import Data.Bits
import Data.Binary.Builder
import Data.Binary.Get
import Control.Monad.Reader
import System.Posix.Files
import Data.Monoid
import Data.Binary
import Text.Printf

data Index = Index {
  getIndexEntries :: [IndexEntry]
} deriving (Show, Eq)

data IndexEntry = IndexEntry {
  ctime :: Int64
  , mtime :: Int64
  , device :: Word64
  , inode :: Word64
  , mode :: Word32
  , uid :: Word32
  , gid :: Word32
  , size :: Int64
  , sha :: [Word8]
  , gitFileMode :: GitFileMode
  , path :: String
} deriving (Eq)

instance Show IndexEntry where
  show IndexEntry{..} = printf ("%s\n  ctime: %d\n  mtime: %d\n  dev: %d  inode: %d\n" ++
                              "  uid: %8d  gid: %d\n  size: %7d  git file mode: %s\n  sha1: %s")
                          path ctime mtime device inode uid gid size (show gitFileMode) (toHex' sha)

toHex' :: [Word8] -> String
toHex' = (printf "%02x" =<<)

instance Binary IndexEntry where
  put (IndexEntry cs ms dev inode' mode' uid' gid' size' sha' gitFileMode' name')
      = do
          put $ coerce cs
          put zero
          put $ coerce ms
          put zero
          put $ coerce dev
          put $ coerce inode'
          put $ toMode gitFileMode' mode'
          put $ coerce uid'
          put $ coerce gid'
          put $ coerce size'
          mapM_ put sha'
          put flags
          mapM_ put finalPath
      where zero = 0 :: Word32
            pathName = name'
            coerce x = (toEnum $ fromEnum x) :: Word32
            toMode gfm fm = (objType gfm `shiftL` 12) .|. permissions gfm fm
            flags = (((toEnum . length $ pathName)::Word16) .&. 0xFFF) :: Word16
            objType Regular = 8 :: Word32
            objType SymLink = 10 :: Word32
            objType GitLink = 14 :: Word32
            permissions Regular fm = fromIntegral fm :: Word32
            permissions _ _ = 0 :: Word32
            !finalPath = let n = CS.encode (pathName ++ "\0")
                             toPad = 8 - ((length n - 2) `mod` 8)
                             pad = C.replicate toPad '\NUL'
                             padded = if toPad /= 8 then n ++ B.unpack pad else n
                          in padded
  get = readIndexEntry

readIndex :: FilePath -> IO [IndexEntry]
readIndex fullPath = do
  content <- L.readFile fullPath
  (_, _, num) <- return $ runGet readHeader content
  return $ readMany [] (L.drop 12 content) 0 num
  where readMany acc remaining' offset toRead | toRead > 0 = do
          (ie, bs, consumed) <- return $ runGetState readIndexEntry remaining' offset
          readMany (ie : acc) bs (consumed + offset) (toRead - 1)
        readMany acc _ _ _ = acc
        readHeader = do
          magic <- getWord32be
          version <- getWord32be
          num <- getWord32be
          return (magic, version, num)

readIndexEntry :: Get IndexEntry
readIndexEntry = do
  ctime' <- getWord64be
  mtime' <- getWord64be
  device' <- getWord32be
  inode' <- getWord32be
  mode' <- getWord32be
  uid' <- getWord32be
  gid' <- getWord32be
  size' <- getWord32be
  sha' <- replicateM 20 getWord8
  (pathLength, _stage) <- toFlags
  let toPad = 8 - ((pathLength - 2) `mod` 8)
      objType = toGitFileMode (mode' `shiftR` 12)
  path' <- getByteString (coerce pathLength)
  _ <- getByteString (coerce toPad)
  return $ IndexEntry (coerce $ ctime' `shiftR` 32) (coerce $ mtime' `shiftR` 32) (coerce device')
                      (coerce inode') (coerce mode') (coerce uid')
                      (coerce gid') (coerce size') sha' objType (C.unpack path')
  where coerce = fromIntegral
        toFlags = do
                word16 <- getWord16be
                let pathLength = (word16 .&. 0xFFF) :: Word16
                    stage = (word16 `shiftR` 12) .&. 3 :: Word16
                return (pathLength, stage)
        toGitFileMode :: Word32 -> GitFileMode
        toGitFileMode 10 = SymLink
        toGitFileMode 14 = GitLink
        toGitFileMode _ = Regular

writeIndex :: [IndexEntry] -> WithRepository ()
writeIndex [] = return ()
writeIndex entries = do
  fullPath <- indexFilePath
  content <- encodeIndex $ Index entries
  liftIO $ B.writeFile fullPath content

indexFilePath :: WithRepository FilePath
indexFilePath = do
  repo <- ask
  return $ getGitDirectory repo </> "index"

indexEntryFor :: FilePath -> GitFileMode -> B.ByteString -> FileStatus -> WithRepository IndexEntry
indexEntryFor filePath gitFileMode' sha' stat = do
  repo <- ask
  let fileName = makeRelativeToRepoRoot (getName repo) filePath
  return $ IndexEntry (coerce $ statusChangeTime stat) (coerce $ modificationTime stat)
                  (coerce $ deviceID stat) (coerce $ fileID stat) (coerce $ fileMode stat)
                  (coerce $ fileOwner stat) (coerce $ fileGroup stat) (coerce $ fileSize stat)
                  (B.unpack sha') gitFileMode' fileName
  where coerce = fromIntegral . fromEnum

data GitFileMode = Regular | SymLink | GitLink deriving (Eq, Show)

makeRelativeToRepoRoot :: String -> FilePath -> FilePath
makeRelativeToRepoRoot repoName path' =
  joinPath $ dropWhile (== repoName) $ dirs path'
  where dirs = splitDirectories . normalise

encodeIndex :: Index -> WithRepository B.ByteString
encodeIndex toWrite = do
  let indexEntries = sortIndexEntries $ getIndexEntries toWrite
      numEntries = toEnum . fromEnum $ length indexEntries
      header = indexHeader numEntries
      entries = mconcat $ map encode indexEntries
      idx = toLazyByteString header `L.append` entries
  return $ lazyToStrictBS idx `B.append` SHA1.hashlazy idx

sortIndexEntries :: [IndexEntry] -> [IndexEntry]
sortIndexEntries = sortBy (compare `on` path)

lazyToStrictBS :: L.ByteString -> B.ByteString
lazyToStrictBS x = B.concat $ L.toChunks x

indexHeader :: Word32 -> Builder
indexHeader num =
  putWord32be magic
  <> putWord32be 2
  <> putWord32be num
  where magic = fromOctets $ map (fromIntegral . ord) "DIRC"