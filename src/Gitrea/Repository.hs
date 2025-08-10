{-# LANGUAGE OverloadedStrings, DoAndIfThenElse #-}

module Gitrea.Repository (
  checkoutHead
  , readHead
  , resolveTree
) where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Text.Printf (printf)
import Gitrea.Common (GitRepository(..), ObjectId, WithRepository)
import Numeric (readOct)
import Gitrea.Store.Object
import Gitrea.Store.ObjectStore
import Gitrea.Store.Index
import Gitrea.Store.Index (IndexEntry, GitFileMode(..), writeIndex, indexEntryFor)
import System.FilePath
import System.Directory
import System.Posix.Files
import Control.Monad.Reader

checkoutHead :: WithRepository ()
checkoutHead = do
  repo <- ask
  let dir = getName repo
  tip <- readHead
  maybeTree <- resolveTree tip
  indexEntries <- maybe (return []) (walkTree [] dir) maybeTree
  writeIndex indexEntries

walkTree :: [IndexEntry] -> FilePath -> Tree -> WithRepository [IndexEntry]
walkTree acc parent tree = do
  let entries = getEntries tree
  foldM handleEntry acc entries
  where handleEntry acc' (TreeEntry "40000" path sha') = do
                            let dir = parent </> toFilePath path
                            liftIO $ createDirectory dir
                            maybeTree <- resolveTree $ toHex sha'
                            maybe (return acc') (walkTree acc' dir) maybeTree
        handleEntry acc' (TreeEntry mode path sha') = do
                            repo <- ask
                            let fullPath = parent </> toFilePath path
                            content <- liftIO $ readObject repo $ toHex sha'
                            maybe (return acc') (\e -> do
                                    liftIO $ B.writeFile fullPath (getBlobContent e)
                                    let fMode = fst . head . readOct $ C.unpack mode
                                    liftIO $ setFileMode fullPath fMode
                                    indexEntry <- asIndexEntry fullPath sha'
                                    return $ indexEntry : acc') content
        toFilePath = C.unpack
        asIndexEntry path sha' = do
              stat <- liftIO $ getFileStatus path
              indexEntryFor path Regular sha' stat

resolveTree :: ObjectId -> WithRepository (Maybe Tree)
resolveTree sha' = do
        repo <- ask
        blob <- liftIO $ readObject repo sha'
        maybe (return Nothing) walk blob
  where walk (Object _ BTree sha1) = do
                                     repo <- ask
                                     liftIO $ readTree repo sha1
        walk c@(Object _ BCommit _) = do
                                       let maybeCommit = parseCommit $ getBlobContent c
                                       maybe (return Nothing) extractTree maybeCommit
        walk _                      = return Nothing

extractTree :: Commit -> WithRepository (Maybe Tree)
extractTree commit = do
  let sha' = C.unpack $ getTree commit
  repo <- ask
  liftIO $ readTree repo sha'

toHex :: C.ByteString -> String
toHex bytes = C.unpack bytes >>= printf "%02x"

readHead :: WithRepository ObjectId
readHead = readSymRef "HEAD"