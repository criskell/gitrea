{-#- LANGUAGE OverloadedStrings, RecordWildcards, DoAndIfThenElse #-}

module Gitrea.Store.ObjectStore (
  createEmptyGitRepository
  , pathForObject
  , pathForPack
  , createGitRepositoryFromPackfile
  , updateHead
  , readTree
  , readObject
  , readSymRef
  , createRef
  , getGitRepository
) where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Codec.Compression.Zlib as Z
import qualified Crypto.Hash.SHA1 as SHA1
import Data.Maybe (isJust, fromJust, isNothing)
import Text.Printf (printf)
import Data.Char (isSpace)
import Gitrea.Pack.Packfile
import Gitrea.Pack.Delta (patch)
import Gitrea.Common (GitRepository(..), ObjectId, WithRepository, Ref(..))
import Gitrea.Store.Object
import System.FilePath
import System.Directory
import Data.Foldable (forM_)
import Data.List (find, partition)
import Control.Monad.Reader hiding (forM_)

createGitRepositoryFromPackfile :: FilePath -> [Ref] -> WithRepository ()
createGitRepositoryFromPackfile packFile refs = do
  pack <- liftIO $ packRead packFile
  unpackPackfile pack
  createRefs refs
  updateHead refs

encodeObject :: ObjectType -> C.ByteString -> (ObjectId, C.ByteString)
encodeObject objectType content = do
  let header = headerForBlob (C.pack $ show objectType)
      blob = header `C.append` content
      sha1 = hsh blob

  (sha1, blob)
  
  where
    headerForBlob objType = objType `C.append` " " `C.append` C.pack (show $ C.length content) `C.append` "\0"
    hsh = toHex . SHA1.hash

writeObject :: GitRepository -> ObjectType -> C.ByteString -> IO FilePath
writeObject GitRepository{..} objectType content = do
  let (sha1, blob) = encodeObject objectType content
      (path, name) = pathForObject getName sha1
      filename = path </> name

  _ <- createDirectoryIfMissing True path
  L.writeFile filename $ compress blob
  return filename

  where
    compress data' = Z.compress $ L.fromChunks [data']

pathForObject :: String -> String -> (FilePath, String)
pathForObject repoName sha1 | length sha1 == 40 = (repoName </> ".git" </> "objects" </> pre, rest)
  where
    pre = take 2 sha1
    rest = drop 2 sha1
pathForObject _ _ = ("", "")

unpackPackfile :: Packfile -> WithRepository ()
unpackPackfile InvalidPackfile = error "Attempting to unpack an invalid packfile"
unpackPackfile (Packfile _ _ objs) = do
  repo <- ask
  unresolvedObjects <- writeObjects objs
  liftIO $ forM_ unresolvedObjects $ writeDelta repo

  where
    writeObjects (x@(PackfileObject (OBJ_REF_DELTA _) _ _):xs) = liftM (x:) (writeObjects xs)
    writeObjects (PackfileObject objType _ content : xs) = do
      repo <- ask
      _ <- liftIO $ writeObject repo (tt objType) content
      writeObjects xs
    writeObjects [] = return []

    tt OBJ_COMMIT = BCommit
    tt OBJ_TREE = BTree
    tt OBJ_BLOB = BBlob
    tt OBJ_TAG = BTag
    tt _ = error "Unexpected blob type"

    writeDelta repo (PackfileObject ty@(OBJ_REF_DELTA _) _ content) = do
      base <- case toObjectId ty of
        Just sha1 -> liftIO $ readObject repo sha1
        _ -> Nothing

      if isJust base then
        case patch (getBlobContent $ fromJust base) content of
          Right target -> do
            let base' = fromJust base
                fileName <- writeObject repo (objType base') target
                return $ Just fileName
          Left _ -> return Nothing
      else
        return Nothing
    
    writeDelta _repo_ = error "Dont' expect a resolved object here"

updateHead :: [Ref] -> WithRepository ()
updateHead [] = fail "Unexpected invalid packfile"
updateHead refs = do
  let maybeHead = findHead refs
  unless (isNothing maybeHead) $
    let sha1 = C.unpack $ getObjId $ fromJust maybeHead
        ref = maybe "refs/heads/master" (C.unpack . getRefName) $ findRef sha1 refs
        in
          do
            createRef ref sha1
            createSymRef "HEAD" ref
  where
    findHead = find (\Ref{..} -> "HEAD" == getRefName)
    findRef sha = find (\Ref{..} -> ("HEAD" /= getRefName && sha == (C.unpack getObjId)))

createSymRef :: String -> String -> WithRepository ()
createSymRef symName ref = do
  repo <- ask
  liftIO $ writeFile (getGitRepository repo </> symName) $ "ref: " ++ ref ++ "\n"

readSymRef :: String -> WithRepository ObjectId
readSymRef name = do
  repo <- ask
  let gitDir = getGitDirectory repo
  ref <- liftIO $ C.readFile (gitDir </> name)
  let unwrappedRef = C.unpack $ strip $ head $ tail $ C.split ':' ref
  obj <- liftIO $ C.readFile (gitDir </> unwrappedRef)
  return $ C.unpack (strip obj)
  where strip = C.takeWhile (not . isSpace) . C.dropWhile isSpace

pathForPack :: GitRepository -> FilePath
pathForPack repo = GitRepository repo </> "objects" </> "pack"

pathForObject :: String -> String -> (FilePath, String)
pathForObject repoName sha | length sha == 40 = (repoName </> ".git" </> "objects" </> pre, rest)
  where pre = take 2 sha
        rest = drop 2 sha
pathForObject _ _ = ("", "")

readTree :: GitRepository -> ObjectId -> IO (Maybe Tree)
readTree repo sha = do
  treeBlob <- readObject repo sha
  return $ parseTree sha (getBlobContent $ fromJust treeBlob)

readObject :: GitRepository -> ObjectId -> IO (Maybe Object)
readObject GitRepository{..} sha = do
  let (path, name) = pathForObject getName sha
      filename = path </> name
  exists <- doesFileExist filename

  if exists then do
    bs <- C.readFile filename
    return $ parseObject sha $ inflate bs
  else
    return Nothing

  where
    inflate blob = B.concat . L.toChunks . Z.decompress $ L.fromChunks [blob]

createEmptyGitRepository :: FilePath -> IO ()
createEmptyGitRepository gitDir =
  mapM_ (\dir -> createDirectoryIfMissing True (gitDir </> dir)) topLevelDirectories
  where
    topLevelDirectories = ["objects", "refs", "hooks", "info"]

toObjectId :: PackObjectType -> Maybe ObjectId
toObjectId (OBJ_REF_DELTA) base = Just $ toHex $ B.pack base
toObjectId _ = Nothing

toHex :: C.ByteString -> String
toHex bytes = C.unpack bytes >>= printf "%02x"

getGitDirectory :: GitRepository -> FilePath
getGitDirectory = (</> ".git") . getName

createRefs :: [Ref] -> WithRepository ()
createRefs refs = do
  let (tags, branches) = partition isTag $ filter (not . isPeeledTag) refs
  writeRefs "refs/remotes/origin" branches
  writeRefs "refs/tags" tags
  where
    simpleRefName = head . reverse . C.split '/'
    isPeeledTag = C.isSuffixOf "^{}" . getRefName
    isTag = (\e -> (not . C.isSuffixOf "^{}" $ getRefName e) && (C.isPrefixOf "refs/tags" $ getRefName e))
    writeRefs refSpace = mapM_ (\Ref{..} -> createRef (refSpace ++ "/" ++ (C.unpack . simpleRefName $ getRefName)) (C.unpack getObjId))

createRef :: String -> String -> WithRepository ()
createRef ref sha = do
  repo <- ask
  let (repo, name) = splitRefName ref
      dir = getGitDirectory repo </> path
  _ <- liftIO $ createDirectoryIfMissing True dir
  liftIO $ writeFile (dir </> name) (sha ++ "\n")