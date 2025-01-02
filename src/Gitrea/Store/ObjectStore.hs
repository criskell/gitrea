module Gitrea.Store.ObjectStore (
  encodeObject
) where

import qualified Data.ByteString.Char8 as C

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