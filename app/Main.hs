{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Gitrea.Remote.TcpClient
import Network.Socket

data Remote = Remote
  { getHost :: String,
    getPort :: Maybe Int,
    getRepository :: String
  }
  deriving (Eq, Show)

main :: IO ()
main = return ()

-- `pkt-line` format.

-- | Represents a string in pkt-line format.
-- First four bytes is the string length and remaining is the overall string.
pktLine :: String -> String
pktLine msg = printf "%04s%s" (toHex . (4 +) $ length msg) msg

-- We use it when we want to notify that an agreed point in communication has been reached.
flushPktLine = "0000"

-- Git protocol.

-- ABNF: `git-proto-request = request-command SP pathname NUL [ host-parameter NUL ]`
gitProtocolRequest :: String -> String -> String
gitProtocolRequest host repo = pktLine $ "git-upload-pack /" ++ repo ++ "\0host=" ++ host ++ "\0"

lsRemote' :: Remote -> IO [PacketLine]
lsRemote' Remote {..} = withSocketsDo $
  withConnection getHost (show $ fromMaybe 9418 getPort) $ \socket -> do
    let payload = gitProtocolRequest getHost getRepository

    send socket payload

    response <- receive socket

    send socket flushPktLine

    return $ parsePacket $ L.fromChunks [response]

receivePack :: Remote -> IO ([Ref], B.ByteString)
receivePack Remote {..} = withSocketsDo $
  withConnection getHost (show $ fromMaybe 9418 getPort) $ \sock -> do
    let initialRequest = gitProtocolRequest getHost getRepository
    send sock initialRequest
    response <- receive sock

    let pack = parsePacket $ L.fromChunks [response]
        request = createNegotiationRequest ["multi_ack_detailed", "side-band-64k", "agent=git/1.8.1"] pack ++ flushPktLine ++ pktLine "done\n"

    send sock request
    !rawPack <- receiveWithSideband sock (printSideband . C.unpack)
    return (mapMaybe toRef pack, rawPack)
  where
    printSideband str = do
      hPutStr stderr str
      hFlush stderr

toObjectId :: PacketLine -> Maybe String
toObjectId (FirstLine object _ _) = Just $ C.unpack object
toObjectId (RefLine object _) = Just $ C.unpack object
toObjectId _ = Nothing

createNegotiationRequest :: [String] -> [PacketLine] -> String
createNegotiationRequest capabilities =
    concat
    . nub -- Remove duplicates.
    . map (pktLine . (++ "\n")) -- Convert to pkt-line format.
    . foldl'
        (\acc e ->
            if null acc -- If the first want.
            then first acc e -- Add capabilities to the first want.
            else additional acc e -- Add capabilities to the additional wants.
        ) []
    . wants
    . filter filterPeeledTags
    . filter filterRefs
  where
    wants :: [PacketLine] -> [String]
    wants = mapMaybe toObjId

    first :: [String] -> String -> [String]
    first acc obj = acc ++ ["want " ++ obj ++ " " ++ unwords capabilities]

    additional :: [String] -> String -> [String]
    additional acc obj = acc ++ ["want " ++ obj]

    filterPeeledTags :: PacketLine -> Bool
    filterPeeledTags = not . isSuffixOf "^{}" . C.unpack . ref

    filterRefs :: PacketLine -> Bool
    filterRefs line =
        let r = C.unpack $ ref line
            predicates = map ($ r) [isPrefixOf "refs/tags/", isPrefixOf "refs/heads/"]
        in or predicates

receiveWithSideband :: Socket -> (B.ByteString IO a) -> IO B.ByteString
receiveWithSideband sock f = recrec mempty
  where
    recrec acc = do
      !maybeLIne <- readPacketLine sock
      let skip = recrec acc
      case maybeLine of
        Just "NAK\n" -> skip
        Just line -> case B.uncons line of
                      Just (1, rest) -> recrec (acc `mappend` rest)
                      Just (2, rest) -> f ("remote: " `C.append` rest) >> skip
                      Just (_, rest) -> fail $ C.unpack rest
                      Nothing -> skip
        Nothing -> return acc

clone' :: GitRepository -> Remote -> IO ()
clone' repo remote@Remote{..} = do
  (refs, packFile) <- receivePack remote

  let dir = pathForPack repo
      tmpPack = dir </> "tmp_pack_incoming"

  B.writeFile tmpPack packFile
  _ <- runReaderT (createGitRepositoryFromPackfile tmpPack refs) repo

  removeFile tmpPack
  runReaderT checkoutHead repo