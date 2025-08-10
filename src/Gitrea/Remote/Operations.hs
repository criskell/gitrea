{-# LANGUAGE OverloadedStrings, BangPatterns, RecordWildCards #-}

module Gitrea.Remote.Operations (
  Remote(..)
  , clone
  , lsRemote
  , parseRemote
) where

import qualified Data.Attoparsec.Char8 as AC
import Data.Attoparsec.Combinator
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Control.Applicative ((<$>))
import Control.Monad.Reader (runReaderT)
import System.Directory (removeFile, createDirectoryIfMissing)
import System.FilePath ((</>), takeFileName, dropExtension)
import Network.Socket (withSocketsDo)
import System.IO (hPutStr, stderr, hFlush)
import Text.Printf
import Data.Maybe
import Data.List
import Gitrea.Common
import Gitrea.Remote.TcpClient
import Gitrea.Remote.PackProtocol
import Gitrea.Store.ObjectStore
import Gitrea.Repository

data Remote = Remote {
  getHost :: String
  , getPort :: Maybe Int
  , getRepository :: String
} deriving (Show, Eq)

parseRemote :: B.ByteString -> Maybe Remote
parseRemote = eitherToMaybe . AC.parseOnly parser
  where parser = do
          host <- "git://" AC..*> domain
          port <- option Nothing (Just <$> (":" AC..*> AC.decimal))
          slash
          repo <- AC.takeByteString
          return $ Remote (C.unpack host) port (C.unpack repo)
        domain = AC.takeTill (\x -> x == '/' || x == ':')
        slash = AC.satisfy (== '/')

clone :: String
      -> Maybe String
      -> IO ()
clone url maybeDirectory =
  case parseRemote $ C.pack url of
    Just remote -> let gitRepoName = fromMaybe (repositoryName remote) maybeDirectory
                   in clone' (GitRepository gitRepoName) remote
    _           -> putStrLn $ "Invalid URL: " ++ url

clone' :: GitRepository -> Remote -> IO ()
clone' repo remote@Remote{..} = do
  (refs,packFile) <- receivePack remote
  let dir = pathForPack repo
      tmpPack = dir </> "tmp_pack_incoming"
  _ <- createDirectoryIfMissing True dir
  B.writeFile tmpPack packFile
  _ <- runReaderT (createGitRepositoryFromPackfile tmpPack refs) repo
  removeFile tmpPack
  runReaderT checkoutHead repo

lsRemote :: String -> IO ()
lsRemote url =
  case parseRemote $ C.pack url of
    Just remote -> do
      packetLines <- lsRemote' remote
      mapM_ (\line -> printf "%s\t%s\n" (C.unpack $ objId line) (C.unpack $ ref line)) packetLines
    _ -> putStrLn $ "Invalid URL: " ++ url

lsRemote' :: Remote -> IO [PacketLine]
lsRemote' Remote{..} = withSocketsDo $
  withConnection getHost (show $ fromMaybe 9418 getPort) $ \sock -> do
    let payload = gitProtoRequest getHost getRepository
    send sock payload
    response <- receive sock
    send sock flushPkt
    return $ parsePacket $ L.fromChunks [response]

gitProtoRequest :: String -> String -> String
gitProtoRequest host repo = pktLine $ "git-upload-pack /" ++ repo ++ "\0host=" ++ host ++ "\0"

repositoryName :: Remote -> String
repositoryName = takeFileName . dropExtension . getRepository

toObjId :: PacketLine -> Maybe String
toObjId (FirstLine obj _ _) = Just $ C.unpack obj
toObjId (RefLine obj _) = Just $ C.unpack obj
toObjId _ = Nothing

createNegotiationRequest :: [String] -> [PacketLine] -> String
createNegotiationRequest capabilities = concatMap (++ "")
  . nub
  . map (pktLine . (++ "\n"))
  . foldl' (\acc e -> if null acc then first acc e else additional acc e) []
  . wants
  . filter filterPeeledTags
  . filter filterRefs
  where wants = mapMaybe toObjId
        first acc obj = acc ++ ["want " ++ obj ++ " " ++ unwords capabilities]
        additional acc obj = acc ++ ["want " ++ obj]
        filterPeeledTags = not . isSuffixOf "^{}" . C.unpack . ref
        filterRefs line = let r = C.unpack $ ref line
                              predicates = map ($ r) [isPrefixOf "refs/tags/", isPrefixOf "refs/heads/"]
                          in or predicates

receivePack :: Remote -> IO ([Ref], B.ByteString)
receivePack Remote{..} = withSocketsDo $
  withConnection getHost (show $ fromMaybe 9418 getPort) $ \sock -> do
    let payload = gitProtoRequest getHost getRepository
    send sock payload
    response <- receive sock
    let pack = parsePacket $ L.fromChunks [response]
        request = createNegotiationRequest ["multi_ack_detailed",
                    "side-band-64k",
                    "agent=git/1.8.1"] pack ++ flushPkt ++ pktLine "done\n"
    send sock request
    !rawPack <- receiveWithSideband sock (printSideband . C.unpack)
    return (mapMaybe toRef pack, rawPack)
  where printSideband str = do
                      hPutStr stderr str
                      hFlush stderr