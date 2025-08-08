module Gitrea.Remote.TcpClient
  ( withConnection,
    send,
    receive,
    receiveWithSideband,
    receiveFully,
    receive,
  )
where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.List.NonEmpty as NE
import Data.Monoid (mappend, mempty)
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString hiding (recv, sendAll)
import Numeric (readHex)

withConnection :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
withConnection host port consumer = do
  socket <- openConnection host port
  response <- consumer socket
  sClose socket
  return response

send :: Socket -> String -> IO ()
send socket message = sendAll socket $ C.pack message

receive :: Socket -> IO C.ByteString
receive socket = receive' socket mempty
  where
    receive' s acc = do
      maybeLine <- readPacketLine s
      maybe (return acc) (receive' s . mappend acc) maybeLine

openConnection :: HostName -> ServiceName -> IO Socket
openConnection host port = do
  serverAddress <- NE.head <$> getAddrInfo Nothing (Just host) (Just port)
  sock <- socket (addrFamily serverAddress) Stream defaultProtocol
  connect sock (addrAddress serverAddress)
  return sock

readPacketLine :: Socket -> IO (Maybe C.ByteString)
readPacketLine socket = do
  length <- readFully mempty 4

  if C.null length
    then return Nothing
    
    else case readHex $ C.unpack length of
      ((length, _) : _) | length > 4 -> do
        line <- readFully mempty (length - 4)
        return $ Just line
      _ -> return Nothing

  where
    -- Completely read the specified amount of bytes from the socket.
    -- The `read` function does not guarantee that the quantity will be returned at once.
    readFully acc expected = do
      line <- recv socket expected

      let length = C.length line
          acc' = acc `mappend` line
          haveMoreReading = len /= expected && not (C.null line)

      if haveMoreReading then readFully acc' (expected - len) else return acc'