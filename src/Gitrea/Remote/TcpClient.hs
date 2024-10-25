module Gitrea.Remote.TcpClient (withConnection) where

import Network.Socket

withConnection :: HostName -> ServiceName -> (Socket -> IO b) -> IO b
withConnection host port consumer = do
    socket <- openConnection host port
    r <- consumer socket
    sClose socket
    return r

send :: Socket -> String -> IO ()
send socket message = sendAll socket $ C.pack message

receive :: Socket -> IO C.ByteString
receive socket = receive' socket mempty
    where
        receive' s acc = do
            maybeLine <- readPacketLine s
            maybe (return acc) (receive' s . mappend acc) maybeLine

readPacketLine :: Socket -> IO (Maybe C.ByteString)
readPacketLine socket = do
    length <- readFully mempty 4

    if C.null length then return Nothing else
        case readHex $ C.unpack length of
            ((length, _):_) | length > 4 -> do
                line <- readFully mempty (length - 4)
                return $ Just line
            _ -> return Nothing
    
    where
        -- | Completely read the specified amount of bytes from the socket.
        -- The `read` function does not guarantee that the quantity will be returned at once.
        readFully acc expected = do
            line <- recv socket expected

            let length = C.length line
                acc' = acc `mappend` line
                cont = len /= expected && not (C.null line)

            if cont then readFully acc' (expected - len) else return acc'