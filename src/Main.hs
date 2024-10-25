module Main where

data Remote = {
      getHost        :: String
    , getPort        :: Maybe Int
    , getRepository  :: String 
} deriving (Eq, Show)

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
lsRemote' Remote{..} = withSocketsDo $
    withConnection getHost (show $ fromMaybe 9418 getPort) $ \socket ->
        let payload = gitProtocolRequest getHost getRepository
        
        send socket payload

        response <- receive socket

        send socket flushPktLine

        return $ parsePacket $ L.fromChunks [response]