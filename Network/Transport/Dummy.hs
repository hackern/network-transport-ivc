module Network.Transport.Dummy where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import Data.Word (Word64)

data Transport = Transport {
    newEndPoint :: IO EndPoint,
    closeTransport :: IO ()
}

data EndPoint = EndPoint {
    receive :: IO Event,
    address :: EndPointAddress,
    connect :: EndPointAddress -> IO Connection
}

data Event = Received         {-# UNPACK #-} !ConnectionId [ByteString]
           | ConnectionOpened {-# UNPACK #-} !ConnectionId EndPointAddress
           deriving (Show, Eq)

newtype EndPointAddress = EndPointAddress { 
  endPointAddressToByteString :: ByteString
} deriving (Eq, Ord)

instance Show EndPointAddress where
  show = BSC.unpack . endPointAddressToByteString

type ConnectionId = Word64

data Connection = Connection {
  send :: [ByteString] -> IO ()
}
