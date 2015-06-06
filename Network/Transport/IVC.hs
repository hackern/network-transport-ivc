module Network.Transport.IVC where
-- export everything for testing

import Network.Transport.Dummy
import Network.Transport.IVC.Internal

import Data.Word (Word32)
import Data.List (intercalate)
import qualified Data.Map.Strict as M(Map, empty, lookup, insert)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Data.List.Split (splitOn)

import Hypervisor.Debug
import Hypervisor.XenStore (XenStore, xsGetDomId,
                            xsRead, xsWrite, xsRemove, xsMakeDirectory,
                            xsSetPermissions, XSPerm(ReadWritePerm))
import Hypervisor.DomainInfo (DomId)
import Communication.IVC -- (InChannel, OutChannel, get, put)
import Communication.Rendezvous --  (peerConnection)

data IVCTransport = IVCTransport {
  transportDomId :: DomId,
  transportState :: MVar TransportState
}

data TransportState = TransportState {
  localEndPoints :: M.Map EndPointAddress LocalEndPoint,
  nextEndPointId :: EndPointId
}

data LocalEndPoint = LocalEndPoint {
  eventChan           :: Chan Event,
  localEndPointState  :: MVar LocalEndPointState
}

data LocalEndPointState = LocalEndPointState {
  nextRemoteConnectionId :: ConnectionId,
  nextLocalConnectionId  :: ConnectionId
}

type EndPointId = Word32


createTransport :: XenStore -> IO Transport
createTransport xs = do
  me <- xsGetDomId xs
  ts <- newMVar (TransportState M.empty 0)
  let transport = IVCTransport me ts
      rootPath = "/transport/" ++ show me
  removePath xs rootPath
  xsMakeDirectory xs rootPath
  xsSetPermissions xs rootPath [ReadWritePerm me]
  forkServer xs me (createHandler xs ts)
  return Transport { newEndPoint = apiNewEndPoint xs transport }

forkServer :: XenStore -> DomId
           -> (EndPointAddress -> EndPointAddress -> String -> IO ())
           -> IO ()
forkServer xs me handler =
  void . forkIO $ do
    let rootPath = "/transport/" ++ show me
    forever $ do
      conns <- listKeys xs rootPath
      forM_ conns $ \connName -> do
        let from = EndPointAddress . BSC.pack $
                     intercalate "-" (take 2 (splitOn "-" connName))
        val <- xsRead xs (rootPath ++ "/" ++ connName)
        let to = EndPointAddress . BSC.pack $ val
        xsRemove xs (rootPath ++ "/" ++ connName)
        handler from to connName
      threadDelay 100000

-- handle imcoming connection to a transport (dommain)
createHandler :: XenStore -> MVar TransportState
              -> EndPointAddress -> EndPointAddress -> String -> IO ()
createHandler xs ts from to connName =
  void . forkIO $ do
    state <- takeMVar ts
    let Just localendpoint    = M.lookup to (localEndPoints state)
        es                    = localEndPointState localendpoint
        chan                  = eventChan localendpoint
        leftSide              :: XenStore -> IO (OutChannel ByteString)
        rightSide             :: XenStore -> IO (InChannel ByteString)
        (leftSide, rightSide) = peerConnection connName 1
    inChan <- rightSide xs
    connectId <- modifyMVar es $ \state -> do
      let connectId = nextRemoteConnectionId state
      return (state { nextRemoteConnectionId = connectId + 1 }, connectId)
    writeChan chan (ConnectionOpened connectId from)
    forever $ do
      bs <- get inChan  -- expected to block while waiting
      writeChan chan (Received connectId [bs])

apiNewEndPoint :: XenStore -> IVCTransport -> IO EndPoint
apiNewEndPoint xs transport = do
  chan <- newChan
  es <- newMVar (LocalEndPointState 0 0)
  let localendpoint = LocalEndPoint chan es
      me            = transportDomId transport
      ts            = transportState transport
  addr <- modifyMVar ts $ \state -> do
    let addr = encodeEndPointAddress me (nextEndPointId state)
    return (state {
              localEndPoints = M.insert addr localendpoint (localEndPoints state),
              nextEndPointId = nextEndPointId state + 1
            }, addr)
  return EndPoint { receive = readChan chan,
                    address = addr,
                    connect = apiConnect xs es addr }

-- pass in client address to build unique connection name in xenstore
apiConnect :: XenStore -> MVar LocalEndPointState
              -> EndPointAddress -> EndPointAddress -> IO Connection
apiConnect xs es from to = do
  connectId <- modifyMVar es $ \state -> do
    let connectId = nextLocalConnectionId state
    return (state { nextLocalConnectionId = connectId + 1 }, connectId)
  let connName = endPointAddressToString from ++ "-" ++ show connectId
      Just (other, _) = decodeEndPointAddress to
  xsWrite xs ("/transport/" ++ show other ++ "/" ++ connName)
             (endPointAddressToString to)
  let leftSide              :: XenStore -> IO (OutChannel ByteString)
      rightSide             :: XenStore -> IO (InChannel ByteString)
      (leftSide, rightSide) = peerConnection connName 1
  outChan <- leftSide xs
  return Connection { send = apiSend outChan }

-- non-blocking semantics
apiSend :: OutChannel ByteString -> [ByteString] -> IO ()
apiSend outChan bss = void . forkIO $ do
  forM_ bss $ \bs -> do
    put outChan bs

endPointAddressToString :: EndPointAddress -> String
endPointAddressToString (EndPointAddress bs) =
  BSC.unpack bs

-- in the format of domXX:XX
encodeEndPointAddress :: DomId -> EndPointId -> EndPointAddress
encodeEndPointAddress domId ix =
  EndPointAddress . BSC.pack $ show domId ++ "-" ++ show ix

decodeEndPointAddress :: EndPointAddress -> Maybe (DomId, EndPointId)
decodeEndPointAddress addr =
  case splitOn "-" (endPointAddressToString addr) of
    h : t : _  -> Just (read h, read t)
    _ -> Nothing
