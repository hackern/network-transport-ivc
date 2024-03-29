import Network.Transport
import Network.Transport.IVC

import Hypervisor.XenStore
import Hypervisor.Debug

import Control.Monad
import Control.Concurrent
import Control.Concurrent.QSemN
import Data.ByteString.Char8 as BSC

main :: IO ()
main = do
  xs <- initXenStore

  Right transport <- createTransport xs
  Right endpoint <- newEndPoint transport

  sem <- newQSemN 0
  forkIO . forever $ do
    event <- receive endpoint
    case event of
      ConnectionOpened _ _ addr ->
        writeDebugConsole $ "connection from " ++ show addr ++ "\n"
      Received _ bss -> do 
        forM bss $ \bs ->
          writeDebugConsole $ (BSC.unpack bs) ++ "\n"
        signalQSemN sem 1

  Right conn <- connect endpoint (address endpoint)
                        ReliableOrdered defaultConnectHints
  writeDebugConsole "first connection setup\n"
  send conn [BSC.pack "hello"]
  Right conn' <- connect endpoint (address endpoint)
                         ReliableOrdered defaultConnectHints
  writeDebugConsole "second connection setup\n"
  send conn' [BSC.pack "world"]

  waitQSemN sem 2
  closeTransport transport
