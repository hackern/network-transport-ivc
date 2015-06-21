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

  Right conn <- connect endpoint (address endpoint)
                        ReliableOrdered defaultConnectHints

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

  let s = 10
  forM_ [0..(s-1)] $ \i -> do
    send conn [BSC.pack (show i)]

  waitQSemN sem s 
  closeTransport transport
