import Network.Transport
import Network.Transport.IVC
import Network.Transport.IVC.Internal

import Hypervisor.DomainInfo
import Hypervisor.XenStore
import Hypervisor.Debug

import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Data.ByteString.Char8 as BSC

waitForDoms :: XenStore -> IO [DomId]
waitForDoms xs = do
  doms <- listKeys xs "/transport"
  case doms of
    []    -> waitForDoms xs
    _     -> return $ read `fmap` doms

main :: IO ()
main = do
  xs <- initXenStore

  serverDom : _ <- waitForDoms xs -- peers discovering
  Right transport <- createTransport xs

  Right endpoint <- newEndPoint transport
  let serverAddr = encodeEndPointAddress serverDom 0
  Right conn <- connect endpoint serverAddr ReliableOrdered defaultConnectHints
  forM_ [0..9] $ \i ->
    send conn [BSC.pack (show (2*i)), BSC.pack (show (2*i + 1))]

  -- threadDelay 10000000
  closeTransport transport
