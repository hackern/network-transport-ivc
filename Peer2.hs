import Network.Transport.Dummy
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
  transport <- createTransport xs

  endpoint <- newEndPoint transport
  let serverAddr = encodeEndPointAddress serverDom 0
  connection <- connect endpoint serverAddr
  send connection $ [BSC.pack "hello, world"]

  -- threadDelay 10000000
  closeTransport transport
