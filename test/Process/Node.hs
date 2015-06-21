import Hypervisor.XenStore
import Hypervisor.Debug

import Network.Transport.IVC (createTransport)
import Control.Distributed.Process
import Control.Distributed.Process.Node

import Control.Concurrent (threadDelay)

main :: IO ()
main = do
  xs <- initXenStore
  Right t <- createTransport xs

  node <- newLocalNode t initRemoteTable
  forkProcess node $ do
    self <- getSelfPid
    send self "hello, world"
    hello <- expect :: Process String
    liftIO $ writeDebugConsole (hello ++ "\n")
    return ()

  threadDelay 1000000