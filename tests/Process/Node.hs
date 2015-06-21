import Hypervisor.XenStore
import Hypervisor.Debug

import Network.Transport.IVC (createTransport)
import Control.Distributed.Process
import Control.Distributed.Process.Node

main :: IO ()
main = do
  xs <- initXenStore
  Right t <- createTransport xs

  node <- newLocalNode t initRemoteTable
  runProcess node $ do
    self <- getSelfPid
    send self "hello, world"
    hello <- expect :: Process String
    liftIO $ writeDebugConsole (hello ++ "\n")
