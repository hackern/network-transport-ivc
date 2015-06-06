import Network.Transport.Dummy
import Network.Transport.IVC

import Hypervisor.XenStore
import Hypervisor.Debug

import Control.Monad
import Control.Concurrent
import Data.ByteString.Char8 as BSC

main :: IO ()
main = do
  xs <- initXenStore

  transport <- createTransport xs
  writeDebugConsole "transport created\n"

  endpoint1 <- newEndPoint transport
  endpoint2 <- newEndPoint transport
  forkIO . forever $ do
    writeDebugConsole "looping\n"
    event <- receive endpoint2
    case event of
      ConnectionOpened _ addr -> writeDebugConsole $ "connection from" ++ show addr
      Received _ [bs] -> writeDebugConsole $ "mesg: " ++ BSC.unpack bs
  writeDebugConsole "endpoints created\n"

  -- connection <- connect endpoint1 (address endpoint2) 
  writeDebugConsole "connection created\n" 

