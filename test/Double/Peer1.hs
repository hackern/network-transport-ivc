import Network.Transport
import Network.Transport.IVC

import Hypervisor.XenStore
import Hypervisor.Debug

import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Data.ByteString.Char8 as BSC

main :: IO ()
main = do
  xs <- initXenStore

  Right transport <- createTransport xs
  Right endpoint <- newEndPoint transport

  lock <- newEmptyMVar
  forkIO . forever $ do
    event <- receive endpoint
    case event of
      ConnectionOpened _ _ addr ->
        writeDebugConsole $ "connection from " ++ show addr ++ "\n"
      Received _ [bs] -> do 
        writeDebugConsole $ (BSC.unpack bs) ++ "\n"
        putMVar lock ()

  takeMVar lock
  closeTransport transport
