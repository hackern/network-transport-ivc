-- | Like Latency, but creating lots of channels
import Hypervisor.XenStore
import Hypervisor.DomainInfo
import Hypervisor.Debug
import Data.List
import Control.Monad
import Control.Applicative
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport.IVC (createTransport, waitForDoms, waitForKey)
import Data.Binary (encode, decode)

pingServer :: Process ()
pingServer = forever $ do
  them <- expect
  sendChan them ()
  -- TODO: should this be automatic?
  reconnectPort them

pingClient :: Int -> ProcessId -> Process ()
pingClient n them = do
  replicateM_ n $ do
    (sc, rc) <- newChan :: Process (SendPort (), ReceivePort ())
    send them sc
    receiveChan rc
  liftIO . writeDebugConsole $ "Did " ++ show n ++ " pings\n"

initialProcess :: XenStore -> String -> Process ()
initialProcess xs "SERVER" = do
  us <- getSelfPid
  liftIO $ xsWrite xs "/process/server-pid" (show (encode us))
  pingServer
initialProcess xs "CLIENT" = do
  them <- liftIO $ decode . read <$> waitForKey xs "/process/server-pid"
  pingClient 10 them

main :: IO ()
main = do
  xs <- initXenStore
  Right transport <- createTransport xs
  doms <- sort <$> waitForDoms xs 2
  me <- xsGetDomId xs
  let role = if me == head doms then "SERVER" else "CLIENT"

  node <- newLocalNode transport initRemoteTable
  runProcess node $ initialProcess xs role
