-- | Like Throughput, but send every ping from a different process
-- (i.e., require a lightweight connection per ping)
{-# LANGUAGE BangPatterns #-}
import Hypervisor.XenStore
import Hypervisor.DomainInfo
import Hypervisor.Debug
import Data.List
import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport.IVC (createTransport, waitForDoms, waitForKey)
import Data.Binary (encode, decode)

counter :: Process ()
counter = go 0
  where
    go :: Int -> Process ()
    go !n = do
      b <- expect
      case b of
        Nothing   -> go (n + 1)
        Just them -> send them n >> go 0

count :: Int -> ProcessId -> Process ()
count n them = do
  replicateM_ n . spawnLocal $ send them (Nothing :: Maybe ProcessId)
  go 0
  where
    go :: Int -> Process ()
    go n' | n' == n = liftIO $ writeDebugConsole "done\n"
          | otherwise = do
      us <- getSelfPid
      send them (Just us)
      m <- expect :: Process Int
      go (n' + m)

initialProcess :: XenStore -> String -> Process ()
initialProcess xs "SERVER" = do
  us <- getSelfPid
  liftIO $ xsWrite xs "/process/counter-pid" (show (encode us))
  counter
initialProcess xs "CLIENT" = do
  them <- liftIO $ decode . read <$> waitForKey xs "/process/counter-pid"
  count 10 them -- perfom badly for large n

main :: IO ()
main = do
  xs <- initXenStore
  Right transport <- createTransport xs
  doms <- sort <$> waitForDoms xs 2
  me <- xsGetDomId xs
  let role = if me == head doms then "SERVER" else "CLIENT"

  node <- newLocalNode transport initRemoteTable
  runProcess node $ initialProcess xs role
