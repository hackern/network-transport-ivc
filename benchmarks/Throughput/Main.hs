{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
import Hypervisor.DomainInfo
import Hypervisor.XenStore
import Hypervisor.Debug
import Data.List
import Control.Monad
import Control.Applicative
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport.IVC (createTransport, waitForDoms, waitForKey)
import Data.Binary
import Data.Typeable

data SizedList a = SizedList { size :: Int , elems :: [a] }
  deriving (Typeable)

instance Binary a => Binary (SizedList a) where
  put (SizedList sz xs) = put sz >> mapM_ put xs
  get = do
    sz <- get
    xs <- getMany sz
    return (SizedList sz xs)

-- Copied from Data.Binary
getMany :: Binary a => Int -> Get [a]
getMany = go []
 where
    go xs 0 = return $! reverse xs
    go xs i = do x <- get
                 x `seq` go (x:xs) (i-1)
{-# INLINE getMany #-}

nats :: Int -> SizedList Int
nats = \n -> SizedList n (aux n)
  where
    aux 0 = []
    aux n = n : aux (n - 1)

counter :: Process ()
counter = go 0
  where
    go :: Int -> Process ()
    go !n =
      receiveWait
        [ match $ \xs   -> go (n + size (xs :: SizedList Int))
        , match $ \them -> send them n >> go 0
        ]

count :: (Int, Int) -> ProcessId -> Process ()
count (packets, sz) them = do
  us <- getSelfPid
  replicateM_ packets $ send them (nats sz)
  send them us
  n' <- expect
  liftIO $ writeDebugConsole $ (show (packets * sz, n' == packets * sz)) ++ "\n"

initialProcess :: XenStore -> String -> Process ()
initialProcess xs "SERVER" = do
  us <- getSelfPid
  liftIO $ xsWrite xs "/process/counter-pid" (show (encode us))
  counter
initialProcess xs "CLIENT" = do
  us <- getSelfPid
  them <- liftIO $ decode . read <$> waitForKey xs "/process/counter-pid"
  count (10, 10) them

main :: IO ()
main = do
  xs <- initXenStore
  Right transport <- createTransport xs
  doms <- sort <$> waitForDoms xs 2
  me <- xsGetDomId xs
  let role = if me == head doms then "SERVER" else "CLIENT"

  node <- newLocalNode transport initRemoteTable
  runProcess node $ initialProcess xs role
