{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, ImpredicativeTypes #-}
import Hypervisor.DomainInfo
import Hypervisor.XenStore
import Hypervisor.Debug
import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Serializable
import Network.Transport.IVC
import Data.Binary (encode, decode)
import Data.Typeable

data Mapper a b = Mapper { unMapper :: a -> b }
  deriving Typeable

data Reducer a = Reducer { unReducer :: a -> a -> a }
  deriving Typeable

exec :: ([Float],
         [Closure (Mapper Float Float)],
         Closure (Reducer Float),
         ProcessId) -> Process ()
exec (xs, mapperSs, reducerS, master) = do
  mappers <- mapM unClosure mapperSs
  reducer <- unClosure reducerS
  let ys = foldl' (\xs mapper -> mapper <$> xs) xs (unMapper <$> mappers)
      result = foldl1' (unReducer reducer) ys
  send master result

f :: Mapper Float Float
f = Mapper (\x -> x + 1)

g :: Reducer Float
g = Reducer (\x y -> x + y)

$(remotable ['exec, 'f, 'g])

data RDD a = RDD [a] [Closure (Mapper a a)]

parallelize :: [a] -> RDD a
parallelize xs = RDD xs []

($$) :: Closure (Mapper a a) -> RDD a -> RDD a
f $$ (RDD xs fs) = RDD xs (fs ++ [f])
infixr 4 $$

reduce :: [NodeId] -> RDD Float -> Closure (Reducer Float) -> Process Float
reduce workers (RDD xs fs) g = do
  us <- getSelfPid
  g' <- unReducer <$> unClosure g
  cpids <- forM (zip workers [0..numOfWorkers-1]) $ \(worker, i) -> do
    cpid <- spawn worker ($(mkClosure 'exec) (take slice (drop (i * slice) xs), fs, g, us))
    -- monitor cpid
    return cpid
  let go :: (Maybe Float) -> Int -> [(ProcessId, Int)] -> Int -> Process Float
      go partial finished map nworkerIdx | finished == numOfWorkers =
        return $ fromJust partial
                                         | otherwise = do
        (partial', finished', map', nworker') <- receiveWait [
          match (\result -> do
                  case partial of
                    Nothing -> return (Just result, finished + 1, map, nworkerIdx)
                    Just current -> return (Just (g' current result), finished + 1,
                                            map, nworkerIdx)),
          matchIf (\(ProcessMonitorNotification _ _ reason) -> reason /= DiedNormal)
                  (\(ProcessMonitorNotification _ pid reason) -> do
                    liftIO . writeDebugConsole $ show reason ++ "\n"
                    let Just i = lookup pid map
                        nworker = head $ drop nworkerIdx workers
                    cpid <- spawn nworker ($(mkClosure 'exec)
                                  (take slice (drop (i * slice) xs), fs, g, us))
                    monitor cpid
                    return (partial, finished, (cpid, i) : map,
                            (nworkerIdx + 1) `mod` numOfWorkers))]
        go partial' finished' map' nworker'
  go Nothing 0 (zip cpids [0..numOfWorkers-1]) 0
  where
    numOfWorkers = length workers
    slice = (length xs - 1) `div` numOfWorkers + 1

driver :: [NodeId] -> Process ()
driver workers = do
  let rdd = parallelize [1..5]
      rdd' = $(mkStaticClosure 'f) $$ $(mkStaticClosure 'f) $$ rdd
  result <- reduce workers rdd' $(mkStaticClosure 'g)
  liftIO . writeDebugConsole $ show result ++ "\n"

initialProcess :: XenStore -> Int -> Int -> Process ()
initialProcess xs 0 num = do
  workers <- liftIO $ do
    keys <- waitForKeys xs "/process" (num-1) -- should be changed to /workers
    forM keys $ \key -> decode . read <$> xsRead xs ("/process/" ++ key)
  driver workers
initialProcess xs index num = do
  us <- processNodeId <$> getSelfPid
  liftIO $ xsWrite xs ("/process/worker" ++ show index) (show (encode us))
  receiveWait []

main :: IO ()
main = do
  xs <- initXenStore
  Right transport <- createTransport xs
  doms <- sort <$> waitForDoms xs 3
  me <- xsGetDomId xs
  let Just index = elemIndex me doms

  node <- newLocalNode transport (Main.__remoteTable initRemoteTable)
  runProcess node $ initialProcess xs index (length doms)
