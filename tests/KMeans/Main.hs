{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
import Hypervisor.DomainInfo
import Hypervisor.XenStore
import Hypervisor.Debug
import Data.List
import Data.Maybe
import qualified Data.IntMap.Strict as M
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

data Folder b a = Folder { unFolder :: b -> a -> b }
  deriving Typeable

exec :: ([Float],
         Closure (Mapper Float (Float, Int)),
         Closure (Folder (M.IntMap (Float, Int)) (Float, Int)),
         M.IntMap (Float, Int),
         ProcessId) -> Process ()
exec (xs, f, g, init, master) = do
  f' <- unMapper <$> unClosure f
  g' <- unFolder <$> unClosure g
  send master (foldl' g' init (f' <$> xs))

f :: [Float] -> Mapper Float (Float, Int)
f means = Mapper (\x ->
  minimumBy (\(x1, idx1) (x2, idx2) -> compare (abs (x1 - means !! idx1))
                                               (abs (x2 - means !! idx2)))
            (zip (cycle [x]) [0..length means - 1]))

g :: Folder (M.IntMap (Float, Int)) (Float, Int)
g = Folder (\m (x, idx) -> M.adjust (\(sum, cnt) -> (sum + x, cnt + 1)) idx m)

-- Reducer
h :: M.IntMap (Float, Int) -> M.IntMap (Float, Int) -> M.IntMap (Float, Int)
h = M.unionWith (\(sum1, cnt1) (sum2, cnt2) -> (sum1 + sum2, cnt1 + cnt2))

$(remotable ['exec, 'f, 'g])

data RDD = Raw [Float]
         | Composed (Closure (Mapper Float (Float, Int))) RDD
  deriving (Show)

parallelize :: [Float] -> RDD
parallelize xs = Raw xs

($$) :: Closure (Mapper Float (Float, Int)) -> RDD -> RDD
f $$ rdd = Composed f rdd
infixr 4 $$

fold :: [NodeId] -> RDD
     -> Closure (Folder (M.IntMap (Float, Int)) (Float, Int))
     -> (M.IntMap (Float, Int) -> M.IntMap (Float, Int) -> M.IntMap (Float, Int))
     -> M.IntMap (Float, Int) 
     -> Process (M.IntMap (Float, Int))
fold workers (Composed f (Raw xs)) g h init = do
  us <- getSelfPid
  g' <- unFolder <$> unClosure g
  cpids <- forM (zip workers [0..numOfWorkers-1]) $ \(worker, i) -> do
    cpid <- spawn worker ($(mkClosure 'exec) (take slice (drop (i * slice) xs),
                                              f, g, init, us))
    -- monitor cpid
    return cpid
  let go :: (M.IntMap (Float, Int)) -> Int
         -> [(ProcessId, Int)] -> Int -> Process (M.IntMap (Float, Int))
      go partial finished map nworkerIdx | finished == numOfWorkers = return $ partial
                                         | otherwise = do
        (partial', finished', map', nworker') <- receiveWait [
          match (\result ->
                  return (h partial result, finished + 1, map, nworkerIdx)),
          matchIf (\(ProcessMonitorNotification _ _ reason) -> reason /= DiedNormal)
                  (\(ProcessMonitorNotification _ pid reason) -> do
                    liftIO . writeDebugConsole $ show reason ++ "\n"
                    let Just i = lookup pid map
                        nworker = workers !! nworkerIdx 
                    cpid <- spawn nworker ($(mkClosure 'exec)
                                  (take slice (drop (i * slice) xs), f, g, us))
                    monitor cpid
                    return (partial, finished, (cpid, i) : map,
                            (nworkerIdx + 1) `mod` numOfWorkers))]
        go partial' finished' map' nworker'
  go init 0 (zip cpids [0..numOfWorkers-1]) 0
  where
    numOfWorkers = length workers
    slice = (length xs - 1) `div` numOfWorkers + 1

driver :: [NodeId] -> Process ()
driver workers = do
  void $ go 0 ([0, 1] :: [Float])
  where
    go :: Int -> [Float] -> Process [Float]
    go n means | n == 5 = return means
               | otherwise = do
      let rdd' = ($(mkClosure 'f) means) $$ rdd
      result <- fold workers rdd' $(mkStaticClosure 'g) h
                     (M.fromList [(0, (0, 0)), (1, (0, 0))])
      let means' = (\(_, (sum, cnt)) -> sum / fromIntegral cnt) <$> M.toList result
      liftIO . writeDebugConsole $
        "means at iteration " ++ show n ++ " : " ++ show means' ++ "\n"
      go (n + 1) means'

    rdd = parallelize [0..9]

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
