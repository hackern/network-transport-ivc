module Network.Transport.IVC.Util (
  waitForDoms,
  waitForKey,
  listKeys,
  removePath
)
where

import Control.Concurrent
import Control.Exception
import Control.Applicative

import Hypervisor.XenStore
import Hypervisor.DomainInfo
import Hypervisor.ErrorCodes

waitForKey :: XenStore -> String -> IO String
waitForKey xs key = do
  eres <- catch (Right <$> xsRead xs key) leftError
  case eres of
    Left _    -> threadDelay 100000 >> waitForKey xs key
    Right res -> return res
 where
  leftError :: ErrorCode -> IO (Either ErrorCode String)
  leftError = return . Left

listKeys :: XenStore -> FilePath -> IO [FilePath]
listKeys xs here = filter (/= "") `fmap` xsDirectory xs here

removePath :: XenStore -> String -> IO ()
removePath xs str = do catch remSubItems onECContinue
                       catch remItem     onECContinue
  where
    remSubItems = mapM_ (removePath xs) =<< xsDirectory xs str
    remItem     = xsRemove xs str
    onECContinue :: ErrorCode -> IO ()
    onECContinue _ = return ()

waitForDoms :: XenStore -> Int -> IO [DomId]
waitForDoms xs num = do
  doms <- listKeys xs "/transport"
  if length doms < num
  then do
    waitForDoms xs num
  else
    return $ read <$> doms