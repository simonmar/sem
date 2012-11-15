{-# LANGUAGE CPP #-}
module Main where

import Semaphore as S
import Control.Concurrent.QSem as Q
import qualified Control.Concurrent.MSem as M
import Control.Monad
import System.Environment
import Control.Concurrent (forkIO)

#ifdef QSEM
new = Q.newQSem
wait = Q.waitQSem
signal = Q.signalQSem
#elif defined (MSEM)
new = M.new
wait = M.wait
signal = M.signal
#else
new = S.newQSem
wait = S.waitQSem
signal = S.signalQSem
#endif

main = do
  [m, n] <- fmap (fmap read) getArgs
  case m of
    0 -> do
      q <- new n
      replicateM_ n $ wait q
      replicateM_ n $ signal q
    1 -> do
      q <- new 0
      replicateM_ n $ forkIO $ signal q
      replicateM_ n $ wait q
    2 -> do
      q <- new 0
      r <- new 0
      replicateM_ n $ forkIO $ do wait q; signal r
      replicateM_ n $ signal q
      replicateM_ n $ wait r