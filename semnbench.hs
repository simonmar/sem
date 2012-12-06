{-# LANGUAGE CPP #-}
module Main where

import Control.Concurrent.QSemN as Q
import QSemN as Q2
import qualified Control.Concurrent.MSemN as M
import Control.Monad
import System.Environment
import Control.Concurrent

#ifdef QSEMN
new = Q.newQSemN
wait = Q.waitQSemN
signal = Q.signalQSemN
#elif defined(NEWQSEMN)
new = Q2.newQSemN
wait = Q2.waitQSemN
signal = Q2.signalQSemN
#elif defined (MSEMN)
new = M.new
wait = M.wait
signal = M.signal
#endif

main = do
  [m, n] <- fmap (fmap read) getArgs
  case m of
    0 -> do
      q <- new n
      replicateM_ n $ wait q 1
      replicateM_ n $ signal q 1
    1 -> do
      q <- new 0
      replicateM_ n $ forkIO $ signal q 1
      replicateM_ n $ wait q 1
    2 -> do
      q <- new 0
      r <- new 0
      replicateM_ n $ forkIO $ do wait q 1; signal r 1
      replicateM_ n $ signal q 1
      replicateM_ n $ wait r 1
