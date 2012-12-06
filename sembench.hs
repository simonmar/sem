{-# LANGUAGE CPP #-}
module Main where

import QSem as Q
import QSemSTM as S
import STM.Semaphore as SS
import Control.Concurrent.QSem as OldQ
import qualified Control.Concurrent.MSem as M

import Control.Monad
import System.Environment
import Control.Concurrent (forkIO)


#ifdef OLDQ
new = OldQ.newQSem
wait = OldQ.waitQSem
signal = OldQ.signalQSem
#elif defined (NEWQ)
new = Q.newQSem
wait = Q.waitQSem
signal = Q.signalQSem
#elif defined (NEWQS)
new = S.newQSem
wait = S.waitQSem
signal = S.signalQSem
#elif defined(SSEM)
new = SS.newQSem
wait = SS.waitQSem
signal = SS.signalQSem
#elif defined (MSEM)
new = M.new
wait = M.wait
signal = M.signal
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
