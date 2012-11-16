{-# LANGUAGE CPP #-}
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import Test.HUnit

import Semaphore as S
import STM.Semaphore as SS
import Control.Concurrent.QSem as Q
import qualified Control.Concurrent.MSem as M
import Control.Concurrent.Chan
import Control.Concurrent (forkIO, threadDelay, killThread, yield)
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad

#ifdef QSEM
new = Q.newQSem
wait = Q.waitQSem
signal = Q.signalQSem
#elif defined(SSEM)
new = SS.newQSem
wait = SS.waitQSem
signal = SS.signalQSem
#elif defined (MSEM)
new = M.new
wait = M.wait
signal = M.signal
#else
new = S.newQSem
wait = S.waitQSem
signal = S.signalQSem
#endif

main = defaultMain tests

tests = [
    testCase "sem_kill" sem_kill,
    testCase "sem_fifo" sem_fifo,
    testCase "sem_bracket" sem_bracket
 ]

sem_fifo :: Assertion
sem_fifo = do
  c <- newChan
  q <- new 0
  t1 <- forkIO $ do wait q; writeChan c 'a'
  threadDelay 10000
  t2 <- forkIO $ do wait q; writeChan c 'b'
  threadDelay 10000
  t3 <- forkIO $ do wait q; writeChan c 'c'
  threadDelay 10000
  signal q
  a <- readChan c
  signal q
  b <- readChan c
  signal q
  c <- readChan c
  [a,b,c] @?= "abc"

sem_kill :: Assertion
sem_kill  = do
  q <- new 0
  t <- forkIO $ do wait q
  threadDelay 100000
  killThread t
  m <- newEmptyMVar
  t <- forkIO $ do wait q; putMVar m ()
  signal q
  takeMVar m


sem_bracket :: Assertion
sem_bracket = do
  q <- new 1
  replicateM_ 100000 $ do
     t <- forkIO $ bracket_ (wait q) (signal q) (return ())
     yield
     killThread t
  wait q
