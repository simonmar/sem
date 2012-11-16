{-# LANGUAGE DeriveDataTypeable #-}
module STM.Semaphore (
      QSem, newQSem, waitQSem, signalQSem
  ) where

import Control.Concurrent.STM
import Control.Monad
import Data.Typeable
import Control.Exception
-- import Debug.Trace

newtype QSem = QSem (TVar Int)
  deriving (Eq, Typeable)

newQSem :: Int -> IO QSem
newQSem = atomically . newQSemSTM

waitQSem :: QSem -> IO ()
waitQSem = atomically . waitQSemSTM

signalQSem :: QSem -> IO ()
signalQSem = atomically . signalQSemSTM


newQSemSTM :: Int -> STM QSem
newQSemSTM i = fmap QSem (newTVar i)

waitQSemSTM :: QSem -> STM ()
waitQSemSTM (QSem t) = do
  i <- readTVar t
  when (i <= 0) retry
  writeTVar t $! (i-1)

signalQSemSTM :: QSem -> STM ()
signalQSemSTM (QSem t) = do
  i <- readTVar t
  writeTVar t $! i+1

