{-# LANGUAGE DeriveDataTypeable, BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Sem2 (
    QSem, newQSem, waitQSem, signalQSem
  ) where

import Control.Concurrent (MVar, newEmptyMVar, takeMVar, tryTakeMVar, putMVar, modifyMVar, modifyMVar_, newMVar, tryPutMVar)
import Control.Monad
import Data.Typeable
import Control.Exception
import Data.Maybe
-- import Debug.Trace

-- | 'QSem' is a quantity semaphore in which the resource is aqcuired
-- and released in units of one. It provides guaranteed FIFO ordering
-- for satisfying blocked `waitQSem` calls.
--
-- The pattern
--
-- >   bracket_ waitQSem signalQSem (...)
--
-- is safe; it never loses a unit of the resource.
--
data QSem = QSem !(MVar (Int, [MVar ()], [MVar ()]))

-- The semaphore state (i, xs, ys):
--
--   i is the current resource value
--
--   (xs,ys) is the queue of blocked threads, where the queue is
--           given by xs ++ reverse ys.  We can enqueue new blocked threads
--           by consing onto ys, and dequeue by removing from the head of xs.
--
-- A blocked thread is represented by an empty (MVar ()).  To unblock
-- the thread, we put () into the MVar.
--
-- A thread can dequeue itself by also putting () into the MVar, which
-- it must do if it receives an exception while blocked in waitQSem.
-- This means that when unblocking a thread in signalQSem we must
-- first check whether the MVar is already full; the MVar lock on the
-- semaphore itself resolves race conditions between signalQSem and a
-- thread attempting to dequeue itself.

newQSem :: Int -> IO QSem
newQSem i = do
  m <- newMVar (i,[],[])
  return (QSem m)

waitQSem :: QSem -> IO ()
waitQSem (QSem m) = do
  mask_ $ join $ modifyMVar m $ \ (i,b1,b2) ->
    if i == 0
       then do
         b <- newEmptyMVar
         return ((i, b1, b:b2), wait b)
       else do
         let !z = i-1
         return ((z, b1, b2), return ())
  where
    wait b = takeMVar b `onException` do
                (modifyMVar_ m $ \ (i,b1,b2) -> do
                   r <- tryTakeMVar b
                   if isJust r
                      then signal (i,b1,b2)
                      else do putMVar b (); return (i,b1,b2))

signalQSem :: QSem -> IO ()
signalQSem (QSem m) = modifyMVar_ m signal

signal :: (Int,[MVar ()],[MVar ()]) -> IO (Int,[MVar ()],[MVar ()])
signal (i,b1,b2) =
 if i == 0
   then loop b1 b2
   else let !z = i+1 in return (z, b1, b2)
 where
   loop [] [] = return (1, [], [])
   loop [] b2 = loop (reverse b2) []
   loop (b:bs) b2 = do
     r <- tryPutMVar b ()
     if r then return (0, bs, b2)
          else loop bs b2
