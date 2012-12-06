{-# LANGUAGE DeriveDataTypeable, BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module QSemN
      (  -- * General Quantity Semaphores
          QSemN,        -- abstract
          newQSemN,     -- :: Int   -> IO QSemN
          waitQSemN,    -- :: QSemN -> Int -> IO ()
          signalQSemN   -- :: QSemN -> Int -> IO ()
      ) where

import Control.Concurrent ( MVar, newEmptyMVar, takeMVar, tryTakeMVar
                          , putMVar, modifyMVar, modifyMVar_, newMVar
                          , tryPutMVar, isEmptyMVar)
import Control.Monad
import Data.Typeable
import Control.Exception
import Data.Maybe
import Debug.Trace

-- | 'QSemN' is a quantity semaphore in which the resource is aqcuired
-- and released in units of one. It provides guaranteed FIFO ordering
-- for satisfying blocked `waitQSemN` calls.
--
-- The pattern
--
-- >   bracket_ waitQSemN signalQSemN (...)
--
-- is safe; it never loses a unit of the resource.
--
data QSemN = QSemN !(MVar (Int, [(Int, MVar ())], [(Int, MVar ())]))
  deriving Typeable

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
-- it must do if it receives an exception while blocked in waitQSemN.
-- This means that when unblocking a thread in signalQSemN we must
-- first check whether the MVar is already full; the MVar lock on the
-- semaphore itself resolves race conditions between signalQSemN and a
-- thread attempting to dequeue itself.

-- |Build a new 'QSemN' with a supplied initial quantity.
--  The initial quantity must be at least 0.
newQSemN :: Int -> IO QSemN
newQSemN initial
  | initial < 0 = fail "newQSemN: Initial quantity must be non-negative"
  | otherwise   = do
      sem <- newMVar (initial, [], [])
      return (QSemN sem)

waitQSemN :: QSemN -> Int -> IO ()
waitQSemN (QSemN m) sz = do
  mask_ $ join $ modifyMVar m $ \ (i,b1,b2) ->
    let z = i-sz in
    if z < 0
       then do
         b <- newEmptyMVar
         return ((i, b1, (sz,b):b2), wait b)
       else do
         return ((z, b1, b2), return ())
  where
    wait b = do
        takeMVar b `onException` (uninterruptibleMask_ $
                (modifyMVar_ m $ \ (i,b1,b2) -> do
                   r <- tryTakeMVar b
                   if isJust r
                      then signal sz (i,b1,b2)
                      else do putMVar b (); return (i,b1,b2)))

signalQSemN :: QSemN -> Int -> IO ()
signalQSemN (QSemN m) sz = uninterruptibleMask_ $ do
  r <- takeMVar m
  r' <- signal sz r
  putMVar m r'

signal :: Int
       -> (Int,[(Int,MVar ())],[(Int,MVar ())])
       -> IO (Int,[(Int,MVar ())],[(Int,MVar ())])

signal sz (i,b1,b2) = loop (sz + i) b1 b2
 where
   loop 0  bs b2 = return (0,  bs, b2)
   loop sz [] [] = return (sz, [], [])
   loop sz [] b2 = loop sz (reverse b2) []
   loop sz ((j,b):bs) b2
     | j > sz = do
       r <- isEmptyMVar b
       if r then return (sz, (j,b):bs, b2)
            else loop sz bs b2
     | otherwise = do
       r <- tryPutMVar b ()
       if r then loop (sz-j) bs b2
            else loop sz bs b2
