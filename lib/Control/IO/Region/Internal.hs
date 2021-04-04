{-# LANGUAGE DeriveDataTypeable #-}

-- | These module exposes internals of the library

module Control.IO.Region.Internal
where

import Prelude (($!), Enum(..))
import Data.Typeable
import Data.Bool
import Data.Int
import Data.Either
import Data.Eq
import Data.Function
import qualified Data.List as List
import Data.Tuple
import Data.Maybe
import Control.Monad
import Control.Applicative
import Control.Exception
import Control.Concurrent.STM
import System.IO
import Text.Show

-- | Region owns resources and frees them on close
data Region = Region {
  resources :: TVar [(Key, IO ())],
  closed :: TVar Bool
  }
  deriving Eq

-- | Each resource is identified by unique key
data Key = Key {
  keyRegion :: Region,
  keyFreed :: TVar Bool
  }
  deriving Eq

-- | Resource not found in the specified region
data NotFound = NotFound
  deriving (Show, Typeable)

instance Exception NotFound where

-- | Region already closed
data AlreadyClosed = AlreadyClosed
  deriving (Show, Typeable)

instance Exception AlreadyClosed where

-- | Resource already freed
data AlreadyFreed = AlreadyFreed
  deriving (Show, Typeable)

instance Exception AlreadyFreed where

-- | Open new region. Prefer `Control.IO.Region.region` function.
open :: IO Region
open = Region
     <$> newTVarIO []
     <*> newTVarIO False

-- | Close the region. Prefer `Control.IO.Region.region` function.
--
-- It is an error to close region twice.
--
-- When `close` fails for any reason, the region is guaranteed to be closed
-- and all cleanup actions are called.
--
-- It will never ignore asynchronous exception in the cleanup action.
--
-- When exception occurs in one of the cleanup actions, `close` itself will
-- rethrow the exception. If more then one cleanup action throws synchronous
-- exception, then one of them is rethrown, others are ignored. If cleanup
-- action throws asynchronous exception, then subsequent cleanups are
-- executed in masking state `MaskedUninterruptible` to make sure other
-- asynchronous exception won't appear.
close :: Region -> IO ()
close r = mask_ $ do
  ress <- uninterruptibleMask_ $ atomically $ do
    guardOpen r
    ress <- readTVar (resources r)
    writeTVar (resources r) $! []
    writeTVar (closed r) True
    forM_ ress $ \(k, _) ->
      writeTVar (keyFreed k) True
    return (List.map snd ress)
  go ress
  where
  go [] = return $! ()
  go (res:ress) = do
    res `onExceptionEx` go ress
    go ress

-- | Extended version of `onException`, which ignores synchronous
-- exceptions from the handler.
onExceptionEx :: IO a -> IO b -> IO a
onExceptionEx io h = mask $ \restore ->
  -- mask above is necessary to make sure asynchronous exception
  -- won't appear after catching exception, but before handler is executed
  restore io `catch` \e -> do
    case fromException e of
      Just SomeAsyncException{} -> do
        -- we are handling asynchronous exception, and we don't want another
        -- one to appear, so mask them hard
        ignoreExceptions $ uninterruptibleMask_ h
        throwIO e
      Nothing -> ignoreExceptions h >> throwIO e

-- | Ignore any synchronous exception from the action
ignoreExceptions :: IO a -> IO ()
ignoreExceptions io = mask $ \restore -> do
  res <- try (restore io)
  case res of
    Left e -> case fromException e of
      Just SomeAsyncException{} -> throwIO e
      Nothing -> return $! ()
    Right _ -> return $! ()

-- | Allocate resource inside the region
--
-- The cleanup action should guarantee that the resource will be deallocated
-- even if it fails for any reason, including the case when it's interrupted
-- with asynchronous exception.
--
-- Cleanup action might expect to be called with asynchronous exceptions
-- masked, but the exact masking state, `MaskedInterruptible` or
-- `MaskedUninterruptible`, is not specified.
--
-- Cleanup should never throw asynchronous exception under
-- `MaskedUninterruptible`.
alloc :: Region
      -> IO a         -- ^ action to allocate resource
      -> (a -> IO ()) -- ^ action to cleanup resource
      -> IO (a, Key)  -- ^ the resource and it's key
alloc r acquire cleanup = mask_ $ do
  res <- acquire
  uninterruptibleMask_ $ atomically $ do
    guardOpen r
    k <- Key
      <$> pure r
      <*> newTVar False
    modifyTVar' (resources r) ((k, cleanup res) :)
    return (res, k)

-- | Free the resource earlier then it's region will be closed.
-- It will be removed from the region immediately.
-- It is error to free resource twice
free :: Key -> IO ()
free k = mask_ $ join $ atomically $ do
  let r = keyRegion k
  guardLive k
  m_res <- List.lookup k <$> readTVar (resources r)
  case m_res of
    Nothing -> throwSTM NotFound
    Just c -> do
      modifyTVar' (resources r) $ List.filter ((/= k) . fst)
      writeTVar (keyFreed k) True
      return c

-- | Move resource to other region.
-- The old key becomes invalid and should not be used
moveToSTM :: Key -> Region -> STM Key
moveToSTM k r = do
  guardLive k
  guardOpen (keyRegion k)
  m_res <- List.lookup k <$> readTVar (resources $ keyRegion k)
  case m_res of
    Nothing -> throwSTM NotFound
    Just c -> do
      guardOpen r
      modifyTVar' (resources $ keyRegion k) $ List.filter ((/= k) . fst)
      writeTVar (keyFreed k) True
      k' <- Key
         <$> pure r
         <*> newTVar False
      modifyTVar' (resources r) ((k', c) :)
      return k'

guardOpen :: Region -> STM ()
guardOpen r = do
  c <- readTVar (closed r)
  when c $
    throwSTM AlreadyClosed

guardLive :: Key -> STM ()
guardLive k = do
  f <- readTVar (keyFreed k)
  when f $
    throwSTM AlreadyFreed
