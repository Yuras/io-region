{-# LANGUAGE DeriveDataTypeable #-}

-- | Exception safe resource management

module Control.IO.Region
(
  Region,
  Key,
  AlreadyClosed(..),
  AlreadyFreed(..),
  region,
  open,
  close,
  alloc,
  alloc_,
  free,
  moveToSTM,
  moveTo,
  defer
)
where

import Data.Typeable
import Control.Applicative
import Control.Monad
import Control.Exception
import Control.Concurrent.STM

-- | Region owns resources and frees them on close
data Region = Region {
  resources :: TVar [(Key, IO ())],
  closed :: TVar Bool,
  nextKey :: TVar Int
  }
  deriving Eq

-- | Each resource is identified by unique key
data Key = Key {
  _keyIndex :: Int,
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

-- | Create new region. It will be automatically closed on exit
region :: (Region -> IO a) -> IO a
region = bracket open close

-- | Open new region. Prefer `region` function.
open :: IO Region
open = Region
     <$> newTVarIO []
     <*> newTVarIO False
     <*> newTVarIO 1

-- | Close the region. You probably should called it
-- when async exceptions are masked. Prefer `region` function.
-- It is error to close region twice
close :: Region -> IO ()
close r = do
  ress <- uninterruptibleMask_ $ atomically $ do
    guardOpen r
    ress <- readTVar (resources r)
    writeTVar (resources r) $! []
    writeTVar (closed r) True
    forM_ ress $ \(k, _) ->
      writeTVar (keyFreed k) True
    return (map snd ress)
  go ress
  where
  go [] = return $! ()
  go (res:ress) = res `finally` go ress

-- | Allocate resource inside the region
alloc :: Region
      -> IO a         -- ^ action to allocate resource
      -> (a -> IO ()) -- ^ action to cleanup resource
      -> IO (a, Key)  -- ^ the resource and it's key
alloc r acquire cleanup = mask_ $ do
  res <- acquire
  uninterruptibleMask_ $ atomically $ do
    guardOpen r
    k <- Key
      <$> readTVar (nextKey r)
      <*> pure r
      <*> newTVar False
    modifyTVar' (nextKey r) succ
    modifyTVar' (resources r) ((k, cleanup res) :)
    return (res, k)

-- | The same as `alloc`, but doesn't return the key
alloc_ :: Region -> IO a -> (a -> IO ()) -> IO a
alloc_ r a f = fst <$> alloc r a f

-- | Free the resource earler then it's region will be closed.
-- It will be removed from the region immediately.
-- It is error to free resource twice
free :: Key -> IO ()
free k = mask_ $ join $ atomically $ do
  let r = keyRegion k
  guardLive k
  m_res <- lookup k <$> readTVar (resources r)
  case m_res of
    Nothing -> throwSTM NotFound
    Just c -> do
      modifyTVar' (resources r) $ filter ((/= k) . fst)
      writeTVar (keyFreed k) True
      return c

-- | Move resource to other region.
-- The old key becomes invalid and should not be used
moveToSTM :: Key -> Region -> STM Key
moveToSTM k r = do
  guardLive k
  guardOpen (keyRegion k)
  m_res <- lookup k <$> readTVar (resources $ keyRegion k)
  case m_res of
    Nothing -> throwSTM NotFound
    Just c -> do
      guardOpen r
      modifyTVar' (resources $ keyRegion k) $ filter ((/= k) . fst)
      writeTVar (keyFreed k) True
      k' <- Key
         <$> readTVar (nextKey r)
         <*> pure r
         <*> newTVar False
      modifyTVar' (nextKey r) succ
      modifyTVar' (resources r) ((k', c) :)
      return k'

-- | Move resource to other region. See also `moveToSTM`
moveTo :: Key -> Region -> IO Key
moveTo k = atomically . moveToSTM k

-- | Defer action until region closed
defer :: Region -> IO () -> IO ()
defer r a = void $ alloc_ r (return $! ()) (const a)

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
