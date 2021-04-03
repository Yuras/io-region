-- | Exception safe resource management
--
-- Examples:
--
-- @
-- import Control.IO.Region (region)
-- import qualified Control.IO.Region as R
--
-- ...
--   region $ \\r -> do
--     resource <- R.alloc_ r allocate free
--     use resource
--     -- resource will be automatically freed here
--
-- ...
--   region $ \\r -> do
--     (resource, key) <- R.alloc r allocate free
--     use resource
--     if ...
--       then R.free key  -- free it earler
--       else use resource
--
-- ...
--   region $ \\r1 -> do
--     resource \<- region $ \\r2 -> do
--       (resource1, key) <- R.alloc r2 allocate free
--       use resource
--       resource \`R.moveTo\` r1  -- transfer ownership to region r1
--       return resource
--     doSomethingElse resource
--     -- resource will be freed here
--
-- ...
--   region $ \\r1 -> do
--     (r2, r2Key) <- R.alloc r1 R.open R.close  -- region is a resource too
--     resource <- R.alloc r2 allocate free
--     use resource
--     r2Key \`R.moveTo\` r3  -- move region r2 ownership (and also the resource) to other region
-- @

module Control.IO.Region
(
  Region,
  Key,
  region,
  open,
  close,
  alloc,
  alloc_,
  free,
  moveTo,
  defer
)
where

import Control.Concurrent.STM
import Control.Exception
import Control.IO.Region.Internal
import Control.Monad

-- | Create new region. It will be automatically closed on exit
region :: (Region -> IO a) -> IO a
region use = mask $ \restore -> do
  r <- open
  result <- restore (use r)
    `onExceptionEx` close r
  close r
  return result

-- | The same as `alloc`, but doesn't return the key
alloc_ :: Region -> IO a -> (a -> IO ()) -> IO a
alloc_ r a f = fst <$> alloc r a f

-- | Move resource to other region. See also `moveToSTM`
moveTo :: Key -> Region -> IO Key
moveTo k = atomically . moveToSTM k

-- | Defer action until region is closed
defer :: Region -> IO () -> IO ()
defer r a = void $ alloc_ r (return $! ()) (const a)
