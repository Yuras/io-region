{-# LANGUAGE DeriveDataTypeable #-}

-- example6

import Data.Typeable
import Data.IORef
import Control.Applicative
import Control.Monad
import Control.Exception (Exception, SomeException)
import qualified Control.Exception as E
import Control.Concurrent hiding (killThread, throwTo)
import qualified Control.Concurrent as CC
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

{-# NOINLINE numHandles #-}
numHandles :: IORef Int
numHandles = unsafePerformIO $ newIORef 0

{-# NOINLINE dataWritten #-}
dataWritten :: IORef [String]
dataWritten = unsafePerformIO $ newIORef []

{-# NOINLINE asyncLock #-}
asyncLock :: MVar ()
asyncLock = unsafePerformIO $ newMVar ()

test :: IO () -> IO ()
test action = do
  action `E.catch` \e -> do
    putStrLn $ "exception: " ++ show (e :: SomeException)
  readIORef numHandles >>= putStrLn . ("Number of open handles: " ++) . show
  readIORef dataWritten >>= putStrLn . ("Data writtern to file: " ++) . show

data Handle = Handle (IORef (Maybe String))

openFile :: FilePath -> IO Handle
openFile _ = do
  modifyIORef' numHandles succ
  Handle <$> newIORef Nothing

hClose :: Handle -> IO ()
hClose h = hFlushFailing h
  `E.finally` modifyIORef numHandles pred

hFlushFailing :: Handle -> IO ()
hFlushFailing _ = do
  threadDelay (20 * 1000 * 1000)
  error "hFlush failed"

hFlush :: Handle -> IO ()
hFlush (Handle ref) = do
  val <- readIORef ref
  case val of
    Just str' -> modifyIORef dataWritten (str' :)
    _ -> return ()
  writeIORef ref Nothing

hPutStr :: Handle -> String -> IO ()
hPutStr h@(Handle ref) str = do
  hFlush h
  writeIORef ref (Just str)

data Ex = Ex
  deriving (Show, Typeable)

instance Exception Ex where

-- disable async exceptions, but automatically interrupt any interruptible action
--
-- pure man's implementation, requires support from RTS
-- it is even is not safe
interruptibleMask :: IO a -> IO a
interruptibleMask action = withMVar asyncLock $ \_ -> do
  pid <- myThreadId
  E.bracket (forkIOWithUnmask $ \unmask ->
              unmask $ forever $ CC.throwTo pid Ex)
            (E.uninterruptibleMask_ . CC.killThread)
            (const action)

killThread :: ThreadId -> IO ()
killThread pid = throwTo pid E.ThreadKilled

throwTo :: Exception e => ThreadId -> e -> IO ()
throwTo pid e = withMVar asyncLock $ const (E.throwTo pid e)

bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket allocate release use =
  E.mask $ \restore -> do
    resource <- allocate
    result <- E.catch (restore $ use resource) $ \e -> do
      void (interruptibleMask $ release resource) `E.catch` \e' ->
        putStrLn ("Ignoring exception: " ++ show (e' :: SomeException))
      E.throw (e :: SomeException)
    void (release resource)
    return result

example :: IO ()
example = void $ timeout (1 * 1000 * 1000) $
  bracket (openFile "path") hClose $ \h -> do
    hPutStr h "Hello"
    hPutStr h "World"
    threadDelay (2 * 1000 * 1000)

main :: IO ()
main = do
  pid <- myThreadId
  void $ forkIO $ do
    threadDelay (2 * 1000 * 1000)
    killThread pid
  test example
