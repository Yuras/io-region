
-- example2

import Data.IORef
import Control.Applicative
import Control.Monad
import Control.Exception (SomeException)
import qualified Control.Exception as E
import Control.Concurrent (threadDelay)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

{-# NOINLINE numHandles #-}
numHandles :: IORef Int
numHandles = unsafePerformIO $ newIORef 0

{-# NOINLINE dataWritten #-}
dataWritten :: IORef [String]
dataWritten = unsafePerformIO $ newIORef []

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
hFlushFailing _ = error "hFlush failed"

hFlush :: Handle -> IO ()
hFlush (Handle ref) = do
  val <- readIORef ref
  case val of
    Just str -> modifyIORef dataWritten (str :)
    _ -> return ()
  writeIORef ref Nothing

hPutStr :: Handle -> String -> IO ()
hPutStr h@(Handle ref) str = do
  hFlush h
  writeIORef ref (Just str)

bracket :: IO a -> (a -> IO ()) -> (a -> IO b) -> IO b
bracket allocate release use =
  E.mask $ \restore -> do
    resource <- allocate
    restore (use resource)
      `E.finally` release resource

example :: IO ()
example = void $ timeout (1 * 1000 * 1000) $
  bracket (openFile "path") hClose $ \h -> do
    hPutStr h "Hello"
    hPutStr h "World"
    threadDelay (2 * 1000 * 1000)

main :: IO ()
main = test example
