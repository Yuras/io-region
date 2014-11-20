
-- example1

import Data.IORef
import Control.Exception (SomeException)
import qualified Control.Exception as E
import System.IO.Unsafe (unsafePerformIO)

numHandles :: IORef Int
numHandles = unsafePerformIO $ newIORef 0

data Handle = Handle

openFile :: FilePath -> IO Handle
openFile _ = do
  modifyIORef' numHandles succ
  return Handle

hClose :: Handle -> IO ()
hClose _ = modifyIORef numHandles pred

hPutStr :: Handle -> String -> IO ()
hPutStr _ _ = return ()

test :: IO () -> IO ()
test action = do
  action `E.catch` \e -> do
    putStrLn $ "exception: " ++ show (e :: SomeException)
  readIORef numHandles >>= putStrLn . ("Number of open handles: " ++) . show

example :: IO ()
example = do
  h <- openFile "path"
  hPutStr h "Hello"
  --error "something went wrong"
  hClose h

main :: IO ()
main = test example
