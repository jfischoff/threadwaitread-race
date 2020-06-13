module Main where

import Network.Socket
import Network.Socket.ByteString
import Control.Concurrent.STM
import Control.Concurrent
import Control.Exception
import Network.Socket.Free
import Control.Concurrent.Async
import System.Timeout
import Control.Monad
import System.Exit

main :: IO ()
main = bracket openFreePort (close . snd) $ \(port, sock) -> do
  putStrLn "starting"
  listen sock 128
  ready <- newEmptyMVar
  serverThread <- async $ server ready sock
  clientThread <- async $ client ready port
  wait serverThread
  cancel clientThread

client :: MVar () -> Int -> IO ()
client ready port = do
  bracket (socket AF_INET Stream defaultProtocol) close $ \s -> do
    connect s $ SockAddrInet  (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1))
    -- Wait for threadWaitReadSTM to register a socket read ready callback
    takeMVar ready
    void $ send s "hi"
    forever $ threadDelay 10000000

server :: MVar () -> Socket -> IO ()
server ready sock = flip onException (tryPutMVar ready ()) $ do
  (aSock, _) <- accept sock
  withFdSocket aSock $ \fd -> do
    -- Register a socket read ready callback
    waiter <- fmap fst $ threadWaitReadSTM (fromIntegral fd)
    waitThread <- async $ atomically waiter >> putStrLn "threadWaitRead return!"
    putMVar ready ()
    threadDelay 1
    print =<< recv aSock 3
    timeout 1000000 (wait waitThread) >>= \case
      Just _ -> pure ()
      Nothing -> die "timed out"
