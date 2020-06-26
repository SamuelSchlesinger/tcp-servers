{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import Data.IORef
import Control.Concurrent
import Options.Commander
import Control.Exception
import Data.Word
import Network.Socket
import Network.Socket.ByteString
import Text.Read (readMaybe)
import Data.Text (unpack)
import Control.Monad (forever, guard)

data Server = Server
  { listener :: Socket
  , serverConfig :: ServerConfig }

data ServerConfig = ServerConfig
  { maximumQueuedConnections :: Int
  , serverAddress :: SockAddr }

createServer :: ServerConfig -> IO Server
createServer serverConfig@ServerConfig
  { maximumQueuedConnections
  , serverAddress } = do
  listener <- socket AF_INET Stream 0
  -- basically, this is convenient.
  -- see: https://hea-www.harvard.edu/~fine/Tech/addrinuse.html
  setSocketOption listener ReuseAddr 1
  bind listener serverAddress
  listen listener maximumQueuedConnections
  pure $ Server
    { listener
    , serverConfig }

destroyServer :: Server -> IO ()
destroyServer Server { listener } = close listener

runServer :: (Socket -> SockAddr -> IO ()) -> Server -> IO ()
runServer perClient server@Server
  { listener } = forever $ do
    client <- accept listener
    forkIO $ uncurry perClient client

withServer :: ServerConfig -> (Server -> IO ()) -> IO ()
withServer serverConfig = bracket (createServer serverConfig) destroyServer

data ClientConfig = ClientConfig
  { remoteAddress :: SockAddr }

data Client = Client
  { interface :: Socket
  , clientConfig :: ClientConfig
  }

createClient :: ClientConfig -> IO Client
createClient clientConfig@ClientConfig
  { remoteAddress } = do
  interface <- socket AF_INET Stream 0
  connect interface remoteAddress
  pure $ Client { interface, clientConfig }

destroyClient :: Client -> IO ()
destroyClient = close . interface

withClient :: ClientConfig -> (Client -> IO ()) -> IO ()
withClient clientConfig = bracket (createClient clientConfig) destroyClient

main :: IO ()
main = do
  n <- newIORef 0
  sequence_ [forkIO $ forever $ do
    threadDelay 10
    client <- createClient clientConfig
    let cliSock = interface client
    send cliSock "hello" 
    bs <- recv cliSock 12
    guard (bs == "how are you?")
    pure () | i <- [1..4]]
  withServer serverConfig (runServer (perClient n))
  where
    clientConfig = ClientConfig {
        remoteAddress =
          SockAddrInet 8081 (tupleToHostAddress (127, 0, 0, 1))
      }
    serverConfig = ServerConfig {
        maximumQueuedConnections = 100,
        serverAddress =
          SockAddrInet 8081 (tupleToHostAddress (127, 0, 0, 1)) 
      }
    perClient n clientSocket _ = flip finally (close clientSocket) $ do
      nn <- atomicModifyIORef' n (\n -> (n + 1, n))
      bs <- recv clientSocket 10
      guard (bs == "hello")
      send clientSocket "how are you?"
      putStrLn ("server: " <> show nn)
      close clientSocket
