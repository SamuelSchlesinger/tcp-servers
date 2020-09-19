{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import Data.IORef
import Control.Concurrent
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

withServer :: ServerConfig -> (Server -> IO a) -> IO a
withServer serverConfig action = uninterruptibleMask \restore -> do
  server <- createServer serverConfig
  catch (Right <$> action server) (pure . Left @SomeException)
    >>= \case
      Left err -> do
        destroyServer server
        throwIO err
      Right a -> pure a

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

data Client = Client
  { interface :: Socket
  , clientConfig :: ClientConfig
  }

data ClientConfig = ClientConfig
  { remoteAddress :: SockAddr }

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
  sequence_ do
    _ <- [1..4]
    pure . forever $ do
      threadDelay 10
      client <- createClient clientConfig
      let cliSock = interface client
      sendMany cliSock ["h","e","l","l","o"]
      bs <- recv cliSock 12
      guard (bs == "how are you?")
      pure ()
  withServer serverConfig (runServer (perClient n))
  where
    socketAddress = SockAddrInet 8081 (tupleToHostAddress (127, 0, 0, 1))
    clientConfig = ClientConfig { remoteAddress = socketAddress }
    serverConfig = ServerConfig { maximumQueuedConnections = 100, serverAddress = socketAddress }
    perClient n clientSocket _ = flip finally (close clientSocket) $ do
      nn <- atomicModifyIORef' n (\n -> (n + 1, n))
      bs <- recv clientSocket 10
      guard (bs == "hello")
      send clientSocket "how are you?"
      putStrLn ("server: " <> show nn)
      close clientSocket
