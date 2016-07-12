-- ChatDemo.hs
-- Copyright (c) 2013 Well-Typed LLP
--
-- WARNING:
--
-- This is an example solution of (more or less) what's requested
-- in Chat.hs. You can use it to test the behaviour of your client or
-- server as long as you haven't implemented the other part yet.
--
-- Compile this file with GHC. Then invoke
--
--   ./ChatDemo server
--
-- or
--
--   ./ChatDemo client [hostname]
--
-- to start a server or client, respectively. If you start the server
-- locally, use localhost as hostname.
--
-- The port number is hardcoded below to be 9999. If this is impossible
-- on your system, just change it.

{-# LANGUAGE ScopedTypeVariables #-}
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Data.Functor
import Data.Map as Map
import Network
import Prelude hiding (catch)
import System.Console.Haskeline hiding (catch)
import System.Environment
import System.IO

-- A chat server.

type Nick = String

data Server = Server
  { clients :: TVar (Map Nick Handle) }

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["server"] -> server
    ["client", host] -> client host

chatPort :: PortID
chatPort = PortNumber 9999

client :: String -> IO ()
client host = withSocketsDo $ do
  handle <- connectTo host chatPort
  hSetBuffering handle LineBuffering
  nick <- putInitialNick handle
  forkIO $ clientReceive handle
  clientPost handle nick

clientReceive :: Handle -> IO ()
clientReceive handle = forever $ do
  txt <- hGetLine handle
  putStrLn txt

clientPost :: Handle -> Nick -> IO ()
clientPost handle nick = runInputT defaultSettings go
  where
    go :: InputT IO ()
    go = do
      inp <- getInputLine $ "> "
      case inp of
        Nothing  -> return () -- disconnect
        Just txt -> do
          liftIO $ hPutStrLn handle txt
          go

putInitialNick :: Handle -> IO Nick
putInitialNick handle = do
  putStrLn "Enter nick:"
  nick <- getLine
  hPutStrLn handle nick
  reply <- hGetLine handle
  if read reply
    then do
      putStrLn "Nick accepted. Welcome to the chat."
      return nick
    else do
      putStrLn "Nick is illegal or already taken."
      putInitialNick handle

server :: IO ()
server = withSocketsDo $ do
  server <- Server <$> newTVarIO empty
  s <- listenOn chatPort
  forever $ do
    (handle, host, port) <- accept s -- blocks
    hSetBuffering handle LineBuffering
    putStrLn $ "connection from " ++ show host ++ ":" ++ show port
    forkIO $ handleClient server handle

handleClient :: Server -> Handle -> IO ()
handleClient server@(Server clients) handle = do
  nick <- getInitialNick server handle
  catch (chatLoop server handle nick)
        (\ (e :: IOException) -> do
           -- clean up
           putStrLn $ "closing connection for " ++ nick
           atomically $ modifyTVar clients (Map.delete nick)
           hClose handle
        )

chatLoop :: Server -> Handle -> Nick -> IO ()
chatLoop server@(Server clients) handle nick = forever $ do
  txt <- hGetLine handle
  -- broadcast to everyone but me
  -- we cannot synchronize the actual broadcast
  clientMap <- readTVarIO clients
  let handles = Map.elems
              $ Map.filterWithKey (\ otherNick _ -> nick /= otherNick)
                                  clientMap 
  catch (forM_ handles $ \ handle ->
           hPutStrLn handle ("[" ++ nick ++ "] " ++ txt))
        (\ (e :: IOException) -> return ())

getInitialNick :: Server -> Handle -> IO Nick
getInitialNick server@(Server clients) handle = do
  initialNick <- hGetLine handle
  
  -- try to register the nickname
  join $ atomically $ do
    clientsMap <- readTVar clients
    if initialNick `elem` keys clientsMap
      then return $ do
        hPutStrLn handle (show False)
        getInitialNick server handle
      else do
        writeTVar clients $ insert initialNick handle clientsMap
        return $ do
          hPutStrLn handle (show True)
          return initialNick
