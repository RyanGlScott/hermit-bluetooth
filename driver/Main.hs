{-# LANGUAGE BangPatterns, LambdaCase #-}
{- This is an experimental version of the server to allow multiple client connections, 
   which is still a work in progress -}

module Main where

import Control.Concurrent
import Control.Monad

import HERMIT.Bluetooth

import Network.Socket

import System.IO
import Control.Exception
import Control.Concurrent.Chan
import Control.Concurrent
import Control.Monad.Fix (fix)

backlog, defaultPort :: Int
backlog = 7
defaultPort = 3

traceAction :: String -> IO a -> IO a
traceAction = (>>) . putStrLn

main :: IO ()
main = do
    withSocketsDo $ hSetBuffering stdout NoBuffering >> defaultAdapter >>= \case
            Nothing -> putStrLn "Error: cannot find Bluetooth adapter"
            Just _ -> do
                broadcast <- newChan
                uuids     <- newChan
                mapM_ (writeChan uuids) [0] --[0..mAX_CONNECTIONS-1]

                traceAction "startupBluetoothService" startupBluetoothService
                handshakeSock <- traceAction "socketRFCOMM" socketRFCOMM
                traceAction "bindRFCOMM" $ bindRFCOMM handshakeSock defaultPort
                traceAction "listenRFCOMM" $ listenRFCOMM handshakeSock backlog

                forkIO $ mainLoop handshakeSock broadcast uuids
                -- this uses the main thread to college garbage written to threads
                fix $ \loop -> do 
                  msg <- readChan broadcast
                  putStrLn $ "Broadcasted: " ++ msg
                  loop
                
mainLoop :: Socket -> Chan String -> Chan Int -> IO ()
mainLoop handshakeSock chan uuids = do
  id <- readChan uuids -- get next available uuid
  uuids' <- dupChan uuids
  -- register the port before accepting anything 
  service <- traceAction ("registerBluetoothService (port " ++ show defaultPort ++ ")")
               $ registerBluetoothService handshakeSock id defaultPort
  connSock <- traceAction "acceptRFCOMM" $ acceptRFCOMM handshakeSock   
  putStrLn "Accepted new device"
  forkIO $ hermitLoop connSock chan uuids' id service
  mainLoop handshakeSock chan uuids


hermitLoop :: Socket -> Chan String -> Chan Int -> Int -> BluetoothService -> IO ()
hermitLoop sock broadChan uuids id service = do
    let broadcast msg = writeChan broadChan msg
    comChan <- dupChan broadChan
    -- loop that handles sending messages to the attached device
    reader <- forkIO $ fix $ \loop -> do
      !msg <- readChan comChan
      traceAction ("send " ++ msg) $ send sock msg
      loop
    -- receives messages from device, writes them to the broadcast channel
    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
      msg <- recv sock 4092
      case msg of 
        "quit" -> return ()
        _      -> do 
           let response = hermitMagic msg
           broadcast response
           threadDelay 2000
           loop
    -- shutdown down reader and clean up, push this UUID back into pool
    killThread reader
    send sock "Closing Connection"
    traceAction "Close Client Connection" $ close sock
    traceAction "Close Bluetooth Service" $ closeBluetoothService service
    traceAction "cleanupBluetoothService" cleanupBluetoothService
    traceAction "Push UUID back to pool " $ writeChan uuids id
    
-- Fill in these trivial details later ;)
hermitMagic :: String -> String
hermitMagic = flip (++) " (don't forget Haskell!)"
