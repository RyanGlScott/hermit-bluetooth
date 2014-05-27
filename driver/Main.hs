{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Concurrent
import Control.Monad

import HERMIT.Bluetooth

import Network.Socket

backlog, defaultPort :: Int
backlog = 1
defaultPort = 3

traceAction :: String -> IO a -> IO a
traceAction = (>>) . putStrLn

main :: IO ()
main = do
    withSocketsDo $ defaultAdapter >>= \da -> case da of
            Nothing -> putStrLn "Error: cannot find Bluetooth adapter"
            Just _ -> do
                traceAction "startupBluetoothService" startupBluetoothService
                mvar <- newEmptyMVar
                _ <- forkIO $ do
                    handshakeSock <- traceAction "socketRFCOMM" socketRFCOMM
                    traceAction "bindRFCOMM" $ bindRFCOMM handshakeSock defaultPort
                    traceAction "listenRFCOMM" $ listenRFCOMM handshakeSock backlog
                    service <- traceAction ("registerBluetoothService (port " ++ show defaultPort ++ ")")
                                    $ registerBluetoothService handshakeSock 0 defaultPort
                    connSock <- traceAction "acceptRFCOMM" $ acceptRFCOMM handshakeSock
                    hermitLoop handshakeSock connSock service defaultPort mvar
                void $ takeMVar mvar
                
hermitLoop :: Socket -> Socket -> BluetoothService -> Int -> MVar Int -> IO ()
hermitLoop hSock cSock service port mvar = do
    putStr "recv: "
    mm <- recv cSock 4092
    case mm of
         [] -> do
             traceAction "close connection socket" $ close cSock
             traceAction "closeBluetoothService" $ closeBluetoothService service
             traceAction "close handshakeSock socket" $ close hSock
             traceAction "cleanupBluetoothService" cleanupBluetoothService
             putMVar mvar port
         message -> do
             putStrLn message
             let !response = hermitMagic message
             _ <- traceAction ("send: " ++ response) $ send cSock response
             hermitLoop hSock cSock service port mvar

-- Fill in these trivial details later ;)
hermitMagic :: String -> String
hermitMagic = flip (++) " (don't forget Haskell!)"