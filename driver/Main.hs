{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Concurrent
import Control.Monad

import Foreign.Ptr

import HERMIT.Bluetooth.Adapter

import Network.Socket

defaultPort :: Int
defaultPort = 3

main :: IO ()
main = defaultAdapter >>= \da -> case da of
            Nothing -> putStrLn "Error: cannot find Bluetooth adapter"
            Just _ -> do
                putStrLn $ "sdp_register_service on port " ++ show defaultPort
                sdpSession <- registerSDPService 0 defaultPort
                mvar <- newEmptyMVar
                forkListenThread $ do
                    putStrLn "socket()"
                    handshakeSock <- socketRFCOMM
                    putStrLn "bind()"
                    bindRFCOMM handshakeSock defaultPort
                    putStrLn "listen()"
                    listenRFCOMM handshakeSock 1
                    putStrLn "accept()"
                    connSock <- acceptRFCOMM handshakeSock
                    hermitLoop handshakeSock connSock sdpSession defaultPort mvar
                void $ takeMVar mvar

forkListenThread :: IO () -> IO ()
forkListenThread = void . forkIO
                
hermitLoop :: Socket -> Socket -> Ptr SDPSession -> Int -> MVar Int -> IO ()
hermitLoop hSock cSock sess port mvar = do
    putStr "recv: "
    mm <- recv cSock 4092
    case mm of
         [] -> do
             close cSock
             closeSDPSession sess
             close hSock
             putMVar mvar port
         message -> do
             putStrLn message
             let !response = hermitMagic message
             putStrLn $ "send: " ++ response
             _ <- send cSock response
             hermitLoop hSock cSock sess port mvar

-- Fill in these trivial details later ;)
hermitMagic :: String -> String
hermitMagic = flip (++) " (don't forget Haskell!)"