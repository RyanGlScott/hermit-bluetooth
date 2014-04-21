{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Concurrent
import Control.Monad
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
                    hermitLoop connSock sdpSession defaultPort mvar
                void $ takeMVar mvar

forkListenThread :: IO () -> IO ()
forkListenThread = void . forkIO
                
hermitLoop :: Socket -> SDPSession -> Int -> MVar Int -> IO ()
hermitLoop sock sess port mvar = do
    putStr "recv: "
    mm <- recv sock 4092
    case mm of
         [] -> do
             close sock
             closeSDPSession sess
             putMVar mvar port
         message -> do
             putStrLn message
             let !response = hermitMagic message
             putStrLn $ "send: " ++ response
             _ <- send sock response
             hermitLoop sock sess port mvar

-- Fill in these trivial details later ;)
hermitMagic :: String -> String
hermitMagic = flip (++) " (don't forget Haskell!)"