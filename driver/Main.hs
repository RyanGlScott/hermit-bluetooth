{-# LANGUAGE BangPatterns #-}
module Main where

import HERMIT.Bluetooth.Adapter
import Network.Socket

port :: Num a => a
port = 3

main :: IO ()
main = defaultAdapter >>= \da -> case da of
            Nothing -> putStrLn "Error: cannot find Bluetooth adapter"
            Just _ -> do
                putStrLn "sdp_register_service()"
                sdpSession <- registerSDPService 0 port
                putStrLn "socket()"
                handshakeSock <- socketRFCOMM
                putStrLn "bind()"
                bindRFCOMM handshakeSock port
                putStrLn "listen()"
                listenRFCOMM handshakeSock 1
                putStrLn "accept()"
                connSock <- acceptRFCOMM handshakeSock
                hermitLoop connSock sdpSession

hermitLoop :: Socket -> SDPSession -> IO ()
hermitLoop sock sess = do
    putStrLn "recv()"
    mm <- recv sock 4092
    case mm of
         [] -> do
             close sock
             closeSDPSession sess
         message -> do
             let !response = hermitMagic message
             putStrLn "send()"
             _ <- send sock response
             hermitLoop sock sess

-- Fill in these trivial details later ;)
hermitMagic :: String -> String
hermitMagic = flip (++) " (don't forget Haskell!)"