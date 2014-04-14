{-# LANGUAGE BangPatterns #-}
module Main where

import HERMIT.Bluetooth.Adapter
import HERMIT.Bluetooth.Info
import Network.Socket

port :: Num a => a
port = 3

main :: IO ()
main = defaultAdapter >>= \da -> case da of
            Nothing -> putStrLn "Error: cannot find Bluetooth adapter"
            Just _ -> do
                sdpSession <- registerSDPService 0 port
                handshakeSock <- socket AF_BLUETOOTH Stream bTPROTO_RFCOMM
                bind handshakeSock $ SockAddrInet port iNADDR_ANY
                listen handshakeSock 1
                (connSock, _) <- accept handshakeSock
                hermitLoop connSock sdpSession

hermitLoop :: Socket -> SDPSession -> IO ()
hermitLoop sock sess = do
    mm <- recv sock 4092
    case mm of
         [] -> do
             close sock
             closeSDPSession sess
         message -> do
             let !response = hermitMagic message
             _ <- send sock response
             hermitLoop sock sess

-- Fill in these trivial details later ;)
hermitMagic :: String -> String
hermitMagic = flip (++) " (don't forget Haskell!)"