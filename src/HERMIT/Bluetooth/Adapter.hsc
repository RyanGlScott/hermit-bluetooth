{-# LANGUAGE CPP, EmptyDataDecls, ForeignFunctionInterface #-}
module HERMIT.Bluetooth.Adapter (
      allAdapters
    , defaultAdapter
    , registerSDPService
    , closeSDPSession
    , socketRFCOMM
    , bindRFCOMM
    , listenRFCOMM
    , acceptRFCOMM

    , Adapter
    , SDPSession
    ) where

#if defined(mingw32_HOST_OS)
#include <windows.h>
#elif defined(linux_HOST_OS)
#include <bluetooth/bluetooth.h>
#include <bluetooth/hci.h>
#endif
#include <stddef.h>

import Control.Applicative
import Control.Concurrent
import Control.Exception
import qualified Control.Monad as M

import Data.IORef

import Foreign.C.Error
import Foreign.C.String
import Foreign.Ptr

import HERMIT.Bluetooth.Types
#if defined(mingw32_HOST_OS)
import HERMIT.Bluetooth.Win32
#elif defined(darwin_HOST_OS)
import HERMIT.Bluetooth.MacOSX
#elif defined(linux_HOST_OS)
import HERMIT.Bluetooth.Linux
#endif

import Network.Socket

#if defined(mingw32_HOST_OS)
import System.Win32.Types
#endif

mAX_CONNECTIONS :: Num a => a
mAX_CONNECTIONS = 7

bTPROTO_RFCOMM :: ProtocolNumber
#if defined(mingw32_HOST_OS)
bTPROTO_RFCOMM = #{const BTHPROTO_RFCOMM}
#elif defined(linux_HOST_OS)
bTPROTO_RFCOMM = #{const BTPROTO_RFCOMM}
#endif

allAdapters :: IO [Adapter]
#if defined(mingw32_HOST_OS)
allAdapters = return [Adapter]
#elif defined(linux_HOST_OS)
allAdapters = do
    devsRef <- newIORef []
    cb <- mkVisitDev $ \_ dev_id _ -> do
        modifyIORef devsRef (dev_id:)
        pure 0
    hci_for_each_dev #{const HCI_UP} cb 0
      `finally`
        freeHaskellFunPtr cb
    dev_ids <- reverse <$> readIORef devsRef
    mapM openDev dev_ids
#endif

defaultAdapter :: IO (Maybe Adapter)
#if defined(mingw32_HOST_OS)
defaultAdapter = return $ Just Adapter
#elif defined(linux_HOST_OS)
defaultAdapter = do
    ret <- hci_get_route nullPtr
    if ret < 0 then do
        errno@(Errno errno_) <- getErrno
        if errno == eINTR
            then defaultAdapter
          else if errno == eNODEV
            then pure Nothing
            else do
                err <- peekCString (strerror errno_)
                throwIO $ BluetoothException "defaultAdapter" err
      else
        Just <$> openDev ret
#endif

#if defined(linux_HOST_OS)
registerSDPService :: Int -> Int -> IO (Ptr SDPSession)
registerSDPService ind port = sdp_register_service (fromIntegral safeInd) (fromIntegral port)
    where safeInd | 0 <= ind && ind < mAX_CONNECTIONS = ind
                  | otherwise = 0

closeSDPSession :: Ptr SDPSession -> IO ()
closeSDPSession = M.void . sdp_close
#endif

socketRFCOMM :: IO Socket
socketRFCOMM = do
    _ <- packSocketTypeOrThrow "socketRFCOMM" Stream
    fd <- throwSocketErrorIfMinus1 "socketRFCOMM" $ socket_rfcomm
    socketStatus <- newMVar NotConnected
    return $ MkSocket fd AF_BLUETOOTH Stream bTPROTO_RFCOMM socketStatus

bindRFCOMM :: Socket -> Int -> IO ()
bindRFCOMM (MkSocket s _family _stype _protocol socketStatus) port = do
    modifyMVar_ socketStatus $ \ status -> do
        if status /= NotConnected
           then ioError (userError ("bindRFCOMM: can't perform bind on socket in status " ++ show status))
           else do
               _ <- throwSocketErrorIfMinus1 "bind" $ bind_rfcomm s $ fromIntegral port
               return Bound

throwSocketErrorIfMinus1 :: (Eq a, Num a) => String -> IO a -> IO a
throwSocketErrorIfMinus1 = throwErrnoIfMinus1

listenRFCOMM :: Socket -> Int -> IO ()
listenRFCOMM (MkSocket s _family _stype _protocol socketStatus) backlog = do
    modifyMVar_ socketStatus $ \ status -> do
        if status /= Bound
           then ioError (userError ("listenRFCOMM: can't peform listen on socket in status " ++ show status))
           else do
               _ <- throwSocketErrorIfMinus1 "listenRFCOMM" $ listen_rfcomm s $ fromIntegral backlog
               return Listening

acceptRFCOMM :: Socket -> IO Socket
acceptRFCOMM (MkSocket s family stype protocol socketStatus) = do
    status <- readMVar socketStatus
    if status /= Listening
       then ioError (userError ("acceptRFCOMM: can't perform accept on socket in status" ++ show status))
       else do
           new_sock <- throwSocketErrorIfMinus1 "acceptRFCOMM" $ accept_rfcomm s
           new_status <- newMVar Connected
           return $ MkSocket new_sock family stype protocol new_status