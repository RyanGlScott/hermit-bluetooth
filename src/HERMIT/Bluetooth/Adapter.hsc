{-# LANGUAGE CPP, EmptyDataDecls #-}
module HERMIT.Bluetooth.Adapter (
      defaultAdapter
    , startupBluetoothService
    , registerBluetoothService
    , closeBluetoothService
    , cleanupBluetoothService
    , socketRFCOMM
    , bindRFCOMM
    , listenRFCOMM
    , acceptRFCOMM

    , Adapter
    , BluetoothService(..)
    ) where

#if defined(mingw32_HOST_OS)
#include <windows.h>
#elif defined(linux_HOST_OS)
#include <bluetooth/bluetooth.h>
#include <bluetooth/hci.h>
#endif
#include <stddef.h>

import Control.Concurrent
import qualified Control.Monad as M

import Foreign.C.Error
import Foreign.Ptr

import HERMIT.Bluetooth.Types
#if defined(mingw32_HOST_OS)
import HERMIT.Bluetooth.Win32
#elif defined(darwin_HOST_OS)
import HERMIT.Bluetooth.MacOSX
#elif defined(linux_HOST_OS)
import Control.Applicative
import Control.Exception

import Foreign.C.String
import Foreign.C.Types

import HERMIT.Bluetooth.Linux
#endif

import Network.Socket

mAX_CONNECTIONS :: Num a => a
mAX_CONNECTIONS = 7

bTPROTO_RFCOMM :: ProtocolNumber
#if defined(mingw32_HOST_OS)
bTPROTO_RFCOMM = 0x003 -- BTHPROTO_RFCOMM
#elif defined(linux_HOST_OS)
bTPROTO_RFCOMM = #{const BTPROTO_RFCOMM}
#endif

-- allAdapters :: IO [Adapter]
-- #if defined(mingw32_HOST_OS)
-- allAdapters = return [Adapter]
-- #elif defined(linux_HOST_OS)
-- allAdapters = do
    -- devsRef <- newIORef []
    -- cb <- mkVisitDev $ \_ dev_id _ -> do
        -- modifyIORef devsRef (dev_id:)
        -- pure 0
    -- hci_for_each_dev #{const HCI_UP} cb 0
      -- `finally`
        -- freeHaskellFunPtr cb
    -- dev_ids <- reverse <$> readIORef devsRef
    -- mapM openDev dev_ids
-- #endif

#if defined(linux_HOST_OS)
openDev :: CInt -> IO Adapter
openDev dev_id = do
    ret <- hci_open_dev dev_id
    if ret < 0 then do
        errno@(Errno errno_) <- getErrno
        if errno == eINTR
            then openDev dev_id
            else do
                err <- peekCString (strerror errno_)
                throwIO $ BluetoothException "openDev" err
      else
        pure $ Adapter dev_id ret
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
                err <- peekCString $ strerror errno_
                throwIO $ BluetoothException "defaultAdapter" err
      else
        Just <$> openDev ret
#endif

startupBluetoothService :: IO ()
#if defined(mingw32_HOST_OS)
startupBluetoothService = M.void $ throwErrnoIfMinus1 "newBluetoothService" wsa_startup
#elif defined(linux_HOST_OS)
startupBluetoothService = return ()
#endif

registerBluetoothService :: Socket -> Int -> Int -> IO BluetoothService
#if defined(mingw32_HOST_OS)
registerBluetoothService (MkSocket s _ _ _ _) index _ = do
    wqs <- wsa_register_service (fromIntegral s) safeIndex
    M.when (wqs == nullPtr) $ ioError $ userError "registerBluetoothService: service registration failed"
    return $ BluetoothService wqs
#elif defined(linux_HOST_OS)
registerBluetoothService _ index port = fmap BluetoothService $ sdp_register_service safeIndex (fromIntegral port)
#endif
    where safeIndex | 0 <= index && index < mAX_CONNECTIONS = fromIntegral index
                    | otherwise = 0

closeBluetoothService :: BluetoothService -> IO ()
closeBluetoothService service = M.void $ throwErrnoIfMinus1 "closeBluetoothService" $
#if defined(mingw32_HOST_OS)
    wsa_delete_service $ runService service
#elif defined(linux_HOST_OS)
    sdp_close $ runService service
#endif

cleanupBluetoothService :: IO ()
cleanupBluetoothService =
#if defined(mingw32_HOST_OS)
    M.void $ throwErrnoIfMinus1 "cleanupBluetoothService" wsa_cleanup
#elif defined(linux_HOST_OS)
    return ()
#endif

socketRFCOMM :: IO Socket
socketRFCOMM = do
    _ <- packSocketTypeOrThrow "socketRFCOMM" Stream
    fd <- throwErrnoIfMinus1 "socketRFCOMM" $ socket_rfcomm
#if defined(mingw32_HOST_OS)
    M.when (fd == iNVALID_SOCKET) $ ioError $ userError "socketRFCOMM: invalid socket"
    _ <- throwErrnoIfMinus1 "socketRFCOMM" $ get_sock_opt fd
#endif
    socketStatus <- newMVar NotConnected
    return $ MkSocket (fromIntegral fd) AF_BLUETOOTH Stream bTPROTO_RFCOMM socketStatus

bindRFCOMM :: Socket -> Int -> IO ()
bindRFCOMM (MkSocket s _ _ _ socketStatus) _port = do
    modifyMVar_ socketStatus $ \ status -> do
        M.when (status /= NotConnected) $ ioError $ userError $ "bindRFCOMM: can't perform bind on socket in status " ++ show status
        _ <- throwErrnoIfMinus1 "bindRFCOMM" $ bind_rfcomm
#if defined(mingw32_HOST_OS)
                                                           $ fromIntegral s
#elif defined(linux_HOST_OS)
                                                           s $ fromIntegral _port
#endif
        return Bound

listenRFCOMM :: Socket -> Int -> IO ()
listenRFCOMM (MkSocket s _family _stype _protocol socketStatus) backlog = do
    modifyMVar_ socketStatus $ \ status -> do
        M.when (status /= Bound) $ ioError $ userError $ "listenRFCOMM: can't peform listen on socket in status " ++ show status
        _ <- throwErrnoIfMinus1 "listenRFCOMM" $ listen_rfcomm (fromIntegral s) (fromIntegral backlog)
        return Listening

acceptRFCOMM :: Socket -> IO Socket
acceptRFCOMM (MkSocket s family stype protocol socketStatus) = do
    status <- readMVar socketStatus
    M.when (status /= Listening) $ ioError $ userError $ "acceptRFCOMM: can't perform accept on socket in status" ++ show status
    new_sock <- throwErrnoIfMinus1 "acceptRFCOMM" $ accept_rfcomm $ fromIntegral s
#if defined(mingw32_HOST_OS)
    M.when (new_sock == iNVALID_SOCKET) $ ioError $ userError "acceptRFCOMM: invalid socket"
#endif
    new_status <- newMVar Connected
    return $ MkSocket (fromIntegral new_sock) family stype protocol new_status