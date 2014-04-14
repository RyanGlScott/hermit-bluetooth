{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
{-# CFILES sdp_register_service.c #-}
module HERMIT.Bluetooth.Adapter (
        allAdapters,
        defaultAdapter,
        registerSDPService,
        closeSDPSession,

        Adapter,
        BluetoothException(..),
        BluetoothAddr(..),
        SDPSession
    ) where

#if defined(mingw32_HOST_OS)
#include <windows.h>
#else
#include <bluetooth/bluetooth.h>
#include <bluetooth/hci.h>
#include <bluetooth/sdp.h>
#include <bluetooth/sdp_lib.h>
#include "sdp_register_service.h"
#endif
#include <stddef.h>

import HERMIT.Bluetooth.Info
import HERMIT.Bluetooth.Types
#if defined(mingw32_HOST_OS)
import HERMIT.Bluetooth.Win32
#endif

import Control.Applicative
import Control.Exception
import qualified Control.Monad as M
import Data.IORef
import Data.Word
import Foreign
import Foreign.C
#if defined(mingw32_HOST_OS)
import System.Win32.Types
#endif

#if !defined(mingw32_HOST_OS)
foreign import ccall safe "hci_for_each_dev" hci_for_each_dev
    :: CInt -> FunPtr (CInt -> CInt -> CLong -> IO CInt) -> CLong -> IO ()

foreign import ccall safe "wrapper" mkVisitDev
    :: (CInt -> CInt -> CLong -> IO CInt) ->
       IO (FunPtr (CInt -> CInt -> CLong -> IO CInt))

foreign import ccall safe "hci_open_dev" hci_open_dev
    :: CInt -> IO CInt
    
foreign import ccall unsafe "sdp_register_service" sdp_register_service
    :: CUChar -> CUChar -> IO SDPSession

foreign import ccall safe "sdp_close" sdp_close
    :: SDPSession -> IO CInt
#endif

#if !defined(mingw32_HOST_OS)
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

allAdapters :: IO [Adapter]
#if defined(mingw32_HOST_OS)
allAdapters = return [Adapter]
#else
allAdapters = do
    devsRef <- newIORef []
    cb <- mkVisitDev $ \_ dev_id _ -> do
        modifyIORef devsRef (dev_id:)
        pure 0
    hci_for_each_dev (#const HCI_UP) cb 0
      `finally`
        freeHaskellFunPtr cb
    dev_ids <- reverse <$> readIORef devsRef
    mapM openDev dev_ids
#endif

#if !defined(mingw32_HOST_OS)
foreign import ccall unsafe "hci_get_route" hci_get_route
    :: Ptr BluetoothAddr -> IO CInt
#endif

defaultAdapter :: IO (Maybe Adapter)
#if defined(mingw32_HOST_OS)
defaultAdapter = return $ Just Adapter
#else
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

#if !defined(mingw32_HOST_OS)
registerSDPService :: Word8 -> Word8 -> IO SDPSession
registerSDPService ind port = sdp_register_service (CUChar safeInd) (CUChar port)
    where safeInd | 0 <= ind && ind < mAX_CONNECTIONS = ind
                  | otherwise = 0

closeSDPSession :: SDPSession -> IO ()
closeSDPSession = M.void . sdp_close
#endif