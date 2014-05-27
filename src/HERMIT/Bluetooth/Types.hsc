{-# LANGUAGE CPP, EmptyDataDecls #-}
module HERMIT.Bluetooth.Types where

import Foreign.C

import Network.Socket

#if defined(mingw32_HOST_OS)
import HERMIT.Bluetooth.Win32
#elif defined(linux_HOST_OS)
import HERMIT.Bluetooth.Linux
#endif

#if defined(mingw32_HOST_OS)
#include <windows.h>
#elif defined(linux_HOST_OS)
#include <sys/socket.h>
#endif

data Adapter
#if defined(mingw32_HOST_OS)
    = Adapter
#elif defined(linux_HOST_OS)
    = Adapter CInt CInt
#endif
     deriving (Eq, Ord, Show)

newtype BluetoothService
#if defined(mingw32_HOST_OS)
    = BluetoothService { runService :: LPWSAQUERYSET }
#elif defined(linux_HOST_OS)
    = BluetoothService { runService :: Ptr SDPSession }
#endif
    
packSocketTypeOrThrow :: String -> SocketType -> IO CInt
packSocketTypeOrThrow caller stype = maybe err return (packSocketType' stype)
    where err = ioError . userError . concat $ ["Network.Socket.", caller, ": ", "socket type ", show stype, " unsupported on this system"]

packSocketType' :: SocketType -> Maybe CInt
packSocketType' stype = case Just stype of
#ifdef SOCK_STREAM
    Just Stream -> Just #{const SOCK_STREAM}
#endif
    _ -> Nothing