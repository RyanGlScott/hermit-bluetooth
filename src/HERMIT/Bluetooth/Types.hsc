{-# LANGUAGE CPP #-}
module HERMIT.Bluetooth.Types where

-- import Control.Applicative
-- import Control.Exception
-- import Data.ByteString (ByteString)
-- import Data.List
-- import Data.Typeable
-- import Foreign
import Foreign.C
-- import Numeric
import Network.Socket

#if defined(mingw32_HOST_OS)
#include <windows.h>
#elif defined(linux_HOST_OS)
#include <sys/socket.h>
#endif

#if defined(mingw32_HOST_OS)
data Adapter = Adapter deriving (Eq, Ord, Show)
#else
data Adapter = Adapter CInt CInt deriving (Eq, Ord, Show)
#endif

#if defined(mingw32_HOST_OS)
#else
packSocketTypeOrThrow :: String -> SocketType -> IO CInt
packSocketTypeOrThrow caller stype = maybe err return (packSocketType' stype)
    where err = ioError . userError . concat $ ["Network.Socket.", caller, ": ", "socket type ", show stype, " unsupported on this system"]

packSocketType' :: SocketType -> Maybe CInt
packSocketType' stype = case Just stype of
#ifdef SOCK_STREAM
    Just Stream -> Just #{const SOCK_STREAM}
#endif
    _ -> Nothing
#endif