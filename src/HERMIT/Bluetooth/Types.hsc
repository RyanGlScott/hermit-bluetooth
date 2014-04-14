{-# LANGUAGE DeriveDataTypeable #-}
module HERMIT.Bluetooth.Types where

#if defined(mingw32_HOST_OS)
#include <windows.h>
#else
#include <bluetooth/bluetooth.h>
#include <bluetooth/hci.h>
#include <bluetooth/sdp.h>
#include <bluetooth/sdp_lib.h>
#endif
#include <stddef.h>

import Control.Applicative
import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import Data.Char
import Data.List
import Data.Typeable
import Foreign
import Foreign.C
import Numeric

#if !defined(mingw32_HOST_OS)
foreign import ccall unsafe "strerror" strerror
    :: CInt -> CString
#endif

#if defined(mingw32_HOST_OS)
data Adapter = Adapter deriving (Eq, Ord, Show)
#else
data Adapter = Adapter CInt CInt deriving (Eq, Ord, Show)
#endif

data BluetoothException = BluetoothException String String deriving (Show, Typeable)
instance Exception BluetoothException

newtype BluetoothAddr = BluetoothAddr ByteString deriving (Eq, Ord)

instance Show BluetoothAddr where
    show (BluetoothAddr bs) = intercalate ":" $ map (\x -> dig2 $ showHex x "") $ reverse $ B.unpack bs
      where
        dig2 = map toUpper . reverse . take 2 . reverse . ('0':)

instance Read BluetoothAddr where
    readsPrec _ t = go t (6 :: Int) []
      where
        go tt n acc = case readHex tt of
            (x, t'):_ -> case (n, t') of
                (1, _) -> [(BluetoothAddr (B.pack (x:acc)), t')]
                (_, ':':t'') -> go t'' (n-1) (x:acc)
                _ -> []
            _ -> []

instance Storable BluetoothAddr where
#if defined(mingw32_HOST_OS)
    sizeOf _ = 8
#else
    sizeOf _ = (#const sizeof(bdaddr_t))
#endif
    alignment _ = alignment (undefined :: Word64)
    peek p = BluetoothAddr . B.pack <$> peekArray 6 (castPtr p)
    poke p (BluetoothAddr bs) = do
        _ <- BI.memset (castPtr p) 0 (fromIntegral $ sizeOf (undefined :: BluetoothAddr))
        pokeArray (castPtr p) (B.unpack bs)

#if defined(mingw32_HOST_OS)
#else
type SDPSession = Ptr SDPSessionT

data SDPSessionT = SDPSessionT
    CInt
    CInt
    CInt
    CInt
    CUInt -- unsigned short int
    (Ptr ()) -- void*

instance Storable SDPSessionT where
    sizeOf _ = (#const sizeof(sdp_session_t))
    alignment _ = alignment (undefined :: Word64)
    peek p = do
        sock' <- (#peek sdp_session_t, sock) p
        state' <- (#peek sdp_session_t, state) p
        local' <- (#peek sdp_session_t, local) p
        flags' <- (#peek sdp_session_t, flags) p
        tid' <- (#peek sdp_session_t, tid) p
        priv' <- (#peek sdp_session_t, priv) p
        return $ SDPSessionT sock' state' local' flags' tid' priv'
    poke p (SDPSessionT sock' state' local' flags' tid' priv') = do
        (#poke sdp_session_t, sock) p sock'
        (#poke sdp_session_t, state) p state'
        (#poke sdp_session_t, local) p local'
        (#poke sdp_session_t, flags) p flags'
        (#poke sdp_session_t, tid) p tid'
        (#poke sdp_session_t, priv) p priv'
#endif