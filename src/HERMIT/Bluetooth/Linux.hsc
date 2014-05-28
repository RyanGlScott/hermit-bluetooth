{-# LANGUAGE CPP, DeriveDataTypeable, ForeignFunctionInterface #-}
module HERMIT.Bluetooth.Linux where

import Control.Applicative
import Control.Exception

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import Data.Char
import Data.List
import Data.Typeable
import Data.Word

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Numeric

#include <sys/socket.h>
#include <bluetooth/bluetooth.h>
#include <bluetooth/hci.h>
#include <bluetooth/sdp.h>
#include <bluetooth/sdp_lib.h>
#include "bluez_utils.h"

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

foreign import ccall safe "hci_for_each_dev"
    hci_for_each_dev :: CInt -> FunPtr (CInt -> CInt -> CLong -> IO CInt) -> CLong -> IO ()
foreign import ccall safe "wrapper"
    mkVisitDev :: (CInt -> CInt -> CLong -> IO CInt) -> IO (FunPtr (CInt -> CInt -> CLong -> IO CInt))
foreign import ccall safe "hci_open_dev"
    hci_open_dev :: CInt -> IO CInt   
foreign import ccall unsafe "sdp_register_service"
    sdp_register_service :: CUChar -> CUChar -> IO (Ptr SDPSession)
foreign import ccall unsafe "sdp_close"
    sdp_close :: Ptr SDPSession -> IO CInt
foreign import ccall unsafe "socket_rfcomm"
    socket_rfcomm :: IO CInt
foreign import ccall unsafe "bind_rfcomm"
    bind_rfcomm :: CInt -> CUChar -> IO CInt
foreign import ccall unsafe "listen_rfcomm"
    listen_rfcomm :: CInt -> CInt -> IO CInt
foreign import ccall unsafe "accept_rfcomm"
    accept_rfcomm :: CInt -> IO CInt
foreign import ccall unsafe "hci_get_route"
    hci_get_route :: Ptr BdAddr -> IO CInt
foreign import ccall unsafe "strerror"
    strerror :: CInt -> CString

newtype BdAddr = BdAddr ByteString deriving (Eq, Ord)

instance Show BdAddr where
    show (BdAddr bs) = intercalate ":" $ map (\x -> dig2 $ showHex x "") $ reverse $ B.unpack bs
      where
        dig2 = map toUpper . reverse . take 2 . reverse . ('0':)

instance Read BdAddr where
    readsPrec _ t = go t (6 :: Int) []
      where
        go tt n acc = case readHex tt of
            (x, t'):_ -> case (n, t') of
                (1, _) -> [(BdAddr (B.pack (x:acc)), t')]
                (_, ':':t'') -> go t'' (n-1) (x:acc)
                _ -> []
            _ -> []

instance Storable BdAddr where
    sizeOf _ = #{size bdaddr_t}
    alignment _ = #{alignment bdaddr_t}
    peek p = BdAddr . B.pack <$> peekArray 6 (castPtr p)
    poke p (BdAddr bs) = do
        _ <- BI.memset (castPtr p) 0 (fromIntegral $ sizeOf (undefined :: BdAddr))
        pokeArray (castPtr p) (B.unpack bs)

data BluetoothException = BluetoothException String String deriving (Show, Typeable)
instance Exception BluetoothException

type CUInt16 = #{type uint16_t}

data SDPSession = SDPSession CInt CInt CInt CInt CUInt16 (Ptr ())

instance Storable SDPSession where
    sizeOf _ = #{size sdp_session_t}
    alignment _ = #{alignment sdp_session_t}
    peek p = SDPSession <$> #{peek sdp_session_t, sock} p
                        <*> #{peek sdp_session_t, state} p
                        <*> #{peek sdp_session_t, local} p
                        <*> #{peek sdp_session_t, flags} p
                        <*> #{peek sdp_session_t, tid} p
                        <*> #{peek sdp_session_t, priv} p
    poke p (SDPSession sock' state' local' flags' tid' priv') = do
        #{poke sdp_session_t, sock} p sock'
        #{poke sdp_session_t, state} p state'
        #{poke sdp_session_t, local} p local'
        #{poke sdp_session_t, flags} p flags'
        #{poke sdp_session_t, tid} p tid'
        #{poke sdp_session_t, priv} p priv'