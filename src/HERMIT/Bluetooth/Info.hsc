{-# LANGUAGE ForeignFunctionInterface #-}
module HERMIT.Bluetooth.Info where

#if !defined(mingw32_HOST_OS)
#include <bluetooth/bluetooth.h>
#endif

import Network.Socket

mAX_CONNECTIONS :: Num a => a
mAX_CONNECTIONS = 7

#if !defined(mingw32_HOST_OS)
bTPROTO_RFCOMM :: ProtocolNumber
bTPROTO_RFCOMM = (#const BTPROTO_RFCOMM)
#endif