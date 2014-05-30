{-# LANGUAGE CPP, ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}
module HERMIT.Bluetooth.Win32 where

#include <windows.h>
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

import Control.Applicative

import Data.Word

import Foreign.C
import Foreign.Ptr
import Foreign.Storable

import System.Win32.Types

foreign import ccall unsafe "wsa_startup"
    wsa_startup :: IO CInt
foreign import ccall unsafe "socket_rfcomm"
    socket_rfcomm :: IO SOCKET
foreign import ccall unsafe "get_sock_opt"
    get_sock_opt :: SOCKET -> IO CInt
foreign import ccall unsafe "bind_rfcomm"
    bind_rfcomm :: SOCKET -> IO CInt
foreign import ccall unsafe "listen_rfcomm"
    listen_rfcomm :: SOCKET -> CInt -> IO CInt
foreign import ccall unsafe "wsa_register_service"
    wsa_register_service :: SOCKET -> CInt -> IO LPWSAQUERYSET
foreign import ccall unsafe "accept_rfcomm"
    accept_rfcomm :: SOCKET -> IO SOCKET
foreign import ccall unsafe "close_rfcomm"
    close_rfcomm :: SOCKET -> IO CInt
foreign import ccall unsafe "wsa_delete_service"
    wsa_delete_service :: LPWSAQUERYSET -> IO CInt
foreign import ccall unsafe "wsa_cleanup"
    wsa_cleanup :: IO CInt

type SOCKET = #{type SOCKET}

iNVALID_SOCKET :: SOCKET
iNVALID_SOCKET = #{const INVALID_SOCKET}

data SOCKADDR = SOCKADDR USHORT CString deriving Show

type LPSOCKADDR = Ptr SOCKADDR

instance Storable SOCKADDR where
    sizeOf _ = #{size SOCKADDR}
    alignment _ = #{alignment SOCKADDR}
    poke p (SOCKADDR saFamily saData) = do
        #{poke SOCKADDR, sa_family} p saFamily
        #{poke SOCKADDR, sa_data} p saData
    peek p = SOCKADDR <$> #{peek SOCKADDR, sa_family} p
                      <*> #{peek SOCKADDR, sa_data} p

data SOCKET_ADDRESS = SOCKET_ADDRESS LPSOCKADDR INT deriving Show

instance Storable SOCKET_ADDRESS where
    sizeOf _ = #{size SOCKET_ADDRESS}
    alignment _ = #{alignment SOCKET_ADDRESS}
    poke p (SOCKET_ADDRESS sa sal) = do
        #{poke SOCKET_ADDRESS, lpSockaddr} p sa
        #{poke SOCKET_ADDRESS, iSockaddrLength} p sal
    peek p = SOCKET_ADDRESS <$> #{peek SOCKET_ADDRESS, lpSockaddr} p
                            <*> #{peek SOCKET_ADDRESS, iSockaddrLength} p

data CSADDR_INFO = CSADDR_INFO SOCKET_ADDRESS SOCKET_ADDRESS INT INT deriving Show

type LPCSADDR_INFO = Ptr CSADDR_INFO

instance Storable CSADDR_INFO where
    sizeOf _ = #{size CSADDR_INFO}
    alignment _ = #{alignment CSADDR_INFO}
    poke p (CSADDR_INFO la ra st prot) = do
        #{poke CSADDR_INFO, LocalAddr} p la
        #{poke CSADDR_INFO, RemoteAddr} p ra
        #{poke CSADDR_INFO, iSocketType} p st
        #{poke CSADDR_INFO, iProtocol} p prot
    peek p = CSADDR_INFO <$> #{peek CSADDR_INFO, LocalAddr} p
                         <*> #{peek CSADDR_INFO, RemoteAddr} p
                         <*> #{peek CSADDR_INFO, iSocketType} p
                         <*> #{peek CSADDR_INFO, iProtocol} p

data GUID = GUID CULong CUShort CUShort (Ptr CUChar) deriving Show

type LPGUID = Ptr GUID

instance Storable GUID where
    sizeOf _ = #{size GUID}
    alignment _ = #{alignment GUID}
    poke p (GUID d1 d2 d3 d4) = do
        #{poke GUID, Data1} p d1
        #{poke GUID, Data2} p d2
        #{poke GUID, Data3} p d3
        #{poke GUID, Data4} p d4
    peek p = GUID <$> #{peek GUID, Data1} p
                  <*> #{peek GUID, Data2} p
                  <*> #{peek GUID, Data3} p
                  <*> #{peek GUID, Data4} p

newtype WSAECOMPARATOR = WSAECOMPARATOR CInt deriving (Show, Storable)
#{enum WSAECOMPARATOR, WSAECOMPARATOR
    , compEqual = COMP_EQUAL
    , compNotless = COMP_NOTLESS
}

data WSAVERSION = WSAVERSION DWORD WSAECOMPARATOR deriving Show

type LPWSAVERSION = Ptr WSAVERSION

instance Storable WSAVERSION where
    sizeOf _ = #{size WSAVERSION}
    alignment _ = #{alignment WSAVERSION}
    poke p (WSAVERSION ver how) = do
        #{poke WSAVERSION, dwVersion} p ver
        #{poke WSAVERSION, ecHow} p how
    peek p = WSAVERSION <$> #{peek WSAVERSION, dwVersion} p
                        <*> #{peek WSAVERSION, ecHow} p

data AFPROTOCOLS = AFPROTOCOLS INT INT deriving Show

type LPAFPROTOCOLS = Ptr AFPROTOCOLS

instance Storable AFPROTOCOLS where
    sizeOf _ = #{size AFPROTOCOLS}
    alignment _ = #{alignment AFPROTOCOLS}
    poke p (AFPROTOCOLS addrFam prot) = do
        #{poke AFPROTOCOLS, iAddressFamily} p addrFam
        #{poke AFPROTOCOLS, iProtocol} p prot
    peek p = AFPROTOCOLS <$> #{peek AFPROTOCOLS, iAddressFamily} p
                         <*> #{peek AFPROTOCOLS, iProtocol} p

data BLOB = BLOB CULong BYTE deriving Show

type LPBLOB = Ptr BLOB

instance Storable BLOB where
    sizeOf _ = #{size BLOB}
    alignment _ = #{alignment BLOB}
    poke p (BLOB bSize bData) = do
        #{poke BLOB, cbSize} p bSize
        #{poke BLOB, pBlobData} p bData
    peek p = BLOB <$> #{peek BLOB, cbSize} p
                  <*> #{peek BLOB, pBlobData} p

type LPWSAQUERYSET = Ptr WSAQUERYSET

data WSAQUERYSET = WSAQUERYSET DWORD LPWSTR LPGUID LPWSAVERSION LPWSTR DWORD LPGUID DWORD DWORD LPAFPROTOCOLS LPWSTR DWORD LPCSADDR_INFO DWORD LPBLOB deriving Show

instance Storable WSAQUERYSET where
    sizeOf _ = #{size WSAQUERYSET}
    alignment _ = #{alignment WSAQUERYSET}
    poke p (WSAQUERYSET size serviceName serviceId version comment nameSpace providerId context numProts prots queryStr numAddrs buffer outFlags blob) = do
        #{poke WSAQUERYSET, dwSize} p size
        #{poke WSAQUERYSET, lpszServiceInstanceName} p serviceName
        #{poke WSAQUERYSET, lpServiceClassId} p serviceId
        #{poke WSAQUERYSET, lpVersion} p version
        #{poke WSAQUERYSET, lpszComment} p comment
        #{poke WSAQUERYSET, dwNameSpace} p nameSpace
        #{poke WSAQUERYSET, lpNSProviderId} p providerId
        #{poke WSAQUERYSET, lpszContext} p context
        #{poke WSAQUERYSET, dwNumberOfProtocols} p numProts
        #{poke WSAQUERYSET, lpafpProtocols} p prots
        #{poke WSAQUERYSET, lpszQueryString} p queryStr
        #{poke WSAQUERYSET, dwNumberOfCsAddrs} p numAddrs
        #{poke WSAQUERYSET, lpcsaBuffer} p buffer
        #{poke WSAQUERYSET, dwOutputFlags} p outFlags
        #{poke WSAQUERYSET, lpBlob} p blob
    peek p = WSAQUERYSET <$> #{peek WSAQUERYSET, dwSize} p
                         <*> #{peek WSAQUERYSET, lpszServiceInstanceName} p
                         <*> #{peek WSAQUERYSET, lpServiceClassId} p
                         <*> #{peek WSAQUERYSET, lpVersion} p
                         <*> #{peek WSAQUERYSET, lpszComment} p
                         <*> #{peek WSAQUERYSET, dwNameSpace} p
                         <*> #{peek WSAQUERYSET, lpNSProviderId} p
                         <*> #{peek WSAQUERYSET, lpszContext} p
                         <*> #{peek WSAQUERYSET, dwNumberOfProtocols} p
                         <*> #{peek WSAQUERYSET, lpafpProtocols} p
                         <*> #{peek WSAQUERYSET, lpszQueryString} p
                         <*> #{peek WSAQUERYSET, dwNumberOfCsAddrs} p
                         <*> #{peek WSAQUERYSET, lpcsaBuffer} p
                         <*> #{peek WSAQUERYSET, dwOutputFlags} p
                         <*> #{peek WSAQUERYSET, lpBlob} p