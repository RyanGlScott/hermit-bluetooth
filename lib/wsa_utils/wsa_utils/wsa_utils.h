#ifndef WSA_UTILS_H
#define WSA_UTILS_H

#include <WinSock2.h>
#include <ws2bth.h>
#include <bthsdpdef.h>
#include <BluetoothAPIs.h>
#include <stdio.h>
#include <stdlib.h>
#include <tchar.h>

#ifndef INITGUID
#include <initguid.h>
#endif

#ifdef WSA_UTILS_DLL_EXPORTS
#define WSA_UTILS_DLL_API __declspec(dllexport) 
#else
#define WSA_UTILS_DLL_API __declspec(dllimport) 
#endif

WSA_UTILS_DLL_API int wsa_startup();
WSA_UTILS_DLL_API SOCKET socket_rfcomm();
WSA_UTILS_DLL_API int get_sock_opt(SOCKET s);
WSA_UTILS_DLL_API int bind_rfcomm(SOCKET s);
WSA_UTILS_DLL_API int listen_rfcomm(SOCKET s, int backlog);
WSA_UTILS_DLL_API LPWSAQUERYSET wsa_register_service(SOCKET s, const int uuidIndex);
WSA_UTILS_DLL_API SOCKET accept_rfcomm(SOCKET s);
WSA_UTILS_DLL_API int close_rfcomm(SOCKET s);
WSA_UTILS_DLL_API int wsa_delete_service(LPWSAQUERYSET service);
WSA_UTILS_DLL_API int wsa_cleanup();
BOOL ctrl_handler(DWORD fdwCtrlType);

#endif