#include "wsa_utils.h"

#define WSA_UTILS_DLL_EXPORTS

/**
* Adapted from http://www.codeproject.com/Articles/252882/Bluetooth-Server-Programming-on-Windows,
* under The Code Project Open License (CPOL).
*
* Copyright (c) 2011 Vijay Rajanna
*/

DEFINE_GUID(UUID1, 0x00001101, 0x0000, 0x1000, 0x80, 0x00, 0x00, 0x80, 0x5F, 0x9B, 0x34, 0xFB);
//DEFINE_GUID(UUID1, 0xB7746A40, 0xC758, 0x4868, 0xAA, 0x19, 0x7A, 0xC6, 0xB3, 047, 0x5D, 0xFC);
DEFINE_GUID(UUID2, 0x2D64189D, 0x5A2C, 0x4511, 0xA0, 0x74, 0x77, 0xF1, 0x99, 0xFD, 0x08, 0x34);
DEFINE_GUID(UUID3, 0xE442E09A, 0x51F3, 0x4A7B, 0x91, 0xCB, 0xF6, 0x38, 0x49, 0x1D, 0x14, 0x12);
DEFINE_GUID(UUID4, 0xA81D6504, 0x4536, 0x49EE, 0xA4, 0x75, 0x7D, 0x96, 0xD0, 0x94, 0x39, 0xE4);
DEFINE_GUID(UUID5, 0xAA91EAB1, 0xD8AD, 0x448E, 0xAB, 0xDB, 0x95, 0xEB, 0xBA, 0x4A, 0x9B, 0x55);
DEFINE_GUID(UUID6, 0x4D34DA73, 0xD0A4, 0x4F40, 0xAC, 0x38, 0x91, 0x7E, 0x0A, 0x9D, 0xEE, 0x97);
DEFINE_GUID(UUID7, 0x5E14D4DF, 0x9C8A, 0x4DB7, 0x81, 0xE4, 0xC9, 0x37, 0x56, 0x4C, 0x86, 0xE0);
const int NUM_UUIDS = 7;
const GUID UUID_LIST[NUM_UUIDS] = {
	UUID1, UUID2, UUID3, UUID4, UUID5, UUID6, UUID7
};

const bool DEBUG = false;
const SOCKADDR_BTH ADDRESS = { AF_BTH, 0, GUID_NULL, BT_PORT_ANY };

int wsa_startup() {
	if (DEBUG) {
		printf("wsa_startup\n");
	}

	WORD wVersionRequested = 0x202;
	WSADATA m_data;
	return WSAStartup(wVersionRequested, &m_data);
}

SOCKET socket_rfcomm() {
	if (DEBUG) {
		printf("socket_rfcomm\n");
	}

	return socket(AF_BTH, SOCK_STREAM, BTHPROTO_RFCOMM);
}

int get_sock_opt(SOCKET s) {
	if (DEBUG) {
		printf("get_sock_opt\n");
	}

	WSAPROTOCOL_INFO protocolInfo;
	int protocolInfoSize = sizeof(protocolInfo);
	return getsockopt(s, SOL_SOCKET, SO_PROTOCOL_INFO, (char *)&protocolInfo, &protocolInfoSize);
}

int bind_rfcomm(SOCKET s) {
	if (DEBUG) {
		printf("bind_rfcomm\n");
	}

	return bind(s, (const sockaddr *) &ADDRESS, sizeof(SOCKADDR_BTH));
}

int listen_rfcomm(SOCKET s, int backlog) {
	if (DEBUG) {
		printf("listen_rfcomm\n");
	}

	return listen(s, backlog);
}

LPWSAQUERYSET wsa_register_service(SOCKET s, const int uuidIndex) {
	if (DEBUG) {
		printf("wsa_register_service\n");
	}

	PSOCKADDR_BTH addr = new SOCKADDR_BTH;
	addr->addressFamily = ADDRESS.addressFamily;
	addr->btAddr = ADDRESS.btAddr;
	addr->serviceClassId = ADDRESS.serviceClassId;
	addr->port = ADDRESS.port;
	sockaddr *pAddr = (sockaddr *) addr;
	int size = sizeof(SOCKADDR_BTH);
	if (getsockname(s, pAddr, &size) < 0) {
		return NULL;
    };

	if (DEBUG) {
		printf("getsockname\n");
	}

	LPWSAQUERYSET service = new WSAQUERYSET;
	memset(service, 0, sizeof(*service));
	service->dwSize = sizeof(*service);
	service->lpszServiceInstanceName = _T("Armatus Bluetooth server");
	service->lpszComment = _T("A HERMIT server that interfaces with the Armatus Android app");

	if (DEBUG) {
		printf("LPWSAQUERYSET service\n");
	}

	LPGUID serviceID = new GUID(UUID_LIST[uuidIndex]);
	service->lpServiceClassId = serviceID;
	service->dwNumberOfCsAddrs = 1;
	service->dwNameSpace = NS_BTH;

	if (DEBUG) {
		printf("LPGUID serviceID\n");
	}

	LPCSADDR_INFO csAddr = new CSADDR_INFO;
	memset(csAddr, 0, sizeof(csAddr));
	csAddr->LocalAddr.iSockaddrLength = sizeof(SOCKADDR_BTH);
	csAddr->LocalAddr.lpSockaddr = pAddr;
	csAddr->iSocketType = SOCK_STREAM;
	csAddr->iProtocol = BTHPROTO_RFCOMM;
	service->lpcsaBuffer = csAddr;

	if (DEBUG) {
		printf("LPSCADDR_INFO csAddr\n");
	}

	if (WSASetService(service, RNRSERVICE_REGISTER, 0) < 0) {
		return NULL;
	}

	if (DEBUG) {
		printf("WSASetService\n");
	}

	return service;
}

SOCKET accept_rfcomm(SOCKET s) {
	if (DEBUG) {
		printf("accept_rfcomm\n");
	}

	SetConsoleCtrlHandler((PHANDLER_ROUTINE)ctrl_handler, TRUE);

	SOCKADDR_BTH sab2;
	int ilen = sizeof(sab2);
	const int res = accept(s, (sockaddr*)&sab2, &ilen);

	SetConsoleCtrlHandler((PHANDLER_ROUTINE)ctrl_handler, FALSE);

	return res;
}

int close_rfcomm(SOCKET s) {
	if (DEBUG) {
		printf("close_rfcomm\n");
	}

	return closesocket(s);
}

int wsa_delete_service(LPWSAQUERYSET service) {
	if (DEBUG) {
		printf("wsa_delete_service\n");
	}

	const int res = WSASetService(service, RNRSERVICE_DELETE, 0);
	delete service->lpServiceClassId;
	delete service->lpcsaBuffer->LocalAddr.lpSockaddr;
	delete service->lpcsaBuffer;
	delete service;
	return res;
}

int wsa_cleanup() {
	if (DEBUG) {
		printf("wsa_cleanup\n");
	}

	return WSACleanup();
}

BOOL ctrl_handler(DWORD fdwCtrlType) {
	switch (fdwCtrlType) {
	case CTRL_C_EVENT:
		if (DEBUG) {
			printf("Ctrl-C event\n\n");
		}
		exit(EXIT_FAILURE);

	case CTRL_BREAK_EVENT:
		if (DEBUG) {
			printf("Ctrl-Break event\n\n");
		}
		exit(EXIT_FAILURE);

	default:
		return FALSE;
	}
}