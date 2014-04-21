#ifndef BLUEZ_UTILS_H
#define BLUEZ_UTILS_H

#include <bluetooth/sdp.h>
#include <bluetooth/sdp_lib.h>
#include <sys/socket.h>

sdp_session_t *sdp_register_service(const uint8_t uuid_index, const uint8_t rfcomm_channel);
int socket_rfcomm();
int bind_rfcomm(int fd, uint8_t port);
int listen_rfcomm(int fd, int n);
int accept_rfcomm(int fd);

#endif