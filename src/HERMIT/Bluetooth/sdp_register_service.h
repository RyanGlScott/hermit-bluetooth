#ifndef SDP_REGISTER_SERVICE_H
#define SDP_REGISTER_SERVICE_H

#include <bluetooth/sdp.h>
#include <bluetooth/sdp_lib.h>

sdp_session_t *sdp_register_service(const uint8_t uuid_index, const uint8_t rfcomm_channel);

#endif