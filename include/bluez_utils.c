#include <bluetooth/bluetooth.h>
#include <bluetooth/rfcomm.h>
#include <bluetooth/sdp.h>
#include <bluetooth/sdp_lib.h>
#include <sys/socket.h>
#include "bluez_utils.h"

#define NUM_UUID_WORDS 4
#define NUM_UUIDS 7

/*
 * A 128-bit number used to identify this service. The words are ordered from most to least
 * significant, but within each word, the octets are ordered from least to most significant.
 * For example, the UUID represneted by this array is 00001101-0000-1000-8000-00805F9B34FB. (The
 * hyphenation is a convention specified by the Service Discovery Protocol of the Bluetooth Core
 * Specification, but is not particularly important for this program.)
 *
 * This UUID is the Bluetooth Base UUID and is commonly used for simple Bluetooth applications.
 * Regardless of the UUID used, it must match the one that the Armatus Android app is searching
 * for.
 */
// #define UUID1 { 0x01110000, 0x00100000, 0x80000080, 0xFB349B5F }
// //const uint32_t UUID1[NUM_UUID_WORDS] = { 0x406A74B7, 0x684858C7, 0xC67A19AA, 0xFC5D47B3 };
// #define UUID2 { 0x9D18642D, 0x11452C5A, 0xF17774A0, 0x3408FD99 }
// #define UUID3 { 0x9AE042E4, 0x7B4AF351, 0x38F6CB91, 0x12141D49 }
// #define UUID4 { 0x04651DA8, 0xEE493645, 0x967D75A4, 0xE43994D0 }
// #define UUID5 { 0xB1EA91AA, 0x8E44ADD8, 0xEB95DBAB, 0x559B4ABA }
// #define UUID6 { 0x73DA344D, 0x404FA4D0, 0x7E9138AC, 0x97EE9D0A }
// #define UUID7 { 0xDFD4145E, 0xB74D8A9C, 0x37C9E481, 0xE0864C56 }

const uint32_t UUID_LIST[NUM_UUIDS][NUM_UUID_WORDS] = {
    { 0x01110000, 0x00100000, 0x80000080, 0xFB349B5F },
    { 0x9D18642D, 0x11452C5A, 0xF17774A0, 0x3408FD99 },
    { 0x9AE042E4, 0x7B4AF351, 0x38F6CB91, 0x12141D49 },
    { 0x04651DA8, 0xEE493645, 0x967D75A4, 0xE43994D0 },
    { 0xB1EA91AA, 0x8E44ADD8, 0xEB95DBAB, 0x559B4ABA },
    { 0x73DA344D, 0x404FA4D0, 0x7E9138AC, 0x97EE9D0A },
    { 0xDFD4145E, 0xB74D8A9C, 0x37C9E481, 0xE0864C56 }
};

// const uint32_t UUID_LIST[NUM_UUIDS][NUM_UUID_WORDS] = {
//     UUID1, UUID2, UUID3, UUID4, UUID5, UUID6, UUID7
// };

/*
 * Allows this service to be discovered when running sdptool. For example, you can find this service
 * after starting it by running
 *
 * $ sdptool browse local
 *
 * (Adapted from http://www.btessentials.com/examples/bluez/sdp-register.c)
 *
 */
sdp_session_t *sdp_register_service(const uint8_t uuid_index, const uint8_t rfcomm_channel) {
    const uint32_t *svc_uuid_int = UUID_LIST[uuid_index];
    const char *service_name = "Armatus Bluetooth server";
    const char *svc_dsc = "A HERMIT server that interfaces with the Armatus Android app";
    const char *service_prov = "Armatus";

    uuid_t root_uuid, l2cap_uuid, rfcomm_uuid, svc_uuid,
           svc_class_uuid;
    sdp_list_t *l2cap_list = 0,
                *rfcomm_list = 0,
                 *root_list = 0,
                  *proto_list = 0,
                   *access_proto_list = 0,
                    *svc_class_list = 0,
                     *profile_list = 0;
    sdp_data_t *channel = 0;
    sdp_profile_desc_t profile;
    sdp_record_t record = { 0 };
    sdp_session_t *session = 0;

    // set the general service ID
    sdp_uuid128_create(&svc_uuid, svc_uuid_int);
    sdp_set_service_id(&record, svc_uuid);

    char str[256] = "";
    sdp_uuid2strn(&svc_uuid, str, 256);
    printf("Registering UUID %s\n", str);

    // set the service class
    sdp_uuid16_create(&svc_class_uuid, SERIAL_PORT_SVCLASS_ID);
    svc_class_list = sdp_list_append(0, &svc_class_uuid);
    sdp_set_service_classes(&record, svc_class_list);

    // set the Bluetooth profile information
    sdp_uuid16_create(&profile.uuid, SERIAL_PORT_PROFILE_ID);
    profile.version = 0x0100;
    profile_list = sdp_list_append(0, &profile);
    sdp_set_profile_descs(&record, profile_list);

    // make the service record publicly browsable
    sdp_uuid16_create(&root_uuid, PUBLIC_BROWSE_GROUP);
    root_list = sdp_list_append(0, &root_uuid);
    sdp_set_browse_groups(&record, root_list);

    // set l2cap information
    sdp_uuid16_create(&l2cap_uuid, L2CAP_UUID);
    l2cap_list = sdp_list_append(0, &l2cap_uuid);
    proto_list = sdp_list_append(0, l2cap_list);

    // register the RFCOMM channel for RFCOMM sockets
    sdp_uuid16_create(&rfcomm_uuid, RFCOMM_UUID);
    channel = sdp_data_alloc(SDP_UINT8, &rfcomm_channel);
    rfcomm_list = sdp_list_append(0, &rfcomm_uuid);
    sdp_list_append(rfcomm_list, channel);
    sdp_list_append(proto_list, rfcomm_list);

    access_proto_list = sdp_list_append(0, proto_list);
    sdp_set_access_protos(&record, access_proto_list);

    // set the name, provider, and description
    sdp_set_info_attr(&record, service_name, service_prov, svc_dsc);

    // connect to the local SDP server, register the service record,
    // and disconnect
    session = sdp_connect(BDADDR_ANY, BDADDR_LOCAL, SDP_RETRY_IF_BUSY);

    if (session != NULL) {
        sdp_record_register(session, &record, 0);

        // cleanup
        sdp_data_free(channel);
        sdp_list_free(l2cap_list, 0);
        sdp_list_free(rfcomm_list, 0);
        sdp_list_free(root_list, 0);
        sdp_list_free(access_proto_list, 0);
        sdp_list_free(svc_class_list, 0);
        sdp_list_free(profile_list, 0);
        return session;
    }
    
    return session;
}

int socket_rfcomm() {
    return socket(AF_BLUETOOTH, SOCK_STREAM, BTPROTO_RFCOMM);
}

int bind_rfcomm(int fd, uint8_t port) {
    struct sockaddr_rc loc_addr = { 0 };
    loc_addr.rc_family = AF_BLUETOOTH;
    loc_addr.rc_bdaddr = *BDADDR_ANY;
    loc_addr.rc_channel = port;

    return bind(fd, (struct sockaddr *)&loc_addr, sizeof(loc_addr));
}

int listen_rfcomm(int fd, int n) {
    return listen(fd, n);
}

int accept_rfcomm(int fd) {
    struct sockaddr_rc rem_addr = { 0 };
    socklen_t opt = sizeof(rem_addr);

    return accept(fd, (struct sockaddr *)&rem_addr, &opt);
}