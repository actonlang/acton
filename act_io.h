#pragma once

#include "act_platform.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>

#include <netinet/in.h>

#include <sys/socket.h>
#include <sys/types.h>

typedef struct act_io_env_t act_io_env_t;

typedef int act_io_channel_id_t; // > 0
typedef int act_io_channel_error_t; // < 0

typedef enum {
    ACT_IO_ENV_ERROR_NONE = 0,
} act_io_env_error_t;

typedef enum {
    ACT_IO_CHANNEL_TYPE_IPV4_TCP_LISTEN,
    ACT_IO_CHANNEL_TYPE_IPV4_TCP_CONNECT,
    ACT_IO_CHANNEL_TYPE_IPV6_TCP_LISTEN,
    ACT_IO_CHANNEL_TYPE_IPV6_TCP_CONNECT,
    ACT_IO_CHANNEL_TYPE_USER_EVENT,
}   act_io_channel_type_t;

typedef struct {
    act_io_channel_type_t type;
    union {
        struct sockaddr_in ipv4;
        struct sockaddr_in6 ipv6;
    };
} act_io_channel_description_t;

typedef enum {
    ACT_IO_EVENT_TYPE_READ,
    ACT_IO_EVENT_TYPE_WRITABLE,
    ACT_IO_EVENT_TYPE_CLOSED,
    ACT_IO_EVENT_TYPE_ACCEPT,
} act_io_event_type_t;

typedef union {
    void (*on_read)(void* arg, uint8_t* data, size_t len);
    void (*on_writable)(void* arg, size_t len_hint);
    void (*on_closed)(void* arg, act_io_channel_id_t channel_id, act_io_channel_error_t cause);
    void (*on_accept)(void* arg, act_io_channel_id_t channel_id);
} act_io_event_callback_t;

typedef struct {
    act_io_event_type_t event;
    act_io_event_callback_t callback;
    void* arg;
    bool paused;
} act_io_event_handler_description_t;

//

act_io_env_t* act_io_env_create();

act_io_env_error_t act_io_env_step(
    act_io_env_t* env);

act_io_channel_id_t act_io_open(
    act_io_env_t* env,
    act_io_channel_description_t* channel,
    act_io_event_handler_description_t* handlerlist,
    size_t nhandlers);

act_io_channel_error_t act_io_subscribe(
    act_io_env_t* env,
    act_io_event_handler_description_t* handlerlist,
    size_t nhandlers);

act_io_channel_error_t acton_io_send(
    act_io_env_t* env,
    act_io_channel_id_t* channel_id,
    uint8_t* data,
    size_t len); // TOOD: Completion confirmation callback?

act_io_channel_error_t acton_io_pause(
    act_io_env_t* env,
    act_io_channel_id_t* channel_id,
    act_io_event_type_t eventlist,
    size_t nevents);

act_io_channel_error_t acton_io_resume(
    act_io_env_t* env,
    act_io_channel_id_t* channel_id,
    act_io_event_type_t eventlist,
    size_t nevents);

// acton_io_get_event_handlers // TODO: return events and their pause-state

act_io_channel_error_t acton_io_unsubscribe(
    act_io_env_t* env,
    act_io_channel_id_t* channel_id,
    act_io_event_type_t eventlist,
    size_t nevents);

act_io_channel_error_t act_io_close(
    act_io_env_t* env,
    act_io_channel_id_t* channel_id);

void act_io_env_release(
    act_io_env_t* env);
