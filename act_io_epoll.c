#include "act_platform.h"
#ifdef USE_EPOLL
#include "act_io_epoll.h"
#include "act_trace.h"

act_io_env_t* act_io_env_create() {
    act_io_env_t* env = malloc(sizeof(act_io_env_t));

    int epoll_fd = epoll_create1(EPOLL_CLOEXEC);
    if (epoll_fd < 0) {
        PANIC_ERRNO("epoll_create1");
    }
    else {
        env->epoll_fd = epoll_fd;

        return env;
    }

    act_io_env_release(env);
    return NULL;
}

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

act_io_channel_error_t acton_io_unsubscribe(
    act_io_env_t* env,
    act_io_channel_id_t* channel_id,
    act_io_event_type_t eventlist,
    size_t nevents);

act_io_channel_error_t act_io_close(
    act_io_env_t* env,
    act_io_channel_id_t* channel_id);

void act_io_env_release(
    act_io_env_t* env) {
    
    if (env) {
        if (env->epoll_fd >= 0) {
            close(env->epoll_fd);
        }

        free(env);
    }    
}

// TODO: ...

#else
typedef int iso_c_forbids_an_empty_translation_unit;
#endif // USE_EPOLL