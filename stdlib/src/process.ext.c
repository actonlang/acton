
#include <uv.h>
#include "../rts/io.h"
#include "../rts/log.h"

void process$$__ext_init__() {
    // NOP
}

#define MAX_CMD_ARGS 32768

struct process_data {
    $function on_stdout;
    $function on_stderr;
    $function on_exit;
    uv_pipe_t stdin_pipe;
    uv_pipe_t stdout_pipe;
    uv_pipe_t stderr_pipe;
};


void exit_handler(uv_process_t *req, int64_t exit_status, int term_signal) {
    struct process_data *process_data = req->data;
    uv_close((uv_handle_t *)&process_data->stdin_pipe, NULL);
    uv_close((uv_handle_t *)req, NULL);
    process_data->on_exit->$class->__call__(process_data->on_exit, to$int(exit_status), to$int(term_signal));
}

void alloc_buffer(uv_handle_t *handle, size_t size, uv_buf_t *buf) {
    *buf = uv_buf_init((char*) malloc(size), size);
}

void read_stderrout(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buf) {
    if (nread < 0){
        if (nread == UV_EOF) {
            uv_close((uv_handle_t *)stream, NULL);
        }
    } else if (nread > 0) {
        if (stream->data) {
            $function cb = stream->data;
            cb->$class->__call__(cb, to$bytes(buf->base));
        }
    }

    if (buf->base)
        free(buf->base);
}

$R process$$Process$_create_process (process$$Process __self__, $Cont c$cont) {
    pin_actor_affinity(($Actor)__self__);
    struct process_data *process_data = calloc(1, sizeof(struct process_data));
    process_data->on_stdout = __self__->on_stdout;
    process_data->on_stderr = __self__->on_stderr;
    process_data->on_exit = __self__->on_exit;

    uv_process_options_t *options = calloc(1, sizeof(uv_process_options_t));

    uv_process_t *req = calloc(1, sizeof(uv_process_t));
    // NOTE: storing a uv_process_t pointer as a hex formatted Acton str
    // This sucks but how else?
    char pihas[] = "0x7f563c001700"; // pointer in hex as string
    snprintf(pihas, sizeof(pihas), "%p", req);
    __self__->_p = to$str(pihas);

    req->data = process_data;

    char *args[MAX_CMD_ARGS];

    if ($list_len(__self__->cmd) > MAX_CMD_ARGS) {
        $RAISE((($BaseException)$RuntimeError$new(to$str("Unable to span process; cmd too large (>MAX_CMD_ARGS)"))));
    }
    int i;
    for (i = 0; i < $list_len(__self__->cmd); i++) {
        args[i] = from$str($list_getitem(__self__->cmd, i));
    }
    args[++i] = NULL;

    uv_pipe_init(get_uv_loop(), &process_data->stdin_pipe, 0);
    uv_pipe_init(get_uv_loop(), &process_data->stdout_pipe, 0);
    process_data->stdout_pipe.data = __self__->on_stdout;
    uv_pipe_init(get_uv_loop(), &process_data->stderr_pipe, 0);
    process_data->stderr_pipe.data = __self__->on_stderr;

    uv_stdio_container_t process_stdio[3];
    options->stdio = process_stdio;

    process_stdio[0].flags = UV_CREATE_PIPE | UV_READABLE_PIPE;
    process_stdio[0].data.stream = (uv_stream_t*)&(process_data->stdin_pipe);

    process_stdio[1].flags = UV_CREATE_PIPE | UV_WRITABLE_PIPE;
    process_stdio[1].data.stream = (uv_stream_t*)&(process_data->stdout_pipe);

    process_stdio[2].flags = UV_CREATE_PIPE | UV_WRITABLE_PIPE;
    process_stdio[2].data.stream = (uv_stream_t*)&(process_data->stderr_pipe);

    options->stdio_count = 3;

    options->exit_cb = exit_handler;
    options->file = args[0];
    options->args = args;

    int r;
    if ((r = uv_spawn(get_uv_loop(), req, options))) {
        log_debug("Failed to spawn process: %s", uv_strerror(r));
        $RAISE((($BaseException)$RuntimeError$new(to$str("Unable to spawn process"))));
    }
    // TODO: do we need to do some magic to read any data produced before this
    // callback is installed?
    uv_read_start((uv_stream_t*)&process_data->stdout_pipe, alloc_buffer, read_stderrout);
    uv_read_start((uv_stream_t*)&process_data->stderr_pipe, alloc_buffer, read_stderrout);

    return $R_CONT(c$cont, $None);
}

$R process$$Process$signal$local (process$$Process __self__, $int signal, $Cont c$cont) {
    uv_process_t *p = (uv_process_t *)strtol(from$str(__self__->_p), NULL, 16);
    uv_process_kill(p, from$int(signal));
    return $R_CONT(c$cont, $None);
}

$R process$$Process$pid$local (process$$Process __self__, $Cont c$cont) {
    uv_process_t *p = (uv_process_t *)strtol(from$str(__self__->_p), NULL, 16);
    return $R_CONT(c$cont, ($atom)to$int(p->pid));
}
