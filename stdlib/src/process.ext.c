
#include <uv.h>
#include "../rts/io.h"
#include "../rts/log.h"

void process$$__ext_init__() {
    // NOP
}

struct process_data {
    process$$Process process;
    $function on_stdout;
    $function on_stderr;
    $function on_exit;
    uv_pipe_t stdin_pipe;
    uv_pipe_t stdout_pipe;
    uv_pipe_t stderr_pipe;
};


void exit_handler(uv_process_t *req, int64_t exit_status, int term_signal) {
    struct process_data *process_data = req->data;
    uv_stream_t *stdin = (uv_stream_t *)&process_data->stdin_pipe;
    // stdin might be closed already from done_writing
    if (uv_is_closing(stdin) == 0)
        uv_close((uv_handle_t *)stdin, NULL);
    uv_close((uv_handle_t *)req, NULL);
    process_data->on_exit->$class->__call__(process_data->on_exit, process_data->process, to$int(exit_status), to$int(term_signal));
}

void alloc_buffer(uv_handle_t *handle, size_t size, uv_buf_t *buf) {
    *buf = uv_buf_init((char*) malloc(size), size);
}

void read_stderr(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buf) {
    if (nread < 0){
        if (nread == UV_EOF) {
            uv_close((uv_handle_t *)stream, NULL);
        }
    } else if (nread > 0) {
        if (stream->data) {
            struct process_data *process_data = (struct process_data *)stream->data;
            $function cb = process_data->on_stderr;
            cb->$class->__call__(cb, process_data->process, to$bytes(buf->base));
        }
    }

    if (buf->base)
        free(buf->base);
}

void read_stdout(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buf) {
    if (nread < 0){
        if (nread == UV_EOF) {
            uv_close((uv_handle_t *)stream, NULL);
        }
    } else if (nread > 0) {
        if (stream->data) {
            struct process_data *process_data = (struct process_data *)stream->data;
            $function cb = process_data->on_stdout;
            cb->$class->__call__(cb, process_data->process, to$bytes(buf->base));
        }
    }

    if (buf->base)
        free(buf->base);
}

$R process$$Process$_create_process (process$$Process __self__, $Cont c$cont) {
    pin_actor_affinity(($Actor)__self__);
    struct process_data *process_data = calloc(1, sizeof(struct process_data));
    process_data->process = __self__;
    process_data->on_stdout = __self__->on_stdout;
    process_data->on_stderr = __self__->on_stderr;
    process_data->on_exit = __self__->on_exit;

    uv_process_options_t *options = calloc(1, sizeof(uv_process_options_t));

    uv_process_t *req = calloc(1, sizeof(uv_process_t));
    __self__->_p = to$int(req);

    req->data = process_data;

    char **args = (char **)malloc(($list_len(__self__->cmd)+1) * sizeof(char *));

    int i;
    for (i = 0; i < $list_len(__self__->cmd); i++) {
        args[i] = from$str($list_getitem(__self__->cmd, i));
    }
    args[i] = NULL;

    if (__self__->workdir != $None) {
        options->cwd = from$str(__self__->workdir);
    };

    if (__self__->env == $None) {
        options->env = NULL;
    } else {
        char **env = (char **)calloc(($dict_len(__self__->env)+1), sizeof(char *));
        $Iterator$dict$items iter = $NEW($Iterator$dict$items, __self__->env);
        $tuple item;
        for (i=0; i < $dict_len(__self__->env); i++) {
            item = ($tuple)iter->$class->__next__(iter);
            char *key = from$str(($str)item->components[0]);
            char *value = from$str(($str)item->components[1]);
            size_t env_size = strlen(key) + strlen(value) + 2;
            char *env_var = malloc(env_size);
            snprintf(env_var, env_size, "%s=%s", key, value);
            env[i] = env_var;
        }
        env[i] = NULL;

        options->env = env;
    }

    uv_pipe_init(get_uv_loop(), &process_data->stdin_pipe, 0);
    uv_pipe_init(get_uv_loop(), &process_data->stdout_pipe, 0);
    process_data->stdout_pipe.data = process_data;
    uv_pipe_init(get_uv_loop(), &process_data->stderr_pipe, 0);
    process_data->stderr_pipe.data = process_data;

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
        char errmsg[1024] = "Failed to spawn process: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        __self__->on_error->$class->__call__(__self__->on_error, process_data->process, to$str(errmsg));
    }
    // TODO: do we need to do some magic to read any data produced before this
    // callback is installed?
    uv_read_start((uv_stream_t*)&process_data->stdout_pipe, alloc_buffer, read_stdout);
    uv_read_start((uv_stream_t*)&process_data->stderr_pipe, alloc_buffer, read_stderr);

    return $R_CONT(c$cont, $None);
}

void close_cb(uv_handle_t *handle) {
    // TODO: clean something up?
}

$R process$$Process$done_writing$local (process$$Process __self__, $Cont c$cont) {
    uv_process_t *p = (uv_process_t *)from$int(__self__->_p);
    struct process_data *process_data = (struct process_data *)p->data;
    uv_stream_t *stdin = (uv_stream_t *)&process_data->stdin_pipe;
    uv_close(stdin, close_cb);
    return $R_CONT(c$cont, $None);
}

$R process$$Process$pid$local (process$$Process __self__, $Cont c$cont) {
    uv_process_t *p = (uv_process_t *)from$int(__self__->_p);
    return $R_CONT(c$cont, ($atom)to$int(p->pid));
}

$R process$$Process$signal$local (process$$Process __self__, $int signal, $Cont c$cont) {
    uv_process_t *p = (uv_process_t *)from$int(__self__->_p);
    uv_process_kill(p, from$int(signal));
    return $R_CONT(c$cont, $None);
}

$R process$$Process$write$local (process$$Process __self__, $bytes data, $Cont c$cont) {
    uv_process_t *p = (uv_process_t *)from$int(__self__->_p);

    uv_write_t *req = (uv_write_t *)malloc(sizeof(uv_write_t));
    uv_buf_t buf = uv_buf_init(data->str, data->nbytes);

    struct process_data *process_data = (struct process_data *)p->data;
    uv_stream_t *stdin = (uv_stream_t *)&process_data->stdin_pipe;

    int r = uv_write(req, stdin, &buf, 1, NULL);
    if (r != 0) {
        char errmsg[1024] = "Error writing to stdin of process: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
    }

    return $R_CONT(c$cont, $None);
}
