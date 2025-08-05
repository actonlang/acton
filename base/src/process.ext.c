#include "rts/common.h"

#include <uv.h>
#include "../rts/io.h"
#include "../rts/log.h"

void processQ___ext_init__() {
    // NOP
}

struct process_data {
    processQ_Process process;
    $action on_stdout;
    $action on_stderr;
    $action on_exit;
    uv_pipe_t stdin_pipe;
    uv_pipe_t stdout_pipe;
    uv_pipe_t stderr_pipe;
};

void exit_handler(uv_process_t *req, int64_t exit_status, int term_signal) {
    struct process_data *process_data = req->data;
    uv_handle_t *stdin_handle = (uv_handle_t *)&process_data->stdin_pipe;
    // Ensure stdin is closed properly. stdin might be closed already from
    // done_writing, so check if it's closing first
    if (uv_is_closing(stdin_handle) == 0)
        uv_close(stdin_handle, NULL);

    // Close the process handle
    uv_close((uv_handle_t *)req, NULL);

    process_data->process->_p = to$int(0);

    // Trigger the on_exit callback
    $action3 f = ($action3)process_data->on_exit;
    f->$class->__asyn__(f, process_data->process, to$int(exit_status), to$int(term_signal));
}

void read_stderr(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buf) {
    struct process_data *process_data = (struct process_data *)stream->data;
    processQ_Process self = process_data->process;
    $action2 f = ($action2)process_data->on_stderr;

    if (nread < 0) {
        if (nread == UV_EOF) {
            uv_close((uv_handle_t *)stream, NULL);
            f->$class->__asyn__(f, self, B_None);
        } else {
            // Log and handle read error
            log_warn("Error reading from stderr");
        }
    } else if (nread > 0) {
        if (stream->data) {
            f->$class->__asyn__(f, self, to$bytesD_len(buf->base, nread));
        }
    }
}

void read_stdout(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buf) {
    struct process_data *process_data = (struct process_data *)stream->data;
    processQ_Process self = process_data->process;
    $action2 f = ($action2)process_data->on_stdout;

    if (nread < 0) {
        if (nread == UV_EOF) {
            uv_close((uv_handle_t *)stream, NULL);
            f->$class->__asyn__(f, self, B_None);
        } else {
            // Log and handle read error
            log_warn("Error reading from stdout");
        }
    } else if (nread > 0) {
        if (stream->data) {
            f->$class->__asyn__(f, self, to$bytesD_len(buf->base, nread));
        }
    }
}

$R processQ_ProcessD_aidG_local(processQ_Process self, $Cont c$cont) {
    return $R_CONT(c$cont, to$int(self->$globkey));
}

$R processQ_ProcessD__create_processG_local(processQ_Process self, $Cont c$cont) {
    pin_actor_affinity();
    struct process_data *process_data = acton_calloc(1, sizeof(struct process_data));
    process_data->process = self;
    process_data->on_stdout = self->on_stdout;
    process_data->on_stderr = self->on_stderr;
    process_data->on_exit = self->on_exit;

    uv_process_options_t *options = acton_calloc(1, sizeof(uv_process_options_t));

    uv_process_t *req = acton_calloc(1, sizeof(uv_process_t));
    self->_p = to$int((long)req);

    req->data = process_data;

    char **args = (char **)acton_malloc((self->cmd->length + 1) * sizeof(char *));

    for (int i = 0; i < self->cmd->length; i++) {
        args[i] = (char *)fromB_str(self->cmd->data[i]);
    }
    args[self->cmd->length] = NULL;

    if (self->workdir != B_None) {
        options->cwd = fromB_str(self->workdir);
    }

    if (self->new_env == B_None) {
        options->env = NULL;
    } else {
        char **env = (char **)acton_calloc((self->new_env->numelements + 1), sizeof(char *));
        B_IteratorD_dict_items iter = $NEW(B_IteratorD_dict_items, self->new_env);
        B_tuple item;

        for (int i = 0; i < self->new_env->numelements; i++) {
            item = (B_tuple)iter->$class->__next__(iter);
            char *key = fromB_str((B_str)item->components[0]);
            char *value = fromB_str((B_str)item->components[1]);
            size_t env_size = strlen(key) + strlen(value) + 2;
            char *env_var = acton_malloc(env_size);
            snprintf(env_var, env_size, "%s=%s", key, value);
            env[i] = env_var;
        }
        env[self->new_env->numelements] = NULL;
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
    process_stdio[0].data.stream = (uv_stream_t *)&(process_data->stdin_pipe);

    process_stdio[1].flags = UV_CREATE_PIPE | UV_WRITABLE_PIPE;
    process_stdio[1].data.stream = (uv_stream_t *)&(process_data->stdout_pipe);

    process_stdio[2].flags = UV_CREATE_PIPE | UV_WRITABLE_PIPE;
    process_stdio[2].data.stream = (uv_stream_t *)&(process_data->stderr_pipe);

    options->stdio_count = 3;

    options->exit_cb = exit_handler;
    options->file = args[0];
    options->args = args;

    int r = uv_spawn(get_uv_loop(), req, options);
    if (r != 0) {
        char errmsg[1024] = "Failed to spawn process: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg) - strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)self->on_error;
        f->$class->__asyn__(f, process_data->process, to$str(errmsg));
        return $R_CONT(c$cont, B_None);
    }
    // TODO: do we need to do some magic to read any data produced before this
    // callback is installed?

    uv_read_start((uv_stream_t *)&process_data->stdout_pipe, alloc_buffer, read_stdout);
    uv_read_start((uv_stream_t *)&process_data->stderr_pipe, alloc_buffer, read_stderr);

    return $R_CONT(c$cont, B_None);
}

void close_cb(uv_handle_t *handle) {
    // TODO: clean something up?
}

$R processQ_ProcessD_done_writingG_local(processQ_Process self, $Cont c$cont) {
    uv_process_t *p = (uv_process_t *)from$int(self->_p);
    struct process_data *process_data = (struct process_data *)p->data;
    uv_handle_t *stdin_handle = (uv_handle_t *)&process_data->stdin_pipe;
    // Ensure stdin is closed properly. stdin might be closed already from
    // exit_handler, so check if it's closing first
    if (uv_is_closing(stdin_handle) == 0)
        uv_close(stdin_handle, close_cb);
    return $R_CONT(c$cont, B_None);
}

$R processQ_ProcessD_pidG_local(processQ_Process self, $Cont c$cont) {
    uv_process_t *p = (uv_process_t *)from$int(self->_p);
    if (p == 0) {
        log_warn("Process has exited, ignoring PID request");
        return $R_CONT(c$cont, B_None);
    }
    return $R_CONT(c$cont, (B_atom)to$int(p->pid));
}

$R processQ_ProcessD_signalG_local(processQ_Process self, $Cont c$cont, B_int signal) {
    uv_process_t *p = (uv_process_t *)from$int(self->_p);
    if (p == 0) {
        log_warn("Process has exited, ignoring signal request");
        return $R_CONT(c$cont, B_None);
    }
    uv_process_kill(p, from$int(signal));
    return $R_CONT(c$cont, B_None);
}

$R processQ_ProcessD_writeG_local(processQ_Process self, $Cont c$cont, B_bytes data) {
    uv_process_t *p = (uv_process_t *)from$int(self->_p);
    if (p == 0) {
        log_warn("Process has exited, ignoring write request");
        return $R_CONT(c$cont, B_None);
    }

    uv_write_t *req = (uv_write_t *)acton_malloc(sizeof(uv_write_t));
    uv_buf_t buf = uv_buf_init(data->str, data->nbytes);

    struct process_data *process_data = (struct process_data *)p->data;
    uv_stream_t *stdin_handle = (uv_stream_t *)&process_data->stdin_pipe;

    int r = uv_write(req, stdin_handle, &buf, 1, NULL);
    if (r != 0) {
        char errmsg[1024] = "Error writing to stdin of process: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg) - strlen(errmsg));
        log_warn(errmsg);
    }

    return $R_CONT(c$cont, B_None);
}

B_str processQ__get_env_path() {
    size_t path_size = 1024;  // Initial buffer size
    char *path_buf = acton_malloc(path_size);

    int r = uv_os_getenv("PATH", path_buf, &path_size);
    if (r == UV_ENOBUFS) {
        // Buffer too small, reallocate with required size
        acton_free(path_buf);
        path_buf = acton_malloc(path_size);
        r = uv_os_getenv("PATH", path_buf, &path_size);
    }

    if (r != 0) {
        // If PATH is not set or other error occurred, return None
        acton_free(path_buf);
        return (B_str)B_None;
    }

    B_str result = to$str(path_buf);
    acton_free(path_buf);
    return result;
}
