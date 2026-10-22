#include "rts/rts.c"

static struct B_ValueError B_EnvVar_nul_name_error =
    STATIC_EXCEPTION(B_ValueError, "environment variable name contains a NUL byte");
static struct B_ValueError B_EnvVar_nul_value_error =
    STATIC_EXCEPTION(B_ValueError, "environment variable value contains a NUL byte");

void B___ext_init__() {
}

B_str B_BaseExceptionD__name (B_BaseException self) {
    return actStrFromCString(unmangle_name(self->$class->$GCINFO));
}

B_str B_type(B_value a) {
    if (a)
        return actStrFromCString(unmangle_name(a->$class->$GCINFO));
    return B_None_str;
}

// Environment variable names and values are C strings, so they cannot hold
// a NUL byte. Reject one rather than cut the name or value short at it, and
// return a NUL-terminated copy to pass to libuv.
static const char *env_cstring(B_bytes b, B_ValueError nul_error) {
    if (memchr(b->str, 0, b->nbytes))
        RAISE_EXC(nul_error);
    char *res = acton_malloc_atomic(b->nbytes + 1);
    memcpy(res, b->str, b->nbytes);
    res[b->nbytes] = '\0';
    return res;
}

$R B_EnvVarD_getenvbG_local (B_EnvVar self, $Cont C_cont, B_bytes name) {
    // uv_os_getenv is not threadsafe but our EnvVar actor forces serial execution

    // Try to use a small fixed size buffer
    size_t len = 256;
    char smallval[256];
    char *value = smallval;

    const char* env_var = env_cstring(name, &B_EnvVar_nul_name_error);

    // First, query the required buffer size by passing NULL as the buffer
    int r = uv_os_getenv(env_var, value, &len);
    if (r == UV_ENOENT) {
        // The environment variable does not exist
        return $R_CONT(C_cont, B_None);
    } else if (r == UV_ENOBUFS) {
        // Allocate the buffer and actually get the environment variable value
        value = (char*)acton_malloc(len);
        r = uv_os_getenv(env_var, value, &len);
    }
    if (r < 0) {
        RAISE(B_RuntimeError, $FORMAT("Failed to read the environment variable %s: %s", env_var, uv_strerror(r)));
    }
    return $R_CONT(C_cont, actBytesFromCStringCopy(value));
}

$R B_EnvVarD_setenvbG_local (B_EnvVar self, $Cont C_cont, B_bytes name, B_bytes value) {
    const char* env_var = env_cstring(name, &B_EnvVar_nul_name_error);
    const char* env_val = env_cstring(value, &B_EnvVar_nul_value_error);
    int r = uv_os_setenv(env_var, env_val);
    if (r < 0) {
        RAISE(B_RuntimeError, $FORMAT("Failed to set the environment variable %s: %s", env_var, uv_strerror(r)));
    }
    return $R_CONT(C_cont, B_None);
}

$R B_EnvVarD_unsetenvbG_local (B_EnvVar self, $Cont C_cont, B_bytes name) {
    const char* env_var = env_cstring(name, &B_EnvVar_nul_name_error);
    int r = uv_os_unsetenv(env_var);
    if (r < 0) {
        RAISE(B_RuntimeError, $FORMAT("Failed to unset the environment variable %s: %s", env_var, uv_strerror(r)));
    }
    return $R_CONT(C_cont, B_None);
}

// action def is_tty() -> bool:
$R B_StdioD_is_ttyG_local (B_Stdio self, $Cont C_cont) {
    return $R_CONT(C_cont, toB_bool(isatty(1)));
}

struct child_process_data {
    B_Process process;
    $action on_stdout;
    $action on_stderr;
    $action on_exit;
    uv_pipe_t stdin_pipe;
    uv_pipe_t stdout_pipe;
    uv_pipe_t stderr_pipe;
};

static void child_process_exit_handler(uv_process_t *req, int64_t exit_status, int term_signal) {
    struct child_process_data *process_data = req->data;
    uv_handle_t *stdin_handle = (uv_handle_t *)&process_data->stdin_pipe;
    // Ensure stdin is closed properly. stdin might be closed already from
    // done_writing, so check if it's closing first
    if (uv_is_closing(stdin_handle) == 0)
        uv_close(stdin_handle, NULL);

    // Close the process handle
    uv_close((uv_handle_t *)req, NULL);

    process_data->process->_p = 0;

    // Trigger the on_exit callback
    $action3 f = ($action3)process_data->on_exit;
    f->$class->__asyn__(f, process_data->process, toB_int(exit_status), toB_int(term_signal));
}

static void child_process_read_stderr(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buf) {
    struct child_process_data *process_data = (struct child_process_data *)stream->data;
    B_Process self = process_data->process;
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
            f->$class->__asyn__(f, self, actBytesFromCStringLengthCopy(buf->base, nread));
        }
    }
}

static void child_process_read_stdout(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buf) {
    struct child_process_data *process_data = (struct child_process_data *)stream->data;
    B_Process self = process_data->process;
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
            f->$class->__asyn__(f, self, actBytesFromCStringLengthCopy(buf->base, nread));
        }
    }
}

$R B_ProcessD_aidG_local(B_Process self, $Cont c$cont) {
    return $R_CONT(c$cont, toB_int(self->$globkey));
}

$R B_ProcessD__create_processG_local(B_Process self, $Cont c$cont) {
    pin_actor_affinity();

    if (self->cmd->length == 0) {
        char errmsg[1024] = "Failed to spawn process '<empty command>': empty command";
        log_warn("%s", errmsg);
        $action2 f = ($action2)self->on_error;
        f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
        return $R_CONT(c$cont, B_None);
    }

    struct child_process_data *process_data = acton_calloc(1, sizeof(struct child_process_data));
    process_data->process = self;
    process_data->on_stdout = self->on_stdout;
    process_data->on_stderr = self->on_stderr;
    process_data->on_exit = self->on_exit;

    uv_process_options_t *options = acton_calloc(1, sizeof(uv_process_options_t));

    uv_process_t *req = acton_calloc(1, sizeof(uv_process_t));
    self->_p = (int64_t)(intptr_t)req;

    req->data = process_data;

    char **args = (char **)acton_malloc((self->cmd->length + 1) * sizeof(char *));

    for (int i = 0; i < self->cmd->length; i++) {
        args[i] = (char *)fromB_str(self->cmd->data[i]);
    }
    args[self->cmd->length] = NULL;

    if (self->workdir != B_None) {
        options->cwd = (const char *)fromB_str(self->workdir);
    }

    if (self->new_env == B_None) {
        options->env = NULL;
    } else {
        char **env = (char **)acton_calloc((self->new_env->numelements + 1), sizeof(char *));
        B_IteratorD_dict_items iter = $NEW(B_IteratorD_dict_items, self->new_env);
        B_tuple item;

        for (int i = 0; i < self->new_env->numelements; i++) {
            $WORD next;
            iter->$class->__next__(iter, &next);
            item = (B_tuple)next;
            char *key = (char *)fromB_str((B_str)item->components[0]);
            char *value = (char *)fromB_str((B_str)item->components[1]);
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

    options->exit_cb = child_process_exit_handler;
    options->file = args[0];
    options->args = args;

    int r = uv_spawn(get_uv_loop(), req, options);
    if (r != 0) {
        self->_p = 0;
        uv_handle_t *process_handle = (uv_handle_t *)req;
        uv_handle_t *stdin_handle = (uv_handle_t *)&process_data->stdin_pipe;
        uv_handle_t *stdout_handle = (uv_handle_t *)&process_data->stdout_pipe;
        uv_handle_t *stderr_handle = (uv_handle_t *)&process_data->stderr_pipe;
        if (uv_handle_get_type(process_handle) != UV_UNKNOWN_HANDLE &&
            uv_is_closing(process_handle) == 0)
            uv_close(process_handle, NULL);
        if (uv_is_closing(stdin_handle) == 0)
            uv_close(stdin_handle, NULL);
        if (uv_is_closing(stdout_handle) == 0)
            uv_close(stdout_handle, NULL);
        if (uv_is_closing(stderr_handle) == 0)
            uv_close(stderr_handle, NULL);

        char errmsg[1024];
        snprintf(errmsg, sizeof(errmsg), "Failed to spawn process '%s': ", args[0]);
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg) - strlen(errmsg));
        log_warn("%s", errmsg);
        $action2 f = ($action2)self->on_error;
        f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
        return $R_CONT(c$cont, B_None);
    }
    // TODO: do we need to do some magic to read any data produced before this
    // callback is installed?

    uv_read_start((uv_stream_t *)&process_data->stdout_pipe, alloc_buffer, child_process_read_stdout);
    uv_read_start((uv_stream_t *)&process_data->stderr_pipe, alloc_buffer, child_process_read_stderr);

    return $R_CONT(c$cont, B_None);
}

static void child_process_close_cb(uv_handle_t *handle) {
    // TODO: clean something up?
}

$R B_ProcessD_done_writingG_local(B_Process self, $Cont c$cont) {
    uv_process_t *p = (uv_process_t *)(intptr_t)self->_p;
    if (p == 0) {
        log_warn("Process is not running, ignoring done_writing request");
        return $R_CONT(c$cont, B_None);
    }

    struct child_process_data *process_data = (struct child_process_data *)p->data;
    uv_handle_t *stdin_handle = (uv_handle_t *)&process_data->stdin_pipe;
    // Ensure stdin is closed properly. stdin might be closed already from
    // child_process_exit_handler, so check if it's closing first
    if (uv_is_closing(stdin_handle) == 0)
        uv_close(stdin_handle, child_process_close_cb);
    return $R_CONT(c$cont, B_None);
}

$R B_ProcessD_pidG_local(B_Process self, $Cont c$cont) {
    uv_process_t *p = (uv_process_t *)(intptr_t)self->_p;
    if (p == 0) {
        log_warn("Process is not running, ignoring PID request");
        return $R_CONT(c$cont, B_None);
    }

    return $R_CONT(c$cont, (B_atom)toB_int(p->pid));
}

$R B_ProcessD_signalG_local(B_Process self, $Cont c$cont, int64_t signal) {
    uv_process_t *p = (uv_process_t *)(intptr_t)self->_p;
    if (p == 0) {
        log_warn("Process is not running, ignoring signal request");
        return $R_CONT(c$cont, B_None);
    }

    uv_process_kill(p, (int)signal);
    return $R_CONT(c$cont, B_None);
}

$R B_ProcessD_writeG_local(B_Process self, $Cont c$cont, B_bytes data) {
    uv_process_t *p = (uv_process_t *)(intptr_t)self->_p;
    if (p == 0) {
        log_warn("Process is not running, ignoring write request");
        return $R_CONT(c$cont, B_None);
    }

    uv_write_t *req = (uv_write_t *)acton_malloc(sizeof(uv_write_t));
    uv_buf_t buf = uv_buf_init((char *)data->str, (unsigned int)data->nbytes);

    struct child_process_data *process_data = (struct child_process_data *)p->data;
    uv_stream_t *stdin_handle = (uv_stream_t *)&process_data->stdin_pipe;

    int r = uv_write(req, stdin_handle, &buf, 1, NULL);
    if (r != 0) {
        char errmsg[1024] = "Error writing to stdin of process: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg) - strlen(errmsg));
        log_warn(errmsg);
    }

    return $R_CONT(c$cont, B_None);
}

B_str B_actorid() {
    $Actor a = GET_SELF();
    return $FORMAT("%" PRId64, a->$globkey);
}
