#include "rts/rts.c"

static struct B_ValueError B_Env_nul_name_error =
    STATIC_EXCEPTION(B_ValueError, "environment variable name contains a NUL byte");
static struct B_ValueError B_Env_nul_value_error =
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

$R B_EnvD_getenvbG_local (B_Env self, $Cont C_cont, B_bytes name) {
    // uv_os_getenv is not threadsafe but our Env actor forces serial execution

    // Try to use a small fixed size buffer
    size_t len = 256;
    char smallval[256];
    char *value = smallval;

    const char* env_var = env_cstring(name, &B_Env_nul_name_error);

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

$R B_EnvD_setenvbG_local (B_Env self, $Cont C_cont, B_bytes name, B_bytes value) {
    const char* env_var = env_cstring(name, &B_Env_nul_name_error);
    const char* env_val = env_cstring(value, &B_Env_nul_value_error);
    int r = uv_os_setenv(env_var, env_val);
    if (r < 0) {
        RAISE(B_RuntimeError, $FORMAT("Failed to set the environment variable %s: %s", env_var, uv_strerror(r)));
    }
    return $R_CONT(C_cont, B_None);
}

$R B_EnvD_unsetenvbG_local (B_Env self, $Cont C_cont, B_bytes name) {
    const char* env_var = env_cstring(name, &B_Env_nul_name_error);
    int r = uv_os_unsetenv(env_var);
    if (r < 0) {
        RAISE(B_RuntimeError, $FORMAT("Failed to unset the environment variable %s: %s", env_var, uv_strerror(r)));
    }
    return $R_CONT(C_cont, B_None);
}

// action def is_tty() -> bool:
$R B_EnvD_is_ttyG_local (B_Env self, $Cont C_cont) {
    return $R_CONT(C_cont, toB_bool(isatty(1)));
}

B_str B_actorid() {
    $Actor a = GET_SELF();
    return $FORMAT("%ld", a->$globkey);
}
