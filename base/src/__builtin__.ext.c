#include "rts/rts.c"

void B___ext_init__() {

    B_HashableD_intG_methods.__eq__ = (B_bool (*)(B_HashableD_int, B_int, B_int))B_OrdD_intD___eq__;
    B_HashableD_intG_methods.__eq__ = (B_bool (*)(B_HashableD_int, B_int, B_int))B_OrdD_intD___eq__;
    B_HashableD_i32G_methods.__eq__ = (B_bool (*)(B_HashableD_i32, B_i32, B_i32))B_OrdD_i32D___eq__;
    B_HashableD_i16G_methods.__eq__ = (B_bool (*)(B_HashableD_i16, B_i16, B_i16))B_OrdD_i16D___eq__;
    B_HashableD_u64G_methods.__eq__ = (B_bool (*)(B_HashableD_u64, B_u64, B_u64))B_OrdD_u64D___eq__;
    B_HashableD_u32G_methods.__eq__ = (B_bool (*)(B_HashableD_u32, B_u32, B_u32))B_OrdD_u32D___eq__;
    B_HashableD_u16G_methods.__eq__ = (B_bool (*)(B_HashableD_u16, B_u16, B_u16))B_OrdD_u16D___eq__;
    B_HashableD_floatG_methods.__eq__ = (B_bool (*)(B_HashableD_float, B_float, B_float))B_OrdD_floatD___eq__;
    B_HashableD_strG_methods.__eq__ = (B_bool (*)(B_HashableD_str, B_str, B_str))B_OrdD_strD___eq__;
    B_HashableD_bytesG_methods.__eq__ = (B_bool (*)(B_HashableD_bytes, B_bytes, B_bytes))B_OrdD_bytesD___eq__;
    B_HashableD_complexG_methods.__eq__ = (B_bool (*)(B_HashableD_complex, B_complex, B_complex))B_HashableD_complexD___eq__;

    B_ContainerD_listG_methods.__len__ = (B_int (*)(B_ContainerD_list, B_list))B_CollectionD_SequenceD_listD___len__;
    B_ContainerD_listG_methods.__fromiter__ = (B_list (*)(B_ContainerD_list, B_Iterable, $WORD))B_CollectionD_SequenceD_listD___fromiter__;
    B_ContainerD_listG_methods.__iter__ = (B_Iterator (*)(B_ContainerD_list, B_list))B_CollectionD_SequenceD_listD___iter__;
}

B_str B_BaseExceptionD__name (B_BaseException self) {
    return to$str(unmangle_name(self->$class->$GCINFO));
}

B_str B_type(B_value a) {
    if (a)
        return to$str(unmangle_name(a->$class->$GCINFO));
    return to$str("None");
}

$R B_EnvD_getenvbG_local (B_Env self, $Cont C_cont, B_bytes name) {
    // uv_os_getenv is not threadsafe but our Env actor forces serial execution

    // Try to use a small fixed size buffer
    size_t len = 256;
    char smallval[256];
    char *value = smallval;

    const char* env_var = (char*)fromB_bytes(name);

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
        $RAISE((B_BaseException)B_RuntimeErrorG_new($FORMAT("Failed to read the environment variable %s: %s", env_var, uv_strerror(r))));
    }
    return $R_CONT(C_cont, to$bytes(value));
}

$R B_EnvD_setenvbG_local (B_Env self, $Cont C_cont, B_bytes name, B_bytes value) {
    const char* env_var = fromB_bytes(name);
    const char* env_val = fromB_bytes(value);
    int r = uv_os_setenv(env_var, env_val);
    if (r < 0) {
        $RAISE((B_BaseException)B_RuntimeErrorG_new($FORMAT("Failed to set the environment variable %s: %s", env_var, uv_strerror(r))));
    }
    return $R_CONT(C_cont, B_None);
}

$R B_EnvD_unsetenvbG_local (B_Env self, $Cont C_cont, B_bytes name) {
    const char* env_var = fromB_bytes(name);
    int r = uv_os_unsetenv(env_var);
    if (r < 0) {
        $RAISE((B_BaseException)B_RuntimeErrorG_new($FORMAT("Failed to unset the environment variable %s: %s", env_var, uv_strerror(r))));
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
