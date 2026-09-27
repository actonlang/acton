void test_env_bytesQ___ext_init__() {}

B_bytes test_env_bytesQ_borrowed_name() {
    static char name[] = "=BADTRAIL";
    return actBytesFromCStringLengthNoCopy(name, 4);
}
