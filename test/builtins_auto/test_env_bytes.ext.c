void test_env_bytesQ___ext_init__() {}

B_bytes test_env_bytesQ_borrowed_name() {
    static const char name[] = "=BADTRAIL";
    return actBytesFromCStringLength(name, 4);
}
