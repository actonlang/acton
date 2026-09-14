void set_freezeQ___ext_init__() {
}

uint64_t set_freezeQ_object_address(B_object value) {
    return (uint64_t)(uintptr_t)value;
}

uint64_t set_freezeQ_table_address(B_object value) {
    return (uint64_t)(uintptr_t)((B_set)value)->data.table;
}
