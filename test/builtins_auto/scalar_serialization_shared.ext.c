void scalar_serialization_sharedQ___ext_init__(void) {
}

B_list scalar_serialization_sharedQ_shared($WORD x) {
    B_list res = B_listD_new(3);
    res->data[0] = x;
    res->data[1] = x;
    res->data[2] = x;
    res->length = 3;
    return res;
}
