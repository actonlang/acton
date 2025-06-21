B_list B_mk_list(int len, ...) {
    B_list res = B_listD_new(len);
    res->length = len;
    va_list args;
    va_start(args, len);
    for (int i = 0; i < len; i++)
        res->data[i] = va_arg(args, $WORD);
    return res;
}

B_set B_mk_set(int len, B_Hashable w,...) {
    B_set res = B_setG_new(w, NULL, NULL);
    va_list args;
    va_start(args, w);
    for (int i=0; i < len; i++) {
        $WORD elem = va_arg(args, $WORD);
        B_set_add_entry(res,w,elem,fromB_u64(B_hash(w, elem)));
    }
    return res;
}

B_dict B_mk_dict(int len, B_Hashable w,...) {
    B_dict res =  acton_malloc(sizeof(struct B_dict));
    res->$class = &B_dictG_methods;
    res->numelements = 0;
    res->table = NULL;
    va_list args;
    va_start(args, w);
    for (int i=0; i < len; i++) {
        B_tuple t =  va_arg(args, B_tuple);
        B_dictD_setitem(res, w, t->components[0], t->components[1]);
    }
    return res;
}
