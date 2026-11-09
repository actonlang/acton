void container_hashableQ___ext_init__(void) {}

$WORD container_hashableQ_round_trip($WORD value) {
    return $deserialize($serialize(($Serializable)value, NULL), NULL);
}

bool container_hashableQ_same_object(B_value a, B_value b) {
    return a == b;
}

bool container_hashableQ_native_checks(void) {
    B_Hashable hashwit = (B_Hashable)B_HashableD_intG_witness;
    B_Iterable iterable = (B_Iterable)B_SequenceD_listG_witness->W_Collection;
    B_dict dict = B_dictG_new(hashwit, NULL, NULL);
    B_set set = B_setG_new(hashwit, NULL, NULL);
    B_list pairs = B_listD_new(40);
    B_list keys = B_listD_new(40);
    for (int i = 0; i < 40; i++) {
        B_int key = toB_int(i);
        B_dictD_setitem(dict, key, toB_int(i + 1));
        B_SetD_setD_add(NULL, set, key);
        $listD_U_append(pairs, $NEWTUPLE(2, key, toB_int(i + 1)));
        $listD_U_append(keys, key);
    }
    if (dict->hashwit != hashwit || set->data.hashwit != hashwit)
        return false;
    // Existing-container wrappers need no receiver witness. Fresh boxes force
    // hashing/equality rather than the pointer-identity shortcut.
    if (fromB_int(B_IndexedD_MappingD_dictD___getitem__(NULL, dict, toB_int(17))) != 18
        || !B_SetD_setD___contains__(NULL, set, toB_int(17)))
        return false;
    B_dict from_mapping = B_MappingD_dictD___fromiter__(B_MappingD_dictG_new(hashwit),
        iterable, pairs);
    B_set from_set = B_SetD_setD___fromiter__(B_SetD_setG_new(hashwit),
        iterable, keys);
    B_idict from_imapping = B_IMappingD_idictD___fromiter__(B_IMappingD_idictG_new(hashwit),
        iterable, pairs);
    B_iset from_iset = B_ISetD_isetD___fromiter__(B_ISetD_isetG_new(hashwit),
        iterable, keys);
    return from_mapping->hashwit == hashwit && from_set->data.hashwit == hashwit
        && from_imapping->hashwit == hashwit && from_iset->data.hashwit == hashwit
        && fromB_int(B_dictD_get(from_mapping, toB_int(17), NULL)) == 18
        && fromB_int(B_IIndexedD_IMappingD_idictD___getitem__(NULL, from_imapping, toB_int(17))) == 18
        && B_ISetD_isetD___contains__(NULL, from_iset, toB_int(17));
}
