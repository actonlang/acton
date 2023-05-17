
void B___ext_init__() {
    // Here is a list of things that have no fields that need to be scanned. It
    // is noted down as an explicit negative list of things so we don't have to
    // think "did we miss this". No, we did not, it just does not need scanning
    // and the GC bitmap is already initialized to 0 due to the GC malloc
    // actually being a calloc.
    // - B_bool / B_boolD_gcbm
    // - B_complex / B_complexD_gcbm
    // - B_float / B_floatD_gcbm
    // - B_i16 / B_i16D_gcbm
    // - B_i32 / B_i32D_gcbm
    // - B_i64 / B_i64D_gcbm
    // - B_u16 / B_u16D_gcbm
    // - B_u32 / B_u32D_gcbm
    // - B_u64 / B_u64D_gcbm
    // - B_range / B_rangeD_gcbm
    GC_set_bit(B_bytearrayD_gcbm, GC_WORD_OFFSET(struct B_bytearray, str));
    GC_set_bit(B_bytesD_gcbm, GC_WORD_OFFSET(struct B_bytes, str));
    GC_set_bit(B_dictD_gcbm, GC_WORD_OFFSET(struct B_dict, table));
    GC_set_bit(B_intD_gcbm, GC_WORD_OFFSET(struct B_int, val.n));
    GC_set_bit(B_listD_gcbm, GC_WORD_OFFSET(struct B_list, data));
    GC_set_bit(B_setD_gcbm, GC_WORD_OFFSET(struct B_set, table));
    GC_set_bit(B_sliceD_gcbm, GC_WORD_OFFSET(struct B_slice, start));
    GC_set_bit(B_sliceD_gcbm, GC_WORD_OFFSET(struct B_slice, step));
    GC_set_bit(B_sliceD_gcbm, GC_WORD_OFFSET(struct B_slice, stop));
    GC_set_bit(B_strD_gcbm, GC_WORD_OFFSET(struct B_str, str));

    B_HashableD_intG_methods.__eq__ = (B_bool (*)(B_HashableD_int, B_int, B_int))B_OrdD_intD___eq__;
    B_HashableD_i64G_methods.__eq__ = (B_bool (*)(B_HashableD_i64, B_i64, B_i64))B_OrdD_i64D___eq__;
    B_HashableD_i32G_methods.__eq__ = (B_bool (*)(B_HashableD_i32, B_i32, B_i32))B_OrdD_i32D___eq__;
    B_HashableD_i16G_methods.__eq__ = (B_bool (*)(B_HashableD_i16, B_i16, B_i16))B_OrdD_i16D___eq__;
    B_HashableD_u64G_methods.__eq__ = (B_bool (*)(B_HashableD_u64, B_u64, B_u64))B_OrdD_u64D___eq__;
    B_HashableD_u32G_methods.__eq__ = (B_bool (*)(B_HashableD_u32, B_u32, B_u32))B_OrdD_u32D___eq__;
    B_HashableD_u16G_methods.__eq__ = (B_bool (*)(B_HashableD_u16, B_u16, B_u16))B_OrdD_u16D___eq__;
    B_HashableD_floatG_methods.__eq__ = (B_bool (*)(B_HashableD_float, B_float, B_float))B_OrdD_floatD___eq__;
    B_HashableD_strG_methods.__eq__ = (B_bool (*)(B_HashableD_str, B_str, B_str))B_OrdD_strD___eq__;
    B_HashableD_bytesG_methods.__eq__ = (B_bool (*)(B_HashableD_bytes, B_bytes, B_bytes))B_OrdD_bytesD___eq__;

    B_ContainerD_listG_methods.__len__ = (B_int (*)(B_ContainerD_list, B_list))B_CollectionD_SequenceD_listD___len__;
    B_ContainerD_listG_methods.__fromiter__ = (B_list (*)(B_ContainerD_list, B_Iterable, $WORD))B_CollectionD_SequenceD_listD___fromiter__;
    B_ContainerD_listG_methods.__iter__ = (B_Iterator (*)(B_ContainerD_list, B_list))B_CollectionD_SequenceD_listD___iter__;
}
