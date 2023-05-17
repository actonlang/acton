
void B___ext_init__() {
    memset(B_boolD_gcbm, 0xFF, sizeof(B_boolD_gcbm));
    memset(B_complexD_gcbm, 0xFF, sizeof(B_complexD_gcbm));
    memset(B_dictD_gcbm, 0xFF, sizeof(B_dictD_gcbm));
    memset(B_floatD_gcbm, 0xFF, sizeof(B_floatD_gcbm));
    memset(B_i16D_gcbm, 0xFF, sizeof(B_i16D_gcbm));
    memset(B_i32D_gcbm, 0xFF, sizeof(B_i32D_gcbm));
    memset(B_i64D_gcbm, 0xFF, sizeof(B_i64D_gcbm));
    memset(B_intD_gcbm, 0xFF, sizeof(B_intD_gcbm));
    memset(B_listD_gcbm, 0xFF, sizeof(B_listD_gcbm));
    memset(B_rangeD_gcbm, 0xFF, sizeof(B_rangeD_gcbm));
    memset(B_setD_gcbm, 0xFF, sizeof(B_setD_gcbm));
    memset(B_sliceD_gcbm, 0xFF, sizeof(B_sliceD_gcbm));
    memset(B_strD_gcbm, 0xFF, sizeof(B_strD_gcbm));
    memset(B_bytearrayD_gcbm, 0xFF, sizeof(B_bytearrayD_gcbm));
    memset(B_bytesD_gcbm, 0xFF, sizeof(B_bytesD_gcbm));
    memset(B_u16D_gcbm, 0xFF, sizeof(B_u16D_gcbm));
    memset(B_u32D_gcbm, 0xFF, sizeof(B_u32D_gcbm));
    memset(B_u64D_gcbm, 0xFF, sizeof(B_u64D_gcbm));

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
