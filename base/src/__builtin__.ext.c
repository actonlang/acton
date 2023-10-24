#include "rts/rts.c"

void B___ext_init__() {

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

B_str B_BaseExceptionD__name (B_BaseException self) {
    return to$str(unmangle_name(self->$class->$GCINFO));
}

B_str B_type(B_value a) {
    return to$str(unmangle_name(a->$class->$GCINFO));
}
