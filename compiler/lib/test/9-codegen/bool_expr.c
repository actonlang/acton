/* Acton source hash: test-hash */
#include "rts/common.h"
#include "out/types/bool_expr.h"
B_NoneType bool_exprQ_t () {
    B_Hashable W_44 = (B_Hashable)B_HashableD_strG_witness;
    B_Hashable W_61 = (B_Hashable)B_HashableD_intG_witness;
    B_int my_int = to$int(1337);
    B_atom b_int = ((B_atom)toB_bool(((B_True)->val && my_int->val)));
    B_str my_str = to$str("hello");
    B_atom b_str = ((B_atom)toB_bool(((B_True)->val && my_str->val)));
    B_bytes my_bytes = to$bytesD_len("data", 4);
    B_value b_bytes = ((B_value)toB_bool(((B_True)->val && my_bytes->val)));
    B_list my_list = B_mk_list(3, to$int(1) , to$int(2) , to$int(3));
    B_value b_list = ((B_value)toB_bool(((B_True)->val && my_list->val)));
    B_dict my_dict = B_mk_dict(1, W_44, $NEWTUPLE(2, to$str("key"), to$int(42)));
    B_value b_dict = ((B_value)toB_bool(((B_True)->val && my_dict->val)));
    B_set my_set = B_mk_set(3, W_61, to$int(1) , to$int(2) , to$int(3));
    B_value b_set = ((B_value)toB_bool(((B_True)->val && my_set->val)));
    int64_t U_my_i64 = 64;
    B_atom b_i64 = ((B_atom)toB_bool(((B_True)->val && U_my_i64)));
    int32_t U_1my_i32 = 32;
    B_atom b_i32 = ((B_atom)toB_bool(((B_True)->val && U_1my_i32)));
    int16_t U_2my_i16 = 16;
    B_atom b_i16 = ((B_atom)toB_bool(((B_True)->val && U_2my_i16)));
    B_u64 my_u64 = B_u64G_new(((B_atom)to$int(64)), B_None);
    B_atom b_u64 = ((B_atom)toB_bool(((B_True)->val && my_u64->val)));
    uint32_t U_3my_u32 = 32;
    B_atom b_u32 = ((B_atom)toB_bool(((B_True)->val && U_3my_u32)));
    uint16_t U_4my_u16 = 16;
    B_atom b_u16 = ((B_atom)toB_bool(((B_True)->val && U_4my_u16)));
    double U_5my_float = 3.14;
    B_atom b_float = ((B_atom)toB_bool(((B_True)->val && U_5my_float)));
    B_atom or_int = ((B_atom)toB_bool(((B_False)->val || my_int->val)));
    B_atom or_str = ((B_atom)toB_bool(((B_False)->val || my_str->val)));
    B_atom or_i32 = ((B_atom)toB_bool(((B_False)->val || U_1my_i32)));
    B_atom or_float = ((B_atom)toB_bool(((B_False)->val || U_5my_float)));
    t = B_True;
    B_bool tc1 = toB_bool(((B_False)->val || t->val));
    B_bool f = B_False;
    B_bool tc2 = toB_bool(((B_True)->val && f->val));
    B_bool not_int = $NOT(B_bool, ((B_bool (*) (B_int))my_int->$class->__bool__)(my_int));
    B_bool not_str = $NOT(B_bool, ((B_bool (*) (B_str))my_str->$class->__bool__)(my_str));
    B_bool not_list = $NOT(B_bool, ((B_bool (*) (B_list))my_list->$class->__bool__)(my_list));
    B_bool not_i32 = $NOT(B_bool, ({ B_i32 $tmp = toB_i32(U_1my_i32);
                                     ((B_bool (*) (B_i32))$tmp->$class->__bool__)($tmp); }));
    B_bool not_float = $NOT(B_bool, ({ B_float $tmp = toB_float(U_5my_float);
                                       ((B_bool (*) (B_float))$tmp->$class->__bool__)($tmp); }));
    B_bool not_bool = $NOT(B_bool, t);
    B_atom mixed_chain = ((B_atom)toB_bool(((my_int->val && U_1my_i32) && my_u64->val)));
    return B_None;
}
int bool_exprQ_done$ = 0;
void bool_exprQ___init__ () {
    if (bool_exprQ_done$) return;
    bool_exprQ_done$ = 1;
}