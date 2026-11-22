
void loggingQ___ext_init__() {}

int64_t loggingQ_MessageD__get_actor_id (loggingQ_Message self) {
    $Actor actor_self = GET_SELF();
    return actor_self->$globkey;
}

B_str loggingQ_MessageD__get_actor_class (loggingQ_Message self) {
    $Actor actor_self = GET_SELF();
    return actStrFromCString(unmangle_name(actor_self->$class->$GCINFO));
}

// Structured logging deliberately accepts erased values. This local policy
// knows the builtin representations; it does not try to recover a user Show
// extension after its evidence has been erased. The stateless witness is used
// synchronously by the container formatters, never stored or serialized.
static B_str logging_repr(B_Show witness, B_value value);
static struct B_ShowG_class logging_show_methods = {
    .$GCINFO = "logging builtin rendering",
    .$class_id = UNASSIGNED,
    .__str__ = logging_repr,
    .__repr__ = logging_repr
};
static struct B_Show logging_show_instance = {&logging_show_methods};

static B_str logging_repr(B_Show witness, B_value value) {
    if (!value)
        return B_None_str;
    switch (value->$class->$class_id) {
#define LOG_SCALAR(ID, T) case ID: return B_##T##_repr((B_##T)value)
        LOG_SCALAR(INT_ID, int);
        LOG_SCALAR(I64_ID, int);
        LOG_SCALAR(I32_ID, i32);
        LOG_SCALAR(I16_ID, i16);
        LOG_SCALAR(I8_ID, i8);
        LOG_SCALAR(U64_ID, u64);
        LOG_SCALAR(U32_ID, u32);
        LOG_SCALAR(U16_ID, u16);
        LOG_SCALAR(U8_ID, u8);
        LOG_SCALAR(U1_ID, u1);
        LOG_SCALAR(BIGINT_ID, bigint);
        LOG_SCALAR(FLOAT_ID, float);
        LOG_SCALAR(BOOL_ID, bool);
        LOG_SCALAR(COMPLEX_ID, complex);
        LOG_SCALAR(STR_ID, str);
        LOG_SCALAR(BYTES_ID, bytes);
        LOG_SCALAR(BYTEARRAY_ID, bytearray);
        LOG_SCALAR(RANGE_ID, range);
        LOG_SCALAR(SLICE_ID, slice);
        LOG_SCALAR(BITARRAY_ID, bitarray);
        LOG_SCALAR(BITSET_ID, bitset);
#undef LOG_SCALAR
        case LIST_ID: return B_list_base_str((B_list_base)value, witness);
        case ILIST_ID: return B_ilist_str((B_ilist)value, witness);
        case DICT_ID: return B_dict_base_str((B_dict_base)value, witness, witness);
        case IDICT_ID: return B_idict_str((B_idict)value, witness, witness);
        case SET_ID: return B_set_table_str(&((B_set)value)->data, witness);
        case ISET_ID: return B_iset_str((B_iset)value, witness);
        case ARRAY_ID: return B_array_str((B_array)value, NULL);
        case TUPLE_ID: {
            B_tuple tuple = (B_tuple)value;
            B_list parts = B_listD_new(tuple->size);
            for (int i = 0; i < tuple->size; i++)
                B_list_base_append((B_list_base)parts,
                    logging_repr(witness, (B_value)tuple->components[i]));
            return B_strD_join_par('(', parts, ')');
        }
        default: return B_valueD___repr__(value);
    }
}

B_str loggingQ_format_data(B_dict data) {
    return B_dict_base_str((B_dict_base)data, (B_Show)B_ShowD_strG_witness,
                          &logging_show_instance);
}
