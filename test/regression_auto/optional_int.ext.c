void optional_intQ___ext_init__(void) {}
B_int optional_intQ_native_identity(B_int value) { return value; }
$WORD optional_intQ_round_trip($WORD value) {
    return $deserialize($serialize(($Serializable)value, NULL), NULL);
}
B_int optional_intQ_NativeD_read(optional_intQ_Native self) { return self->item; }
bool optional_intQ_native_checks(void) {
    _Static_assert(__builtin_types_compatible_p(__typeof__(((optional_intQ_Slot)0)->item), int64_t), "optional int payload is inline");
    _Static_assert(__builtin_types_compatible_p(__typeof__(((optional_intQ_Specialized)0)->item), $WORD), "generic parent keeps boxed layout");
    _Static_assert(__builtin_types_compatible_p(__typeof__(((optional_intQ_Native)0)->item), B_int), "extension C keeps boxed fields");
    return true;
}
