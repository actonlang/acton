void show_bool_slotsQ___ext_init__(void) {}

// The prefix ends at serialization. Only protocol tables contain these slots.
_Static_assert(sizeof(struct B_valueG_class) == sizeof(struct $SerializableG_class), "value has legacy slots");
_Static_assert(sizeof(struct B_objectG_class) == sizeof(struct $SerializableG_class), "object has legacy slots");
_Static_assert(offsetof(struct B_ShowG_class, __str__) == sizeof(struct $SerializableG_class), "Show prefix mismatch");
_Static_assert(offsetof(struct B_BoolG_class, __bool__) == sizeof(struct $SerializableG_class), "Bool prefix mismatch");
_Static_assert(offsetof(struct $ContG_class, __call__) == sizeof(struct $SerializableG_class), "Cont has legacy slots");
_Static_assert(offsetof(struct B_BaseExceptionG_class, _name) == sizeof(struct $SerializableG_class), "exception has legacy slots");

bool show_bool_slotsQ_layouts(void) { return true; }
B_str show_bool_slotsQ_diagnostic(B_BaseException error) { return B_exception_str(error); }

// Control exceptions inherit the same serialized prefix, including metadata.
bool show_bool_slotsQ_control_roundtrip(void) {
    B_tuple original = $NEWTUPLE(4, $SEQG_new(), $BRKG_new(), $CNTG_new(),
                                $RETG_new((B_value)toB_int(7)));
    B_tuple restored = (B_tuple)B_deserialize(B_serialize((B_value)original));
    return restored->size == 4 &&
        (($SEQ)restored->components[0])->$class == &$SEQG_methods &&
        (($BRK)restored->components[1])->$class == &$BRKG_methods &&
        (($CNT)restored->components[2])->$class == &$CNTG_methods &&
        (($RET)restored->components[3])->$class == &$RETG_methods &&
        fromB_int((B_int)(($RET)restored->components[3])->val) == 7 &&
        (($SEQ)restored->components[0])->_exception_show == NULL;
}
