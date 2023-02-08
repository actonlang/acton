/*
Building project in /Users/sydow/acton/builtin/ty
  Compiling __builtin__.act for release in stub mode
#include "types/__builtin__.h"
#include "src/__builtin__.ext.c"
*/
B_NoneType B_valueD___init__ (B_value self) {
    return B_None;
}
B_bool B_valueD___bool__ (B_value self);
B_str B_valueD___str__ (B_value self);
B_str B_valueD___repr__ (B_value self);
void B_valueD___serialize__ (B_value self, $Serial$state state) {
}
B_value B_valueD___deserialize__ (B_value self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_value));
            self->$class = &B_valueG_methods;
            return self;
        }
        self = $DNEW(B_value, state);
    }
    return self;
}
B_value B_valueG_new() {
    B_value $tmp = malloc(sizeof(struct B_value));
    $tmp->$class = &B_valueG_methods;
    B_valueG_methods.__init__($tmp);
    return $tmp;
}
struct B_valueG_class B_valueG_methods;
void B_objectD___serialize__ (B_object self, $Serial$state state) {
}
B_object B_objectD___deserialize__ (B_object self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_object));
            self->$class = &B_objectG_methods;
            return self;
        }
        self = $DNEW(B_object, state);
    }
    return self;
}
B_object B_objectG_new() {
    B_object $tmp = malloc(sizeof(struct B_object));
    $tmp->$class = &B_objectG_methods;
    B_objectG_methods.__init__($tmp);
    return $tmp;
}
struct B_objectG_class B_objectG_methods;
void B_atomD___serialize__ (B_atom self, $Serial$state state) {
}
B_atom B_atomD___deserialize__ (B_atom self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_atom));
            self->$class = &B_atomG_methods;
            return self;
        }
        self = $DNEW(B_atom, state);
    }
    return self;
}
/*
B_atom B_atomG_new() {
    B_atom $tmp = malloc(sizeof(struct B_atom));
    $tmp->$class = &B_atomG_methods;
    B_atomG_methods.__init__($tmp);
    return $tmp;
}
*/
struct B_atomG_class B_atomG_methods;
/*
B_NoneType B_intD___init__ (B_int self, B_atom val);
void B_intD___serialize__ (B_int self, $Serial$state state) {
}
B_int B_intD___deserialize__ (B_int self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_int));
            self->$class = &B_intG_methods;
            return self;
        }
        self = $DNEW(B_int, state);
    }
    return self;
}
B_int B_intG_new(B_atom G_1) {
    B_int $tmp = malloc(sizeof(struct B_int));
    $tmp->$class = &B_intG_methods;
    B_intG_methods.__init__($tmp, G_1);
    return $tmp;
}
*/
struct B_intG_class B_intG_methods;
/*
B_NoneType B_i64D___init__ (B_i64 self, B_atom val);
void B_i64D___serialize__ (B_i64 self, $Serial$state state){
}
B_i64 B_i64D___deserialize__ (B_i64 self, $Serial$state state); {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_i64));
            self->$class = &B_i64G_methods;
            return self;
        }
        self = $DNEW(B_i64, state);
    }
    return self;
}
B_i64 B_i64G_new(B_atom G_1) {
    B_i64 $tmp = malloc(sizeof(struct B_i64));
    $tmp->$class = &B_i64G_methods;
    B_i64G_methods.__init__($tmp, G_1);
    return $tmp;
}
*/
struct B_i64G_class B_i64G_methods;
/*
B_NoneType B_floatD___init__ (B_float self, B_atom val);
void B_floatD___serialize__ (B_float self, $Serial$state state) {
}
B_float B_floatD___deserialize__ (B_float self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_float));
            self->$class = &B_floatG_methods;
            return self;
        }
        self = $DNEW(B_float, state);
    }
    return self;
}
B_float B_floatG_new(B_atom G_1) {
    B_float $tmp = malloc(sizeof(struct B_float));
    $tmp->$class = &B_floatG_methods;
    B_floatG_methods.__init__($tmp, G_1);
    return $tmp;
}
*/
struct B_floatG_class B_floatG_methods;
/*
B_NoneType B_boolD___init__ (B_bool self, B_value val);
void B_boolD___serialize__ (B_bool self, $Serial$state state) {
}
B_bool B_boolD___deserialize__ (B_bool self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_bool));
            self->$class = &B_boolG_methods;
            return self;
        }
        self = $DNEW(B_bool, state);
    }
    return self;
}
B_bool B_boolG_new(B_value G_1) {
    B_bool $tmp = malloc(sizeof(struct B_bool));
    $tmp->$class = &B_boolG_methods;
    B_boolG_methods.__init__($tmp, G_1);
    return $tmp;
}
*/
struct B_boolG_class B_boolG_methods;
/*
B_NoneType B_sliceD___init__ (B_slice self, B_int start, B_int stop, B_int step);
void B_sliceD___serialize__ (B_slice self, $Serial$state state) {
}
B_slice B_sliceD___deserialize__ (B_slice self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_slice));
            self->$class = &B_sliceG_methods;
            return self;
        }
        self = $DNEW(B_slice, state);
    }
    return self;
}
B_slice B_sliceG_new(B_int G_1, B_int G_2, B_int G_3) {
    B_slice $tmp = malloc(sizeof(struct B_slice));
    $tmp->$class = &B_sliceG_methods;
    B_sliceG_methods.__init__($tmp, G_1, G_2, G_3);
    return $tmp;
}
*/
struct B_sliceG_class B_sliceG_methods;
/*
B_NoneType B_listD___init__ (B_list self, B_Iterable W_IterableE_62, $WORD val);
B_list B_listD_copy (B_list self);
void B_listD___serialize__ (B_list self, $Serial$state state) {
}
B_list B_listD___deserialize__ (B_list self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_list));
            self->$class = &B_listG_methods;
            return self;
        }
        self = $DNEW(B_list, state);
    }
    return self;
}
B_list B_listG_new(B_Iterable G_1, $WORD G_2) {
    B_list $tmp = malloc(sizeof(struct B_list));
    $tmp->$class = &B_listG_methods;
    B_listG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
*/
struct B_listG_class B_listG_methods;
/*
B_NoneType B_rangeD___init__ (B_range self, B_int start, B_int stop, B_int step);
void B_rangeD___serialize__ (B_range self, $Serial$state state) {
}
B_range B_rangeD___deserialize__ (B_range self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_range));
            self->$class = &B_rangeG_methods;
            return self;
        }
        self = $DNEW(B_range, state);
    }
    return self;
}
B_range B_rangeG_new(B_int G_1, B_int G_2, B_int G_3) {
    B_range $tmp = malloc(sizeof(struct B_range));
    $tmp->$class = &B_rangeG_methods;
    B_rangeG_methods.__init__($tmp, G_1, G_2, G_3);
    return $tmp;
}
*/
struct B_rangeG_class B_rangeG_methods;
void B_IteratorD___serialize__ (B_Iterator self, $Serial$state state) {
}
B_Iterator B_IteratorD___deserialize__ (B_Iterator self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_Iterator));
            self->$class = &B_IteratorG_methods;
            return self;
        }
        self = $DNEW(B_Iterator, state);
    }
    return self;
}
struct B_IteratorG_class B_IteratorG_methods;
B_NoneType B_IterableD___init__ (B_Iterable W_self) {
    return B_None;
}
void B_IterableD___serialize__ (B_Iterable self, $Serial$state state) {
}
B_Iterable B_IterableD___deserialize__ (B_Iterable self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_Iterable));
            self->$class = &B_IterableG_methods;
            return self;
        }
        self = $DNEW(B_Iterable, state);
    }
    return self;
}
struct B_IterableG_class B_IterableG_methods;
B_NoneType B_strD___init__ (B_str self, B_value val);
B_str B_strD_capitalize (B_str self);
B_str B_strD_center (B_str self, B_int width, B_str fillchar);
B_int B_strD_count (B_str self, B_str sub, B_int start, B_int end);
B_bytes B_strD_encode (B_str self);
B_bool B_strD_endswith (B_str self, B_str suffix, B_int start, B_int end);
B_str B_strD_expandtabs (B_str self, B_int tabsize);
B_int B_strD_find (B_str self, B_str sub, B_int start, B_int end);
B_int B_strD_index (B_str self, B_str sub, B_int start, B_int end);
B_bool B_strD_isalnum (B_str self);
B_bool B_strD_isalpha (B_str self);
B_bool B_strD_isascii (B_str self);
B_bool B_strD_isdecimal (B_str self);
B_bool B_strD_islower (B_str self);
B_bool B_strD_isprintable (B_str self);
B_bool B_strD_isspace (B_str self);
B_bool B_strD_istitle (B_str self);
B_bool B_strD_isupper (B_str self);
B_str B_strD_join (B_str self, B_Iterable W_IterableE_154, $WORD iterable);
B_str B_strD_ljust (B_str self, B_int width, B_str fillchar);
B_str B_strD_lower (B_str self);
B_str B_strD_lstrip (B_str self, B_str chars);
B_tuple B_strD_partition (B_str self, B_str sep);
B_str B_strD_replace (B_str self, B_str old, B_str new, B_int count);
B_int B_strD_rfind (B_str self, B_str sub, B_int start, B_int end);
B_int B_strD_rindex (B_str self, B_str sub, B_int start, B_int end);
B_str B_strD_rjust (B_str self, B_int width, B_str fillchar);
B_tuple B_strD_rpartition (B_str self, B_str sep);
B_str B_strD_rstrip (B_str self, B_str chars);
B_list B_strD_split (B_str self, B_str sep, B_int maxsplit);
B_list B_strD_splitlines (B_str self, B_bool keepends);
B_bool B_strD_startswith (B_str self, B_str prefix, B_int start, B_int end);
B_str B_strD_strip (B_str self, B_str chars);
B_str B_strD_upper (B_str self);
B_str B_strD_zfill (B_str self, B_int width);
/*
void B_strD___serialize__ (B_str self, $Serial$state state) {
}
B_str B_strD___deserialize__ (B_str self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_str));
            self->$class = &B_strG_methods;
            return self;
        }
        self = $DNEW(B_str, state);
    }
    return self;
}
B_str B_strG_new(B_value G_1) {
    B_str $tmp = malloc(sizeof(struct B_str));
    $tmp->$class = &B_strG_methods;
    B_strG_methods.__init__($tmp, G_1);
    return $tmp;
}
*/
struct B_strG_class B_strG_methods;
B_NoneType B_bytesD___init__ (B_bytes self, B_Iterable W_IterableE_226, $WORD iterable);
B_bytes B_bytesD_capitalize (B_bytes self);
B_bytes B_bytesD_center (B_bytes self, B_int width, B_bytes fillchar);
B_int B_bytesD_count (B_bytes self, B_bytes sub, B_int start, B_int end);
B_str B_bytesD_decode (B_bytes self);
B_bool B_bytesD_endswith (B_bytes self, B_bytes suffix, B_int start, B_int end);
B_bytes B_bytesD_expandtabs (B_bytes self, B_int tabsize);
B_int B_bytesD_find (B_bytes self, B_bytes sub, B_int start, B_int end);
B_int B_bytesD_index (B_bytes self, B_bytes sub, B_int start, B_int end);
B_bool B_bytesD_isalnum (B_bytes self);
B_bool B_bytesD_isalpha (B_bytes self);
B_bool B_bytesD_isascii (B_bytes self);
B_bool B_bytesD_isdigit (B_bytes self);
B_bool B_bytesD_islower (B_bytes self);
B_bool B_bytesD_isspace (B_bytes self);
B_bool B_bytesD_istitle (B_bytes self);
B_bool B_bytesD_isupper (B_bytes self);
B_bytes B_bytesD_join (B_bytes self, B_Iterable W_IterableE_296, $WORD iterable);
B_bytes B_bytesD_ljust (B_bytes self, B_int width, B_bytes fillchar);
B_bytes B_bytesD_lower (B_bytes self);
B_bytes B_bytesD_lstrip (B_bytes self, B_bytes chars);
B_tuple B_bytesD_partition (B_bytes self, B_bytes sep);
B_bytes B_bytesD_replace (B_bytes self, B_bytes old, B_bytes new, B_int count);
B_int B_bytesD_rfind (B_bytes self, B_bytes sub, B_int start, B_int end);
B_int B_bytesD_rindex (B_bytes self, B_bytes sub, B_int start, B_int end);
B_bytes B_bytesD_rjust (B_bytes self, B_int width, B_bytes fillchar);
B_tuple B_bytesD_rpartition (B_bytes self, B_bytes sep);
B_bytes B_bytesD_rstrip (B_bytes self, B_bytes chars);
B_list B_bytesD_split (B_bytes self, B_bytes sep, B_int maxsplit);
B_list B_bytesD_splitlines (B_bytes self, B_bool keepends);
B_bool B_bytesD_startswith (B_bytes self, B_bytes prefix, B_int start, B_int end);
B_bytes B_bytesD_strip (B_bytes self, B_bytes chars);
B_bytes B_bytesD_upper (B_bytes self);
B_bytes B_bytesD_zfill (B_bytes self, B_int width);
/*
void B_bytesD___serialize__ (B_bytes self, $Serial$state state) {
}
B_bytes B_bytesD___deserialize__ (B_bytes self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_bytes));
            self->$class = &B_bytesG_methods;
            return self;
        }
        self = $DNEW(B_bytes, state);
    }
    return self;
}
B_bytes B_bytesG_new(B_Iterable G_1, $WORD G_2) {
    B_bytes $tmp = malloc(sizeof(struct B_bytes));
    $tmp->$class = &B_bytesG_methods;
    B_bytesG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
*/
struct B_bytesG_class B_bytesG_methods;
B_NoneType B_bytearrayD___init__ (B_bytearray self, B_bytes val);
B_bytearray B_bytearrayD_capitalize (B_bytearray self);
B_bytearray B_bytearrayD_center (B_bytearray self, B_int width, B_bytearray fillchar);
B_int B_bytearrayD_count (B_bytearray self, B_bytearray sub, B_int start, B_int end);
B_str B_bytearrayD_decode (B_bytearray self);
B_bool B_bytearrayD_endswith (B_bytearray self, B_bytearray suffix, B_int start, B_int end);
B_bytearray B_bytearrayD_expandtabs (B_bytearray self, B_int tabsize);
B_int B_bytearrayD_find (B_bytearray self, B_bytearray sub, B_int start, B_int end);
B_int B_bytearrayD_index (B_bytearray self, B_bytearray sub, B_int start, B_int end);
B_bool B_bytearrayD_isalnum (B_bytearray self);
B_bool B_bytearrayD_isalpha (B_bytearray self);
B_bool B_bytearrayD_isascii (B_bytearray self);
B_bool B_bytearrayD_isdigit (B_bytearray self);
B_bool B_bytearrayD_islower (B_bytearray self);
B_bool B_bytearrayD_isspace (B_bytearray self);
B_bool B_bytearrayD_istitle (B_bytearray self);
B_bool B_bytearrayD_isupper (B_bytearray self);
B_bytearray B_bytearrayD_join (B_bytearray self, B_Iterable W_IterableE_436, $WORD iterable);
B_bytearray B_bytearrayD_ljust (B_bytearray self, B_int width, B_bytearray fillchar);
B_bytearray B_bytearrayD_lower (B_bytearray self);
B_bytearray B_bytearrayD_lstrip (B_bytearray self, B_bytearray chars);
B_tuple B_bytearrayD_partition (B_bytearray self, B_bytearray sep);
B_bytearray B_bytearrayD_replace (B_bytearray self, B_bytearray old, B_bytearray new, B_int count);
B_int B_bytearrayD_rfind (B_bytearray self, B_bytearray sub, B_int start, B_int end);
B_int B_bytearrayD_rindex (B_bytearray self, B_bytearray sub, B_int start, B_int end);
B_bytearray B_bytearrayD_rjust (B_bytearray self, B_int width, B_bytearray fillchar);
B_tuple B_bytearrayD_rpartition (B_bytearray self, B_bytearray sep);
B_bytearray B_bytearrayD_rstrip (B_bytearray self, B_bytearray chars);
B_list B_bytearrayD_split (B_bytearray self, B_bytearray sep, B_int maxsplit);
B_list B_bytearrayD_splitlines (B_bytearray self, B_bool keepends);
B_bool B_bytearrayD_startswith (B_bytearray self, B_bytearray prefix, B_int start, B_int end);
B_bytearray B_bytearrayD_strip (B_bytearray self, B_bytearray chars);
B_bytearray B_bytearrayD_upper (B_bytearray self);
B_bytearray B_bytearrayD_zfill (B_bytearray self, B_int width);
/*
void B_bytearrayD___serialize__ (B_bytearray self, $Serial$state state) {
}
B_bytearray B_bytearrayD___deserialize__ (B_bytearray self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_bytearray));
            self->$class = &B_bytearrayG_methods;
            return self;
        }
        self = $DNEW(B_bytearray, state);
    }
    return self;
}
B_bytearray B_bytearrayG_new(B_bytes G_1) {
    B_bytearray $tmp = malloc(sizeof(struct B_bytearray));
    $tmp->$class = &B_bytearrayG_methods;
    B_bytearrayG_methods.__init__($tmp, G_1);
    return $tmp;
}
*/
struct B_bytearrayG_class B_bytearrayG_methods;
/*
void B_MsgD___serialize__ (B_Msg self, $Serial$state state) {
}
B_Msg B_MsgD___deserialize__ (B_Msg self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_Msg));
            self->$class = &B_MsgG_methods;
            return self;
        }
        self = $DNEW(B_Msg, state);
    }
    return self;
}
B_Msg B_MsgG_new() {
    B_Msg $tmp = malloc(sizeof(struct B_Msg));
    $tmp->$class = &B_MsgG_methods;
    B_MsgG_methods.__init__($tmp);
    return $tmp;
}
struct B_MsgG_class B_MsgG_methods;
*/
B_NoneType B_BaseExceptionD___init__ (B_BaseException self, B_str msg) {
    self->error_message = msg;
    return B_None;
}
void B_BaseExceptionD___serialize__ (B_BaseException self, $Serial$state state) {
    $step_serialize(self->error_message, state);
}
B_BaseException B_BaseExceptionD___deserialize__ (B_BaseException self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_BaseException));
            self->$class = &B_BaseExceptionG_methods;
            return self;
        }
        self = $DNEW(B_BaseException, state);
    }
    self->error_message = $step_deserialize(state);
    return self;
}
B_BaseException B_BaseExceptionG_new(B_str G_1) {
    B_BaseException $tmp = malloc(sizeof(struct B_BaseException));
    $tmp->$class = &B_BaseExceptionG_methods;
    B_BaseExceptionG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct B_BaseExceptionG_class B_BaseExceptionG_methods;
void B_SystemExitD___serialize__ (B_SystemExit self, $Serial$state state) {
    $step_serialize(self->error_message, state);
}
B_SystemExit B_SystemExitD___deserialize__ (B_SystemExit self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_SystemExit));
            self->$class = &B_SystemExitG_methods;
            return self;
        }
        self = $DNEW(B_SystemExit, state);
    }
    self->error_message = $step_deserialize(state);
    return self;
}
B_SystemExit B_SystemExitG_new(B_str G_1) {
    B_SystemExit $tmp = malloc(sizeof(struct B_SystemExit));
    $tmp->$class = &B_SystemExitG_methods;
    B_SystemExitG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct B_SystemExitG_class B_SystemExitG_methods;
void B_KeyboardInterruptD___serialize__ (B_KeyboardInterrupt self, $Serial$state state) {
    $step_serialize(self->error_message, state);
}
B_KeyboardInterrupt B_KeyboardInterruptD___deserialize__ (B_KeyboardInterrupt self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_KeyboardInterrupt));
            self->$class = &B_KeyboardInterruptG_methods;
            return self;
        }
        self = $DNEW(B_KeyboardInterrupt, state);
    }
    self->error_message = $step_deserialize(state);
    return self;
}
B_KeyboardInterrupt B_KeyboardInterruptG_new(B_str G_1) {
    B_KeyboardInterrupt $tmp = malloc(sizeof(struct B_KeyboardInterrupt));
    $tmp->$class = &B_KeyboardInterruptG_methods;
    B_KeyboardInterruptG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct B_KeyboardInterruptG_class B_KeyboardInterruptG_methods;
void B_ExceptionD___serialize__ (B_Exception self, $Serial$state state) {
    $step_serialize(self->error_message, state);
}
B_Exception B_ExceptionD___deserialize__ (B_Exception self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_Exception));
            self->$class = &B_ExceptionG_methods;
            return self;
        }
        self = $DNEW(B_Exception, state);
    }
    self->error_message = $step_deserialize(state);
    return self;
}
B_Exception B_ExceptionG_new(B_str G_1) {
    B_Exception $tmp = malloc(sizeof(struct B_Exception));
    $tmp->$class = &B_ExceptionG_methods;
    B_ExceptionG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct B_ExceptionG_class B_ExceptionG_methods;
void B_AssertionErrorD___serialize__ (B_AssertionError self, $Serial$state state) {
    $step_serialize(self->error_message, state);
}
B_AssertionError B_AssertionErrorD___deserialize__ (B_AssertionError self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_AssertionError));
            self->$class = &B_AssertionErrorG_methods;
            return self;
        }
        self = $DNEW(B_AssertionError, state);
    }
    self->error_message = $step_deserialize(state);
    return self;
}
B_AssertionError B_AssertionErrorG_new(B_str G_1) {
    B_AssertionError $tmp = malloc(sizeof(struct B_AssertionError));
    $tmp->$class = &B_AssertionErrorG_methods;
    B_AssertionErrorG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct B_AssertionErrorG_class B_AssertionErrorG_methods;
void B_LookupErrorD___serialize__ (B_LookupError self, $Serial$state state) {
    $step_serialize(self->error_message, state);
}
B_LookupError B_LookupErrorD___deserialize__ (B_LookupError self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_LookupError));
            self->$class = &B_LookupErrorG_methods;
            return self;
        }
        self = $DNEW(B_LookupError, state);
    }
    self->error_message = $step_deserialize(state);
    return self;
}
B_LookupError B_LookupErrorG_new(B_str G_1) {
    B_LookupError $tmp = malloc(sizeof(struct B_LookupError));
    $tmp->$class = &B_LookupErrorG_methods;
    B_LookupErrorG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct B_LookupErrorG_class B_LookupErrorG_methods;
void B_IndexErrorD___serialize__ (B_IndexError self, $Serial$state state) {
    $step_serialize(self->error_message, state);
}
B_IndexError B_IndexErrorD___deserialize__ (B_IndexError self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_IndexError));
            self->$class = &B_IndexErrorG_methods;
            return self;
        }
        self = $DNEW(B_IndexError, state);
    }
    self->error_message = $step_deserialize(state);
    return self;
}
B_IndexError B_IndexErrorG_new(B_str G_1) {
    B_IndexError $tmp = malloc(sizeof(struct B_IndexError));
    $tmp->$class = &B_IndexErrorG_methods;
    B_IndexErrorG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct B_IndexErrorG_class B_IndexErrorG_methods;
void B_KeyErrorD___serialize__ (B_KeyError self, $Serial$state state) {
    $step_serialize(self->error_message, state);
}
B_KeyError B_KeyErrorD___deserialize__ (B_KeyError self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_KeyError));
            self->$class = &B_KeyErrorG_methods;
            return self;
        }
        self = $DNEW(B_KeyError, state);
    }
    self->error_message = $step_deserialize(state);
    return self;
}
B_KeyError B_KeyErrorG_new(B_str G_1) {
    B_KeyError $tmp = malloc(sizeof(struct B_KeyError));
    $tmp->$class = &B_KeyErrorG_methods;
    B_KeyErrorG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct B_KeyErrorG_class B_KeyErrorG_methods;
void B_MemoryErrorD___serialize__ (B_MemoryError self, $Serial$state state) {
    $step_serialize(self->error_message, state);
}
B_MemoryError B_MemoryErrorD___deserialize__ (B_MemoryError self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_MemoryError));
            self->$class = &B_MemoryErrorG_methods;
            return self;
        }
        self = $DNEW(B_MemoryError, state);
    }
    self->error_message = $step_deserialize(state);
    return self;
}
B_MemoryError B_MemoryErrorG_new(B_str G_1) {
    B_MemoryError $tmp = malloc(sizeof(struct B_MemoryError));
    $tmp->$class = &B_MemoryErrorG_methods;
    B_MemoryErrorG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct B_MemoryErrorG_class B_MemoryErrorG_methods;
void B_OSErrorD___serialize__ (B_OSError self, $Serial$state state) {
    $step_serialize(self->error_message, state);
}
B_OSError B_OSErrorD___deserialize__ (B_OSError self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_OSError));
            self->$class = &B_OSErrorG_methods;
            return self;
        }
        self = $DNEW(B_OSError, state);
    }
    self->error_message = $step_deserialize(state);
    return self;
}
B_OSError B_OSErrorG_new(B_str G_1) {
    B_OSError $tmp = malloc(sizeof(struct B_OSError));
    $tmp->$class = &B_OSErrorG_methods;
    B_OSErrorG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct B_OSErrorG_class B_OSErrorG_methods;
void B_RuntimeErrorD___serialize__ (B_RuntimeError self, $Serial$state state) {
    $step_serialize(self->error_message, state);
}
B_RuntimeError B_RuntimeErrorD___deserialize__ (B_RuntimeError self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_RuntimeError));
            self->$class = &B_RuntimeErrorG_methods;
            return self;
        }
        self = $DNEW(B_RuntimeError, state);
    }
    self->error_message = $step_deserialize(state);
    return self;
}
B_RuntimeError B_RuntimeErrorG_new(B_str G_1) {
    B_RuntimeError $tmp = malloc(sizeof(struct B_RuntimeError));
    $tmp->$class = &B_RuntimeErrorG_methods;
    B_RuntimeErrorG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct B_RuntimeErrorG_class B_RuntimeErrorG_methods;
void B_NotImplementedErrorD___serialize__ (B_NotImplementedError self, $Serial$state state) {
    $step_serialize(self->error_message, state);
}
B_NotImplementedError B_NotImplementedErrorD___deserialize__ (B_NotImplementedError self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_NotImplementedError));
            self->$class = &B_NotImplementedErrorG_methods;
            return self;
        }
        self = $DNEW(B_NotImplementedError, state);
    }
    self->error_message = $step_deserialize(state);
    return self;
}
B_NotImplementedError B_NotImplementedErrorG_new(B_str G_1) {
    B_NotImplementedError $tmp = malloc(sizeof(struct B_NotImplementedError));
    $tmp->$class = &B_NotImplementedErrorG_methods;
    B_NotImplementedErrorG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct B_NotImplementedErrorG_class B_NotImplementedErrorG_methods;
void B_ValueErrorD___serialize__ (B_ValueError self, $Serial$state state) {
    $step_serialize(self->error_message, state);
}
B_ValueError B_ValueErrorD___deserialize__ (B_ValueError self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_ValueError));
            self->$class = &B_ValueErrorG_methods;
            return self;
        }
        self = $DNEW(B_ValueError, state);
    }
    self->error_message = $step_deserialize(state);
    return self;
}
B_ValueError B_ValueErrorG_new(B_str G_1) {
    B_ValueError $tmp = malloc(sizeof(struct B_ValueError));
    $tmp->$class = &B_ValueErrorG_methods;
    B_ValueErrorG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct B_ValueErrorG_class B_ValueErrorG_methods;
B_NoneType B_IdentityD___init__ (B_Identity W_self) {
    return B_None;
}
void B_IdentityD___serialize__ (B_Identity self, $Serial$state state) {
}
B_Identity B_IdentityD___deserialize__ (B_Identity self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_Identity));
            self->$class = &B_IdentityG_methods;
            return self;
        }
        self = $DNEW(B_Identity, state);
    }
    return self;
}
struct B_IdentityG_class B_IdentityG_methods;
B_NoneType B_EqD___init__ (B_Eq W_self) {
    return B_None;
}
B_bool B_EqD___ne__ (B_Eq W_self, $WORD a, $WORD b) {
    return $NOT(B_bool, ((B_bool (*) (B_Eq, $WORD, $WORD))W_self->$class->__eq__)(W_self, a, b));
}
void B_EqD___serialize__ (B_Eq self, $Serial$state state) {
}
B_Eq B_EqD___deserialize__ (B_Eq self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_Eq));
            self->$class = &B_EqG_methods;
            return self;
        }
        self = $DNEW(B_Eq, state);
    }
    return self;
}
struct B_EqG_class B_EqG_methods;
B_NoneType B_OrdD___init__ (B_Ord W_self) {
    ((B_NoneType (*) (B_Eq))B_EqG_methods.__init__)(((B_Eq)W_self));
    return B_None;
}
B_bool B_OrdD___le__ (B_Ord W_self, $WORD a, $WORD b) {
    return $OR(B_bool, ((B_bool (*) (B_Ord, $WORD, $WORD))W_self->$class->__lt__)(W_self, a, b), ((B_bool (*) (B_Ord, $WORD, $WORD))W_self->$class->__eq__)(W_self, a, b));
}
B_bool B_OrdD___gt__ (B_Ord W_self, $WORD a, $WORD b) {
    return ((B_bool (*) (B_Ord, $WORD, $WORD))W_self->$class->__lt__)(W_self, b, a);
}
B_bool B_OrdD___ge__ (B_Ord W_self, $WORD a, $WORD b) {
    return ((B_bool (*) (B_Ord, $WORD, $WORD))W_self->$class->__le__)(W_self, b, a);
}
void B_OrdD___serialize__ (B_Ord self, $Serial$state state) {
}
B_Ord B_OrdD___deserialize__ (B_Ord self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_Ord));
            self->$class = &B_OrdG_methods;
            return self;
        }
        self = $DNEW(B_Ord, state);
    }
    return self;
}
struct B_OrdG_class B_OrdG_methods;
B_NoneType B_LogicalD___init__ (B_Logical W_self) {
    return B_None;
}
$WORD B_LogicalD___iand__ (B_Logical W_self, $WORD a, $WORD b) {
    return (($WORD (*) (B_Logical, $WORD, $WORD))W_self->$class->__and__)(W_self, a, b);
}
$WORD B_LogicalD___ior__ (B_Logical W_self, $WORD a, $WORD b) {
    return (($WORD (*) (B_Logical, $WORD, $WORD))W_self->$class->__or__)(W_self, a, b);
}
$WORD B_LogicalD___ixor__ (B_Logical W_self, $WORD a, $WORD b) {
    return (($WORD (*) (B_Logical, $WORD, $WORD))W_self->$class->__xor__)(W_self, a, b);
}
void B_LogicalD___serialize__ (B_Logical self, $Serial$state state) {
}
B_Logical B_LogicalD___deserialize__ (B_Logical self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_Logical));
            self->$class = &B_LogicalG_methods;
            return self;
        }
        self = $DNEW(B_Logical, state);
    }
    return self;
}
struct B_LogicalG_class B_LogicalG_methods;
B_NoneType B_PlusD___init__ (B_Plus W_self) {
    return B_None;
}
$WORD B_PlusD___iadd__ (B_Plus W_self, $WORD a, $WORD b) {
    return (($WORD (*) (B_Plus, $WORD, $WORD))W_self->$class->__add__)(W_self, a, b);
}
void B_PlusD___serialize__ (B_Plus self, $Serial$state state) {
}
B_Plus B_PlusD___deserialize__ (B_Plus self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_Plus));
            self->$class = &B_PlusG_methods;
            return self;
        }
        self = $DNEW(B_Plus, state);
    }
    return self;
}
struct B_PlusG_class B_PlusG_methods;
B_NoneType B_MinusD___init__ (B_Minus W_self) {
    return B_None;
}
$WORD B_MinusD___isub__ (B_Minus W_self, $WORD a, $WORD b) {
    return (($WORD (*) (B_Minus, $WORD, $WORD))W_self->$class->__sub__)(W_self, a, b);
}
void B_MinusD___serialize__ (B_Minus self, $Serial$state state) {
}
B_Minus B_MinusD___deserialize__ (B_Minus self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_Minus));
            self->$class = &B_MinusG_methods;
            return self;
        }
        self = $DNEW(B_Minus, state);
    }
    return self;
}
struct B_MinusG_class B_MinusG_methods;
B_NoneType B_TimesD___init__ (B_Times W_self) {
    ((B_NoneType (*) (B_Plus))B_PlusG_methods.__init__)(((B_Plus)W_self));
    return B_None;
}
$WORD B_TimesD___imul__ (B_Times W_self, $WORD a, $WORD b) {
    return (($WORD (*) (B_Times, $WORD, $WORD))W_self->$class->__mul__)(W_self, a, b);
}
void B_TimesD___serialize__ (B_Times self, $Serial$state state) {
}
B_Times B_TimesD___deserialize__ (B_Times self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_Times));
            self->$class = &B_TimesG_methods;
            return self;
        }
        self = $DNEW(B_Times, state);
    }
    return self;
}
struct B_TimesG_class B_TimesG_methods;
B_NoneType B_DivD___init__ (B_Div W_self) {
    return B_None;
}
$WORD B_DivD___itruediv__ (B_Div W_self, $WORD a, $WORD b) {
    return (($WORD (*) (B_Div, $WORD, $WORD))W_self->$class->__truediv__)(W_self, a, b);
}
void B_DivD___serialize__ (B_Div self, $Serial$state state) {
}
B_Div B_DivD___deserialize__ (B_Div self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_Div));
            self->$class = &B_DivG_methods;
            return self;
        }
        self = $DNEW(B_Div, state);
    }
    return self;
}
struct B_DivG_class B_DivG_methods;
B_NoneType B_HashableD___init__ (B_Hashable W_self) {
    ((B_NoneType (*) (B_Eq))B_EqG_methods.__init__)(((B_Eq)W_self));
    return B_None;
}
void B_HashableD___serialize__ (B_Hashable self, $Serial$state state) {
}
B_Hashable B_HashableD___deserialize__ (B_Hashable self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_Hashable));
            self->$class = &B_HashableG_methods;
            return self;
        }
        self = $DNEW(B_Hashable, state);
    }
    return self;
}
struct B_HashableG_class B_HashableG_methods;
/*
B_NoneType B_complexD___init__ (B_complex self, B_Number W_NumberE_604, $WORD val);
void B_complexD___serialize__ (B_complex self, $Serial$state state) {
}
B_complex B_complexD___deserialize__ (B_complex self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_complex));
            self->$class = &B_complexG_methods;
            return self;
        }
        self = $DNEW(B_complex, state);
    }
    return self;
}
B_complex B_complexG_new(B_Number G_1, $WORD G_2) {
    B_complex $tmp = malloc(sizeof(struct B_complex));
    $tmp->$class = &B_complexG_methods;
    B_complexG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
*/
struct B_complexG_class B_complexG_methods;
/*
B_NoneType B_dictD___init__ (B_dict self, B_Hashable W_HashableD_A, B_Iterable W_IterableE_609, $WORD iterable);
void B_dictD___serialize__ (B_dict self, $Serial$state state) {
}
B_dict B_dictD___deserialize__ (B_dict self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_dict));
            self->$class = &B_dictG_methods;
            return self;
        }
        self = $DNEW(B_dict, state);
    }
    return self;
}
B_dict B_dictG_new(B_Hashable G_1, B_Iterable G_2, $WORD G_3) {
    B_dict $tmp = malloc(sizeof(struct B_dict));
    $tmp->$class = &B_dictG_methods;
    B_dictG_methods.__init__($tmp, G_1, G_2, G_3);
    return $tmp;
}
*/
struct B_dictG_class B_dictG_methods;
/*
B_NoneType B_setD___init__ (B_set self, B_Hashable W_HashableD_A, B_Iterable W_IterableE_617, $WORD iterable);
void B_setD___serialize__ (B_set self, $Serial$state state) {
}
B_set B_setD___deserialize__ (B_set self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_set));
            self->$class = &B_setG_methods;
            return self;
        }
        self = $DNEW(B_set, state);
    }
    return self;
}
B_set B_setG_new(B_Hashable G_1, B_Iterable G_2, $WORD G_3) {
    B_set $tmp = malloc(sizeof(struct B_set));
    $tmp->$class = &B_setG_methods;
    B_setG_methods.__init__($tmp, G_1, G_2, G_3);
    return $tmp;
}
*/
struct B_setG_class B_setG_methods;
B_NoneType B_NumberD___init__ (B_Number W_self, B_Minus W_Minus) {
    ((B_NoneType (*) (B_Times))B_TimesG_methods.__init__)(((B_Times)W_self));
    W_self->W_Minus = W_Minus;
    return B_None;
}
$WORD B_NumberD___ipow__ (B_Number W_self, $WORD a, $WORD b) {
    return (($WORD (*) (B_Number, $WORD, $WORD))W_self->$class->__pow__)(W_self, a, b);
}
void B_NumberD___serialize__ (B_Number self, $Serial$state state) {
    $step_serialize(self->W_Minus, state);
}
B_Number B_NumberD___deserialize__ (B_Number self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_Number));
            self->$class = &B_NumberG_methods;
            return self;
        }
        self = $DNEW(B_Number, state);
    }
    self->W_Minus = $step_deserialize(state);
    return self;
}
struct B_NumberG_class B_NumberG_methods;
B_NoneType B_MinusD_NumberD___init__ (B_MinusD_Number W_self, B_Number W_Number) {
    ((B_NoneType (*) (B_Minus))B_MinusG_methods.__init__)(((B_Minus)W_self));
    W_self->W_Number = W_Number;
    return B_None;
}
void B_MinusD_NumberD___serialize__ (B_MinusD_Number self, $Serial$state state) {
    $step_serialize(self->W_Number, state);
}
B_MinusD_Number B_MinusD_NumberD___deserialize__ (B_MinusD_Number self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_MinusD_Number));
            self->$class = &B_MinusD_NumberG_methods;
            return self;
        }
        self = $DNEW(B_MinusD_Number, state);
    }
    self->W_Number = $step_deserialize(state);
    return self;
}
struct B_MinusD_NumberG_class B_MinusD_NumberG_methods;
B_NoneType B_RealD___init__ (B_Real W_self, B_Minus W_Minus) {
    ((B_NoneType (*) (B_Number, B_Minus))B_NumberG_methods.__init__)(((B_Number)W_self), W_Minus);
    return B_None;
}
void B_RealD___serialize__ (B_Real self, $Serial$state state) {
    $step_serialize(self->W_Minus, state);
}
B_Real B_RealD___deserialize__ (B_Real self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_Real));
            self->$class = &B_RealG_methods;
            return self;
        }
        self = $DNEW(B_Real, state);
    }
    self->W_Minus = $step_deserialize(state);
    return self;
}
struct B_RealG_class B_RealG_methods;
B_NoneType B_MinusD_RealD___init__ (B_MinusD_Real W_self, B_Real W_Real) {
    ((B_NoneType (*) (B_MinusD_Number, B_Number))B_MinusD_NumberG_methods.__init__)(((B_MinusD_Number)W_self), ((B_Number)W_Real));
    W_self->W_Real = W_Real;
    return B_None;
}
void B_MinusD_RealD___serialize__ (B_MinusD_Real self, $Serial$state state) {
    $step_serialize(self->W_Number, state);
    $step_serialize(self->W_Real, state);
}
B_MinusD_Real B_MinusD_RealD___deserialize__ (B_MinusD_Real self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_MinusD_Real));
            self->$class = &B_MinusD_RealG_methods;
            return self;
        }
        self = $DNEW(B_MinusD_Real, state);
    }
    self->W_Number = $step_deserialize(state);
    self->W_Real = $step_deserialize(state);
    return self;
}
struct B_MinusD_RealG_class B_MinusD_RealG_methods;
B_NoneType B_RealFloatD___init__ (B_RealFloat W_self, B_Minus W_Minus) {
    ((B_NoneType (*) (B_Real, B_Minus))B_RealG_methods.__init__)(((B_Real)W_self), W_Minus);
    return B_None;
}
void B_RealFloatD___serialize__ (B_RealFloat self, $Serial$state state) {
    $step_serialize(self->W_Minus, state);
}
B_RealFloat B_RealFloatD___deserialize__ (B_RealFloat self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_RealFloat));
            self->$class = &B_RealFloatG_methods;
            return self;
        }
        self = $DNEW(B_RealFloat, state);
    }
    self->W_Minus = $step_deserialize(state);
    return self;
}
struct B_RealFloatG_class B_RealFloatG_methods;
B_NoneType B_MinusD_RealFloatD___init__ (B_MinusD_RealFloat W_self, B_RealFloat W_RealFloat) {
    ((B_NoneType (*) (B_MinusD_Real, B_Real))B_MinusD_RealG_methods.__init__)(((B_MinusD_Real)W_self), ((B_Real)W_RealFloat));
    W_self->W_RealFloat = W_RealFloat;
    return B_None;
}
void B_MinusD_RealFloatD___serialize__ (B_MinusD_RealFloat self, $Serial$state state) {
    $step_serialize(self->W_Number, state);
    $step_serialize(self->W_Real, state);
    $step_serialize(self->W_RealFloat, state);
}
B_MinusD_RealFloat B_MinusD_RealFloatD___deserialize__ (B_MinusD_RealFloat self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_MinusD_RealFloat));
            self->$class = &B_MinusD_RealFloatG_methods;
            return self;
        }
        self = $DNEW(B_MinusD_RealFloat, state);
    }
    self->W_Number = $step_deserialize(state);
    self->W_Real = $step_deserialize(state);
    self->W_RealFloat = $step_deserialize(state);
    return self;
}
struct B_MinusD_RealFloatG_class B_MinusD_RealFloatG_methods;
B_NoneType B_RationalD___init__ (B_Rational W_self, B_Minus W_Minus) {
    ((B_NoneType (*) (B_Real, B_Minus))B_RealG_methods.__init__)(((B_Real)W_self), W_Minus);
    return B_None;
}
void B_RationalD___serialize__ (B_Rational self, $Serial$state state) {
    $step_serialize(self->W_Minus, state);
}
B_Rational B_RationalD___deserialize__ (B_Rational self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_Rational));
            self->$class = &B_RationalG_methods;
            return self;
        }
        self = $DNEW(B_Rational, state);
    }
    self->W_Minus = $step_deserialize(state);
    return self;
}
struct B_RationalG_class B_RationalG_methods;
B_NoneType B_MinusD_RationalD___init__ (B_MinusD_Rational W_self, B_Rational W_Rational) {
    ((B_NoneType (*) (B_MinusD_Real, B_Real))B_MinusD_RealG_methods.__init__)(((B_MinusD_Real)W_self), ((B_Real)W_Rational));
    W_self->W_Rational = W_Rational;
    return B_None;
}
void B_MinusD_RationalD___serialize__ (B_MinusD_Rational self, $Serial$state state) {
    $step_serialize(self->W_Number, state);
    $step_serialize(self->W_Real, state);
    $step_serialize(self->W_Rational, state);
}
B_MinusD_Rational B_MinusD_RationalD___deserialize__ (B_MinusD_Rational self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_MinusD_Rational));
            self->$class = &B_MinusD_RationalG_methods;
            return self;
        }
        self = $DNEW(B_MinusD_Rational, state);
    }
    self->W_Number = $step_deserialize(state);
    self->W_Real = $step_deserialize(state);
    self->W_Rational = $step_deserialize(state);
    return self;
}
struct B_MinusD_RationalG_class B_MinusD_RationalG_methods;
B_NoneType B_IntegralD___init__ (B_Integral W_self, B_Minus W_Minus, B_Logical W_Logical) {
    ((B_NoneType (*) (B_Rational, B_Minus))B_RationalG_methods.__init__)(((B_Rational)W_self), W_Minus);
    W_self->W_Logical = W_Logical;
    return B_None;
}
$WORD B_IntegralD___ifloordiv__ (B_Integral W_self, $WORD a, $WORD b) {
    return (($WORD (*) (B_Integral, $WORD, $WORD))W_self->$class->__floordiv__)(W_self, a, b);
}
$WORD B_IntegralD___imod__ (B_Integral W_self, $WORD a, $WORD b) {
    return (($WORD (*) (B_Integral, $WORD, $WORD))W_self->$class->__mod__)(W_self, a, b);
}
$WORD B_IntegralD___ilshift__ (B_Integral W_self, $WORD a, B_int b) {
    return (($WORD (*) (B_Integral, $WORD, B_int))W_self->$class->__lshift__)(W_self, a, b);
}
$WORD B_IntegralD___irshift__ (B_Integral W_self, $WORD a, B_int b) {
    return (($WORD (*) (B_Integral, $WORD, B_int))W_self->$class->__rshift__)(W_self, a, b);
}
void B_IntegralD___serialize__ (B_Integral self, $Serial$state state) {
    $step_serialize(self->W_Minus, state);
    $step_serialize(self->W_Logical, state);
}
B_Integral B_IntegralD___deserialize__ (B_Integral self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_Integral));
            self->$class = &B_IntegralG_methods;
            return self;
        }
        self = $DNEW(B_Integral, state);
    }
    self->W_Minus = $step_deserialize(state);
    self->W_Logical = $step_deserialize(state);
    return self;
}
struct B_IntegralG_class B_IntegralG_methods;
B_NoneType B_MinusD_IntegralD___init__ (B_MinusD_Integral W_self, B_Integral W_Integral) {
    ((B_NoneType (*) (B_MinusD_Rational, B_Rational))B_MinusD_RationalG_methods.__init__)(((B_MinusD_Rational)W_self), ((B_Rational)W_Integral));
    W_self->W_Integral = W_Integral;
    return B_None;
}
void B_MinusD_IntegralD___serialize__ (B_MinusD_Integral self, $Serial$state state) {
    $step_serialize(self->W_Number, state);
    $step_serialize(self->W_Real, state);
    $step_serialize(self->W_Rational, state);
    $step_serialize(self->W_Integral, state);
}
B_MinusD_Integral B_MinusD_IntegralD___deserialize__ (B_MinusD_Integral self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_MinusD_Integral));
            self->$class = &B_MinusD_IntegralG_methods;
            return self;
        }
        self = $DNEW(B_MinusD_Integral, state);
    }
    self->W_Number = $step_deserialize(state);
    self->W_Real = $step_deserialize(state);
    self->W_Rational = $step_deserialize(state);
    self->W_Integral = $step_deserialize(state);
    return self;
}
struct B_MinusD_IntegralG_class B_MinusD_IntegralG_methods;
B_NoneType B_LogicalD_IntegralD___init__ (B_LogicalD_Integral W_self, B_Integral W_Integral) {
    ((B_NoneType (*) (B_Logical))B_LogicalG_methods.__init__)(((B_Logical)W_self));
    W_self->W_Integral = W_Integral;
    return B_None;
}
void B_LogicalD_IntegralD___serialize__ (B_LogicalD_Integral self, $Serial$state state) {
    $step_serialize(self->W_Integral, state);
}
B_LogicalD_Integral B_LogicalD_IntegralD___deserialize__ (B_LogicalD_Integral self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_LogicalD_Integral));
            self->$class = &B_LogicalD_IntegralG_methods;
            return self;
        }
        self = $DNEW(B_LogicalD_Integral, state);
    }
    self->W_Integral = $step_deserialize(state);
    return self;
}
struct B_LogicalD_IntegralG_class B_LogicalD_IntegralG_methods;
B_NoneType B_HashableD_boolD___init__ (B_HashableD_bool W_self) {
    ((B_NoneType (*) (B_Hashable))B_HashableG_methods.__init__)(((B_Hashable)W_self));
    return B_None;
}
B_int B_HashableD_boolD___hash__ (B_HashableD_bool W_self, B_bool G_1p);
B_bool B_HashableD_boolD___eq__ (B_HashableD_bool W_self, B_bool G_1p, B_bool G_2p);
void B_HashableD_boolD___serialize__ (B_HashableD_bool self, $Serial$state state) {
}
B_HashableD_bool B_HashableD_boolD___deserialize__ (B_HashableD_bool self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_HashableD_bool));
            self->$class = &B_HashableD_boolG_methods;
            return self;
        }
        self = $DNEW(B_HashableD_bool, state);
    }
    return self;
}
B_HashableD_bool B_HashableD_boolG_new() {
    B_HashableD_bool $tmp = malloc(sizeof(struct B_HashableD_bool));
    $tmp->$class = &B_HashableD_boolG_methods;
    B_HashableD_boolG_methods.__init__($tmp);
    return $tmp;
}
struct B_HashableD_boolG_class B_HashableD_boolG_methods;
B_NoneType B_IntegralD_intD___init__ (B_IntegralD_int W_self) {
    ((B_NoneType (*) (B_Integral, B_Minus, B_Logical))B_IntegralG_methods.__init__)(((B_Integral)W_self), ((B_Minus)B_MinusD_IntegralD_intG_new(((B_Integral)W_self))), ((B_Logical)B_LogicalD_IntegralD_intG_new(((B_Integral)W_self))));
    return B_None;
}
B_int B_IntegralD_intD___invert__ (B_IntegralD_int W_self, B_int G_1p);
B_int B_IntegralD_intD___rshift__ (B_IntegralD_int W_self, B_int G_1p, B_int G_2p);
B_int B_IntegralD_intD___lshift__ (B_IntegralD_int W_self, B_int G_1p, B_int G_2p);
B_int B_IntegralD_intD___mod__ (B_IntegralD_int W_self, B_int G_1p, B_int G_2p);
B_int B_IntegralD_intD___floordiv__ (B_IntegralD_int W_self, B_int G_1p, B_int G_2p);
B_tuple B_IntegralD_intD___divmod__ (B_IntegralD_int W_self, B_int G_1p, B_int G_2p);
B_int B_IntegralD_intD___index__ (B_IntegralD_int W_self, B_int G_1p);
B_int B_IntegralD_intD___int__ (B_IntegralD_int W_self, B_int G_1p);
$WORD B_IntegralD_intD_denominator (B_IntegralD_int W_self, B_int G_1p, B_Integral W_IntegralE_638);
$WORD B_IntegralD_intD_numerator (B_IntegralD_int W_self, B_int G_1p, B_Integral W_IntegralE_637);
B_int B_IntegralD_intD___round__ (B_IntegralD_int W_self, B_int G_1p, B_int G_2p);
$WORD B_IntegralD_intD___ceil__ (B_IntegralD_int W_self, B_int G_1p, B_Integral W_IntegralE_636);
$WORD B_IntegralD_intD___floor__ (B_IntegralD_int W_self, B_int G_1p, B_Integral W_IntegralE_635);
$WORD B_IntegralD_intD___trunc__ (B_IntegralD_int W_self, B_int G_1p, B_Integral W_IntegralE_634);
B_float B_IntegralD_intD___float__ (B_IntegralD_int W_self, B_int G_1p);
B_int B_IntegralD_intD_conjugate (B_IntegralD_int W_self, B_int G_1p);
$WORD B_IntegralD_intD___abs__ (B_IntegralD_int W_self, B_int G_1p, B_Real W_RealE_633);
$WORD B_IntegralD_intD_imag (B_IntegralD_int W_self, B_int G_1p, B_Real W_RealE_632);
$WORD B_IntegralD_intD_real (B_IntegralD_int W_self, B_int G_1p, B_Real W_RealE_631);
B_int B_IntegralD_intD___pos__ (B_IntegralD_int W_self, B_int G_1p);
B_int B_IntegralD_intD___neg__ (B_IntegralD_int W_self, B_int G_1p);
B_int B_IntegralD_intD___pow__ (B_IntegralD_int W_self, B_int G_1p, B_int G_2p);
B_complex B_IntegralD_intD___complex__ (B_IntegralD_int W_self, B_int G_1p);
B_int B_IntegralD_intD___fromatom__ (B_IntegralD_int W_self, B_atom G_1p);
B_int B_IntegralD_intD___mul__ (B_IntegralD_int W_self, B_int G_1p, B_int G_2p);
B_int B_IntegralD_intD___add__ (B_IntegralD_int W_self, B_int G_1p, B_int G_2p);
void B_IntegralD_intD___serialize__ (B_IntegralD_int self, $Serial$state state) {
    $step_serialize(self->W_Minus, state);
    $step_serialize(self->W_Logical, state);
}
B_IntegralD_int B_IntegralD_intD___deserialize__ (B_IntegralD_int self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_IntegralD_int));
            self->$class = &B_IntegralD_intG_methods;
            return self;
        }
        self = $DNEW(B_IntegralD_int, state);
    }
    self->W_Minus = $step_deserialize(state);
    self->W_Logical = $step_deserialize(state);
    return self;
}
B_IntegralD_int B_IntegralD_intG_new() {
    B_IntegralD_int $tmp = malloc(sizeof(struct B_IntegralD_int));
    $tmp->$class = &B_IntegralD_intG_methods;
    B_IntegralD_intG_methods.__init__($tmp);
    return $tmp;
}
struct B_IntegralD_intG_class B_IntegralD_intG_methods;
B_NoneType B_MinusD_IntegralD_intD___init__ (B_MinusD_IntegralD_int W_self, B_Integral W_Integral) {
    ((B_NoneType (*) (B_MinusD_Integral, B_Integral))B_MinusD_IntegralG_methods.__init__)(((B_MinusD_Integral)W_self), ((B_Integral)W_Integral));
    return B_None;
}
B_int B_MinusD_IntegralD_intD___sub__ (B_MinusD_IntegralD_int W_self, B_int G_1p, B_int G_2p);
void B_MinusD_IntegralD_intD___serialize__ (B_MinusD_IntegralD_int self, $Serial$state state) {
    $step_serialize(self->W_Number, state);
    $step_serialize(self->W_Real, state);
    $step_serialize(self->W_Rational, state);
    $step_serialize(self->W_Integral, state);
}
B_MinusD_IntegralD_int B_MinusD_IntegralD_intD___deserialize__ (B_MinusD_IntegralD_int self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_MinusD_IntegralD_int));
            self->$class = &B_MinusD_IntegralD_intG_methods;
            return self;
        }
        self = $DNEW(B_MinusD_IntegralD_int, state);
    }
    self->W_Number = $step_deserialize(state);
    self->W_Real = $step_deserialize(state);
    self->W_Rational = $step_deserialize(state);
    self->W_Integral = $step_deserialize(state);
    return self;
}
B_MinusD_IntegralD_int B_MinusD_IntegralD_intG_new(B_Integral G_1) {
    B_MinusD_IntegralD_int $tmp = malloc(sizeof(struct B_MinusD_IntegralD_int));
    $tmp->$class = &B_MinusD_IntegralD_intG_methods;
    B_MinusD_IntegralD_intG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct B_MinusD_IntegralD_intG_class B_MinusD_IntegralD_intG_methods;
B_NoneType B_LogicalD_IntegralD_intD___init__ (B_LogicalD_IntegralD_int W_self, B_Integral W_Integral) {
    ((B_NoneType (*) (B_LogicalD_Integral, B_Integral))B_LogicalD_IntegralG_methods.__init__)(((B_LogicalD_Integral)W_self), ((B_Integral)W_Integral));
    return B_None;
}
B_int B_LogicalD_IntegralD_intD___xor__ (B_LogicalD_IntegralD_int W_self, B_int G_1p, B_int G_2p);
B_int B_LogicalD_IntegralD_intD___or__ (B_LogicalD_IntegralD_int W_self, B_int G_1p, B_int G_2p);
B_int B_LogicalD_IntegralD_intD___and__ (B_LogicalD_IntegralD_int W_self, B_int G_1p, B_int G_2p);
void B_LogicalD_IntegralD_intD___serialize__ (B_LogicalD_IntegralD_int self, $Serial$state state) {
    $step_serialize(self->W_Integral, state);
}
B_LogicalD_IntegralD_int B_LogicalD_IntegralD_intD___deserialize__ (B_LogicalD_IntegralD_int self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_LogicalD_IntegralD_int));
            self->$class = &B_LogicalD_IntegralD_intG_methods;
            return self;
        }
        self = $DNEW(B_LogicalD_IntegralD_int, state);
    }
    self->W_Integral = $step_deserialize(state);
    return self;
}
B_LogicalD_IntegralD_int B_LogicalD_IntegralD_intG_new(B_Integral G_1) {
    B_LogicalD_IntegralD_int $tmp = malloc(sizeof(struct B_LogicalD_IntegralD_int));
    $tmp->$class = &B_LogicalD_IntegralD_intG_methods;
    B_LogicalD_IntegralD_intG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct B_LogicalD_IntegralD_intG_class B_LogicalD_IntegralD_intG_methods;
B_NoneType B_DivD_intD___init__ (B_DivD_int W_self) {
    ((B_NoneType (*) (B_Div))B_DivG_methods.__init__)(((B_Div)W_self));
    return B_None;
}
B_float B_DivD_intD___truediv__ (B_DivD_int W_self, B_int G_1p, B_int G_2p);
void B_DivD_intD___serialize__ (B_DivD_int self, $Serial$state state) {
}
B_DivD_int B_DivD_intD___deserialize__ (B_DivD_int self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_DivD_int));
            self->$class = &B_DivD_intG_methods;
            return self;
        }
        self = $DNEW(B_DivD_int, state);
    }
    return self;
}
B_DivD_int B_DivD_intG_new() {
    B_DivD_int $tmp = malloc(sizeof(struct B_DivD_int));
    $tmp->$class = &B_DivD_intG_methods;
    B_DivD_intG_methods.__init__($tmp);
    return $tmp;
}
struct B_DivD_intG_class B_DivD_intG_methods;
B_NoneType B_OrdD_intD___init__ (B_OrdD_int W_self) {
    ((B_NoneType (*) (B_Ord))B_OrdG_methods.__init__)(((B_Ord)W_self));
    return B_None;
}
B_bool B_OrdD_intD___lt__ (B_OrdD_int W_self, B_int G_1p, B_int G_2p);
B_bool B_OrdD_intD___eq__ (B_OrdD_int W_self, B_int G_1p, B_int G_2p);
void B_OrdD_intD___serialize__ (B_OrdD_int self, $Serial$state state) {
}
B_OrdD_int B_OrdD_intD___deserialize__ (B_OrdD_int self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_OrdD_int));
            self->$class = &B_OrdD_intG_methods;
            return self;
        }
        self = $DNEW(B_OrdD_int, state);
    }
    return self;
}
B_OrdD_int B_OrdD_intG_new() {
    B_OrdD_int $tmp = malloc(sizeof(struct B_OrdD_int));
    $tmp->$class = &B_OrdD_intG_methods;
    B_OrdD_intG_methods.__init__($tmp);
    return $tmp;
}
struct B_OrdD_intG_class B_OrdD_intG_methods;
B_NoneType B_HashableD_intD___init__ (B_HashableD_int W_self) {
    ((B_NoneType (*) (B_Hashable))B_HashableG_methods.__init__)(((B_Hashable)W_self));
    return B_None;
}
B_int B_HashableD_intD___hash__ (B_HashableD_int W_self, B_int G_1p);
void B_HashableD_intD___serialize__ (B_HashableD_int self, $Serial$state state) {
}
B_HashableD_int B_HashableD_intD___deserialize__ (B_HashableD_int self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_HashableD_int));
            self->$class = &B_HashableD_intG_methods;
            return self;
        }
        self = $DNEW(B_HashableD_int, state);
    }
    return self;
}
B_HashableD_int B_HashableD_intG_new() {   // manually added
    return $NEW(B_HashableD_int);          //
}                                          //
struct B_HashableD_intG_class B_HashableD_intG_methods;
B_NoneType B_IntegralD_i64D___init__ (B_IntegralD_i64 W_self) {
    ((B_NoneType (*) (B_Integral, B_Minus, B_Logical))B_IntegralG_methods.__init__)(((B_Integral)W_self), ((B_Minus)B_MinusD_IntegralD_i64G_new(((B_Integral)W_self))), ((B_Logical)B_LogicalD_IntegralD_i64G_new(((B_Integral)W_self))));
    return B_None;
}
B_i64 B_IntegralD_i64D___invert__ (B_IntegralD_i64 W_self, B_i64 G_1p);
B_i64 B_IntegralD_i64D___rshift__ (B_IntegralD_i64 W_self, B_i64 G_1p, B_int G_2p);
B_i64 B_IntegralD_i64D___lshift__ (B_IntegralD_i64 W_self, B_i64 G_1p, B_int G_2p);
B_i64 B_IntegralD_i64D___mod__ (B_IntegralD_i64 W_self, B_i64 G_1p, B_i64 G_2p);
B_i64 B_IntegralD_i64D___floordiv__ (B_IntegralD_i64 W_self, B_i64 G_1p, B_i64 G_2p);
B_tuple B_IntegralD_i64D___divmod__ (B_IntegralD_i64 W_self, B_i64 G_1p, B_i64 G_2p);
B_int B_IntegralD_i64D___index__ (B_IntegralD_i64 W_self, B_i64 G_1p);
B_int B_IntegralD_i64D___int__ (B_IntegralD_i64 W_self, B_i64 G_1p);
$WORD B_IntegralD_i64D_denominator (B_IntegralD_i64 W_self, B_i64 G_1p, B_Integral W_IntegralE_638);
$WORD B_IntegralD_i64D_numerator (B_IntegralD_i64 W_self, B_i64 G_1p, B_Integral W_IntegralE_637);
B_i64 B_IntegralD_i64D___round__ (B_IntegralD_i64 W_self, B_i64 G_1p, B_int G_2p);
$WORD B_IntegralD_i64D___ceil__ (B_IntegralD_i64 W_self, B_i64 G_1p, B_Integral W_IntegralE_636);
$WORD B_IntegralD_i64D___floor__ (B_IntegralD_i64 W_self, B_i64 G_1p, B_Integral W_IntegralE_635);
$WORD B_IntegralD_i64D___trunc__ (B_IntegralD_i64 W_self, B_i64 G_1p, B_Integral W_IntegralE_634);
B_float B_IntegralD_i64D___float__ (B_IntegralD_i64 W_self, B_i64 G_1p);
B_i64 B_IntegralD_i64D_conjugate (B_IntegralD_i64 W_self, B_i64 G_1p);
$WORD B_IntegralD_i64D___abs__ (B_IntegralD_i64 W_self, B_i64 G_1p, B_Real W_RealE_633);
$WORD B_IntegralD_i64D_imag (B_IntegralD_i64 W_self, B_i64 G_1p, B_Real W_RealE_632);
$WORD B_IntegralD_i64D_real (B_IntegralD_i64 W_self, B_i64 G_1p, B_Real W_RealE_631);
B_i64 B_IntegralD_i64D___pos__ (B_IntegralD_i64 W_self, B_i64 G_1p);
B_i64 B_IntegralD_i64D___neg__ (B_IntegralD_i64 W_self, B_i64 G_1p);
B_i64 B_IntegralD_i64D___pow__ (B_IntegralD_i64 W_self, B_i64 G_1p, B_i64 G_2p);
B_complex B_IntegralD_i64D___complex__ (B_IntegralD_i64 W_self, B_i64 G_1p);
B_i64 B_IntegralD_i64D___fromatom__ (B_IntegralD_i64 W_self, B_atom G_1p);
B_i64 B_IntegralD_i64D___mul__ (B_IntegralD_i64 W_self, B_i64 G_1p, B_i64 G_2p);
B_i64 B_IntegralD_i64D___add__ (B_IntegralD_i64 W_self, B_i64 G_1p, B_i64 G_2p);
void B_IntegralD_i64D___serialize__ (B_IntegralD_i64 self, $Serial$state state) {
    $step_serialize(self->W_Minus, state);
    $step_serialize(self->W_Logical, state);
}
B_IntegralD_i64 B_IntegralD_i64D___deserialize__ (B_IntegralD_i64 self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_IntegralD_i64));
            self->$class = &B_IntegralD_i64G_methods;
            return self;
        }
        self = $DNEW(B_IntegralD_i64, state);
    }
    self->W_Minus = $step_deserialize(state);
    self->W_Logical = $step_deserialize(state);
    return self;
}
B_IntegralD_i64 B_IntegralD_i64G_new() {
    B_IntegralD_i64 $tmp = malloc(sizeof(struct B_IntegralD_i64));
    $tmp->$class = &B_IntegralD_i64G_methods;
    B_IntegralD_i64G_methods.__init__($tmp);
    return $tmp;
}
struct B_IntegralD_i64G_class B_IntegralD_i64G_methods;
B_NoneType B_MinusD_IntegralD_i64D___init__ (B_MinusD_IntegralD_i64 W_self, B_Integral W_Integral) {
    ((B_NoneType (*) (B_MinusD_Integral, B_Integral))B_MinusD_IntegralG_methods.__init__)(((B_MinusD_Integral)W_self), ((B_Integral)W_Integral));
    return B_None;
}
B_i64 B_MinusD_IntegralD_i64D___sub__ (B_MinusD_IntegralD_i64 W_self, B_i64 G_1p, B_i64 G_2p);
void B_MinusD_IntegralD_i64D___serialize__ (B_MinusD_IntegralD_i64 self, $Serial$state state) {
    $step_serialize(self->W_Number, state);
    $step_serialize(self->W_Real, state);
    $step_serialize(self->W_Rational, state);
    $step_serialize(self->W_Integral, state);
}
B_MinusD_IntegralD_i64 B_MinusD_IntegralD_i64D___deserialize__ (B_MinusD_IntegralD_i64 self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_MinusD_IntegralD_i64));
            self->$class = &B_MinusD_IntegralD_i64G_methods;
            return self;
        }
        self = $DNEW(B_MinusD_IntegralD_i64, state);
    }
    self->W_Number = $step_deserialize(state);
    self->W_Real = $step_deserialize(state);
    self->W_Rational = $step_deserialize(state);
    self->W_Integral = $step_deserialize(state);
    return self;
}
B_MinusD_IntegralD_i64 B_MinusD_IntegralD_i64G_new(B_Integral G_1) {
    B_MinusD_IntegralD_i64 $tmp = malloc(sizeof(struct B_MinusD_IntegralD_i64));
    $tmp->$class = &B_MinusD_IntegralD_i64G_methods;
    B_MinusD_IntegralD_i64G_methods.__init__($tmp, G_1);
    return $tmp;
}
struct B_MinusD_IntegralD_i64G_class B_MinusD_IntegralD_i64G_methods;
B_NoneType B_LogicalD_IntegralD_i64D___init__ (B_LogicalD_IntegralD_i64 W_self, B_Integral W_Integral) {
    ((B_NoneType (*) (B_LogicalD_Integral, B_Integral))B_LogicalD_IntegralG_methods.__init__)(((B_LogicalD_Integral)W_self), ((B_Integral)W_Integral));
    return B_None;
}
B_i64 B_LogicalD_IntegralD_i64D___xor__ (B_LogicalD_IntegralD_i64 W_self, B_i64 G_1p, B_i64 G_2p);
B_i64 B_LogicalD_IntegralD_i64D___or__ (B_LogicalD_IntegralD_i64 W_self, B_i64 G_1p, B_i64 G_2p);
B_i64 B_LogicalD_IntegralD_i64D___and__ (B_LogicalD_IntegralD_i64 W_self, B_i64 G_1p, B_i64 G_2p);
void B_LogicalD_IntegralD_i64D___serialize__ (B_LogicalD_IntegralD_i64 self, $Serial$state state) {
    $step_serialize(self->W_Integral, state);
}
B_LogicalD_IntegralD_i64 B_LogicalD_IntegralD_i64D___deserialize__ (B_LogicalD_IntegralD_i64 self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_LogicalD_IntegralD_i64));
            self->$class = &B_LogicalD_IntegralD_i64G_methods;
            return self;
        }
        self = $DNEW(B_LogicalD_IntegralD_i64, state);
    }
    self->W_Integral = $step_deserialize(state);
    return self;
}
B_LogicalD_IntegralD_i64 B_LogicalD_IntegralD_i64G_new(B_Integral G_1) {
    B_LogicalD_IntegralD_i64 $tmp = malloc(sizeof(struct B_LogicalD_IntegralD_i64));
    $tmp->$class = &B_LogicalD_IntegralD_i64G_methods;
    B_LogicalD_IntegralD_i64G_methods.__init__($tmp, G_1);
    return $tmp;
}
struct B_LogicalD_IntegralD_i64G_class B_LogicalD_IntegralD_i64G_methods;
B_NoneType B_DivD_i64D___init__ (B_DivD_i64 W_self) {
    ((B_NoneType (*) (B_Div))B_DivG_methods.__init__)(((B_Div)W_self));
    return B_None;
}
B_float B_DivD_i64D___truediv__ (B_DivD_i64 W_self, B_i64 G_1p, B_i64 G_2p);
void B_DivD_i64D___serialize__ (B_DivD_i64 self, $Serial$state state) {
}
B_DivD_i64 B_DivD_i64D___deserialize__ (B_DivD_i64 self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_DivD_i64));
            self->$class = &B_DivD_i64G_methods;
            return self;
        }
        self = $DNEW(B_DivD_i64, state);
    }
    return self;
}
B_DivD_i64 B_DivD_i64G_new() {
    B_DivD_i64 $tmp = malloc(sizeof(struct B_DivD_i64));
    $tmp->$class = &B_DivD_i64G_methods;
    B_DivD_i64G_methods.__init__($tmp);
    return $tmp;
}
struct B_DivD_i64G_class B_DivD_i64G_methods;
B_NoneType B_OrdD_i64D___init__ (B_OrdD_i64 W_self) {
    ((B_NoneType (*) (B_Ord))B_OrdG_methods.__init__)(((B_Ord)W_self));
    return B_None;
}
B_bool B_OrdD_i64D___lt__ (B_OrdD_i64 W_self, B_i64 G_1p, B_i64 G_2p);
B_bool B_OrdD_i64D___eq__ (B_OrdD_i64 W_self, B_i64 G_1p, B_i64 G_2p);
void B_OrdD_i64D___serialize__ (B_OrdD_i64 self, $Serial$state state) {
}
B_OrdD_i64 B_OrdD_i64D___deserialize__ (B_OrdD_i64 self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_OrdD_i64));
            self->$class = &B_OrdD_i64G_methods;
            return self;
        }
        self = $DNEW(B_OrdD_i64, state);
    }
    return self;
}
B_OrdD_i64 B_OrdD_i64G_new() {
    B_OrdD_i64 $tmp = malloc(sizeof(struct B_OrdD_i64));
    $tmp->$class = &B_OrdD_i64G_methods;
    B_OrdD_i64G_methods.__init__($tmp);
    return $tmp;
}
struct B_OrdD_i64G_class B_OrdD_i64G_methods;
B_NoneType B_HashableD_i64D___init__ (B_HashableD_i64 W_self) {
    ((B_NoneType (*) (B_Hashable))B_HashableG_methods.__init__)(((B_Hashable)W_self));
    return B_None;
}
B_int B_HashableD_i64D___hash__ (B_HashableD_i64 W_self, B_i64 G_1p);
void B_HashableD_i64D___serialize__ (B_HashableD_i64 self, $Serial$state state) {
}
B_HashableD_i64 B_HashableD_i64D___deserialize__ (B_HashableD_i64 self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_HashableD_i64));
            self->$class = &B_HashableD_i64G_methods;
            return self;
        }
        self = $DNEW(B_HashableD_i64, state);
    }
    return self;
}
struct B_HashableD_i64G_class B_HashableD_i64G_methods;
B_NoneType B_RealFloatD_floatD___init__ (B_RealFloatD_float W_self) {
    ((B_NoneType (*) (B_RealFloat, B_Minus))B_RealFloatG_methods.__init__)(((B_RealFloat)W_self), ((B_Minus)B_MinusD_RealFloatD_floatG_new(((B_RealFloat)W_self))));
    return B_None;
}
B_float B_RealFloatD_floatD___round__ (B_RealFloatD_float W_self, B_float G_1p, B_int G_2p);
$WORD B_RealFloatD_floatD___ceil__ (B_RealFloatD_float W_self, B_float G_1p, B_Integral W_IntegralE_636);
$WORD B_RealFloatD_floatD___floor__ (B_RealFloatD_float W_self, B_float G_1p, B_Integral W_IntegralE_635);
$WORD B_RealFloatD_floatD___trunc__ (B_RealFloatD_float W_self, B_float G_1p, B_Integral W_IntegralE_634);
B_float B_RealFloatD_floatD___float__ (B_RealFloatD_float W_self, B_float G_1p);
B_float B_RealFloatD_floatD_conjugate (B_RealFloatD_float W_self, B_float G_1p);
$WORD B_RealFloatD_floatD___abs__ (B_RealFloatD_float W_self, B_float G_1p, B_Real W_RealE_633);
$WORD B_RealFloatD_floatD_imag (B_RealFloatD_float W_self, B_float G_1p, B_Real W_RealE_632);
$WORD B_RealFloatD_floatD_real (B_RealFloatD_float W_self, B_float G_1p, B_Real W_RealE_631);
B_float B_RealFloatD_floatD___pos__ (B_RealFloatD_float W_self, B_float G_1p);
B_float B_RealFloatD_floatD___neg__ (B_RealFloatD_float W_self, B_float G_1p);
B_float B_RealFloatD_floatD___pow__ (B_RealFloatD_float W_self, B_float G_1p, B_float G_2p);
B_complex B_RealFloatD_floatD___complex__ (B_RealFloatD_float W_self, B_float G_1p);
B_float B_RealFloatD_floatD___fromatom__ (B_RealFloatD_float W_self, B_atom G_1p);
B_float B_RealFloatD_floatD___mul__ (B_RealFloatD_float W_self, B_float G_1p, B_float G_2p);
B_float B_RealFloatD_floatD___add__ (B_RealFloatD_float W_self, B_float G_1p, B_float G_2p);
void B_RealFloatD_floatD___serialize__ (B_RealFloatD_float self, $Serial$state state) {
    $step_serialize(self->W_Minus, state);
}
B_RealFloatD_float B_RealFloatD_floatD___deserialize__ (B_RealFloatD_float self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_RealFloatD_float));
            self->$class = &B_RealFloatD_floatG_methods;
            return self;
        }
        self = $DNEW(B_RealFloatD_float, state);
    }
    self->W_Minus = $step_deserialize(state);
    return self;
}
B_RealFloatD_float B_RealFloatD_floatG_new() {
    B_RealFloatD_float $tmp = malloc(sizeof(struct B_RealFloatD_float));
    $tmp->$class = &B_RealFloatD_floatG_methods;
    B_RealFloatD_floatG_methods.__init__($tmp);
    return $tmp;
}
struct B_RealFloatD_floatG_class B_RealFloatD_floatG_methods;
B_NoneType B_MinusD_RealFloatD_floatD___init__ (B_MinusD_RealFloatD_float W_self, B_RealFloat W_RealFloat) {
    ((B_NoneType (*) (B_MinusD_RealFloat, B_RealFloat))B_MinusD_RealFloatG_methods.__init__)(((B_MinusD_RealFloat)W_self), ((B_RealFloat)W_RealFloat));
    return B_None;
}
B_float B_MinusD_RealFloatD_floatD___sub__ (B_MinusD_RealFloatD_float W_self, B_float G_1p, B_float G_2p);
void B_MinusD_RealFloatD_floatD___serialize__ (B_MinusD_RealFloatD_float self, $Serial$state state) {
    $step_serialize(self->W_Number, state);
    $step_serialize(self->W_Real, state);
    $step_serialize(self->W_RealFloat, state);
}
B_MinusD_RealFloatD_float B_MinusD_RealFloatD_floatD___deserialize__ (B_MinusD_RealFloatD_float self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_MinusD_RealFloatD_float));
            self->$class = &B_MinusD_RealFloatD_floatG_methods;
            return self;
        }
        self = $DNEW(B_MinusD_RealFloatD_float, state);
    }
    self->W_Number = $step_deserialize(state);
    self->W_Real = $step_deserialize(state);
    self->W_RealFloat = $step_deserialize(state);
    return self;
}
B_MinusD_RealFloatD_float B_MinusD_RealFloatD_floatG_new(B_RealFloat G_1) {
    B_MinusD_RealFloatD_float $tmp = malloc(sizeof(struct B_MinusD_RealFloatD_float));
    $tmp->$class = &B_MinusD_RealFloatD_floatG_methods;
    B_MinusD_RealFloatD_floatG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct B_MinusD_RealFloatD_floatG_class B_MinusD_RealFloatD_floatG_methods;
B_NoneType B_DivD_floatD___init__ (B_DivD_float W_self) {
    ((B_NoneType (*) (B_Div))B_DivG_methods.__init__)(((B_Div)W_self));
    return B_None;
}
B_float B_DivD_floatD___truediv__ (B_DivD_float W_self, B_float G_1p, B_float G_2p);
void B_DivD_floatD___serialize__ (B_DivD_float self, $Serial$state state) {
}
B_DivD_float B_DivD_floatD___deserialize__ (B_DivD_float self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_DivD_float));
            self->$class = &B_DivD_floatG_methods;
            return self;
        }
        self = $DNEW(B_DivD_float, state);
    }
    return self;
}
B_DivD_float B_DivD_floatG_new() {
    B_DivD_float $tmp = malloc(sizeof(struct B_DivD_float));
    $tmp->$class = &B_DivD_floatG_methods;
    B_DivD_floatG_methods.__init__($tmp);
    return $tmp;
}
struct B_DivD_floatG_class B_DivD_floatG_methods;
B_NoneType B_OrdD_floatD___init__ (B_OrdD_float W_self) {
    ((B_NoneType (*) (B_Ord))B_OrdG_methods.__init__)(((B_Ord)W_self));
    return B_None;
}
B_bool B_OrdD_floatD___lt__ (B_OrdD_float W_self, B_float G_1p, B_float G_2p);
B_bool B_OrdD_floatD___eq__ (B_OrdD_float W_self, B_float G_1p, B_float G_2p);
void B_OrdD_floatD___serialize__ (B_OrdD_float self, $Serial$state state) {
}
B_OrdD_float B_OrdD_floatD___deserialize__ (B_OrdD_float self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_OrdD_float));
            self->$class = &B_OrdD_floatG_methods;
            return self;
        }
        self = $DNEW(B_OrdD_float, state);
    }
    return self;
}
B_OrdD_float B_OrdD_floatG_new() {
    B_OrdD_float $tmp = malloc(sizeof(struct B_OrdD_float));
    $tmp->$class = &B_OrdD_floatG_methods;
    B_OrdD_floatG_methods.__init__($tmp);
    return $tmp;
}
struct B_OrdD_floatG_class B_OrdD_floatG_methods;
B_NoneType B_HashableD_floatD___init__ (B_HashableD_float W_self) {
    ((B_NoneType (*) (B_Hashable))B_HashableG_methods.__init__)(((B_Hashable)W_self));
    return B_None;
}
B_int B_HashableD_floatD___hash__ (B_HashableD_float W_self, B_float G_1p);
void B_HashableD_floatD___serialize__ (B_HashableD_float self, $Serial$state state) {
}
B_HashableD_float B_HashableD_floatD___deserialize__ (B_HashableD_float self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_HashableD_float));
            self->$class = &B_HashableD_floatG_methods;
            return self;
        }
        self = $DNEW(B_HashableD_float, state);
    }
    return self;
}
struct B_HashableD_floatG_class B_HashableD_floatG_methods;
B_NoneType B_NumberD_complexD___init__ (B_NumberD_complex W_self) {
    ((B_NoneType (*) (B_Number, B_Minus))B_NumberG_methods.__init__)(((B_Number)W_self), ((B_Minus)B_MinusD_NumberD_complexG_new(((B_Number)W_self))));
    return B_None;
}
B_complex B_NumberD_complexD_conjugate (B_NumberD_complex W_self, B_complex G_1p);
$WORD B_NumberD_complexD___abs__ (B_NumberD_complex W_self, B_complex G_1p, B_Real W_RealE_633);
$WORD B_NumberD_complexD_imag (B_NumberD_complex W_self, B_complex G_1p, B_Real W_RealE_632);
$WORD B_NumberD_complexD_real (B_NumberD_complex W_self, B_complex G_1p, B_Real W_RealE_631);
B_complex B_NumberD_complexD___pos__ (B_NumberD_complex W_self, B_complex G_1p);
B_complex B_NumberD_complexD___neg__ (B_NumberD_complex W_self, B_complex G_1p);
B_complex B_NumberD_complexD___pow__ (B_NumberD_complex W_self, B_complex G_1p, B_complex G_2p);
B_complex B_NumberD_complexD___complex__ (B_NumberD_complex W_self, B_complex G_1p);
B_complex B_NumberD_complexD___fromatom__ (B_NumberD_complex W_self, B_atom G_1p);
B_complex B_NumberD_complexD___mul__ (B_NumberD_complex W_self, B_complex G_1p, B_complex G_2p);
B_complex B_NumberD_complexD___add__ (B_NumberD_complex W_self, B_complex G_1p, B_complex G_2p);
void B_NumberD_complexD___serialize__ (B_NumberD_complex self, $Serial$state state) {
    $step_serialize(self->W_Minus, state);
}
B_NumberD_complex B_NumberD_complexD___deserialize__ (B_NumberD_complex self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_NumberD_complex));
            self->$class = &B_NumberD_complexG_methods;
            return self;
        }
        self = $DNEW(B_NumberD_complex, state);
    }
    self->W_Minus = $step_deserialize(state);
    return self;
}
B_NumberD_complex B_NumberD_complexG_new() {
    B_NumberD_complex $tmp = malloc(sizeof(struct B_NumberD_complex));
    $tmp->$class = &B_NumberD_complexG_methods;
    B_NumberD_complexG_methods.__init__($tmp);
    return $tmp;
}
struct B_NumberD_complexG_class B_NumberD_complexG_methods;
B_NoneType B_MinusD_NumberD_complexD___init__ (B_MinusD_NumberD_complex W_self, B_Number W_Number) {
    ((B_NoneType (*) (B_MinusD_Number, B_Number))B_MinusD_NumberG_methods.__init__)(((B_MinusD_Number)W_self), ((B_Number)W_Number));
    return B_None;
}
B_complex B_MinusD_NumberD_complexD___sub__ (B_MinusD_NumberD_complex W_self, B_complex G_1p, B_complex G_2p);
void B_MinusD_NumberD_complexD___serialize__ (B_MinusD_NumberD_complex self, $Serial$state state) {
    $step_serialize(self->W_Number, state);
}
B_MinusD_NumberD_complex B_MinusD_NumberD_complexD___deserialize__ (B_MinusD_NumberD_complex self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_MinusD_NumberD_complex));
            self->$class = &B_MinusD_NumberD_complexG_methods;
            return self;
        }
        self = $DNEW(B_MinusD_NumberD_complex, state);
    }
    self->W_Number = $step_deserialize(state);
    return self;
}
B_MinusD_NumberD_complex B_MinusD_NumberD_complexG_new(B_Number G_1) {
    B_MinusD_NumberD_complex $tmp = malloc(sizeof(struct B_MinusD_NumberD_complex));
    $tmp->$class = &B_MinusD_NumberD_complexG_methods;
    B_MinusD_NumberD_complexG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct B_MinusD_NumberD_complexG_class B_MinusD_NumberD_complexG_methods;
B_NoneType B_DivD_complexD___init__ (B_DivD_complex W_self) {
    ((B_NoneType (*) (B_Div))B_DivG_methods.__init__)(((B_Div)W_self));
    return B_None;
}
B_complex B_DivD_complexD___truediv__ (B_DivD_complex W_self, B_complex G_1p, B_complex G_2p);
void B_DivD_complexD___serialize__ (B_DivD_complex self, $Serial$state state) {
}
B_DivD_complex B_DivD_complexD___deserialize__ (B_DivD_complex self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_DivD_complex));
            self->$class = &B_DivD_complexG_methods;
            return self;
        }
        self = $DNEW(B_DivD_complex, state);
    }
    return self;
}
B_DivD_complex B_DivD_complexG_new() {
    B_DivD_complex $tmp = malloc(sizeof(struct B_DivD_complex));
    $tmp->$class = &B_DivD_complexG_methods;
    B_DivD_complexG_methods.__init__($tmp);
    return $tmp;
}
struct B_DivD_complexG_class B_DivD_complexG_methods;
B_NoneType B_EqD_complexD___init__ (B_EqD_complex W_self) {
    ((B_NoneType (*) (B_Eq))B_EqG_methods.__init__)(((B_Eq)W_self));
    return B_None;
}
B_bool B_EqD_complexD___eq__ (B_EqD_complex W_self, B_complex G_1p, B_complex G_2p);
void B_EqD_complexD___serialize__ (B_EqD_complex self, $Serial$state state) {
}
B_EqD_complex B_EqD_complexD___deserialize__ (B_EqD_complex self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_EqD_complex));
            self->$class = &B_EqD_complexG_methods;
            return self;
        }
        self = $DNEW(B_EqD_complex, state);
    }
    return self;
}
B_EqD_complex B_EqD_complexG_new() {
    B_EqD_complex $tmp = malloc(sizeof(struct B_EqD_complex));
    $tmp->$class = &B_EqD_complexG_methods;
    B_EqD_complexG_methods.__init__($tmp);
    return $tmp;
}
struct B_EqD_complexG_class B_EqD_complexG_methods;
B_NoneType B_HashableD_complexD___init__ (B_HashableD_complex W_self) {
    ((B_NoneType (*) (B_Hashable))B_HashableG_methods.__init__)(((B_Hashable)W_self));
    return B_None;
}
B_int B_HashableD_complexD___hash__ (B_HashableD_complex W_self, B_complex G_1p);
void B_HashableD_complexD___serialize__ (B_HashableD_complex self, $Serial$state state) {
}
B_HashableD_complex B_HashableD_complexD___deserialize__ (B_HashableD_complex self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_HashableD_complex));
            self->$class = &B_HashableD_complexG_methods;
            return self;
        }
        self = $DNEW(B_HashableD_complex, state);
    }
    return self;
}
struct B_HashableD_complexG_class B_HashableD_complexG_methods;
B_NoneType B_IndexedD___init__ (B_Indexed W_self, B_Eq W_EqD_A) {
    W_self->W_EqD_AD_Indexed = W_EqD_A;
    return B_None;
}
void B_IndexedD___serialize__ (B_Indexed self, $Serial$state state) {
    $step_serialize(self->W_EqD_AD_Indexed, state);
}
B_Indexed B_IndexedD___deserialize__ (B_Indexed self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_Indexed));
            self->$class = &B_IndexedG_methods;
            return self;
        }
        self = $DNEW(B_Indexed, state);
    }
    self->W_EqD_AD_Indexed = $step_deserialize(state);
    return self;
}
struct B_IndexedG_class B_IndexedG_methods;
B_NoneType B_SliceableD___init__ (B_Sliceable W_self) {
    B_Eq W_333 = ((B_Eq)B_OrdD_intG_new());
    ((B_NoneType (*) (B_Indexed, B_Eq))B_IndexedG_methods.__init__)(((B_Indexed)W_self), W_333);
    return B_None;
}
void B_SliceableD___serialize__ (B_Sliceable self, $Serial$state state) {
    $step_serialize(self->W_EqD_AD_Indexed, state);
}
B_Sliceable B_SliceableD___deserialize__ (B_Sliceable self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_Sliceable));
            self->$class = &B_SliceableG_methods;
            return self;
        }
        self = $DNEW(B_Sliceable, state);
    }
    self->W_EqD_AD_Indexed = $step_deserialize(state);
    return self;
}
struct B_SliceableG_class B_SliceableG_methods;
B_NoneType B_CollectionD___init__ (B_Collection W_self) {
    ((B_NoneType (*) (B_Iterable))B_IterableG_methods.__init__)(((B_Iterable)W_self));
    return B_None;
}
void B_CollectionD___serialize__ (B_Collection self, $Serial$state state) {
}
B_Collection B_CollectionD___deserialize__ (B_Collection self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_Collection));
            self->$class = &B_CollectionG_methods;
            return self;
        }
        self = $DNEW(B_Collection, state);
    }
    return self;
}
struct B_CollectionG_class B_CollectionG_methods;
B_NoneType B_ContainerD___init__ (B_Container W_self, B_Eq W_EqD_A) {
    ((B_NoneType (*) (B_Collection))B_CollectionG_methods.__init__)(((B_Collection)W_self));
    W_self->W_EqD_AD_Container = W_EqD_A;
    return B_None;
}
void B_ContainerD___serialize__ (B_Container self, $Serial$state state) {
    $step_serialize(self->W_EqD_AD_Container, state);
}
B_Container B_ContainerD___deserialize__ (B_Container self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_Container));
            self->$class = &B_ContainerG_methods;
            return self;
        }
        self = $DNEW(B_Container, state);
    }
    self->W_EqD_AD_Container = $step_deserialize(state);
    return self;
}
struct B_ContainerG_class B_ContainerG_methods;
B_NoneType B_SequenceD___init__ (B_Sequence W_self, B_Collection W_Collection, B_Times W_Times) {
    ((B_NoneType (*) (B_Sliceable))B_SliceableG_methods.__init__)(((B_Sliceable)W_self));
    W_self->W_Collection = W_Collection;
    W_self->W_Times = W_Times;
    return B_None;
}
void B_SequenceD___serialize__ (B_Sequence self, $Serial$state state) {
    $step_serialize(self->W_EqD_AD_Indexed, state);
    $step_serialize(self->W_Collection, state);
    $step_serialize(self->W_Times, state);
}
B_Sequence B_SequenceD___deserialize__ (B_Sequence self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_Sequence));
            self->$class = &B_SequenceG_methods;
            return self;
        }
        self = $DNEW(B_Sequence, state);
    }
    self->W_EqD_AD_Indexed = $step_deserialize(state);
    self->W_Collection = $step_deserialize(state);
    self->W_Times = $step_deserialize(state);
    return self;
}
struct B_SequenceG_class B_SequenceG_methods;
B_NoneType B_CollectionD_SequenceD___init__ (B_CollectionD_Sequence W_self, B_Sequence W_Sequence) {
    ((B_NoneType (*) (B_Collection))B_CollectionG_methods.__init__)(((B_Collection)W_self));
    W_self->W_Sequence = W_Sequence;
    return B_None;
}
void B_CollectionD_SequenceD___serialize__ (B_CollectionD_Sequence self, $Serial$state state) {
    $step_serialize(self->W_Sequence, state);
}
B_CollectionD_Sequence B_CollectionD_SequenceD___deserialize__ (B_CollectionD_Sequence self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_CollectionD_Sequence));
            self->$class = &B_CollectionD_SequenceG_methods;
            return self;
        }
        self = $DNEW(B_CollectionD_Sequence, state);
    }
    self->W_Sequence = $step_deserialize(state);
    return self;
}
struct B_CollectionD_SequenceG_class B_CollectionD_SequenceG_methods;
B_NoneType B_TimesD_SequenceD___init__ (B_TimesD_Sequence W_self, B_Sequence W_Sequence) {
    ((B_NoneType (*) (B_Times))B_TimesG_methods.__init__)(((B_Times)W_self));
    W_self->W_Sequence = W_Sequence;
    return B_None;
}
void B_TimesD_SequenceD___serialize__ (B_TimesD_Sequence self, $Serial$state state) {
    $step_serialize(self->W_Sequence, state);
}
B_TimesD_Sequence B_TimesD_SequenceD___deserialize__ (B_TimesD_Sequence self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_TimesD_Sequence));
            self->$class = &B_TimesD_SequenceG_methods;
            return self;
        }
        self = $DNEW(B_TimesD_Sequence, state);
    }
    self->W_Sequence = $step_deserialize(state);
    return self;
}
struct B_TimesD_SequenceG_class B_TimesD_SequenceG_methods;
B_NoneType B_MappingD___init__ (B_Mapping W_self, B_Eq W_EqD_A, B_Indexed W_Indexed) {
    ((B_NoneType (*) (B_Container, B_Eq))B_ContainerG_methods.__init__)(((B_Container)W_self), W_EqD_A);
    W_self->W_EqD_AD_Mapping = W_EqD_A;
    W_self->W_Indexed = W_Indexed;
    return B_None;
}
void B_MappingD___serialize__ (B_Mapping self, $Serial$state state) {
    $step_serialize(self->W_EqD_AD_Container, state);
    $step_serialize(self->W_EqD_AD_Mapping, state);
    $step_serialize(self->W_Indexed, state);
}
B_Mapping B_MappingD___deserialize__ (B_Mapping self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_Mapping));
            self->$class = &B_MappingG_methods;
            return self;
        }
        self = $DNEW(B_Mapping, state);
    }
    self->W_EqD_AD_Container = $step_deserialize(state);
    self->W_EqD_AD_Mapping = $step_deserialize(state);
    self->W_Indexed = $step_deserialize(state);
    return self;
}
struct B_MappingG_class B_MappingG_methods;
B_NoneType B_IndexedD_MappingD___init__ (B_IndexedD_Mapping W_self, B_Eq W_EqD_A, B_Mapping W_Mapping) {
    ((B_NoneType (*) (B_Indexed, B_Eq))B_IndexedG_methods.__init__)(((B_Indexed)W_self), W_EqD_A);
    W_self->W_EqD_AD_Mapping = W_EqD_A;
    W_self->W_Mapping = W_Mapping;
    return B_None;
}
void B_IndexedD_MappingD___serialize__ (B_IndexedD_Mapping self, $Serial$state state) {
    $step_serialize(self->W_EqD_AD_Indexed, state);
    $step_serialize(self->W_EqD_AD_Mapping, state);
    $step_serialize(self->W_Mapping, state);
}
B_IndexedD_Mapping B_IndexedD_MappingD___deserialize__ (B_IndexedD_Mapping self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_IndexedD_Mapping));
            self->$class = &B_IndexedD_MappingG_methods;
            return self;
        }
        self = $DNEW(B_IndexedD_Mapping, state);
    }
    self->W_EqD_AD_Indexed = $step_deserialize(state);
    self->W_EqD_AD_Mapping = $step_deserialize(state);
    self->W_Mapping = $step_deserialize(state);
    return self;
}
struct B_IndexedD_MappingG_class B_IndexedD_MappingG_methods;
B_NoneType B_SetD___init__ (B_Set W_self, B_Eq W_EqD_A, B_Ord W_Ord, B_Logical W_Logical, B_Minus W_Minus) {
    ((B_NoneType (*) (B_Container, B_Eq))B_ContainerG_methods.__init__)(((B_Container)W_self), W_EqD_A);
    W_self->W_EqD_AD_Set = W_EqD_A;
    W_self->W_Ord = W_Ord;
    W_self->W_Logical = W_Logical;
    W_self->W_Minus = W_Minus;
    return B_None;
}
void B_SetD___serialize__ (B_Set self, $Serial$state state) {
    $step_serialize(self->W_EqD_AD_Container, state);
    $step_serialize(self->W_EqD_AD_Set, state);
    $step_serialize(self->W_Ord, state);
    $step_serialize(self->W_Logical, state);
    $step_serialize(self->W_Minus, state);
}
B_Set B_SetD___deserialize__ (B_Set self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_Set));
            self->$class = &B_SetG_methods;
            return self;
        }
        self = $DNEW(B_Set, state);
    }
    self->W_EqD_AD_Container = $step_deserialize(state);
    self->W_EqD_AD_Set = $step_deserialize(state);
    self->W_Ord = $step_deserialize(state);
    self->W_Logical = $step_deserialize(state);
    self->W_Minus = $step_deserialize(state);
    return self;
}
struct B_SetG_class B_SetG_methods;
B_NoneType B_OrdD_SetD___init__ (B_OrdD_Set W_self, B_Eq W_EqD_A, B_Set W_Set) {
    ((B_NoneType (*) (B_Ord))B_OrdG_methods.__init__)(((B_Ord)W_self));
    W_self->W_EqD_AD_Set = W_EqD_A;
    W_self->W_Set = W_Set;
    return B_None;
}
void B_OrdD_SetD___serialize__ (B_OrdD_Set self, $Serial$state state) {
    $step_serialize(self->W_EqD_AD_Set, state);
    $step_serialize(self->W_Set, state);
}
B_OrdD_Set B_OrdD_SetD___deserialize__ (B_OrdD_Set self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_OrdD_Set));
            self->$class = &B_OrdD_SetG_methods;
            return self;
        }
        self = $DNEW(B_OrdD_Set, state);
    }
    self->W_EqD_AD_Set = $step_deserialize(state);
    self->W_Set = $step_deserialize(state);
    return self;
}
struct B_OrdD_SetG_class B_OrdD_SetG_methods;
B_NoneType B_LogicalD_SetD___init__ (B_LogicalD_Set W_self, B_Eq W_EqD_A, B_Set W_Set) {
    ((B_NoneType (*) (B_Logical))B_LogicalG_methods.__init__)(((B_Logical)W_self));
    W_self->W_EqD_AD_Set = W_EqD_A;
    W_self->W_Set = W_Set;
    return B_None;
}
void B_LogicalD_SetD___serialize__ (B_LogicalD_Set self, $Serial$state state) {
    $step_serialize(self->W_EqD_AD_Set, state);
    $step_serialize(self->W_Set, state);
}
B_LogicalD_Set B_LogicalD_SetD___deserialize__ (B_LogicalD_Set self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_LogicalD_Set));
            self->$class = &B_LogicalD_SetG_methods;
            return self;
        }
        self = $DNEW(B_LogicalD_Set, state);
    }
    self->W_EqD_AD_Set = $step_deserialize(state);
    self->W_Set = $step_deserialize(state);
    return self;
}
struct B_LogicalD_SetG_class B_LogicalD_SetG_methods;
B_NoneType B_MinusD_SetD___init__ (B_MinusD_Set W_self, B_Eq W_EqD_A, B_Set W_Set) {
    ((B_NoneType (*) (B_Minus))B_MinusG_methods.__init__)(((B_Minus)W_self));
    W_self->W_EqD_AD_Set = W_EqD_A;
    W_self->W_Set = W_Set;
    return B_None;
}
void B_MinusD_SetD___serialize__ (B_MinusD_Set self, $Serial$state state) {
    $step_serialize(self->W_EqD_AD_Set, state);
    $step_serialize(self->W_Set, state);
}
B_MinusD_Set B_MinusD_SetD___deserialize__ (B_MinusD_Set self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_MinusD_Set));
            self->$class = &B_MinusD_SetG_methods;
            return self;
        }
        self = $DNEW(B_MinusD_Set, state);
    }
    self->W_EqD_AD_Set = $step_deserialize(state);
    self->W_Set = $step_deserialize(state);
    return self;
}
struct B_MinusD_SetG_class B_MinusD_SetG_methods;
B_NoneType B_SequenceD_listD___init__ (B_SequenceD_list W_self) {
    ((B_NoneType (*) (B_Sequence, B_Collection, B_Times))B_SequenceG_methods.__init__)(((B_Sequence)W_self), ((B_Collection)B_CollectionD_SequenceD_listG_new(((B_Sequence)W_self))), ((B_Times)B_TimesD_SequenceD_listG_new(((B_Sequence)W_self))));
    return B_None;
}
B_NoneType B_SequenceD_listD_reverse (B_SequenceD_list W_self, B_list G_1p);
B_NoneType B_SequenceD_listD_append (B_SequenceD_list W_self, B_list G_1p, $WORD G_2p);
B_NoneType B_SequenceD_listD_insert (B_SequenceD_list W_self, B_list G_1p, B_int G_2p, $WORD G_3p);
B_Iterator B_SequenceD_listD___reversed__ (B_SequenceD_list W_self, B_list G_1p);
B_NoneType B_SequenceD_listD___delslice__ (B_SequenceD_list W_self, B_list G_1p, B_slice G_2p);
B_NoneType B_SequenceD_listD___setslice__ (B_SequenceD_list W_self, B_list G_1p, B_Iterable W_IterableE_682, B_slice G_2p, $WORD G_3p);
B_list B_SequenceD_listD___getslice__ (B_SequenceD_list W_self, B_list G_1p, B_slice G_2p);
B_NoneType B_SequenceD_listD___delitem__ (B_SequenceD_list W_self, B_list G_1p, B_int G_2p);
B_NoneType B_SequenceD_listD___setitem__ (B_SequenceD_list W_self, B_list G_1p, B_int G_2p, $WORD G_3p);
$WORD B_SequenceD_listD___getitem__ (B_SequenceD_list W_self, B_list G_1p, B_int G_2p);
void B_SequenceD_listD___serialize__ (B_SequenceD_list self, $Serial$state state) {
    $step_serialize(self->W_EqD_AD_Indexed, state);
    $step_serialize(self->W_Collection, state);
    $step_serialize(self->W_Times, state);
}
B_SequenceD_list B_SequenceD_listD___deserialize__ (B_SequenceD_list self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_SequenceD_list));
            self->$class = &B_SequenceD_listG_methods;
            return self;
        }
        self = $DNEW(B_SequenceD_list, state);
    }
    self->W_EqD_AD_Indexed = $step_deserialize(state);
    self->W_Collection = $step_deserialize(state);
    self->W_Times = $step_deserialize(state);
    return self;
}
B_SequenceD_list B_SequenceD_listG_new() {
    B_SequenceD_list $tmp = malloc(sizeof(struct B_SequenceD_list));
    $tmp->$class = &B_SequenceD_listG_methods;
    B_SequenceD_listG_methods.__init__($tmp);
    return $tmp;
}
struct B_SequenceD_listG_class B_SequenceD_listG_methods;
B_NoneType B_CollectionD_SequenceD_listD___init__ (B_CollectionD_SequenceD_list W_self, B_Sequence W_Sequence) {
    ((B_NoneType (*) (B_CollectionD_Sequence, B_Sequence))B_CollectionD_SequenceG_methods.__init__)(((B_CollectionD_Sequence)W_self), ((B_Sequence)W_Sequence));
    return B_None;
}
B_int B_CollectionD_SequenceD_listD___len__ (B_CollectionD_SequenceD_list W_self, B_list G_1p);
B_list B_CollectionD_SequenceD_listD___fromiter__ (B_CollectionD_SequenceD_list W_self, B_Iterable W_IterableE_687, $WORD G_1p);
B_Iterator B_CollectionD_SequenceD_listD___iter__ (B_CollectionD_SequenceD_list W_self, B_list G_1p);
void B_CollectionD_SequenceD_listD___serialize__ (B_CollectionD_SequenceD_list self, $Serial$state state) {
    $step_serialize(self->W_Sequence, state);
}
B_CollectionD_SequenceD_list B_CollectionD_SequenceD_listD___deserialize__ (B_CollectionD_SequenceD_list self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_CollectionD_SequenceD_list));
            self->$class = &B_CollectionD_SequenceD_listG_methods;
            return self;
        }
        self = $DNEW(B_CollectionD_SequenceD_list, state);
    }
    self->W_Sequence = $step_deserialize(state);
    return self;
}
B_CollectionD_SequenceD_list B_CollectionD_SequenceD_listG_new(B_Sequence G_1) {
    B_CollectionD_SequenceD_list $tmp = malloc(sizeof(struct B_CollectionD_SequenceD_list));
    $tmp->$class = &B_CollectionD_SequenceD_listG_methods;
    B_CollectionD_SequenceD_listG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct B_CollectionD_SequenceD_listG_class B_CollectionD_SequenceD_listG_methods;
B_NoneType B_TimesD_SequenceD_listD___init__ (B_TimesD_SequenceD_list W_self, B_Sequence W_Sequence) {
    ((B_NoneType (*) (B_TimesD_Sequence, B_Sequence))B_TimesD_SequenceG_methods.__init__)(((B_TimesD_Sequence)W_self), ((B_Sequence)W_Sequence));
    return B_None;
}
B_list B_TimesD_SequenceD_listD___mul__ (B_TimesD_SequenceD_list W_self, B_list G_1p, B_int G_2p);
B_list B_TimesD_SequenceD_listD___add__ (B_TimesD_SequenceD_list W_self, B_list G_1p, B_list G_2p);
void B_TimesD_SequenceD_listD___serialize__ (B_TimesD_SequenceD_list self, $Serial$state state) {
    $step_serialize(self->W_Sequence, state);
}
B_TimesD_SequenceD_list B_TimesD_SequenceD_listD___deserialize__ (B_TimesD_SequenceD_list self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_TimesD_SequenceD_list));
            self->$class = &B_TimesD_SequenceD_listG_methods;
            return self;
        }
        self = $DNEW(B_TimesD_SequenceD_list, state);
    }
    self->W_Sequence = $step_deserialize(state);
    return self;
}
B_TimesD_SequenceD_list B_TimesD_SequenceD_listG_new(B_Sequence G_1) {
    B_TimesD_SequenceD_list $tmp = malloc(sizeof(struct B_TimesD_SequenceD_list));
    $tmp->$class = &B_TimesD_SequenceD_listG_methods;
    B_TimesD_SequenceD_listG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct B_TimesD_SequenceD_listG_class B_TimesD_SequenceD_listG_methods;
B_NoneType B_ContainerD_listD___init__ (B_ContainerD_list W_self, B_Eq W_EqD_A) {
    ((B_NoneType (*) (B_Container, B_Eq))B_ContainerG_methods.__init__)(((B_Container)W_self), W_EqD_A);
    W_self->W_EqD_AD_ContainerD_list = W_EqD_A;
    return B_None;
}
B_bool B_ContainerD_listD___containsnot__ (B_ContainerD_list W_self, B_list G_1p, $WORD G_2p);
B_bool B_ContainerD_listD___contains__ (B_ContainerD_list W_self, B_list G_1p, $WORD G_2p);
void B_ContainerD_listD___serialize__ (B_ContainerD_list self, $Serial$state state) {
    $step_serialize(self->W_EqD_AD_Container, state);
    $step_serialize(self->W_EqD_AD_ContainerD_list, state);
}
B_ContainerD_list B_ContainerD_listD___deserialize__ (B_ContainerD_list self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_ContainerD_list));
            self->$class = &B_ContainerD_listG_methods;
            return self;
        }
        self = $DNEW(B_ContainerD_list, state);
    }
    self->W_EqD_AD_Container = $step_deserialize(state);
    self->W_EqD_AD_ContainerD_list = $step_deserialize(state);
    return self;
}
B_ContainerD_list B_ContainerD_listG_new(B_Eq w) { // Why is this not generated by actonc?
    return $NEW(B_ContainerD_list,w);              //
}                                                  //
struct B_ContainerD_listG_class B_ContainerD_listG_methods;
B_NoneType B_OrdD_listD___init__ (B_OrdD_list W_self, B_Ord W_OrdD_A) {
    ((B_NoneType (*) (B_Ord))B_OrdG_methods.__init__)(((B_Ord)W_self));
    W_self->W_OrdD_AD_OrdD_list = W_OrdD_A;
    return B_None;
}
B_bool B_OrdD_listD___lt__ (B_OrdD_list W_self, B_list G_1p, B_list G_2p);
B_bool B_OrdD_listD___eq__ (B_OrdD_list W_self, B_list G_1p, B_list G_2p);
void B_OrdD_listD___serialize__ (B_OrdD_list self, $Serial$state state) {
    $step_serialize(self->W_OrdD_AD_OrdD_list, state);
}
B_OrdD_list B_OrdD_listD___deserialize__ (B_OrdD_list self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_OrdD_list));
            self->$class = &B_OrdD_listG_methods;
            return self;
        }
        self = $DNEW(B_OrdD_list, state);
    }
    self->W_OrdD_AD_OrdD_list = $step_deserialize(state);
    return self;
}
B_OrdD_list B_OrdD_listG_new(B_Ord G_1) {
    B_OrdD_list $tmp = malloc(sizeof(struct B_OrdD_list));
    $tmp->$class = &B_OrdD_listG_methods;
    B_OrdD_listG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct B_OrdD_listG_class B_OrdD_listG_methods;
B_NoneType B_MappingD_dictD___init__ (B_MappingD_dict W_self, B_Hashable W_HashableD_A) {
    ((B_NoneType (*) (B_Mapping, B_Eq, B_Indexed))B_MappingG_methods.__init__)(((B_Mapping)W_self), ((B_Eq)W_HashableD_A), ((B_Indexed)B_IndexedD_MappingD_dictG_new(W_HashableD_A, ((B_Mapping)W_self))));
    W_self->W_HashableD_AD_MappingD_dict = W_HashableD_A;
    return B_None;
}
B_NoneType B_MappingD_dictD_setdefault (B_MappingD_dict W_self, B_dict G_1p, $WORD G_2p, $WORD G_3p);
B_tuple B_MappingD_dictD_popitem (B_MappingD_dict W_self, B_dict G_1p);
B_NoneType B_MappingD_dictD_update (B_MappingD_dict W_self, B_dict G_1p, B_Iterable W_IterableE_718, $WORD G_2p);
B_Iterator B_MappingD_dictD_items (B_MappingD_dict W_self, B_dict G_1p);
B_Iterator B_MappingD_dictD_values (B_MappingD_dict W_self, B_dict G_1p);
B_Iterator B_MappingD_dictD_keys (B_MappingD_dict W_self, B_dict G_1p);
$WORD B_MappingD_dictD_get (B_MappingD_dict W_self, B_dict G_1p, $WORD G_2p, $WORD G_3p);
B_bool B_MappingD_dictD___containsnot__ (B_MappingD_dict W_self, B_dict G_1p, $WORD G_2p);
B_bool B_MappingD_dictD___contains__ (B_MappingD_dict W_self, B_dict G_1p, $WORD G_2p);
B_int B_MappingD_dictD___len__ (B_MappingD_dict W_self, B_dict G_1p);
B_dict B_MappingD_dictD___fromiter__ (B_MappingD_dict W_self, B_Iterable W_IterableE_687, $WORD G_1p);
B_Iterator B_MappingD_dictD___iter__ (B_MappingD_dict W_self, B_dict G_1p);
void B_MappingD_dictD___serialize__ (B_MappingD_dict self, $Serial$state state) {
    $step_serialize(self->W_EqD_AD_Container, state);
    $step_serialize(self->W_EqD_AD_Mapping, state);
    $step_serialize(self->W_Indexed, state);
    $step_serialize(self->W_HashableD_AD_MappingD_dict, state);
}
B_MappingD_dict B_MappingD_dictD___deserialize__ (B_MappingD_dict self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_MappingD_dict));
            self->$class = &B_MappingD_dictG_methods;
            return self;
        }
        self = $DNEW(B_MappingD_dict, state);
    }
    self->W_EqD_AD_Container = $step_deserialize(state);
    self->W_EqD_AD_Mapping = $step_deserialize(state);
    self->W_Indexed = $step_deserialize(state);
    self->W_HashableD_AD_MappingD_dict = $step_deserialize(state);
    return self;
}
B_MappingD_dict B_MappingD_dictG_new(B_Hashable G_1) {
    B_MappingD_dict $tmp = malloc(sizeof(struct B_MappingD_dict));
    $tmp->$class = &B_MappingD_dictG_methods;
    B_MappingD_dictG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct B_MappingD_dictG_class B_MappingD_dictG_methods;
B_NoneType B_IndexedD_MappingD_dictD___init__ (B_IndexedD_MappingD_dict W_self, B_Hashable W_HashableD_A, B_Mapping W_Mapping) {
    ((B_NoneType (*) (B_IndexedD_Mapping, B_Eq, B_Mapping))B_IndexedD_MappingG_methods.__init__)(((B_IndexedD_Mapping)W_self), ((B_Eq)W_HashableD_A), ((B_Mapping)W_Mapping));
    W_self->W_HashableD_AD_MappingD_dict = W_HashableD_A;
    return B_None;
}
B_NoneType B_IndexedD_MappingD_dictD___delitem__ (B_IndexedD_MappingD_dict W_self, B_dict G_1p, $WORD G_2p);
B_NoneType B_IndexedD_MappingD_dictD___setitem__ (B_IndexedD_MappingD_dict W_self, B_dict G_1p, $WORD G_2p, $WORD G_3p);
$WORD B_IndexedD_MappingD_dictD___getitem__ (B_IndexedD_MappingD_dict W_self, B_dict G_1p, $WORD G_2p);
void B_IndexedD_MappingD_dictD___serialize__ (B_IndexedD_MappingD_dict self, $Serial$state state) {
    $step_serialize(self->W_EqD_AD_Indexed, state);
    $step_serialize(self->W_EqD_AD_Mapping, state);
    $step_serialize(self->W_Mapping, state);
    $step_serialize(self->W_HashableD_AD_MappingD_dict, state);
}
B_IndexedD_MappingD_dict B_IndexedD_MappingD_dictD___deserialize__ (B_IndexedD_MappingD_dict self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_IndexedD_MappingD_dict));
            self->$class = &B_IndexedD_MappingD_dictG_methods;
            return self;
        }
        self = $DNEW(B_IndexedD_MappingD_dict, state);
    }
    self->W_EqD_AD_Indexed = $step_deserialize(state);
    self->W_EqD_AD_Mapping = $step_deserialize(state);
    self->W_Mapping = $step_deserialize(state);
    self->W_HashableD_AD_MappingD_dict = $step_deserialize(state);
    return self;
}
B_IndexedD_MappingD_dict B_IndexedD_MappingD_dictG_new(B_Hashable G_1, B_Mapping G_2) {
    B_IndexedD_MappingD_dict $tmp = malloc(sizeof(struct B_IndexedD_MappingD_dict));
    $tmp->$class = &B_IndexedD_MappingD_dictG_methods;
    B_IndexedD_MappingD_dictG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct B_IndexedD_MappingD_dictG_class B_IndexedD_MappingD_dictG_methods;
B_NoneType B_OrdD_dictD___init__ (B_OrdD_dict W_self, B_Hashable W_HashableD_A, B_Eq W_EqD_B) {
    ((B_NoneType (*) (B_Ord))B_OrdG_methods.__init__)(((B_Ord)W_self));
    W_self->W_HashableD_AD_OrdD_dict = W_HashableD_A;
    W_self->W_EqD_BD_OrdD_dict = W_EqD_B;
    return B_None;
}
B_bool B_OrdD_dictD___lt__ (B_OrdD_dict W_self, B_dict G_1p, B_dict G_2p);
B_bool B_OrdD_dictD___eq__ (B_OrdD_dict W_self, B_dict G_1p, B_dict G_2p);
void B_OrdD_dictD___serialize__ (B_OrdD_dict self, $Serial$state state) {
    $step_serialize(self->W_HashableD_AD_OrdD_dict, state);
    $step_serialize(self->W_EqD_BD_OrdD_dict, state);
}
B_OrdD_dict B_OrdD_dictD___deserialize__ (B_OrdD_dict self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_OrdD_dict));
            self->$class = &B_OrdD_dictG_methods;
            return self;
        }
        self = $DNEW(B_OrdD_dict, state);
    }
    self->W_HashableD_AD_OrdD_dict = $step_deserialize(state);
    self->W_EqD_BD_OrdD_dict = $step_deserialize(state);
    return self;
}
B_OrdD_dict B_OrdD_dictG_new(B_Hashable G_1, B_Eq G_2) {
    B_OrdD_dict $tmp = malloc(sizeof(struct B_OrdD_dict));
    $tmp->$class = &B_OrdD_dictG_methods;
    B_OrdD_dictG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct B_OrdD_dictG_class B_OrdD_dictG_methods;
B_NoneType B_SetD_setD___init__ (B_SetD_set W_self, B_Hashable W_HashableD_A) {
    ((B_NoneType (*) (B_Set, B_Eq, B_Ord, B_Logical, B_Minus))B_SetG_methods.__init__)(((B_Set)W_self), ((B_Eq)W_HashableD_A), ((B_Ord)B_OrdD_SetD_setG_new(W_HashableD_A, ((B_Set)W_self))), ((B_Logical)B_LogicalD_SetD_setG_new(W_HashableD_A, ((B_Set)W_self))), ((B_Minus)B_MinusD_SetD_setG_new(W_HashableD_A, ((B_Set)W_self))));
    W_self->W_HashableD_AD_SetD_set = W_HashableD_A;
    return B_None;
}
$WORD B_SetD_setD_pop (B_SetD_set W_self, B_set G_1p);
B_NoneType B_SetD_setD_discard (B_SetD_set W_self, B_set G_1p, $WORD G_2p);
B_NoneType B_SetD_setD_add (B_SetD_set W_self, B_set G_1p, $WORD G_2p);
B_bool B_SetD_setD_isdisjoint (B_SetD_set W_self, B_set G_1p, B_set G_2p);
B_bool B_SetD_setD___containsnot__ (B_SetD_set W_self, B_set G_1p, $WORD G_2p);
B_bool B_SetD_setD___contains__ (B_SetD_set W_self, B_set G_1p, $WORD G_2p);
B_int B_SetD_setD___len__ (B_SetD_set W_self, B_set G_1p);
B_set B_SetD_setD___fromiter__ (B_SetD_set W_self, B_Iterable W_IterableE_687, $WORD G_1p);
B_Iterator B_SetD_setD___iter__ (B_SetD_set W_self, B_set G_1p);
void B_SetD_setD___serialize__ (B_SetD_set self, $Serial$state state) {
    $step_serialize(self->W_EqD_AD_Container, state);
    $step_serialize(self->W_EqD_AD_Set, state);
    $step_serialize(self->W_Ord, state);
    $step_serialize(self->W_Logical, state);
    $step_serialize(self->W_Minus, state);
    $step_serialize(self->W_HashableD_AD_SetD_set, state);
}
B_SetD_set B_SetD_setD___deserialize__ (B_SetD_set self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_SetD_set));
            self->$class = &B_SetD_setG_methods;
            return self;
        }
        self = $DNEW(B_SetD_set, state);
    }
    self->W_EqD_AD_Container = $step_deserialize(state);
    self->W_EqD_AD_Set = $step_deserialize(state);
    self->W_Ord = $step_deserialize(state);
    self->W_Logical = $step_deserialize(state);
    self->W_Minus = $step_deserialize(state);
    self->W_HashableD_AD_SetD_set = $step_deserialize(state);
    return self;
}
B_SetD_set B_SetD_setG_new(B_Hashable G_1) {
    B_SetD_set $tmp = malloc(sizeof(struct B_SetD_set));
    $tmp->$class = &B_SetD_setG_methods;
    B_SetD_setG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct B_SetD_setG_class B_SetD_setG_methods;
B_NoneType B_OrdD_SetD_setD___init__ (B_OrdD_SetD_set W_self, B_Hashable W_HashableD_A, B_Set W_Set) {
    ((B_NoneType (*) (B_OrdD_Set, B_Eq, B_Set))B_OrdD_SetG_methods.__init__)(((B_OrdD_Set)W_self), ((B_Eq)W_HashableD_A), ((B_Set)W_Set));
    W_self->W_HashableD_AD_SetD_set = W_HashableD_A;
    return B_None;
}
B_bool B_OrdD_SetD_setD___lt__ (B_OrdD_SetD_set W_self, B_set G_1p, B_set G_2p);
B_bool B_OrdD_SetD_setD___eq__ (B_OrdD_SetD_set W_self, B_set G_1p, B_set G_2p);
void B_OrdD_SetD_setD___serialize__ (B_OrdD_SetD_set self, $Serial$state state) {
    $step_serialize(self->W_EqD_AD_Set, state);
    $step_serialize(self->W_Set, state);
    $step_serialize(self->W_HashableD_AD_SetD_set, state);
}
B_OrdD_SetD_set B_OrdD_SetD_setD___deserialize__ (B_OrdD_SetD_set self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_OrdD_SetD_set));
            self->$class = &B_OrdD_SetD_setG_methods;
            return self;
        }
        self = $DNEW(B_OrdD_SetD_set, state);
    }
    self->W_EqD_AD_Set = $step_deserialize(state);
    self->W_Set = $step_deserialize(state);
    self->W_HashableD_AD_SetD_set = $step_deserialize(state);
    return self;
}
B_OrdD_SetD_set B_OrdD_SetD_setG_new(B_Hashable G_1, B_Set G_2) {
    B_OrdD_SetD_set $tmp = malloc(sizeof(struct B_OrdD_SetD_set));
    $tmp->$class = &B_OrdD_SetD_setG_methods;
    B_OrdD_SetD_setG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct B_OrdD_SetD_setG_class B_OrdD_SetD_setG_methods;
B_NoneType B_LogicalD_SetD_setD___init__ (B_LogicalD_SetD_set W_self, B_Hashable W_HashableD_A, B_Set W_Set) {
    ((B_NoneType (*) (B_LogicalD_Set, B_Eq, B_Set))B_LogicalD_SetG_methods.__init__)(((B_LogicalD_Set)W_self), ((B_Eq)W_HashableD_A), ((B_Set)W_Set));
    W_self->W_HashableD_AD_SetD_set = W_HashableD_A;
    return B_None;
}
B_set B_LogicalD_SetD_setD___xor__ (B_LogicalD_SetD_set W_self, B_set G_1p, B_set G_2p);
B_set B_LogicalD_SetD_setD___or__ (B_LogicalD_SetD_set W_self, B_set G_1p, B_set G_2p);
B_set B_LogicalD_SetD_setD___and__ (B_LogicalD_SetD_set W_self, B_set G_1p, B_set G_2p);
void B_LogicalD_SetD_setD___serialize__ (B_LogicalD_SetD_set self, $Serial$state state) {
    $step_serialize(self->W_EqD_AD_Set, state);
    $step_serialize(self->W_Set, state);
    $step_serialize(self->W_HashableD_AD_SetD_set, state);
}
B_LogicalD_SetD_set B_LogicalD_SetD_setD___deserialize__ (B_LogicalD_SetD_set self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_LogicalD_SetD_set));
            self->$class = &B_LogicalD_SetD_setG_methods;
            return self;
        }
        self = $DNEW(B_LogicalD_SetD_set, state);
    }
    self->W_EqD_AD_Set = $step_deserialize(state);
    self->W_Set = $step_deserialize(state);
    self->W_HashableD_AD_SetD_set = $step_deserialize(state);
    return self;
}
B_LogicalD_SetD_set B_LogicalD_SetD_setG_new(B_Hashable G_1, B_Set G_2) {
    B_LogicalD_SetD_set $tmp = malloc(sizeof(struct B_LogicalD_SetD_set));
    $tmp->$class = &B_LogicalD_SetD_setG_methods;
    B_LogicalD_SetD_setG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct B_LogicalD_SetD_setG_class B_LogicalD_SetD_setG_methods;
B_NoneType B_MinusD_SetD_setD___init__ (B_MinusD_SetD_set W_self, B_Hashable W_HashableD_A, B_Set W_Set) {
    ((B_NoneType (*) (B_MinusD_Set, B_Eq, B_Set))B_MinusD_SetG_methods.__init__)(((B_MinusD_Set)W_self), ((B_Eq)W_HashableD_A), ((B_Set)W_Set));
    W_self->W_HashableD_AD_SetD_set = W_HashableD_A;
    return B_None;
}
B_set B_MinusD_SetD_setD___sub__ (B_MinusD_SetD_set W_self, B_set G_1p, B_set G_2p);
void B_MinusD_SetD_setD___serialize__ (B_MinusD_SetD_set self, $Serial$state state) {
    $step_serialize(self->W_EqD_AD_Set, state);
    $step_serialize(self->W_Set, state);
    $step_serialize(self->W_HashableD_AD_SetD_set, state);
}
B_MinusD_SetD_set B_MinusD_SetD_setD___deserialize__ (B_MinusD_SetD_set self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_MinusD_SetD_set));
            self->$class = &B_MinusD_SetD_setG_methods;
            return self;
        }
        self = $DNEW(B_MinusD_SetD_set, state);
    }
    self->W_EqD_AD_Set = $step_deserialize(state);
    self->W_Set = $step_deserialize(state);
    self->W_HashableD_AD_SetD_set = $step_deserialize(state);
    return self;
}
B_MinusD_SetD_set B_MinusD_SetD_setG_new(B_Hashable G_1, B_Set G_2) {
    B_MinusD_SetD_set $tmp = malloc(sizeof(struct B_MinusD_SetD_set));
    $tmp->$class = &B_MinusD_SetD_setG_methods;
    B_MinusD_SetD_setG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct B_MinusD_SetD_setG_class B_MinusD_SetD_setG_methods;
B_NoneType B_IterableD_IteratorD___init__ (B_IterableD_Iterator W_self) {
    ((B_NoneType (*) (B_Iterable))B_IterableG_methods.__init__)(((B_Iterable)W_self));
    return B_None;
}
B_Iterator B_IterableD_IteratorD___iter__ (B_IterableD_Iterator W_self, B_Iterator G_1p);
void B_IterableD_IteratorD___serialize__ (B_IterableD_Iterator self, $Serial$state state) {
}
B_IterableD_Iterator B_IterableD_IteratorD___deserialize__ (B_IterableD_Iterator self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_IterableD_Iterator));
            self->$class = &B_IterableD_IteratorG_methods;
            return self;
        }
        self = $DNEW(B_IterableD_Iterator, state);
    }
    return self;
}
B_IterableD_Iterator B_IterableD_IteratorG_new() {
    B_IterableD_Iterator $tmp = malloc(sizeof(struct B_IterableD_Iterator));
    $tmp->$class = &B_IterableD_IteratorG_methods;
    B_IterableD_IteratorG_methods.__init__($tmp);
    return $tmp;
}
struct B_IterableD_IteratorG_class B_IterableD_IteratorG_methods;
B_NoneType B_IterableD_rangeD___init__ (B_IterableD_range W_self) {
    ((B_NoneType (*) (B_Iterable))B_IterableG_methods.__init__)(((B_Iterable)W_self));
    return B_None;
}
B_Iterator B_IterableD_rangeD___iter__ (B_IterableD_range W_self, B_range G_1p);
void B_IterableD_rangeD___serialize__ (B_IterableD_range self, $Serial$state state) {
}
B_IterableD_range B_IterableD_rangeD___deserialize__ (B_IterableD_range self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_IterableD_range));
            self->$class = &B_IterableD_rangeG_methods;
            return self;
        }
        self = $DNEW(B_IterableD_range, state);
    }
    return self;
}
B_IterableD_range B_IterableD_rangeG_new() {
    B_IterableD_range $tmp = malloc(sizeof(struct B_IterableD_range));
    $tmp->$class = &B_IterableD_rangeG_methods;
    B_IterableD_rangeG_methods.__init__($tmp);
    return $tmp;
}
struct B_IterableD_rangeG_class B_IterableD_rangeG_methods;
B_NoneType B_OrdD_strD___init__ (B_OrdD_str W_self) {
    ((B_NoneType (*) (B_Ord))B_OrdG_methods.__init__)(((B_Ord)W_self));
    return B_None;
}
B_bool B_OrdD_strD___lt__ (B_OrdD_str W_self, B_str G_1p, B_str G_2p);
B_bool B_OrdD_strD___eq__ (B_OrdD_str W_self, B_str G_1p, B_str G_2p);
void B_OrdD_strD___serialize__ (B_OrdD_str self, $Serial$state state) {
}
B_OrdD_str B_OrdD_strD___deserialize__ (B_OrdD_str self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_OrdD_str));
            self->$class = &B_OrdD_strG_methods;
            return self;
        }
        self = $DNEW(B_OrdD_str, state);
    }
    return self;
}
B_OrdD_str B_OrdD_strG_new() {
    B_OrdD_str $tmp = malloc(sizeof(struct B_OrdD_str));
    $tmp->$class = &B_OrdD_strG_methods;
    B_OrdD_strG_methods.__init__($tmp);
    return $tmp;
}
struct B_OrdD_strG_class B_OrdD_strG_methods;
B_NoneType B_ContainerD_strD___init__ (B_ContainerD_str W_self) {
    B_Eq W_395 = ((B_Eq)B_OrdD_strG_new());
    ((B_NoneType (*) (B_Container, B_Eq))B_ContainerG_methods.__init__)(((B_Container)W_self), W_395);
    return B_None;
}
B_bool B_ContainerD_strD___containsnot__ (B_ContainerD_str W_self, B_str G_1p, B_str G_2p);
B_bool B_ContainerD_strD___contains__ (B_ContainerD_str W_self, B_str G_1p, B_str G_2p);
B_int B_ContainerD_strD___len__ (B_ContainerD_str W_self, B_str G_1p);
B_str B_ContainerD_strD___fromiter__ (B_ContainerD_str W_self, B_Iterable W_IterableE_687, $WORD G_1p);
B_Iterator B_ContainerD_strD___iter__ (B_ContainerD_str W_self, B_str G_1p);
void B_ContainerD_strD___serialize__ (B_ContainerD_str self, $Serial$state state) {
    $step_serialize(self->W_EqD_AD_Container, state);
}
B_ContainerD_str B_ContainerD_strD___deserialize__ (B_ContainerD_str self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_ContainerD_str));
            self->$class = &B_ContainerD_strG_methods;
            return self;
        }
        self = $DNEW(B_ContainerD_str, state);
    }
    self->W_EqD_AD_Container = $step_deserialize(state);
    return self;
}
B_ContainerD_str B_ContainerD_strG_new() {
    B_ContainerD_str $tmp = malloc(sizeof(struct B_ContainerD_str));
    $tmp->$class = &B_ContainerD_strG_methods;
    B_ContainerD_strG_methods.__init__($tmp);
    return $tmp;
}
struct B_ContainerD_strG_class B_ContainerD_strG_methods;
B_NoneType B_SliceableD_strD___init__ (B_SliceableD_str W_self) {
    ((B_NoneType (*) (B_Sliceable))B_SliceableG_methods.__init__)(((B_Sliceable)W_self));
    return B_None;
}
B_NoneType B_SliceableD_strD___delslice__ (B_SliceableD_str W_self, B_str G_1p, B_slice G_2p);
B_NoneType B_SliceableD_strD___setslice__ (B_SliceableD_str W_self, B_str G_1p, B_Iterable W_IterableE_682, B_slice G_2p, $WORD G_3p);
B_str B_SliceableD_strD___getslice__ (B_SliceableD_str W_self, B_str G_1p, B_slice G_2p);
B_NoneType B_SliceableD_strD___delitem__ (B_SliceableD_str W_self, B_str G_1p, B_int G_2p);
B_NoneType B_SliceableD_strD___setitem__ (B_SliceableD_str W_self, B_str G_1p, B_int G_2p, B_str G_3p);
B_str B_SliceableD_strD___getitem__ (B_SliceableD_str W_self, B_str G_1p, B_int G_2p);
void B_SliceableD_strD___serialize__ (B_SliceableD_str self, $Serial$state state) {
    $step_serialize(self->W_EqD_AD_Indexed, state);
}
B_SliceableD_str B_SliceableD_strD___deserialize__ (B_SliceableD_str self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_SliceableD_str));
            self->$class = &B_SliceableD_strG_methods;
            return self;
        }
        self = $DNEW(B_SliceableD_str, state);
    }
    self->W_EqD_AD_Indexed = $step_deserialize(state);
    return self;
}
B_SliceableD_str B_SliceableD_strG_new() {
    B_SliceableD_str $tmp = malloc(sizeof(struct B_SliceableD_str));
    $tmp->$class = &B_SliceableD_strG_methods;
    B_SliceableD_strG_methods.__init__($tmp);
    return $tmp;
}
struct B_SliceableD_strG_class B_SliceableD_strG_methods;
B_NoneType B_TimesD_strD___init__ (B_TimesD_str W_self) {
    ((B_NoneType (*) (B_Times))B_TimesG_methods.__init__)(((B_Times)W_self));
    return B_None;
}
B_str B_TimesD_strD___mul__ (B_TimesD_str W_self, B_str G_1p, B_int G_2p);
B_str B_TimesD_strD___add__ (B_TimesD_str W_self, B_str G_1p, B_str G_2p);
void B_TimesD_strD___serialize__ (B_TimesD_str self, $Serial$state state) {
}
B_TimesD_str B_TimesD_strD___deserialize__ (B_TimesD_str self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_TimesD_str));
            self->$class = &B_TimesD_strG_methods;
            return self;
        }
        self = $DNEW(B_TimesD_str, state);
    }
    return self;
}
B_TimesD_str B_TimesD_strG_new() {
    B_TimesD_str $tmp = malloc(sizeof(struct B_TimesD_str));
    $tmp->$class = &B_TimesD_strG_methods;
    B_TimesD_strG_methods.__init__($tmp);
    return $tmp;
}
struct B_TimesD_strG_class B_TimesD_strG_methods;
B_NoneType B_HashableD_strD___init__ (B_HashableD_str W_self) {
    ((B_NoneType (*) (B_Hashable))B_HashableG_methods.__init__)(((B_Hashable)W_self));
    return B_None;
}
B_int B_HashableD_strD___hash__ (B_HashableD_str W_self, B_str G_1p);
void B_HashableD_strD___serialize__ (B_HashableD_str self, $Serial$state state) {
}
B_HashableD_str B_HashableD_strD___deserialize__ (B_HashableD_str self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_HashableD_str));
            self->$class = &B_HashableD_strG_methods;
            return self;
        }
        self = $DNEW(B_HashableD_str, state);
    }
    return self;
}
B_HashableD_str B_HashableD_strG_new() {   // Manually added.
    return $NEW(B_HashableD_str);          //
}                                          //
struct B_HashableD_strG_class B_HashableD_strG_methods;
B_NoneType B_OrdD_bytearrayD___init__ (B_OrdD_bytearray W_self) {
    ((B_NoneType (*) (B_Ord))B_OrdG_methods.__init__)(((B_Ord)W_self));
    return B_None;
}
B_bool B_OrdD_bytearrayD___lt__ (B_OrdD_bytearray W_self, B_bytearray G_1p, B_bytearray G_2p);
B_bool B_OrdD_bytearrayD___eq__ (B_OrdD_bytearray W_self, B_bytearray G_1p, B_bytearray G_2p);
void B_OrdD_bytearrayD___serialize__ (B_OrdD_bytearray self, $Serial$state state) {
}
B_OrdD_bytearray B_OrdD_bytearrayD___deserialize__ (B_OrdD_bytearray self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_OrdD_bytearray));
            self->$class = &B_OrdD_bytearrayG_methods;
            return self;
        }
        self = $DNEW(B_OrdD_bytearray, state);
    }
    return self;
}
B_OrdD_bytearray B_OrdD_bytearrayG_new() {
    B_OrdD_bytearray $tmp = malloc(sizeof(struct B_OrdD_bytearray));
    $tmp->$class = &B_OrdD_bytearrayG_methods;
    B_OrdD_bytearrayG_methods.__init__($tmp);
    return $tmp;
}
struct B_OrdD_bytearrayG_class B_OrdD_bytearrayG_methods;
B_NoneType B_SequenceD_bytearrayD___init__ (B_SequenceD_bytearray W_self) {
    ((B_NoneType (*) (B_Sequence, B_Collection, B_Times))B_SequenceG_methods.__init__)(((B_Sequence)W_self), ((B_Collection)B_CollectionD_SequenceD_bytearrayG_new(((B_Sequence)W_self))), ((B_Times)B_TimesD_SequenceD_bytearrayG_new(((B_Sequence)W_self))));
    return B_None;
}
B_NoneType B_SequenceD_bytearrayD_reverse (B_SequenceD_bytearray W_self, B_bytearray G_1p);
B_NoneType B_SequenceD_bytearrayD_append (B_SequenceD_bytearray W_self, B_bytearray G_1p, B_int G_2p);
B_NoneType B_SequenceD_bytearrayD_insert (B_SequenceD_bytearray W_self, B_bytearray G_1p, B_int G_2p, B_int G_3p);
B_Iterator B_SequenceD_bytearrayD___reversed__ (B_SequenceD_bytearray W_self, B_bytearray G_1p);
B_NoneType B_SequenceD_bytearrayD___delslice__ (B_SequenceD_bytearray W_self, B_bytearray G_1p, B_slice G_2p);
B_NoneType B_SequenceD_bytearrayD___setslice__ (B_SequenceD_bytearray W_self, B_bytearray G_1p, B_Iterable W_IterableE_682, B_slice G_2p, $WORD G_3p);
B_bytearray B_SequenceD_bytearrayD___getslice__ (B_SequenceD_bytearray W_self, B_bytearray G_1p, B_slice G_2p);
B_NoneType B_SequenceD_bytearrayD___delitem__ (B_SequenceD_bytearray W_self, B_bytearray G_1p, B_int G_2p);
B_NoneType B_SequenceD_bytearrayD___setitem__ (B_SequenceD_bytearray W_self, B_bytearray G_1p, B_int G_2p, B_int G_3p);
B_int B_SequenceD_bytearrayD___getitem__ (B_SequenceD_bytearray W_self, B_bytearray G_1p, B_int G_2p);
void B_SequenceD_bytearrayD___serialize__ (B_SequenceD_bytearray self, $Serial$state state) {
    $step_serialize(self->W_EqD_AD_Indexed, state);
    $step_serialize(self->W_Collection, state);
    $step_serialize(self->W_Times, state);
}
B_SequenceD_bytearray B_SequenceD_bytearrayD___deserialize__ (B_SequenceD_bytearray self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_SequenceD_bytearray));
            self->$class = &B_SequenceD_bytearrayG_methods;
            return self;
        }
        self = $DNEW(B_SequenceD_bytearray, state);
    }
    self->W_EqD_AD_Indexed = $step_deserialize(state);
    self->W_Collection = $step_deserialize(state);
    self->W_Times = $step_deserialize(state);
    return self;
}
B_SequenceD_bytearray B_SequenceD_bytearrayG_new() {
    B_SequenceD_bytearray $tmp = malloc(sizeof(struct B_SequenceD_bytearray));
    $tmp->$class = &B_SequenceD_bytearrayG_methods;
    B_SequenceD_bytearrayG_methods.__init__($tmp);
    return $tmp;
}
struct B_SequenceD_bytearrayG_class B_SequenceD_bytearrayG_methods;
B_NoneType B_CollectionD_SequenceD_bytearrayD___init__ (B_CollectionD_SequenceD_bytearray W_self, B_Sequence W_Sequence) {
    ((B_NoneType (*) (B_CollectionD_Sequence, B_Sequence))B_CollectionD_SequenceG_methods.__init__)(((B_CollectionD_Sequence)W_self), ((B_Sequence)W_Sequence));
    return B_None;
}
B_int B_CollectionD_SequenceD_bytearrayD___len__ (B_CollectionD_SequenceD_bytearray W_self, B_bytearray G_1p);
B_bytearray B_CollectionD_SequenceD_bytearrayD___fromiter__ (B_CollectionD_SequenceD_bytearray W_self, B_Iterable W_IterableE_687, $WORD G_1p);
B_Iterator B_CollectionD_SequenceD_bytearrayD___iter__ (B_CollectionD_SequenceD_bytearray W_self, B_bytearray G_1p);
void B_CollectionD_SequenceD_bytearrayD___serialize__ (B_CollectionD_SequenceD_bytearray self, $Serial$state state) {
    $step_serialize(self->W_Sequence, state);
}
B_CollectionD_SequenceD_bytearray B_CollectionD_SequenceD_bytearrayD___deserialize__ (B_CollectionD_SequenceD_bytearray self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_CollectionD_SequenceD_bytearray));
            self->$class = &B_CollectionD_SequenceD_bytearrayG_methods;
            return self;
        }
        self = $DNEW(B_CollectionD_SequenceD_bytearray, state);
    }
    self->W_Sequence = $step_deserialize(state);
    return self;
}
B_CollectionD_SequenceD_bytearray B_CollectionD_SequenceD_bytearrayG_new(B_Sequence G_1) {
    B_CollectionD_SequenceD_bytearray $tmp = malloc(sizeof(struct B_CollectionD_SequenceD_bytearray));
    $tmp->$class = &B_CollectionD_SequenceD_bytearrayG_methods;
    B_CollectionD_SequenceD_bytearrayG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct B_CollectionD_SequenceD_bytearrayG_class B_CollectionD_SequenceD_bytearrayG_methods;
B_NoneType B_TimesD_SequenceD_bytearrayD___init__ (B_TimesD_SequenceD_bytearray W_self, B_Sequence W_Sequence) {
    ((B_NoneType (*) (B_TimesD_Sequence, B_Sequence))B_TimesD_SequenceG_methods.__init__)(((B_TimesD_Sequence)W_self), ((B_Sequence)W_Sequence));
    return B_None;
}
B_bytearray B_TimesD_SequenceD_bytearrayD___mul__ (B_TimesD_SequenceD_bytearray W_self, B_bytearray G_1p, B_int G_2p);
B_bytearray B_TimesD_SequenceD_bytearrayD___add__ (B_TimesD_SequenceD_bytearray W_self, B_bytearray G_1p, B_bytearray G_2p);
void B_TimesD_SequenceD_bytearrayD___serialize__ (B_TimesD_SequenceD_bytearray self, $Serial$state state) {
    $step_serialize(self->W_Sequence, state);
}
B_TimesD_SequenceD_bytearray B_TimesD_SequenceD_bytearrayD___deserialize__ (B_TimesD_SequenceD_bytearray self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_TimesD_SequenceD_bytearray));
            self->$class = &B_TimesD_SequenceD_bytearrayG_methods;
            return self;
        }
        self = $DNEW(B_TimesD_SequenceD_bytearray, state);
    }
    self->W_Sequence = $step_deserialize(state);
    return self;
}
B_TimesD_SequenceD_bytearray B_TimesD_SequenceD_bytearrayG_new(B_Sequence G_1) {
    B_TimesD_SequenceD_bytearray $tmp = malloc(sizeof(struct B_TimesD_SequenceD_bytearray));
    $tmp->$class = &B_TimesD_SequenceD_bytearrayG_methods;
    B_TimesD_SequenceD_bytearrayG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct B_TimesD_SequenceD_bytearrayG_class B_TimesD_SequenceD_bytearrayG_methods;
B_NoneType B_ContainerD_bytearrayD___init__ (B_ContainerD_bytearray W_self) {
    B_Eq W_427 = ((B_Eq)B_OrdD_intG_new());
    ((B_NoneType (*) (B_Container, B_Eq))B_ContainerG_methods.__init__)(((B_Container)W_self), W_427);
    return B_None;
}
B_bool B_ContainerD_bytearrayD___containsnot__ (B_ContainerD_bytearray W_self, B_bytearray G_1p, B_int G_2p);
B_bool B_ContainerD_bytearrayD___contains__ (B_ContainerD_bytearray W_self, B_bytearray G_1p, B_int G_2p);
void B_ContainerD_bytearrayD___serialize__ (B_ContainerD_bytearray self, $Serial$state state) {
    $step_serialize(self->W_EqD_AD_Container, state);
}
B_ContainerD_bytearray B_ContainerD_bytearrayD___deserialize__ (B_ContainerD_bytearray self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_ContainerD_bytearray));
            self->$class = &B_ContainerD_bytearrayG_methods;
            return self;
        }
        self = $DNEW(B_ContainerD_bytearray, state);
    }
    self->W_EqD_AD_Container = $step_deserialize(state);
    return self;
}
struct B_ContainerD_bytearrayG_class B_ContainerD_bytearrayG_methods;
B_NoneType B_OrdD_bytesD___init__ (B_OrdD_bytes W_self) {
    ((B_NoneType (*) (B_Ord))B_OrdG_methods.__init__)(((B_Ord)W_self));
    return B_None;
}
B_bool B_OrdD_bytesD___lt__ (B_OrdD_bytes W_self, B_bytes G_1p, B_bytes G_2p);
B_bool B_OrdD_bytesD___eq__ (B_OrdD_bytes W_self, B_bytes G_1p, B_bytes G_2p);
void B_OrdD_bytesD___serialize__ (B_OrdD_bytes self, $Serial$state state) {
}
B_OrdD_bytes B_OrdD_bytesD___deserialize__ (B_OrdD_bytes self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_OrdD_bytes));
            self->$class = &B_OrdD_bytesG_methods;
            return self;
        }
        self = $DNEW(B_OrdD_bytes, state);
    }
    return self;
}
B_OrdD_bytes B_OrdD_bytesG_new() {
    B_OrdD_bytes $tmp = malloc(sizeof(struct B_OrdD_bytes));
    $tmp->$class = &B_OrdD_bytesG_methods;
    B_OrdD_bytesG_methods.__init__($tmp);
    return $tmp;
}
struct B_OrdD_bytesG_class B_OrdD_bytesG_methods;
B_NoneType B_SliceableD_bytesD___init__ (B_SliceableD_bytes W_self) {
    ((B_NoneType (*) (B_Sliceable))B_SliceableG_methods.__init__)(((B_Sliceable)W_self));
    return B_None;
}
B_NoneType B_SliceableD_bytesD___delslice__ (B_SliceableD_bytes W_self, B_bytes G_1p, B_slice G_2p);
B_NoneType B_SliceableD_bytesD___setslice__ (B_SliceableD_bytes W_self, B_bytes G_1p, B_Iterable W_IterableE_682, B_slice G_2p, $WORD G_3p);
B_bytes B_SliceableD_bytesD___getslice__ (B_SliceableD_bytes W_self, B_bytes G_1p, B_slice G_2p);
B_NoneType B_SliceableD_bytesD___delitem__ (B_SliceableD_bytes W_self, B_bytes G_1p, B_int G_2p);
B_NoneType B_SliceableD_bytesD___setitem__ (B_SliceableD_bytes W_self, B_bytes G_1p, B_int G_2p, B_int G_3p);
B_int B_SliceableD_bytesD___getitem__ (B_SliceableD_bytes W_self, B_bytes G_1p, B_int G_2p);
void B_SliceableD_bytesD___serialize__ (B_SliceableD_bytes self, $Serial$state state) {
    $step_serialize(self->W_EqD_AD_Indexed, state);
}
B_SliceableD_bytes B_SliceableD_bytesD___deserialize__ (B_SliceableD_bytes self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_SliceableD_bytes));
            self->$class = &B_SliceableD_bytesG_methods;
            return self;
        }
        self = $DNEW(B_SliceableD_bytes, state);
    }
    self->W_EqD_AD_Indexed = $step_deserialize(state);
    return self;
}
B_SliceableD_bytes B_SliceableD_bytesG_new() {
    B_SliceableD_bytes $tmp = malloc(sizeof(struct B_SliceableD_bytes));
    $tmp->$class = &B_SliceableD_bytesG_methods;
    B_SliceableD_bytesG_methods.__init__($tmp);
    return $tmp;
}
struct B_SliceableD_bytesG_class B_SliceableD_bytesG_methods;
B_NoneType B_ContainerD_bytesD___init__ (B_ContainerD_bytes W_self) {
    B_Eq W_438 = ((B_Eq)B_OrdD_intG_new());
    ((B_NoneType (*) (B_Container, B_Eq))B_ContainerG_methods.__init__)(((B_Container)W_self), W_438);
    return B_None;
}
B_bool B_ContainerD_bytesD___containsnot__ (B_ContainerD_bytes W_self, B_bytes G_1p, B_int G_2p);
B_bool B_ContainerD_bytesD___contains__ (B_ContainerD_bytes W_self, B_bytes G_1p, B_int G_2p);
B_int B_ContainerD_bytesD___len__ (B_ContainerD_bytes W_self, B_bytes G_1p);
B_bytes B_ContainerD_bytesD___fromiter__ (B_ContainerD_bytes W_self, B_Iterable W_IterableE_687, $WORD G_1p);
B_Iterator B_ContainerD_bytesD___iter__ (B_ContainerD_bytes W_self, B_bytes G_1p);
void B_ContainerD_bytesD___serialize__ (B_ContainerD_bytes self, $Serial$state state) {
    $step_serialize(self->W_EqD_AD_Container, state);
}
B_ContainerD_bytes B_ContainerD_bytesD___deserialize__ (B_ContainerD_bytes self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_ContainerD_bytes));
            self->$class = &B_ContainerD_bytesG_methods;
            return self;
        }
        self = $DNEW(B_ContainerD_bytes, state);
    }
    self->W_EqD_AD_Container = $step_deserialize(state);
    return self;
}
B_ContainerD_bytes B_ContainerD_bytesG_new() {
    B_ContainerD_bytes $tmp = malloc(sizeof(struct B_ContainerD_bytes));
    $tmp->$class = &B_ContainerD_bytesG_methods;
    B_ContainerD_bytesG_methods.__init__($tmp);
    return $tmp;
}
struct B_ContainerD_bytesG_class B_ContainerD_bytesG_methods;
B_NoneType B_TimesD_bytesD___init__ (B_TimesD_bytes W_self) {
    ((B_NoneType (*) (B_Times))B_TimesG_methods.__init__)(((B_Times)W_self));
    return B_None;
}
B_bytes B_TimesD_bytesD___mul__ (B_TimesD_bytes W_self, B_bytes G_1p, B_int G_2p);
B_bytes B_TimesD_bytesD___add__ (B_TimesD_bytes W_self, B_bytes G_1p, B_bytes G_2p);
void B_TimesD_bytesD___serialize__ (B_TimesD_bytes self, $Serial$state state) {
}
B_TimesD_bytes B_TimesD_bytesD___deserialize__ (B_TimesD_bytes self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_TimesD_bytes));
            self->$class = &B_TimesD_bytesG_methods;
            return self;
        }
        self = $DNEW(B_TimesD_bytes, state);
    }
    return self;
}
B_TimesD_bytes B_TimesD_bytesG_new() {
    B_TimesD_bytes $tmp = malloc(sizeof(struct B_TimesD_bytes));
    $tmp->$class = &B_TimesD_bytesG_methods;
    B_TimesD_bytesG_methods.__init__($tmp);
    return $tmp;
}
struct B_TimesD_bytesG_class B_TimesD_bytesG_methods;
B_NoneType B_HashableD_bytesD___init__ (B_HashableD_bytes W_self) {
    ((B_NoneType (*) (B_Hashable))B_HashableG_methods.__init__)(((B_Hashable)W_self));
    return B_None;
}
B_int B_HashableD_bytesD___hash__ (B_HashableD_bytes W_self, B_bytes G_1p);
void B_HashableD_bytesD___serialize__ (B_HashableD_bytes self, $Serial$state state) {
}
B_HashableD_bytes B_HashableD_bytesD___deserialize__ (B_HashableD_bytes self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_HashableD_bytes));
            self->$class = &B_HashableD_bytesG_methods;
            return self;
        }
        self = $DNEW(B_HashableD_bytes, state);
    }
    return self;
}
struct B_HashableD_bytesG_class B_HashableD_bytesG_methods;
$WORD B_abs (B_Real W_RealE_768, B_Number W_NumberE_767, $WORD x) {
    return (($WORD (*) (B_Number, $WORD, B_Real))W_NumberE_767->$class->__abs__)(W_NumberE_767, x, W_RealE_768);
}
B_bool B_all (B_Iterable W_IterableE_772, $WORD it) {
    B_Iterator N_iter = ((B_Iterator (*) (B_Iterable, $WORD))W_IterableE_772->$class->__iter__)(W_IterableE_772, it);
    $WORD N_1val = (($WORD (*) (B_Iterator))N_iter->$class->__next__)(N_iter);
    while ($ISNOTNONE(N_1val)->val) {
        // $WORD x = N_1val;
        // if ($NOT(B_bool, ((B_bool (*) (B_value))x->$class->__bool__)(x))->val) { These two lines manually replaced by the two following
        B_value x = (B_value)N_1val;
        if ($NOT(B_bool, (x->$class->__bool__)(x))->val) {
            return B_False;
        }
        N_1val = (($WORD (*) (B_Iterator))N_iter->$class->__next__)(N_iter);
    }
    return B_True;
}
B_bool B_any (B_Iterable W_IterableE_780, $WORD it) {
    B_Iterator N_2iter = ((B_Iterator (*) (B_Iterable, $WORD))W_IterableE_780->$class->__iter__)(W_IterableE_780, it);
    $WORD N_3val = (($WORD (*) (B_Iterator))N_2iter->$class->__next__)(N_2iter);
    while ($ISNOTNONE(N_3val)->val) {
        // $WORD x = N_3val;
        // if (((B_bool (*) ($WORD))x->$class->__bool__)(x)->val) {  These two lines manually replaced by the two following
        B_value x = (B_value)N_3val;
        if ((x->$class->__bool__)(x)->val) {
            return B_True;
        }
        N_3val = (($WORD (*) (B_Iterator))N_2iter->$class->__next__)(N_2iter);
    }
    return B_False;
}
B_str B_ascii (B_tuple args);
B_str B_bin (B_Integral W_IntegralE_791, $WORD x);
B_str B_chr (B_Integral W_IntegralE_794, $WORD i);
B_tuple B_divmod (B_Integral W_IntegralD_A, $WORD a, $WORD b) {
    return ((B_tuple (*) (B_Integral, $WORD, $WORD))W_IntegralD_A->$class->__divmod__)(W_IntegralD_A, a, b);
}
B_Iterator B_enumerate (B_Iterable W_IterableE_805, $WORD iterable, B_int start);
// B_Iterator B_filter (B_Iterable W_IterableE_814, $pure function, $WORD iterable);
B_int B_hash (B_Hashable W_HashableE_823, $WORD x) {
    return ((B_int (*) (B_Hashable, $WORD))W_HashableE_823->$class->__hash__)(W_HashableE_823, x);
}
B_str B_hex (B_Integral W_IntegralE_826, $WORD i);
B_Iterator B_iter (B_Iterable W_IterableE_830, $WORD x) {
    return ((B_Iterator (*) (B_Iterable, $WORD))W_IterableE_830->$class->__iter__)(W_IterableE_830, x);
}
B_int B_len (B_Collection W_CollectionE_838, $WORD x) {
    return ((B_int (*) (B_Collection, $WORD))W_CollectionE_838->$class->__len__)(W_CollectionE_838, x);
}
// B_Iterator B_map (B_Iterable W_IterableE_845, $pure function, $WORD iterable);
$WORD B_max (B_Ord W_OrdD_A, B_Iterable W_IterableE_855, $WORD iter, $WORD dflt);
$WORD B_min (B_Ord W_OrdD_A, B_Iterable W_IterableE_863, $WORD iter, $WORD dflt);
$WORD B_next (B_Iterator x) {
    return (($WORD (*) (B_Iterator))x->$class->__next__)(x);
}
B_int B_ord (B_str c);
$WORD B_pow (B_Number W_NumberD_A, $WORD a, $WORD b) {
    return (($WORD (*) (B_Number, $WORD, $WORD))W_NumberD_A->$class->__pow__)(W_NumberD_A, a, b);
}
// B_NoneType B_print (B_tuple args);
B_str B_repr (B_value x) {
    return ((B_str (*) (B_value))x->$class->__repr__)(x);
}
B_Iterator B_reversed (B_Sequence W_SequenceE_893, $WORD seq) {
    return ((B_Iterator (*) (B_Sequence, $WORD))W_SequenceE_893->$class->__reversed__)(W_SequenceE_893, seq);
}
$WORD B_round (B_Real W_RealD_A, $WORD x, B_int n) {
    return (($WORD (*) (B_Real, $WORD, B_int))W_RealD_A->$class->__round__)(W_RealD_A, x, n);
}
B_list B_sorted (B_Ord W_OrdD_A, B_Iterable W_IterableE_906, $WORD iter);
$WORD B_sum (B_Plus W_PlusD_A, B_Iterable W_IterableE_914, $WORD iter, $WORD start);
B_Iterator B_zip (B_Iterable W_IterableE_924, B_Iterable W_IterableE_923, $WORD a, $WORD b);
B_int B_gcd (B_int a, B_int b);
B_tuple B_xgcd (B_int a, B_int b);
/*
B_NoneType B_L_1procD___init__ (B_L_1proc L_self, B_Env self, B_str s) {
    L_self->self = self;
    L_self->s = s;
    return B_None;
}
$R B_L_1procD___call__ (B_L_1proc L_self, $Cont C_cont) {
    B_Env self = L_self->self;
    B_str s = L_self->s;
    return (($R (*) (B_Env, $Cont, B_str))self->$class->stdout_writeG_local)(self, C_cont, s);
}
$R B_L_1procD___exec__ (B_L_1proc L_self, $Cont C_cont) {
    return (($R (*) (B_L_1proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void B_L_1procD___serialize__ (B_L_1proc self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->s, state);
}
B_L_1proc B_L_1procD___deserialize__ (B_L_1proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_L_1proc));
            self->$class = &B_L_1procG_methods;
            return self;
        }
        self = $DNEW(B_L_1proc, state);
    }
    self->self = $step_deserialize(state);
    self->s = $step_deserialize(state);
    return self;
}
B_L_1proc B_L_1procG_new(B_Env G_1, B_str G_2) {
    B_L_1proc $tmp = malloc(sizeof(struct B_L_1proc));
    $tmp->$class = &B_L_1procG_methods;
    B_L_1procG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct B_L_1procG_class B_L_1procG_methods;
B_NoneType B_L_2procD___init__ (B_L_2proc L_self, B_Env self, $action cb) {
    L_self->self = self;
    L_self->cb = cb;
    return B_None;
}
$R B_L_2procD___call__ (B_L_2proc L_self, $Cont C_cont) {
    B_Env self = L_self->self;
    $action cb = L_self->cb;
    return (($R (*) (B_Env, $Cont, $action))self->$class->stdin_installG_local)(self, C_cont, cb);
}
$R B_L_2procD___exec__ (B_L_2proc L_self, $Cont C_cont) {
    return (($R (*) (B_L_2proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void B_L_2procD___serialize__ (B_L_2proc self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->cb, state);
}
B_L_2proc B_L_2procD___deserialize__ (B_L_2proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_L_2proc));
            self->$class = &B_L_2procG_methods;
            return self;
        }
        self = $DNEW(B_L_2proc, state);
    }
    self->self = $step_deserialize(state);
    self->cb = $step_deserialize(state);
    return self;
}
B_L_2proc B_L_2procG_new(B_Env G_1, $action G_2) {
    B_L_2proc $tmp = malloc(sizeof(struct B_L_2proc));
    $tmp->$class = &B_L_2procG_methods;
    B_L_2procG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct B_L_2procG_class B_L_2procG_methods;
B_NoneType B_L_3procD___init__ (B_L_3proc L_self, B_Env self, B_int n) {
    L_self->self = self;
    L_self->n = n;
    return B_None;
}
$R B_L_3procD___call__ (B_L_3proc L_self, $Cont C_cont) {
    B_Env self = L_self->self;
    B_int n = L_self->n;
    return (($R (*) (B_Env, $Cont, B_int))self->$class->exitG_local)(self, C_cont, n);
}
$R B_L_3procD___exec__ (B_L_3proc L_self, $Cont C_cont) {
    return (($R (*) (B_L_3proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void B_L_3procD___serialize__ (B_L_3proc self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->n, state);
}
B_L_3proc B_L_3procD___deserialize__ (B_L_3proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_L_3proc));
            self->$class = &B_L_3procG_methods;
            return self;
        }
        self = $DNEW(B_L_3proc, state);
    }
    self->self = $step_deserialize(state);
    self->n = $step_deserialize(state);
    return self;
}
B_L_3proc B_L_3procG_new(B_Env G_1, B_int G_2) {
    B_L_3proc $tmp = malloc(sizeof(struct B_L_3proc));
    $tmp->$class = &B_L_3procG_methods;
    B_L_3procG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct B_L_3procG_class B_L_3procG_methods;
$R B_L_4C_1cont (B_Env G_act, $Cont C_cont, B_NoneType C_2res) {
    return $RU_CONT(C_cont, G_act);
}
B_NoneType B_L_5ContD___init__ (B_L_5Cont L_self, B_Env G_act, $Cont C_cont) {
    L_self->G_act = G_act;
    L_self->C_cont = C_cont;
    return B_None;
}
$R B_L_5ContD___call__ (B_L_5Cont L_self, B_NoneType G_1) {
    B_Env G_act = L_self->G_act;
    $Cont C_cont = L_self->C_cont;
    return B_L_4C_1cont(G_act, C_cont, G_1);
}
void B_L_5ContD___serialize__ (B_L_5Cont self, $Serial$state state) {
    $step_serialize(self->G_act, state);
    $step_serialize(self->C_cont, state);
}
B_L_5Cont B_L_5ContD___deserialize__ (B_L_5Cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_L_5Cont));
            self->$class = &B_L_5ContG_methods;
            return self;
        }
        self = $DNEW(B_L_5Cont, state);
    }
    self->G_act = $step_deserialize(state);
    self->C_cont = $step_deserialize(state);
    return self;
}
B_L_5Cont B_L_5ContG_new(B_Env G_1, $Cont G_2) {
    B_L_5Cont $tmp = malloc(sizeof(struct B_L_5Cont));
    $tmp->$class = &B_L_5ContG_methods;
    B_L_5ContG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct B_L_5ContG_class B_L_5ContG_methods;
B_NoneType B_L_6procD___init__ (B_L_6proc L_self, B_Env G_act, B_WorldAuth token, B_list args) {
    L_self->G_act = G_act;
    L_self->token = token;
    L_self->args = args;
    return B_None;
}
$R B_L_6procD___call__ (B_L_6proc L_self, $Cont C_cont) {
    B_Env G_act = L_self->G_act;
    B_WorldAuth token = L_self->token;
    B_list args = L_self->args;
    return (($R (*) (B_Env, $Cont, B_WorldAuth, B_list))G_act->$class->__init__)(G_act, C_cont, token, args);
}
$R B_L_6procD___exec__ (B_L_6proc L_self, $Cont C_cont) {
    return (($R (*) (B_L_6proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void B_L_6procD___serialize__ (B_L_6proc self, $Serial$state state) {
    $step_serialize(self->G_act, state);
    $step_serialize(self->token, state);
    $step_serialize(self->args, state);
}
B_L_6proc B_L_6procD___deserialize__ (B_L_6proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_L_6proc));
            self->$class = &B_L_6procG_methods;
            return self;
        }
        self = $DNEW(B_L_6proc, state);
    }
    self->G_act = $step_deserialize(state);
    self->token = $step_deserialize(state);
    self->args = $step_deserialize(state);
    return self;
}
B_L_6proc B_L_6procG_new(B_Env G_1, B_WorldAuth G_2, B_list G_3) {
    B_L_6proc $tmp = malloc(sizeof(struct B_L_6proc));
    $tmp->$class = &B_L_6procG_methods;
    B_L_6procG_methods.__init__($tmp, G_1, G_2, G_3);
    return $tmp;
}
struct B_L_6procG_class B_L_6procG_methods;
void B_WorldAuthD___serialize__ (B_WorldAuth self, $Serial$state state) {
}
B_WorldAuth B_WorldAuthD___deserialize__ (B_WorldAuth self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_WorldAuth));
            self->$class = &B_WorldAuthG_methods;
            return self;
        }
        self = $DNEW(B_WorldAuth, state);
    }
    return self;
}
struct B_WorldAuthG_class B_WorldAuthG_methods;
$R B_EnvD___init__ (B_Env self, $Cont C_cont, B_WorldAuth token, B_list args) {
    self->token = token;
    self->args = args;
    self->auth = self->token;
    self->argv = self->args;
    return $RU_CONT(C_cont, B_None);
}
$R B_EnvD_stdout_writeG_local (B_Env self, $Cont C_cont, B_str s);
$R B_EnvD_stdin_installG_local (B_Env self, $Cont C_cont, $action cb);
$R B_EnvD_exitG_local (B_Env self, $Cont C_cont, B_int n);
B_Msg B_EnvD_stdout_write (B_Env self, B_str s) {
    return $ASYNC((($Actor)self), (($Cont)B_L_1procG_new(((B_Env)self), s)));
}
B_Msg B_EnvD_stdin_install (B_Env self, $action cb) {
    return $ASYNC((($Actor)self), (($Cont)B_L_2procG_new(((B_Env)self), cb)));
}
B_Msg B_EnvD_exit (B_Env self, B_int n) {
    return $ASYNC((($Actor)self), (($Cont)B_L_3procG_new(((B_Env)self), n)));
}
void B_EnvD___serialize__ (B_Env self, $Serial$state state) {
    $ActorG_methods.__serialize__(($Actor)self, state);
    $step_serialize(self->token, state);
    $step_serialize(self->args, state);
    $step_serialize(self->auth, state);
    $step_serialize(self->argv, state);
}
B_Env B_EnvD___deserialize__ (B_Env self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_Env));
            self->$class = &B_EnvG_methods;
            return self;
        }
        self = $DNEW(B_Env, state);
    }
    $ActorG_methods.__deserialize__(($Actor)self, state);
    self->token = $step_deserialize(state);
    self->args = $step_deserialize(state);
    self->auth = $step_deserialize(state);
    self->argv = $step_deserialize(state);
    return self;
}
$R B_EnvG_new($Cont G_1, B_WorldAuth G_2, B_list G_3) {
    B_Env $tmp = malloc(sizeof(struct B_Env));
    $tmp->$class = &B_EnvG_methods;
    return B_EnvG_methods.__init__($tmp, G_1, G_2, $CONSTCONT(G_3, $tmp));
}
struct B_EnvG_class B_EnvG_methods;
$R B_EnvG_newact ($Cont C_cont, B_WorldAuth token, B_list args) {
    B_Env G_act = $NEWACTOR(B_Env);
    return $AWAIT((($Cont)B_L_5ContG_new(G_act, C_cont)), $ASYNC((($Actor)G_act), (($Cont)B_L_6procG_new(G_act, token, args))));
}
*/
int B_done$ = 0;
void B___init__ () {
    if (B_done$) return;
    B_done$ = 1;
    //    B___ext_init__ ();
    {
        B_valueG_methods.$GCINFO = "B_value";
        B_valueG_methods.$superclass = NULL;
        B_valueG_methods.__init__ = B_valueD___init__;
        B_valueG_methods.__bool__ = B_valueD___bool__;
        B_valueG_methods.__str__ = B_valueD___str__;
        B_valueG_methods.__repr__ = B_valueD___str__;
        B_valueG_methods.__serialize__ = B_valueD___serialize__;
        B_valueG_methods.__deserialize__ = B_valueD___deserialize__;
    }
    {
        B_objectG_methods.$GCINFO = "B_object";
        B_objectG_methods.$superclass = ($SuperG_class)&B_valueG_methods;
        B_objectG_methods.__init__ = (B_NoneType (*) (B_object))B_valueG_methods.__init__;
        B_objectG_methods.__bool__ = (B_bool (*) (B_object))B_valueG_methods.__bool__;
        B_objectG_methods.__str__ = (B_str (*) (B_object))B_valueG_methods.__str__;
        B_objectG_methods.__repr__ = (B_str (*) (B_object))B_valueG_methods.__repr__;
        B_objectG_methods.__serialize__ = B_objectD___serialize__;
        B_objectG_methods.__deserialize__ = B_objectD___deserialize__;
    }
    {
        B_atomG_methods.$GCINFO = "B_atom";
        B_atomG_methods.$superclass = ($SuperG_class)&B_valueG_methods;
        B_atomG_methods.__init__ = (B_NoneType (*) (B_atom))B_valueG_methods.__init__;
        B_atomG_methods.__bool__ = (B_bool (*) (B_atom))B_valueG_methods.__bool__;
        B_atomG_methods.__str__ = (B_str (*) (B_atom))B_valueG_methods.__str__;
        B_atomG_methods.__repr__ = (B_str (*) (B_atom))B_valueG_methods.__str__;
        B_atomG_methods.__serialize__ = B_atomD___serialize__;
        B_atomG_methods.__deserialize__ = B_atomD___deserialize__;
    }
    {
        B_intG_methods.$GCINFO = "B_int";
        B_intG_methods.$superclass = ($SuperG_class)&B_atomG_methods;
        B_intG_methods.__bool__ = B_intD___bool__;
        B_intG_methods.__str__ = B_intD___str__;
        B_intG_methods.__repr__ = B_intD___str__;
        B_intG_methods.__init__ = B_intD___init__;
        B_intG_methods.__serialize__ = B_intD___serialize__;
        B_intG_methods.__deserialize__ = B_intD___deserialize__;
    }
    {
        B_i64G_methods.$GCINFO = "B_i64";
        B_i64G_methods.$superclass = ($SuperG_class)&B_atomG_methods;
        B_i64G_methods.__bool__ = B_i64D___bool__;
        B_i64G_methods.__str__ = B_i64D___str__;  
        B_i64G_methods.__str__ = B_i64D___str__; 
        B_i64G_methods.__init__ = B_i64D___init__;
        B_i64G_methods.__serialize__ = B_i64D___serialize__;
        B_i64G_methods.__deserialize__ = B_i64D___deserialize__;
    }
    {
        B_floatG_methods.$GCINFO = "B_float";
        B_floatG_methods.$superclass = ($SuperG_class)&B_atomG_methods;
        B_floatG_methods.__bool__ = B_floatD___bool__;
        B_floatG_methods.__str__ = B_floatD___str__;
        B_floatG_methods.__repr__ = B_floatD___str__;
        B_floatG_methods.__init__ = B_floatD___init__;
        B_floatG_methods.__serialize__ = B_floatD___serialize__;
        B_floatG_methods.__deserialize__ = B_floatD___deserialize__;
    }
    {
        B_boolG_methods.$GCINFO = "B_bool";
        B_boolG_methods.$superclass = ($SuperG_class)&B_atomG_methods;
        B_boolG_methods.__bool__ = B_boolD___bool__;
        B_boolG_methods.__str__ = B_boolD___str__;
        B_boolG_methods.__repr__ = B_boolD___str__;
        B_boolG_methods.__init__ = B_boolD___init__;
        B_boolG_methods.__serialize__ = B_boolD___serialize__;
        B_boolG_methods.__deserialize__ = B_boolD___deserialize__;
    }
    {
        B_sliceG_methods.$GCINFO = "B_slice";
        B_sliceG_methods.$superclass = ($SuperG_class)&B_valueG_methods;
        B_sliceG_methods.__bool__ = (B_bool (*) (B_slice))B_valueG_methods.__bool__;
        B_sliceG_methods.__str__ = (B_str (*) (B_slice))B_valueG_methods.__str__;
        B_sliceG_methods.__repr__ = (B_str (*) (B_slice))B_valueG_methods.__repr__;
        B_sliceG_methods.__init__ = B_sliceD___init__;
        B_sliceG_methods.__serialize__ = B_sliceD___serialize__;
        B_sliceG_methods.__deserialize__ = B_sliceD___deserialize__;
    }
    {
        B_listG_methods.$GCINFO = "B_list";
        B_listG_methods.$superclass = ($SuperG_class)&B_objectG_methods;
        B_listG_methods.__bool__ = (B_bool (*) (B_list))B_valueG_methods.__bool__;
        B_listG_methods.__str__ = B_listD___str__;
        B_listG_methods.__repr__ = B_listD___str__;
        B_listG_methods.__init__ = B_listD___init__;
        B_listG_methods.copy = B_listD_copy;
        B_listG_methods.__serialize__ = B_listD___serialize__;
        B_listG_methods.__deserialize__ = B_listD___deserialize__;
    }
    {
        B_rangeG_methods.$GCINFO = "B_range";
        B_rangeG_methods.$superclass = ($SuperG_class)&B_valueG_methods;
        B_rangeG_methods.__bool__ = (B_bool (*) (B_range))B_valueG_methods.__bool__;
        B_rangeG_methods.__str__ = (B_str (*) (B_range))B_valueG_methods.__str__;
        B_rangeG_methods.__repr__ = (B_str (*) (B_range))B_valueG_methods.__repr__;
        B_rangeG_methods.__init__ = B_rangeD___init__;
        B_rangeG_methods.__serialize__ = B_rangeD___serialize__;
        B_rangeG_methods.__deserialize__ = B_rangeD___deserialize__;
    }
    {
        B_IteratorG_methods.$GCINFO = "B_Iterator";
        B_IteratorG_methods.$superclass = ($SuperG_class)&B_objectG_methods;
        B_IteratorG_methods.__bool__ = (B_bool (*) (B_Iterator))B_valueG_methods.__bool__;
        B_IteratorG_methods.__str__ = (B_str (*) (B_Iterator))B_valueG_methods.__str__;
        B_IteratorG_methods.__repr__ = (B_str (*) (B_Iterator))B_valueG_methods.__repr__;
        B_IteratorG_methods.__serialize__ = B_IteratorD___serialize__;
        B_IteratorG_methods.__deserialize__ = B_IteratorD___deserialize__;
    }
    {
        B_IterableG_methods.$GCINFO = "B_Iterable";
        B_IterableG_methods.$superclass = ($SuperG_class)&B_valueG_methods;
        B_IterableG_methods.__bool__ = (B_bool (*) (B_Iterable))B_valueG_methods.__bool__;
        B_IterableG_methods.__str__ = (B_str (*) (B_Iterable))B_valueG_methods.__str__;
        B_IterableG_methods.__repr__ = (B_str (*) (B_Iterable))B_valueG_methods.__repr__;
        B_IterableG_methods.__init__ = B_IterableD___init__;
        B_IterableG_methods.__serialize__ = B_IterableD___serialize__;
        B_IterableG_methods.__deserialize__ = B_IterableD___deserialize__;
        $register(&B_IterableG_methods);
    }
    {
        B_strG_methods.$GCINFO = "B_str";
        B_strG_methods.$superclass = ($SuperG_class)&B_atomG_methods;
        B_strG_methods.__bool__ = (B_bool (*) (B_str))B_valueG_methods.__bool__;
        B_strG_methods.__str__ = B_strD___str__;
        B_strG_methods.__repr__ = B_strD___repr__;
        B_strG_methods.__init__ = B_strD___init__;
        B_strG_methods.capitalize = B_strD_capitalize;
        B_strG_methods.center = B_strD_center;
        B_strG_methods.count = B_strD_count;
        B_strG_methods.encode = B_strD_encode;
        B_strG_methods.endswith = B_strD_endswith;
        B_strG_methods.expandtabs = B_strD_expandtabs;
        B_strG_methods.find = B_strD_find;
        B_strG_methods.index = B_strD_index;
        B_strG_methods.isalnum = B_strD_isalnum;
        B_strG_methods.isalpha = B_strD_isalpha;
        B_strG_methods.isascii = B_strD_isascii;
        B_strG_methods.isdecimal = B_strD_isdecimal;
        B_strG_methods.islower = B_strD_islower;
        B_strG_methods.isprintable = B_strD_isprintable;
        B_strG_methods.isspace = B_strD_isspace;
        B_strG_methods.istitle = B_strD_istitle;
        B_strG_methods.isupper = B_strD_isupper;
        B_strG_methods.join = B_strD_join;
        B_strG_methods.ljust = B_strD_ljust;
        B_strG_methods.lower = B_strD_lower;
        B_strG_methods.lstrip = B_strD_lstrip;
        B_strG_methods.partition = B_strD_partition;
        B_strG_methods.replace = B_strD_replace;
        B_strG_methods.rfind = B_strD_rfind;
        B_strG_methods.rindex = B_strD_rindex;
        B_strG_methods.rjust = B_strD_rjust;
        B_strG_methods.rpartition = B_strD_rpartition;
        B_strG_methods.rstrip = B_strD_rstrip;
        B_strG_methods.split = B_strD_split;
        B_strG_methods.splitlines = B_strD_splitlines;
        B_strG_methods.startswith = B_strD_startswith;
        B_strG_methods.strip = B_strD_strip;
        B_strG_methods.upper = B_strD_upper;
        B_strG_methods.zfill = B_strD_zfill;
        B_strG_methods.__serialize__ = B_strD___serialize__;
        B_strG_methods.__deserialize__ = B_strD___deserialize__;
    }
    {
        B_bytesG_methods.$GCINFO = "B_bytes";
        B_bytesG_methods.$superclass = ($SuperG_class)&B_valueG_methods;
        B_bytesG_methods.__bool__ = (B_bool (*) (B_bytes))B_valueG_methods.__bool__;
        B_bytesG_methods.__str__ = B_bytesD___str__;
        B_bytesG_methods.__repr__ = B_bytesD___str__;
        B_bytesG_methods.__init__ = B_bytesD___init__;
        B_bytesG_methods.capitalize = B_bytesD_capitalize;
        B_bytesG_methods.center = B_bytesD_center;
        B_bytesG_methods.count = B_bytesD_count;
        B_bytesG_methods.decode = B_bytesD_decode;
        B_bytesG_methods.endswith = B_bytesD_endswith;
        B_bytesG_methods.expandtabs = B_bytesD_expandtabs;
        B_bytesG_methods.find = B_bytesD_find;
        B_bytesG_methods.index = B_bytesD_index;
        B_bytesG_methods.isalnum = B_bytesD_isalnum;
        B_bytesG_methods.isalpha = B_bytesD_isalpha;
        B_bytesG_methods.isascii = B_bytesD_isascii;
        B_bytesG_methods.isdigit = B_bytesD_isdigit;
        B_bytesG_methods.islower = B_bytesD_islower;
        B_bytesG_methods.isspace = B_bytesD_isspace;
        B_bytesG_methods.istitle = B_bytesD_istitle;
        B_bytesG_methods.isupper = B_bytesD_isupper;
        B_bytesG_methods.join = B_bytesD_join;
        B_bytesG_methods.ljust = B_bytesD_ljust;
        B_bytesG_methods.lower = B_bytesD_lower;
        B_bytesG_methods.lstrip = B_bytesD_lstrip;
        B_bytesG_methods.partition = B_bytesD_partition;
        B_bytesG_methods.replace = B_bytesD_replace;
        B_bytesG_methods.rfind = B_bytesD_rfind;
        B_bytesG_methods.rindex = B_bytesD_rindex;
        B_bytesG_methods.rjust = B_bytesD_rjust;
        B_bytesG_methods.rpartition = B_bytesD_rpartition;
        B_bytesG_methods.rstrip = B_bytesD_rstrip;
        B_bytesG_methods.split = B_bytesD_split;
        B_bytesG_methods.splitlines = B_bytesD_splitlines;
        B_bytesG_methods.startswith = B_bytesD_startswith;
        B_bytesG_methods.strip = B_bytesD_strip;
        B_bytesG_methods.upper = B_bytesD_upper;
        B_bytesG_methods.zfill = B_bytesD_zfill;
        B_bytesG_methods.__serialize__ = B_bytesD___serialize__;
        B_bytesG_methods.__deserialize__ = B_bytesD___deserialize__;
    }
    {
        B_bytearrayG_methods.$GCINFO = "B_bytearray";
        B_bytearrayG_methods.$superclass = ($SuperG_class)&B_objectG_methods;
        B_bytearrayG_methods.__bool__ = (B_bool (*) (B_bytearray))B_valueG_methods.__bool__;
        B_bytearrayG_methods.__str__ = B_bytearrayD___str__;
        B_bytearrayG_methods.__repr__ = B_bytearrayD___str__;
        B_bytearrayG_methods.__init__ = B_bytearrayD___init__;
        B_bytearrayG_methods.capitalize = B_bytearrayD_capitalize;
        B_bytearrayG_methods.center = B_bytearrayD_center;
        B_bytearrayG_methods.count = B_bytearrayD_count;
        B_bytearrayG_methods.decode = B_bytearrayD_decode;
        B_bytearrayG_methods.endswith = B_bytearrayD_endswith;
        B_bytearrayG_methods.expandtabs = B_bytearrayD_expandtabs;
        B_bytearrayG_methods.find = B_bytearrayD_find;
        B_bytearrayG_methods.index = B_bytearrayD_index;
        B_bytearrayG_methods.isalnum = B_bytearrayD_isalnum;
        B_bytearrayG_methods.isalpha = B_bytearrayD_isalpha;
        B_bytearrayG_methods.isascii = B_bytearrayD_isascii;
        B_bytearrayG_methods.isdigit = B_bytearrayD_isdigit;
        B_bytearrayG_methods.islower = B_bytearrayD_islower;
        B_bytearrayG_methods.isspace = B_bytearrayD_isspace;
        B_bytearrayG_methods.istitle = B_bytearrayD_istitle;
        B_bytearrayG_methods.isupper = B_bytearrayD_isupper;
        B_bytearrayG_methods.join = B_bytearrayD_join;
        B_bytearrayG_methods.ljust = B_bytearrayD_ljust;
        B_bytearrayG_methods.lower = B_bytearrayD_lower;
        B_bytearrayG_methods.lstrip = B_bytearrayD_lstrip;
        B_bytearrayG_methods.partition = B_bytearrayD_partition;
        B_bytearrayG_methods.replace = B_bytearrayD_replace;
        B_bytearrayG_methods.rfind = B_bytearrayD_rfind;
        B_bytearrayG_methods.rindex = B_bytearrayD_rindex;
        B_bytearrayG_methods.rjust = B_bytearrayD_rjust;
        B_bytearrayG_methods.rpartition = B_bytearrayD_rpartition;
        B_bytearrayG_methods.rstrip = B_bytearrayD_rstrip;
        B_bytearrayG_methods.split = B_bytearrayD_split;
        B_bytearrayG_methods.splitlines = B_bytearrayD_splitlines;
        B_bytearrayG_methods.startswith = B_bytearrayD_startswith;
        B_bytearrayG_methods.strip = B_bytearrayD_strip;
        B_bytearrayG_methods.upper = B_bytearrayD_upper;
        B_bytearrayG_methods.zfill = B_bytearrayD_zfill;
        B_bytearrayG_methods.__serialize__ = B_bytearrayD___serialize__;
        B_bytearrayG_methods.__deserialize__ = B_bytearrayD___deserialize__;
    }
    /*
    {
        B_MsgG_methods.$GCINFO = "B_Msg";
        B_MsgG_methods.$superclass = ($SuperG_class)&B_valueG_methods;
        B_MsgG_methods.__bool__ = (B_bool (*) (B_Msg))B_valueG_methods.__bool__;
        B_MsgG_methods.__str__ = (B_str (*) (B_Msg))B_valueG_methods.__str__;
        B_MsgG_methods.__repr__ = (B_str (*) (B_Msg))B_valueG_methods.__repr__;
        B_MsgG_methods.__serialize__ = B_MsgD___serialize__;
        B_MsgG_methods.__deserialize__ = B_MsgD___deserialize__;
        $register(&B_MsgG_methods);
    }
    */
    {
        B_BaseExceptionG_methods.$GCINFO = "B_BaseException";
        B_BaseExceptionG_methods.$superclass = ($SuperG_class)&B_valueG_methods;
        B_BaseExceptionG_methods.__bool__ = (B_bool (*) (B_BaseException))B_valueG_methods.__bool__;
        B_BaseExceptionG_methods.__str__ = B_BaseExceptionD___str__;
        B_BaseExceptionG_methods.__repr__ = B_BaseExceptionD___str__;
        B_BaseExceptionG_methods.__init__ = B_BaseExceptionD___init__;
        B_BaseExceptionG_methods.__serialize__ = B_BaseExceptionD___serialize__;
        B_BaseExceptionG_methods.__deserialize__ = B_BaseExceptionD___deserialize__; 
    }
    {
        B_SystemExitG_methods.$GCINFO = "B_SystemExit";
        B_SystemExitG_methods.$superclass = ($SuperG_class)&B_BaseExceptionG_methods;
        B_SystemExitG_methods.__init__ = (B_NoneType (*) (B_SystemExit, B_str))B_BaseExceptionG_methods.__init__;
        B_SystemExitG_methods.__bool__ = (B_bool (*) (B_SystemExit))B_valueG_methods.__bool__;
        B_SystemExitG_methods.__str__ = B_SystemExitD___str__;
        B_SystemExitG_methods.__repr__ = B_SystemExitD___str__;
        B_SystemExitG_methods.__serialize__ = B_SystemExitD___serialize__;
        B_SystemExitG_methods.__deserialize__ = B_SystemExitD___deserialize__;
    }
    {
        B_KeyboardInterruptG_methods.$GCINFO = "B_KeyboardInterrupt";
        B_KeyboardInterruptG_methods.$superclass = ($SuperG_class)&B_BaseExceptionG_methods;
        B_KeyboardInterruptG_methods.__init__ = (B_NoneType (*) (B_KeyboardInterrupt, B_str))B_BaseExceptionG_methods.__init__;
        B_KeyboardInterruptG_methods.__bool__ = (B_bool (*) (B_KeyboardInterrupt))B_valueG_methods.__bool__;
        B_KeyboardInterruptG_methods.__str__ = B_KeyboardInterruptD___str__;
        B_KeyboardInterruptG_methods.__repr__ = B_KeyboardInterruptD___str__;
        B_KeyboardInterruptG_methods.__serialize__ = B_KeyboardInterruptD___serialize__;
        B_KeyboardInterruptG_methods.__deserialize__ = B_KeyboardInterruptD___deserialize__;
    }
    {
        B_ExceptionG_methods.$GCINFO = "B_Exception";
        B_ExceptionG_methods.$superclass = ($SuperG_class)&B_BaseExceptionG_methods;
        B_ExceptionG_methods.__init__ = (B_NoneType (*) (B_Exception, B_str))B_BaseExceptionG_methods.__init__;
        B_ExceptionG_methods.__bool__ = (B_bool (*) (B_Exception))B_valueG_methods.__bool__;
        B_ExceptionG_methods.__str__ = B_ExceptionD___str__;
        B_ExceptionG_methods.__repr__ = B_ExceptionD___str__;
        B_ExceptionG_methods.__serialize__ = B_ExceptionD___serialize__;
        B_ExceptionG_methods.__deserialize__ = B_ExceptionD___deserialize__;
    }
    {
        B_AssertionErrorG_methods.$GCINFO = "B_AssertionError";
        B_AssertionErrorG_methods.$superclass = ($SuperG_class)&B_ExceptionG_methods;
        B_AssertionErrorG_methods.__init__ = (B_NoneType (*) (B_AssertionError, B_str))B_BaseExceptionG_methods.__init__;
        B_AssertionErrorG_methods.__bool__ = (B_bool (*) (B_AssertionError))B_valueG_methods.__bool__;
        B_AssertionErrorG_methods.__str__ = B_AssertionErrorD___str__;
        B_AssertionErrorG_methods.__repr__ = B_AssertionErrorD___str__;
        B_AssertionErrorG_methods.__serialize__ = B_AssertionErrorD___serialize__;
        B_AssertionErrorG_methods.__deserialize__ = B_AssertionErrorD___deserialize__;
    }
    {
        B_LookupErrorG_methods.$GCINFO = "B_LookupError";
        B_LookupErrorG_methods.$superclass = ($SuperG_class)&B_ExceptionG_methods;
        B_LookupErrorG_methods.__init__ = (B_NoneType (*) (B_LookupError, B_str))B_BaseExceptionG_methods.__init__;
        B_LookupErrorG_methods.__bool__ = (B_bool (*) (B_LookupError))B_valueG_methods.__bool__;
        B_LookupErrorG_methods.__str__ = B_LookupErrorD___str__;
        B_LookupErrorG_methods.__repr__ = B_LookupErrorD___str__;
        B_LookupErrorG_methods.__serialize__ = B_LookupErrorD___serialize__;
        B_LookupErrorG_methods.__deserialize__ = B_LookupErrorD___deserialize__;
    }
    {
        B_IndexErrorG_methods.$GCINFO = "B_IndexError";
        B_IndexErrorG_methods.$superclass = ($SuperG_class)&B_LookupErrorG_methods;
        B_IndexErrorG_methods.__init__ = (B_NoneType (*) (B_IndexError, B_str))B_BaseExceptionG_methods.__init__;
        B_IndexErrorG_methods.__bool__ = (B_bool (*) (B_IndexError))B_valueG_methods.__bool__;
        B_IndexErrorG_methods.__str__ = B_IndexErrorD___str__;
        B_IndexErrorG_methods.__repr__ = B_IndexErrorD___str__;
        B_IndexErrorG_methods.__serialize__ = B_IndexErrorD___serialize__;
        B_IndexErrorG_methods.__deserialize__ = B_IndexErrorD___deserialize__;
    }
    {
        B_KeyErrorG_methods.$GCINFO = "B_KeyError";
        B_KeyErrorG_methods.$superclass = ($SuperG_class)&B_LookupErrorG_methods;
        B_KeyErrorG_methods.__init__ = (B_NoneType (*) (B_KeyError, B_str))B_BaseExceptionG_methods.__init__;
        B_KeyErrorG_methods.__bool__ = (B_bool (*) (B_KeyError))B_valueG_methods.__bool__;
        B_KeyErrorG_methods.__str__ = B_KeyErrorD___str__;
        B_KeyErrorG_methods.__repr__ = B_KeyErrorD___str__;
        B_KeyErrorG_methods.__serialize__ = B_KeyErrorD___serialize__;
        B_KeyErrorG_methods.__deserialize__ = B_KeyErrorD___deserialize__;
    }
    {
        B_MemoryErrorG_methods.$GCINFO = "B_MemoryError";
        B_MemoryErrorG_methods.$superclass = ($SuperG_class)&B_ExceptionG_methods;
        B_MemoryErrorG_methods.__init__ = (B_NoneType (*) (B_MemoryError, B_str))B_BaseExceptionG_methods.__init__;
        B_MemoryErrorG_methods.__bool__ = (B_bool (*) (B_MemoryError))B_valueG_methods.__bool__;
        B_MemoryErrorG_methods.__str__ = B_MemoryErrorD___str__;
        B_MemoryErrorG_methods.__repr__ = B_MemoryErrorD___str__;
        B_MemoryErrorG_methods.__serialize__ = B_MemoryErrorD___serialize__;
        B_MemoryErrorG_methods.__deserialize__ = B_MemoryErrorD___deserialize__;
    }
    {
        B_OSErrorG_methods.$GCINFO = "B_OSError";
        B_OSErrorG_methods.$superclass = ($SuperG_class)&B_ExceptionG_methods;
        B_OSErrorG_methods.__init__ = (B_NoneType (*) (B_OSError, B_str))B_BaseExceptionG_methods.__init__;
        B_OSErrorG_methods.__bool__ = (B_bool (*) (B_OSError))B_valueG_methods.__bool__;
        B_OSErrorG_methods.__str__ = B_OSErrorD___str__;
        B_OSErrorG_methods.__repr__ = B_OSErrorD___str__;
        B_OSErrorG_methods.__serialize__ = B_OSErrorD___serialize__;
        B_OSErrorG_methods.__deserialize__ = B_OSErrorD___deserialize__;
    }
    {
        B_RuntimeErrorG_methods.$GCINFO = "B_RuntimeError";
        B_RuntimeErrorG_methods.$superclass = ($SuperG_class)&B_ExceptionG_methods;
        B_RuntimeErrorG_methods.__init__ = (B_NoneType (*) (B_RuntimeError, B_str))B_BaseExceptionG_methods.__init__;
        B_RuntimeErrorG_methods.__bool__ = (B_bool (*) (B_RuntimeError))B_valueG_methods.__bool__;
        B_RuntimeErrorG_methods.__str__ = B_RuntimeErrorD___str__;
        B_RuntimeErrorG_methods.__repr__ = B_RuntimeErrorD___str__;
        B_RuntimeErrorG_methods.__serialize__ = B_RuntimeErrorD___serialize__;
        B_RuntimeErrorG_methods.__deserialize__ = B_RuntimeErrorD___deserialize__;
    }
    {
        B_NotImplementedErrorG_methods.$GCINFO = "B_NotImplementedError";
        B_NotImplementedErrorG_methods.$superclass = ($SuperG_class)&B_RuntimeErrorG_methods;
        B_NotImplementedErrorG_methods.__init__ = (B_NoneType (*) (B_NotImplementedError, B_str))B_BaseExceptionG_methods.__init__;
        B_NotImplementedErrorG_methods.__bool__ = (B_bool (*) (B_NotImplementedError))B_valueG_methods.__bool__;
        B_NotImplementedErrorG_methods.__str__ = B_NotImplementedErrorD___str__;
        B_NotImplementedErrorG_methods.__repr__ = B_NotImplementedErrorD___str__;
        B_NotImplementedErrorG_methods.__serialize__ = B_NotImplementedErrorD___serialize__;
        B_NotImplementedErrorG_methods.__deserialize__ = B_NotImplementedErrorD___deserialize__;
    }
    {
        B_ValueErrorG_methods.$GCINFO = "B_ValueError";
        B_ValueErrorG_methods.$superclass = ($SuperG_class)&B_ExceptionG_methods;
        B_ValueErrorG_methods.__init__ = (B_NoneType (*) (B_ValueError, B_str))B_BaseExceptionG_methods.__init__;
        B_ValueErrorG_methods.__bool__ = (B_bool (*) (B_ValueError))B_valueG_methods.__bool__;
        B_ValueErrorG_methods.__str__ = B_ValueErrorD___str__;
        B_ValueErrorG_methods.__repr__ = B_ValueErrorD___str__;
        B_ValueErrorG_methods.__serialize__ = B_ValueErrorD___serialize__;
        B_ValueErrorG_methods.__deserialize__ = B_ValueErrorD___deserialize__;
    }
    {
        B_IdentityG_methods.$GCINFO = "B_Identity";
        B_IdentityG_methods.$superclass = ($SuperG_class)&B_valueG_methods;
        B_IdentityG_methods.__bool__ = (B_bool (*) (B_Identity))B_valueG_methods.__bool__;
        B_IdentityG_methods.__str__ = (B_str (*) (B_Identity))B_valueG_methods.__str__;
        B_IdentityG_methods.__repr__ = (B_str (*) (B_Identity))B_valueG_methods.__repr__;
        B_IdentityG_methods.__init__ = B_IdentityD___init__;
        B_IdentityG_methods.__serialize__ = B_IdentityD___serialize__;
        B_IdentityG_methods.__deserialize__ = B_IdentityD___deserialize__;
        $register(&B_IdentityG_methods);
    }
    {
        B_EqG_methods.$GCINFO = "B_Eq";
        B_EqG_methods.$superclass = ($SuperG_class)&B_valueG_methods;
        B_EqG_methods.__bool__ = (B_bool (*) (B_Eq))B_valueG_methods.__bool__;
        B_EqG_methods.__str__ = (B_str (*) (B_Eq))B_valueG_methods.__str__;
        B_EqG_methods.__repr__ = (B_str (*) (B_Eq))B_valueG_methods.__repr__;
        B_EqG_methods.__init__ = B_EqD___init__;
        B_EqG_methods.__ne__ = B_EqD___ne__;
        B_EqG_methods.__serialize__ = B_EqD___serialize__;
        B_EqG_methods.__deserialize__ = B_EqD___deserialize__;
        $register(&B_EqG_methods);
    }
    {
        B_OrdG_methods.$GCINFO = "B_Ord";
        B_OrdG_methods.$superclass = ($SuperG_class)&B_EqG_methods;
        B_OrdG_methods.__bool__ = (B_bool (*) (B_Ord))B_valueG_methods.__bool__;
        B_OrdG_methods.__str__ = (B_str (*) (B_Ord))B_valueG_methods.__str__;
        B_OrdG_methods.__repr__ = (B_str (*) (B_Ord))B_valueG_methods.__repr__;
        B_OrdG_methods.__ne__ = (B_bool (*) (B_Ord, $WORD, $WORD))B_EqG_methods.__ne__;
        B_OrdG_methods.__init__ = B_OrdD___init__;
        B_OrdG_methods.__le__ = B_OrdD___le__;
        B_OrdG_methods.__gt__ = B_OrdD___gt__;
        B_OrdG_methods.__ge__ = B_OrdD___ge__;
        B_OrdG_methods.__serialize__ = B_OrdD___serialize__;
        B_OrdG_methods.__deserialize__ = B_OrdD___deserialize__;
        $register(&B_OrdG_methods);
    }
    {
        B_LogicalG_methods.$GCINFO = "B_Logical";
        B_LogicalG_methods.$superclass = ($SuperG_class)&B_valueG_methods;
        B_LogicalG_methods.__bool__ = (B_bool (*) (B_Logical))B_valueG_methods.__bool__;
        B_LogicalG_methods.__str__ = (B_str (*) (B_Logical))B_valueG_methods.__str__;
        B_LogicalG_methods.__repr__ = (B_str (*) (B_Logical))B_valueG_methods.__repr__;
        B_LogicalG_methods.__init__ = B_LogicalD___init__;
        B_LogicalG_methods.__iand__ = B_LogicalD___iand__;
        B_LogicalG_methods.__ior__ = B_LogicalD___ior__;
        B_LogicalG_methods.__ixor__ = B_LogicalD___ixor__;
        B_LogicalG_methods.__serialize__ = B_LogicalD___serialize__;
        B_LogicalG_methods.__deserialize__ = B_LogicalD___deserialize__;
        $register(&B_LogicalG_methods);
    }
    {
        B_PlusG_methods.$GCINFO = "B_Plus";
        B_PlusG_methods.$superclass = ($SuperG_class)&B_valueG_methods;
        B_PlusG_methods.__bool__ = (B_bool (*) (B_Plus))B_valueG_methods.__bool__;
        B_PlusG_methods.__str__ = (B_str (*) (B_Plus))B_valueG_methods.__str__;
        B_PlusG_methods.__repr__ = (B_str (*) (B_Plus))B_valueG_methods.__repr__;
        B_PlusG_methods.__init__ = B_PlusD___init__;
        B_PlusG_methods.__iadd__ = B_PlusD___iadd__;
        B_PlusG_methods.__serialize__ = B_PlusD___serialize__;
        B_PlusG_methods.__deserialize__ = B_PlusD___deserialize__;
        $register(&B_PlusG_methods);
    }
    {
        B_MinusG_methods.$GCINFO = "B_Minus";
        B_MinusG_methods.$superclass = ($SuperG_class)&B_valueG_methods;
        B_MinusG_methods.__bool__ = (B_bool (*) (B_Minus))B_valueG_methods.__bool__;
        B_MinusG_methods.__str__ = (B_str (*) (B_Minus))B_valueG_methods.__str__;
        B_MinusG_methods.__repr__ = (B_str (*) (B_Minus))B_valueG_methods.__repr__;
        B_MinusG_methods.__init__ = B_MinusD___init__;
        B_MinusG_methods.__isub__ = B_MinusD___isub__;
        B_MinusG_methods.__serialize__ = B_MinusD___serialize__;
        B_MinusG_methods.__deserialize__ = B_MinusD___deserialize__;
        $register(&B_MinusG_methods);
    }
    {
        B_TimesG_methods.$GCINFO = "B_Times";
        B_TimesG_methods.$superclass = ($SuperG_class)&B_PlusG_methods;
        B_TimesG_methods.__bool__ = (B_bool (*) (B_Times))B_valueG_methods.__bool__;
        B_TimesG_methods.__str__ = (B_str (*) (B_Times))B_valueG_methods.__str__;
        B_TimesG_methods.__repr__ = (B_str (*) (B_Times))B_valueG_methods.__repr__;
        B_TimesG_methods.__iadd__ = ($WORD (*) (B_Times, $WORD, $WORD))B_PlusG_methods.__iadd__;
        B_TimesG_methods.__init__ = B_TimesD___init__;
        B_TimesG_methods.__imul__ = B_TimesD___imul__;
        B_TimesG_methods.__serialize__ = B_TimesD___serialize__;
        B_TimesG_methods.__deserialize__ = B_TimesD___deserialize__;
        $register(&B_TimesG_methods);
    }
    {
        B_DivG_methods.$GCINFO = "B_Div";
        B_DivG_methods.$superclass = ($SuperG_class)&B_valueG_methods;
        B_DivG_methods.__bool__ = (B_bool (*) (B_Div))B_valueG_methods.__bool__;
        B_DivG_methods.__str__ = (B_str (*) (B_Div))B_valueG_methods.__str__;
        B_DivG_methods.__repr__ = (B_str (*) (B_Div))B_valueG_methods.__repr__;
        B_DivG_methods.__init__ = B_DivD___init__;
        B_DivG_methods.__itruediv__ = B_DivD___itruediv__;
        B_DivG_methods.__serialize__ = B_DivD___serialize__;
        B_DivG_methods.__deserialize__ = B_DivD___deserialize__;
        $register(&B_DivG_methods);
    }
    {
        B_HashableG_methods.$GCINFO = "B_Hashable";
        B_HashableG_methods.$superclass = ($SuperG_class)&B_EqG_methods;
        B_HashableG_methods.__bool__ = (B_bool (*) (B_Hashable))B_valueG_methods.__bool__;
        B_HashableG_methods.__str__ = (B_str (*) (B_Hashable))B_valueG_methods.__str__;
        B_HashableG_methods.__repr__ = (B_str (*) (B_Hashable))B_valueG_methods.__repr__;
        B_HashableG_methods.__ne__ = (B_bool (*) (B_Hashable, $WORD, $WORD))B_EqG_methods.__ne__;
        B_HashableG_methods.__init__ = B_HashableD___init__;
        B_HashableG_methods.__serialize__ = B_HashableD___serialize__;
        B_HashableG_methods.__deserialize__ = B_HashableD___deserialize__;
        $register(&B_HashableG_methods);
    }
    {
        B_complexG_methods.$GCINFO = "B_complex";
        B_complexG_methods.$superclass = ($SuperG_class)&B_valueG_methods;
        B_complexG_methods.__bool__ = (B_bool (*) (B_complex))B_valueG_methods.__bool__;
        B_complexG_methods.__str__ = B_complexD___str__;
        B_complexG_methods.__repr__ = B_complexD___str__;
        B_complexG_methods.__init__ = B_complexD___init__;
        B_complexG_methods.__serialize__ = B_complexD___serialize__;
        B_complexG_methods.__deserialize__ = B_complexD___deserialize__;
    }
    {
        B_dictG_methods.$GCINFO = "B_dict";
        B_dictG_methods.$superclass = ($SuperG_class)&B_objectG_methods;
        B_dictG_methods.__bool__ = (B_bool (*) (B_dict))B_valueG_methods.__bool__;
        B_dictG_methods.__str__ = B_dictD___str__;
        B_dictG_methods.__repr__ = B_dictD___str__;
        B_dictG_methods.__init__ = B_dictD___init__;
        B_dictG_methods.__serialize__ = B_dictD___serialize__;
        B_dictG_methods.__deserialize__ = B_dictD___deserialize__;
    }
    {
        B_setG_methods.$GCINFO = "B_set";
        B_setG_methods.$superclass = ($SuperG_class)&B_objectG_methods;
        B_setG_methods.__bool__ = (B_bool (*) (B_set))B_valueG_methods.__bool__;
        B_setG_methods.__str__ = B_setD___str__;
        B_setG_methods.__repr__ = (B_str (*) (B_set))B_valueG_methods.__repr__;
        B_setG_methods.__init__ = B_setD___init__;
        B_setG_methods.__serialize__ = B_setD___serialize__;
        B_setG_methods.__deserialize__ = B_setD___deserialize__;
    }
    {
        B_NumberG_methods.$GCINFO = "B_Number";
        B_NumberG_methods.$superclass = ($SuperG_class)&B_TimesG_methods;
        B_NumberG_methods.__bool__ = (B_bool (*) (B_Number))B_valueG_methods.__bool__;
        B_NumberG_methods.__str__ = (B_str (*) (B_Number))B_valueG_methods.__str__;
        B_NumberG_methods.__repr__ = (B_str (*) (B_Number))B_valueG_methods.__repr__;
        B_NumberG_methods.__iadd__ = ($WORD (*) (B_Number, $WORD, $WORD))B_PlusG_methods.__iadd__;
        B_NumberG_methods.__imul__ = ($WORD (*) (B_Number, $WORD, $WORD))B_TimesG_methods.__imul__;
        B_NumberG_methods.__init__ = B_NumberD___init__;
        B_NumberG_methods.__ipow__ = B_NumberD___ipow__;
        B_NumberG_methods.__serialize__ = B_NumberD___serialize__;
        B_NumberG_methods.__deserialize__ = B_NumberD___deserialize__;
        $register(&B_NumberG_methods);
    }
    {
        B_MinusD_NumberG_methods.$GCINFO = "B_MinusD_Number";
        B_MinusD_NumberG_methods.$superclass = ($SuperG_class)&B_MinusG_methods;
        B_MinusD_NumberG_methods.__bool__ = (B_bool (*) (B_MinusD_Number))B_valueG_methods.__bool__;
        B_MinusD_NumberG_methods.__str__ = (B_str (*) (B_MinusD_Number))B_valueG_methods.__str__;
        B_MinusD_NumberG_methods.__repr__ = (B_str (*) (B_MinusD_Number))B_valueG_methods.__repr__;
        B_MinusD_NumberG_methods.__isub__ = ($WORD (*) (B_MinusD_Number, $WORD, $WORD))B_MinusG_methods.__isub__;
        B_MinusD_NumberG_methods.__init__ = B_MinusD_NumberD___init__;
        B_MinusD_NumberG_methods.__serialize__ = B_MinusD_NumberD___serialize__;
        B_MinusD_NumberG_methods.__deserialize__ = B_MinusD_NumberD___deserialize__;
        $register(&B_MinusD_NumberG_methods);
    }
    {
        B_RealG_methods.$GCINFO = "B_Real";
        B_RealG_methods.$superclass = ($SuperG_class)&B_NumberG_methods;
        B_RealG_methods.__bool__ = (B_bool (*) (B_Real))B_valueG_methods.__bool__;
        B_RealG_methods.__str__ = (B_str (*) (B_Real))B_valueG_methods.__str__;
        B_RealG_methods.__repr__ = (B_str (*) (B_Real))B_valueG_methods.__repr__;
        B_RealG_methods.__iadd__ = ($WORD (*) (B_Real, $WORD, $WORD))B_PlusG_methods.__iadd__;
        B_RealG_methods.__imul__ = ($WORD (*) (B_Real, $WORD, $WORD))B_TimesG_methods.__imul__;
        B_RealG_methods.__ipow__ = ($WORD (*) (B_Real, $WORD, $WORD))B_NumberG_methods.__ipow__;
        B_RealG_methods.__init__ = B_RealD___init__;
        B_RealG_methods.__serialize__ = B_RealD___serialize__;
        B_RealG_methods.__deserialize__ = B_RealD___deserialize__;
        $register(&B_RealG_methods);
    }
    {
        B_MinusD_RealG_methods.$GCINFO = "B_MinusD_Real";
        B_MinusD_RealG_methods.$superclass = ($SuperG_class)&B_MinusD_NumberG_methods;
        B_MinusD_RealG_methods.__bool__ = (B_bool (*) (B_MinusD_Real))B_valueG_methods.__bool__;
        B_MinusD_RealG_methods.__str__ = (B_str (*) (B_MinusD_Real))B_valueG_methods.__str__;
        B_MinusD_RealG_methods.__repr__ = (B_str (*) (B_MinusD_Real))B_valueG_methods.__repr__;
        B_MinusD_RealG_methods.__isub__ = ($WORD (*) (B_MinusD_Real, $WORD, $WORD))B_MinusG_methods.__isub__;
        B_MinusD_RealG_methods.__init__ = B_MinusD_RealD___init__;
        B_MinusD_RealG_methods.__serialize__ = B_MinusD_RealD___serialize__;
        B_MinusD_RealG_methods.__deserialize__ = B_MinusD_RealD___deserialize__;
        $register(&B_MinusD_RealG_methods);
    }
    {
        B_RealFloatG_methods.$GCINFO = "B_RealFloat";
        B_RealFloatG_methods.$superclass = ($SuperG_class)&B_RealG_methods;
        B_RealFloatG_methods.__bool__ = (B_bool (*) (B_RealFloat))B_valueG_methods.__bool__;
        B_RealFloatG_methods.__str__ = (B_str (*) (B_RealFloat))B_valueG_methods.__str__;
        B_RealFloatG_methods.__repr__ = (B_str (*) (B_RealFloat))B_valueG_methods.__repr__;
        B_RealFloatG_methods.__iadd__ = ($WORD (*) (B_RealFloat, $WORD, $WORD))B_PlusG_methods.__iadd__;
        B_RealFloatG_methods.__imul__ = ($WORD (*) (B_RealFloat, $WORD, $WORD))B_TimesG_methods.__imul__;
        B_RealFloatG_methods.__ipow__ = ($WORD (*) (B_RealFloat, $WORD, $WORD))B_NumberG_methods.__ipow__;
        B_RealFloatG_methods.__init__ = B_RealFloatD___init__;
        B_RealFloatG_methods.__serialize__ = B_RealFloatD___serialize__;
        B_RealFloatG_methods.__deserialize__ = B_RealFloatD___deserialize__;
        $register(&B_RealFloatG_methods);
    }
    {
        B_MinusD_RealFloatG_methods.$GCINFO = "B_MinusD_RealFloat";
        B_MinusD_RealFloatG_methods.$superclass = ($SuperG_class)&B_MinusD_RealG_methods;
        B_MinusD_RealFloatG_methods.__bool__ = (B_bool (*) (B_MinusD_RealFloat))B_valueG_methods.__bool__;
        B_MinusD_RealFloatG_methods.__str__ = (B_str (*) (B_MinusD_RealFloat))B_valueG_methods.__str__;
        B_MinusD_RealFloatG_methods.__repr__ = (B_str (*) (B_MinusD_RealFloat))B_valueG_methods.__repr__;
        B_MinusD_RealFloatG_methods.__isub__ = ($WORD (*) (B_MinusD_RealFloat, $WORD, $WORD))B_MinusG_methods.__isub__;
        B_MinusD_RealFloatG_methods.__init__ = B_MinusD_RealFloatD___init__;
        B_MinusD_RealFloatG_methods.__serialize__ = B_MinusD_RealFloatD___serialize__;
        B_MinusD_RealFloatG_methods.__deserialize__ = B_MinusD_RealFloatD___deserialize__;
        $register(&B_MinusD_RealFloatG_methods);
    }
    {
        B_RationalG_methods.$GCINFO = "B_Rational";
        B_RationalG_methods.$superclass = ($SuperG_class)&B_RealG_methods;
        B_RationalG_methods.__bool__ = (B_bool (*) (B_Rational))B_valueG_methods.__bool__;
        B_RationalG_methods.__str__ = (B_str (*) (B_Rational))B_valueG_methods.__str__;
        B_RationalG_methods.__repr__ = (B_str (*) (B_Rational))B_valueG_methods.__repr__;
        B_RationalG_methods.__iadd__ = ($WORD (*) (B_Rational, $WORD, $WORD))B_PlusG_methods.__iadd__;
        B_RationalG_methods.__imul__ = ($WORD (*) (B_Rational, $WORD, $WORD))B_TimesG_methods.__imul__;
        B_RationalG_methods.__ipow__ = ($WORD (*) (B_Rational, $WORD, $WORD))B_NumberG_methods.__ipow__;
        B_RationalG_methods.__init__ = B_RationalD___init__;
        B_RationalG_methods.__serialize__ = B_RationalD___serialize__;
        B_RationalG_methods.__deserialize__ = B_RationalD___deserialize__;
        $register(&B_RationalG_methods);
    }
    {
        B_MinusD_RationalG_methods.$GCINFO = "B_MinusD_Rational";
        B_MinusD_RationalG_methods.$superclass = ($SuperG_class)&B_MinusD_RealG_methods;
        B_MinusD_RationalG_methods.__bool__ = (B_bool (*) (B_MinusD_Rational))B_valueG_methods.__bool__;
        B_MinusD_RationalG_methods.__str__ = (B_str (*) (B_MinusD_Rational))B_valueG_methods.__str__;
        B_MinusD_RationalG_methods.__repr__ = (B_str (*) (B_MinusD_Rational))B_valueG_methods.__repr__;
        B_MinusD_RationalG_methods.__isub__ = ($WORD (*) (B_MinusD_Rational, $WORD, $WORD))B_MinusG_methods.__isub__;
        B_MinusD_RationalG_methods.__init__ = B_MinusD_RationalD___init__;
        B_MinusD_RationalG_methods.__serialize__ = B_MinusD_RationalD___serialize__;
        B_MinusD_RationalG_methods.__deserialize__ = B_MinusD_RationalD___deserialize__;
        $register(&B_MinusD_RationalG_methods);
    }
    {
        B_IntegralG_methods.$GCINFO = "B_Integral";
        B_IntegralG_methods.$superclass = ($SuperG_class)&B_RationalG_methods;
        B_IntegralG_methods.__bool__ = (B_bool (*) (B_Integral))B_valueG_methods.__bool__;
        B_IntegralG_methods.__str__ = (B_str (*) (B_Integral))B_valueG_methods.__str__;
        B_IntegralG_methods.__repr__ = (B_str (*) (B_Integral))B_valueG_methods.__repr__;
        B_IntegralG_methods.__iadd__ = ($WORD (*) (B_Integral, $WORD, $WORD))B_PlusG_methods.__iadd__;
        B_IntegralG_methods.__imul__ = ($WORD (*) (B_Integral, $WORD, $WORD))B_TimesG_methods.__imul__;
        B_IntegralG_methods.__ipow__ = ($WORD (*) (B_Integral, $WORD, $WORD))B_NumberG_methods.__ipow__;
        B_IntegralG_methods.__init__ = B_IntegralD___init__;
        B_IntegralG_methods.__ifloordiv__ = B_IntegralD___ifloordiv__;
        B_IntegralG_methods.__imod__ = B_IntegralD___imod__;
        B_IntegralG_methods.__ilshift__ = B_IntegralD___ilshift__;
        B_IntegralG_methods.__irshift__ = B_IntegralD___irshift__;
        B_IntegralG_methods.__serialize__ = B_IntegralD___serialize__;
        B_IntegralG_methods.__deserialize__ = B_IntegralD___deserialize__;
        $register(&B_IntegralG_methods);
    }
    {
        B_MinusD_IntegralG_methods.$GCINFO = "B_MinusD_Integral";
        B_MinusD_IntegralG_methods.$superclass = ($SuperG_class)&B_MinusD_RationalG_methods;
        B_MinusD_IntegralG_methods.__bool__ = (B_bool (*) (B_MinusD_Integral))B_valueG_methods.__bool__;
        B_MinusD_IntegralG_methods.__str__ = (B_str (*) (B_MinusD_Integral))B_valueG_methods.__str__;
        B_MinusD_IntegralG_methods.__repr__ = (B_str (*) (B_MinusD_Integral))B_valueG_methods.__repr__;
        B_MinusD_IntegralG_methods.__isub__ = ($WORD (*) (B_MinusD_Integral, $WORD, $WORD))B_MinusG_methods.__isub__;
        B_MinusD_IntegralG_methods.__init__ = B_MinusD_IntegralD___init__;
        B_MinusD_IntegralG_methods.__serialize__ = B_MinusD_IntegralD___serialize__;
        B_MinusD_IntegralG_methods.__deserialize__ = B_MinusD_IntegralD___deserialize__;
        $register(&B_MinusD_IntegralG_methods);
    }
    {
        B_LogicalD_IntegralG_methods.$GCINFO = "B_LogicalD_Integral";
        B_LogicalD_IntegralG_methods.$superclass = ($SuperG_class)&B_LogicalG_methods;
        B_LogicalD_IntegralG_methods.__bool__ = (B_bool (*) (B_LogicalD_Integral))B_valueG_methods.__bool__;
        B_LogicalD_IntegralG_methods.__str__ = (B_str (*) (B_LogicalD_Integral))B_valueG_methods.__str__;
        B_LogicalD_IntegralG_methods.__repr__ = (B_str (*) (B_LogicalD_Integral))B_valueG_methods.__repr__;
        B_LogicalD_IntegralG_methods.__iand__ = ($WORD (*) (B_LogicalD_Integral, $WORD, $WORD))B_LogicalG_methods.__iand__;
        B_LogicalD_IntegralG_methods.__ior__ = ($WORD (*) (B_LogicalD_Integral, $WORD, $WORD))B_LogicalG_methods.__ior__;
        B_LogicalD_IntegralG_methods.__ixor__ = ($WORD (*) (B_LogicalD_Integral, $WORD, $WORD))B_LogicalG_methods.__ixor__;
        B_LogicalD_IntegralG_methods.__init__ = B_LogicalD_IntegralD___init__;
        B_LogicalD_IntegralG_methods.__serialize__ = B_LogicalD_IntegralD___serialize__;
        B_LogicalD_IntegralG_methods.__deserialize__ = B_LogicalD_IntegralD___deserialize__;
        $register(&B_LogicalD_IntegralG_methods);
    }
    {
        B_HashableD_boolG_methods.$GCINFO = "B_HashableD_bool";
        B_HashableD_boolG_methods.$superclass = ($SuperG_class)&B_HashableG_methods;
        B_HashableD_boolG_methods.__bool__ = (B_bool (*) (B_HashableD_bool))B_valueG_methods.__bool__;
        B_HashableD_boolG_methods.__str__ = (B_str (*) (B_HashableD_bool))B_valueG_methods.__str__;
        B_HashableD_boolG_methods.__repr__ = (B_str (*) (B_HashableD_bool))B_valueG_methods.__repr__;
        B_HashableD_boolG_methods.__ne__ = (B_bool (*) (B_HashableD_bool, B_bool, B_bool))B_EqG_methods.__ne__;
        B_HashableD_boolG_methods.__init__ = B_HashableD_boolD___init__;
        B_HashableD_boolG_methods.__hash__ = B_HashableD_boolD___hash__;
        B_HashableD_boolG_methods.__eq__ = B_HashableD_boolD___eq__;
        B_HashableD_boolG_methods.__serialize__ = B_HashableD_boolD___serialize__;
        B_HashableD_boolG_methods.__deserialize__ = B_HashableD_boolD___deserialize__;
        $register(&B_HashableD_boolG_methods);
    }
    {
        B_IntegralD_intG_methods.$GCINFO = "B_IntegralD_int";
        B_IntegralD_intG_methods.$superclass = ($SuperG_class)&B_IntegralG_methods;
        B_IntegralD_intG_methods.__bool__ = (B_bool (*) (B_IntegralD_int))B_valueG_methods.__bool__;
        B_IntegralD_intG_methods.__str__ = (B_str (*) (B_IntegralD_int))B_valueG_methods.__str__;
        B_IntegralD_intG_methods.__repr__ = (B_str (*) (B_IntegralD_int))B_valueG_methods.__repr__;
        B_IntegralD_intG_methods.__iadd__ = (B_int (*) (B_IntegralD_int, B_int, B_int))B_PlusG_methods.__iadd__;
        B_IntegralD_intG_methods.__imul__ = (B_int (*) (B_IntegralD_int, B_int, B_int))B_TimesG_methods.__imul__;
        B_IntegralD_intG_methods.__ipow__ = (B_int (*) (B_IntegralD_int, B_int, B_int))B_NumberG_methods.__ipow__;
        B_IntegralD_intG_methods.__ifloordiv__ = (B_int (*) (B_IntegralD_int, B_int, B_int))B_IntegralG_methods.__ifloordiv__;
        B_IntegralD_intG_methods.__imod__ = (B_int (*) (B_IntegralD_int, B_int, B_int))B_IntegralG_methods.__imod__;
        B_IntegralD_intG_methods.__ilshift__ = (B_int (*) (B_IntegralD_int, B_int, B_int))B_IntegralG_methods.__ilshift__;
        B_IntegralD_intG_methods.__irshift__ = (B_int (*) (B_IntegralD_int, B_int, B_int))B_IntegralG_methods.__irshift__;
        B_IntegralD_intG_methods.__init__ = B_IntegralD_intD___init__;
        B_IntegralD_intG_methods.__invert__ = B_IntegralD_intD___invert__;
        B_IntegralD_intG_methods.__rshift__ = B_IntegralD_intD___rshift__;
        B_IntegralD_intG_methods.__lshift__ = B_IntegralD_intD___lshift__;
        B_IntegralD_intG_methods.__mod__ = B_IntegralD_intD___mod__;
        B_IntegralD_intG_methods.__floordiv__ = B_IntegralD_intD___floordiv__;
        B_IntegralD_intG_methods.__divmod__ = B_IntegralD_intD___divmod__;
        B_IntegralD_intG_methods.__index__ = B_IntegralD_intD___index__;
        B_IntegralD_intG_methods.__int__ = B_IntegralD_intD___int__;
        B_IntegralD_intG_methods.denominator = B_IntegralD_intD_denominator;
        B_IntegralD_intG_methods.numerator = B_IntegralD_intD_numerator;
        B_IntegralD_intG_methods.__round__ = B_IntegralD_intD___round__;
        B_IntegralD_intG_methods.__ceil__ = B_IntegralD_intD___ceil__;
        B_IntegralD_intG_methods.__floor__ = B_IntegralD_intD___floor__;
        B_IntegralD_intG_methods.__trunc__ = B_IntegralD_intD___trunc__;
        B_IntegralD_intG_methods.__float__ = B_IntegralD_intD___float__;
        B_IntegralD_intG_methods.conjugate = B_IntegralD_intD_conjugate;
        B_IntegralD_intG_methods.__abs__ = B_IntegralD_intD___abs__;
        B_IntegralD_intG_methods.imag = B_IntegralD_intD_imag;
        B_IntegralD_intG_methods.real = B_IntegralD_intD_real;
        B_IntegralD_intG_methods.__pos__ = B_IntegralD_intD___pos__;
        B_IntegralD_intG_methods.__neg__ = B_IntegralD_intD___neg__;
        B_IntegralD_intG_methods.__pow__ = B_IntegralD_intD___pow__;
        B_IntegralD_intG_methods.__complx__ = B_IntegralD_intD___complex__;
        B_IntegralD_intG_methods.__fromatom__ = B_IntegralD_intD___fromatom__;
        B_IntegralD_intG_methods.__mul__ = B_IntegralD_intD___mul__;
        B_IntegralD_intG_methods.__add__ = B_IntegralD_intD___add__;
        B_IntegralD_intG_methods.__serialize__ = B_IntegralD_intD___serialize__;
        B_IntegralD_intG_methods.__deserialize__ = B_IntegralD_intD___deserialize__;
        $register(&B_IntegralD_intG_methods);
    }
    {
        B_MinusD_IntegralD_intG_methods.$GCINFO = "B_MinusD_IntegralD_int";
        B_MinusD_IntegralD_intG_methods.$superclass = ($SuperG_class)&B_MinusD_IntegralG_methods;
        B_MinusD_IntegralD_intG_methods.__bool__ = (B_bool (*) (B_MinusD_IntegralD_int))B_valueG_methods.__bool__;
        B_MinusD_IntegralD_intG_methods.__str__ = (B_str (*) (B_MinusD_IntegralD_int))B_valueG_methods.__str__;
        B_MinusD_IntegralD_intG_methods.__repr__ = (B_str (*) (B_MinusD_IntegralD_int))B_valueG_methods.__repr__;
        B_MinusD_IntegralD_intG_methods.__isub__ = (B_int (*) (B_MinusD_IntegralD_int, B_int, B_int))B_MinusG_methods.__isub__;
        B_MinusD_IntegralD_intG_methods.__init__ = B_MinusD_IntegralD_intD___init__;
        B_MinusD_IntegralD_intG_methods.__sub__ = B_MinusD_IntegralD_intD___sub__;
        B_MinusD_IntegralD_intG_methods.__serialize__ = B_MinusD_IntegralD_intD___serialize__;
        B_MinusD_IntegralD_intG_methods.__deserialize__ = B_MinusD_IntegralD_intD___deserialize__;
        $register(&B_MinusD_IntegralD_intG_methods);
    }
    {
        B_LogicalD_IntegralD_intG_methods.$GCINFO = "B_LogicalD_IntegralD_int";
        B_LogicalD_IntegralD_intG_methods.$superclass = ($SuperG_class)&B_LogicalD_IntegralG_methods;
        B_LogicalD_IntegralD_intG_methods.__bool__ = (B_bool (*) (B_LogicalD_IntegralD_int))B_valueG_methods.__bool__;
        B_LogicalD_IntegralD_intG_methods.__str__ = (B_str (*) (B_LogicalD_IntegralD_int))B_valueG_methods.__str__;
        B_LogicalD_IntegralD_intG_methods.__repr__ = (B_str (*) (B_LogicalD_IntegralD_int))B_valueG_methods.__repr__;
        B_LogicalD_IntegralD_intG_methods.__iand__ = (B_int (*) (B_LogicalD_IntegralD_int, B_int, B_int))B_LogicalG_methods.__iand__;
        B_LogicalD_IntegralD_intG_methods.__ior__ = (B_int (*) (B_LogicalD_IntegralD_int, B_int, B_int))B_LogicalG_methods.__ior__;
        B_LogicalD_IntegralD_intG_methods.__ixor__ = (B_int (*) (B_LogicalD_IntegralD_int, B_int, B_int))B_LogicalG_methods.__ixor__;
        B_LogicalD_IntegralD_intG_methods.__init__ = B_LogicalD_IntegralD_intD___init__;
        B_LogicalD_IntegralD_intG_methods.__xor__ = B_LogicalD_IntegralD_intD___xor__;
        B_LogicalD_IntegralD_intG_methods.__or__ = B_LogicalD_IntegralD_intD___or__;
        B_LogicalD_IntegralD_intG_methods.__and__ = B_LogicalD_IntegralD_intD___and__;
        B_LogicalD_IntegralD_intG_methods.__serialize__ = B_LogicalD_IntegralD_intD___serialize__;
        B_LogicalD_IntegralD_intG_methods.__deserialize__ = B_LogicalD_IntegralD_intD___deserialize__;
        $register(&B_LogicalD_IntegralD_intG_methods);
    }
    {
        B_DivD_intG_methods.$GCINFO = "B_DivD_int";
        B_DivD_intG_methods.$superclass = ($SuperG_class)&B_DivG_methods;
        B_DivD_intG_methods.__bool__ = (B_bool (*) (B_DivD_int))B_valueG_methods.__bool__;
        B_DivD_intG_methods.__str__ = (B_str (*) (B_DivD_int))B_valueG_methods.__str__;
        B_DivD_intG_methods.__repr__ = (B_str (*) (B_DivD_int))B_valueG_methods.__repr__;
        B_DivD_intG_methods.__itruediv__ = (B_float (*) (B_DivD_int, B_int, B_int))B_DivG_methods.__itruediv__;
        B_DivD_intG_methods.__init__ = B_DivD_intD___init__;
        B_DivD_intG_methods.__truediv__ = B_DivD_intD___truediv__;
        B_DivD_intG_methods.__serialize__ = B_DivD_intD___serialize__;
        B_DivD_intG_methods.__deserialize__ = B_DivD_intD___deserialize__;
        $register(&B_DivD_intG_methods);
    }
    {
        B_OrdD_intG_methods.$GCINFO = "B_OrdD_int";
        B_OrdD_intG_methods.$superclass = ($SuperG_class)&B_OrdG_methods;
        B_OrdD_intG_methods.__bool__ = (B_bool (*) (B_OrdD_int))B_valueG_methods.__bool__;
        B_OrdD_intG_methods.__str__ = (B_str (*) (B_OrdD_int))B_valueG_methods.__str__;
        B_OrdD_intG_methods.__repr__ = (B_str (*) (B_OrdD_int))B_valueG_methods.__repr__;
        B_OrdD_intG_methods.__ne__ = (B_bool (*) (B_OrdD_int, B_int, B_int))B_EqG_methods.__ne__;
        B_OrdD_intG_methods.__le__ = (B_bool (*) (B_OrdD_int, B_int, B_int))B_OrdG_methods.__le__;
        B_OrdD_intG_methods.__gt__ = (B_bool (*) (B_OrdD_int, B_int, B_int))B_OrdG_methods.__gt__;
        B_OrdD_intG_methods.__ge__ = (B_bool (*) (B_OrdD_int, B_int, B_int))B_OrdG_methods.__ge__;
        B_OrdD_intG_methods.__init__ = B_OrdD_intD___init__;
        B_OrdD_intG_methods.__lt__ = B_OrdD_intD___lt__;
        B_OrdD_intG_methods.__eq__ = B_OrdD_intD___eq__;
        B_OrdD_intG_methods.__serialize__ = B_OrdD_intD___serialize__;
        B_OrdD_intG_methods.__deserialize__ = B_OrdD_intD___deserialize__;
        $register(&B_OrdD_intG_methods);
    }
    {
        B_HashableD_intG_methods.$GCINFO = "B_HashableD_int";
        B_HashableD_intG_methods.$superclass = ($SuperG_class)&B_HashableG_methods;
        B_HashableD_intG_methods.__bool__ = (B_bool (*) (B_HashableD_int))B_valueG_methods.__bool__;
        B_HashableD_intG_methods.__str__ = (B_str (*) (B_HashableD_int))B_valueG_methods.__str__;
        B_HashableD_intG_methods.__repr__ = (B_str (*) (B_HashableD_int))B_valueG_methods.__repr__;
        B_HashableD_intG_methods.__eq__ = B_HashableD_intD___eq__;
        B_HashableD_intG_methods.__ne__ = (B_bool (*) (B_HashableD_int, B_int, B_int))B_EqG_methods.__ne__;
        B_HashableD_intG_methods.__init__ = B_HashableD_intD___init__;
        B_HashableD_intG_methods.__hash__ = B_HashableD_intD___hash__;
        B_HashableD_intG_methods.__serialize__ = B_HashableD_intD___serialize__;
        B_HashableD_intG_methods.__deserialize__ = B_HashableD_intD___deserialize__;
        $register(&B_HashableD_intG_methods);
    }
    {
        B_IntegralD_i64G_methods.$GCINFO = "B_IntegralD_i64";
        B_IntegralD_i64G_methods.$superclass = ($SuperG_class)&B_IntegralG_methods;
        B_IntegralD_i64G_methods.__bool__ = (B_bool (*) (B_IntegralD_i64))B_valueG_methods.__bool__;
        B_IntegralD_i64G_methods.__str__ = (B_str (*) (B_IntegralD_i64))B_valueG_methods.__str__;
        B_IntegralD_i64G_methods.__repr__ = (B_str (*) (B_IntegralD_i64))B_valueG_methods.__repr__;
        B_IntegralD_i64G_methods.__iadd__ = (B_i64 (*) (B_IntegralD_i64, B_i64, B_i64))B_PlusG_methods.__iadd__;
        B_IntegralD_i64G_methods.__imul__ = (B_i64 (*) (B_IntegralD_i64, B_i64, B_i64))B_TimesG_methods.__imul__;
        B_IntegralD_i64G_methods.__ipow__ = (B_i64 (*) (B_IntegralD_i64, B_i64, B_i64))B_NumberG_methods.__ipow__;
        B_IntegralD_i64G_methods.__ifloordiv__ = (B_i64 (*) (B_IntegralD_i64, B_i64, B_i64))B_IntegralG_methods.__ifloordiv__;
        B_IntegralD_i64G_methods.__imod__ = (B_i64 (*) (B_IntegralD_i64, B_i64, B_i64))B_IntegralG_methods.__imod__;
        B_IntegralD_i64G_methods.__ilshift__ = (B_i64 (*) (B_IntegralD_i64, B_i64, B_int))B_IntegralG_methods.__ilshift__;
        B_IntegralD_i64G_methods.__irshift__ = (B_i64 (*) (B_IntegralD_i64, B_i64, B_int))B_IntegralG_methods.__irshift__;
        B_IntegralD_i64G_methods.__init__ = B_IntegralD_i64D___init__;
        B_IntegralD_i64G_methods.__invert__ = B_IntegralD_i64D___invert__;
        B_IntegralD_i64G_methods.__rshift__ = B_IntegralD_i64D___rshift__;
        B_IntegralD_i64G_methods.__lshift__ = B_IntegralD_i64D___lshift__;
        B_IntegralD_i64G_methods.__mod__ = B_IntegralD_i64D___mod__;
        B_IntegralD_i64G_methods.__floordiv__ = B_IntegralD_i64D___floordiv__;
        B_IntegralD_i64G_methods.__divmod__ = B_IntegralD_i64D___divmod__;
        B_IntegralD_i64G_methods.__index__ = B_IntegralD_i64D___index__;
        B_IntegralD_i64G_methods.__int__ = B_IntegralD_i64D___int__;
        B_IntegralD_i64G_methods.denominator = B_IntegralD_i64D_denominator;
        B_IntegralD_i64G_methods.numerator = B_IntegralD_i64D_numerator;
        B_IntegralD_i64G_methods.__round__ = B_IntegralD_i64D___round__;
        B_IntegralD_i64G_methods.__ceil__ = B_IntegralD_i64D___ceil__;
        B_IntegralD_i64G_methods.__floor__ = B_IntegralD_i64D___floor__;
        B_IntegralD_i64G_methods.__trunc__ = B_IntegralD_i64D___trunc__;
        B_IntegralD_i64G_methods.__float__ = B_IntegralD_i64D___float__;
        B_IntegralD_i64G_methods.conjugate = B_IntegralD_i64D_conjugate;
        B_IntegralD_i64G_methods.__abs__ = B_IntegralD_i64D___abs__;
        B_IntegralD_i64G_methods.imag = B_IntegralD_i64D_imag;
        B_IntegralD_i64G_methods.real = B_IntegralD_i64D_real;
        B_IntegralD_i64G_methods.__pos__ = B_IntegralD_i64D___pos__;
        B_IntegralD_i64G_methods.__neg__ = B_IntegralD_i64D___neg__;
        B_IntegralD_i64G_methods.__pow__ = B_IntegralD_i64D___pow__;
        B_IntegralD_i64G_methods.__complx__ = B_IntegralD_i64D___complex__;
        B_IntegralD_i64G_methods.__fromatom__ = B_IntegralD_i64D___fromatom__;
        B_IntegralD_i64G_methods.__mul__ = B_IntegralD_i64D___mul__;
        B_IntegralD_i64G_methods.__add__ = B_IntegralD_i64D___add__;
        B_IntegralD_i64G_methods.__serialize__ = B_IntegralD_i64D___serialize__;
        B_IntegralD_i64G_methods.__deserialize__ = B_IntegralD_i64D___deserialize__;
        $register(&B_IntegralD_i64G_methods);
    }
    {
        B_MinusD_IntegralD_i64G_methods.$GCINFO = "B_MinusD_IntegralD_i64";
        B_MinusD_IntegralD_i64G_methods.$superclass = ($SuperG_class)&B_MinusD_IntegralG_methods;
        B_MinusD_IntegralD_i64G_methods.__bool__ = (B_bool (*) (B_MinusD_IntegralD_i64))B_valueG_methods.__bool__;
        B_MinusD_IntegralD_i64G_methods.__str__ = (B_str (*) (B_MinusD_IntegralD_i64))B_valueG_methods.__str__;
        B_MinusD_IntegralD_i64G_methods.__repr__ = (B_str (*) (B_MinusD_IntegralD_i64))B_valueG_methods.__repr__;
        B_MinusD_IntegralD_i64G_methods.__isub__ = (B_i64 (*) (B_MinusD_IntegralD_i64, B_i64, B_i64))B_MinusG_methods.__isub__;
        B_MinusD_IntegralD_i64G_methods.__init__ = B_MinusD_IntegralD_i64D___init__;
        B_MinusD_IntegralD_i64G_methods.__sub__ = B_MinusD_IntegralD_i64D___sub__;
        B_MinusD_IntegralD_i64G_methods.__serialize__ = B_MinusD_IntegralD_i64D___serialize__;
        B_MinusD_IntegralD_i64G_methods.__deserialize__ = B_MinusD_IntegralD_i64D___deserialize__;
        $register(&B_MinusD_IntegralD_i64G_methods);
    }
    {
        B_LogicalD_IntegralD_i64G_methods.$GCINFO = "B_LogicalD_IntegralD_i64";
        B_LogicalD_IntegralD_i64G_methods.$superclass = ($SuperG_class)&B_LogicalD_IntegralG_methods;
        B_LogicalD_IntegralD_i64G_methods.__bool__ = (B_bool (*) (B_LogicalD_IntegralD_i64))B_valueG_methods.__bool__;
        B_LogicalD_IntegralD_i64G_methods.__str__ = (B_str (*) (B_LogicalD_IntegralD_i64))B_valueG_methods.__str__;
        B_LogicalD_IntegralD_i64G_methods.__repr__ = (B_str (*) (B_LogicalD_IntegralD_i64))B_valueG_methods.__repr__;
        B_LogicalD_IntegralD_i64G_methods.__iand__ = (B_i64 (*) (B_LogicalD_IntegralD_i64, B_i64, B_i64))B_LogicalG_methods.__iand__;
        B_LogicalD_IntegralD_i64G_methods.__ior__ = (B_i64 (*) (B_LogicalD_IntegralD_i64, B_i64, B_i64))B_LogicalG_methods.__ior__;
        B_LogicalD_IntegralD_i64G_methods.__ixor__ = (B_i64 (*) (B_LogicalD_IntegralD_i64, B_i64, B_i64))B_LogicalG_methods.__ixor__;
        B_LogicalD_IntegralD_i64G_methods.__init__ = B_LogicalD_IntegralD_i64D___init__;
        B_LogicalD_IntegralD_i64G_methods.__xor__ = B_LogicalD_IntegralD_i64D___xor__;
        B_LogicalD_IntegralD_i64G_methods.__or__ = B_LogicalD_IntegralD_i64D___or__;
        B_LogicalD_IntegralD_i64G_methods.__and__ = B_LogicalD_IntegralD_i64D___and__;
        B_LogicalD_IntegralD_i64G_methods.__serialize__ = B_LogicalD_IntegralD_i64D___serialize__;
        B_LogicalD_IntegralD_i64G_methods.__deserialize__ = B_LogicalD_IntegralD_i64D___deserialize__;
        $register(&B_LogicalD_IntegralD_i64G_methods);
    }
    {
        B_DivD_i64G_methods.$GCINFO = "B_DivD_i64";
        B_DivD_i64G_methods.$superclass = ($SuperG_class)&B_DivG_methods;
        B_DivD_i64G_methods.__bool__ = (B_bool (*) (B_DivD_i64))B_valueG_methods.__bool__;
        B_DivD_i64G_methods.__str__ = (B_str (*) (B_DivD_i64))B_valueG_methods.__str__;
        B_DivD_i64G_methods.__repr__ = (B_str (*) (B_DivD_i64))B_valueG_methods.__repr__;
        B_DivD_i64G_methods.__itruediv__ = (B_float (*) (B_DivD_i64, B_i64, B_i64))B_DivG_methods.__itruediv__;
        B_DivD_i64G_methods.__init__ = B_DivD_i64D___init__;
        B_DivD_i64G_methods.__truediv__ = B_DivD_i64D___truediv__;
        B_DivD_i64G_methods.__serialize__ = B_DivD_i64D___serialize__;
        B_DivD_i64G_methods.__deserialize__ = B_DivD_i64D___deserialize__;
        $register(&B_DivD_i64G_methods);
    }
    {
        B_OrdD_i64G_methods.$GCINFO = "B_OrdD_i64";
        B_OrdD_i64G_methods.$superclass = ($SuperG_class)&B_OrdG_methods;
        B_OrdD_i64G_methods.__bool__ = (B_bool (*) (B_OrdD_i64))B_valueG_methods.__bool__;
        B_OrdD_i64G_methods.__str__ = (B_str (*) (B_OrdD_i64))B_valueG_methods.__str__;
        B_OrdD_i64G_methods.__repr__ = (B_str (*) (B_OrdD_i64))B_valueG_methods.__repr__;
        B_OrdD_i64G_methods.__ne__ = (B_bool (*) (B_OrdD_i64, B_i64, B_i64))B_EqG_methods.__ne__;
        B_OrdD_i64G_methods.__le__ = (B_bool (*) (B_OrdD_i64, B_i64, B_i64))B_OrdG_methods.__le__;
        B_OrdD_i64G_methods.__gt__ = (B_bool (*) (B_OrdD_i64, B_i64, B_i64))B_OrdG_methods.__gt__;
        B_OrdD_i64G_methods.__ge__ = (B_bool (*) (B_OrdD_i64, B_i64, B_i64))B_OrdG_methods.__ge__;
        B_OrdD_i64G_methods.__init__ = B_OrdD_i64D___init__;
        B_OrdD_i64G_methods.__lt__ = B_OrdD_i64D___lt__;
        B_OrdD_i64G_methods.__eq__ = B_OrdD_i64D___eq__;
        B_OrdD_i64G_methods.__serialize__ = B_OrdD_i64D___serialize__;
        B_OrdD_i64G_methods.__deserialize__ = B_OrdD_i64D___deserialize__;
        $register(&B_OrdD_i64G_methods);
    }
    {
        B_HashableD_i64G_methods.$GCINFO = "B_HashableD_i64";
        B_HashableD_i64G_methods.$superclass = ($SuperG_class)&B_HashableG_methods;
        B_HashableD_i64G_methods.__bool__ = (B_bool (*) (B_HashableD_i64))B_valueG_methods.__bool__;
        B_HashableD_i64G_methods.__str__ = (B_str (*) (B_HashableD_i64))B_valueG_methods.__str__;
        B_HashableD_i64G_methods.__repr__ = (B_str (*) (B_HashableD_i64))B_valueG_methods.__repr__;
        B_HashableD_i64G_methods.__eq__ = B_HashableD_i64D___eq__;
        B_HashableD_i64G_methods.__ne__ = (B_bool (*) (B_HashableD_i64, B_i64, B_i64))B_EqG_methods.__ne__;
        B_HashableD_i64G_methods.__init__ = B_HashableD_i64D___init__;
        B_HashableD_i64G_methods.__hash__ = B_HashableD_i64D___hash__;
        B_HashableD_i64G_methods.__serialize__ = B_HashableD_i64D___serialize__;
        B_HashableD_i64G_methods.__deserialize__ = B_HashableD_i64D___deserialize__;
        $register(&B_HashableD_i64G_methods);
    }
    {
        B_RealFloatD_floatG_methods.$GCINFO = "B_RealFloatD_float";
        B_RealFloatD_floatG_methods.$superclass = ($SuperG_class)&B_RealFloatG_methods;
        B_RealFloatD_floatG_methods.__bool__ = (B_bool (*) (B_RealFloatD_float))B_valueG_methods.__bool__;
        B_RealFloatD_floatG_methods.__str__ = (B_str (*) (B_RealFloatD_float))B_valueG_methods.__str__;
        B_RealFloatD_floatG_methods.__repr__ = (B_str (*) (B_RealFloatD_float))B_valueG_methods.__repr__;
        B_RealFloatD_floatG_methods.__iadd__ = (B_float (*) (B_RealFloatD_float, B_float, B_float))B_PlusG_methods.__iadd__;
        B_RealFloatD_floatG_methods.__imul__ = (B_float (*) (B_RealFloatD_float, B_float, B_float))B_TimesG_methods.__imul__;
        B_RealFloatD_floatG_methods.__ipow__ = (B_float (*) (B_RealFloatD_float, B_float, B_float))B_NumberG_methods.__ipow__;
        B_RealFloatD_floatG_methods.__init__ = B_RealFloatD_floatD___init__;
        B_RealFloatD_floatG_methods.__round__ = B_RealFloatD_floatD___round__;
        B_RealFloatD_floatG_methods.__ceil__ = B_RealFloatD_floatD___ceil__;
        B_RealFloatD_floatG_methods.__floor__ = B_RealFloatD_floatD___floor__;
        B_RealFloatD_floatG_methods.__trunc__ = B_RealFloatD_floatD___trunc__;
        B_RealFloatD_floatG_methods.__float__ = B_RealFloatD_floatD___float__;
        B_RealFloatD_floatG_methods.conjugate = B_RealFloatD_floatD_conjugate;
        B_RealFloatD_floatG_methods.__abs__ = B_RealFloatD_floatD___abs__;
        B_RealFloatD_floatG_methods.imag = B_RealFloatD_floatD_imag;
        B_RealFloatD_floatG_methods.real = B_RealFloatD_floatD_real;
        B_RealFloatD_floatG_methods.__pos__ = B_RealFloatD_floatD___pos__;
        B_RealFloatD_floatG_methods.__neg__ = B_RealFloatD_floatD___neg__;
        B_RealFloatD_floatG_methods.__pow__ = B_RealFloatD_floatD___pow__;
        B_RealFloatD_floatG_methods.__complx__ = B_RealFloatD_floatD___complex__;
        B_RealFloatD_floatG_methods.__fromatom__ = B_RealFloatD_floatD___fromatom__;
        B_RealFloatD_floatG_methods.__mul__ = B_RealFloatD_floatD___mul__;
        B_RealFloatD_floatG_methods.__add__ = B_RealFloatD_floatD___add__;
        B_RealFloatD_floatG_methods.__serialize__ = B_RealFloatD_floatD___serialize__;
        B_RealFloatD_floatG_methods.__deserialize__ = B_RealFloatD_floatD___deserialize__;
        $register(&B_RealFloatD_floatG_methods);
    }
    {
        B_MinusD_RealFloatD_floatG_methods.$GCINFO = "B_MinusD_RealFloatD_float";
        B_MinusD_RealFloatD_floatG_methods.$superclass = ($SuperG_class)&B_MinusD_RealFloatG_methods;
        B_MinusD_RealFloatD_floatG_methods.__bool__ = (B_bool (*) (B_MinusD_RealFloatD_float))B_valueG_methods.__bool__;
        B_MinusD_RealFloatD_floatG_methods.__str__ = (B_str (*) (B_MinusD_RealFloatD_float))B_valueG_methods.__str__;
        B_MinusD_RealFloatD_floatG_methods.__repr__ = (B_str (*) (B_MinusD_RealFloatD_float))B_valueG_methods.__repr__;
        B_MinusD_RealFloatD_floatG_methods.__isub__ = (B_float (*) (B_MinusD_RealFloatD_float, B_float, B_float))B_MinusG_methods.__isub__;
        B_MinusD_RealFloatD_floatG_methods.__init__ = B_MinusD_RealFloatD_floatD___init__;
        B_MinusD_RealFloatD_floatG_methods.__sub__ = B_MinusD_RealFloatD_floatD___sub__;
        B_MinusD_RealFloatD_floatG_methods.__serialize__ = B_MinusD_RealFloatD_floatD___serialize__;
        B_MinusD_RealFloatD_floatG_methods.__deserialize__ = B_MinusD_RealFloatD_floatD___deserialize__;
        $register(&B_MinusD_RealFloatD_floatG_methods);
    }
    {
        B_DivD_floatG_methods.$GCINFO = "B_DivD_float";
        B_DivD_floatG_methods.$superclass = ($SuperG_class)&B_DivG_methods;
        B_DivD_floatG_methods.__bool__ = (B_bool (*) (B_DivD_float))B_valueG_methods.__bool__;
        B_DivD_floatG_methods.__str__ = (B_str (*) (B_DivD_float))B_valueG_methods.__str__;
        B_DivD_floatG_methods.__repr__ = (B_str (*) (B_DivD_float))B_valueG_methods.__repr__;
        B_DivD_floatG_methods.__itruediv__ = (B_float (*) (B_DivD_float, B_float, B_float))B_DivG_methods.__itruediv__;
        B_DivD_floatG_methods.__init__ = B_DivD_floatD___init__;
        B_DivD_floatG_methods.__truediv__ = B_DivD_floatD___truediv__;
        B_DivD_floatG_methods.__serialize__ = B_DivD_floatD___serialize__;
        B_DivD_floatG_methods.__deserialize__ = B_DivD_floatD___deserialize__;
        $register(&B_DivD_floatG_methods);
    }
    {
        B_OrdD_floatG_methods.$GCINFO = "B_OrdD_float";
        B_OrdD_floatG_methods.$superclass = ($SuperG_class)&B_OrdG_methods;
        B_OrdD_floatG_methods.__bool__ = (B_bool (*) (B_OrdD_float))B_valueG_methods.__bool__;
        B_OrdD_floatG_methods.__str__ = (B_str (*) (B_OrdD_float))B_valueG_methods.__str__;
        B_OrdD_floatG_methods.__repr__ = (B_str (*) (B_OrdD_float))B_valueG_methods.__repr__;
        B_OrdD_floatG_methods.__ne__ = (B_bool (*) (B_OrdD_float, B_float, B_float))B_EqG_methods.__ne__;
        B_OrdD_floatG_methods.__le__ = (B_bool (*) (B_OrdD_float, B_float, B_float))B_OrdG_methods.__le__;
        B_OrdD_floatG_methods.__gt__ = (B_bool (*) (B_OrdD_float, B_float, B_float))B_OrdG_methods.__gt__;
        B_OrdD_floatG_methods.__ge__ = (B_bool (*) (B_OrdD_float, B_float, B_float))B_OrdG_methods.__ge__;
        B_OrdD_floatG_methods.__init__ = B_OrdD_floatD___init__;
        B_OrdD_floatG_methods.__lt__ = B_OrdD_floatD___lt__;
        B_OrdD_floatG_methods.__eq__ = B_OrdD_floatD___eq__;
        B_OrdD_floatG_methods.__serialize__ = B_OrdD_floatD___serialize__;
        B_OrdD_floatG_methods.__deserialize__ = B_OrdD_floatD___deserialize__;
        $register(&B_OrdD_floatG_methods);
    }
    {
        B_HashableD_floatG_methods.$GCINFO = "B_HashableD_float";
        B_HashableD_floatG_methods.$superclass = ($SuperG_class)&B_HashableG_methods;
        B_HashableD_floatG_methods.__bool__ = (B_bool (*) (B_HashableD_float))B_valueG_methods.__bool__;
        B_HashableD_floatG_methods.__str__ = (B_str (*) (B_HashableD_float))B_valueG_methods.__str__;
        B_HashableD_floatG_methods.__repr__ = (B_str (*) (B_HashableD_float))B_valueG_methods.__repr__;
        B_HashableD_floatG_methods.__ne__ = (B_bool (*) (B_HashableD_float, B_float, B_float))B_EqG_methods.__ne__;
        B_HashableD_floatG_methods.__init__ = B_HashableD_floatD___init__;
        B_HashableD_floatG_methods.__hash__ = B_HashableD_floatD___hash__;
        B_HashableD_floatG_methods.__serialize__ = B_HashableD_floatD___serialize__;
        B_HashableD_floatG_methods.__deserialize__ = B_HashableD_floatD___deserialize__;
        $register(&B_HashableD_floatG_methods);
    }
    {
        B_NumberD_complexG_methods.$GCINFO = "B_NumberD_complex";
        B_NumberD_complexG_methods.$superclass = ($SuperG_class)&B_NumberG_methods;
        B_NumberD_complexG_methods.__bool__ = (B_bool (*) (B_NumberD_complex))B_valueG_methods.__bool__;
        B_NumberD_complexG_methods.__str__ = (B_str (*) (B_NumberD_complex))B_valueG_methods.__str__;
        B_NumberD_complexG_methods.__repr__ = (B_str (*) (B_NumberD_complex))B_valueG_methods.__repr__;
        B_NumberD_complexG_methods.__iadd__ = (B_complex (*) (B_NumberD_complex, B_complex, B_complex))B_PlusG_methods.__iadd__;
        B_NumberD_complexG_methods.__imul__ = (B_complex (*) (B_NumberD_complex, B_complex, B_complex))B_TimesG_methods.__imul__;
        B_NumberD_complexG_methods.__ipow__ = (B_complex (*) (B_NumberD_complex, B_complex, B_complex))B_NumberG_methods.__ipow__;
        B_NumberD_complexG_methods.__init__ = B_NumberD_complexD___init__;
        B_NumberD_complexG_methods.conjugate = B_NumberD_complexD_conjugate;
        B_NumberD_complexG_methods.__abs__ = B_NumberD_complexD___abs__;
        B_NumberD_complexG_methods.imag = B_NumberD_complexD_imag;
        B_NumberD_complexG_methods.real = B_NumberD_complexD_real;
        B_NumberD_complexG_methods.__pos__ = B_NumberD_complexD___pos__;
        B_NumberD_complexG_methods.__neg__ = B_NumberD_complexD___neg__;
        B_NumberD_complexG_methods.__pow__ = B_NumberD_complexD___pow__;
        B_NumberD_complexG_methods.__complx__ = B_NumberD_complexD___complex__;
        B_NumberD_complexG_methods.__fromatom__ = B_NumberD_complexD___fromatom__;
        B_NumberD_complexG_methods.__mul__ = B_NumberD_complexD___mul__;
        B_NumberD_complexG_methods.__add__ = B_NumberD_complexD___add__;
        B_NumberD_complexG_methods.__serialize__ = B_NumberD_complexD___serialize__;
        B_NumberD_complexG_methods.__deserialize__ = B_NumberD_complexD___deserialize__;
        $register(&B_NumberD_complexG_methods);
    }
    {
        B_MinusD_NumberD_complexG_methods.$GCINFO = "B_MinusD_NumberD_complex";
        B_MinusD_NumberD_complexG_methods.$superclass = ($SuperG_class)&B_MinusD_NumberG_methods;
        B_MinusD_NumberD_complexG_methods.__bool__ = (B_bool (*) (B_MinusD_NumberD_complex))B_valueG_methods.__bool__;
        B_MinusD_NumberD_complexG_methods.__str__ = (B_str (*) (B_MinusD_NumberD_complex))B_valueG_methods.__str__;
        B_MinusD_NumberD_complexG_methods.__repr__ = (B_str (*) (B_MinusD_NumberD_complex))B_valueG_methods.__repr__;
        B_MinusD_NumberD_complexG_methods.__isub__ = (B_complex (*) (B_MinusD_NumberD_complex, B_complex, B_complex))B_MinusG_methods.__isub__;
        B_MinusD_NumberD_complexG_methods.__init__ = B_MinusD_NumberD_complexD___init__;
        B_MinusD_NumberD_complexG_methods.__sub__ = B_MinusD_NumberD_complexD___sub__;
        B_MinusD_NumberD_complexG_methods.__serialize__ = B_MinusD_NumberD_complexD___serialize__;
        B_MinusD_NumberD_complexG_methods.__deserialize__ = B_MinusD_NumberD_complexD___deserialize__;
        $register(&B_MinusD_NumberD_complexG_methods);
    }
    {
        B_DivD_complexG_methods.$GCINFO = "B_DivD_complex";
        B_DivD_complexG_methods.$superclass = ($SuperG_class)&B_DivG_methods;
        B_DivD_complexG_methods.__bool__ = (B_bool (*) (B_DivD_complex))B_valueG_methods.__bool__;
        B_DivD_complexG_methods.__str__ = (B_str (*) (B_DivD_complex))B_valueG_methods.__str__;
        B_DivD_complexG_methods.__repr__ = (B_str (*) (B_DivD_complex))B_valueG_methods.__repr__;
        B_DivD_complexG_methods.__itruediv__ = (B_complex (*) (B_DivD_complex, B_complex, B_complex))B_DivG_methods.__itruediv__;
        B_DivD_complexG_methods.__init__ = B_DivD_complexD___init__;
        B_DivD_complexG_methods.__truediv__ = B_DivD_complexD___truediv__;
        B_DivD_complexG_methods.__serialize__ = B_DivD_complexD___serialize__;
        B_DivD_complexG_methods.__deserialize__ = B_DivD_complexD___deserialize__;
        $register(&B_DivD_complexG_methods);
    }
    {
        B_EqD_complexG_methods.$GCINFO = "B_EqD_complex";
        B_EqD_complexG_methods.$superclass = ($SuperG_class)&B_EqG_methods;
        B_EqD_complexG_methods.__bool__ = (B_bool (*) (B_EqD_complex))B_valueG_methods.__bool__;
        B_EqD_complexG_methods.__str__ = (B_str (*) (B_EqD_complex))B_valueG_methods.__str__;
        B_EqD_complexG_methods.__repr__ = (B_str (*) (B_EqD_complex))B_valueG_methods.__repr__;
        B_EqD_complexG_methods.__ne__ = (B_bool (*) (B_EqD_complex, B_complex, B_complex))B_EqG_methods.__ne__;
        B_EqD_complexG_methods.__init__ = B_EqD_complexD___init__;
        B_EqD_complexG_methods.__eq__ = B_EqD_complexD___eq__;
        B_EqD_complexG_methods.__serialize__ = B_EqD_complexD___serialize__;
        B_EqD_complexG_methods.__deserialize__ = B_EqD_complexD___deserialize__;
        $register(&B_EqD_complexG_methods);
    }
    {
        B_HashableD_complexG_methods.$GCINFO = "B_HashableD_complex";
        B_HashableD_complexG_methods.$superclass = ($SuperG_class)&B_HashableG_methods;
        B_HashableD_complexG_methods.__bool__ = (B_bool (*) (B_HashableD_complex))B_valueG_methods.__bool__;
        B_HashableD_complexG_methods.__str__ = (B_str (*) (B_HashableD_complex))B_valueG_methods.__str__;
        B_HashableD_complexG_methods.__repr__ = (B_str (*) (B_HashableD_complex))B_valueG_methods.__repr__;
        B_HashableD_complexG_methods.__ne__ = (B_bool (*) (B_HashableD_complex, B_complex, B_complex))B_EqG_methods.__ne__;
        B_HashableD_complexG_methods.__init__ = B_HashableD_complexD___init__;
        B_HashableD_complexG_methods.__hash__ = B_HashableD_complexD___hash__;
        B_HashableD_complexG_methods.__serialize__ = B_HashableD_complexD___serialize__;
        B_HashableD_complexG_methods.__deserialize__ = B_HashableD_complexD___deserialize__;
        $register(&B_HashableD_complexG_methods);
    }
    {
        B_IndexedG_methods.$GCINFO = "B_Indexed";
        B_IndexedG_methods.$superclass = ($SuperG_class)&B_valueG_methods;
        B_IndexedG_methods.__bool__ = (B_bool (*) (B_Indexed))B_valueG_methods.__bool__;
        B_IndexedG_methods.__str__ = (B_str (*) (B_Indexed))B_valueG_methods.__str__;
        B_IndexedG_methods.__repr__ = (B_str (*) (B_Indexed))B_valueG_methods.__repr__;
        B_IndexedG_methods.__init__ = B_IndexedD___init__;
        B_IndexedG_methods.__serialize__ = B_IndexedD___serialize__;
        B_IndexedG_methods.__deserialize__ = B_IndexedD___deserialize__;
        $register(&B_IndexedG_methods);
    }
    {
        B_SliceableG_methods.$GCINFO = "B_Sliceable";
        B_SliceableG_methods.$superclass = ($SuperG_class)&B_IndexedG_methods;
        B_SliceableG_methods.__bool__ = (B_bool (*) (B_Sliceable))B_valueG_methods.__bool__;
        B_SliceableG_methods.__str__ = (B_str (*) (B_Sliceable))B_valueG_methods.__str__;
        B_SliceableG_methods.__repr__ = (B_str (*) (B_Sliceable))B_valueG_methods.__repr__;
        B_SliceableG_methods.__init__ = B_SliceableD___init__;
        B_SliceableG_methods.__serialize__ = B_SliceableD___serialize__;
        B_SliceableG_methods.__deserialize__ = B_SliceableD___deserialize__;
        $register(&B_SliceableG_methods);
    }
    {
        B_CollectionG_methods.$GCINFO = "B_Collection";
        B_CollectionG_methods.$superclass = ($SuperG_class)&B_IterableG_methods;
        B_CollectionG_methods.__bool__ = (B_bool (*) (B_Collection))B_valueG_methods.__bool__;
        B_CollectionG_methods.__str__ = (B_str (*) (B_Collection))B_valueG_methods.__str__;
        B_CollectionG_methods.__repr__ = (B_str (*) (B_Collection))B_valueG_methods.__repr__;
        B_CollectionG_methods.__init__ = B_CollectionD___init__;
        B_CollectionG_methods.__serialize__ = B_CollectionD___serialize__;
        B_CollectionG_methods.__deserialize__ = B_CollectionD___deserialize__;
        $register(&B_CollectionG_methods);
    }
    {
        B_ContainerG_methods.$GCINFO = "B_Container";
        B_ContainerG_methods.$superclass = ($SuperG_class)&B_CollectionG_methods;
        B_ContainerG_methods.__bool__ = (B_bool (*) (B_Container))B_valueG_methods.__bool__;
        B_ContainerG_methods.__str__ = (B_str (*) (B_Container))B_valueG_methods.__str__;
        B_ContainerG_methods.__repr__ = (B_str (*) (B_Container))B_valueG_methods.__repr__;
        B_ContainerG_methods.__init__ = B_ContainerD___init__;
        B_ContainerG_methods.__serialize__ = B_ContainerD___serialize__;
        B_ContainerG_methods.__deserialize__ = B_ContainerD___deserialize__;
        $register(&B_ContainerG_methods);
    }
    {
        B_SequenceG_methods.$GCINFO = "B_Sequence";
        B_SequenceG_methods.$superclass = ($SuperG_class)&B_SliceableG_methods;
        B_SequenceG_methods.__bool__ = (B_bool (*) (B_Sequence))B_valueG_methods.__bool__;
        B_SequenceG_methods.__str__ = (B_str (*) (B_Sequence))B_valueG_methods.__str__;
        B_SequenceG_methods.__repr__ = (B_str (*) (B_Sequence))B_valueG_methods.__repr__;
        B_SequenceG_methods.__init__ = B_SequenceD___init__;
        B_SequenceG_methods.__serialize__ = B_SequenceD___serialize__;
        B_SequenceG_methods.__deserialize__ = B_SequenceD___deserialize__;
        $register(&B_SequenceG_methods);
    }
    {
        B_CollectionD_SequenceG_methods.$GCINFO = "B_CollectionD_Sequence";
        B_CollectionD_SequenceG_methods.$superclass = ($SuperG_class)&B_CollectionG_methods;
        B_CollectionD_SequenceG_methods.__bool__ = (B_bool (*) (B_CollectionD_Sequence))B_valueG_methods.__bool__;
        B_CollectionD_SequenceG_methods.__str__ = (B_str (*) (B_CollectionD_Sequence))B_valueG_methods.__str__;
        B_CollectionD_SequenceG_methods.__repr__ = (B_str (*) (B_CollectionD_Sequence))B_valueG_methods.__repr__;
        B_CollectionD_SequenceG_methods.__init__ = B_CollectionD_SequenceD___init__;
        B_CollectionD_SequenceG_methods.__serialize__ = B_CollectionD_SequenceD___serialize__;
        B_CollectionD_SequenceG_methods.__deserialize__ = B_CollectionD_SequenceD___deserialize__;
        $register(&B_CollectionD_SequenceG_methods);
    }
    {
        B_TimesD_SequenceG_methods.$GCINFO = "B_TimesD_Sequence";
        B_TimesD_SequenceG_methods.$superclass = ($SuperG_class)&B_TimesG_methods;
        B_TimesD_SequenceG_methods.__bool__ = (B_bool (*) (B_TimesD_Sequence))B_valueG_methods.__bool__;
        B_TimesD_SequenceG_methods.__str__ = (B_str (*) (B_TimesD_Sequence))B_valueG_methods.__str__;
        B_TimesD_SequenceG_methods.__repr__ = (B_str (*) (B_TimesD_Sequence))B_valueG_methods.__repr__;
        B_TimesD_SequenceG_methods.__iadd__ = ($WORD (*) (B_TimesD_Sequence, $WORD, $WORD))B_PlusG_methods.__iadd__;
        B_TimesD_SequenceG_methods.__imul__ = ($WORD (*) (B_TimesD_Sequence, $WORD, B_int))B_TimesG_methods.__imul__;
        B_TimesD_SequenceG_methods.__init__ = B_TimesD_SequenceD___init__;
        B_TimesD_SequenceG_methods.__serialize__ = B_TimesD_SequenceD___serialize__;
        B_TimesD_SequenceG_methods.__deserialize__ = B_TimesD_SequenceD___deserialize__;
        $register(&B_TimesD_SequenceG_methods);
    }
    {
        B_MappingG_methods.$GCINFO = "B_Mapping";
        B_MappingG_methods.$superclass = ($SuperG_class)&B_ContainerG_methods;
        B_MappingG_methods.__bool__ = (B_bool (*) (B_Mapping))B_valueG_methods.__bool__;
        B_MappingG_methods.__str__ = (B_str (*) (B_Mapping))B_valueG_methods.__str__;
        B_MappingG_methods.__repr__ = (B_str (*) (B_Mapping))B_valueG_methods.__repr__;
        B_MappingG_methods.__init__ = B_MappingD___init__;
        B_MappingG_methods.__serialize__ = B_MappingD___serialize__;
        B_MappingG_methods.__deserialize__ = B_MappingD___deserialize__;
        $register(&B_MappingG_methods);
    }
    {
        B_IndexedD_MappingG_methods.$GCINFO = "B_IndexedD_Mapping";
        B_IndexedD_MappingG_methods.$superclass = ($SuperG_class)&B_IndexedG_methods;
        B_IndexedD_MappingG_methods.__bool__ = (B_bool (*) (B_IndexedD_Mapping))B_valueG_methods.__bool__;
        B_IndexedD_MappingG_methods.__str__ = (B_str (*) (B_IndexedD_Mapping))B_valueG_methods.__str__;
        B_IndexedD_MappingG_methods.__repr__ = (B_str (*) (B_IndexedD_Mapping))B_valueG_methods.__repr__;
        B_IndexedD_MappingG_methods.__init__ = B_IndexedD_MappingD___init__;
        B_IndexedD_MappingG_methods.__serialize__ = B_IndexedD_MappingD___serialize__;
        B_IndexedD_MappingG_methods.__deserialize__ = B_IndexedD_MappingD___deserialize__;
        $register(&B_IndexedD_MappingG_methods);
    }
    {
        B_SetG_methods.$GCINFO = "B_Set";
        B_SetG_methods.$superclass = ($SuperG_class)&B_ContainerG_methods;
        B_SetG_methods.__bool__ = (B_bool (*) (B_Set))B_valueG_methods.__bool__;
        B_SetG_methods.__str__ = (B_str (*) (B_Set))B_valueG_methods.__str__;
        B_SetG_methods.__repr__ = (B_str (*) (B_Set))B_valueG_methods.__repr__;
        B_SetG_methods.__init__ = B_SetD___init__;
        B_SetG_methods.__serialize__ = B_SetD___serialize__;
        B_SetG_methods.__deserialize__ = B_SetD___deserialize__;
        $register(&B_SetG_methods);
    }
    {
        B_OrdD_SetG_methods.$GCINFO = "B_OrdD_Set";
        B_OrdD_SetG_methods.$superclass = ($SuperG_class)&B_OrdG_methods;
        B_OrdD_SetG_methods.__bool__ = (B_bool (*) (B_OrdD_Set))B_valueG_methods.__bool__;
        B_OrdD_SetG_methods.__str__ = (B_str (*) (B_OrdD_Set))B_valueG_methods.__str__;
        B_OrdD_SetG_methods.__repr__ = (B_str (*) (B_OrdD_Set))B_valueG_methods.__repr__;
        B_OrdD_SetG_methods.__ne__ = (B_bool (*) (B_OrdD_Set, $WORD, $WORD))B_EqG_methods.__ne__;
        B_OrdD_SetG_methods.__le__ = (B_bool (*) (B_OrdD_Set, $WORD, $WORD))B_OrdG_methods.__le__;
        B_OrdD_SetG_methods.__gt__ = (B_bool (*) (B_OrdD_Set, $WORD, $WORD))B_OrdG_methods.__gt__;
        B_OrdD_SetG_methods.__ge__ = (B_bool (*) (B_OrdD_Set, $WORD, $WORD))B_OrdG_methods.__ge__;
        B_OrdD_SetG_methods.__init__ = B_OrdD_SetD___init__;
        B_OrdD_SetG_methods.__serialize__ = B_OrdD_SetD___serialize__;
        B_OrdD_SetG_methods.__deserialize__ = B_OrdD_SetD___deserialize__;
        $register(&B_OrdD_SetG_methods);
    }
    {
        B_LogicalD_SetG_methods.$GCINFO = "B_LogicalD_Set";
        B_LogicalD_SetG_methods.$superclass = ($SuperG_class)&B_LogicalG_methods;
        B_LogicalD_SetG_methods.__bool__ = (B_bool (*) (B_LogicalD_Set))B_valueG_methods.__bool__;
        B_LogicalD_SetG_methods.__str__ = (B_str (*) (B_LogicalD_Set))B_valueG_methods.__str__;
        B_LogicalD_SetG_methods.__repr__ = (B_str (*) (B_LogicalD_Set))B_valueG_methods.__repr__;
        B_LogicalD_SetG_methods.__iand__ = ($WORD (*) (B_LogicalD_Set, $WORD, $WORD))B_LogicalG_methods.__iand__;
        B_LogicalD_SetG_methods.__ior__ = ($WORD (*) (B_LogicalD_Set, $WORD, $WORD))B_LogicalG_methods.__ior__;
        B_LogicalD_SetG_methods.__ixor__ = ($WORD (*) (B_LogicalD_Set, $WORD, $WORD))B_LogicalG_methods.__ixor__;
        B_LogicalD_SetG_methods.__init__ = B_LogicalD_SetD___init__;
        B_LogicalD_SetG_methods.__serialize__ = B_LogicalD_SetD___serialize__;
        B_LogicalD_SetG_methods.__deserialize__ = B_LogicalD_SetD___deserialize__;
        $register(&B_LogicalD_SetG_methods);
    }
    {
        B_MinusD_SetG_methods.$GCINFO = "B_MinusD_Set";
        B_MinusD_SetG_methods.$superclass = ($SuperG_class)&B_MinusG_methods;
        B_MinusD_SetG_methods.__bool__ = (B_bool (*) (B_MinusD_Set))B_valueG_methods.__bool__;
        B_MinusD_SetG_methods.__str__ = (B_str (*) (B_MinusD_Set))B_valueG_methods.__str__;
        B_MinusD_SetG_methods.__repr__ = (B_str (*) (B_MinusD_Set))B_valueG_methods.__repr__;
        B_MinusD_SetG_methods.__isub__ = ($WORD (*) (B_MinusD_Set, $WORD, $WORD))B_MinusG_methods.__isub__;
        B_MinusD_SetG_methods.__init__ = B_MinusD_SetD___init__;
        B_MinusD_SetG_methods.__serialize__ = B_MinusD_SetD___serialize__;
        B_MinusD_SetG_methods.__deserialize__ = B_MinusD_SetD___deserialize__;
        $register(&B_MinusD_SetG_methods);
    }
    {
        B_SequenceD_listG_methods.$GCINFO = "B_SequenceD_list";
        B_SequenceD_listG_methods.$superclass = ($SuperG_class)&B_SequenceG_methods;
        B_SequenceD_listG_methods.__bool__ = (B_bool (*) (B_SequenceD_list))B_valueG_methods.__bool__;
        B_SequenceD_listG_methods.__str__ = (B_str (*) (B_SequenceD_list))B_valueG_methods.__str__;
        B_SequenceD_listG_methods.__repr__ = (B_str (*) (B_SequenceD_list))B_valueG_methods.__repr__;
        B_SequenceD_listG_methods.__init__ = B_SequenceD_listD___init__;
        B_SequenceD_listG_methods.reverse = B_SequenceD_listD_reverse;
        B_SequenceD_listG_methods.append = B_SequenceD_listD_append;
        B_SequenceD_listG_methods.insert = B_SequenceD_listD_insert;
        B_SequenceD_listG_methods.__reversed__ = B_SequenceD_listD___reversed__;
        B_SequenceD_listG_methods.__delslice__ = B_SequenceD_listD___delslice__;
        B_SequenceD_listG_methods.__setslice__ = B_SequenceD_listD___setslice__;
        B_SequenceD_listG_methods.__getslice__ = B_SequenceD_listD___getslice__;
        B_SequenceD_listG_methods.__delitem__ = B_SequenceD_listD___delitem__;
        B_SequenceD_listG_methods.__setitem__ = B_SequenceD_listD___setitem__;
        B_SequenceD_listG_methods.__getitem__ = B_SequenceD_listD___getitem__;
        B_SequenceD_listG_methods.__serialize__ = B_SequenceD_listD___serialize__;
        B_SequenceD_listG_methods.__deserialize__ = B_SequenceD_listD___deserialize__;
        $register(&B_SequenceD_listG_methods);
    }
    {
        B_CollectionD_SequenceD_listG_methods.$GCINFO = "B_CollectionD_SequenceD_list";
        B_CollectionD_SequenceD_listG_methods.$superclass = ($SuperG_class)&B_CollectionD_SequenceG_methods;
        B_CollectionD_SequenceD_listG_methods.__bool__ = (B_bool (*) (B_CollectionD_SequenceD_list))B_valueG_methods.__bool__;
        B_CollectionD_SequenceD_listG_methods.__str__ = (B_str (*) (B_CollectionD_SequenceD_list))B_valueG_methods.__str__;
        B_CollectionD_SequenceD_listG_methods.__repr__ = (B_str (*) (B_CollectionD_SequenceD_list))B_valueG_methods.__repr__;
        B_CollectionD_SequenceD_listG_methods.__init__ = B_CollectionD_SequenceD_listD___init__;
        B_CollectionD_SequenceD_listG_methods.__len__ = B_CollectionD_SequenceD_listD___len__;
        B_CollectionD_SequenceD_listG_methods.__fromiter__ = B_CollectionD_SequenceD_listD___fromiter__;
        B_CollectionD_SequenceD_listG_methods.__iter__ = B_CollectionD_SequenceD_listD___iter__;
        B_CollectionD_SequenceD_listG_methods.__serialize__ = B_CollectionD_SequenceD_listD___serialize__;
        B_CollectionD_SequenceD_listG_methods.__deserialize__ = B_CollectionD_SequenceD_listD___deserialize__;
        $register(&B_CollectionD_SequenceD_listG_methods);
    }
    {
        B_TimesD_SequenceD_listG_methods.$GCINFO = "B_TimesD_SequenceD_list";
        B_TimesD_SequenceD_listG_methods.$superclass = ($SuperG_class)&B_TimesD_SequenceG_methods;
        B_TimesD_SequenceD_listG_methods.__bool__ = (B_bool (*) (B_TimesD_SequenceD_list))B_valueG_methods.__bool__;
        B_TimesD_SequenceD_listG_methods.__str__ = (B_str (*) (B_TimesD_SequenceD_list))B_valueG_methods.__str__;
        B_TimesD_SequenceD_listG_methods.__repr__ = (B_str (*) (B_TimesD_SequenceD_list))B_valueG_methods.__repr__;
        B_TimesD_SequenceD_listG_methods.__iadd__ = (B_list (*) (B_TimesD_SequenceD_list, B_list, B_list))B_PlusG_methods.__iadd__;
        B_TimesD_SequenceD_listG_methods.__imul__ = (B_list (*) (B_TimesD_SequenceD_list, B_list, B_int))B_TimesG_methods.__imul__;
        B_TimesD_SequenceD_listG_methods.__init__ = B_TimesD_SequenceD_listD___init__;
        B_TimesD_SequenceD_listG_methods.__mul__ = B_TimesD_SequenceD_listD___mul__;
        B_TimesD_SequenceD_listG_methods.__add__ = B_TimesD_SequenceD_listD___add__;
        B_TimesD_SequenceD_listG_methods.__serialize__ = B_TimesD_SequenceD_listD___serialize__;
        B_TimesD_SequenceD_listG_methods.__deserialize__ = B_TimesD_SequenceD_listD___deserialize__;
        $register(&B_TimesD_SequenceD_listG_methods);
    }
    {
        B_ContainerD_listG_methods.$GCINFO = "B_ContainerD_list";
        B_ContainerD_listG_methods.$superclass = ($SuperG_class)&B_ContainerG_methods;
        B_ContainerD_listG_methods.__bool__ = (B_bool (*) (B_ContainerD_list))B_valueG_methods.__bool__;
        B_ContainerD_listG_methods.__str__ = (B_str (*) (B_ContainerD_list))B_valueG_methods.__str__;
        B_ContainerD_listG_methods.__repr__ = (B_str (*) (B_ContainerD_list))B_valueG_methods.__repr__;
        B_ContainerD_listG_methods.__init__ = B_ContainerD_listD___init__;
        B_ContainerD_listG_methods.__containsnot__ = B_ContainerD_listD___containsnot__;
        B_ContainerD_listG_methods.__contains__ = B_ContainerD_listD___contains__;
        B_ContainerD_listG_methods.__serialize__ = B_ContainerD_listD___serialize__;
        B_ContainerD_listG_methods.__deserialize__ = B_ContainerD_listD___deserialize__;
        $register(&B_ContainerD_listG_methods);
    }
    {
        B_OrdD_listG_methods.$GCINFO = "B_OrdD_list";
        B_OrdD_listG_methods.$superclass = ($SuperG_class)&B_OrdG_methods;
        B_OrdD_listG_methods.__bool__ = (B_bool (*) (B_OrdD_list))B_valueG_methods.__bool__;
        B_OrdD_listG_methods.__str__ = (B_str (*) (B_OrdD_list))B_valueG_methods.__str__;
        B_OrdD_listG_methods.__repr__ = (B_str (*) (B_OrdD_list))B_valueG_methods.__repr__;
        B_OrdD_listG_methods.__ne__ = (B_bool (*) (B_OrdD_list, B_list, B_list))B_EqG_methods.__ne__;
        B_OrdD_listG_methods.__le__ = (B_bool (*) (B_OrdD_list, B_list, B_list))B_OrdG_methods.__le__;
        B_OrdD_listG_methods.__gt__ = (B_bool (*) (B_OrdD_list, B_list, B_list))B_OrdG_methods.__gt__;
        B_OrdD_listG_methods.__ge__ = (B_bool (*) (B_OrdD_list, B_list, B_list))B_OrdG_methods.__ge__;
        B_OrdD_listG_methods.__init__ = B_OrdD_listD___init__;
        B_OrdD_listG_methods.__lt__ = B_OrdD_listD___lt__;
        B_OrdD_listG_methods.__eq__ = B_OrdD_listD___eq__;
        B_OrdD_listG_methods.__serialize__ = B_OrdD_listD___serialize__;
        B_OrdD_listG_methods.__deserialize__ = B_OrdD_listD___deserialize__;
        $register(&B_OrdD_listG_methods);
    }
    {
        B_MappingD_dictG_methods.$GCINFO = "B_MappingD_dict";
        B_MappingD_dictG_methods.$superclass = ($SuperG_class)&B_MappingG_methods;
        B_MappingD_dictG_methods.__bool__ = (B_bool (*) (B_MappingD_dict))B_valueG_methods.__bool__;
        B_MappingD_dictG_methods.__str__ = (B_str (*) (B_MappingD_dict))B_valueG_methods.__str__;
        B_MappingD_dictG_methods.__repr__ = (B_str (*) (B_MappingD_dict))B_valueG_methods.__repr__;
        B_MappingD_dictG_methods.__init__ = B_MappingD_dictD___init__;
        B_MappingD_dictG_methods.setdefault = B_MappingD_dictD_setdefault;
        B_MappingD_dictG_methods.popitem = B_MappingD_dictD_popitem;
        B_MappingD_dictG_methods.update = B_MappingD_dictD_update;
        B_MappingD_dictG_methods.items = B_MappingD_dictD_items;
        B_MappingD_dictG_methods.values = B_MappingD_dictD_values;
        B_MappingD_dictG_methods.keys = B_MappingD_dictD_keys;
        B_MappingD_dictG_methods.get = B_MappingD_dictD_get;
        B_MappingD_dictG_methods.__containsnot__ = B_MappingD_dictD___containsnot__;
        B_MappingD_dictG_methods.__contains__ = B_MappingD_dictD___contains__;
        B_MappingD_dictG_methods.__len__ = B_MappingD_dictD___len__;
        B_MappingD_dictG_methods.__fromiter__ = B_MappingD_dictD___fromiter__;
        B_MappingD_dictG_methods.__iter__ = B_MappingD_dictD___iter__;
        B_MappingD_dictG_methods.__serialize__ = B_MappingD_dictD___serialize__;
        B_MappingD_dictG_methods.__deserialize__ = B_MappingD_dictD___deserialize__;
        $register(&B_MappingD_dictG_methods);
    }
    {
        B_IndexedD_MappingD_dictG_methods.$GCINFO = "B_IndexedD_MappingD_dict";
        B_IndexedD_MappingD_dictG_methods.$superclass = ($SuperG_class)&B_IndexedD_MappingG_methods;
        B_IndexedD_MappingD_dictG_methods.__bool__ = (B_bool (*) (B_IndexedD_MappingD_dict))B_valueG_methods.__bool__;
        B_IndexedD_MappingD_dictG_methods.__str__ = (B_str (*) (B_IndexedD_MappingD_dict))B_valueG_methods.__str__;
        B_IndexedD_MappingD_dictG_methods.__repr__ = (B_str (*) (B_IndexedD_MappingD_dict))B_valueG_methods.__repr__;
        B_IndexedD_MappingD_dictG_methods.__init__ = B_IndexedD_MappingD_dictD___init__;
        B_IndexedD_MappingD_dictG_methods.__delitem__ = B_IndexedD_MappingD_dictD___delitem__;
        B_IndexedD_MappingD_dictG_methods.__setitem__ = B_IndexedD_MappingD_dictD___setitem__;
        B_IndexedD_MappingD_dictG_methods.__getitem__ = B_IndexedD_MappingD_dictD___getitem__;
        B_IndexedD_MappingD_dictG_methods.__serialize__ = B_IndexedD_MappingD_dictD___serialize__;
        B_IndexedD_MappingD_dictG_methods.__deserialize__ = B_IndexedD_MappingD_dictD___deserialize__;
        $register(&B_IndexedD_MappingD_dictG_methods);
    }
    {
        B_OrdD_dictG_methods.$GCINFO = "B_OrdD_dict";
        B_OrdD_dictG_methods.$superclass = ($SuperG_class)&B_OrdG_methods;
        B_OrdD_dictG_methods.__bool__ = (B_bool (*) (B_OrdD_dict))B_valueG_methods.__bool__;
        B_OrdD_dictG_methods.__str__ = (B_str (*) (B_OrdD_dict))B_valueG_methods.__str__;
        B_OrdD_dictG_methods.__repr__ = (B_str (*) (B_OrdD_dict))B_valueG_methods.__repr__;
        B_OrdD_dictG_methods.__ne__ = (B_bool (*) (B_OrdD_dict, B_dict, B_dict))B_EqG_methods.__ne__;
        B_OrdD_dictG_methods.__le__ = (B_bool (*) (B_OrdD_dict, B_dict, B_dict))B_OrdG_methods.__le__;
        B_OrdD_dictG_methods.__gt__ = (B_bool (*) (B_OrdD_dict, B_dict, B_dict))B_OrdG_methods.__gt__;
        B_OrdD_dictG_methods.__ge__ = (B_bool (*) (B_OrdD_dict, B_dict, B_dict))B_OrdG_methods.__ge__;
        B_OrdD_dictG_methods.__init__ = B_OrdD_dictD___init__;
        B_OrdD_dictG_methods.__lt__ = B_OrdD_dictD___lt__;
        B_OrdD_dictG_methods.__eq__ = B_OrdD_dictD___eq__;
        B_OrdD_dictG_methods.__serialize__ = B_OrdD_dictD___serialize__;
        B_OrdD_dictG_methods.__deserialize__ = B_OrdD_dictD___deserialize__;
        $register(&B_OrdD_dictG_methods);
    }
    {
        B_SetD_setG_methods.$GCINFO = "B_SetD_set";
        B_SetD_setG_methods.$superclass = ($SuperG_class)&B_SetG_methods;
        B_SetD_setG_methods.__bool__ = (B_bool (*) (B_SetD_set))B_valueG_methods.__bool__;
        B_SetD_setG_methods.__str__ = (B_str (*) (B_SetD_set))B_valueG_methods.__str__;
        B_SetD_setG_methods.__repr__ = (B_str (*) (B_SetD_set))B_valueG_methods.__repr__;
        B_SetD_setG_methods.__init__ = B_SetD_setD___init__;
        B_SetD_setG_methods.pop = B_SetD_setD_pop;
        B_SetD_setG_methods.discard = B_SetD_setD_discard;
        B_SetD_setG_methods.add = B_SetD_setD_add;
        B_SetD_setG_methods.isdisjoint = B_SetD_setD_isdisjoint;
        B_SetD_setG_methods.__containsnot__ = B_SetD_setD___containsnot__;
        B_SetD_setG_methods.__contains__ = B_SetD_setD___contains__;
        B_SetD_setG_methods.__len__ = B_SetD_setD___len__;
        B_SetD_setG_methods.__fromiter__ = B_SetD_setD___fromiter__;
        B_SetD_setG_methods.__iter__ = B_SetD_setD___iter__;
        B_SetD_setG_methods.__serialize__ = B_SetD_setD___serialize__;
        B_SetD_setG_methods.__deserialize__ = B_SetD_setD___deserialize__;
        $register(&B_SetD_setG_methods);
    }
    {
        B_OrdD_SetD_setG_methods.$GCINFO = "B_OrdD_SetD_set";
        B_OrdD_SetD_setG_methods.$superclass = ($SuperG_class)&B_OrdD_SetG_methods;
        B_OrdD_SetD_setG_methods.__bool__ = (B_bool (*) (B_OrdD_SetD_set))B_valueG_methods.__bool__;
        B_OrdD_SetD_setG_methods.__str__ = (B_str (*) (B_OrdD_SetD_set))B_valueG_methods.__str__;
        B_OrdD_SetD_setG_methods.__repr__ = (B_str (*) (B_OrdD_SetD_set))B_valueG_methods.__repr__;
        B_OrdD_SetD_setG_methods.__ne__ = (B_bool (*) (B_OrdD_SetD_set, B_set, B_set))B_EqG_methods.__ne__;
        B_OrdD_SetD_setG_methods.__le__ = (B_bool (*) (B_OrdD_SetD_set, B_set, B_set))B_OrdG_methods.__le__;
        B_OrdD_SetD_setG_methods.__gt__ = (B_bool (*) (B_OrdD_SetD_set, B_set, B_set))B_OrdG_methods.__gt__;
        B_OrdD_SetD_setG_methods.__ge__ = (B_bool (*) (B_OrdD_SetD_set, B_set, B_set))B_OrdG_methods.__ge__;
        B_OrdD_SetD_setG_methods.__init__ = B_OrdD_SetD_setD___init__;
        B_OrdD_SetD_setG_methods.__lt__ = B_OrdD_SetD_setD___lt__;
        B_OrdD_SetD_setG_methods.__eq__ = B_OrdD_SetD_setD___eq__;
        B_OrdD_SetD_setG_methods.__serialize__ = B_OrdD_SetD_setD___serialize__;
        B_OrdD_SetD_setG_methods.__deserialize__ = B_OrdD_SetD_setD___deserialize__;
        $register(&B_OrdD_SetD_setG_methods);
    }
    {
        B_LogicalD_SetD_setG_methods.$GCINFO = "B_LogicalD_SetD_set";
        B_LogicalD_SetD_setG_methods.$superclass = ($SuperG_class)&B_LogicalD_SetG_methods;
        B_LogicalD_SetD_setG_methods.__bool__ = (B_bool (*) (B_LogicalD_SetD_set))B_valueG_methods.__bool__;
        B_LogicalD_SetD_setG_methods.__str__ = (B_str (*) (B_LogicalD_SetD_set))B_valueG_methods.__str__;
        B_LogicalD_SetD_setG_methods.__repr__ = (B_str (*) (B_LogicalD_SetD_set))B_valueG_methods.__repr__;
        B_LogicalD_SetD_setG_methods.__iand__ = (B_set (*) (B_LogicalD_SetD_set, B_set, B_set))B_LogicalG_methods.__iand__;
        B_LogicalD_SetD_setG_methods.__ior__ = (B_set (*) (B_LogicalD_SetD_set, B_set, B_set))B_LogicalG_methods.__ior__;
        B_LogicalD_SetD_setG_methods.__ixor__ = (B_set (*) (B_LogicalD_SetD_set, B_set, B_set))B_LogicalG_methods.__ixor__;
        B_LogicalD_SetD_setG_methods.__init__ = B_LogicalD_SetD_setD___init__;
        B_LogicalD_SetD_setG_methods.__xor__ = B_LogicalD_SetD_setD___xor__;
        B_LogicalD_SetD_setG_methods.__or__ = B_LogicalD_SetD_setD___or__;
        B_LogicalD_SetD_setG_methods.__and__ = B_LogicalD_SetD_setD___and__;
        B_LogicalD_SetD_setG_methods.__serialize__ = B_LogicalD_SetD_setD___serialize__;
        B_LogicalD_SetD_setG_methods.__deserialize__ = B_LogicalD_SetD_setD___deserialize__;
        $register(&B_LogicalD_SetD_setG_methods);
    }
    {
        B_MinusD_SetD_setG_methods.$GCINFO = "B_MinusD_SetD_set";
        B_MinusD_SetD_setG_methods.$superclass = ($SuperG_class)&B_MinusD_SetG_methods;
        B_MinusD_SetD_setG_methods.__bool__ = (B_bool (*) (B_MinusD_SetD_set))B_valueG_methods.__bool__;
        B_MinusD_SetD_setG_methods.__str__ = (B_str (*) (B_MinusD_SetD_set))B_valueG_methods.__str__;
        B_MinusD_SetD_setG_methods.__repr__ = (B_str (*) (B_MinusD_SetD_set))B_valueG_methods.__repr__;
        B_MinusD_SetD_setG_methods.__isub__ = (B_set (*) (B_MinusD_SetD_set, B_set, B_set))B_MinusG_methods.__isub__;
        B_MinusD_SetD_setG_methods.__init__ = B_MinusD_SetD_setD___init__;
        B_MinusD_SetD_setG_methods.__sub__ = B_MinusD_SetD_setD___sub__;
        B_MinusD_SetD_setG_methods.__serialize__ = B_MinusD_SetD_setD___serialize__;
        B_MinusD_SetD_setG_methods.__deserialize__ = B_MinusD_SetD_setD___deserialize__;
        $register(&B_MinusD_SetD_setG_methods);
    }
    {
        B_IterableD_IteratorG_methods.$GCINFO = "B_IterableD_Iterator";
        B_IterableD_IteratorG_methods.$superclass = ($SuperG_class)&B_IterableG_methods;
        B_IterableD_IteratorG_methods.__bool__ = (B_bool (*) (B_IterableD_Iterator))B_valueG_methods.__bool__;
        B_IterableD_IteratorG_methods.__str__ = (B_str (*) (B_IterableD_Iterator))B_valueG_methods.__str__;
        B_IterableD_IteratorG_methods.__repr__ = (B_str (*) (B_IterableD_Iterator))B_valueG_methods.__repr__;
        B_IterableD_IteratorG_methods.__init__ = B_IterableD_IteratorD___init__;
        B_IterableD_IteratorG_methods.__iter__ = B_IterableD_IteratorD___iter__;
        B_IterableD_IteratorG_methods.__serialize__ = B_IterableD_IteratorD___serialize__;
        B_IterableD_IteratorG_methods.__deserialize__ = B_IterableD_IteratorD___deserialize__;
        $register(&B_IterableD_IteratorG_methods);
    }
    {
        B_IterableD_rangeG_methods.$GCINFO = "B_IterableD_range";
        B_IterableD_rangeG_methods.$superclass = ($SuperG_class)&B_IterableG_methods;
        B_IterableD_rangeG_methods.__bool__ = (B_bool (*) (B_IterableD_range))B_valueG_methods.__bool__;
        B_IterableD_rangeG_methods.__str__ = (B_str (*) (B_IterableD_range))B_valueG_methods.__str__;
        B_IterableD_rangeG_methods.__repr__ = (B_str (*) (B_IterableD_range))B_valueG_methods.__repr__;
        B_IterableD_rangeG_methods.__init__ = B_IterableD_rangeD___init__;
        B_IterableD_rangeG_methods.__iter__ = B_IterableD_rangeD___iter__;
        B_IterableD_rangeG_methods.__serialize__ = B_IterableD_rangeD___serialize__;
        B_IterableD_rangeG_methods.__deserialize__ = B_IterableD_rangeD___deserialize__;
        $register(&B_IterableD_rangeG_methods);
    }
    {
        B_OrdD_strG_methods.$GCINFO = "B_OrdD_str";
        B_OrdD_strG_methods.$superclass = ($SuperG_class)&B_OrdG_methods;
        B_OrdD_strG_methods.__bool__ = (B_bool (*) (B_OrdD_str))B_valueG_methods.__bool__;
        B_OrdD_strG_methods.__str__ = (B_str (*) (B_OrdD_str))B_valueG_methods.__str__;
        B_OrdD_strG_methods.__repr__ = (B_str (*) (B_OrdD_str))B_valueG_methods.__repr__;
        B_OrdD_strG_methods.__ne__ = (B_bool (*) (B_OrdD_str, B_str, B_str))B_EqG_methods.__ne__;
        B_OrdD_strG_methods.__le__ = (B_bool (*) (B_OrdD_str, B_str, B_str))B_OrdG_methods.__le__;
        B_OrdD_strG_methods.__gt__ = (B_bool (*) (B_OrdD_str, B_str, B_str))B_OrdG_methods.__gt__;
        B_OrdD_strG_methods.__ge__ = (B_bool (*) (B_OrdD_str, B_str, B_str))B_OrdG_methods.__ge__;
        B_OrdD_strG_methods.__init__ = B_OrdD_strD___init__;
        B_OrdD_strG_methods.__lt__ = B_OrdD_strD___lt__;
        B_OrdD_strG_methods.__eq__ = B_OrdD_strD___eq__;
        B_OrdD_strG_methods.__serialize__ = B_OrdD_strD___serialize__;
        B_OrdD_strG_methods.__deserialize__ = B_OrdD_strD___deserialize__;
        $register(&B_OrdD_strG_methods);
    }
    {
        B_ContainerD_strG_methods.$GCINFO = "B_ContainerD_str";
        B_ContainerD_strG_methods.$superclass = ($SuperG_class)&B_ContainerG_methods;
        B_ContainerD_strG_methods.__bool__ = (B_bool (*) (B_ContainerD_str))B_valueG_methods.__bool__;
        B_ContainerD_strG_methods.__str__ = (B_str (*) (B_ContainerD_str))B_valueG_methods.__str__;
        B_ContainerD_strG_methods.__repr__ = (B_str (*) (B_ContainerD_str))B_valueG_methods.__repr__;
        B_ContainerD_strG_methods.__init__ = B_ContainerD_strD___init__;
        B_ContainerD_strG_methods.__containsnot__ = B_ContainerD_strD___containsnot__;
        B_ContainerD_strG_methods.__contains__ = B_ContainerD_strD___contains__;
        B_ContainerD_strG_methods.__len__ = B_ContainerD_strD___len__;
        B_ContainerD_strG_methods.__fromiter__ = B_ContainerD_strD___fromiter__;
        B_ContainerD_strG_methods.__iter__ = B_ContainerD_strD___iter__;
        B_ContainerD_strG_methods.__serialize__ = B_ContainerD_strD___serialize__;
        B_ContainerD_strG_methods.__deserialize__ = B_ContainerD_strD___deserialize__;
        $register(&B_ContainerD_strG_methods);
    }
    {
        B_SliceableD_strG_methods.$GCINFO = "B_SliceableD_str";
        B_SliceableD_strG_methods.$superclass = ($SuperG_class)&B_SliceableG_methods;
        B_SliceableD_strG_methods.__bool__ = (B_bool (*) (B_SliceableD_str))B_valueG_methods.__bool__;
        B_SliceableD_strG_methods.__str__ = (B_str (*) (B_SliceableD_str))B_valueG_methods.__str__;
        B_SliceableD_strG_methods.__repr__ = (B_str (*) (B_SliceableD_str))B_valueG_methods.__repr__;
        B_SliceableD_strG_methods.__init__ = B_SliceableD_strD___init__;
        B_SliceableD_strG_methods.__delslice__ = B_SliceableD_strD___delslice__;
        B_SliceableD_strG_methods.__setslice__ = B_SliceableD_strD___setslice__;
        B_SliceableD_strG_methods.__getslice__ = B_SliceableD_strD___getslice__;
        B_SliceableD_strG_methods.__delitem__ = B_SliceableD_strD___delitem__;
        B_SliceableD_strG_methods.__setitem__ = B_SliceableD_strD___setitem__;
        B_SliceableD_strG_methods.__getitem__ = B_SliceableD_strD___getitem__;
        B_SliceableD_strG_methods.__serialize__ = B_SliceableD_strD___serialize__;
        B_SliceableD_strG_methods.__deserialize__ = B_SliceableD_strD___deserialize__;
        $register(&B_SliceableD_strG_methods);
    }
    {
        B_TimesD_strG_methods.$GCINFO = "B_TimesD_str";
        B_TimesD_strG_methods.$superclass = ($SuperG_class)&B_TimesG_methods;
        B_TimesD_strG_methods.__bool__ = (B_bool (*) (B_TimesD_str))B_valueG_methods.__bool__;
        B_TimesD_strG_methods.__str__ = (B_str (*) (B_TimesD_str))B_valueG_methods.__str__;
        B_TimesD_strG_methods.__repr__ = (B_str (*) (B_TimesD_str))B_valueG_methods.__repr__;
        B_TimesD_strG_methods.__iadd__ = (B_str (*) (B_TimesD_str, B_str, B_str))B_PlusG_methods.__iadd__;
        B_TimesD_strG_methods.__imul__ = (B_str (*) (B_TimesD_str, B_str, B_int))B_TimesG_methods.__imul__;
        B_TimesD_strG_methods.__init__ = B_TimesD_strD___init__;
        B_TimesD_strG_methods.__mul__ = B_TimesD_strD___mul__;
        B_TimesD_strG_methods.__add__ = B_TimesD_strD___add__;
        B_TimesD_strG_methods.__serialize__ = B_TimesD_strD___serialize__;
        B_TimesD_strG_methods.__deserialize__ = B_TimesD_strD___deserialize__;
        $register(&B_TimesD_strG_methods);
    }
    {
        B_HashableD_strG_methods.$GCINFO = "B_HashableD_str";
        B_HashableD_strG_methods.$superclass = ($SuperG_class)&B_HashableG_methods;
        B_HashableD_strG_methods.__bool__ = (B_bool (*) (B_HashableD_str))B_valueG_methods.__bool__;
        B_HashableD_strG_methods.__str__ = (B_str (*) (B_HashableD_str))B_valueG_methods.__str__;
        B_HashableD_strG_methods.__repr__ = (B_str (*) (B_HashableD_str))B_valueG_methods.__repr__;
        B_HashableD_strG_methods.__eq__ =  B_HashableD_strD___eq__;                                  // This line was not generated
        B_HashableD_strG_methods.__ne__ = (B_bool (*) (B_HashableD_str, B_str, B_str))B_EqG_methods.__ne__;
        B_HashableD_strG_methods.__init__ = B_HashableD_strD___init__;
        B_HashableD_strG_methods.__hash__ = B_HashableD_strD___hash__;
        B_HashableD_strG_methods.__serialize__ = B_HashableD_strD___serialize__;
        B_HashableD_strG_methods.__deserialize__ = B_HashableD_strD___deserialize__;
        $register(&B_HashableD_strG_methods);
    }
    {
        B_OrdD_bytearrayG_methods.$GCINFO = "B_OrdD_bytearray";
        B_OrdD_bytearrayG_methods.$superclass = ($SuperG_class)&B_OrdG_methods;
        B_OrdD_bytearrayG_methods.__bool__ = (B_bool (*) (B_OrdD_bytearray))B_valueG_methods.__bool__;
        B_OrdD_bytearrayG_methods.__str__ = (B_str (*) (B_OrdD_bytearray))B_valueG_methods.__str__;
        B_OrdD_bytearrayG_methods.__repr__ = (B_str (*) (B_OrdD_bytearray))B_valueG_methods.__repr__;
        B_OrdD_bytearrayG_methods.__ne__ = (B_bool (*) (B_OrdD_bytearray, B_bytearray, B_bytearray))B_EqG_methods.__ne__;
        B_OrdD_bytearrayG_methods.__le__ = (B_bool (*) (B_OrdD_bytearray, B_bytearray, B_bytearray))B_OrdG_methods.__le__;
        B_OrdD_bytearrayG_methods.__gt__ = (B_bool (*) (B_OrdD_bytearray, B_bytearray, B_bytearray))B_OrdG_methods.__gt__;
        B_OrdD_bytearrayG_methods.__ge__ = (B_bool (*) (B_OrdD_bytearray, B_bytearray, B_bytearray))B_OrdG_methods.__ge__;
        B_OrdD_bytearrayG_methods.__init__ = B_OrdD_bytearrayD___init__;
        B_OrdD_bytearrayG_methods.__lt__ = B_OrdD_bytearrayD___lt__;
        B_OrdD_bytearrayG_methods.__eq__ = B_OrdD_bytearrayD___eq__;
        B_OrdD_bytearrayG_methods.__serialize__ = B_OrdD_bytearrayD___serialize__;
        B_OrdD_bytearrayG_methods.__deserialize__ = B_OrdD_bytearrayD___deserialize__;
        $register(&B_OrdD_bytearrayG_methods);
    }
    {
        B_SequenceD_bytearrayG_methods.$GCINFO = "B_SequenceD_bytearray";
        B_SequenceD_bytearrayG_methods.$superclass = ($SuperG_class)&B_SequenceG_methods;
        B_SequenceD_bytearrayG_methods.__bool__ = (B_bool (*) (B_SequenceD_bytearray))B_valueG_methods.__bool__;
        B_SequenceD_bytearrayG_methods.__str__ = (B_str (*) (B_SequenceD_bytearray))B_valueG_methods.__str__;
        B_SequenceD_bytearrayG_methods.__repr__ = (B_str (*) (B_SequenceD_bytearray))B_valueG_methods.__repr__;
        B_SequenceD_bytearrayG_methods.__init__ = B_SequenceD_bytearrayD___init__;
        B_SequenceD_bytearrayG_methods.reverse = B_SequenceD_bytearrayD_reverse;
        B_SequenceD_bytearrayG_methods.append = B_SequenceD_bytearrayD_append;
        B_SequenceD_bytearrayG_methods.insert = B_SequenceD_bytearrayD_insert;
        B_SequenceD_bytearrayG_methods.__reversed__ = B_SequenceD_bytearrayD___reversed__;
        B_SequenceD_bytearrayG_methods.__delslice__ = B_SequenceD_bytearrayD___delslice__;
        B_SequenceD_bytearrayG_methods.__setslice__ = B_SequenceD_bytearrayD___setslice__;
        B_SequenceD_bytearrayG_methods.__getslice__ = B_SequenceD_bytearrayD___getslice__;
        B_SequenceD_bytearrayG_methods.__delitem__ = B_SequenceD_bytearrayD___delitem__;
        B_SequenceD_bytearrayG_methods.__setitem__ = B_SequenceD_bytearrayD___setitem__;
        B_SequenceD_bytearrayG_methods.__getitem__ = B_SequenceD_bytearrayD___getitem__;
        B_SequenceD_bytearrayG_methods.__serialize__ = B_SequenceD_bytearrayD___serialize__;
        B_SequenceD_bytearrayG_methods.__deserialize__ = B_SequenceD_bytearrayD___deserialize__;
        $register(&B_SequenceD_bytearrayG_methods);
    }
    {
        B_CollectionD_SequenceD_bytearrayG_methods.$GCINFO = "B_CollectionD_SequenceD_bytearray";
        B_CollectionD_SequenceD_bytearrayG_methods.$superclass = ($SuperG_class)&B_CollectionD_SequenceG_methods;
        B_CollectionD_SequenceD_bytearrayG_methods.__bool__ = (B_bool (*) (B_CollectionD_SequenceD_bytearray))B_valueG_methods.__bool__;
        B_CollectionD_SequenceD_bytearrayG_methods.__str__ = (B_str (*) (B_CollectionD_SequenceD_bytearray))B_valueG_methods.__str__;
        B_CollectionD_SequenceD_bytearrayG_methods.__repr__ = (B_str (*) (B_CollectionD_SequenceD_bytearray))B_valueG_methods.__repr__;
        B_CollectionD_SequenceD_bytearrayG_methods.__init__ = B_CollectionD_SequenceD_bytearrayD___init__;
        B_CollectionD_SequenceD_bytearrayG_methods.__len__ = B_CollectionD_SequenceD_bytearrayD___len__;
        B_CollectionD_SequenceD_bytearrayG_methods.__fromiter__ = B_CollectionD_SequenceD_bytearrayD___fromiter__;
        B_CollectionD_SequenceD_bytearrayG_methods.__iter__ = B_CollectionD_SequenceD_bytearrayD___iter__;
        B_CollectionD_SequenceD_bytearrayG_methods.__serialize__ = B_CollectionD_SequenceD_bytearrayD___serialize__;
        B_CollectionD_SequenceD_bytearrayG_methods.__deserialize__ = B_CollectionD_SequenceD_bytearrayD___deserialize__;
        $register(&B_CollectionD_SequenceD_bytearrayG_methods);
    }
    {
        B_TimesD_SequenceD_bytearrayG_methods.$GCINFO = "B_TimesD_SequenceD_bytearray";
        B_TimesD_SequenceD_bytearrayG_methods.$superclass = ($SuperG_class)&B_TimesD_SequenceG_methods;
        B_TimesD_SequenceD_bytearrayG_methods.__bool__ = (B_bool (*) (B_TimesD_SequenceD_bytearray))B_valueG_methods.__bool__;
        B_TimesD_SequenceD_bytearrayG_methods.__str__ = (B_str (*) (B_TimesD_SequenceD_bytearray))B_valueG_methods.__str__;
        B_TimesD_SequenceD_bytearrayG_methods.__repr__ = (B_str (*) (B_TimesD_SequenceD_bytearray))B_valueG_methods.__repr__;
        B_TimesD_SequenceD_bytearrayG_methods.__iadd__ = (B_bytearray (*) (B_TimesD_SequenceD_bytearray, B_bytearray, B_bytearray))B_PlusG_methods.__iadd__;
        B_TimesD_SequenceD_bytearrayG_methods.__imul__ = (B_bytearray (*) (B_TimesD_SequenceD_bytearray, B_bytearray, B_int))B_TimesG_methods.__imul__;
        B_TimesD_SequenceD_bytearrayG_methods.__init__ = B_TimesD_SequenceD_bytearrayD___init__;
        B_TimesD_SequenceD_bytearrayG_methods.__mul__ = B_TimesD_SequenceD_bytearrayD___mul__;
        B_TimesD_SequenceD_bytearrayG_methods.__add__ = B_TimesD_SequenceD_bytearrayD___add__;
        B_TimesD_SequenceD_bytearrayG_methods.__serialize__ = B_TimesD_SequenceD_bytearrayD___serialize__;
        B_TimesD_SequenceD_bytearrayG_methods.__deserialize__ = B_TimesD_SequenceD_bytearrayD___deserialize__;
        $register(&B_TimesD_SequenceD_bytearrayG_methods);
    }
    {
        B_ContainerD_bytearrayG_methods.$GCINFO = "B_ContainerD_bytearray";
        B_ContainerD_bytearrayG_methods.$superclass = ($SuperG_class)&B_ContainerG_methods;
        B_ContainerD_bytearrayG_methods.__bool__ = (B_bool (*) (B_ContainerD_bytearray))B_valueG_methods.__bool__;
        B_ContainerD_bytearrayG_methods.__str__ = (B_str (*) (B_ContainerD_bytearray))B_valueG_methods.__str__;
        B_ContainerD_bytearrayG_methods.__repr__ = (B_str (*) (B_ContainerD_bytearray))B_valueG_methods.__repr__;
        B_ContainerD_bytearrayG_methods.__init__ = B_ContainerD_bytearrayD___init__;
        B_ContainerD_bytearrayG_methods.__containsnot__ = B_ContainerD_bytearrayD___containsnot__;
        B_ContainerD_bytearrayG_methods.__contains__ = B_ContainerD_bytearrayD___contains__;
        B_ContainerD_bytearrayG_methods.__serialize__ = B_ContainerD_bytearrayD___serialize__;
        B_ContainerD_bytearrayG_methods.__deserialize__ = B_ContainerD_bytearrayD___deserialize__;
        $register(&B_ContainerD_bytearrayG_methods);
    }
    {
        B_OrdD_bytesG_methods.$GCINFO = "B_OrdD_bytes";
        B_OrdD_bytesG_methods.$superclass = ($SuperG_class)&B_OrdG_methods;
        B_OrdD_bytesG_methods.__bool__ = (B_bool (*) (B_OrdD_bytes))B_valueG_methods.__bool__;
        B_OrdD_bytesG_methods.__str__ = (B_str (*) (B_OrdD_bytes))B_valueG_methods.__str__;
        B_OrdD_bytesG_methods.__repr__ = (B_str (*) (B_OrdD_bytes))B_valueG_methods.__repr__;
        B_OrdD_bytesG_methods.__ne__ = (B_bool (*) (B_OrdD_bytes, B_bytes, B_bytes))B_EqG_methods.__ne__;
        B_OrdD_bytesG_methods.__le__ = (B_bool (*) (B_OrdD_bytes, B_bytes, B_bytes))B_OrdG_methods.__le__;
        B_OrdD_bytesG_methods.__gt__ = (B_bool (*) (B_OrdD_bytes, B_bytes, B_bytes))B_OrdG_methods.__gt__;
        B_OrdD_bytesG_methods.__ge__ = (B_bool (*) (B_OrdD_bytes, B_bytes, B_bytes))B_OrdG_methods.__ge__;
        B_OrdD_bytesG_methods.__init__ = B_OrdD_bytesD___init__;
        B_OrdD_bytesG_methods.__lt__ = B_OrdD_bytesD___lt__;
        B_OrdD_bytesG_methods.__eq__ = B_OrdD_bytesD___eq__;
        B_OrdD_bytesG_methods.__serialize__ = B_OrdD_bytesD___serialize__;
        B_OrdD_bytesG_methods.__deserialize__ = B_OrdD_bytesD___deserialize__;
        $register(&B_OrdD_bytesG_methods);
    }
    {
        B_SliceableD_bytesG_methods.$GCINFO = "B_SliceableD_bytes";
        B_SliceableD_bytesG_methods.$superclass = ($SuperG_class)&B_SliceableG_methods;
        B_SliceableD_bytesG_methods.__bool__ = (B_bool (*) (B_SliceableD_bytes))B_valueG_methods.__bool__;
        B_SliceableD_bytesG_methods.__str__ = (B_str (*) (B_SliceableD_bytes))B_valueG_methods.__str__;
        B_SliceableD_bytesG_methods.__repr__ = (B_str (*) (B_SliceableD_bytes))B_valueG_methods.__repr__;
        B_SliceableD_bytesG_methods.__init__ = B_SliceableD_bytesD___init__;
        B_SliceableD_bytesG_methods.__delslice__ = B_SliceableD_bytesD___delslice__;
        B_SliceableD_bytesG_methods.__setslice__ = B_SliceableD_bytesD___setslice__;
        B_SliceableD_bytesG_methods.__getslice__ = B_SliceableD_bytesD___getslice__;
        B_SliceableD_bytesG_methods.__delitem__ = B_SliceableD_bytesD___delitem__;
        B_SliceableD_bytesG_methods.__setitem__ = B_SliceableD_bytesD___setitem__;
        B_SliceableD_bytesG_methods.__getitem__ = B_SliceableD_bytesD___getitem__;
        B_SliceableD_bytesG_methods.__serialize__ = B_SliceableD_bytesD___serialize__;
        B_SliceableD_bytesG_methods.__deserialize__ = B_SliceableD_bytesD___deserialize__;
        $register(&B_SliceableD_bytesG_methods);
    }
    {
        B_ContainerD_bytesG_methods.$GCINFO = "B_ContainerD_bytes";
        B_ContainerD_bytesG_methods.$superclass = ($SuperG_class)&B_ContainerG_methods;
        B_ContainerD_bytesG_methods.__bool__ = (B_bool (*) (B_ContainerD_bytes))B_valueG_methods.__bool__;
        B_ContainerD_bytesG_methods.__str__ = (B_str (*) (B_ContainerD_bytes))B_valueG_methods.__str__;
        B_ContainerD_bytesG_methods.__repr__ = (B_str (*) (B_ContainerD_bytes))B_valueG_methods.__repr__;
        B_ContainerD_bytesG_methods.__init__ = B_ContainerD_bytesD___init__;
        B_ContainerD_bytesG_methods.__containsnot__ = B_ContainerD_bytesD___containsnot__;
        B_ContainerD_bytesG_methods.__contains__ = B_ContainerD_bytesD___contains__;
        B_ContainerD_bytesG_methods.__len__ = B_ContainerD_bytesD___len__;
        B_ContainerD_bytesG_methods.__fromiter__ = B_ContainerD_bytesD___fromiter__;
        B_ContainerD_bytesG_methods.__iter__ = B_ContainerD_bytesD___iter__;
        B_ContainerD_bytesG_methods.__serialize__ = B_ContainerD_bytesD___serialize__;
        B_ContainerD_bytesG_methods.__deserialize__ = B_ContainerD_bytesD___deserialize__;
        $register(&B_ContainerD_bytesG_methods);
    }
    {
        B_TimesD_bytesG_methods.$GCINFO = "B_TimesD_bytes";
        B_TimesD_bytesG_methods.$superclass = ($SuperG_class)&B_TimesG_methods;
        B_TimesD_bytesG_methods.__bool__ = (B_bool (*) (B_TimesD_bytes))B_valueG_methods.__bool__;
        B_TimesD_bytesG_methods.__str__ = (B_str (*) (B_TimesD_bytes))B_valueG_methods.__str__;
        B_TimesD_bytesG_methods.__repr__ = (B_str (*) (B_TimesD_bytes))B_valueG_methods.__repr__;
        B_TimesD_bytesG_methods.__iadd__ = (B_bytes (*) (B_TimesD_bytes, B_bytes, B_bytes))B_PlusG_methods.__iadd__;
        B_TimesD_bytesG_methods.__imul__ = (B_bytes (*) (B_TimesD_bytes, B_bytes, B_int))B_TimesG_methods.__imul__;
        B_TimesD_bytesG_methods.__init__ = B_TimesD_bytesD___init__;
        B_TimesD_bytesG_methods.__mul__ = B_TimesD_bytesD___mul__;
        B_TimesD_bytesG_methods.__add__ = B_TimesD_bytesD___add__;
        B_TimesD_bytesG_methods.__serialize__ = B_TimesD_bytesD___serialize__;
        B_TimesD_bytesG_methods.__deserialize__ = B_TimesD_bytesD___deserialize__;
        $register(&B_TimesD_bytesG_methods);
    }
    {
        B_HashableD_bytesG_methods.$GCINFO = "B_HashableD_bytes";
        B_HashableD_bytesG_methods.$superclass = ($SuperG_class)&B_HashableG_methods;
        B_HashableD_bytesG_methods.__bool__ = (B_bool (*) (B_HashableD_bytes))B_valueG_methods.__bool__;
        B_HashableD_bytesG_methods.__str__ = (B_str (*) (B_HashableD_bytes))B_valueG_methods.__str__;
        B_HashableD_bytesG_methods.__repr__ = (B_str (*) (B_HashableD_bytes))B_valueG_methods.__repr__;
        B_HashableD_bytesG_methods.__ne__ = (B_bool (*) (B_HashableD_bytes, B_bytes, B_bytes))B_EqG_methods.__ne__;
        B_HashableD_bytesG_methods.__init__ = B_HashableD_bytesD___init__;
        B_HashableD_bytesG_methods.__hash__ = B_HashableD_bytesD___hash__;
        B_HashableD_bytesG_methods.__serialize__ = B_HashableD_bytesD___serialize__;
        B_HashableD_bytesG_methods.__deserialize__ = B_HashableD_bytesD___deserialize__;
        $register(&B_HashableD_bytesG_methods);
    }
    /*
    {
        B_L_1procG_methods.$GCINFO = "B_L_1proc";
        B_L_1procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        B_L_1procG_methods.__bool__ = (B_bool (*) (B_L_1proc))B_valueG_methods.__bool__;
        B_L_1procG_methods.__str__ = (B_str (*) (B_L_1proc))B_valueG_methods.__str__;
        B_L_1procG_methods.__repr__ = (B_str (*) (B_L_1proc))B_valueG_methods.__repr__;
        B_L_1procG_methods.__init__ = B_L_1procD___init__;
        B_L_1procG_methods.__call__ = B_L_1procD___call__;
        B_L_1procG_methods.__exec__ = B_L_1procD___exec__;
        B_L_1procG_methods.__serialize__ = B_L_1procD___serialize__;
        B_L_1procG_methods.__deserialize__ = B_L_1procD___deserialize__;
        $register(&B_L_1procG_methods);
    }
    {
        B_L_2procG_methods.$GCINFO = "B_L_2proc";
        B_L_2procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        B_L_2procG_methods.__bool__ = (B_bool (*) (B_L_2proc))B_valueG_methods.__bool__;
        B_L_2procG_methods.__str__ = (B_str (*) (B_L_2proc))B_valueG_methods.__str__;
        B_L_2procG_methods.__repr__ = (B_str (*) (B_L_2proc))B_valueG_methods.__repr__;
        B_L_2procG_methods.__init__ = B_L_2procD___init__;
        B_L_2procG_methods.__call__ = B_L_2procD___call__;
        B_L_2procG_methods.__exec__ = B_L_2procD___exec__;
        B_L_2procG_methods.__serialize__ = B_L_2procD___serialize__;
        B_L_2procG_methods.__deserialize__ = B_L_2procD___deserialize__;
        $register(&B_L_2procG_methods);
    }
    {
        B_L_3procG_methods.$GCINFO = "B_L_3proc";
        B_L_3procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        B_L_3procG_methods.__bool__ = (B_bool (*) (B_L_3proc))B_valueG_methods.__bool__;
        B_L_3procG_methods.__str__ = (B_str (*) (B_L_3proc))B_valueG_methods.__str__;
        B_L_3procG_methods.__repr__ = (B_str (*) (B_L_3proc))B_valueG_methods.__repr__;
        B_L_3procG_methods.__init__ = B_L_3procD___init__;
        B_L_3procG_methods.__call__ = B_L_3procD___call__;
        B_L_3procG_methods.__exec__ = B_L_3procD___exec__;
        B_L_3procG_methods.__serialize__ = B_L_3procD___serialize__;
        B_L_3procG_methods.__deserialize__ = B_L_3procD___deserialize__;
        $register(&B_L_3procG_methods);
    }
    {
        B_L_5ContG_methods.$GCINFO = "B_L_5Cont";
        B_L_5ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        B_L_5ContG_methods.__bool__ = (B_bool (*) (B_L_5Cont))B_valueG_methods.__bool__;
        B_L_5ContG_methods.__str__ = (B_str (*) (B_L_5Cont))B_valueG_methods.__str__;
        B_L_5ContG_methods.__repr__ = (B_str (*) (B_L_5Cont))B_valueG_methods.__repr__;
        B_L_5ContG_methods.__init__ = B_L_5ContD___init__;
        B_L_5ContG_methods.__call__ = B_L_5ContD___call__;
        B_L_5ContG_methods.__serialize__ = B_L_5ContD___serialize__;
        B_L_5ContG_methods.__deserialize__ = B_L_5ContD___deserialize__;
        $register(&B_L_5ContG_methods);
    }
    {
        B_L_6procG_methods.$GCINFO = "B_L_6proc";
        B_L_6procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        B_L_6procG_methods.__bool__ = (B_bool (*) (B_L_6proc))B_valueG_methods.__bool__;
        B_L_6procG_methods.__str__ = (B_str (*) (B_L_6proc))B_valueG_methods.__str__;
        B_L_6procG_methods.__repr__ = (B_str (*) (B_L_6proc))B_valueG_methods.__repr__;
        B_L_6procG_methods.__init__ = B_L_6procD___init__;
        B_L_6procG_methods.__call__ = B_L_6procD___call__;
        B_L_6procG_methods.__exec__ = B_L_6procD___exec__;
        B_L_6procG_methods.__serialize__ = B_L_6procD___serialize__;
        B_L_6procG_methods.__deserialize__ = B_L_6procD___deserialize__;
        $register(&B_L_6procG_methods);
    }
    {
        B_WorldAuthG_methods.$GCINFO = "B_WorldAuth";
        B_WorldAuthG_methods.$superclass = ($SuperG_class)&B_valueG_methods;
        B_WorldAuthG_methods.__bool__ = (B_bool (*) (B_WorldAuth))B_valueG_methods.__bool__;
        B_WorldAuthG_methods.__str__ = (B_str (*) (B_WorldAuth))B_valueG_methods.__str__;
        B_WorldAuthG_methods.__repr__ = (B_str (*) (B_WorldAuth))B_valueG_methods.__repr__;
        ;
        B_WorldAuthG_methods.__serialize__ = B_WorldAuthD___serialize__;
        B_WorldAuthG_methods.__deserialize__ = B_WorldAuthD___deserialize__;
        $register(&B_WorldAuthG_methods);
    }
    {
        B_EnvG_methods.$GCINFO = "B_Env";
        B_EnvG_methods.$superclass = ($SuperG_class)&$ActorG_methods;
        B_EnvG_methods.__bool__ = (B_bool (*) (B_Env))$ActorG_methods.__bool__;
        B_EnvG_methods.__str__ = (B_str (*) (B_Env))$ActorG_methods.__str__;
        B_EnvG_methods.__repr__ = (B_str (*) (B_Env))$ActorG_methods.__repr__;
        B_EnvG_methods.__resume__ = (B_NoneType (*) (B_Env))$ActorG_methods.__resume__;
        B_EnvG_methods.__init__ = B_EnvD___init__;
        B_EnvG_methods.stdout_writeG_local = B_EnvD_stdout_writeG_local;
        B_EnvG_methods.stdin_installG_local = B_EnvD_stdin_installG_local;
        B_EnvG_methods.exitG_local = B_EnvD_exitG_local;
        B_EnvG_methods.stdout_write = B_EnvD_stdout_write;
        B_EnvG_methods.stdin_install = B_EnvD_stdin_install;
        B_EnvG_methods.exit = B_EnvD_exit;
        B_EnvG_methods.__serialize__ = B_EnvD___serialize__;
        B_EnvG_methods.__deserialize__ = B_EnvD___deserialize__;
        $register(&B_EnvG_methods);
    }
    */
}
