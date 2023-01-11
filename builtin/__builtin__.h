#pragma once

#include "common.h"

struct B_atom;
typedef struct B_atom *B_atom;

struct B_list;
typedef struct B_list *B_list;

struct B_dict;
typedef struct B_dict *B_dict;

struct B_set;
typedef struct B_set *B_set;

struct B_str;
typedef struct B_str *B_str;

struct B_bytearray;
typedef struct B_bytearray *B_bytearray;

struct B_bytes;
typedef struct B_bytes *B_bytes;

struct $NoneType;
typedef struct $NoneType *$NoneType;

struct B_int;
typedef struct B_int *B_int;

struct B_i64;
typedef struct B_i64 *B_i64;

struct B_float;
typedef struct B_float *B_float;

struct B_complex;
typedef struct B_complex *B_complex;

struct B_bool;
typedef struct B_bool *B_bool;

struct $Serial$state;
typedef struct $Serial$state *$Serial$state;

#include "class_hierarchy.h"

struct B_range;
typedef struct B_range *B_range;

struct B_tuple;
typedef struct B_tuple *B_tuple;

struct B_Iterator;
typedef struct B_Iterator *B_Iterator;

struct B_slice;
typedef struct B_slice *B_slice;

struct B_BaseException;
typedef struct B_BaseException *B_BaseException;

struct B_BaseExceptionG_class;
typedef struct B_BaseExceptionG_class *B_BaseExceptionG_class;

struct B_SystemExit;
typedef struct B_SystemExit *B_SystemExit;

struct B_SystemExitG_class;
typedef struct B_SystemExitG_class *B_SystemExitG_class;

struct B_KeyboardInterrupt;
typedef struct B_KeyboardInterrupt *B_KeyboardInterrupt;

struct B_KeyboardInterruptG_class;
typedef struct B_KeyboardInterruptG_class *B_KeyboardInterruptG_class;

struct B_Exception;
typedef struct B_Exception *B_Exception;

struct B_ExceptionG_class;
typedef struct B_ExceptionG_class *B_ExceptionG_class;

struct B_AssertionError;
typedef struct B_AssertionError *B_AssertionError;

struct B_AssertionErrorG_class;
typedef struct B_AssertionErrorG_class *B_AssertionErrorG_class;

struct B_LookupError;
typedef struct B_LookupError *B_LookupError;

struct B_LookupErrorG_class;
typedef struct B_LookupErrorG_class *B_LookupErrorG_class;

struct B_IndexError;
typedef struct B_IndexError *B_IndexError;

struct B_IndexErrorG_class;
typedef struct B_IndexErrorG_class *B_IndexErrorG_class;

struct B_KeyError;
typedef struct B_KeyError *B_KeyError;

struct B_KeyErrorG_class;
typedef struct B_KeyErrorG_class *B_KeyErrorG_class;

struct B_MemoryError;
typedef struct B_MemoryError *B_MemoryError;

struct B_MemoryErrorG_class;
typedef struct B_MemoryErrorG_class *B_MemoryErrorG_class;

struct B_OSError;
typedef struct B_OSError *B_OSError;

struct B_OSErrorG_class;
typedef struct B_OSErrorG_class *B_OSErrorG_class;

struct B_RuntimeError;
typedef struct B_RuntimeError *B_RuntimeError;

struct B_RuntimeErrorG_class;
typedef struct B_RuntimeErrorG_class *B_RuntimeErrorG_class;

struct B_NotImplementedError;
typedef struct B_NotImplementedError *B_NotImplementedError;

struct B_NotImplementedErrorG_class;
typedef struct B_NotImplementedErrorG_class *B_NotImplementedErrorG_class;

struct B_ValueError;
typedef struct B_ValueError *B_ValueError;

struct B_ValueErrorG_class;
typedef struct B_ValueErrorG_class *B_ValueErrorG_class;

struct B_Eq;
typedef struct B_Eq *B_Eq;

struct B_EqG_class;
typedef struct B_EqG_class *B_EqG_class;

struct B_Ord;
typedef struct B_Ord *B_Ord;

struct B_OrdG_class;
typedef struct B_OrdG_class *B_OrdG_class;

struct B_Logical;
typedef struct B_Logical *B_Logical;

struct B_LogicalG_class;
typedef struct B_LogicalG_class *B_LogicalG_class;

struct $Plus;
typedef struct $Plus *$Plus;

struct $PlusG_class;
typedef struct $PlusG_class *$PlusG_class;

struct B_Times;
typedef struct B_Times *B_Times;

struct B_TimesG_class;
typedef struct B_TimesG_class *B_TimesG_class;

struct B_Div;
typedef struct B_Div *B_Div;

struct B_DivG_class;
typedef struct B_DivG_class *B_DivG_class;

struct B_Minus;
typedef struct B_Minus *B_Minus;

struct B_MinusG_class;
typedef struct B_MinusG_class *B_MinusG_class;

struct B_Hashable;
typedef struct B_Hashable *B_Hashable;

struct B_HashableG_class;
typedef struct B_HashableG_class *B_HashableG_class;

struct B_Indexed;
typedef struct B_Indexed *B_Indexed;

struct B_IndexedG_class;
typedef struct B_IndexedG_class *B_IndexedG_class;

struct B_Sliceable;
typedef struct B_Sliceable *B_Sliceable;

struct B_SliceableG_class;
typedef struct B_SliceableG_class *B_SliceableG_class;

struct B_Iterable;
typedef struct B_Iterable *B_Iterable;

struct B_IterableG_class;
typedef struct B_IterableG_class *B_IterableG_class;

struct B_Collection;
typedef struct B_Collection *B_Collection;

struct B_CollectionG_class;
typedef struct B_CollectionG_class *B_CollectionG_class;

struct B_Container;
typedef struct B_Container *B_Container;

struct B_ContainerG_class;
typedef struct B_ContainerG_class *B_ContainerG_class;

struct B_Sequence;
typedef struct B_Sequence *B_Sequence;

struct B_SequenceG_class;
typedef struct B_SequenceG_class *B_SequenceG_class;

struct B_Mapping;
typedef struct B_Mapping *B_Mapping;

struct B_MappingG_class;
typedef struct B_MappingG_class *B_MappingG_class;

struct B_Set;
typedef struct B_Set *B_Set;

struct B_SetG_class;
typedef struct B_SetG_class *B_SetG_class;

struct B_Number;
typedef struct B_Number *B_Number;

struct B_NumberG_class;
typedef struct B_NumberG_class *B_NumberG_class;

struct B_Real;
typedef struct B_Real *B_Real;

struct B_RealG_class;
typedef struct B_RealG_class *B_RealG_class;

struct B_Rational;
typedef struct B_Rational *B_Rational;

struct B_RationalG_class;
typedef struct B_RationalG_class *B_RationalG_class;

struct B_Integral;
typedef struct B_Integral *B_Integral;

struct B_IntegralG_class;
typedef struct B_IntegralG_class *B_IntegralG_class;

struct B_HashableD_bool;
typedef struct B_HashableD_bool *B_HashableD_bool;

struct B_HashableD_boolG_class;
typedef struct B_HashableD_boolG_class *B_HashableD_boolG_class;

struct B_SequenceD_list;
typedef struct B_SequenceD_list *B_SequenceD_list;

struct B_SequenceD_listG_class;
typedef struct B_SequenceD_listG_class *B_SequenceD_listG_class;

struct B_CollectionD_SequenceD_list;
typedef struct B_CollectionD_SequenceD_list *B_CollectionD_SequenceD_list;

struct B_CollectionD_SequenceD_listG_class;
typedef struct B_CollectionD_SequenceD_listG_class *B_CollectionD_SequenceD_listG_class;

struct B_TimesD_SequenceD_list;
typedef struct B_TimesD_SequenceD_list *B_TimesD_SequenceD_list;

struct B_TimesD_SequenceD_listG_class;
typedef struct B_TimesD_SequenceD_listG_class *B_TimesD_SequenceD_listG_class;

struct B_ContainerD_list;
typedef struct B_ContainerD_list *B_ContainerD_list;

struct B_ContainerD_listG_class;
typedef struct B_ContainerD_listG_class *B_ContainerD_listG_class;

struct B_OrdD_list;
typedef struct B_OrdD_list *B_OrdD_list;

struct B_OrdD_listG_class;
typedef struct B_OrdD_listG_class *B_OrdD_listG_class;

struct B_MappingD_dict;
typedef struct B_MappingD_dict *B_MappingD_dict;

struct B_MappingD_dictG_class;
typedef struct B_MappingD_dictG_class *B_MappingD_dictG_class;

struct B_IndexedD_MappingD_dict;
typedef struct B_IndexedD_MappingD_dict *B_IndexedD_MappingD_dict;

struct B_IndexedD_MappingD_dictG_class;
typedef struct B_IndexedD_MappingD_dictG_class *B_IndexedD_MappingD_dictG_class;

struct B_OrdD_dict;
typedef struct B_OrdD_dict *B_OrdD_dict;

struct B_OrdD_dictG_class;
typedef struct B_OrdD_dictG_class *B_OrdD_dictG_class;

struct B_SetD_set;
typedef struct B_SetD_set *B_SetD_set;

struct B_SetD_setG_class;
typedef struct B_SetD_setG_class *B_SetD_setG_class;

struct B_OrdD_SetD_set;
typedef struct B_OrdD_SetD_set *B_OrdD_SetD_set;

struct B_OrdD_SetD_setG_class;
typedef struct B_OrdD_SetD_setG_class *B_OrdD_SetD_setG_class;

struct B_LogicalD_SetD_set;
typedef struct B_LogicalD_SetD_set *B_LogicalD_SetD_set;

struct B_LogicalD_SetD_setG_class;
typedef struct B_LogicalD_SetD_setG_class *B_LogicalD_SetD_setG_class;

struct B_MinusD_SetD_set;
typedef struct B_MinusD_SetD_set *B_MinusD_SetD_set;

struct B_MinusD_SetD_setG_class;
typedef struct B_MinusD_SetD_setG_class *B_MinusD_SetD_setG_class;

struct B_IterableD_Iterator;
typedef struct B_IterableD_Iterator *B_IterableD_Iterator;

struct B_IterableD_IteratorG_class;
typedef struct B_IterableD_IteratorG_class *B_IterableD_IteratorG_class;

struct B_OrdD_str;
typedef struct B_OrdD_str *B_OrdD_str;

struct B_OrdD_strG_class;
typedef struct B_OrdD_strG_class *B_OrdD_strG_class;

struct B_ContainerD_str;
typedef struct B_ContainerD_str *B_ContainerD_str;

struct B_ContainerD_strG_class;
typedef struct B_ContainerD_strG_class *B_ContainerD_strG_class;

struct B_SliceableD_str;
typedef struct B_SliceableD_str *B_SliceableD_str;

struct B_SliceableD_strG_class;
typedef struct B_SliceableD_strG_class *B_SliceableD_strG_class;

struct B_TimesD_str;
typedef struct B_TimesD_str *B_TimesD_str;

struct B_TimesD_strG_class;
typedef struct B_TimesD_strG_class *B_TimesD_strG_class;

struct B_HashableD_str;
typedef struct B_HashableD_str *B_HashableD_str;

struct B_HashableD_strG_class;
typedef struct B_HashableD_strG_class *B_HashableD_strG_class;

struct B_IntegralD_i64;
typedef struct B_IntegralD_i64 *B_IntegralD_i64;

struct B_IntegralD_i64G_class;
typedef struct B_IntegralD_i64G_class *B_IntegralD_i64G_class;

struct B_LogicalD_IntegralD_i64;
typedef struct B_LogicalD_IntegralD_i64 *B_LogicalD_IntegralD_i64;

struct B_LogicalD_IntegralD_i64G_class;
typedef struct B_LogicalD_IntegralD_i64G_class *B_LogicalD_IntegralD_i64G_class;

struct B_MinusD_IntegralD_i64;
typedef struct B_MinusD_IntegralD_i64 *B_MinusD_IntegralD_i64;

struct B_MinusD_IntegralD_i64G_class;
typedef struct B_MinusD_IntegralD_i64G_class *B_MinusD_IntegralD_i64G_class;

struct B_DivD_i64;
typedef struct B_DivD_i64 *B_DivD_i64;

struct B_DivD_i64G_class;
typedef struct B_DivD_i64G_class *B_DivD_i64G_class;

struct B_OrdD_i64;
typedef struct B_OrdD_i64 *B_OrdD_i64;

struct B_OrdD_i64G_class;
typedef struct B_OrdD_i64G_class *B_OrdD_i64G_class;

struct B_HashableD_i64;
typedef struct B_HashableD_i64 *B_HashableD_i64;

struct B_HashableD_i64G_class;
typedef struct B_HashableD_i64G_class *B_HashableD_i64G_class;

struct B_IntegralD_int;
typedef struct B_IntegralD_int *B_IntegralD_int;

struct B_IntegralD_intG_class;
typedef struct B_IntegralD_intG_class *B_IntegralD_intG_class;

struct B_LogicalD_IntegralD_int;
typedef struct B_LogicalD_IntegralD_int *B_LogicalD_IntegralD_int;

struct B_LogicalD_IntegralD_intG_class;
typedef struct B_LogicalD_IntegralD_intG_class *B_LogicalD_IntegralD_intG_class;

struct B_MinusD_IntegralD_int;
typedef struct B_MinusD_IntegralD_int *B_MinusD_IntegralD_int;

struct B_MinusD_IntegralD_intG_class;
typedef struct B_MinusD_IntegralD_intG_class *B_MinusD_IntegralD_intG_class;

struct B_DivD_int;
typedef struct B_DivD_int *B_DivD_int;

struct B_DivD_intG_class;
typedef struct B_DivD_intG_class *B_DivD_intG_class;

struct B_OrdD_int;
typedef struct B_OrdD_int *B_OrdD_int;

struct B_OrdD_intG_class;
typedef struct B_OrdD_intG_class *B_OrdD_intG_class;

struct B_HashableD_int;
typedef struct B_HashableD_int *B_HashableD_int;

struct B_HashableD_intG_class;
typedef struct B_HashableD_intG_class *B_HashableD_intG_class;

struct B_RealD_float;
typedef struct B_RealD_float *B_RealD_float;

struct B_RealD_floatG_class;
typedef struct B_RealD_floatG_class *B_RealD_floatG_class;

struct B_DivD_float;
typedef struct B_DivD_float *B_DivD_float;

struct B_DivD_floatG_class;
typedef struct B_DivD_floatG_class *B_DivD_floatG_class;

struct B_MinusD_RealD_float;
typedef struct B_MinusD_RealD_float *B_MinusD_RealD_float;

struct B_MinusD_RealD_floatG_class;
typedef struct B_MinusD_RealD_floatG_class *B_MinusD_RealD_floatG_class;

struct B_OrdD_float;
typedef struct B_OrdD_float *B_OrdD_float;

struct B_OrdD_floatG_class;
typedef struct B_OrdD_floatG_class *B_OrdD_floatG_class;

struct B_HashableD_float;
typedef struct B_HashableD_float *B_HashableD_float;

struct B_HashableD_floatG_class;
typedef struct B_HashableD_floatG_class *B_HashableD_floatG_class;

struct B_NumberD_complex;
typedef struct B_NumberD_complex *B_NumberD_complex;

struct B_NumberD_complexG_class;
typedef struct B_NumberD_complexG_class *B_NumberD_complexG_class;

struct B_DivD_complex;
typedef struct B_DivD_complex *B_DivD_complex;

struct B_DivD_complexG_class;
typedef struct B_DivD_complexG_class *B_DivD_complexG_class;

struct B_MinusD_NumberD_complex;
typedef struct B_MinusD_NumberD_complex *B_MinusD_NumberD_complex;

struct B_MinusD_NumberD_complexG_class;
typedef struct B_MinusD_NumberD_complexG_class *B_MinusD_NumberD_complexG_class;

struct B_EqD_complex;
typedef struct B_EqD_complex *B_EqD_complex;

struct B_EqD_complexG_class;
typedef struct B_EqD_complexG_class *B_EqD_complexG_class;

struct B_HashableD_complex;
typedef struct B_HashableD_complex *B_HashableD_complex;

struct B_HashableD_complexG_class;
typedef struct B_HashableD_complexG_class *B_HashableD_complexG_class;

struct B_IterableD_range;
typedef struct B_IterableD_range *B_IterableD_range;

struct B_IterableD_rangeG_class;
typedef struct B_IterableD_rangeG_class *B_IterableD_rangeG_class;

struct B_IterableD_tuple;
typedef struct B_IterableD_tuple *B_IterableD_tuple;

struct B_IterableD_tupleG_class;
typedef struct B_IterableD_tupleG_class *B_IterableD_tupleG_class;

struct B_SliceableD_tuple;
typedef struct B_SliceableD_tuple *B_SliceableD_tuple;

struct B_SliceableD_tupleG_class;
typedef struct B_SliceableD_tupleG_class *B_SliceableD_tupleG_class;

struct B_HashableD_tuple;
typedef struct B_HashableD_tuple *B_HashableD_tuple;

struct B_HashableD_tupleG_class;
typedef struct B_HashableD_tupleG_class *B_HashableD_tupleG_class;

struct B_OrdD_bytearray;
typedef struct B_OrdD_bytearray *B_OrdD_bytearray;

struct B_OrdD_bytearrayG_class;
typedef struct B_OrdD_bytearrayG_class *B_OrdD_bytearrayG_class;

struct B_SequenceD_bytearray;
typedef struct B_SequenceD_bytearray *B_SequenceD_bytearray;

struct B_SequenceD_bytearrayG_class;
typedef struct B_SequenceD_bytearrayG_class *B_SequenceD_bytearrayG_class;

struct B_CollectionD_SequenceD_bytearray;
typedef struct B_CollectionD_SequenceD_bytearray *B_CollectionD_SequenceD_bytearray;

struct B_CollectionD_SequenceD_bytearrayG_class;
typedef struct B_CollectionD_SequenceD_bytearrayG_class *B_CollectionD_SequenceD_bytearrayG_class;

struct B_TimesD_SequenceD_bytearray;
typedef struct B_TimesD_SequenceD_bytearray *B_TimesD_SequenceD_bytearray;

struct B_TimesD_SequenceD_bytearrayG_class;
typedef struct B_TimesD_SequenceD_bytearrayG_class *B_TimesD_SequenceD_bytearrayG_class;

struct B_ContainerD_bytearray;
typedef struct B_ContainerD_bytearray *B_ContainerD_bytearray;

struct B_ContainerD_bytearrayG_class;
typedef struct B_ContainerD_bytearrayG_class *B_ContainerD_bytearrayG_class;

struct B_OrdD_bytes;
typedef struct B_OrdD_bytes *B_OrdD_bytes;

struct B_OrdD_bytesG_class;
typedef struct B_OrdD_bytesG_class *B_OrdD_bytesG_class;

struct B_ContainerD_bytes;
typedef struct B_ContainerD_bytes *B_ContainerD_bytes;

struct B_ContainerD_bytesG_class;
typedef struct B_ContainerD_bytesG_class *B_ContainerD_bytesG_class;

struct B_SliceableD_bytes;
typedef struct B_SliceableD_bytes *B_SliceableD_bytes;

struct B_SliceableD_bytesG_class;
typedef struct B_SliceableD_bytesG_class *B_SliceableD_bytesG_class;

struct B_TimesD_bytes;
typedef struct B_TimesD_bytes *B_TimesD_bytes;

struct B_TimesD_bytesG_class;
typedef struct B_TimesD_bytesG_class *B_TimesD_bytesG_class;

struct B_HashableD_bytes;
typedef struct B_HashableD_bytes *B_HashableD_bytes;

struct B_HashableD_bytesG_class;
typedef struct B_HashableD_bytesG_class *B_HashableD_bytesG_class;

struct B_HashableD_WORD;
typedef struct B_HashableD_WORD *B_HashableD_WORD;

struct B_HashableD_WORDG_class;
typedef struct B_HashableD_WORDG_class *B_HashableD_WORDG_class;


// B_Eq ////////////////////////////////////////////////////////////

struct B_Eq {
    B_EqG_class $class;
};

struct B_EqG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_Eq);
    void (*__serialize__)(B_Eq,$Serial$state);
    B_Eq (*__deserialize__)(B_Eq,$Serial$state);
    B_bool (*__bool__)(B_Eq);
    B_str (*__str__)(B_Eq);
    B_str (*__repr__)(B_Eq);
    B_bool (*__eq__)(B_Eq, $WORD, $WORD);
    B_bool (*__ne__)(B_Eq, $WORD, $WORD);
};

$WORD B_EqD___ne__(B_Eq wit, $WORD a, $WORD b);

extern struct B_EqG_class B_EqG_methods;
B_Eq B_EqG_new();

// B_Ord ////////////////////////////////////////////////////////////

struct B_Ord {
    B_OrdG_class $class;
};

struct B_OrdG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_Ord);
    void (*__serialize__)(B_Ord,$Serial$state);
    B_Ord (*__deserialize__)(B_Ord,$Serial$state);
    B_bool (*__bool__)(B_Ord);
    B_str (*__str__)(B_Ord);
    B_str (*__repr__)(B_Ord);
    B_bool (*__eq__)(B_Ord, $WORD, $WORD);
    B_bool (*__ne__)(B_Ord, $WORD, $WORD);
    B_bool (*__lt__)(B_Ord, $WORD, $WORD);
    B_bool (*__le__)(B_Ord, $WORD, $WORD);
    B_bool (*__gt__)(B_Ord, $WORD, $WORD);
    B_bool (*__ge__)(B_Ord, $WORD, $WORD);
};

$WORD B_OrdD___gt__(B_Ord wit, $WORD a, $WORD b);
$WORD B_OrdD___ge__(B_Ord wit, $WORD a, $WORD b);

extern struct B_OrdG_class B_OrdG_methods;
B_Ord B_OrdG_new();

// B_Logical ////////////////////////////////////////////////////////////

struct B_Logical {
    B_LogicalG_class $class;
};

struct B_LogicalG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_Logical);
    void (*__serialize__)(B_Logical,$Serial$state);
    B_Logical (*__deserialize__)(B_Logical,$Serial$state);
    B_bool (*__bool__)(B_Logical);
    B_str (*__str__)(B_Logical);
    B_str (*__repr__)(B_Logical);
    $WORD (*__and__)(B_Logical, $WORD, $WORD);
    $WORD (*__or__)(B_Logical, $WORD, $WORD);
    $WORD (*__xor__)(B_Logical, $WORD, $WORD);
    $WORD (*__iand__)(B_Logical, $WORD, $WORD);
    $WORD (*__ior__)(B_Logical, $WORD, $WORD);
    $WORD (*__ixor__)(B_Logical, $WORD, $WORD);
};

$WORD B_LogicalD___iand__(B_Logical, $WORD, $WORD);
$WORD B_LogicalD___ior__(B_Logical, $WORD, $WORD);
$WORD B_LogicalD___ixor__(B_Logical, $WORD, $WORD);

extern struct B_LogicalG_class B_LogicalG_methods;
B_Logical B_LogicalG_new();

// $Plus ////////////////////////////////////////////////////////////

struct $Plus {
    $PlusG_class $class;
};

struct $PlusG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($Plus);
    void (*__serialize__)($Plus,$Serial$state);
    $Plus (*__deserialize__)($Plus,$Serial$state);
    B_bool (*__bool__)($Plus);
    B_str (*__str__)($Plus);
    B_str (*__repr__)($Plus);
    $WORD (*__add__)($Plus, $WORD, $WORD);
    $WORD (*__iadd__)($Plus, $WORD, $WORD);
};

$WORD $PlusD___iadd__ ($Plus, $WORD, $WORD);

extern struct $PlusG_class $PlusG_methods;
$Plus $PlusG_new();

// B_Times ////////////////////////////////////////////////////////////

struct B_Times {
    B_TimesG_class $class;
};

struct B_TimesG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_Times);
    void (*__serialize__)(B_Times,$Serial$state);
    B_Times (*__deserialize__)(B_Times,$Serial$state);
    B_bool (*__bool__)(B_Times);
    B_str (*__str__)(B_Times);
    B_str (*__repr__)(B_Times);
    $WORD (*__add__)(B_Times, $WORD, $WORD);
    $WORD (*__iadd__)(B_Times, $WORD, $WORD);
    $WORD (*__mul__)(B_Times, $WORD, $WORD);
    $WORD (*__imul__)(B_Times, $WORD, $WORD);
};

$WORD B_TimesD___imul__ (B_Times, $WORD, $WORD);

extern struct B_TimesG_class B_TimesG_methods;
B_Times B_TimesG_new();

// B_Div ////////////////////////////////////////////////////////////

struct B_Div {
    B_DivG_class $class;
};

struct B_DivG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_Div);
    void (*__serialize__)(B_Div,$Serial$state);
    B_Div (*__deserialize__)(B_Div,$Serial$state);
    B_bool (*__bool__)(B_Div);
    B_str (*__str__)(B_Div);
    B_str (*__repr__)(B_Div);
    $WORD (*__truediv__)(B_Div, $WORD, $WORD);
    $WORD (*__itruediv__)(B_Div, $WORD, $WORD);
};

$WORD B_DivD___itruediv__ (B_Div, $WORD, $WORD);

extern struct B_DivG_class B_DivG_methods;
B_Div B_DivG_new();

// B_Minus ////////////////////////////////////////////////////////////

struct B_Minus {
    B_MinusG_class $class;
};

struct B_MinusG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_Minus);
    void (*__serialize__)(B_Minus,$Serial$state);
    B_Minus (*__deserialize__)(B_Minus,$Serial$state);
    B_bool (*__bool__)(B_Minus);
    B_str (*__str__)(B_Minus);
    B_str (*__repr__)(B_Minus);
    $WORD (*__sub__)(B_Minus, $WORD, $WORD);
    $WORD (*__isub__)(B_Minus, $WORD, $WORD);
};

$WORD B_MinusD___isub__ (B_Minus, $WORD, $WORD);

extern struct B_MinusG_class B_MinusG_methods;
B_Minus B_MinusG_new();

// B_Hashable ////////////////////////////////////////////////////////////

struct B_Hashable {
    B_HashableG_class $class;
};

struct B_HashableG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_Hashable);
    void (*__serialize__)(B_Hashable,$Serial$state);
    B_Hashable (*__deserialize__)(B_Hashable,$Serial$state);
    B_bool (*__bool__)(B_Hashable);
    B_str (*__str__)(B_Hashable);
    B_str (*__repr__)(B_Hashable);
    B_bool (*__eq__)(B_Hashable, $WORD, $WORD);
    B_bool (*__ne__)(B_Hashable, $WORD, $WORD);
    B_int (*__hash__)(B_Hashable, $WORD);
};

extern struct B_HashableG_class B_HashableG_methods;
B_Hashable B_HashableG_new();

// B_Indexed ////////////////////////////////////////////////////////////

struct B_Indexed {
    B_IndexedG_class $class;
    B_Eq W_EqD_AD_Indexed;
};

struct B_IndexedG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_Indexed, B_Eq);
    void (*__serialize__)(B_Indexed,$Serial$state);
    B_Indexed (*__deserialize__)(B_Indexed,$Serial$state);
    B_bool (*__bool__)(B_Indexed);
    B_str (*__str__)(B_Indexed);
    B_str (*__repr__)(B_Indexed);
    $WORD (*__getitem__)(B_Indexed, $WORD, $WORD);
    void (*__setitem__)(B_Indexed, $WORD, $WORD, $WORD);
    void (*__delitem__)(B_Indexed, $WORD, $WORD);
};

extern struct B_IndexedG_class B_IndexedG_methods;
B_Indexed B_IndexedG_new(B_Eq);

// B_Sliceable ////////////////////////////////////////////////////////////

struct B_Sliceable {
    B_SliceableG_class $class;
};

struct B_SliceableG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_Sliceable);
    void (*__serialize__)(B_Sliceable,$Serial$state);
    B_Sliceable (*__deserialize__)(B_Sliceable,$Serial$state);
    B_bool (*__bool__)(B_Sliceable);
    B_str (*__str__)(B_Sliceable);
    B_str (*__repr__)(B_Sliceable);
    $WORD (*__getitem__)(B_Sliceable, $WORD, B_int);
    void (*__setitem__)(B_Sliceable, $WORD, B_int, $WORD);
    void (*__delitem__)(B_Sliceable, $WORD, B_int);
    $WORD (*__getslice__)(B_Sliceable, $WORD, B_slice);
    void (*__setslice__)(B_Sliceable, $WORD, B_Iterable, B_slice, $WORD);
    void (*__delslice__)(B_Sliceable, $WORD, B_slice);
};

extern struct B_SliceableG_class B_SliceableG_methods;
B_Sliceable B_SliceableG_new();

// B_Iterable ////////////////////////////////////////////////////////////

struct B_Iterable {
    B_IterableG_class $class;
};

struct B_IterableG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_Iterable);
    void (*__serialize__)(B_Iterable,$Serial$state);
    B_Iterable (*__deserialize__)(B_Iterable,$Serial$state);
    B_bool (*__bool__)(B_Iterable);
    B_str (*__str__)(B_Iterable);
    B_str (*__repr__)(B_Iterable);
    B_Iterator (*__iter__)(B_Iterable, $WORD);
};

extern struct B_IterableG_class B_IterableG_methods;
B_Iterable B_IterableG_new();

// B_Collection ////////////////////////////////////////////////////////////

struct B_Collection {
    B_CollectionG_class $class;
};

struct B_CollectionG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_Collection);
    void (*__serialize__)(B_Collection,$Serial$state);
    B_Collection (*__deserialize__)(B_Collection,$Serial$state);
    B_bool (*__bool__)(B_Collection);
    B_str (*__str__)(B_Collection);
    B_str (*__repr__)(B_Collection);
    B_Iterator (*__iter__)(B_Collection, $WORD);
    $WORD (*__fromiter__)(B_Collection, B_Iterable, $WORD);
    B_int (*__len__)(B_Collection, $WORD);
};

extern struct B_CollectionG_class B_CollectionG_methods;
B_Collection B_CollectionG_new();

// B_Container ////////////////////////////////////////////////////////////

struct B_Container {
    B_ContainerG_class $class;
    B_Eq W_EqD_AD_Container;
};

struct B_ContainerG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_Container, B_Eq);
    void (*__serialize__)(B_Container,$Serial$state);
    B_Container (*__deserialize__)(B_Container,$Serial$state);
    B_bool (*__bool__)(B_Container);
    B_str (*__str__)(B_Container);
    B_str (*__repr__)(B_Container);
    B_Iterator (*__iter__)(B_Container, $WORD);
    $WORD (*__fromiter__)(B_Container, B_Iterable, $WORD);
    B_int (*__len__)(B_Container, $WORD);
    B_bool (*__contains__)(B_Container, $WORD, $WORD);
    B_bool (*__containsnot__)(B_Container, $WORD, $WORD);
};

extern struct B_ContainerG_class B_ContainerG_methods;
B_Container B_ContainerG_new();

// B_Sequence ////////////////////////////////////////////////////////////

struct B_Sequence {
    B_SequenceG_class $class;
    B_Collection W_Collection;
    B_Times W_Times;
};

struct B_SequenceG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_Sequence);
    void (*__serialize__)(B_Sequence,$Serial$state);
    B_Sequence (*__deserialize__)(B_Sequence,$Serial$state);
    B_bool (*__bool__)(B_Sequence);
    B_str (*__str__)(B_Sequence);
    B_str (*__repr__)(B_Sequence);
    $WORD (*__getitem__)(B_Sequence, $WORD, B_int);
    void (*__setitem__)(B_Sequence, $WORD, B_int, $WORD);
    void (*__delitem__)(B_Sequence, $WORD, B_int);
    $WORD (*__getslice__)(B_Sequence, $WORD, B_slice);
    void (*__setslice__)(B_Sequence, $WORD, B_Iterable, B_slice, $WORD);
    void (*__delslice__)(B_Sequence, $WORD, B_slice);
    B_Iterator (*__reversed__)(B_Sequence, $WORD);
    void (*insert)(B_Sequence, $WORD, B_int, $WORD);
    void (*append)(B_Sequence, $WORD, $WORD);
    void (*reverse)(B_Sequence, $WORD);
};

extern struct B_SequenceG_class B_SequenceG_methods;
B_Sequence B_SequenceG_new();

// B_CollectionD_Sequence //////////////////////////////////////////////

#define B_CollectionD_Sequence B_Collection
#define B_CollectionD_SequenceG_class B_CollectionG_class
#define B_CollectionD_SequenceG_methods B_CollectionG_methods
#define B_CollectionD_SequenceG_new(...) B_CollectionG_new(__VA_ARGS__)

// B_TimesD_Sequence ///////////////////////////////////////////////////

#define B_TimesD_Sequence B_Times
#define B_TimesD_SequenceG_class B_TimesG_class
#define B_TimesD_SequenceG_methods B_TimesG_methods
#define B_TimesD_SequenceG_new(...) B_TimesG_new(__VA_ARGS__)

// B_Mapping ////////////////////////////////////////////////////////////

struct B_Mapping {
    B_MappingG_class $class;
    B_Indexed W_Indexed;
    B_Eq W_EqD_AD_Mapping;
};

struct B_MappingG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_Mapping, B_Eq);
    void (*__serialize__)(B_Mapping,$Serial$state);
    B_Mapping (*__deserialize__)(B_Mapping,$Serial$state);
    B_bool (*__bool__)(B_Mapping);
    B_str (*__str__)(B_Mapping);
    B_str (*__repr__)(B_Mapping);
    B_Iterator (*__iter__)(B_Mapping, $WORD);
    $WORD (*__fromiter__)(B_Mapping, B_Iterable, $WORD);
    B_int (*__len__)(B_Mapping, $WORD);
    B_bool (*__contains__)(B_Mapping, $WORD, $WORD);
    B_bool (*__containsnot__)(B_Mapping, $WORD, $WORD);
    $WORD (*get)(B_Mapping, $WORD, $WORD, $WORD);
    B_Iterator (*keys)(B_Mapping, $WORD);
    B_Iterator (*values)(B_Mapping, $WORD);
    B_Iterator (*items)(B_Mapping, $WORD);
    void (*update)(B_Mapping, $WORD, B_Iterable, $WORD);
    B_tuple (*popitem)(B_Mapping, $WORD);
    void (*setdefault)(B_Mapping, $WORD, $WORD, $WORD);
};

extern struct B_MappingG_class B_MappingG_methods;
B_Mapping B_MappingG_new(B_Eq);

// B_IndexedD_Mapping ///////////////////////////////////////////////

#define B_IndexedD_Mapping B_Indexed
#define B_IndexedD_MappingG_class B_IndexedG_class
#define B_IndexedD_MappingG_methods B_IndexedG_methods
#define B_IndexedD_MappingG_new(...) B_IndexedG_new(__VA_ARGS__)

// B_Set ////////////////////////////////////////////////////////////

struct B_Set {
    B_SetG_class $class;
    B_Ord W_Ord;
    B_Logical W_Logical;
    B_Minus W_Minus;
    B_Eq W_EqD_AD_Set;
};

struct B_SetG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_Set, B_Eq);
    void (*__serialize__)(B_Set,$Serial$state);
    B_Set (*__deserialize__)(B_Set,$Serial$state);
    B_bool (*__bool__)(B_Set);
    B_str (*__str__)(B_Set);
    B_str (*__repr__)(B_Set);
    B_Iterator (*__iter__)(B_Set, $WORD);
    $WORD (*__fromiter__)(B_Set, B_Iterable, $WORD);
    B_int (*__len__)(B_Set, $WORD);
    B_bool (*__contains__)(B_Set, $WORD, $WORD);
    B_bool (*__containsnot__)(B_Set, $WORD, $WORD);
    B_bool (*isdisjoint)(B_Set, $WORD, $WORD);
    void (*add)(B_Set, $WORD, $WORD);
    void (*discard)(B_Set, $WORD, $WORD);
    $WORD (*pop)(B_Set, $WORD);
};

extern struct B_SetG_class B_SetG_methods;
B_Set B_SetG_new(B_Eq);

// B_OrdD_Set ////////////////////////////////////////////////////////////

#define B_OrdD_Set B_Ord
#define B_OrdD_SetD_class B_OrdD_class
#define B_OrdD_SetD_methods B_OrdD_methods
#define B_OrdD_SetD_new(...) B_OrdD_new(__VA_ARGS__)

// B_LogicalD_Set ////////////////////////////////////////////////////////

#define B_LogicalD_Set B_Logical
#define B_LogicalD_SetD_class B_LogicalD_class
#define B_LogicalD_SetD_methods B_LogicalD_methods
#define B_LogicalD_SetD_new(...) B_LogicalD_new(__VA_ARGS__)

// B_MinusD_Set //////////////////////////////////////////////////////////

#define B_MinusD_Set B_Minus
#define B_MinusD_SetD_class B_MinusD_class
#define B_MinusD_SetD_methods B_MinusD_methods
#define B_MinusD_SetD_new(...) B_MinusD_new(__VA_ARGS__)

// B_Number ////////////////////////////////////////////////////////////

struct B_Number {
    B_NumberG_class $class;
    B_Minus W_Minus;
};

struct B_NumberG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_Number);
    void (*__serialize__)(B_Number,$Serial$state);
    B_Number (*__deserialize__)(B_Number,$Serial$state);
    B_bool (*__bool__)(B_Number);
    B_str (*__str__)(B_Number);
    B_str (*__repr__)(B_Number);
    $WORD (*__add__)(B_Number, $WORD, $WORD);
    $WORD (*__iadd__)(B_Number, $WORD, $WORD);
    $WORD (*__mul__)(B_Number, $WORD, $WORD);
    $WORD (*__imul__)(B_Number, $WORD, $WORD);
    $WORD (*__fromatom__)(B_Number,B_atom);
    B_complex (*__complx__)(B_Number, $WORD);
    //    $WORD (*__truediv__)(B_Number, $WORD, $WORD);
    $WORD (*__pow__)(B_Number, $WORD, $WORD);
    //    $WORD (*__itruediv__)(B_Number, $WORD, $WORD);
    $WORD (*__ipow__)(B_Number, $WORD, $WORD);
    $WORD (*__neg__)(B_Number, $WORD);
    $WORD (*__pos__)(B_Number, $WORD);
    $WORD (*real)(B_Number, $WORD, B_Real);
    $WORD (*imag)(B_Number, $WORD, B_Real);
    $WORD (*__abs__)(B_Number, $WORD, B_Real);
    $WORD (*conjugate)(B_Number, $WORD);
};

//$WORD B_NumberD___itruediv__(B_Number, $WORD, $WORD);
$WORD B_NumberD___ipow__(B_Number, $WORD, $WORD);

extern struct B_NumberG_class B_NumberG_methods;
B_Number B_NumberG_new();

// B_MinusD_Number ///////////////////////////////////////////////////

#define B_MinusD_Number B_Minus
#define B_MinusD_NumberG_class B_MinusG_class
#define B_MinusD_NumberG_methods B_MinusG_methods
#define B_MinusD_NumberG_new(...) B_MinusG_new(__VA_ARGS__)

// B_Real ////////////////////////////////////////////////////////////

struct B_Real {
    B_RealG_class $class;
};

struct B_RealG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_Real);
    void (*__serialize__)(B_Real,$Serial$state);
    B_Real (*__deserialize__)(B_Real,$Serial$state);
    B_bool (*__bool__)(B_Real);
    B_str (*__str__)(B_Real);
    B_str (*__repr__)(B_Real);
    $WORD (*__add__)(B_Real, $WORD, $WORD);
    $WORD (*__iadd__)(B_Real, $WORD, $WORD);
    $WORD (*__mul__)(B_Real, $WORD, $WORD);
    $WORD (*__imul__)(B_Real, $WORD, $WORD);
    $WORD (*__fromatom__)(B_Real,B_atom);
    B_complex (*__complx__)(B_Real, $WORD);
    //    $WORD (*__truediv__)(B_Real, $WORD, $WORD);
    $WORD (*__pow__)(B_Real, $WORD, $WORD);
    //    $WORD (*__itruediv__)(B_Real, $WORD, $WORD);
    $WORD (*__ipow__)(B_Real, $WORD, $WORD);
    $WORD (*__neg__)(B_Real, $WORD);
    $WORD (*__pos__)(B_Real, $WORD);
    $WORD (*real)(B_Real, $WORD, B_Real);
    $WORD (*imag)(B_Real, $WORD, B_Real);
    $WORD (*__abs__)(B_Real, $WORD, B_Real);
    $WORD (*conjugate)(B_Real, $WORD);
    B_float (*__float__)(B_Real, $WORD);
    $WORD (*__trunc__)(B_Real, $WORD, B_Integral);
    $WORD (*__floor__)(B_Real, $WORD, B_Integral);
    $WORD (*__ceil__)(B_Real, $WORD, B_Integral);
    $WORD (*__round__)(B_Real, $WORD, B_int);
};

extern struct B_RealG_class B_RealG_methods;
B_Real B_RealG_new();

// B_MinusD_Real /////////////////////////////////////////////////////////

#define B_MinusD_Real B_Minus
#define B_MinusD_RealG_class B_MinusG_class
#define B_MinusD_RealG_methods B_MinusG_methods
#define B_MinusD_RealG_new(...) B_MinusG_new(__VA_ARGS__)

// B_RealFloat ///////////////////////////////////////////////////////////

#define B_RealFloat B_Real
#define B_RealFloatG_class B_RealG_class
#define B_RealFloatG_methods B_RealG_methods
#define B_RealFloatG_new(...) B_RealG_new(__VA_ARGS__)

// B_MinusD_RealFloat ////////////////////////////////////////////////////

#define B_MinusD_RealFloat B_Minus
#define B_MinusD_RealFloatG_class B_MinusG_class
#define B_MinusD_RealFloatG_methods B_MinusG_methods
#define B_MinusD_RealFloatG_new(...) B_MinusG_new(__VA_ARGS__)

// B_Rational ////////////////////////////////////////////////////////////

struct B_Rational {
    B_RationalG_class $class;
};

struct B_RationalG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_Rational);
    void (*__serialize__)(B_Rational,$Serial$state);
    B_Rational (*__deserialize__)(B_Rational,$Serial$state);
    B_bool (*__bool__)(B_Rational);
    B_str (*__str__)(B_Rational);
    B_str (*__repr__)(B_Rational);
    $WORD (*__add__)(B_Rational, $WORD, $WORD);
    $WORD (*__iadd__)(B_Rational, $WORD, $WORD);
    $WORD (*__mul__)(B_Rational, $WORD, $WORD);
    $WORD (*__imul__)(B_Rational, $WORD, $WORD);
    $WORD (*__fromatom__)(B_Rational,B_atom);
    B_complex (*__complx__)(B_Rational, $WORD);
    //    $WORD (*__truediv__)(B_Rational, $WORD, $WORD);
    $WORD (*__pow__)(B_Rational, $WORD, $WORD);
    //    $WORD (*__itruediv__)(B_Rational, $WORD, $WORD);
    $WORD (*__ipow__)(B_Rational, $WORD, $WORD);
    $WORD (*__neg__)(B_Rational, $WORD);
    $WORD (*__pos__)(B_Rational, $WORD);
    $WORD (*real)(B_Rational, $WORD, B_Real);
    $WORD (*imag)(B_Rational, $WORD, B_Real);
    $WORD (*__abs__)(B_Rational, $WORD, B_Real);
    $WORD (*conjugate)(B_Rational, $WORD);
    B_float (*__float__)(B_Rational, $WORD);
    $WORD (*__trunc__)(B_Rational, $WORD, B_Integral);
    $WORD (*__floor__)(B_Rational, $WORD, B_Integral);
    $WORD (*__ceil__)(B_Rational, $WORD, B_Integral);
    $WORD (*__round__)(B_Rational, $WORD, B_int);
    $WORD (*numerator)(B_Rational, $WORD, B_Integral);
    $WORD (*denominator)(B_Rational, $WORD, B_Integral);
};

extern struct B_RationalG_class B_RationalG_methods;
B_Rational B_RationalG_new();

// B_MinusD_Rational /////////////////////////////////////////////////////

#define B_MinusD_Rational B_Minus
#define B_MinusD_RationalG_class B_MinusG_class
#define B_MinusD_RationalG_methods B_MinusG_methods
#define B_MinusD_RationalG_new(...) B_MinusG_new(__VA_ARGS__)

// B_Integral ////////////////////////////////////////////////////////////

struct B_Integral {
    B_IntegralG_class $class;
    B_Minus W_Minus;
    B_Logical W_Logical;
};

struct B_IntegralG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_Integral);
    void (*__serialize__)(B_Integral,$Serial$state);
    B_Integral (*__deserialize__)(B_Integral,$Serial$state);
    B_bool (*__bool__)(B_Integral);
    B_str (*__str__)(B_Integral);
    B_str (*__repr__)(B_Integral);
    $WORD (*__add__)(B_Integral, $WORD, $WORD);
    $WORD (*__iadd__)(B_Integral, $WORD, $WORD);
    $WORD (*__mul__)(B_Integral, $WORD, $WORD);
    $WORD (*__imul__)(B_Integral, $WORD, $WORD);
    $WORD (*__fromatom__)(B_Integral,B_atom);
    B_complex (*__complx__)(B_Integral, $WORD);
    //    $WORD (*__truediv__)(B_Integral, $WORD, $WORD);
    $WORD (*__pow__)(B_Integral, $WORD, $WORD);
    //    $WORD (*__itruediv__)(B_Integral, $WORD, $WORD);
    $WORD (*__ipow__)(B_Integral, $WORD, $WORD);
    $WORD (*__neg__)(B_Integral, $WORD);
    $WORD (*__pos__)(B_Integral, $WORD);
    $WORD (*real)(B_Integral, $WORD, B_Real);
    $WORD (*imag)(B_Integral, $WORD, B_Real);
    $WORD (*__abs__)(B_Integral, $WORD, B_Real);
    $WORD (*conjugate)(B_Integral, $WORD);
    B_float (*__float__)(B_Integral, $WORD);
    $WORD (*__trunc__)(B_Integral, $WORD, B_Integral);
    $WORD (*__floor__)(B_Integral, $WORD, B_Integral);
    $WORD (*__ceil__)(B_Integral, $WORD, B_Integral);
    $WORD (*__round__)(B_Integral, $WORD, B_int);
    $WORD (*numerator)(B_Integral, $WORD, B_Integral);
    $WORD (*denominator)(B_Integral, $WORD, B_Integral);
    B_int (*__int__)(B_Integral, $WORD);
    B_int (*__index__)(B_Integral, $WORD);
    B_tuple (*__divmod__)(B_Integral, $WORD, $WORD);
    $WORD (*__floordiv__)(B_Integral, $WORD, $WORD);
    $WORD (*__mod__)(B_Integral, $WORD, $WORD);
    $WORD (*__lshift__)(B_Integral, $WORD, B_int);
    $WORD (*__rshift__)(B_Integral, $WORD, B_int);
    $WORD (*__ifloordiv__)(B_Integral, $WORD, $WORD);
    $WORD (*__imod__)(B_Integral, $WORD, $WORD);
    $WORD (*__ilshift__)(B_Integral, $WORD, B_int);
    $WORD (*__irshift__)(B_Integral, $WORD, B_int);
    $WORD (*__invert__)(B_Integral, $WORD);
};

$WORD B_IntegralD___ifloordiv__(B_Integral, $WORD, $WORD);
$WORD B_IntegralD___imod__(B_Integral, $WORD, $WORD);
$WORD B_IntegralD___ilshift__(B_Integral, $WORD, B_int);
$WORD B_IntegralD___irshift__(B_Integral, $WORD, B_int);

extern struct B_IntegralG_class B_IntegralG_methods;
B_Integral B_IntegralG_new();

// B_MinusD_Integral //////////////////////////////////////////////////////////

#define B_MinusD_Integral B_Minus
#define B_MinusD_IntegralG_class B_MinusG_class
#define B_MinusD_IntegralG_methods B_MinusG_methods
#define B_MinusD_IntegralG_new(...) B_MinusG_new(__VA_ARGS__)

// B_LogicalD_Integral ////////////////////////////////////////////////////////

#define B_LogicalD_Integral B_Logical
#define B_LogicalD_IntegralG_class B_LogicalG_class
#define B_LogicalD_IntegralG_methods B_LogicalG_methods
#define B_LogicalD_IntegralG_new(...) B_LogicalG_new(__VA_ARGS__)

// B_HashableD_bool ////////////////////////////////////////////////////////////

struct B_HashableD_bool {
    B_HashableD_boolG_class $class;
};

struct B_HashableD_boolG_class {
    char *$GCINFO;
    bool $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_HashableD_bool);
    void (*__serialize__)(B_HashableD_bool,$Serial$state);
    B_HashableD_bool (*__deserialize__)(B_HashableD_bool,$Serial$state);
    B_bool (*__bool__)(B_HashableD_bool);
    B_str (*__str__)(B_HashableD_bool);
    B_str (*__repr__)(B_HashableD_bool);
    B_bool (*__eq__)(B_HashableD_bool, B_bool, B_bool);
    B_bool (*__ne__)(B_HashableD_bool, B_bool, B_bool);
    B_int (*__hash__)(B_HashableD_bool, B_bool);
};

void B_HashableD_boolD___init__ (B_HashableD_bool);
void B_HashableD_boolD___serialize__(B_HashableD_bool, $Serial$state);
B_HashableD_bool B_HashableD_boolD___deserialize__(B_HashableD_bool, $Serial$state);
B_bool B_HashableD_boolD___eq__ (B_HashableD_bool, B_bool, B_bool);
B_bool B_HashableD_boolD___ne__ (B_HashableD_bool, B_bool, B_bool);
B_int B_HashableD_boolD___hash__ (B_HashableD_bool, B_bool);

extern struct B_HashableG_class B_HashableG_methods;
B_Hashable B_HashableG_new();

// B_SequenceD_list ////////////////////////////////////////////////////////////

struct B_SequenceD_list {
    B_SequenceD_listG_class $class;
    B_Collection W_Collection;
    B_Times W_Times;
};

struct B_SequenceD_listG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_SequenceD_list);
    void (*__serialize__)(B_SequenceD_list,$Serial$state);
    B_SequenceD_list (*__deserialize__)(B_SequenceD_list,$Serial$state);
    B_bool (*__bool__)(B_SequenceD_list);
    B_str (*__str__)(B_SequenceD_list);
    B_str (*__repr__)(B_SequenceD_list);
    $WORD (*__getitem__)(B_SequenceD_list, B_list, B_int);
    void (*__setitem__)(B_SequenceD_list, B_list, B_int, $WORD);
    void (*__delitem__)(B_SequenceD_list, B_list, B_int);
    B_list (*__getslice__)(B_SequenceD_list, B_list, B_slice);
    void (*__setslice__)(B_SequenceD_list, B_list, B_Iterable, B_slice, $WORD);
    void (*__delslice__)(B_SequenceD_list, B_list, B_slice);
    B_Iterator (*__reversed__)(B_SequenceD_list, B_list);
    void (*insert)(B_SequenceD_list, B_list, B_int, $WORD);
    void (*append)(B_SequenceD_list, B_list, $WORD);
    void (*reverse)(B_SequenceD_list, B_list);
};

void B_SequenceD_listD___init__ (B_SequenceD_list);
void B_SequenceD_listD___serialize__(B_SequenceD_list, $Serial$state);
B_SequenceD_list B_SequenceD_listD___deserialize__(B_SequenceD_list, $Serial$state);
$WORD B_SequenceD_listD___getitem__ (B_SequenceD_list, B_list, B_int);
void B_SequenceD_listD___setitem__ (B_SequenceD_list, B_list, B_int, $WORD);
void B_SequenceD_listD___delitem__ (B_SequenceD_list, B_list, B_int);
B_list B_SequenceD_listD___getslice__ (B_SequenceD_list, B_list, B_slice);
void B_SequenceD_listD___setslice__ (B_SequenceD_list, B_list, B_Iterable, B_slice, $WORD);
void B_SequenceD_listD___delslice__ (B_SequenceD_list, B_list, B_slice);
B_Iterator B_SequenceD_listD___reversed__ (B_SequenceD_list, B_list);
void B_SequenceD_list$insert (B_SequenceD_list, B_list, B_int, $WORD);
void B_SequenceD_list$append (B_SequenceD_list, B_list, $WORD);
void B_SequenceD_list$reverse (B_SequenceD_list, B_list);

extern struct B_SequenceD_listG_class B_SequenceD_listG_methods;
B_SequenceD_list B_SequenceD_listG_new();

// B_CollectionD_SequenceD_list ////////////////////////////////////////////////////////////

struct B_CollectionD_SequenceD_list {
    B_CollectionD_SequenceD_listG_class $class;
    B_Sequence W_Sequence;
};

struct B_CollectionD_SequenceD_listG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_CollectionD_SequenceD_list, B_Sequence);
    void (*__serialize__)(B_CollectionD_SequenceD_list,$Serial$state);
    B_CollectionD_SequenceD_list (*__deserialize__)(B_CollectionD_SequenceD_list,$Serial$state);
    B_bool (*__bool__)(B_CollectionD_SequenceD_list);
    B_str (*__str__)(B_CollectionD_SequenceD_list);
    B_str (*__repr__)(B_CollectionD_SequenceD_list);
    B_Iterator (*__iter__)(B_CollectionD_SequenceD_list, B_list);
    B_list (*__fromiter__)(B_CollectionD_SequenceD_list, B_Iterable, $WORD);
    B_int (*__len__)(B_CollectionD_SequenceD_list, B_list);
};

void B_CollectionD_SequenceD_listD___init__ (B_CollectionD_SequenceD_list, B_Sequence);
void B_CollectionD_SequenceD_listD___serialize__(B_CollectionD_SequenceD_list, $Serial$state);
B_CollectionD_SequenceD_list B_CollectionD_SequenceD_listD___deserialize__(B_CollectionD_SequenceD_list, $Serial$state);
B_Iterator B_CollectionD_SequenceD_listD___iter__ (B_CollectionD_SequenceD_list, B_list);
B_list B_CollectionD_SequenceD_listD___fromiter__ (B_CollectionD_SequenceD_list, B_Iterable, $WORD);
B_int B_CollectionD_SequenceD_listD___len__ (B_CollectionD_SequenceD_list, B_list);

extern struct BCollectionD_SequenceD_listG_class BCollectionD_SequenceD_listG_methods;
B_CollectionD_SequenceD_list B_CollectionD_SequenceD_listG_new(B_Sequence);

// B_TimesD_SequenceD_list ////////////////////////////////////////////////////////////

struct B_TimesD_SequenceD_list {
    B_TimesD_SequenceD_listG_class $class;
    B_Sequence W_Sequence;
};

struct B_TimesD_SequenceD_listG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_TimesD_SequenceD_list, B_Sequence);
    void (*__serialize__)(B_TimesD_SequenceD_list,$Serial$state);
    B_TimesD_SequenceD_list (*__deserialize__)(B_TimesD_SequenceD_list,$Serial$state);
    B_bool (*__bool__)(B_TimesD_SequenceD_list);
    B_str (*__str__)(B_TimesD_SequenceD_list);
    B_str (*__repr__)(B_TimesD_SequenceD_list);
    B_list (*__add__)(B_TimesD_SequenceD_list, B_list, B_list);
    B_list (*__iadd__)(B_TimesD_SequenceD_list, B_list, B_list);
    B_list (*__mul__)(B_TimesD_SequenceD_list, B_list, B_int);
    B_list (*__imul__)(B_TimesD_SequenceD_list, B_list, B_int);
};

void B_TimesD_SequenceD_listD___init__ (B_TimesD_SequenceD_list, B_Sequence);
void B_TimesD_SequenceD_listD___serialize__(B_TimesD_SequenceD_list, $Serial$state);
B_TimesD_SequenceD_list B_TimesD_SequenceD_listD___deserialize__(B_TimesD_SequenceD_list, $Serial$state);
B_list B_TimesD_SequenceD_listD___add__ (B_TimesD_SequenceD_list, B_list, B_list);
B_list B_TimesD_SequenceD_listD___mul__(B_TimesD_SequenceD_list, B_list, B_int);

extern struct B_TimesD_SequenceD_listG_class B_TimesD_SequenceD_listG_methods;
B_TimesD_SequenceD_list B_TimesD_SequenceD_listG_new(B_Sequence);

// B_ContainerD_list ////////////////////////////////////////////////////////////

struct B_ContainerD_list {
    B_ContainerD_listG_class $class;
    B_Eq W_EqD_AD_ContainerB_list;
};

struct B_ContainerD_listG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_ContainerD_list, B_Eq);
    void (*__serialize__)(B_ContainerD_list,$Serial$state);
    B_ContainerD_list (*__deserialize__)(B_ContainerD_list,$Serial$state);
    B_bool (*__bool__)(B_ContainerD_list);
    B_str (*__str__)(B_ContainerD_list);
    B_str (*__repr__)(B_ContainerD_list);
    B_Iterator (*__iter__)(B_ContainerD_list, B_list);
    B_list (*__fromiter__)(B_ContainerD_list, B_Iterable, $WORD);
    B_int (*__len__)(B_ContainerD_list, B_list);
    B_bool (*__contains__)(B_ContainerD_list, B_list, $WORD);
    B_bool (*__containsnot__)(B_ContainerD_list, B_list, $WORD);
};

void B_ContainerD_listD___init__ (B_ContainerD_list, B_Eq);
void B_ContainerD_listD___serialize__(B_ContainerD_list, $Serial$state);
B_ContainerD_list B_ContainerD_listD___deserialize__(B_ContainerD_list, $Serial$state);
B_Iterator B_ContainerD_listD___iter__ (B_ContainerD_list, B_list);
B_list B_ContainerD_listD___fromiter__ (B_ContainerD_list, B_Iterable, $WORD);
B_int B_ContainerD_listD___len__ (B_ContainerD_list, B_list);
B_bool B_ContainerD_listD___contains__ (B_ContainerD_list, B_list, $WORD);
B_bool B_ContainerD_listD___containsnot__ (B_ContainerD_list, B_list, $WORD);

extern struct B_ContainerD_listG_class B_ContainerD_listG_methods;
B_ContainerD_list B_ContainerD_listG_new(B_Eq);

// B_OrdD_list //////////////////////////////////////////////////////////////////////

struct B_OrdD_list {
    B_OrdD_listG_class $class;
    B_Ord W_OrdD_AD_OrdB_list;
};

struct B_OrdD_listG_class {  
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_OrdD_list,B_Ord);
    void (*__serialize__)(B_OrdD_list,$Serial$state);
    B_OrdD_list (*__deserialize__)(B_OrdD_list,$Serial$state);
    B_bool (*__bool__)(B_OrdD_list);
    B_str (*__str__)(B_OrdD_list);
    B_str (*__repr__)(B_OrdD_list);
    B_bool (*__eq__)(B_OrdD_list, B_list, B_list);
    B_bool (*__ne__)(B_OrdD_list, B_list, B_list);
    B_bool (*__lt__)(B_OrdD_list, B_list, B_list);
    B_bool (*__le__)(B_OrdD_list, B_list, B_list);
    B_bool (*__gt__)(B_OrdD_list, B_list, B_list);
    B_bool (*__ge__)(B_OrdD_list, B_list, B_list);
};

void B_OrdD_listD___init__ (B_OrdD_list, B_Ord);
void B_OrdD_listD___serialize__(B_OrdD_list, $Serial$state);
B_OrdD_list B_OrdD_listD___deserialize__(B_OrdD_list, $Serial$state);
B_bool B_OrdD_listD___eq__ (B_OrdD_list, B_list, B_list);
B_bool B_OrdD_listD___ne__ (B_OrdD_list, B_list, B_list);
B_bool B_OrdD_listD___lt__ (B_OrdD_list, B_list, B_list);
B_bool B_OrdD_listD___le__ (B_OrdD_list, B_list, B_list);
B_bool B_OrdD_listD___gt__ (B_OrdD_list, B_list, B_list);
B_bool B_OrdD_listD___ge__ (B_OrdD_list, B_list, B_list);

extern struct B_OrdD_listG_class B_OrdD_listG_methods;
B_OrdD_list B_OrdD_listG_new(B_Ord);
  
// B_MappingD_dict ////////////////////////////////////////////////////////////

struct B_MappingD_dict {
    B_MappingD_dictG_class $class;
    B_Indexed W_Indexed;
    B_Eq W_EqD_AD_MappingB_dict;
    B_Hashable W_HashableD_AD_MappingB_dict;
};

struct B_MappingD_dictG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_MappingD_dict, B_Hashable);
    void (*__serialize__)(B_MappingD_dict,$Serial$state);
    B_MappingD_dict (*__deserialize__)(B_MappingD_dict,$Serial$state);
    B_bool (*__bool__)(B_MappingD_dict);
    B_str (*__str__)(B_MappingD_dict);
    B_str (*__repr__)(B_MappingD_dict);
    B_Iterator (*__iter__)(B_MappingD_dict, B_dict);
    B_dict (*__fromiter__)(B_MappingD_dict, B_Iterable, $WORD);
    B_int (*__len__)(B_MappingD_dict, B_dict);
    B_bool (*__contains__)(B_MappingD_dict, B_dict, $WORD);
    B_bool (*__containsnot__)(B_MappingD_dict, B_dict, $WORD);
    $WORD (*get)(B_MappingD_dict, B_dict, $WORD, $WORD);
    B_Iterator (*keys)(B_MappingD_dict, B_dict);
    B_Iterator (*values)(B_MappingD_dict, B_dict);
    B_Iterator (*items)(B_MappingD_dict, B_dict);
    void (*update)(B_MappingD_dict, B_dict, B_Iterable, $WORD);
    B_tuple (*popitem)(B_MappingD_dict, B_dict);
    void (*setdefault)(B_MappingD_dict, B_dict, $WORD, $WORD);
};

void B_MappingD_dictD___init__ (B_MappingD_dict, B_Hashable);
void B_MappingD_dictD___serialize__(B_MappingD_dict, $Serial$state);
B_MappingD_dict B_MappingD_dictD___deserialize__(B_MappingD_dict, $Serial$state);
B_Iterator B_MappingD_dictD___iter__ (B_MappingD_dict, B_dict);
B_dict B_MappingD_dictD___fromiter__ (B_MappingD_dict, B_Iterable, $WORD);
B_int B_MappingD_dictD___len__ (B_MappingD_dict, B_dict);
B_bool B_MappingD_dictD___contains__ (B_MappingD_dict, B_dict, $WORD);
B_bool B_MappingD_dictD___containsnot__ (B_MappingD_dict, B_dict, $WORD);
$WORD B_MappingD_dictD_get (B_MappingD_dict, B_dict, $WORD, $WORD);
B_Iterator B_MappingD_dictD_keys (B_MappingD_dict, B_dict);
B_Iterator B_MappingD_dictD_values (B_MappingD_dict, B_dict);
B_Iterator B_MappingD_dictD_items (B_MappingD_dict, B_dict);
void B_MappingD_dictD_update (B_MappingD_dict, B_dict, B_Iterable, $WORD);
B_tuple B_MappingD_dictD_popitem (B_MappingD_dict, B_dict);
void B_MappingD_dictD_setdefault (B_MappingD_dict, B_dict, $WORD, $WORD);

extern struct B_MappingD_dictG_class B_MappingD_dictG_methods;
B_MappingD_dict B_MappingD_dictG_new(B_Hashable);

// B_IndexedD_MappingD_dict ////////////////////////////////////////////////////////////

struct B_IndexedD_MappingD_dict {
    B_IndexedD_MappingD_dictG_class $class;
    B_Mapping W_Mapping;
    B_Eq W_EqD_AD_MappingB_dict;
    B_Hashable W_HashableD_AD_MappingB_dict;
};

struct B_IndexedD_MappingD_dictG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_IndexedD_MappingD_dict, B_Mapping, B_Eq);
    void (*__serialize__)(B_IndexedD_MappingD_dict,$Serial$state);
    B_IndexedD_MappingD_dict (*__deserialize__)(B_IndexedD_MappingD_dict,$Serial$state);
    B_bool (*__bool__)(B_IndexedD_MappingD_dict);
    B_str (*__str__)(B_IndexedD_MappingD_dict);
    B_str (*__repr__)(B_IndexedD_MappingD_dict);
    $WORD (*__getitem__)(B_IndexedD_MappingD_dict, B_dict, $WORD);
    void (*__setitem__)(B_IndexedD_MappingD_dict, B_dict, $WORD, $WORD);
    void (*__delitem__)(B_IndexedD_MappingD_dict, B_dict, $WORD);
};

void B_IndexedD_MappingD_dictD___init__ (B_IndexedD_MappingD_dict, B_Mapping, B_Eq);
void B_IndexedD_MappingD_dictD___serialize__(B_IndexedD_MappingD_dict, $Serial$state);
B_IndexedD_MappingD_dict B_IndexedD_MappingD_dictD___deserialize__(B_IndexedD_MappingD_dict, $Serial$state);
$WORD B_IndexedD_MappingD_dictD___getitem__ (B_IndexedD_MappingD_dict, B_dict, $WORD);
void B_IndexedD_MappingD_dictD___setitem__ (B_IndexedD_MappingD_dict, B_dict, $WORD, $WORD);
void B_IndexedD_MappingD_dictD___delitem__ (B_IndexedD_MappingD_dict, B_dict, $WORD);

extern struct B_IndexedD_MappingD_dictG_class B_IndexedD_MappingD_dictG_methods;
B_IndexedD_MappingD_dict B_IndexedD_MappingD_dictG_new(B_Mapping, B_Eq);

// B_OrdD_dict //////////////////////////////////////////////////////////////////////

struct B_OrdD_dict {
    B_OrdD_dictG_class $class;
    B_Eq W_EqD_AD_OrdD_dict;
    B_Hashable W_HashableD_AD_OrdD_dict;
    B_Eq W_EqD_BD_OrdD_dict;
};

struct B_OrdD_dictG_class {  
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_OrdD_dict, B_Hashable, B_Eq);
    void (*__serialize__)(B_OrdD_dict,$Serial$state);
    B_OrdD_dict (*__deserialize__)(B_OrdD_dict,$Serial$state);
    B_bool (*__bool__)(B_OrdD_dict);
    B_str (*__str__)(B_OrdD_dict);
    B_str (*__repr__)(B_OrdD_dict);
    B_bool (*__eq__)(B_OrdD_dict, B_dict, B_dict);
    B_bool (*__ne__)(B_OrdD_dict, B_dict, B_dict);
    B_bool (*__lt__)(B_OrdD_dict, B_dict, B_dict);
    B_bool (*__le__)(B_OrdD_dict, B_dict, B_dict);
    B_bool (*__gt__)(B_OrdD_dict, B_dict, B_dict);
    B_bool (*__ge__)(B_OrdD_dict, B_dict, B_dict);
};

void B_OrdD_dictD___init__ (B_OrdD_dict, B_Hashable, B_Eq);
void B_OrdD_dictD___serialize__(B_OrdD_dict, $Serial$state);
B_OrdD_dict B_OrdD_dictD___deserialize__(B_OrdD_dict, $Serial$state);
B_bool B_OrdD_dictD___eq__ (B_OrdD_dict, B_dict, B_dict);
B_bool B_OrdD_dictD___ne__ (B_OrdD_dict, B_dict, B_dict);
B_bool B_OrdD_dictD___lt__ (B_OrdD_dict, B_dict, B_dict);
B_bool B_OrdD_dictD___le__ (B_OrdD_dict, B_dict, B_dict);
B_bool B_OrdD_dictD___gt__ (B_OrdD_dict, B_dict, B_dict);
B_bool B_OrdD_dictD___ge__ (B_OrdD_dict, B_dict, B_dict);

extern struct B_OrdD_dictG_class B_OrdD_dictG_methods;
B_OrdD_dict B_OrdD_dictG_new(B_Hashable, B_Eq);

// B_SetD_set ////////////////////////////////////////////////////////////

struct B_SetD_set {
    B_SetD_setG_class $class;
    B_Ord W_Ord;
    B_Logical W_Logical;
    B_Minus W_Minus;
    B_Eq W_EqD_AD_SetB_set;
    B_Hashable W_HashableD_AD_SetB_set;
};

struct B_SetD_setG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_SetD_set, B_Hashable);
    void (*__serialize__)(B_SetD_set,$Serial$state);
    B_SetD_set (*__deserialize__)(B_SetD_set,$Serial$state);
    B_bool (*__bool__)(B_SetD_set);
    B_str (*__str__)(B_SetD_set);
    B_str (*__repr__)(B_SetD_set);
    B_Iterator (*__iter__)(B_SetD_set, B_set);
    B_set (*__fromiter__)(B_SetD_set, B_Iterable, $WORD);
    B_int (*__len__)(B_SetD_set, B_set);
    B_bool (*__contains__)(B_SetD_set, B_set, $WORD);
    B_bool (*__containsnot__)(B_SetD_set, B_set, $WORD);
    B_bool (*isdisjoint)(B_SetD_set, B_set, B_set);
    void (*add)(B_SetD_set, B_set, $WORD);
    void (*discard)(B_SetD_set, B_set, $WORD);
    $WORD (*pop)(B_SetD_set, B_set);
};

void B_SetD_setD___init__ (B_SetD_set, B_Hashable);
void B_SetD_setD___serialize__(B_SetD_set, $Serial$state);
B_SetD_set B_SetD_setD___deserialize__(B_SetD_set, $Serial$state);
B_Iterator B_SetD_setD___iter__ (B_SetD_set, B_set);
B_set B_SetD_setD___fromiter__ (B_SetD_set, B_Iterable, $WORD);
B_int B_SetD_setD___len__ (B_SetD_set, B_set);
B_bool B_SetD_setD___contains__ (B_SetD_set, B_set, $WORD);
B_bool B_SetD_setD___containsnot__ (B_SetD_set, B_set, $WORD);
B_bool B_SetD_set$isdisjoint (B_SetD_set, B_set, B_set);
void B_SetD_set$add (B_SetD_set, B_set, $WORD);
void B_SetD_set$discard (B_SetD_set, B_set, $WORD);
$WORD B_SetD_set$pop (B_SetD_set, B_set);

extern struct B_SetD_setG_class B_SetD_setG_methods;
B_SetD_set B_SetD_setG_new(B_Hashable);

// B_OrdD_SetD_set ////////////////////////////////////////////////////////////

struct B_OrdD_SetD_set {
    B_OrdD_SetD_setG_class $class;
    B_Set W_Set;
};

struct B_OrdD_SetD_setG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_OrdD_SetD_set, B_Set);
    void (*__serialize__)(B_OrdD_SetD_set,$Serial$state);
    B_OrdD_SetD_set (*__deserialize__)(B_OrdD_SetD_set,$Serial$state);
    B_bool (*__bool__)(B_OrdD_SetD_set);
    B_str (*__str__)(B_OrdD_SetD_set);
    B_str (*__repr__)(B_OrdD_SetD_set);
    B_bool (*__eq__)(B_OrdD_SetD_set, B_set, B_set);
    B_bool (*__ne__)(B_OrdD_SetD_set, B_set, B_set);
    B_bool (*__lt__)(B_OrdD_SetD_set, B_set, B_set);
    B_bool (*__le__)(B_OrdD_SetD_set, B_set, B_set);
    B_bool (*__gt__)(B_OrdD_SetD_set, B_set, B_set);
    B_bool (*__ge__)(B_OrdD_SetD_set, B_set, B_set);
};

void B_OrdD_SetD_setD___init__ (B_OrdD_SetD_set, B_Set);
void B_OrdD_SetD_setD___serialize__(B_OrdD_SetD_set, $Serial$state);
B_OrdD_SetD_set B_OrdD_SetD_setD___deserialize__(B_OrdD_SetD_set, $Serial$state);
B_bool B_OrdD_SetD_setD___eq__ (B_OrdD_SetD_set, B_set, B_set);
B_bool B_OrdD_SetD_setD___ne__ (B_OrdD_SetD_set, B_set, B_set);
B_bool B_OrdD_SetD_setD___lt__ (B_OrdD_SetD_set, B_set, B_set);
B_bool B_OrdD_SetD_setD___le__ (B_OrdD_SetD_set, B_set, B_set);
B_bool B_OrdD_SetD_setD___gt__ (B_OrdD_SetD_set, B_set, B_set);
B_bool B_OrdD_SetD_setD___ge__ (B_OrdD_SetD_set, B_set, B_set);

extern struct B_OrdD_SetD_setG_class B_OrdD_SetD_setG_methods;
B_OrdD_SetD_set B_OrdD_SetD_setG_new(B_SetD_set);

// B_LogicalD_SetD_set ///////////////////////////////////////////////////////

struct B_LogicalD_SetD_set {
    B_LogicalD_SetD_setG_class $class;
    B_Set W_Set;
};

struct B_LogicalD_SetD_setG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_LogicalD_SetD_set, B_Set);
    void (*__serialize__)(B_LogicalD_SetD_set,$Serial$state);
    B_LogicalD_SetD_set (*__deserialize__)(B_LogicalD_SetD_set,$Serial$state);
    B_bool (*__bool__)(B_LogicalD_SetD_set);
    B_str (*__str__)(B_LogicalD_SetD_set);
    B_str (*__repr__)(B_LogicalD_SetD_set);
    B_set (*__and__)(B_LogicalD_SetD_set, B_set, B_set);
    B_set (*__or__)(B_LogicalD_SetD_set, B_set, B_set);
    B_set (*__xor__)(B_LogicalD_SetD_set, B_set, B_set);
    B_set (*__iand__)(B_LogicalD_SetD_set, B_set, B_set);
    B_set (*__ior__)(B_LogicalD_SetD_set, B_set, B_set);
    B_set (*__ixor__)(B_LogicalD_SetD_set, B_set, B_set);
};

void B_LogicalD_SetD_setD___init__ (B_LogicalD_SetD_set, B_Set);
void B_LogicalD_SetD_setD___serialize__(B_LogicalD_SetD_set, $Serial$state);
B_LogicalD_SetD_set B_LogicalD_SetD_setD___deserialize__(B_LogicalD_SetD_set, $Serial$state);
B_set B_LogicalD_SetD_setD___and__ (B_LogicalD_SetD_set, B_set, B_set);
B_set B_LogicalD_SetD_setD___or__ (B_LogicalD_SetD_set, B_set, B_set);
B_set B_LogicalD_SetD_setD___xor__ (B_LogicalD_SetD_set, B_set, B_set);

extern struct B_LogicalD_SetD_setG_class B_LogicalD_SetD_setG_methods;
B_LogicalD_SetD_set B_LogicalD_SetD_setG_new(B_SetD_set);

// B_MinusD_SetD_set ////////////////////////////////////////////////////////////

struct B_MinusD_SetD_set {
    B_MinusD_SetD_setG_class $class;
    B_Set W_Set;
};

struct B_MinusD_SetD_setG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_MinusD_SetD_set, B_Set);
    void (*__serialize__)(B_MinusD_SetD_set,$Serial$state);
    B_MinusD_SetD_set (*__deserialize__)(B_MinusD_SetD_set,$Serial$state);
    B_bool (*__bool__)(B_MinusD_SetD_set);
    B_str (*__str__)(B_MinusD_SetD_set);
    B_str (*__repr__)(B_MinusD_SetD_set);
    B_set (*__sub__)(B_MinusD_SetD_set, B_set, B_set);
    B_set (*__isub__)(B_MinusD_SetD_set, B_set, B_set);
};

void B_MinusD_SetD_setD___init__ (B_MinusD_SetD_set, B_Set);
void B_MinusD_SetD_setD___serialize__(B_MinusD_SetD_set, $Serial$state);
B_MinusD_SetD_set B_MinusD_SetD_setD___deserialize__(B_MinusD_SetD_set, $Serial$state);
B_set B_MinusD_SetD_setD___sub__ (B_MinusD_SetD_set, B_set, B_set);

extern struct B_MinusD_SetD_setG_class B_MinusD_SetD_setG_methods;
B_MinusD_SetD_set B_MinusD_SetD_setG_new(B_SetD_set);

// B_IterableD_Iterator ////////////////////////////////////////////////////////////

struct B_IterableD_Iterator {
    B_IterableD_IteratorG_class $class;
};

struct B_IterableD_IteratorG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_IterableD_Iterator);
    void (*__serialize__)(B_IterableD_Iterator,$Serial$state);
    B_IterableD_Iterator (*__deserialize__)(B_IterableD_Iterator,$Serial$state);
    B_bool (*__bool__)(B_IterableD_Iterator);
    B_str (*__str__)(B_IterableD_Iterator);
    B_str (*__repr__)(B_IterableD_Iterator);
    B_Iterator (*__iter__)(B_IterableD_Iterator, B_Iterator);
};

void B_IterableD_IteratorD___init__ (B_IterableD_Iterator);
void B_IterableD_IteratorD___serialize__(B_IterableD_Iterator, $Serial$state);
B_IterableD_Iterator B_IterableD_IteratorD___deserialize__(B_IterableD_Iterator, $Serial$state);
B_Iterator B_IterableD_IteratorD___iter__ (B_IterableD_Iterator, B_Iterator);

extern struct B_IterableD_IteratorG_class B_IterableD_IteratorG_methods;
B_IterableD_Iterator B_IterableD_IteratorG_new();
 
// B_OrdD_str ////////////////////////////////////////////////////////////

struct B_OrdD_str {
    B_OrdD_strG_class $class;
};

struct B_OrdD_strG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_OrdD_str);
    void (*__serialize__)(B_OrdD_str,$Serial$state);
    B_OrdD_str (*__deserialize__)(B_OrdD_str,$Serial$state);
    B_bool (*__bool__)(B_OrdD_str);
    B_str (*__str__)(B_OrdD_str);
    B_str (*__repr__)(B_OrdD_str);
    B_bool (*__eq__)(B_OrdD_str, B_str, B_str);
    B_bool (*__ne__)(B_OrdD_str, B_str, B_str);
    B_bool (*__lt__)(B_OrdD_str, B_str, B_str);
    B_bool (*__le__)(B_OrdD_str, B_str, B_str);
    B_bool (*__gt__)(B_OrdD_str, B_str, B_str);
    B_bool (*__ge__)(B_OrdD_str, B_str, B_str);
};

void B_OrdD_strD___init__ (B_OrdD_str);
void B_OrdD_strD___serialize__(B_OrdD_str, $Serial$state);
B_OrdD_str B_OrdD_strD___deserialize__(B_OrdD_str, $Serial$state);
B_bool B_OrdD_strD___eq__ (B_OrdD_str, B_str, B_str);
B_bool B_OrdD_strD___ne__ (B_OrdD_str, B_str, B_str);
B_bool B_OrdD_strD___lt__ (B_OrdD_str, B_str, B_str);
B_bool B_OrdD_strD___le__ (B_OrdD_str, B_str, B_str);
B_bool B_OrdD_strD___gt__ (B_OrdD_str, B_str, B_str);
B_bool B_OrdD_strD___ge__ (B_OrdD_str, B_str, B_str);

extern struct B_OrdD_strG_class B_OrdD_strG_methods;
B_OrdD_str B_OrdD_strG_new();

// B_ContainerD_str ////////////////////////////////////////////////////////////

struct B_ContainerD_str {
    B_ContainerD_strG_class $class;
};

struct B_ContainerD_strG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_ContainerD_str);
    void (*__serialize__)(B_ContainerD_str,$Serial$state);
    B_ContainerD_str (*__deserialize__)(B_ContainerD_str,$Serial$state);
    B_bool (*__bool__)(B_ContainerD_str);
    B_str (*__str__)(B_ContainerD_str);
    B_str (*__repr__)(B_ContainerD_str);
    B_Iterator (*__iter__)(B_ContainerD_str, B_str);
    B_str (*__fromiter__)(B_ContainerD_str, B_Iterable, $WORD);
    B_int (*__len__)(B_ContainerD_str, B_str);
    B_bool (*__contains__)(B_ContainerD_str, B_str, B_str);
    B_bool (*__containsnot__)(B_ContainerD_str, B_str, B_str);
};

void B_ContainerD_strD___init__ (B_ContainerD_str);
void B_ContainerD_strD___serialize__(B_ContainerD_str, $Serial$state);
B_ContainerD_str B_ContainerD_strD___deserialize__(B_ContainerD_str, $Serial$state);
B_Iterator B_ContainerD_strD___iter__ (B_ContainerD_str, B_str);
B_int B_ContainerD_strD___len__ (B_ContainerD_str, B_str);
B_bool B_ContainerD_strD___contains__ (B_ContainerD_str, B_str, B_str);
B_bool B_ContainerD_strD___containsnot__ (B_ContainerD_str, B_str, B_str);

extern struct B_ContainerD_strG_class B_ContainerD_strG_methods;
B_ContainerD_str B_ContainerD_strG_new();

// B_SliceableD_str ////////////////////////////////////////////////////////////

struct B_SliceableD_str {
    B_SliceableD_strG_class $class;
};

struct B_SliceableD_strG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_SliceableD_str);
    void (*__serialize__)(B_SliceableD_str,$Serial$state);
    B_SliceableD_str (*__deserialize__)(B_SliceableD_str,$Serial$state);
    B_bool (*__bool__)(B_SliceableD_str);
    B_str (*__str__)(B_SliceableD_str);
    B_str (*__repr__)(B_SliceableD_str);
    B_str (*__getitem__)(B_SliceableD_str, B_str, B_int);
    void (*__setitem__)(B_SliceableD_str, B_str, B_int, B_str);
    void (*__delitem__)(B_SliceableD_str, B_str, B_int);
    B_str (*__getslice__)(B_SliceableD_str, B_str, B_slice);
    void (*__setslice__)(B_SliceableD_str, B_str, B_Iterable, B_slice, $WORD);
    void (*__delslice__)(B_SliceableD_str, B_str, B_slice);
};

void B_SliceableD_strD___init__ (B_SliceableD_str);
void B_SliceableD_strD___serialize__(B_SliceableD_str, $Serial$state);
B_SliceableD_str B_SliceableD_strD___deserialize__(B_SliceableD_str, $Serial$state);
B_str B_SliceableD_strD___getitem__ (B_SliceableD_str, B_str, B_int);
void B_SliceableD_strD___setitem__ (B_SliceableD_str, B_str, B_int, B_str);
void B_SliceableD_strD___delitem__ (B_SliceableD_str, B_str, B_int);
B_str B_SliceableD_strD___getslice__ (B_SliceableD_str, B_str, B_slice);
void B_SliceableD_strD___setslice__ (B_SliceableD_str, B_str, B_Iterable, B_slice, $WORD);
void B_SliceableD_strD___delslice__ (B_SliceableD_str, B_str, B_slice);

extern struct B_SliceableD_strG_class B_SliceableD_strG_methods;
B_SliceableD_str B_SliceableD_strG_new();

// B_TimesD_str ////////////////////////////////////////////////////////////

struct B_TimesD_str {
    B_TimesD_strG_class $class;
};

struct B_TimesD_strG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_TimesD_str);
    void (*__serialize__)(B_TimesD_str,$Serial$state);
    B_TimesD_str (*__deserialize__)(B_TimesD_str,$Serial$state);
    B_bool (*__bool__)(B_TimesD_str);
    B_str (*__str__)(B_TimesD_str);
    B_str (*__repr__)(B_TimesD_str);
    B_str (*__add__)(B_TimesD_str, B_str, B_str);
    B_str (*__iadd__)(B_TimesD_str, B_str, B_str);
    B_str (*__mul__)(B_TimesD_str, B_str, B_int);
    B_str (*__imul__)(B_TimesD_str, B_str, B_int);
};

void B_TimesD_strD___init__ (B_TimesD_str);
void B_TimesD_strD___serialize__(B_TimesD_str, $Serial$state);
B_TimesD_str B_TimesD_strD___deserialize__(B_TimesD_str, $Serial$state);
B_bool B_TimesD_strD___bool__(B_TimesD_str);
B_str B_TimesD_strD___str__(B_TimesD_str);
B_str B_TimesD_strD___add__ (B_TimesD_str, B_str, B_str);
B_str B_TimesD_strD___mul__(B_TimesD_str, B_str, B_int);

extern struct B_TimesD_strG_class B_TimesD_strG_methods;
B_TimesD_str B_TimesD_strG_new();

// B_HashableD_str ////////////////////////////////////////////////////////////

struct B_HashableD_str {
    B_HashableD_strG_class $class;
};

struct B_HashableD_strG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_HashableD_str);
    void (*__serialize__)(B_HashableD_str,$Serial$state);
    B_HashableD_str (*__deserialize__)(B_HashableD_str,$Serial$state);
    B_bool (*__bool__)(B_HashableD_str);
    B_str (*__str__)(B_HashableD_str);
    B_str (*__repr__)(B_HashableD_str);
    B_bool (*__eq__)(B_HashableD_str, B_str, B_str);
    B_bool (*__ne__)(B_HashableD_str, B_str, B_str);
    B_int (*__hash__)(B_HashableD_str, B_str);
};

void B_HashableD_strD___init__ (B_HashableD_str);
void B_HashableD_strD___serialize__(B_HashableD_str, $Serial$state);
B_HashableD_str B_HashableD_strD___deserialize__(B_HashableD_str, $Serial$state);
B_bool B_HashableD_strD___eq__ (B_HashableD_str, B_str, B_str);
B_bool B_HashableD_strD___ne__ (B_HashableD_str, B_str, B_str);
B_int B_HashableD_strD___hash__ (B_HashableD_str, B_str);

extern struct B_HashableD_strG_class B_HashableD_strG_methods;
B_HashableD_str B_HashableD_strG_new();

// B_IntegralD_i64 ////////////////////////////////////////////////////////////

struct B_IntegralD_i64 {
    B_IntegralD_i64G_class $class;
    B_Minus W_Minus;
    B_Logical W_Logical;
};

struct B_IntegralD_i64G_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_IntegralD_i64);
    void (*__serialize__)(B_IntegralD_i64,$Serial$state);
    B_IntegralD_i64 (*__deserialize__)(B_IntegralD_i64,$Serial$state);
    B_bool (*__bool__)(B_IntegralD_i64);
    B_str (*__str__)(B_IntegralD_i64);
    B_str (*__repr__)(B_IntegralD_i64);
    B_i64 (*__add__)(B_IntegralD_i64, B_i64, B_i64);
    B_i64 (*__iadd__)(B_IntegralD_i64, B_i64, B_i64);
    B_i64 (*__mul__)(B_IntegralD_i64, B_i64, B_i64);
    B_i64 (*__imul__)(B_IntegralD_i64, B_i64, B_i64);
    B_i64 (*__fromatom__)(B_IntegralD_i64,B_atom);
    B_complex (*__complx__)(B_IntegralD_i64, B_i64);
    //    B_i64 (*__truediv__)(B_IntegralD_i64, B_i64, B_i64);
    B_i64 (*__pow__)(B_IntegralD_i64, B_i64, B_i64);
    //    B_i64 (*__itruediv__)(B_IntegralD_i64, B_i64, B_i64);
    B_i64 (*__ipow__)(B_IntegralD_i64, B_i64, B_i64);
    B_i64 (*__neg__)(B_IntegralD_i64, B_i64);
    B_i64 (*__pos__)(B_IntegralD_i64, B_i64);
    $WORD (*real)(B_IntegralD_i64, B_i64, B_Real);
    $WORD (*imag)(B_IntegralD_i64, B_i64, B_Real);
    $WORD (*__abs__)(B_IntegralD_i64, B_i64, B_Real);
    B_i64 (*conjugate)(B_IntegralD_i64, B_i64);
    B_float (*__float__)(B_IntegralD_i64, B_i64);
    $WORD (*__trunc__)(B_IntegralD_i64, B_i64, B_Integral);
    $WORD (*__floor__)(B_IntegralD_i64, B_i64, B_Integral);
    $WORD (*__ceil__)(B_IntegralD_i64, B_i64, B_Integral);
    B_i64 (*__round__)(B_IntegralD_i64, B_i64, B_i64);
    $WORD (*numerator)(B_IntegralD_i64, B_i64, B_Integral);
    $WORD (*denominator)(B_IntegralD_i64, B_i64, B_Integral);
    B_i64 (*__int__)(B_IntegralD_i64, B_int);
    B_i64 (*__index__)(B_IntegralD_i64, B_int);
    B_tuple (*__divmod__)(B_IntegralD_i64, B_i64, B_i64);
    B_i64 (*__floordiv__)(B_IntegralD_i64, B_i64, B_i64);
    B_i64 (*__mod__)(B_IntegralD_i64, B_i64, B_i64);
    B_i64 (*__lshift__)(B_IntegralD_i64, B_i64, B_int);
    B_i64 (*__rshift__)(B_IntegralD_i64, B_i64, B_int);
    B_i64 (*__ifloordiv__)(B_IntegralD_i64, B_i64, B_i64);
    B_i64 (*__imod__)(B_IntegralD_i64, B_i64, B_i64);
    B_i64 (*__ilshift__)(B_IntegralD_i64, B_i64, B_int);
    B_i64 (*__irshift__)(B_IntegralD_i64, B_i64, B_int);
    B_i64 (*__invert__)(B_IntegralD_i64, B_i64);
};

void B_IntegralD_i64D___init__ (B_IntegralD_i64);
void B_IntegralD_i64D___serialize__(B_IntegralD_i64, $Serial$state);
B_IntegralD_i64 B_IntegralD_i64D___deserialize__(B_IntegralD_i64, $Serial$state);
B_i64 B_IntegralD_i64D___add__(B_IntegralD_i64, B_i64, B_i64);
B_i64 B_IntegralD_i64D___fromatom__(B_IntegralD_i64,B_atom);
B_complex B_IntegralD_i64D___complx__(B_IntegralD_i64, B_i64);
B_i64 B_IntegralD_i64D___mul__(B_IntegralD_i64, B_i64, B_i64);
//B_i64 B_IntegralD_i64D___truediv__(B_IntegralD_i64, B_i64, B_i64);
B_i64 B_IntegralD_i64D___pow__(B_IntegralD_i64, B_i64, B_i64);
B_i64 B_IntegralD_i64D___neg__(B_IntegralD_i64, B_i64);
B_i64 B_IntegralD_i64D___pos__(B_IntegralD_i64, B_i64);
$WORD B_IntegralD_i64$real(B_IntegralD_i64, B_i64, B_Real);
$WORD B_IntegralD_i64$imag(B_IntegralD_i64, B_i64, B_Real);
$WORD B_IntegralD_i64D___abs__(B_IntegralD_i64, B_i64, B_Real);
B_i64 B_IntegralD_i64$conjugate(B_IntegralD_i64, B_i64);
B_float B_IntegralD_i64D___float__ (B_IntegralD_i64, B_i64);
$WORD B_IntegralD_i64D___trunc__ (B_IntegralD_i64, B_i64, B_Integral);
$WORD B_IntegralD_i64D___floor__ (B_IntegralD_i64, B_i64, B_Integral);
$WORD B_IntegralD_i64D___ceil__ (B_IntegralD_i64, B_i64, B_Integral);
B_i64 B_IntegralD_i64D___round__ (B_IntegralD_i64, B_i64, B_i64);
$WORD B_IntegralD_i64$numerator (B_IntegralD_i64, B_i64, B_Integral);
$WORD B_IntegralD_i64$denominator (B_IntegralD_i64, B_i64, B_Integral);
B_i64 B_IntegralD_i64D___int__ (B_IntegralD_i64, B_int);
B_i64 B_IntegralD_i64D___index__ (B_IntegralD_i64, B_int);
B_tuple B_IntegralD_i64D___divmod__ (B_IntegralD_i64, B_i64, B_i64);
B_i64 B_IntegralD_i64D___floordiv__ (B_IntegralD_i64, B_i64, B_i64);
B_i64 B_IntegralD_i64D___mod__ (B_IntegralD_i64, B_i64, B_i64);
B_i64 B_IntegralD_i64D___lshift__ (B_IntegralD_i64, B_i64, B_int);
B_i64 B_IntegralD_i64D___rshift__ (B_IntegralD_i64, B_i64, B_int);
B_i64 B_IntegralD_i64D___invert__ (B_IntegralD_i64, B_i64);

extern struct B_IntegralD_i64G_class B_IntegralD_i64G_methods;
B_IntegralD_i64 B_IntegralD_i64G_new();

// B_LogicalD_IntegralD_i64 ////////////////////////////////////////////////////////////

struct B_LogicalD_IntegralD_i64 {
    B_LogicalD_IntegralD_i64G_class $class;
    B_Integral W_Integral;
};

struct B_LogicalD_IntegralD_i64G_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_LogicalD_IntegralD_i64, B_Integral);
    void (*__serialize__)(B_LogicalD_IntegralD_i64,$Serial$state);
    B_LogicalD_IntegralD_i64 (*__deserialize__)(B_LogicalD_IntegralD_i64,$Serial$state);
    B_bool (*__bool__)(B_LogicalD_IntegralD_i64);
    B_str (*__str__)(B_LogicalD_IntegralD_i64);
    B_str (*__repr__)(B_LogicalD_IntegralD_i64);
    B_i64 (*__and__)(B_LogicalD_IntegralD_i64, B_i64, B_i64);
    B_i64 (*__or__)(B_LogicalD_IntegralD_i64, B_i64, B_i64);
    B_i64 (*__xor__)(B_LogicalD_IntegralD_i64, B_i64, B_i64);
    B_i64 (*__iand__)(B_LogicalD_IntegralD_i64, B_i64, B_i64);
    B_i64 (*__ior__)(B_LogicalD_IntegralD_i64, B_i64, B_i64);
    B_i64 (*__ixor__)(B_LogicalD_IntegralD_i64, B_i64, B_i64);
};

void B_LogicalD_IntegralD_i64D___init__ (B_LogicalD_IntegralD_i64, B_Integral);
void B_LogicalD_IntegralD_i64D___serialize__(B_LogicalD_IntegralD_i64, $Serial$state);
B_LogicalD_IntegralD_i64 B_LogicalD_IntegralD_i64D___deserialize__(B_LogicalD_IntegralD_i64, $Serial$state);
B_i64 B_LogicalD_IntegralD_i64D___and__ (B_LogicalD_IntegralD_i64, B_i64, B_i64);
B_i64 B_LogicalD_IntegralD_i64D___or__ (B_LogicalD_IntegralD_i64, B_i64, B_i64);
B_i64 B_LogicalD_IntegralD_i64D___xor__ (B_LogicalD_IntegralD_i64, B_i64, B_i64);

extern struct B_LogicalD_IntegralD_i64G_class B_LogicalD_IntegralD_i64G_methods;
B_LogicalD_IntegralD_i64 B_LogicalD_IntegralD_i64G_new(B_Integral);

// B_MinusD_IntegralD_i64 ////////////////////////////////////////////////////////////

struct B_MinusD_IntegralD_i64 {
    B_MinusD_IntegralD_i64G_class $class;
    B_Integral W_Integral;
};

struct B_MinusD_IntegralD_i64G_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_MinusD_IntegralD_i64, B_Integral);
    void (*__serialize__)(B_MinusD_IntegralD_i64,$Serial$state);
    B_MinusD_IntegralD_i64 (*__deserialize__)(B_MinusD_IntegralD_i64,$Serial$state);
    B_bool (*__bool__)(B_MinusD_IntegralD_i64);
    B_str (*__str__)(B_MinusD_IntegralD_i64);
    B_str (*__repr__)(B_MinusD_IntegralD_i64);
    B_i64 (*__sub__)(B_MinusD_IntegralD_i64, B_i64, B_i64);
    B_i64 (*__isub__)(B_MinusD_IntegralD_i64, B_i64, B_i64);
};

void B_MinusD_IntegralD_i64D___init__ (B_MinusD_IntegralD_i64, B_Integral);
void B_MinusD_IntegralD_i64D___serialize__(B_MinusD_IntegralD_i64, $Serial$state);
B_MinusD_IntegralD_i64 B_MinusD_IntegralD_i64D___deserialize__(B_MinusD_IntegralD_i64, $Serial$state);
B_i64 B_MinusD_IntegralD_i64D___sub__ (B_MinusD_IntegralD_i64, B_i64, B_i64);

extern struct B_MinusD_IntegralD_i64G_class B_MinusD_IntegralD_i64G_methods;
B_MinusD_IntegralD_i64 B_MinusD_IntegralD_i64G_new(B_Integral);

// B_DivD_i64 ////////////////////////////////////////////////////////////

struct B_DivD_i64 {
    B_DivD_i64G_class $class;
};

struct B_DivD_i64G_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_DivD_i64);
    void (*__serialize__)(B_DivD_i64,$Serial$state);
    B_DivD_i64 (*__deserialize__)(B_DivD_i64,$Serial$state);
    B_bool (*__bool__)(B_DivD_i64);
    B_str (*__str__)(B_DivD_i64);
    B_str (*__repr__)(B_DivD_i64);
    B_float (*__truediv__)(B_DivD_i64, B_i64, B_i64);
    B_float (*__itruediv__)(B_DivD_i64, B_i64, B_i64);
};

B_float B_DivD_i64D___truediv__ (B_DivD_i64, B_i64, B_i64);
B_float B_DivD_i64D___itruediv__ (B_DivD_i64, B_i64, B_i64);

extern struct B_DivD_i64G_class B_DivD_i64G_methods;
B_DivD_i64 B_DivD_i64G_new();

// B_OrdD_i64 /////////////////////////////////////////////////////////////////

struct B_OrdD_i64 {
    B_OrdD_i64G_class $class;
};

struct B_OrdD_i64G_class {  
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_OrdD_i64);
    void (*__serialize__)(B_OrdD_i64,$Serial$state);
    B_OrdD_i64 (*__deserialize__)(B_OrdD_i64,$Serial$state);
    B_bool (*__bool__)(B_OrdD_i64);
    B_str (*__str__)(B_OrdD_i64);
    B_str (*__repr__)(B_OrdD_i64);
    B_bool (*__eq__)(B_OrdD_i64, B_i64, B_i64);
    B_bool (*__ne__)(B_OrdD_i64, B_i64, B_i64);
    B_bool (*__lt__)(B_OrdD_i64, B_i64, B_i64);
    B_bool (*__le__)(B_OrdD_i64, B_i64, B_i64);
    B_bool (*__gt__)(B_OrdD_i64, B_i64, B_i64);
    B_bool (*__ge__)(B_OrdD_i64, B_i64, B_i64);
};

void B_OrdD_i64D___init__ (B_OrdD_i64);
void B_OrdD_i64D___serialize__(B_OrdD_i64, $Serial$state);
B_OrdD_i64 B_OrdD_i64D___deserialize__(B_OrdD_i64, $Serial$state);
B_bool B_OrdD_i64D___eq__ (B_OrdD_i64, B_i64, B_i64);
B_bool B_OrdD_i64D___ne__ (B_OrdD_i64, B_i64, B_i64);
B_bool B_OrdD_i64D___lt__ (B_OrdD_i64, B_i64, B_i64);
B_bool B_OrdD_i64D___le__ (B_OrdD_i64, B_i64, B_i64);
B_bool B_OrdD_i64D___gt__ (B_OrdD_i64, B_i64, B_i64);
B_bool B_OrdD_i64D___ge__ (B_OrdD_i64, B_i64, B_i64);

extern struct B_OrdD_i64G_class B_OrdD_i64G_methods;
B_OrdD_i64 B_OrdD_i64G_new();  

// B_HashableD_i64 ////////////////////////////////////////////////////////////

struct B_HashableD_i64 {
    B_HashableD_i64G_class $class;
};

struct B_HashableD_i64G_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_HashableD_i64);
    void (*__serialize__)(B_HashableD_i64,$Serial$state);
    B_HashableD_i64 (*__deserialize__)(B_HashableD_i64,$Serial$state);
    B_bool (*__bool__)(B_HashableD_i64);
    B_str (*__str__)(B_HashableD_i64);
    B_str (*__repr__)(B_HashableD_i64);
    B_bool (*__eq__)(B_HashableD_i64, B_i64, B_i64);
    B_bool (*__ne__)(B_HashableD_i64, B_i64, B_i64);
    B_int (*__hash__)(B_HashableD_i64, B_i64);
};

void B_HashableD_i64D___init__ (B_HashableD_i64);
void B_HashableD_i64D___serialize__(B_HashableD_i64, $Serial$state);
B_HashableD_i64 B_HashableD_i64D___deserialize__(B_HashableD_i64, $Serial$state);
B_bool B_HashableD_i64D___eq__ (B_HashableD_i64, B_i64, B_i64);
B_bool B_HashableD_i64D___ne__ (B_HashableD_i64, B_i64, B_i64);
B_int B_HashableD_i64D___hash__ (B_HashableD_i64, B_i64);

extern struct B_HashableD_i64G_class B_HashableD_i64G_methods;
B_HashableD_i64 B_HashableD_i64G_new();

// B_IntegralD_int ////////////////////////////////////////////////////////////

struct B_IntegralD_int {
    B_IntegralD_intG_class $class;
    B_Minus W_Minus;
    B_Logical W_Logical;
};

struct B_IntegralD_intG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_IntegralD_int);
    void (*__serialize__)(B_IntegralD_int,$Serial$state);
    B_IntegralD_int (*__deserialize__)(B_IntegralD_int,$Serial$state);
    B_bool (*__bool__)(B_IntegralD_int);
    B_str (*__str__)(B_IntegralD_int);
    B_str (*__repr__)(B_IntegralD_int);
    B_int (*__add__)(B_IntegralD_int, B_int, B_int);
    B_int (*__iadd__)(B_IntegralD_int, B_int, B_int);
    B_int (*__mul__)(B_IntegralD_int, B_int, B_int);
    B_int (*__imul__)(B_IntegralD_int, B_int, B_int);
    B_int (*__fromatom__)(B_IntegralD_int,B_atom);
    B_complex (*__complx__)(B_IntegralD_int, B_int);
    //    B_int (*__truediv__)(B_IntegralD_int, B_int, B_int);
    B_int (*__pow__)(B_IntegralD_int, B_int, B_int);
    //    B_int (*__itruediv__)(B_IntegralD_int, B_int, B_int);
    B_int (*__ipow__)(B_IntegralD_int, B_int, B_int);
    B_int (*__neg__)(B_IntegralD_int, B_int);
    B_int (*__pos__)(B_IntegralD_int, B_int);
    $WORD (*real)(B_IntegralD_int, B_int, B_Real);
    $WORD (*imag)(B_IntegralD_int, B_int, B_Real);
    $WORD (*__abs__)(B_IntegralD_int, B_int, B_Real);
    B_int (*conjugate)(B_IntegralD_int, B_int);
    B_float (*__float__)(B_IntegralD_int, B_int);
    $WORD (*__trunc__)(B_IntegralD_int, B_int, B_Integral);
    $WORD (*__floor__)(B_IntegralD_int, B_int, B_Integral);
    $WORD (*__ceil__)(B_IntegralD_int, B_int, B_Integral);
    B_int (*__round__)(B_IntegralD_int, B_int, B_int);
    $WORD (*numerator)(B_IntegralD_int, B_int, B_Integral);
    $WORD (*denominator)(B_IntegralD_int, B_int, B_Integral);
    B_int (*__int__)(B_IntegralD_int, B_int);
    B_int (*__index__)(B_IntegralD_int, B_int);
    B_tuple (*__divmod__)(B_IntegralD_int, B_int, B_int);
    B_int (*__floordiv__)(B_IntegralD_int, B_int, B_int);
    B_int (*__mod__)(B_IntegralD_int, B_int, B_int);
    B_int (*__lshift__)(B_IntegralD_int, B_int, B_int);
    B_int (*__rshift__)(B_IntegralD_int, B_int, B_int);
    B_int (*__ifloordiv__)(B_IntegralD_int, B_int, B_int);
    B_int (*__imod__)(B_IntegralD_int, B_int, B_int);
    B_int (*__ilshift__)(B_IntegralD_int, B_int, B_int);
    B_int (*__irshift__)(B_IntegralD_int, B_int, B_int);
    B_int (*__invert__)(B_IntegralD_int, B_int);
};

void B_IntegralD_intD___init__ (B_IntegralD_int);
void B_IntegralD_intD___serialize__(B_IntegralD_int, $Serial$state);
B_IntegralD_int B_IntegralD_intD___deserialize__(B_IntegralD_int, $Serial$state);
B_int B_IntegralD_intD___add__(B_IntegralD_int, B_int, B_int);
B_int B_IntegralD_intD___fromatom__(B_IntegralD_int,B_atom);
B_complex B_IntegralD_intD___complx__(B_IntegralD_int, B_int);
B_int B_IntegralD_intD___mul__(B_IntegralD_int, B_int, B_int);
//B_int B_IntegralD_intD___truediv__(B_IntegralD_int, B_int, B_int);
B_int B_IntegralD_intD___pow__(B_IntegralD_int, B_int, B_int);
B_int B_IntegralD_intD___neg__(B_IntegralD_int, B_int);
B_int B_IntegralD_intD___pos__(B_IntegralD_int, B_int);
$WORD B_IntegralD_int$real(B_IntegralD_int, B_int, B_Real);
$WORD B_IntegralD_int$imag(B_IntegralD_int, B_int, B_Real);
$WORD B_IntegralD_intD___abs__(B_IntegralD_int, B_int, B_Real);
B_int B_IntegralD_int$conjugate(B_IntegralD_int, B_int);
B_float B_IntegralD_intD___float__ (B_IntegralD_int, B_int);
$WORD B_IntegralD_intD___trunc__ (B_IntegralD_int, B_int, B_Integral);
$WORD B_IntegralD_intD___floor__ (B_IntegralD_int, B_int, B_Integral);
$WORD B_IntegralD_intD___ceil__ (B_IntegralD_int, B_int, B_Integral);
B_int B_IntegralD_intD___round__ (B_IntegralD_int, B_int, B_int);
$WORD B_IntegralD_int$numerator (B_IntegralD_int, B_int, B_Integral);
$WORD B_IntegralD_int$denominator (B_IntegralD_int, B_int, B_Integral);
B_int B_IntegralD_intD___int__ (B_IntegralD_int, B_int);
B_int B_IntegralD_intD___index__ (B_IntegralD_int, B_int);
B_tuple B_IntegralD_intD___divmod__ (B_IntegralD_int, B_int, B_int);
B_int B_IntegralD_intD___floordiv__ (B_IntegralD_int, B_int, B_int);
B_int B_IntegralD_intD___mod__ (B_IntegralD_int, B_int, B_int);
B_int B_IntegralD_intD___lshift__ (B_IntegralD_int, B_int, B_int);
B_int B_IntegralD_intD___rshift__ (B_IntegralD_int, B_int, B_int);
B_int B_IntegralD_intD___invert__ (B_IntegralD_int, B_int);

extern struct B_IntegralD_intG_class B_IntegralD_intG_methods;
B_IntegralD_int B_IntegralD_intG_new();

// B_LogicalD_IntegralD_int ////////////////////////////////////////////////////////////

struct B_LogicalD_IntegralD_int {
    B_LogicalD_IntegralD_intG_class $class;
    B_Integral W_Integral;
};

struct B_LogicalD_IntegralD_intG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_LogicalD_IntegralD_int, B_Integral);
    void (*__serialize__)(B_LogicalD_IntegralD_int,$Serial$state);
    B_LogicalD_IntegralD_int (*__deserialize__)(B_LogicalD_IntegralD_int,$Serial$state);
    B_bool (*__bool__)(B_LogicalD_IntegralD_int);
    B_str (*__str__)(B_LogicalD_IntegralD_int);
    B_str (*__repr__)(B_LogicalD_IntegralD_int);
    B_int (*__and__)(B_LogicalD_IntegralD_int, B_int, B_int);
    B_int (*__or__)(B_LogicalD_IntegralD_int, B_int, B_int);
    B_int (*__xor__)(B_LogicalD_IntegralD_int, B_int, B_int);
    B_int (*__iand__)(B_LogicalD_IntegralD_int, B_int, B_int);
    B_int (*__ior__)(B_LogicalD_IntegralD_int, B_int, B_int);
    B_int (*__ixor__)(B_LogicalD_IntegralD_int, B_int, B_int);
};

void B_LogicalD_IntegralD_intD___init__ (B_LogicalD_IntegralD_int, B_Integral);
void B_LogicalD_IntegralD_intD___serialize__(B_LogicalD_IntegralD_int, $Serial$state);
B_LogicalD_IntegralD_int B_LogicalD_IntegralD_intD___deserialize__(B_LogicalD_IntegralD_int, $Serial$state);
B_int B_LogicalD_IntegralD_intD___and__ (B_LogicalD_IntegralD_int, B_int, B_int);
B_int B_LogicalD_IntegralD_intD___or__ (B_LogicalD_IntegralD_int, B_int, B_int);
B_int B_LogicalD_IntegralD_intD___xor__ (B_LogicalD_IntegralD_int, B_int, B_int);

extern struct B_LogicalD_IntegralD_intG_class B_LogicalD_IntegralD_intG_methods;
B_LogicalD_IntegralD_int B_LogicalD_IntegralD_intG_new(B_Integral);

// B_MinusD_IntegralD_int ////////////////////////////////////////////////////////////

struct B_MinusD_IntegralD_int {
    B_MinusD_IntegralD_intG_class $class;
    B_Integral W_Integral;
};

struct B_MinusD_IntegralD_intG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_MinusD_IntegralD_int, B_Integral);
    void (*__serialize__)(B_MinusD_IntegralD_int,$Serial$state);
    B_MinusD_IntegralD_int (*__deserialize__)(B_MinusD_IntegralD_int,$Serial$state);
    B_bool (*__bool__)(B_MinusD_IntegralD_int);
    B_str (*__str__)(B_MinusD_IntegralD_int);
    B_str (*__repr__)(B_MinusD_IntegralD_int);
    B_int (*__sub__)(B_MinusD_IntegralD_int, B_int, B_int);
    B_int (*__isub__)(B_MinusD_IntegralD_int, B_int, B_int);
};

void B_MinusD_IntegralD_intD___init__ (B_MinusD_IntegralD_int, B_Integral);
void B_MinusD_IntegralD_intD___serialize__(B_MinusD_IntegralD_int, $Serial$state);
B_MinusD_IntegralD_int B_MinusD_IntegralD_intD___deserialize__(B_MinusD_IntegralD_int, $Serial$state);
B_int B_MinusD_IntegralD_intD___sub__ (B_MinusD_IntegralD_int, B_int, B_int);

extern struct B_MinusD_IntegralD_intG_class B_MinusD_IntegralD_intG_methods;
B_MinusD_IntegralD_int B_MinusD_IntegralD_intG_new(B_Integral);

// B_DivD_int ////////////////////////////////////////////////////////////

struct B_DivD_int {
    B_DivD_intG_class $class;
};

struct B_DivD_intG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_DivD_int);
    void (*__serialize__)(B_DivD_int,$Serial$state);
    B_DivD_int (*__deserialize__)(B_DivD_int,$Serial$state);
    B_bool (*__bool__)(B_DivD_int);
    B_str (*__str__)(B_DivD_int);
    B_str (*__repr__)(B_DivD_int);
    B_float (*__truediv__)(B_DivD_int, B_int, B_int);
    B_float (*__itruediv__)(B_DivD_int, B_int, B_int);
};

B_float B_DivD_intD___truediv__ (B_DivD_int, B_int, B_int);
B_float B_DivD_intD___itruediv__ (B_DivD_int, B_int, B_int);

extern struct B_DivD_intG_class B_DivD_intG_methods;
B_DivD_int B_DivD_intG_new();

// B_OrdD_int /////////////////////////////////////////////////////////////////

struct B_OrdD_int {
    B_OrdD_intG_class $class;
};

struct B_OrdD_intG_class {  
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_OrdD_int);
    void (*__serialize__)(B_OrdD_int,$Serial$state);
    B_OrdD_int (*__deserialize__)(B_OrdD_int,$Serial$state);
    B_bool (*__bool__)(B_OrdD_int);
    B_str (*__str__)(B_OrdD_int);
    B_str (*__repr__)(B_OrdD_int);
    B_bool (*__eq__)(B_OrdD_int, B_int, B_int);
    B_bool (*__ne__)(B_OrdD_int, B_int, B_int);
    B_bool (*__lt__)(B_OrdD_int, B_int, B_int);
    B_bool (*__le__)(B_OrdD_int, B_int, B_int);
    B_bool (*__gt__)(B_OrdD_int, B_int, B_int);
    B_bool (*__ge__)(B_OrdD_int, B_int, B_int);
};

void B_OrdD_intD___init__ (B_OrdD_int);
void B_OrdD_intD___serialize__(B_OrdD_int, $Serial$state);
B_OrdD_int B_OrdD_intD___deserialize__(B_OrdD_int, $Serial$state);
B_bool B_OrdD_intD___eq__ (B_OrdD_int, B_int, B_int);
B_bool B_OrdD_intD___ne__ (B_OrdD_int, B_int, B_int);
B_bool B_OrdD_intD___lt__ (B_OrdD_int, B_int, B_int);
B_bool B_OrdD_intD___le__ (B_OrdD_int, B_int, B_int);
B_bool B_OrdD_intD___gt__ (B_OrdD_int, B_int, B_int);
B_bool B_OrdD_intD___ge__ (B_OrdD_int, B_int, B_int);

extern struct B_OrdD_intG_class B_OrdD_intG_methods;
B_OrdD_int B_OrdD_intG_new();  

// B_HashableD_int ////////////////////////////////////////////////////////////

struct B_HashableD_int {
    B_HashableD_intG_class $class;
};

struct B_HashableD_intG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_HashableD_int);
    void (*__serialize__)(B_HashableD_int,$Serial$state);
    B_HashableD_int (*__deserialize__)(B_HashableD_int,$Serial$state);
    B_bool (*__bool__)(B_HashableD_int);
    B_str (*__str__)(B_HashableD_int);
    B_str (*__repr__)(B_HashableD_int);
    B_bool (*__eq__)(B_HashableD_int, B_int, B_int);
    B_bool (*__ne__)(B_HashableD_int, B_int, B_int);
    B_int (*__hash__)(B_HashableD_int, B_int);
};

void B_HashableD_intD___init__ (B_HashableD_int);
void B_HashableD_intD___serialize__(B_HashableD_int, $Serial$state);
B_HashableD_int B_HashableD_intD___deserialize__(B_HashableD_int, $Serial$state);
B_bool B_HashableD_intD___eq__ (B_HashableD_int, B_int, B_int);
B_bool B_HashableD_intD___ne__ (B_HashableD_int, B_int, B_int);
B_int B_HashableD_intD___hash__ (B_HashableD_int, B_int);

extern struct B_HashableD_intG_class B_HashableD_intG_methods;
B_HashableD_int B_HashableD_intG_new();

// B_RealD_float ////////////////////////////////////////////////////////////

struct B_RealD_float {
    B_RealD_floatG_class $class;
    B_Minus W_Minus;
};

struct B_RealD_floatG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_RealD_float);
    void (*__serialize__)(B_RealD_float,$Serial$state);
    B_RealD_float (*__deserialize__)(B_RealD_float,$Serial$state);
    B_bool (*__bool__)(B_RealD_float);
    B_str (*__str__)(B_RealD_float);
    B_str (*__repr__)(B_RealD_float);
    B_float (*__add__)(B_RealD_float, B_float, B_float);
    B_float (*__iadd__)(B_RealD_float, B_float, B_float);
    B_float (*__mul__)(B_RealD_float, B_float, B_float);
    B_float (*__imul__)(B_RealD_float, B_float, B_float);
    B_float (*__fromatom__)(B_RealD_float,B_atom);
    B_complex (*__complx__)(B_RealD_float, B_float);
    //    B_float (*__truediv__)(B_RealD_float, B_float, B_float);
    B_float (*__pow__)(B_RealD_float, B_float, B_float);
    //    B_float (*__itruediv__)(B_RealD_float, B_float, B_float);
    B_float (*__ipow__)(B_RealD_float, B_float, B_float);
    B_float (*__neg__)(B_RealD_float, B_float);
    B_float (*__pos__)(B_RealD_float, B_float);
    $WORD (*real)(B_RealD_float, B_float, B_Real);
    $WORD (*imag)(B_RealD_float, B_float, B_Real);
    $WORD (*__abs__)(B_RealD_float, B_float, B_Real);
    B_float (*conjugate)(B_RealD_float, B_float);
    B_float (*__float__)(B_RealD_float, B_float);
    $WORD (*__trunc__)(B_RealD_float, B_float, B_Integral);
    $WORD (*__floor__)(B_RealD_float, B_float, B_Integral);
    $WORD (*__ceil__)(B_RealD_float, B_float, B_Integral);
    B_float (*__round__)(B_RealD_float, B_float, B_int);
};

void B_RealD_floatD___init__ (B_RealD_float);
void B_RealD_floatD___serialize__(B_RealD_float, $Serial$state);
B_RealD_float B_RealD_floatD___deserialize__(B_RealD_float, $Serial$state);
B_float B_RealD_floatD___add__(B_RealD_float, B_float, B_float);
B_float B_RealD_floatD___fromatom__(B_RealD_float,B_atom);
B_complex B_RealD_floatD___complx__(B_RealD_float, B_float);
B_float B_RealD_floatD___mul__(B_RealD_float, B_float, B_float);
//B_float B_RealD_floatD___truediv__(B_RealD_float, B_float, B_float);
B_float B_RealD_floatD___pow__(B_RealD_float, B_float, B_float);
B_float B_RealD_floatD___neg__(B_RealD_float, B_float);
B_float B_RealD_floatD___pos__(B_RealD_float, B_float);
$WORD B_RealD_float$real(B_RealD_float, B_float, B_Real);
$WORD B_RealD_float$imag(B_RealD_float, B_float, B_Real);
$WORD B_RealD_floatD___abs__(B_RealD_float, B_float, B_Real);
B_float B_RealD_float$conjugate(B_RealD_float, B_float);
B_float B_RealD_floatD___float__ (B_RealD_float, B_float);
$WORD B_RealD_floatD___trunc__ (B_RealD_float, B_float, B_Integral);
$WORD B_RealD_floatD___floor__ (B_RealD_float, B_float, B_Integral);
$WORD B_RealD_floatD___ceil__ (B_RealD_float, B_float, B_Integral);
B_float B_RealD_floatD___round__ (B_RealD_float, B_float, B_int);

extern struct B_RealD_floatG_class B_RealD_floatG_methods;
B_RealD_float B_RealD_floatG_new();

// B_MinusD_RealD_float /////////////////////////////////////////////////////

struct B_MinusD_RealD_float {
    B_MinusD_RealD_floatG_class $class;
    B_Real W_Real;
};

struct B_MinusD_RealD_floatG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_MinusD_RealD_float, B_Real);
    void (*__serialize__)(B_MinusD_RealD_float,$Serial$state);
    B_MinusD_RealD_float (*__deserialize__)(B_MinusD_RealD_float,$Serial$state);
    B_bool (*__bool__)(B_MinusD_RealD_float);
    B_str (*__str__)(B_MinusD_RealD_float);
    B_str (*__repr__)(B_MinusD_RealD_float);
    B_float (*__sub__)(B_MinusD_RealD_float, B_float, B_float);
    B_float (*__isub__)(B_MinusD_RealD_float, B_float, B_float);
};

void B_MinusD_RealD_floatD___init__ (B_MinusD_RealD_float, B_Real);
void B_MinusD_RealD_floatD___serialize__(B_MinusD_RealD_float, $Serial$state);
B_MinusD_RealD_float B_MinusD_RealD_floatD___deserialize__(B_MinusD_RealD_float, $Serial$state);
B_float B_MinusD_RealD_floatD___sub__ (B_MinusD_RealD_float, B_float, B_float);

extern struct B_MinusD_RealD_floatG_class B_MinusD_RealD_floatG_methods;
B_MinusD_RealD_float B_MinusD_RealD_floatG_new(B_Real);

// B_DivD_float ////////////////////////////////////////////////////////////

struct B_DivD_float {
    B_DivD_floatG_class $class;
};

struct B_DivD_floatG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_DivD_float);
    void (*__serialize__)(B_DivD_float,$Serial$state);
    B_DivD_float (*__deserialize__)(B_DivD_float,$Serial$state);
    B_bool (*__bool__)(B_DivD_float);
    B_str (*__str__)(B_DivD_float);
    B_str (*__repr__)(B_DivD_float);
    B_float (*__truediv__)(B_DivD_float, B_float, B_float);
    B_float (*__itruediv__)(B_DivD_float, B_float, B_float);
};

B_float B_DivD_floatD___truediv__ (B_DivD_float, B_float, B_float);
B_float B_DivD_floatD___itruediv__ (B_DivD_float, B_float, B_float);

extern struct B_DivD_floatG_class B_DivD_floatG_methods;
B_DivD_float B_DivD_floatG_new();

// B_OrdD_float /////////////////////////////////////////////////////////////////

struct B_OrdD_float {
    B_OrdD_floatG_class $class;
};

struct B_OrdD_floatG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_OrdD_float);
    void (*__serialize__)(B_OrdD_float,$Serial$state);
    B_OrdD_float (*__deserialize__)(B_OrdD_float,$Serial$state);
    B_bool (*__bool__)(B_OrdD_float);
    B_str (*__str__)(B_OrdD_float);
    B_str (*__repr__)(B_OrdD_float);
    B_bool (*__eq__)(B_OrdD_float, B_float, B_float);
    B_bool (*__ne__)(B_OrdD_float, B_float, B_float);
    B_bool (*__lt__)(B_OrdD_float, B_float, B_float);
    B_bool (*__le__)(B_OrdD_float, B_float, B_float);
    B_bool (*__gt__)(B_OrdD_float, B_float, B_float);
    B_bool (*__ge__)(B_OrdD_float, B_float, B_float);
};

void B_OrdD_floatD___init__ (B_OrdD_float);
void B_OrdD_floatD___serialize__(B_OrdD_float, $Serial$state);
B_OrdD_float B_OrdD_floatD___deserialize__(B_OrdD_float, $Serial$state);
B_bool B_OrdD_floatD___eq__ (B_OrdD_float, B_float, B_float);
B_bool B_OrdD_floatD___ne__ (B_OrdD_float, B_float, B_float);
B_bool B_OrdD_floatD___lt__ (B_OrdD_float, B_float, B_float);
B_bool B_OrdD_floatD___le__ (B_OrdD_float, B_float, B_float);
B_bool B_OrdD_floatD___gt__ (B_OrdD_float, B_float, B_float);
B_bool B_OrdD_floatD___ge__ (B_OrdD_float, B_float, B_float);

extern struct B_OrdD_floatG_class B_OrdD_floatG_methods;
B_OrdD_float B_OrdD_floatG_new();

// B_HashableD_float ////////////////////////////////////////////////////////////

struct B_HashableD_float {
    B_HashableD_floatG_class $class;
};

struct B_HashableD_floatG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_HashableD_float);
    void (*__serialize__)(B_HashableD_float,$Serial$state);
    B_HashableD_float (*__deserialize__)(B_HashableD_float,$Serial$state);
    B_bool (*__bool__)(B_HashableD_float);
    B_str (*__str__)(B_HashableD_float);
    B_str (*__repr__)(B_HashableD_float);
    B_bool (*__eq__)(B_HashableD_float, B_float, B_float);
    B_bool (*__ne__)(B_HashableD_float, B_float, B_float);
    B_int (*__hash__)(B_HashableD_float, B_float);
};

void B_HashableD_floatD___init__ (B_HashableD_float);
void B_HashableD_floatD___serialize__(B_HashableD_float, $Serial$state);
B_HashableD_float B_HashableD_floatD___deserialize__(B_HashableD_float, $Serial$state);
B_bool B_HashableD_floatD___eq__ (B_HashableD_float, B_float, B_float);
B_bool B_HashableD_floatD___ne__ (B_HashableD_float, B_float, B_float);
B_int B_HashableD_floatD___hash__ (B_HashableD_float, B_float);

extern struct B_HashableD_floatG_class B_HashableD_floatG_methods;
B_HashableD_float B_HashableD_floatG_new();

// B_NumberD_complex ////////////////////////////////////////////////////////////

struct B_NumberD_complex {
    B_NumberD_complexG_class $class;
    B_Minus W_Minus;
};

struct B_NumberD_complexG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_NumberD_complex);
    void (*__serialize__)(B_NumberD_complex,$Serial$state);
    B_NumberD_complex (*__deserialize__)(B_NumberD_complex,$Serial$state);
    B_bool (*__bool__)(B_NumberD_complex);
    B_str (*__str__)(B_NumberD_complex);
    B_str (*__repr__)(B_NumberD_complex);
    B_complex (*__add__)(B_NumberD_complex, B_complex, B_complex);
    B_complex (*__iadd__)(B_NumberD_complex, B_complex, B_complex);
    B_complex (*__mul__)(B_NumberD_complex, B_complex, B_complex);
    B_complex (*__imul__)(B_NumberD_complex, B_complex, B_complex);
    B_complex (*__fromatom__)(B_NumberD_complex,B_atom);
    B_complex (*__complx__)(B_NumberD_complex, B_complex);
    //    B_complex (*__truediv__)(B_NumberD_complex, B_complex, B_complex);
    B_complex (*__pow__)(B_NumberD_complex, B_complex, B_complex);
    //    B_complex (*__itruediv__)(B_NumberD_complex, B_complex, B_complex);
    B_complex (*__ipow__)(B_NumberD_complex, B_complex, B_complex);
    B_complex (*__neg__)(B_NumberD_complex, B_complex);
    B_complex (*__pos__)(B_NumberD_complex, B_complex);
    $WORD (*real)(B_NumberD_complex, B_complex, B_Real);
    $WORD (*imag)(B_NumberD_complex, B_complex, B_Real);
    $WORD (*__abs__)(B_NumberD_complex, B_complex, B_Real);
    B_complex (*conjugate)(B_NumberD_complex, B_complex);
};

void B_NumberD_complexD___init__ (B_NumberD_complex);
void B_NumberD_complexD___serialize__(B_NumberD_complex, $Serial$state);
B_NumberD_complex B_NumberD_complexD___deserialize__(B_NumberD_complex, $Serial$state);
B_complex B_NumberD_complexD___add__ (B_NumberD_complex, B_complex, B_complex);
B_complex B_NumberD_complexD___fromatom__(B_NumberD_complex,B_atom);
B_complex B_NumberD_complexD___complx__ (B_NumberD_complex, B_complex);
B_complex B_NumberD_complexD___mul__ (B_NumberD_complex, B_complex, B_complex);
//B_complex B_NumberD_complexD___truediv__ (B_NumberD_complex, B_complex, B_complex);
B_complex B_NumberD_complexD___pow__ (B_NumberD_complex, B_complex, B_complex);
B_complex B_NumberD_complexD___neg__ (B_NumberD_complex, B_complex);
B_complex B_NumberD_complexD___pos__ (B_NumberD_complex, B_complex);
$WORD B_NumberD_complex$real (B_NumberD_complex, B_complex, B_Real);
$WORD B_NumberD_complex$imag (B_NumberD_complex, B_complex, B_Real);
$WORD B_NumberD_complexD___abs__ (B_NumberD_complex, B_complex, B_Real);
B_complex B_NumberD_complex$conjugate (B_NumberD_complex, B_complex);

extern struct B_NumberD_complexG_class B_NumberD_complexG_methods;
B_NumberD_complex B_NumberD_complexG_new();

// B_MinusD_NumberD_complex ////////////////////////////////////////////////////////////

struct B_MinusD_NumberD_complex {
    B_MinusD_NumberD_complexG_class $class;
    B_Number W_Number;
};

struct B_MinusD_NumberD_complexG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_MinusD_NumberD_complex, B_Number);
    void (*__serialize__)(B_MinusD_NumberD_complex,$Serial$state);
    B_MinusD_NumberD_complex (*__deserialize__)(B_MinusD_NumberD_complex,$Serial$state);
    B_bool (*__bool__)(B_MinusD_NumberD_complex);
    B_str (*__str__)(B_MinusD_NumberD_complex);
    B_str (*__repr__)(B_MinusD_NumberD_complex);
    B_complex (*__sub__)(B_MinusD_NumberD_complex, B_complex, B_complex);
    B_complex (*__isub__)(B_MinusD_NumberD_complex, B_complex, B_complex);
};

void B_MinusD_NumberD_complexD___init__ (B_MinusD_NumberD_complex, B_Number);
void B_MinusD_NumberD_complexD___serialize__(B_MinusD_NumberD_complex, $Serial$state);
B_MinusD_NumberD_complex B_MinusD_NumberD_complexD___deserialize__(B_MinusD_NumberD_complex, $Serial$state);
B_complex B_MinusD_NumberD_complexD___sub__ (B_MinusD_NumberD_complex, B_complex, B_complex);

extern struct B_MinusD_NumberD_complexG_class B_MinusD_NumberD_complexG_methods;
B_MinusD_NumberD_complex B_MinusD_NumberD_complexG_new(B_Number);

// B_DivD_complex ////////////////////////////////////////////////////////////

struct B_DivD_complex {
    B_DivD_complexG_class $class;
};

struct B_DivD_complexG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_DivD_complex);
    void (*__serialize__)(B_DivD_complex,$Serial$state);
    B_DivD_complex (*__deserialize__)(B_DivD_complex,$Serial$state);
    B_bool (*__bool__)(B_DivD_complex);
    B_str (*__str__)(B_DivD_complex);
    B_str (*__repr__)(B_DivD_complex);
    B_complex (*__truediv__)(B_DivD_complex, B_complex, B_complex);
    B_complex (*__itruediv__)(B_DivD_complex, B_complex, B_complex);
};

B_complex B_DivD_complexD___truediv__ (B_DivD_complex, B_complex, B_complex);
B_complex B_DivD_complexD___itruediv__ (B_DivD_complex, B_complex, B_complex);

extern struct B_DivD_complexG_class B_DivD_complexG_methods;
B_DivD_complex B_DivD_complexG_new();

// B_EqD_complex ////////////////////////////////////////////////////////////

struct B_EqD_complex {
    B_EqD_complexG_class $class;
};

struct B_EqD_complexG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_EqD_complex);
    void (*__serialize__)(B_EqD_complex,$Serial$state);
    B_EqD_complex (*__deserialize__)(B_EqD_complex,$Serial$state);
    B_bool (*__bool__)(B_EqD_complex);
    B_str (*__str__)(B_EqD_complex);
    B_str (*__repr__)(B_EqD_complex);
    B_bool (*__eq__)(B_EqD_complex, B_complex, B_complex);
    B_bool (*__ne__)(B_EqD_complex, B_complex, B_complex);
};

void B_EqD_complexD___init__(B_EqD_complex);
void B_EqD_complexD___serialize__(B_EqD_complex, $Serial$state);
B_EqD_complex B_EqD_complexD___deserialize__(B_EqD_complex, $Serial$state);
B_bool B_EqD_complexD___eq__ (B_EqD_complex, B_complex, B_complex);
B_bool B_EqD_complexD___ne__ (B_EqD_complex, B_complex, B_complex);

extern struct B_EqD_complexG_class B_EqD_complexG_methods;
B_EqD_complex B_EqD_complexG_new();

// B_HashableD_complex ////////////////////////////////////////////////////////////

struct B_HashableD_complex {
    B_HashableD_complexG_class $class;
};

struct B_HashableD_complexG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_HashableD_complex);
    void (*__serialize__)(B_HashableD_complex,$Serial$state);
    B_HashableD_complex (*__deserialize__)(B_HashableD_complex,$Serial$state);
    B_bool (*__bool__)(B_HashableD_complex);
    B_str (*__str__)(B_HashableD_complex);
    B_str (*__repr__)(B_HashableD_complex);
    B_bool (*__eq__)(B_HashableD_complex, B_complex, B_complex);
    B_bool (*__ne__)(B_HashableD_complex, B_complex, B_complex);
    B_int (*__hash__)(B_HashableD_complex, B_complex);
};

void B_HashableD_complexD___init__ (B_HashableD_complex);
void B_HashableD_complexD___serialize__(B_HashableD_complex, $Serial$state);
B_HashableD_complex B_HashableD_complexD___deserialize__(B_HashableD_complex, $Serial$state);
B_bool B_HashableD_complexD___bool__(B_HashableD_complex);
B_str B_HashableD_complexD___str__(B_HashableD_complex);
B_bool B_HashableD_complexD___eq__ (B_HashableD_complex, B_complex, B_complex);
B_bool B_HashableD_complexD___ne__ (B_HashableD_complex, B_complex, B_complex);
B_int B_HashableD_complexD___hash__ (B_HashableD_complex, B_complex);

extern struct B_HashableD_complexG_class B_HashableD_complexG_methods;
B_HashableD_complex B_HashableD_complexG_new();

// B_IterableD_range ////////////////////////////////////////////////////////////

struct B_IterableD_range {
    B_IterableD_rangeG_class $class;
};

struct B_IterableD_rangeG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_IterableD_range);
    void (*__serialize__)(B_IterableD_range,$Serial$state);
    B_IterableD_range (*__deserialize__)(B_IterableD_range,$Serial$state);
    B_bool (*__bool__)(B_IterableD_range);
    B_str (*__str__)(B_IterableD_range);
    B_str (*__repr__)(B_IterableD_range);
    B_Iterator (*__iter__)(B_IterableD_range, B_range);
};

void B_IterableD_rangeD___init__ (B_IterableD_range);
void B_IterableD_rangeD___serialize__(B_IterableD_range, $Serial$state);
B_IterableD_range B_IterableD_rangeD___deserialize__(B_IterableD_range, $Serial$state);
B_Iterator B_IterableD_rangeD___iter__ (B_IterableD_range, B_range);

extern struct B_IterableD_rangeG_class B_IterableD_rangeG_methods;
B_IterableD_range B_IterableD_rangeG_new();

// B_IterableD_tuple ////////////////////////////////////////////////////////////

struct B_IterableD_tuple {
    B_IterableD_tupleG_class $class;
};

struct B_IterableD_tupleG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_IterableD_tuple);
    void (*__serialize__)(B_IterableD_tuple,$Serial$state);
    B_IterableD_tuple (*__deserialize__)(B_IterableD_tuple,$Serial$state);
    B_bool (*__bool__)(B_IterableD_tuple);
    B_str (*__str__)(B_IterableD_tuple);
    B_str (*__repr__)(B_IterableD_tuple);
    B_Iterator (*__iter__)(B_IterableD_tuple, B_tuple);
};

void B_IterableD_tupleD___init__ (B_IterableD_tuple);
void B_IterableD_tupleD___serialize__(B_IterableD_tuple, $Serial$state);
B_IterableD_tuple B_IterableD_tupleD___deserialize__(B_IterableD_tuple, $Serial$state);
B_Iterator B_IterableD_tupleD___iter__ (B_IterableD_tuple, B_tuple);

extern struct B_IterableD_tupleG_class B_IterableD_tupleG_methods;
B_IterableD_tuple B_IterableD_tupleG_new();

// B_SliceableD_tuple ////////////////////////////////////////////////////////////

// all methods except getitem and getslice will raise NotImplementedError

struct B_SliceableD_tuple {
    B_SliceableD_tupleG_class $class;
};

struct B_SliceableD_tupleG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_SliceableD_tuple);
    void (*__serialize__)(B_SliceableD_tuple,$Serial$state);
    B_SliceableD_tuple (*__deserialize__)(B_SliceableD_tuple,$Serial$state);
    B_bool (*__bool__)(B_SliceableD_tuple);
    B_str (*__str__)(B_SliceableD_tuple);
    B_str (*__repr__)(B_SliceableD_tuple);
    $WORD (*__getitem__)(B_SliceableD_tuple, B_tuple, B_int);
    void (*__setitem__)(B_SliceableD_tuple, B_tuple, B_int, $WORD);
    void (*__delitem__)(B_SliceableD_tuple, B_tuple, B_int);
    B_tuple (*__getslice__)(B_SliceableD_tuple, B_tuple, B_slice);
    void (*__setslice__)(B_SliceableD_tuple, B_tuple, B_Iterable, B_slice, $WORD);
    void (*__delslice__)(B_SliceableD_tuple, B_tuple, B_slice);
};

void B_SliceableD_tupleD___init__ (B_SliceableD_tuple);
void B_SliceableD_tupleD___serialize__(B_SliceableD_tuple, $Serial$state);
B_SliceableD_tuple B_SliceableD_tupleD___deserialize__(B_SliceableD_tuple, $Serial$state);
$WORD B_SliceableD_tupleD___getitem__ (B_SliceableD_tuple, B_tuple, B_int);
void B_SliceableD_tupleD___setitem__ (B_SliceableD_tuple, B_tuple, B_int, $WORD);
void B_SliceableD_tupleD___delitem__ (B_SliceableD_tuple, B_tuple, B_int);
B_tuple B_SliceableD_tupleD___getslice__ (B_SliceableD_tuple, B_tuple, B_slice);
void B_SliceableD_tupleD___setslice__ (B_SliceableD_tuple, B_tuple, B_Iterable, B_slice, $WORD);
void B_SliceableD_tupleD___delslice__ (B_SliceableD_tuple, B_tuple, B_slice);

extern struct B_SliceableD_tupleG_class B_SliceableD_tupleG_methods;
B_SliceableD_tuple B_SliceableD_tupleG_new();

// B_HashableD_tuple ////////////////////////////////////////////////////////////

struct B_HashableD_tuple {
    B_HashableD_tupleG_class $class;
    int W_HashableB_tuple$size;
    B_Hashable *W_Hashable;
};

struct B_HashableD_tupleG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_HashableD_tuple,int,B_Hashable*);
    void (*__serialize__)(B_HashableD_tuple,$Serial$state);
    B_HashableD_tuple (*__deserialize__)(B_HashableD_tuple,$Serial$state);
    B_bool (*__bool__)(B_HashableD_tuple);
    B_str (*__str__)(B_HashableD_tuple);
    B_str (*__repr__)(B_HashableD_tuple);
    B_bool (*__eq__)(B_HashableD_tuple, B_tuple, B_tuple);
    B_bool (*__ne__)(B_HashableD_tuple, B_tuple, B_tuple);
    B_int (*__hash__)(B_HashableD_tuple, B_tuple);
};
  
void B_HashableD_tupleD___init__ (B_HashableD_tuple,int,B_Hashable*);
void B_HashableD_tupleD___serialize__(B_HashableD_tuple, $Serial$state);
B_HashableD_tuple B_HashableD_tupleD___deserialize__(B_HashableD_tuple, $Serial$state);
B_bool B_HashableD_tupleD___eq__ (B_HashableD_tuple, B_tuple, B_tuple);
B_bool B_HashableD_tupleD___ne__ (B_HashableD_tuple, B_tuple, B_tuple);
B_int B_HashableD_tupleD___hash__ (B_HashableD_tuple, B_tuple);

extern struct B_HashableD_tupleG_class B_HashableD_tupleG_methods;
B_HashableD_tuple B_HashableD_tupleG_new();

// B_OrdD_bytearray ////////////////////////////////////////////////////////////

struct B_OrdD_bytearray {
    B_OrdD_bytearrayG_class $class;
};

struct B_OrdD_bytearrayG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_OrdD_bytearray);
    void (*__serialize__)(B_OrdD_bytearray,$Serial$state);
    B_OrdD_bytearray (*__deserialize__)(B_OrdD_bytearray,$Serial$state);
    B_bool (*__bool__)(B_OrdD_bytearray);
    B_str (*__str__)(B_OrdD_bytearray);
    B_str (*__repr__)(B_OrdD_bytearray);
    B_bool (*__eq__)(B_OrdD_bytearray, B_bytearray, B_bytearray);
    B_bool (*__ne__)(B_OrdD_bytearray, B_bytearray, B_bytearray);
    B_bool (*__lt__)(B_OrdD_bytearray, B_bytearray, B_bytearray);
    B_bool (*__le__)(B_OrdD_bytearray, B_bytearray, B_bytearray);
    B_bool (*__gt__)(B_OrdD_bytearray, B_bytearray, B_bytearray);
    B_bool (*__ge__)(B_OrdD_bytearray, B_bytearray, B_bytearray);
};

void B_OrdD_bytearrayD___init__ (B_OrdD_bytearray);
void B_OrdD_bytearrayD___serialize__(B_OrdD_bytearray, $Serial$state);
B_OrdD_bytearray B_OrdD_bytearrayD___deserialize__(B_OrdD_bytearray, $Serial$state);
B_bool B_OrdD_bytearrayD___bool__(B_OrdD_bytearray);
B_str B_OrdD_bytearrayD___str__(B_OrdD_bytearray);
B_bool B_OrdD_bytearrayD___eq__ (B_OrdD_bytearray, B_bytearray, B_bytearray);
B_bool B_OrdD_bytearrayD___ne__ (B_OrdD_bytearray, B_bytearray, B_bytearray);
B_bool B_OrdD_bytearrayD___lt__ (B_OrdD_bytearray, B_bytearray, B_bytearray);
B_bool B_OrdD_bytearrayD___le__ (B_OrdD_bytearray, B_bytearray, B_bytearray);
B_bool B_OrdD_bytearrayD___gt__ (B_OrdD_bytearray, B_bytearray, B_bytearray);
B_bool B_OrdD_bytearrayD___ge__ (B_OrdD_bytearray, B_bytearray, B_bytearray);

extern struct B_OrdD_bytearrayG_class B_OrdD_bytearrayG_methods;
B_OrdD_bytearray B_OrdD_bytearrayG_new();

// B_SequenceD_bytearray ////////////////////////////////////////////////////////////

struct B_SequenceD_bytearray {
    B_SequenceD_bytearrayG_class $class;
    B_Collection W_Collection;
    B_Times W_Times;
};

struct B_SequenceD_bytearrayG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_SequenceD_bytearray);
    void (*__serialize__)(B_SequenceD_bytearray,$Serial$state);
    B_SequenceD_bytearray (*__deserialize__)(B_SequenceD_bytearray,$Serial$state);
    B_bool (*__bool__)(B_SequenceD_bytearray);
    B_str (*__str__)(B_SequenceD_bytearray);
    B_str (*__repr__)(B_SequenceD_bytearray);
    B_int (*__getitem__)(B_SequenceD_bytearray, B_bytearray, B_int);
    void (*__setitem__)(B_SequenceD_bytearray, B_bytearray, B_int, B_int);
    void (*__delitem__)(B_SequenceD_bytearray, B_bytearray, B_int);
    B_bytearray (*__getslice__)(B_SequenceD_bytearray, B_bytearray, B_slice);
    void (*__setslice__)(B_SequenceD_bytearray, B_bytearray, B_Iterable, B_slice, $WORD);
    void (*__delslice__)(B_SequenceD_bytearray, B_bytearray, B_slice);
    B_Iterator (*__reversed__)(B_SequenceD_bytearray, B_bytearray);
    void (*insert)(B_SequenceD_bytearray, B_bytearray, B_int, B_int);
    void (*append)(B_SequenceD_bytearray, B_bytearray, B_int);
    void (*reverse)(B_SequenceD_bytearray, B_bytearray);
};

void B_SequenceD_bytearrayD___init__ (B_SequenceD_bytearray);
void B_SequenceD_bytearrayD___serialize__(B_SequenceD_bytearray, $Serial$state);
B_SequenceD_bytearray B_SequenceD_bytearrayD___deserialize__(B_SequenceD_bytearray, $Serial$state);
B_bool B_SequenceD_bytearrayD___bool__(B_SequenceD_bytearray);
B_str B_SequenceD_bytearrayD___str__(B_SequenceD_bytearray);
B_int B_SequenceD_bytearrayD___getitem__ (B_SequenceD_bytearray, B_bytearray, B_int);
void B_SequenceD_bytearrayD___setitem__ (B_SequenceD_bytearray, B_bytearray, B_int, B_int);
void B_SequenceD_bytearrayD___delitem__ (B_SequenceD_bytearray, B_bytearray, B_int);
B_bytearray B_SequenceD_bytearrayD___getslice__ (B_SequenceD_bytearray, B_bytearray, B_slice);
void B_SequenceD_bytearrayD___setslice__ (B_SequenceD_bytearray, B_bytearray, B_Iterable, B_slice, $WORD);
void B_SequenceD_bytearrayD___delslice__ (B_SequenceD_bytearray, B_bytearray, B_slice);
B_Iterator B_SequenceD_bytearrayD___reversed__ (B_SequenceD_bytearray, B_bytearray);
void B_SequenceD_bytearray$insert (B_SequenceD_bytearray, B_bytearray, B_int, B_int);
void B_SequenceD_bytearray$append (B_SequenceD_bytearray, B_bytearray, B_int);
void B_SequenceD_bytearray$reverse (B_SequenceD_bytearray, B_bytearray);

extern struct B_SequenceD_bytearrayG_class B_SequenceD_bytearrayG_methods;
B_SequenceD_bytearray B_SequenceD_bytearrayG_new();

// B_CollectionD_SequenceD_bytearray ////////////////////////////////////////////////////////////

struct B_CollectionD_SequenceD_bytearray {
    B_CollectionD_SequenceD_bytearrayG_class $class;
    B_Sequence W_Sequence;
};

struct B_CollectionD_SequenceD_bytearrayG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_CollectionD_SequenceD_bytearray, B_Sequence);
    void (*__serialize__)(B_CollectionD_SequenceD_bytearray,$Serial$state);
    B_CollectionD_SequenceD_bytearray (*__deserialize__)(B_CollectionD_SequenceD_bytearray,$Serial$state);
    B_bool (*__bool__)(B_CollectionD_SequenceD_bytearray);
    B_str (*__str__)(B_CollectionD_SequenceD_bytearray);
    B_str (*__repr__)(B_CollectionD_SequenceD_bytearray);
    B_Iterator (*__iter__)(B_CollectionD_SequenceD_bytearray, B_bytearray);
    B_bytearray (*__fromiter__)(B_CollectionD_SequenceD_bytearray, B_Iterable, $WORD);
    B_int (*__len__)(B_CollectionD_SequenceD_bytearray, B_bytearray);
};

void B_CollectionD_SequenceD_bytearrayD___init__ (B_CollectionD_SequenceD_bytearray, B_Sequence);
void B_CollectionD_SequenceD_bytearrayD___serialize__(B_CollectionD_SequenceD_bytearray, $Serial$state);
B_CollectionD_SequenceD_bytearray B_CollectionD_SequenceD_bytearrayD___deserialize__(B_CollectionD_SequenceD_bytearray, $Serial$state);
B_bool B_CollectionD_SequenceD_bytearrayD___bool__(B_CollectionD_SequenceD_bytearray);
B_str B_CollectionD_SequenceD_bytearrayD___str__(B_CollectionD_SequenceD_bytearray);
B_Iterator B_CollectionD_SequenceD_bytearrayD___iter__ (B_CollectionD_SequenceD_bytearray, B_bytearray);
B_bytearray B_CollectionD_SequenceD_bytearrayD___fromiter__ (B_CollectionD_SequenceD_bytearray, B_Iterable, $WORD);
B_int B_CollectionD_SequenceD_bytearrayD___len__ (B_CollectionD_SequenceD_bytearray, B_bytearray);

extern struct B_CollectionD_SequenceD_bytearrayG_class B_CollectionD_SequenceD_bytearrayG_methods;
B_CollectionD_SequenceD_bytearray B_CollectionD_SequenceD_bytearrayG_new(B_Sequence);

// B_TimesD_SequenceD_bytearray ////////////////////////////////////////////////////////////

struct B_TimesD_SequenceD_bytearray {
    B_TimesD_SequenceD_bytearrayG_class $class;
    B_Sequence W_Sequence;
};

struct B_TimesD_SequenceD_bytearrayG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_TimesD_SequenceD_bytearray, B_Sequence);
    void (*__serialize__)(B_TimesD_SequenceD_bytearray,$Serial$state);
    B_TimesD_SequenceD_bytearray (*__deserialize__)(B_TimesD_SequenceD_bytearray,$Serial$state);
    B_bool (*__bool__)(B_TimesD_SequenceD_bytearray);
    B_str (*__str__)(B_TimesD_SequenceD_bytearray);
    B_str (*__repr__)(B_TimesD_SequenceD_bytearray);
    B_bytearray (*__add__)(B_TimesD_SequenceD_bytearray, B_bytearray, B_bytearray);
    B_bytearray (*__iadd__)(B_TimesD_SequenceD_bytearray, B_bytearray, B_bytearray);
    B_bytearray (*__mul__)(B_TimesD_SequenceD_bytearray, B_bytearray, B_int);
    B_bytearray (*__imul__)(B_TimesD_SequenceD_bytearray, B_bytearray, B_int);
};

void B_TimesD_SequenceD_bytearrayD___init__ (B_TimesD_SequenceD_bytearray, B_Sequence);
void B_TimesD_SequenceD_bytearrayD___serialize__(B_TimesD_SequenceD_bytearray, $Serial$state);
B_TimesD_SequenceD_bytearray B_TimesD_SequenceD_bytearrayD___deserialize__(B_TimesD_SequenceD_bytearray, $Serial$state);
B_bool B_TimesD_SequenceD_bytearrayD___bool__(B_TimesD_SequenceD_bytearray);
B_str B_TimesD_SequenceD_bytearrayD___str__(B_TimesD_SequenceD_bytearray);
B_bytearray B_TimesD_SequenceD_bytearrayD___add__ (B_TimesD_SequenceD_bytearray, B_bytearray, B_bytearray);
B_bytearray B_TimesD_SequenceD_bytearrayD___mul__ (B_TimesD_SequenceD_bytearray, B_bytearray, B_int);

extern struct B_TimesD_SequenceD_bytearrayG_class B_TimesD_SequenceD_bytearrayG_methods;
B_TimesD_SequenceD_bytearray B_TimesD_SequenceD_bytearrayG_new(B_Sequence);

// B_ContainerD_bytearray ////////////////////////////////////////////////////////////

struct B_ContainerD_bytearray {
    B_ContainerD_bytearrayG_class $class;
};

struct B_ContainerD_bytearrayG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_ContainerD_bytearray);
    void (*__serialize__)(B_ContainerD_bytearray,$Serial$state);
    B_ContainerD_bytearray (*__deserialize__)(B_ContainerD_bytearray,$Serial$state);
    B_bool (*__bool__)(B_ContainerD_bytearray);
    B_str (*__str__)(B_ContainerD_bytearray);
    B_str (*__repr__)(B_ContainerD_bytearray);
    B_Iterator (*__iter__)(B_ContainerD_bytearray, B_bytearray);
    B_int (*__len__)(B_ContainerD_bytearray, B_bytearray);
    B_bool (*__contains__)(B_ContainerD_bytearray, B_bytearray, B_int);
    B_bool (*__containsnot__)(B_ContainerD_bytearray, B_bytearray, B_int);
};

void B_ContainerD_bytearrayD___init__ (B_ContainerD_bytearray);
void B_ContainerD_bytearrayD___serialize__(B_ContainerD_bytearray, $Serial$state);
B_ContainerD_bytearray B_ContainerD_bytearrayD___deserialize__(B_ContainerD_bytearray, $Serial$state);
B_bool B_ContainerD_bytearrayD___bool__(B_ContainerD_bytearray);
B_str B_ContainerD_bytearrayD___str__(B_ContainerD_bytearray);
B_Iterator B_ContainerD_bytearrayD___iter__ (B_ContainerD_bytearray, B_bytearray);
B_int B_ContainerD_bytearrayD___len__ (B_ContainerD_bytearray, B_bytearray);
B_bool B_ContainerD_bytearrayD___contains__ (B_ContainerD_bytearray, B_bytearray, B_int);
B_bool B_ContainerD_bytearrayD___containsnot__ (B_ContainerD_bytearray, B_bytearray, B_int);

extern struct B_ContainerD_bytearrayG_class B_ContainerD_bytearrayG_methods;
B_ContainerD_bytearray B_ContainerD_bytearrayG_new();

// B_OrdD_bytes ////////////////////////////////////////////////////////////

struct B_OrdD_bytes {
    B_OrdD_bytesG_class $class;
};

struct B_OrdD_bytesG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_OrdD_bytes);
    void (*__serialize__)(B_OrdD_bytes,$Serial$state);
    B_OrdD_bytes (*__deserialize__)(B_OrdD_bytes,$Serial$state);
    B_bool (*__bool__)(B_OrdD_bytes);
    B_str (*__str__)(B_OrdD_bytes);
    B_str (*__repr__)(B_OrdD_bytes);
    B_bool (*__eq__)(B_OrdD_bytes, B_bytes, B_bytes);
    B_bool (*__ne__)(B_OrdD_bytes, B_bytes, B_bytes);
    B_bool (*__lt__)(B_OrdD_bytes, B_bytes, B_bytes);
    B_bool (*__le__)(B_OrdD_bytes, B_bytes, B_bytes);
    B_bool (*__gt__)(B_OrdD_bytes, B_bytes, B_bytes);
    B_bool (*__ge__)(B_OrdD_bytes, B_bytes, B_bytes);
};

void B_OrdD_bytesD___init__ (B_OrdD_bytes);
void B_OrdD_bytesD___serialize__(B_OrdD_bytes, $Serial$state);
B_OrdD_bytes B_OrdD_bytesD___deserialize__(B_OrdD_bytes, $Serial$state);
B_bool B_OrdD_bytesD___eq__ (B_OrdD_bytes, B_bytes, B_bytes);
B_bool B_OrdD_bytesD___ne__ (B_OrdD_bytes, B_bytes, B_bytes);
B_bool B_OrdD_bytesD___lt__ (B_OrdD_bytes, B_bytes, B_bytes);
B_bool B_OrdD_bytesD___le__ (B_OrdD_bytes, B_bytes, B_bytes);
B_bool B_OrdD_bytesD___gt__ (B_OrdD_bytes, B_bytes, B_bytes);
B_bool B_OrdD_bytesD___ge__ (B_OrdD_bytes, B_bytes, B_bytes);

extern struct B_OrdD_bytesG_class B_OrdD_bytesG_methods;
B_OrdD_bytes B_OrdD_bytesG_new();

// B_ContainerD_bytes ////////////////////////////////////////////////////////////

struct B_ContainerD_bytes {
    B_ContainerD_bytesG_class $class;
};

struct B_ContainerD_bytesG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_ContainerD_bytes);
    void (*__serialize__)(B_ContainerD_bytes,$Serial$state);
    B_ContainerD_bytes (*__deserialize__)(B_ContainerD_bytes,$Serial$state);
    B_bool (*__bool__)(B_ContainerD_bytes);
    B_str (*__str__)(B_ContainerD_bytes);
    B_str (*__repr__)(B_ContainerD_bytes);
    B_Iterator (*__iter__)(B_ContainerD_bytes, B_bytes);
    B_bytes (*__fromiter__)(B_ContainerD_bytes, B_Iterable, $WORD);
    B_int (*__len__)(B_ContainerD_bytes, B_bytes);
    B_bool (*__contains__)(B_ContainerD_bytes, B_bytes, B_bytes);
    B_bool (*__containsnot__)(B_ContainerD_bytes, B_bytes, B_bytes);
};

void B_ContainerD_bytesD___init__ (B_ContainerD_bytes);
void B_ContainerD_bytesD___serialize__(B_ContainerD_bytes, $Serial$state);
B_ContainerD_bytes B_ContainerD_bytesD___deserialize__(B_ContainerD_bytes, $Serial$state);
B_Iterator B_ContainerD_bytesD___iter__ (B_ContainerD_bytes, B_bytes);
B_int B_ContainerD_bytesD___len__ (B_ContainerD_bytes, B_bytes);
B_bool B_ContainerD_bytesD___contains__ (B_ContainerD_bytes, B_bytes, B_bytes);
B_bool B_ContainerD_bytesD___containsnot__ (B_ContainerD_bytes, B_bytes, B_bytes);

extern struct B_ContainerD_bytesG_class B_ContainerD_bytesG_methods;
B_ContainerD_bytes B_ContainerD_bytesG_new();

// B_SliceableD_bytes ////////////////////////////////////////////////////////////

struct B_SliceableD_bytes {
    B_SliceableD_bytesG_class $class;
};

struct B_SliceableD_bytesG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_SliceableD_bytes);
    void (*__serialize__)(B_SliceableD_bytes,$Serial$state);
    B_SliceableD_bytes (*__deserialize__)(B_SliceableD_bytes,$Serial$state);
    B_bool (*__bool__)(B_SliceableD_bytes);
    B_str (*__str__)(B_SliceableD_bytes);
    B_str (*__repr__)(B_SliceableD_bytes);
    B_int (*__getitem__)(B_SliceableD_bytes, B_bytes, B_int);
    void (*__setitem__)(B_SliceableD_bytes, B_bytes, B_int, B_bytes);
    void (*__delitem__)(B_SliceableD_bytes, B_bytes, B_int);
    B_bytes (*__getslice__)(B_SliceableD_bytes, B_bytes, B_slice);
    void (*__setslice__)(B_SliceableD_bytes, B_bytes, B_Iterable, B_slice, $WORD);
    void (*__delslice__)(B_SliceableD_bytes, B_bytes, B_slice);
};

void B_SliceableD_bytesD___init__ (B_SliceableD_bytes);
void B_SliceableD_bytesD___serialize__(B_SliceableD_bytes, $Serial$state);
B_SliceableD_bytes B_SliceableD_bytesD___deserialize__(B_SliceableD_bytes, $Serial$state);
B_int B_SliceableD_bytesD___getitem__ (B_SliceableD_bytes, B_bytes, B_int);
void B_SliceableD_bytesD___setitem__ (B_SliceableD_bytes, B_bytes, B_int, B_bytes);
void B_SliceableD_bytesD___delitem__ (B_SliceableD_bytes, B_bytes, B_int);
B_bytes B_SliceableD_bytesD___getslice__ (B_SliceableD_bytes, B_bytes, B_slice);
void B_SliceableD_bytesD___setslice__ (B_SliceableD_bytes, B_bytes, B_Iterable, B_slice, $WORD);
void B_SliceableD_bytesD___delslice__ (B_SliceableD_bytes, B_bytes, B_slice);

extern struct B_SliceableD_bytesG_class B_SliceableD_bytesG_methods;
B_SliceableD_bytes B_SliceableD_bytesG_new();

// B_TimesD_bytes ////////////////////////////////////////////////////////////

struct B_TimesD_bytes {
    B_TimesD_bytesG_class $class;
};

struct B_TimesD_bytesG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_TimesD_bytes);
    void (*__serialize__)(B_TimesD_bytes,$Serial$state);
    B_TimesD_bytes (*__deserialize__)(B_TimesD_bytes,$Serial$state);
    B_bool (*__bool__)(B_TimesD_bytes);
    B_str (*__str__)(B_TimesD_bytes);
    B_str (*__repr__)(B_TimesD_bytes);
    B_bytes (*__add__)(B_TimesD_bytes, B_bytes, B_bytes);
    B_bytes (*__iadd__)(B_TimesD_bytes, B_bytes, B_bytes);
    B_bytes (*__mul__)(B_TimesD_bytes, B_bytes, B_int);
    B_bytes (*__imul__)(B_TimesD_bytes, B_bytes, B_int);
};

void B_TimesD_bytesD___init__ (B_TimesD_bytes);
void B_TimesD_bytesD___serialize__(B_TimesD_bytes, $Serial$state);
B_TimesD_bytes B_TimesD_bytesD___deserialize__(B_TimesD_bytes, $Serial$state);
B_bool B_TimesD_bytesD___bool__(B_TimesD_bytes);
B_bytes B_TimesD_bytesD___str__(B_TimesD_bytes);
B_bytes B_TimesD_bytesD___add__ (B_TimesD_bytes, B_bytes, B_bytes);
B_bytes B_TimesD_bytesD___mul__(B_TimesD_bytes, B_bytes, B_int);

extern struct B_TimesD_bytesG_class B_TimesD_bytesG_methods;
B_TimesD_bytes B_TimesD_bytesG_new();

// B_HashableD_bytes ////////////////////////////////////////////////////////////

struct B_HashableD_bytes {
    B_HashableD_bytesG_class $class;
};

struct B_HashableD_bytesG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_HashableD_bytes);
    void (*__serialize__)(B_HashableD_bytes,$Serial$state);
    B_HashableD_bytes (*__deserialize__)(B_HashableD_bytes,$Serial$state);
    B_bool (*__bool__)(B_HashableD_bytes);
    B_str (*__str__)(B_HashableD_bytes);
    B_str (*__repr__)(B_HashableD_bytes);
    B_bool (*__eq__)(B_HashableD_bytes, B_bytes, B_bytes);
    B_bool (*__ne__)(B_HashableD_bytes, B_bytes, B_bytes);
    B_int (*__hash__)(B_HashableD_bytes, B_bytes);
};

void B_HashableD_bytesD___init__ (B_HashableD_bytes);
void B_HashableD_bytesD___serialize__(B_HashableD_bytes, $Serial$state);
B_HashableD_bytes B_HashableD_bytesD___deserialize__(B_HashableD_bytes, $Serial$state);
B_bool B_HashableD_bytesD___eq__ (B_HashableD_bytes, B_bytes, B_bytes);
B_bool B_HashableD_bytesD___ne__ (B_HashableD_bytes, B_bytes, B_bytes);
B_int B_HashableD_bytesD___hash__ (B_HashableD_bytes, B_bytes);

extern struct B_HashableD_bytesG_class B_HashableD_bytesG_methods;
B_HashableD_bytes B_HashableD_bytesG_new();

void $register_builtin_protocols();
