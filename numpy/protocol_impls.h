#pragma once

#include "../modules/math.h"


struct numpy$$Integral$ndarray;
typedef struct numpy$$Integral$ndarray *numpy$$Integral$ndarray;

struct numpy$$Integral$ndarray$class;
typedef struct numpy$$Integral$ndarray$class *numpy$$Integral$ndarray$class;

struct numpy$$Logical$ndarray;
typedef struct numpy$$Logical$ndarray *numpy$$Logical$ndarray;

struct numpy$$Logical$ndarray$class;
typedef struct numpy$$Logical$ndarray$class *numpy$$Logical$ndarray$class;

struct numpy$$Minus$ndarray;
typedef struct numpy$$Minus$ndarray *numpy$$Minus$ndarray;

struct numpy$$Minus$ndarray$class;
typedef struct numpy$$Minus$ndarray$class *numpy$$Minus$ndarray$class;

struct numpy$$Collection$ndarray;
typedef struct numpy$$Collection$ndarray *numpy$$Collection$ndarray;

struct numpy$$Collection$ndarray$class;
typedef struct numpy$$Collection$ndarray$class *numpy$$Collection$ndarray$class;

// numpy$$Integral$ndarray ////////////////////////////////////////////////////////////

struct numpy$$Integral$ndarray {
    numpy$$Integral$ndarray$class $class;
    $Logical w$Logical;
    $Minus w$Minus;
    numpy$$Primitive w$Primitive$A$Integral$ndarray;
};

struct numpy$$Integral$ndarray$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)(numpy$$Integral$ndarray,numpy$$Primitive);
    void (*__serialize__)(numpy$$Integral$ndarray,$Serial$state); 
    numpy$$Integral$ndarray (*__deserialize__)(numpy$$Integral$ndarray,$Serial$state);
    $bool (*__bool__)(numpy$$Integral$ndarray);
    $str (*__str__)(numpy$$Integral$ndarray);
    numpy$$ndarray (*__add__)(numpy$$Integral$ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__iadd__)(numpy$$Integral$ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__fromatom__)(numpy$$Integral$ndarray,$atom);
    $complex (*__complx__)(numpy$$Integral$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__mul__)(numpy$$Integral$ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__truediv__)(numpy$$Integral$ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__pow__)(numpy$$Integral$ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__imul__)(numpy$$Integral$ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__itruediv__)(numpy$$Integral$ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__ipow__)(numpy$$Integral$ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__neg__)(numpy$$Integral$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__pos__)(numpy$$Integral$ndarray, numpy$$ndarray);
    $WORD (*real)(numpy$$Integral$ndarray, numpy$$ndarray, $Real);
    $WORD (*imag)(numpy$$Integral$ndarray, numpy$$ndarray, $Real);
    $WORD (*__abs__)(numpy$$Integral$ndarray, numpy$$ndarray, $Real);
    numpy$$ndarray (*conjugate)(numpy$$Integral$ndarray, numpy$$ndarray);
    $float (*__float__)(numpy$$Integral$ndarray, numpy$$ndarray);
    $WORD (*__trunc__)(numpy$$Integral$ndarray, numpy$$ndarray, $Integral);
    $WORD (*__floor__)(numpy$$Integral$ndarray, numpy$$ndarray, $Integral);
    $WORD (*__ceil__)(numpy$$Integral$ndarray, numpy$$ndarray, $Integral);
    numpy$$ndarray (*__round__)(numpy$$Integral$ndarray, numpy$$ndarray, numpy$$ndarray);
    $WORD (*numerator)(numpy$$Integral$ndarray, numpy$$ndarray, $Integral);
    $WORD (*denominator)(numpy$$Integral$ndarray, numpy$$ndarray, $Integral);
    numpy$$ndarray (*__int__)(numpy$$Integral$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__index__)(numpy$$Integral$ndarray, numpy$$ndarray);
    $tuple (*__divmod__)(numpy$$Integral$ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__floordiv__)(numpy$$Integral$ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__mod__)(numpy$$Integral$ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__lshift__)(numpy$$Integral$ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__rshift__)(numpy$$Integral$ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__ifloordiv__)(numpy$$Integral$ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__imod__)(numpy$$Integral$ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__ilshift__)(numpy$$Integral$ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__irshift__)(numpy$$Integral$ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__invert__)(numpy$$Integral$ndarray, numpy$$ndarray);
};

void numpy$$Integral$ndarray$__init__ (numpy$$Integral$ndarray, numpy$$Primitive);
void numpy$$Integral$ndarray$__serialize__(numpy$$Integral$ndarray,$Serial$state); 
numpy$$Integral$ndarray numpy$$Integral$ndarray$__deserialize__(numpy$$Integral$ndarray,$Serial$state);
numpy$$ndarray numpy$$Integral$ndarray$__add__(numpy$$Integral$ndarray, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$$Integral$ndarray$__iadd__(numpy$$Integral$ndarray, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$$Integral$ndarray$__fromatom__(numpy$$Integral$ndarray,$atom);
$complex numpy$$Integral$ndarray$__complx__(numpy$$Integral$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$$Integral$ndarray$__mul__(numpy$$Integral$ndarray, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$$Integral$ndarray$__truediv__(numpy$$Integral$ndarray, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$$Integral$ndarray$__pow__(numpy$$Integral$ndarray, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$$Integral$ndarray$__neg__(numpy$$Integral$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$$Integral$ndarray$__pos__(numpy$$Integral$ndarray, numpy$$ndarray);
$WORD numpy$$Integral$ndarray$real(numpy$$Integral$ndarray, numpy$$ndarray, $Real);
$WORD numpy$$Integral$ndarray$imag(numpy$$Integral$ndarray, numpy$$ndarray, $Real);
$WORD numpy$$Integral$ndarray$__abs__(numpy$$Integral$ndarray, numpy$$ndarray, $Real);
numpy$$ndarray numpy$$Integral$ndarray$conjugate(numpy$$Integral$ndarray, numpy$$ndarray);
$float numpy$$Integral$ndarray$__float__ (numpy$$Integral$ndarray, numpy$$ndarray);
$WORD numpy$$Integral$ndarray$__trunc__ (numpy$$Integral$ndarray, numpy$$ndarray, $Integral);
$WORD numpy$$Integral$ndarray$__floor__ (numpy$$Integral$ndarray, numpy$$ndarray, $Integral);
$WORD numpy$$Integral$ndarray$__ceil__ (numpy$$Integral$ndarray, numpy$$ndarray, $Integral);
numpy$$ndarray numpy$$Integral$ndarray$__round__ (numpy$$Integral$ndarray, numpy$$ndarray, numpy$$ndarray);
$WORD numpy$$Integral$ndarray$numerator (numpy$$Integral$ndarray, numpy$$ndarray, $Integral);
$WORD numpy$$Integral$ndarray$denominator (numpy$$Integral$ndarray, numpy$$ndarray, $Integral);
numpy$$ndarray numpy$$Integral$ndarray$__int__ (numpy$$Integral$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$$Integral$ndarray$__index__ (numpy$$Integral$ndarray, numpy$$ndarray);
$tuple numpy$$Integral$ndarray$__divmod__ (numpy$$Integral$ndarray, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$$Integral$ndarray$__floordiv__ (numpy$$Integral$ndarray, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$$Integral$ndarray$__mod__ (numpy$$Integral$ndarray, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$$Integral$ndarray$__lshift__ (numpy$$Integral$ndarray, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$$Integral$ndarray$__rshift__ (numpy$$Integral$ndarray, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$$Integral$ndarray$__invert__ (numpy$$Integral$ndarray, numpy$$ndarray);

// numpy$$Logical$ndarray ////////////////////////////////////////////////////////////

struct numpy$$Logical$ndarray {
    numpy$$Logical$ndarray$class $class;
    $Integral w$Integral;
};

struct numpy$$Logical$ndarray$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)(numpy$$Logical$ndarray, $Integral);
    void (*__serialize__)(numpy$$Logical$ndarray,$Serial$state); 
    numpy$$Logical$ndarray (*__deserialize__)(numpy$$Logical$ndarray,$Serial$state);
    $bool (*__bool__)(numpy$$Logical$ndarray);
    $str (*__str__)(numpy$$Logical$ndarray);
    numpy$$ndarray (*__and__)(numpy$$Logical$ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__or__)(numpy$$Logical$ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__xor__)(numpy$$Logical$ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__iand__)(numpy$$Logical$ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__ior__)(numpy$$Logical$ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__ixor__)(numpy$$Logical$ndarray, numpy$$ndarray, numpy$$ndarray);
};

void numpy$$Logical$ndarray$__init__ (numpy$$Logical$ndarray, $Integral);
void numpy$$Logical$ndarray$__serialize__(numpy$$Logical$ndarray,$Serial$state); 
numpy$$Logical$ndarray numpy$$Logical$ndarray$__deserialize__(numpy$$Logical$ndarray,$Serial$state);
numpy$$ndarray numpy$$Logical$ndarray$__and__ (numpy$$Logical$ndarray, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$$Logical$ndarray$__or__ (numpy$$Logical$ndarray, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$$Logical$ndarray$__xor__ (numpy$$Logical$ndarray, numpy$$ndarray, numpy$$ndarray);

// numpy$$Minus$ndarray ////////////////////////////////////////////////////////////

struct numpy$$Minus$ndarray {
    numpy$$Minus$ndarray$class $class;
    $Integral w$Integral;
};

struct numpy$$Minus$ndarray$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)(numpy$$Minus$ndarray, $Integral);
    void (*__serialize__)(numpy$$Minus$ndarray,$Serial$state); 
    numpy$$Minus$ndarray (*__deserialize__)(numpy$$Minus$ndarray,$Serial$state);
    $bool (*__bool__)(numpy$$Minus$ndarray);
    $str (*__str__)(numpy$$Minus$ndarray);
    numpy$$ndarray (*__sub__)(numpy$$Minus$ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__isub__)(numpy$$Minus$ndarray, numpy$$ndarray, numpy$$ndarray);
};

void numpy$$Minus$ndarray$__init__ (numpy$$Minus$ndarray, $Integral);
void numpy$$Minus$ndarray$__serialize__(numpy$$Minus$ndarray,$Serial$state); 
numpy$$Minus$ndarray numpy$$Minus$ndarray$__deserialize__(numpy$$Minus$ndarray,$Serial$state);
numpy$$ndarray numpy$$Minus$ndarray$__sub__ (numpy$$Minus$ndarray, numpy$$ndarray, numpy$$ndarray);

// numpy$$Sliceable$ndarray /////////////////////////////////////////////////////////////////

struct numpy$$Sliceable$ndarray;
typedef struct numpy$$Sliceable$ndarray *numpy$$Sliceable$ndarray;

struct numpy$$Sliceable$ndarray$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__) (numpy$$Sliceable$ndarray);
    void (*__serialize__) (numpy$$Sliceable$ndarray, $Serial$state);
    numpy$$Sliceable$ndarray (*__deserialize__) (numpy$$Sliceable$ndarray, $Serial$state);
    $bool (*__bool__)(numpy$$Sliceable$ndarray);
    $str (*__str__)(numpy$$Sliceable$ndarray);
    numpy$$ndarray (*__getitem__) (numpy$$Sliceable$ndarray, numpy$$ndarray, $int);
    void (*__setitem__) (numpy$$Sliceable$ndarray, numpy$$ndarray, $int, $WORD);
    void (*__delitem__) (numpy$$Sliceable$ndarray, numpy$$ndarray, $int);
    numpy$$ndarray (*__getslice__) (numpy$$Sliceable$ndarray, numpy$$ndarray, $slice);
    void (*__setslice__) (numpy$$Sliceable$ndarray, numpy$$ndarray, $Iterable, $slice, $WORD);
    void (*__delslice__) (numpy$$Sliceable$ndarray, numpy$$ndarray, $slice);
};
struct numpy$$Sliceable$ndarray {
    struct numpy$$Sliceable$ndarray$class *$class;
};


// numpy$$Collection$ndarray ////////////////////////////////////////////////////////////

struct numpy$$Collection$ndarray {
  numpy$$Collection$ndarray$class $class;
  numpy$$Primitive pwit;
};

struct numpy$$Collection$ndarray$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)(numpy$$Collection$ndarray, numpy$$Primitive);
    void (*__serialize__)(numpy$$Collection$ndarray,$Serial$state); 
    numpy$$Collection$ndarray (*__deserialize__)(numpy$$Collection$ndarray,$Serial$state);
    $bool (*__bool__)(numpy$$Collection$ndarray);
    $str (*__str__)(numpy$$Collection$ndarray);
    $Iterator (*__iter__)(numpy$$Collection$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__fromiter__)(numpy$$Collection$ndarray, $Iterable);
    $int (*__len__)(numpy$$Collection$ndarray, numpy$$ndarray);
};

void numpy$$Collection$ndarray$__init__ (numpy$$Collection$ndarray, numpy$$Primitive);
void numpy$$Collection$ndarray$__serialize__(numpy$$Collection$ndarray,$Serial$state); 
numpy$$Collection$ndarray numpy$$Collection$ndarray$__deserialize__(numpy$$Collection$ndarray,$Serial$state);
$Iterator numpy$$Collection$ndarray$__iter__ (numpy$$Collection$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$$Collection$ndarray$__fromiter__(numpy$$Collection$ndarray, $Iterable);
$int numpy$$Collection$ndarray$__len__(numpy$$Collection$ndarray, numpy$$ndarray);

// numpy$$RealFloat$ndarray ////////////////////////////////////////////////////////

#define numpy$$RealFloat$ndarray (($Real)numpy$$Integral$ndarray)
#define numpy$$RealFloat$ndarray$new(...) ($Real)numpy$$Integral$ndarray$new(__VA_ARGS__)

// numpy$$RealFuns$math$ndarray ////////////////////////////////////////////////////

struct numpy$$RealFuns$math$ndarray;
typedef struct numpy$$RealFuns$math$ndarray *numpy$$RealFuns$math$ndarray;
struct numpy$$RealFuns$math$ndarray$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (numpy$$RealFuns$math$ndarray, numpy$$Primitive, math$$RealFuns);
    $NoneType (*__serialize__) (numpy$$RealFuns$math$ndarray, $Serial$state);
    numpy$$RealFuns$math$ndarray (*__deserialize__) (numpy$$RealFuns$math$ndarray, $Serial$state);
    $bool (*__bool__)(numpy$$RealFuns$math$ndarray);
    $str (*__str__)(numpy$$RealFuns$math$ndarray);
    numpy$$ndarray (*sqrt) (numpy$$RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*exp) (numpy$$RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*log) (numpy$$RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*sin) (numpy$$RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*cos) (numpy$$RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*tan) (numpy$$RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*asin) (numpy$$RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*acos) (numpy$$RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*atan) (numpy$$RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*sinh) (numpy$$RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*cosh) (numpy$$RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*tanh) (numpy$$RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*asinh) (numpy$$RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*acosh) (numpy$$RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*atanh) (numpy$$RealFuns$math$ndarray, numpy$$ndarray);
};
struct numpy$$RealFuns$math$ndarray {
    struct numpy$$RealFuns$math$ndarray$class *$class;
    numpy$$Primitive w$Primitive$A$RealFuns$math$ndarray;
  math$$RealFuns w$RealFuns$math$A$RealFuns$math$ndarray;
};
extern struct numpy$$RealFuns$math$ndarray$class numpy$$RealFuns$math$ndarray$methods;


// method tables /////////////////////////////////////////////////////////////////

extern struct numpy$$Integral$ndarray$class numpy$$Integral$ndarray$methods;
extern struct numpy$$Logical$ndarray$class numpy$$Logical$ndarray$methods;
extern struct numpy$$Minus$ndarray$class numpy$$Minus$ndarray$methods;
extern struct numpy$$Sliceable$ndarray$class numpy$$Sliceable$ndarray$methods;
extern struct numpy$$Collection$ndarray$class numpy$$Collection$ndarray$methods;

numpy$$Integral$ndarray numpy$$Integral$ndarray$new();
numpy$$Logical$ndarray numpy$$Logical$ndarray$new($Integral);
numpy$$Minus$ndarray numpy$$Minus$ndarray$new($Integral);
numpy$$Sliceable$ndarray numpy$$Sliceable$ndarray$new();
numpy$$Collection$ndarray numpy$$Collection$ndarray$new(numpy$$Primitive);
numpy$$RealFuns$math$ndarray numpy$$RealFuns$math$ndarray$new(numpy$$Primitive, math$$RealFuns);

