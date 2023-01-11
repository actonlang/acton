#pragma once
#include "builtin/builtin.h"
#include "builtin/env.h"
#include "rts/rts.h"
struct math$B_RealFuns;
typedef struct math$B_RealFuns *math$B_RealFuns;
struct math$B_RealFunsG_class {
    char *$GCINFO;
    B_int $class_id;
    $SuperG_class $superclass;
    $NoneType (*__init__) (math$B_RealFuns);
    $NoneType (*__serialize__) (math$B_RealFuns, $NoneType);
    math$B_RealFuns (*__deserialize__) (math$B_RealFuns, $NoneType);
    B_bool (*__bool__)(math$B_RealFuns);
    B_str (*__str__)(math$B_RealFuns);
    B_str (*__repr__)(math$B_RealFuns);
    $WORD (*sqrt) (math$B_RealFuns, $WORD);
    $WORD (*exp) (math$B_RealFuns, $WORD);
    $WORD (*log) (math$B_RealFuns, $WORD);
    $WORD (*sin) (math$B_RealFuns, $WORD);
    $WORD (*cos) (math$B_RealFuns, $WORD);
    $WORD (*tan) (math$B_RealFuns, $WORD);
    $WORD (*asin) (math$B_RealFuns, $WORD);
    $WORD (*acos) (math$B_RealFuns, $WORD);
    $WORD (*atan) (math$B_RealFuns, $WORD);
    $WORD (*sinh) (math$B_RealFuns, $WORD);
    $WORD (*cosh) (math$B_RealFuns, $WORD);
    $WORD (*tanh) (math$B_RealFuns, $WORD);
    $WORD (*asinh) (math$B_RealFuns, $WORD);
    $WORD (*acosh) (math$B_RealFuns, $WORD);
    $WORD (*atanh) (math$B_RealFuns, $WORD);
};
struct math$B_RealFuns {
    struct math$B_RealFunsG_class *$class;
};
extern struct math$B_RealFunsG_class math$B_RealFunsG_methods;
math$B_RealFuns math$B_RealFunsG_new();
struct math$B_RealFunsB_float;
typedef struct math$B_RealFunsB_float *math$B_RealFunsB_float;
struct math$B_RealFunsB_floatG_class {
    char *$GCINFO;
    B_int $class_id;
    $SuperG_class $superclass;
    $NoneType (*__init__) (math$B_RealFunsB_float);
    $NoneType (*__serialize__) (math$B_RealFunsB_float, $NoneType);
    math$B_RealFunsB_float (*__deserialize__) (math$B_RealFunsB_float, $NoneType);
    B_bool (*__bool__)(math$B_RealFunsB_float);
    B_str (*__str__)(math$B_RealFunsB_float);
    B_str (*__repr__)(math$B_RealFunsB_float);
    B_float (*sqrt) (math$B_RealFunsB_float, B_float);
    B_float (*exp) (math$B_RealFunsB_float, B_float);
    B_float (*log) (math$B_RealFunsB_float, B_float);
    B_float (*sin) (math$B_RealFunsB_float, B_float);
    B_float (*cos) (math$B_RealFunsB_float, B_float);
    B_float (*tan) (math$B_RealFunsB_float, B_float);
    B_float (*asin) (math$B_RealFunsB_float, B_float);
    B_float (*acos) (math$B_RealFunsB_float, B_float);
    B_float (*atan) (math$B_RealFunsB_float, B_float);
    B_float (*sinh) (math$B_RealFunsB_float, B_float);
    B_float (*cosh) (math$B_RealFunsB_float, B_float);
    B_float (*tanh) (math$B_RealFunsB_float, B_float);
    B_float (*asinh) (math$B_RealFunsB_float, B_float);
    B_float (*acosh) (math$B_RealFunsB_float, B_float);
    B_float (*atanh) (math$B_RealFunsB_float, B_float);
};
struct math$B_RealFunsB_float {
    struct math$B_RealFunsB_floatG_class *$class;
};
extern struct math$B_RealFunsB_floatG_class math$B_RealFunsB_floatG_methods;
math$B_RealFunsB_float math$B_RealFunsB_floatG_new();
$WORD math$$sqrt (math$B_RealFuns, $WORD);
$WORD math$$exp (math$B_RealFuns, $WORD);
$WORD math$$log (math$B_RealFuns, $WORD);
$WORD math$$sin (math$B_RealFuns, $WORD);
$WORD math$$cos (math$B_RealFuns, $WORD);
$WORD math$$tan (math$B_RealFuns, $WORD);
$WORD math$$asin (math$B_RealFuns, $WORD);
$WORD math$$acos (math$B_RealFuns, $WORD);
$WORD math$$atan (math$B_RealFuns, $WORD);
$WORD math$$sinh (math$B_RealFuns, $WORD);
$WORD math$$cosh (math$B_RealFuns, $WORD);
$WORD math$$tanh (math$B_RealFuns, $WORD);
$WORD math$$asinh (math$B_RealFuns, $WORD);
$WORD math$$acosh (math$B_RealFuns, $WORD);
$WORD math$$atanh (math$B_RealFuns, $WORD);
void math$D___init__ ();
