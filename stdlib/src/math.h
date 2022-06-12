#pragma once
#include "builtin/builtin.h"
#include "builtin/env.h"
#include "rts/rts.h"
struct math$$RealFuns;
typedef struct math$$RealFuns *math$$RealFuns;
struct math$$RealFuns$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (math$$RealFuns);
    $NoneType (*__serialize__) (math$$RealFuns, $Serial$state);
    math$$RealFuns (*__deserialize__) (math$$RealFuns, $Serial$state);
    $bool (*__bool__)(math$$RealFuns);
    $str (*__str__)(math$$RealFuns);
    $str (*__repr__)(math$$RealFuns);
    $WORD (*sqrt) (math$$RealFuns, $WORD);
    $WORD (*exp) (math$$RealFuns, $WORD);
    $WORD (*log) (math$$RealFuns, $WORD);
    $WORD (*sin) (math$$RealFuns, $WORD);
    $WORD (*cos) (math$$RealFuns, $WORD);
    $WORD (*tan) (math$$RealFuns, $WORD);
    $WORD (*asin) (math$$RealFuns, $WORD);
    $WORD (*acos) (math$$RealFuns, $WORD);
    $WORD (*atan) (math$$RealFuns, $WORD);
    $WORD (*sinh) (math$$RealFuns, $WORD);
    $WORD (*cosh) (math$$RealFuns, $WORD);
    $WORD (*tanh) (math$$RealFuns, $WORD);
    $WORD (*asinh) (math$$RealFuns, $WORD);
    $WORD (*acosh) (math$$RealFuns, $WORD);
    $WORD (*atanh) (math$$RealFuns, $WORD);
};
struct math$$RealFuns {
    struct math$$RealFuns$class *$class;
};
extern struct math$$RealFuns$class math$$RealFuns$methods;
math$$RealFuns math$$RealFuns$new();
struct math$$RealFuns$float;
typedef struct math$$RealFuns$float *math$$RealFuns$float;
struct math$$RealFuns$float$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (math$$RealFuns$float);
    $NoneType (*__serialize__) (math$$RealFuns$float, $Serial$state);
    math$$RealFuns$float (*__deserialize__) (math$$RealFuns$float, $Serial$state);
    $bool (*__bool__)(math$$RealFuns$float);
    $str (*__str__)(math$$RealFuns$float);
    $float (*sqrt) (math$$RealFuns$float, $float);
    $float (*exp) (math$$RealFuns$float, $float);
    $float (*log) (math$$RealFuns$float, $float);
    $float (*sin) (math$$RealFuns$float, $float);
    $float (*cos) (math$$RealFuns$float, $float);
    $float (*tan) (math$$RealFuns$float, $float);
    $float (*asin) (math$$RealFuns$float, $float);
    $float (*acos) (math$$RealFuns$float, $float);
    $float (*atan) (math$$RealFuns$float, $float);
    $float (*sinh) (math$$RealFuns$float, $float);
    $float (*cosh) (math$$RealFuns$float, $float);
    $float (*tanh) (math$$RealFuns$float, $float);
    $float (*asinh) (math$$RealFuns$float, $float);
    $float (*acosh) (math$$RealFuns$float, $float);
    $float (*atanh) (math$$RealFuns$float, $float);
};
struct math$$RealFuns$float {
    struct math$$RealFuns$float$class *$class;
};
extern struct math$$RealFuns$float$class math$$RealFuns$float$methods;
math$$RealFuns$float math$$RealFuns$float$new();
$WORD math$$sqrt (math$$RealFuns, $WORD);
$WORD math$$exp (math$$RealFuns, $WORD);
$WORD math$$log (math$$RealFuns, $WORD);
$WORD math$$sin (math$$RealFuns, $WORD);
$WORD math$$cos (math$$RealFuns, $WORD);
$WORD math$$tan (math$$RealFuns, $WORD);
$WORD math$$asin (math$$RealFuns, $WORD);
$WORD math$$acos (math$$RealFuns, $WORD);
$WORD math$$atan (math$$RealFuns, $WORD);
$WORD math$$sinh (math$$RealFuns, $WORD);
$WORD math$$cosh (math$$RealFuns, $WORD);
$WORD math$$tanh (math$$RealFuns, $WORD);
$WORD math$$asinh (math$$RealFuns, $WORD);
$WORD math$$acosh (math$$RealFuns, $WORD);
$WORD math$$atanh (math$$RealFuns, $WORD);
void math$$__init__ ();
