#pragma once
#include "builtin/builtin.h"
#include "builtin/minienv.h"
#include "rts/rts.h"
struct time$$RealFuns;
typedef struct time$$RealFuns *time$$RealFuns;
struct time$$RealFuns$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (time$$RealFuns);
    $NoneType (*__serialize__) (time$$RealFuns, $Serial$state);
    time$$RealFuns (*__deserialize__) (time$$RealFuns, $Serial$state);
    $bool (*__bool__)(time$$RealFuns);
    $str (*__str__)(time$$RealFuns);
    $WORD (*time) (time$$RealFuns, $WORD);
};
struct time$$RealFuns {
    struct time$$RealFuns$class *$class;
};
extern struct time$$RealFuns$class time$$RealFuns$methods;
time$$RealFuns time$$RealFuns$new();
struct time$$RealFuns$float;
typedef struct time$$RealFuns$float *time$$RealFuns$float;
struct time$$RealFuns$float$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (time$$RealFuns$float);
    $NoneType (*__serialize__) (time$$RealFuns$float, $Serial$state);
    time$$RealFuns$float (*__deserialize__) (time$$RealFuns$float, $Serial$state);
    $bool (*__bool__)(time$$RealFuns$float);
    $str (*__str__)(time$$RealFuns$float);
    int (*time) (time$$RealFuns$float, $float);
};
struct time$$RealFuns$float {
    struct time$$RealFuns$float$class *$class;
};
extern struct time$$RealFuns$float$class time$$RealFuns$float$methods;
time$$RealFuns$float time$$RealFuns$float$new();
$WORD time$$time (time$$RealFuns, $WORD);
void time$$__init__ ();
