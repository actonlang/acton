#pragma once
#include "builtin/builtin.h"
#include "rts/rts.h"
#include "out/types/b.h"
struct testQ_L_2Cont;
struct testQ_L_3proc;
struct testQ_main;
typedef struct testQ_L_2Cont *testQ_L_2Cont;
typedef struct testQ_L_3proc *testQ_L_3proc;
typedef struct testQ_main *testQ_main;
$R testQ_L_1C_1cont ($Cont, testQ_main, B_NoneType);
struct testQ_L_2ContG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__) (testQ_L_2Cont, $Cont, testQ_main);
    void (*__serialize__) (testQ_L_2Cont, $Serial$state);
    testQ_L_2Cont (*__deserialize__) (testQ_L_2Cont, $Serial$state);
    B_bool (*__bool__) (testQ_L_2Cont);
    B_str (*__str__) (testQ_L_2Cont);
    B_str (*__repr__) (testQ_L_2Cont);
    $R (*__call__) (testQ_L_2Cont, B_NoneType);
};
struct testQ_L_2Cont {
    struct testQ_L_2ContG_class *$class;
    $Cont C_cont;
    testQ_main G_act;
};
struct testQ_L_3procG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__) (testQ_L_3proc, testQ_main, B_Env);
    void (*__serialize__) (testQ_L_3proc, $Serial$state);
    testQ_L_3proc (*__deserialize__) (testQ_L_3proc, $Serial$state);
    B_bool (*__bool__) (testQ_L_3proc);
    B_str (*__str__) (testQ_L_3proc);
    B_str (*__repr__) (testQ_L_3proc);
    $R (*__call__) (testQ_L_3proc, $Cont);
    $R (*__exec__) (testQ_L_3proc, $Cont);
};
struct testQ_L_3proc {
    struct testQ_L_3procG_class *$class;
    testQ_main G_act;
    B_Env env;
};
struct testQ_mainG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    $R (*__init__) (testQ_main, $Cont, B_Env);
    void (*__serialize__) (testQ_main, $Serial$state);
    testQ_main (*__deserialize__) (testQ_main, $Serial$state);
    B_bool (*__bool__) (testQ_main);
    B_str (*__str__) (testQ_main);
    B_str (*__repr__) (testQ_main);
    B_NoneType (*__resume__) (testQ_main);
    B_NoneType (*__cleanup__) (testQ_main);
};
struct testQ_main {
    struct testQ_mainG_class *$class;
    $Actor $next;
    B_Msg $msg;
    B_Msg $msg_tail;
    $Lock $msg_lock;
    $int64 $affinity;
    B_Msg $outgoing;
    B_Msg $waitsfor;
    $int64 $consume_hd;
    $Catcher $catcher;
    $long $globkey;
};
$R testQ_mainG_newact ($Cont, B_Env);
extern struct testQ_L_2ContG_class testQ_L_2ContG_methods;
testQ_L_2Cont testQ_L_2ContG_new($Cont, testQ_main);
extern struct testQ_L_3procG_class testQ_L_3procG_methods;
testQ_L_3proc testQ_L_3procG_new(testQ_main, B_Env);
extern struct testQ_mainG_class testQ_mainG_methods;
$R testQ_mainG_new($Cont, B_Env);
void testQ___init__ ();