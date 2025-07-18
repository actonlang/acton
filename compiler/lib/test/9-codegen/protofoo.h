#pragma once
#include "builtin/builtin.h"
#include "rts/rts.h"
extern B_Eq protofooQ_W_14;
extern B_Hashable protofooQ_W_194;
struct protofooQ_L_2Cont;
struct protofooQ_L_3proc;
struct protofooQ_Key;
struct protofooQ_HashableD_Key;
struct protofooQ_main;
typedef struct protofooQ_L_2Cont *protofooQ_L_2Cont;
typedef struct protofooQ_L_3proc *protofooQ_L_3proc;
typedef struct protofooQ_Key *protofooQ_Key;
typedef struct protofooQ_HashableD_Key *protofooQ_HashableD_Key;
typedef struct protofooQ_main *protofooQ_main;
$R protofooQ_L_1C_1cont ($Cont, protofooQ_main, B_NoneType);
struct protofooQ_L_2ContG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__) (protofooQ_L_2Cont, $Cont, protofooQ_main);
    void (*__serialize__) (protofooQ_L_2Cont, $Serial$state);
    protofooQ_L_2Cont (*__deserialize__) (protofooQ_L_2Cont, $Serial$state);
    B_bool (*__bool__) (protofooQ_L_2Cont);
    B_str (*__str__) (protofooQ_L_2Cont);
    B_str (*__repr__) (protofooQ_L_2Cont);
    $R (*__call__) (protofooQ_L_2Cont, B_NoneType);
};
struct protofooQ_L_2Cont {
    struct protofooQ_L_2ContG_class *$class;
    $Cont C_cont;
    protofooQ_main G_act;
};
struct protofooQ_L_3procG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__) (protofooQ_L_3proc, protofooQ_main, B_Env);
    void (*__serialize__) (protofooQ_L_3proc, $Serial$state);
    protofooQ_L_3proc (*__deserialize__) (protofooQ_L_3proc, $Serial$state);
    B_bool (*__bool__) (protofooQ_L_3proc);
    B_str (*__str__) (protofooQ_L_3proc);
    B_str (*__repr__) (protofooQ_L_3proc);
    $R (*__call__) (protofooQ_L_3proc, $Cont);
    $R (*__exec__) (protofooQ_L_3proc, $Cont);
};
struct protofooQ_L_3proc {
    struct protofooQ_L_3procG_class *$class;
    protofooQ_main G_act;
    B_Env env;
};
struct protofooQ_KeyG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__) (protofooQ_Key, B_int);
    void (*__serialize__) (protofooQ_Key, $Serial$state);
    protofooQ_Key (*__deserialize__) (protofooQ_Key, $Serial$state);
    B_bool (*__bool__) (protofooQ_Key);
    B_str (*__str__) (protofooQ_Key);
    B_str (*__repr__) (protofooQ_Key);
};
struct protofooQ_Key {
    struct protofooQ_KeyG_class *$class;
    B_int x;
};
struct protofooQ_HashableD_KeyG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__) (protofooQ_HashableD_Key);
    void (*__serialize__) (protofooQ_HashableD_Key, $Serial$state);
    protofooQ_HashableD_Key (*__deserialize__) (protofooQ_HashableD_Key, $Serial$state);
    B_bool (*__bool__) (protofooQ_HashableD_Key);
    B_str (*__str__) (protofooQ_HashableD_Key);
    B_str (*__repr__) (protofooQ_HashableD_Key);
    B_bool (*__eq__) (protofooQ_HashableD_Key, protofooQ_Key, protofooQ_Key);
    B_bool (*__ne__) (protofooQ_HashableD_Key, protofooQ_Key, protofooQ_Key);
    B_NoneType (*hash) (protofooQ_HashableD_Key, protofooQ_Key, B_hasher);
};
struct protofooQ_HashableD_Key {
    struct protofooQ_HashableD_KeyG_class *$class;
};
struct protofooQ_mainG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    $R (*__init__) (protofooQ_main, $Cont, B_Env);
    void (*__serialize__) (protofooQ_main, $Serial$state);
    protofooQ_main (*__deserialize__) (protofooQ_main, $Serial$state);
    B_bool (*__bool__) (protofooQ_main);
    B_str (*__str__) (protofooQ_main);
    B_str (*__repr__) (protofooQ_main);
    B_NoneType (*__resume__) (protofooQ_main);
    B_NoneType (*__cleanup__) (protofooQ_main);
};
struct protofooQ_main {
    struct protofooQ_mainG_class *$class;
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
    protofooQ_Key k;
};
$R protofooQ_mainG_newact ($Cont, B_Env);
extern struct protofooQ_L_2ContG_class protofooQ_L_2ContG_methods;
protofooQ_L_2Cont protofooQ_L_2ContG_new($Cont, protofooQ_main);
extern struct protofooQ_L_3procG_class protofooQ_L_3procG_methods;
protofooQ_L_3proc protofooQ_L_3procG_new(protofooQ_main, B_Env);
extern struct protofooQ_KeyG_class protofooQ_KeyG_methods;
protofooQ_Key protofooQ_KeyG_new(B_int);
extern struct protofooQ_HashableD_KeyG_class protofooQ_HashableD_KeyG_methods;
protofooQ_HashableD_Key protofooQ_HashableD_KeyG_new();
extern struct protofooQ_mainG_class protofooQ_mainG_methods;
$R protofooQ_mainG_new($Cont, B_Env);
void protofooQ___init__ ();