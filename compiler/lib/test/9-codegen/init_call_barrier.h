/* Acton impl hash: test-hash */
#pragma once
#include "builtin/builtin.h"
#include "rts/rts.h"
$R init_call_barrierQ_step ($Cont);
struct init_call_barrierQ_Base;
struct init_call_barrierQ_Derived;
struct init_call_barrierQ_Later;
struct init_call_barrierQ_Renew;
typedef struct $Cont_R2 *init_call_barrierQ_L_2Cont;
typedef struct init_call_barrierQ_Base *init_call_barrierQ_Base;
typedef struct init_call_barrierQ_Derived *init_call_barrierQ_Derived;
typedef struct init_call_barrierQ_Later *init_call_barrierQ_Later;
typedef struct init_call_barrierQ_Renew *init_call_barrierQ_Renew;
$R init_call_barrierQ_L_1C_1cont (init_call_barrierQ_Later, $Cont, B_NoneType);
struct init_call_barrierQ_BaseG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__) (init_call_barrierQ_Base);
    void (*__serialize__) (init_call_barrierQ_Base, $Serial$state);
    init_call_barrierQ_Base (*__deserialize__) (init_call_barrierQ_Base, $Serial$state);
    bool (*__bool__) (init_call_barrierQ_Base);
    B_str (*__str__) (init_call_barrierQ_Base);
    B_str (*__repr__) (init_call_barrierQ_Base);
    B_value (*__get_attr__) (init_call_barrierQ_Base, B_str);
    B_NoneType (*G_init) (init_call_barrierQ_Base);
};
struct init_call_barrierQ_Base {
    struct init_call_barrierQ_BaseG_class *$class;
    B_list items;
};
struct init_call_barrierQ_DerivedG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__) (init_call_barrierQ_Derived);
    void (*__serialize__) (init_call_barrierQ_Derived, $Serial$state);
    init_call_barrierQ_Derived (*__deserialize__) (init_call_barrierQ_Derived, $Serial$state);
    bool (*__bool__) (init_call_barrierQ_Derived);
    B_str (*__str__) (init_call_barrierQ_Derived);
    B_str (*__repr__) (init_call_barrierQ_Derived);
    B_value (*__get_attr__) (init_call_barrierQ_Derived, B_str);
    B_NoneType (*G_init) (init_call_barrierQ_Derived);
};
struct init_call_barrierQ_Derived {
    struct init_call_barrierQ_DerivedG_class *$class;
    B_list items;
};
struct init_call_barrierQ_LaterG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    $R (*__init__) (init_call_barrierQ_Later, $Cont);
    void (*__serialize__) (init_call_barrierQ_Later, $Serial$state);
    init_call_barrierQ_Later (*__deserialize__) (init_call_barrierQ_Later, $Serial$state);
    bool (*__bool__) (init_call_barrierQ_Later);
    B_str (*__str__) (init_call_barrierQ_Later);
    B_str (*__repr__) (init_call_barrierQ_Later);
    B_value (*__get_attr__) (init_call_barrierQ_Later, B_str);
    B_NoneType (*G_init) (init_call_barrierQ_Later);
};
struct init_call_barrierQ_Later {
    struct init_call_barrierQ_LaterG_class *$class;
    B_list items;
};
struct init_call_barrierQ_RenewG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__) (init_call_barrierQ_Base);
    void (*__serialize__) (init_call_barrierQ_Renew, $Serial$state);
    init_call_barrierQ_Renew (*__deserialize__) (init_call_barrierQ_Renew, $Serial$state);
    bool (*__bool__) (init_call_barrierQ_Renew);
    B_str (*__str__) (init_call_barrierQ_Renew);
    B_str (*__repr__) (init_call_barrierQ_Renew);
    B_value (*__get_attr__) (init_call_barrierQ_Renew, B_str);
    B_NoneType (*G_init) (init_call_barrierQ_Renew);
    B_NoneType (*hook) (init_call_barrierQ_Renew);
};
struct init_call_barrierQ_Renew {
    struct init_call_barrierQ_RenewG_class *$class;
    B_NoneType items;
};
extern struct $ContClass init_call_barrierQ_L_2ContG_methods;
$R init_call_barrierQ_L_2ContD___call__($Cont $closure, $WORD $result);
$Cont init_call_barrierQ_L_2ContD___deserialize__($Cont self, $Serial$state state);
extern struct init_call_barrierQ_BaseG_class init_call_barrierQ_BaseG_methods;
init_call_barrierQ_Base init_call_barrierQ_BaseG_new();
B_value init_call_barrierQ_BaseD___get_attr__(init_call_barrierQ_Base self, B_str name);
B_NoneType init_call_barrierQ_BaseG_init(init_call_barrierQ_Base self);
B_NoneType init_call_barrierQ_BaseD___init__(init_call_barrierQ_Base self);
extern struct init_call_barrierQ_DerivedG_class init_call_barrierQ_DerivedG_methods;
init_call_barrierQ_Derived init_call_barrierQ_DerivedG_new();
B_value init_call_barrierQ_DerivedD___get_attr__(init_call_barrierQ_Derived self, B_str name);
B_NoneType init_call_barrierQ_DerivedG_init(init_call_barrierQ_Derived self);
B_NoneType init_call_barrierQ_DerivedD___init__(init_call_barrierQ_Derived self);
extern struct init_call_barrierQ_LaterG_class init_call_barrierQ_LaterG_methods;
$R init_call_barrierQ_LaterG_new($Cont);
B_value init_call_barrierQ_LaterD___get_attr__(init_call_barrierQ_Later self, B_str name);
B_NoneType init_call_barrierQ_LaterG_init(init_call_barrierQ_Later self);
$R init_call_barrierQ_LaterD___init__(init_call_barrierQ_Later self, $Cont C_cont);
extern struct init_call_barrierQ_RenewG_class init_call_barrierQ_RenewG_methods;
B_value init_call_barrierQ_RenewD___get_attr__(init_call_barrierQ_Renew self, B_str name);
B_NoneType init_call_barrierQ_RenewG_init(init_call_barrierQ_Renew self);
B_NoneType init_call_barrierQ_RenewD___init__(init_call_barrierQ_Base b);
B_NoneType init_call_barrierQ_reinit (init_call_barrierQ_Base);
B_NoneType init_call_barrierQ_renew (init_call_barrierQ_Base);
void init_call_barrierQ___init__ ();