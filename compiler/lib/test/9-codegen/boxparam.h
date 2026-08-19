/* Acton codegen hash: test-hash */
#pragma once
#include "builtin/builtin.h"
#include "rts/rts.h"
struct boxparamQ_Base;
struct boxparamQ_Deriv;
typedef struct boxparamQ_Base *boxparamQ_Base;
typedef struct boxparamQ_Deriv *boxparamQ_Deriv;
struct boxparamQ_BaseG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__) (boxparamQ_Base);
    void (*__serialize__) (boxparamQ_Base, $Serial$state);
    boxparamQ_Base (*__deserialize__) (boxparamQ_Base, $Serial$state);
    bool (*__bool__) (boxparamQ_Base);
    B_str (*__str__) (boxparamQ_Base);
    B_str (*__repr__) (boxparamQ_Base);
    B_value (*__get_attr__) (boxparamQ_Base, B_str);
    B_NoneType (*G_init) (boxparamQ_Base);
    bool (*cmp) (boxparamQ_Base, $WORD);
    $WORD (*bump) (boxparamQ_Base, $WORD);
    $WORD (*fwd) (boxparamQ_Base, $WORD);
};
struct boxparamQ_Base {
    struct boxparamQ_BaseG_class *$class;
};
struct boxparamQ_DerivG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__) (boxparamQ_Deriv);
    void (*__serialize__) (boxparamQ_Deriv, $Serial$state);
    boxparamQ_Deriv (*__deserialize__) (boxparamQ_Deriv, $Serial$state);
    bool (*__bool__) (boxparamQ_Deriv);
    B_str (*__str__) (boxparamQ_Deriv);
    B_str (*__repr__) (boxparamQ_Deriv);
    B_value (*__get_attr__) (boxparamQ_Deriv, B_str);
    B_NoneType (*G_init) (boxparamQ_Deriv);
    bool (*cmp) (boxparamQ_Deriv, B_int);
    B_int (*bump) (boxparamQ_Deriv, B_int);
    B_int (*fwd) (boxparamQ_Deriv, B_int);
};
struct boxparamQ_Deriv {
    struct boxparamQ_DerivG_class *$class;
};
extern struct boxparamQ_BaseG_class boxparamQ_BaseG_methods;
boxparamQ_Base boxparamQ_BaseG_new();
B_value boxparamQ_BaseD___get_attr__(boxparamQ_Base self, B_str name);
B_NoneType boxparamQ_BaseG_init(boxparamQ_Base self);
B_NoneType boxparamQ_BaseD___init__(boxparamQ_Base self);
bool boxparamQ_BaseD_cmp(boxparamQ_Base self, $WORD x);
$WORD boxparamQ_BaseD_bump(boxparamQ_Base self, $WORD x);
$WORD boxparamQ_BaseD_fwd(boxparamQ_Base self, $WORD x);
extern struct boxparamQ_DerivG_class boxparamQ_DerivG_methods;
boxparamQ_Deriv boxparamQ_DerivG_new();
B_NoneType boxparamQ_DerivD___init__(boxparamQ_Deriv G_1p);
B_value boxparamQ_DerivD___get_attr__(boxparamQ_Deriv self, B_str name);
B_NoneType boxparamQ_DerivG_init(boxparamQ_Deriv self);
bool boxparamQ_DerivD_cmp(boxparamQ_Deriv self, B_int x);
B_int boxparamQ_DerivD_bump(boxparamQ_Deriv self, B_int xD_boxed);
B_int boxparamQ_DerivD_fwd(boxparamQ_Deriv self, B_int x);
void boxparamQ___init__ ();