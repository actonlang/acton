/* Acton impl hash: test-hash */
#pragma once
#include "builtin/builtin.h"
#include "rts/rts.h"
struct witness_forwardQ_PA;
struct witness_forwardQ_PB;
struct witness_forwardQ_PC;
struct witness_forwardQ_Thing;
struct witness_forwardQ_PBD_Thing;
struct witness_forwardQ_PCD_Thing;
typedef struct witness_forwardQ_PA *witness_forwardQ_PA;
typedef struct witness_forwardQ_PB *witness_forwardQ_PB;
typedef struct witness_forwardQ_PC *witness_forwardQ_PC;
typedef struct witness_forwardQ_Thing *witness_forwardQ_Thing;
typedef struct witness_forwardQ_PBD_Thing *witness_forwardQ_PBD_Thing;
typedef struct witness_forwardQ_PCD_Thing *witness_forwardQ_PCD_Thing;
struct witness_forwardQ_PAG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__) (witness_forwardQ_PA);
    void (*__serialize__) (witness_forwardQ_PA, $Serial$state);
    witness_forwardQ_PA (*__deserialize__) (witness_forwardQ_PA, $Serial$state);
    bool (*__bool__) (witness_forwardQ_PA);
    B_str (*__str__) (witness_forwardQ_PA);
    B_str (*__repr__) (witness_forwardQ_PA);
    bool (*same) (witness_forwardQ_PA, $WORD, $WORD);
    int64_t (*total) (witness_forwardQ_PA, $WORD);
};
struct witness_forwardQ_PA {
    struct witness_forwardQ_PAG_class *$class;
};
struct witness_forwardQ_PBG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__) (witness_forwardQ_PB);
    void (*__serialize__) (witness_forwardQ_PB, $Serial$state);
    witness_forwardQ_PB (*__deserialize__) (witness_forwardQ_PB, $Serial$state);
    bool (*__bool__) (witness_forwardQ_PB);
    B_str (*__str__) (witness_forwardQ_PB);
    B_str (*__repr__) (witness_forwardQ_PB);
    bool (*same) (witness_forwardQ_PB, $WORD, $WORD);
    int64_t (*total) (witness_forwardQ_PB, $WORD);
    int64_t (*beta) (witness_forwardQ_PB, $WORD);
};
struct witness_forwardQ_PB {
    struct witness_forwardQ_PBG_class *$class;
};
struct witness_forwardQ_PCG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__) (witness_forwardQ_PC);
    void (*__serialize__) (witness_forwardQ_PC, $Serial$state);
    witness_forwardQ_PC (*__deserialize__) (witness_forwardQ_PC, $Serial$state);
    bool (*__bool__) (witness_forwardQ_PC);
    B_str (*__str__) (witness_forwardQ_PC);
    B_str (*__repr__) (witness_forwardQ_PC);
    bool (*same) (witness_forwardQ_PC, $WORD, $WORD);
    int64_t (*total) (witness_forwardQ_PC, $WORD);
    int64_t (*gamma) (witness_forwardQ_PC, $WORD);
};
struct witness_forwardQ_PC {
    struct witness_forwardQ_PCG_class *$class;
};
struct witness_forwardQ_ThingG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__) (witness_forwardQ_Thing, int64_t);
    void (*__serialize__) (witness_forwardQ_Thing, $Serial$state);
    witness_forwardQ_Thing (*__deserialize__) (witness_forwardQ_Thing, $Serial$state);
    bool (*__bool__) (witness_forwardQ_Thing);
    B_str (*__str__) (witness_forwardQ_Thing);
    B_str (*__repr__) (witness_forwardQ_Thing);
    B_value (*__get_attr__) (witness_forwardQ_Thing, B_str);
    B_NoneType (*G_init) (witness_forwardQ_Thing);
};
struct witness_forwardQ_Thing {
    struct witness_forwardQ_ThingG_class *$class;
    int64_t n;
};
struct witness_forwardQ_PBD_ThingG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__) (witness_forwardQ_PBD_Thing);
    void (*__serialize__) (witness_forwardQ_PBD_Thing, $Serial$state);
    witness_forwardQ_PBD_Thing (*__deserialize__) (witness_forwardQ_PBD_Thing, $Serial$state);
    bool (*__bool__) (witness_forwardQ_PBD_Thing);
    B_str (*__str__) (witness_forwardQ_PBD_Thing);
    B_str (*__repr__) (witness_forwardQ_PBD_Thing);
    bool (*same) (witness_forwardQ_PBD_Thing, witness_forwardQ_Thing, witness_forwardQ_Thing);
    int64_t (*total) (witness_forwardQ_PBD_Thing, witness_forwardQ_Thing);
    int64_t (*beta) (witness_forwardQ_PBD_Thing, witness_forwardQ_Thing);
};
struct witness_forwardQ_PBD_Thing {
    struct witness_forwardQ_PBD_ThingG_class *$class;
};
struct witness_forwardQ_PCD_ThingG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__) (witness_forwardQ_PCD_Thing);
    void (*__serialize__) (witness_forwardQ_PCD_Thing, $Serial$state);
    witness_forwardQ_PCD_Thing (*__deserialize__) (witness_forwardQ_PCD_Thing, $Serial$state);
    bool (*__bool__) (witness_forwardQ_PCD_Thing);
    B_str (*__str__) (witness_forwardQ_PCD_Thing);
    B_str (*__repr__) (witness_forwardQ_PCD_Thing);
    bool (*same) (witness_forwardQ_PCD_Thing, witness_forwardQ_Thing, witness_forwardQ_Thing);
    int64_t (*total) (witness_forwardQ_PCD_Thing, witness_forwardQ_Thing);
    int64_t (*gamma) (witness_forwardQ_PCD_Thing, witness_forwardQ_Thing);
};
struct witness_forwardQ_PCD_Thing {
    struct witness_forwardQ_PCD_ThingG_class *$class;
};
extern struct witness_forwardQ_PAG_class witness_forwardQ_PAG_methods;
B_NoneType witness_forwardQ_PAD___init__(witness_forwardQ_PA W_self);
extern struct witness_forwardQ_PBG_class witness_forwardQ_PBG_methods;
B_NoneType witness_forwardQ_PBD___init__(witness_forwardQ_PB W_self);
extern struct witness_forwardQ_PCG_class witness_forwardQ_PCG_methods;
B_NoneType witness_forwardQ_PCD___init__(witness_forwardQ_PC W_self);
extern struct witness_forwardQ_ThingG_class witness_forwardQ_ThingG_methods;
witness_forwardQ_Thing witness_forwardQ_ThingG_new(int64_t);
B_value witness_forwardQ_ThingD___get_attr__(witness_forwardQ_Thing self, B_str name);
B_NoneType witness_forwardQ_ThingG_init(witness_forwardQ_Thing self);
B_NoneType witness_forwardQ_ThingD___init__(witness_forwardQ_Thing self, int64_t n);
extern struct witness_forwardQ_PBD_ThingG_class witness_forwardQ_PBD_ThingG_methods;
witness_forwardQ_PBD_Thing witness_forwardQ_PBD_ThingG_new();
B_NoneType witness_forwardQ_PBD_ThingD___init__(witness_forwardQ_PBD_Thing W_self);
bool witness_forwardQ_PBD_ThingD_same(witness_forwardQ_PBD_Thing W_self, witness_forwardQ_Thing a, witness_forwardQ_Thing b);
int64_t witness_forwardQ_PBD_ThingD_total(witness_forwardQ_PBD_Thing W_self, witness_forwardQ_Thing self);
int64_t witness_forwardQ_PBD_ThingD_beta(witness_forwardQ_PBD_Thing W_self, witness_forwardQ_Thing self);
extern struct witness_forwardQ_PCD_ThingG_class witness_forwardQ_PCD_ThingG_methods;
B_NoneType witness_forwardQ_PCD_ThingD___init__(witness_forwardQ_PCD_Thing W_self);
int64_t witness_forwardQ_PCD_ThingD_gamma(witness_forwardQ_PCD_Thing W_self, witness_forwardQ_Thing self);
void witness_forwardQ___init__ ();