#pragma once
 
// EqOpt //////////////////////////////////////////////////////

struct $EqOpt;
typedef struct $EqOpt *$EqOpt;

struct $EqOptG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($EqOpt, B_Eq);
    void (*__serialize__)($EqOpt,$Serial$state);
    $EqOpt (*__deserialize__)($EqOpt,$Serial$state);
    bool (*__eq__)($EqOpt, $WORD, $WORD);
    bool (*__ne__)($EqOpt, $WORD, $WORD);
};

struct $EqOpt {
    struct $EqOptG_class *$class;
    B_Eq W_Eq$A;
};

extern struct $EqOptG_class $EqOptG_methods;
$EqOpt $EqOptG_new(B_Eq);


// EqTuple //////////////////////////////////////////////////////

struct $EqTuple;
typedef struct $EqTuple *$EqTuple;

struct $EqTupleG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($EqTuple, B_tuple);
    void (*__serialize__)($EqTuple,$Serial$state);
    $EqTuple (*__deserialize__)($EqTuple,$Serial$state);
    bool (*__eq__)($EqTuple, $WORD, $WORD);
    bool (*__ne__)($EqTuple, $WORD, $WORD);
};

struct $EqTuple {
    struct $EqTupleG_class *$class;
    B_tuple W_Eq;
};

extern struct $EqTupleG_class $EqTupleG_methods;
$EqTuple $EqTupleG_new(B_tuple);


// OrdTuple //////////////////////////////////////////////////////

struct $OrdTuple;
typedef struct $OrdTuple *$OrdTuple;

struct $OrdTupleG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($OrdTuple, B_tuple);
    void (*__serialize__)($OrdTuple,$Serial$state);
    $OrdTuple (*__deserialize__)($OrdTuple,$Serial$state);
    bool (*__eq__)($OrdTuple, $WORD, $WORD);
    bool (*__ne__)($OrdTuple, $WORD, $WORD);
    bool (*__lt__)($OrdTuple, $WORD, $WORD);
    bool (*__le__)($OrdTuple, $WORD, $WORD);
    bool (*__gt__)($OrdTuple, $WORD, $WORD);
    bool (*__ge__)($OrdTuple, $WORD, $WORD);
};

struct $OrdTuple {
    struct $OrdTupleG_class *$class;
    B_tuple W_Ord;
};

extern struct $OrdTupleG_class $OrdTupleG_methods;
$OrdTuple $OrdTupleG_new(B_tuple);


// HashableTuple //////////////////////////////////////////////////////

struct $HashableTuple;
typedef struct $HashableTuple *$HashableTuple;

struct $HashableTupleG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($HashableTuple, B_tuple);
    void (*__serialize__)($HashableTuple,$Serial$state);
    $HashableTuple (*__deserialize__)($HashableTuple,$Serial$state);
    bool (*__eq__)($HashableTuple, $WORD, $WORD);
    bool (*__ne__)($HashableTuple, $WORD, $WORD);
    B_NoneType (*hash)($HashableTuple, $WORD, B_hasher);
};

struct $HashableTuple {
    struct $HashableTupleG_class *$class;
    B_tuple W_Hashable;
};

extern struct $HashableTupleG_class $HashableTupleG_methods;
$HashableTuple $HashableTupleG_new(B_tuple);


// wEqNone //////////////////////////////////////////////////////

extern struct B_EqG_class $wEqNoneG_class;
extern B_Eq $wEqNone;


// Show and Bool for optional values and tuples //////////////////////////////

struct $ShowOpt;
typedef struct $ShowOpt *$ShowOpt;

struct $ShowOptG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($ShowOpt, B_Show);
    void (*__serialize__)($ShowOpt, $Serial$state);
    $ShowOpt (*__deserialize__)($ShowOpt, $Serial$state);
    B_str (*__str__)($ShowOpt, $WORD);
    B_str (*__repr__)($ShowOpt, $WORD);
};

struct $ShowOpt {
    struct $ShowOptG_class *$class;
    B_Show W_Show;
};

extern struct $ShowOptG_class $ShowOptG_methods;
$ShowOpt $ShowOptG_new(B_Show);

struct $BoolOpt;
typedef struct $BoolOpt *$BoolOpt;

struct $BoolOptG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($BoolOpt, B_Bool);
    void (*__serialize__)($BoolOpt, $Serial$state);
    $BoolOpt (*__deserialize__)($BoolOpt, $Serial$state);
    bool (*__bool__)($BoolOpt, $WORD);
};

struct $BoolOpt {
    struct $BoolOptG_class *$class;
    B_Bool W_Bool;
};

extern struct $BoolOptG_class $BoolOptG_methods;
$BoolOpt $BoolOptG_new(B_Bool);

struct $ShowTuple;
typedef struct $ShowTuple *$ShowTuple;

struct $ShowTupleG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($ShowTuple, B_tuple);
    void (*__serialize__)($ShowTuple, $Serial$state);
    $ShowTuple (*__deserialize__)($ShowTuple, $Serial$state);
    B_str (*__str__)($ShowTuple, $WORD);
    B_str (*__repr__)($ShowTuple, $WORD);
};

struct $ShowTuple {
    struct $ShowTupleG_class *$class;
    B_tuple W_Show;
};

extern struct $ShowTupleG_class $ShowTupleG_methods;
$ShowTuple $ShowTupleG_new(B_tuple);
B_Show $ShowTupleJoin(B_Show, B_Show);

struct $BoolTuple;
typedef struct $BoolTuple *$BoolTuple;

struct $BoolTupleG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($BoolTuple);
    void (*__serialize__)($BoolTuple, $Serial$state);
    $BoolTuple (*__deserialize__)($BoolTuple, $Serial$state);
    bool (*__bool__)($BoolTuple, $WORD);
};

struct $BoolTuple {
    struct $BoolTupleG_class *$class;
};

extern struct $BoolTupleG_class $BoolTupleG_methods;
$BoolTuple $BoolTupleG_new(void);

extern struct B_ShowG_class $wShowNoneG_class;
extern B_Show $wShowNone;
extern struct B_BoolG_class $wBoolNoneG_class;
extern B_Bool $wBoolNone;


// IdentityActor //////////////////////////////////////////////////////

struct $IdentityActor;
typedef struct $IdentityActor *$IdentityActor;

struct $IdentityActorG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($IdentityActor);
    void (*__serialize__)($IdentityActor,$Serial$state);
    $IdentityActor (*__deserialize__)($IdentityActor,$Serial$state);
    bool (*__is__)($IdentityActor, $WORD, $WORD);
    bool (*__isnot__)($IdentityActor, $WORD, $WORD);
};

struct $IdentityActor {
    struct $IdentityActorG_class *$class;
};

extern struct $IdentityActorG_class $IdentityActorG_methods;
$IdentityActor $IdentityActorG_new();

$WORD $ASSERT(bool, B_str);
