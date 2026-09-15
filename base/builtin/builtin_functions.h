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
    bool (*__bool__)($EqOpt);
    B_str (*__str__)($EqOpt);
    B_str (*__repr__)($EqOpt);
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
    bool (*__bool__)($EqTuple);
    B_str (*__str__)($EqTuple);
    B_str (*__repr__)($EqTuple);
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
    bool (*__bool__)($OrdTuple);
    B_str (*__str__)($OrdTuple);
    B_str (*__repr__)($OrdTuple);
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
    bool (*__bool__)($HashableTuple);
    B_str (*__str__)($HashableTuple);
    B_str (*__repr__)($HashableTuple);
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
    bool (*__bool__)($IdentityActor);
    B_str (*__str__)($IdentityActor);
    B_str (*__repr__)($IdentityActor);
    bool (*__is__)($IdentityActor, $WORD, $WORD);
    bool (*__isnot__)($IdentityActor, $WORD, $WORD);
};

struct $IdentityActor {
    struct $IdentityActorG_class *$class;
};

extern struct $IdentityActorG_class $IdentityActorG_methods;
$IdentityActor $IdentityActorG_new();

$WORD $ASSERT(bool, B_str);
