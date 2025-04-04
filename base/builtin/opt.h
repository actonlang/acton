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
    B_bool (*__eq__)($EqOpt, $WORD, $WORD);
    B_bool (*__ne__)($EqOpt, $WORD, $WORD);
};

struct $EqOpt {
    struct $EqOptG_class *$class;
    B_Eq W_Eq$A;
};

$EqOpt $EqOptG_new(B_Eq);

// ShowOpt //////////////////////////////////////////////////////

struct $ShowOpt;
typedef struct $ShowOpt *$ShowOpt;

struct $ShowOptG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($ShowOpt, B_Show);
    void (*__serialize__)($ShowOpt,$Serial$state);
    $ShowOpt (*__deserialize__)($ShowOpt,$Serial$state);
    B_str (*__str__)($ShowOpt, $WORD);
    B_str (*__repr__)($ShowOpt, $WORD);
};

struct $ShowOpt {
    struct $ShowOptG_class *$class;
    B_Show W_Show$A;
};

$ShowOpt $ShowOptG_new(B_Show);

