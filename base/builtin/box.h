struct $Box;

typedef struct $Box *$Box;

struct $Box {
    struct $BoxG_class *$class;
    $WORD val;
};

struct $BoxG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__) ($Box, $WORD);
    void (*__serialize__) ($Box, $Serial$state);
    $Box (*__deserialize__) ($Box, $Serial$state);
    B_bool (*__bool__) ($Box);
    B_str (*__str__) ($Box);
    B_str (*__repr__) ($Box);
};

$Box $BoxG_new($WORD);

extern struct $BoxG_class $BoxG_methods;
