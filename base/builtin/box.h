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
};

$Box $BoxG_new($WORD);

extern struct $BoxG_class $BoxG_methods;

struct B_BoolD_$Box;
typedef struct B_BoolD_$Box *B_BoolD_$Box;

struct B_BoolD_$BoxG_class;
typedef struct B_BoolD_$BoxG_class *B_BoolD_$BoxG_class;

extern struct B_BoolD_$BoxG_class B_BoolD_$BoxG_methods;
B_BoolD_$Box B_BoolD_$BoxG_new();

struct B_ShowD_$Box;
typedef struct B_ShowD_$Box *B_ShowD_$Box;

struct B_ShowD_$BoxG_class;
typedef struct B_ShowD_$BoxG_class *B_ShowD_$BoxG_class;

extern struct B_ShowD_$BoxG_class B_ShowD_$BoxG_methods;
B_ShowD_$Box B_ShowD_$BoxG_new();

