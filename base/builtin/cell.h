struct $Cell;

typedef struct $Cell *$Cell;

struct $Cell {
    struct $CellG_class *$class;
    $WORD cell;
};

struct $CellG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__) ($Cell, $WORD);
    void (*__serialize__) ($Cell, $Serial$state);
    $Cell (*__deserialize__) ($Cell, $Serial$state);
    bool (*__bool__) ($Cell);
    B_str (*__str__) ($Cell);
    B_str (*__repr__) ($Cell);
};

$Cell $CellG_new($WORD);

extern struct $CellG_class $CellG_methods;
