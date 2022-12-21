struct $tuple$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($tuple,int,...);
    void (*__serialize__)($tuple,$Serial$state); 
    $tuple (*__deserialize__)($tuple,$Serial$state);
    $bool (*__bool__)($tuple);
    $str (*__str__)($tuple);
    $str (*__repr__)($tuple);
};

struct $tuple {
    struct $tuple$class *$class;
    int size;
    $WORD *components;
};

extern struct $tuple$class $tuple$methods;
$tuple $tuple$new(int,...);

#define $NEWTUPLE($len, ...)  ({ $tuple $t = malloc(sizeof(struct $tuple)+$len*sizeof($WORD)); \
            $t->$class = &$tuple$methods;                               \
            $t->$class->__init__($t, $len, __VA_ARGS__);                \
            $t; })

#define $NEWTUPLE0  ({ $tuple $t = malloc(sizeof(struct $tuple));       \
            $t->$class = &$tuple$methods;                               \
            $t->$class->__init__($t);                                   \
            $t; })

extern struct $Iterable$tuple$class $Iterable$tuple$methods;
$Iterable$tuple $Iterable$tuple$new();
extern struct $Sliceable$tuple$class $Sliceable$tuple$methods;
$Sliceable$tuple $Sliceable$tuple$new();
extern struct $Hashable$tuple$class $Hashable$tuple$methods;
$Hashable$tuple $Hashable$tuple$new();

extern struct $Iterable$tuple *$Iterable$tuple$witness;
extern struct $Sliceable$tuple *$Sliceable$tuple$witness;
extern struct $Hashable$tuple *$Hashable$tuple_new(int,$Hashable*);

// Iterators over tuples ///////////////////////////////////////////////////////

typedef struct $Iterator$tuple *$Iterator$tuple;

struct $Iterator$tuple$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Iterator$tuple, $tuple);
    void (*__serialize__)($Iterator$tuple,$Serial$state);
    $Iterator$tuple (*__deserialize__)($Iterator$tuple,$Serial$state);
    $bool (*__bool__)($Iterator$tuple);
    $str (*__str__)($Iterator$tuple);
    $str (*__repr__)($Iterator$tuple);
    $WORD(*__next__)($Iterator$tuple);
};

struct $Iterator$tuple {
    struct $Iterator$tuple$class *$class;
    $tuple src;
    int nxt;
};

extern struct $Iterator$tuple$class $Iterator$tuple$methods;
$Iterator$tuple $Iterator$tuple$new($tuple);
