struct numpy$$ndselect;
typedef struct numpy$$ndselect *numpy$$ndselect;
struct numpy$$ndselect$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__) (numpy$$ndselect);
    void (*__serialize__) (numpy$$ndselect, $Serial$state);
    numpy$$ndselect (*__deserialize__) ($Serial$state);
    $bool (*__bool__) (numpy$$ndselect);
    $str (*__str__) (numpy$$ndselect);
};
struct numpy$$ndselect {
    struct numpy$$ndselect$class *$class;
};
extern struct numpy$$ndselect$class numpy$$ndselect$methods;
numpy$$ndselect numpy$$ndselect$new();
struct numpy$$ndindex;
typedef struct numpy$$ndindex *numpy$$ndindex;
struct numpy$$ndindex$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__) (numpy$$ndindex, $int);
    void (*__serialize__) (numpy$$ndindex, $Serial$state);
    numpy$$ndindex (*__deserialize__) ($Serial$state);
    $bool (*__bool__) (numpy$$ndindex);
    $str (*__str__) (numpy$$ndindex);
};
struct numpy$$ndindex {
    struct numpy$$ndindex$class *$class;
    $int index;
};
extern struct numpy$$ndindex$class numpy$$ndindex$methods;
numpy$$ndindex numpy$$ndindex$new($int);


struct numpy$$ndslice;
typedef struct numpy$$ndslice *numpy$$ndslice;
struct numpy$$ndslice$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__) (numpy$$ndslice, $Slice);
    void (*__serialize__) (numpy$$ndslice, $Serial$state);
    numpy$$ndslice (*__deserialize__) ($Serial$state);
    $bool (*__bool__) (numpy$$ndslice);
    $str (*__str__) (numpy$$ndslice);
};
struct numpy$$ndslice {
    struct numpy$$ndslice$class *$class;
    $Slice slc;
};
extern struct numpy$$ndslice$class numpy$$ndslice$methods;
numpy$$ndslice numpy$$ndslice$new($Slice);

