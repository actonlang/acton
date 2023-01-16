struct B_atomG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_atom, $WORD);
    void (*__serialize__)(B_atom,$Serial$state);
    B_atom (*__deserialize__)(B_atom,$Serial$state);
    B_bool (*__bool__)(B_atom);
    B_str (*__str__)(B_atom);
    B_str (*__repr__)(B_atom);
};

struct B_atom {
    struct B_atomG_class *$class;
};

extern struct B_atomG_class B_atomG_methods;


