struct $NoneTypeG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($NoneType);
    void (*__serialize__)($NoneType,$Serial$state);
    $NoneType (*__deserialize__)($NoneType,$Serial$state);
    B_bool (*__bool__)($NoneType);
    B_str (*__str__)($NoneType);
    B_str (*__repr__)($NoneType);
};

struct $NoneType {
    struct $NoneTypeG_class *$class;
};

extern struct $NoneTypeG_class $NoneTypeG_methods;
$NoneType $NoneTypeG_new();
