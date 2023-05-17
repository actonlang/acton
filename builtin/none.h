struct B_NoneTypeG_class {
    GC_descr $GCdescr;
    char *$name;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_NoneType);
    void (*__serialize__)(B_NoneType,$Serial$state);
    B_NoneType (*__deserialize__)(B_NoneType,$Serial$state);
    B_bool (*__bool__)(B_NoneType);
    B_str (*__str__)(B_NoneType);
    B_str (*__repr__)(B_NoneType);
};

struct B_NoneType {
    struct B_NoneTypeG_class *$class;
};
extern GC_word B_NoneTypeD_gcbm[GC_BITMAP_SIZE(struct B_NoneType)];

extern struct B_NoneTypeG_class B_NoneTypeG_methods;
B_NoneType B_NoneTypeG_new();
