typedef struct $table_struct *$table;

struct B_dict {
    struct B_dictG_class *$class;
    long numelements;               // nr of elements in dictionary
    $table table;                   // the hashtable
};
extern GC_word B_dictD_gcbm[GC_BITMAP_SIZE(struct B_dict)];

// Iterators over dicts ///////////////////////////////////////////////////////

// keys iterator

typedef struct B_IteratorD_dict *B_IteratorD_dict;

struct B_IteratorD_dictG_class {
    GC_descr $GCdescr;
    char *$name;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_IteratorD_dict, B_dict);
    void (*__serialize__)(B_IteratorD_dict,$Serial$state);
    B_IteratorD_dict (*__deserialize__)(B_IteratorD_dict,$Serial$state);
    B_bool (*__bool__)(B_IteratorD_dict);
    B_str (*__str__)(B_IteratorD_dict);
    B_str (*__repr__)(B_IteratorD_dict);
    $WORD(*__next__)(B_IteratorD_dict);
};

struct B_IteratorD_dict {
    struct B_IteratorD_dictG_class *$class;
    B_dict src;
    int nxt;
};
extern GC_word B_IteratorD_dictD_gcbm[GC_BITMAP_SIZE(struct B_IteratorD_dict)];

extern struct B_IteratorD_dictG_class  B_IteratorD_dictG_methods;
B_IteratorD_dict B_IteratorD_dictG_new(B_dict);

// values iterator

typedef struct B_IteratorD_dict_values *B_IteratorD_dict_values;

struct B_IteratorD_dict_valuesG_class {
    GC_descr $GCdescr;
    char *$name;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_IteratorD_dict_values, B_dict);
    void (*__serialize__)(B_IteratorD_dict_values,$Serial$state);
    B_IteratorD_dict_values (*__deserialize__)(B_IteratorD_dict_values,$Serial$state);
    B_bool (*__bool__)(B_IteratorD_dict_values);
    B_str (*__str__)(B_IteratorD_dict_values);
    B_str (*__repr__)(B_IteratorD_dict_values);
    $WORD(*__next__)(B_IteratorD_dict_values);
};

struct B_IteratorD_dict_values {
    struct B_IteratorD_dict_valuesG_class *$class;
    B_dict src;
    int nxt;
};
extern GC_word B_IteratorD_dict_valuesD_gcbm[GC_BITMAP_SIZE(struct B_IteratorD_dict_values)];

extern struct B_IteratorD_dict_valuesG_class  B_IteratorD_dict_valuesG_methods;
B_IteratorD_dict_values B_IteratorD_dict_valuesG_new(B_dict);

// items iterator

typedef struct B_IteratorD_dict_items *B_IteratorD_dict_items;

struct B_IteratorD_dict_itemsG_class {
    GC_descr $GCdescr;
    char *$name;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_IteratorD_dict_items, B_dict);
    void (*__serialize__)(B_IteratorD_dict_items,$Serial$state);
    B_IteratorD_dict_items (*__deserialize__)(B_IteratorD_dict_items,$Serial$state);
    B_bool (*__bool__)(B_IteratorD_dict_items);
    B_str (*__str__)(B_IteratorD_dict_items);
    B_str (*__repr__)(B_IteratorD_dict_items);
    $WORD(*__next__)(B_IteratorD_dict_items);
};

struct B_IteratorD_dict_items {
    struct B_IteratorD_dict_itemsG_class *$class;
    B_dict src;
    int nxt;
};
extern GC_word B_IteratorD_dict_itemsD_gcbm[GC_BITMAP_SIZE(struct B_IteratorD_dict_items)];

extern struct B_IteratorD_dict_itemsG_class  B_IteratorD_dict_itemsG_methods;
B_IteratorD_dict_items B_IteratorD_dict_itemsG_new(B_dict);


// Convenience methods used for (de)serialization
void B_dictD_setitem(B_dict dict, B_Hashable hashwit, $WORD key, $WORD value);
$WORD B_dictD_get(B_dict dict, B_Hashable hashwit, $WORD key, $WORD deflt);
