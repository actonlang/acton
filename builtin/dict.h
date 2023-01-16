struct B_dictG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void(*__init__)(B_dict, B_Hashable, B_Iterable, $WORD);
    void (*__serialize__)(B_dict,$Serial$state);
    B_dict (*__deserialize__)(B_dict,$Serial$state);
    B_bool (*__bool__)(B_dict);
    B_str (*__str__)(B_dict);
    B_str (*__repr__)(B_dict);
};

typedef struct $table_struct *$table;

struct B_dict {
    struct B_dictG_class *$class;
    long numelements;               // nr of elements in dictionary
    $table table;                   // the hashtable
};

extern struct B_dictG_class B_dictG_methods;
B_dict B_dictG_new(B_Hashable, B_Iterable, $WORD);

extern struct  B_MappingD_dictG_class B_MappingD_dictG_methods;
B_MappingD_dict B_MappingD_dictG_new(B_Hashable);
extern struct  B_IndexedD_MappingD_dictG_class B_IndexedD_MappingD_dictG_methods;
B_IndexedD_MappingD_dict B_IndexedD_MappingD_dictG_new(B_Mapping, B_Eq);
extern struct  B_OrdD_dictG_class B_OrdD_dictG_methods;
B_OrdD_dict B_OrdD_dictG_new(B_Hashable, B_Eq);

// Iterators over dicts ///////////////////////////////////////////////////////

// keys iterator

typedef struct B_IteratorD_dict *B_IteratorD_dict;

struct B_IteratorD_dictG_class {
    char *$GCINFO;
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

extern struct B_IteratorD_dictG_class  B_IteratorD_dictG_methods;
B_IteratorD_dict B_IteratorD_dictG_new(B_dict);

// values iterator

typedef struct B_InteratorD_dict_values *B_InteratorD_dict_values;

struct B_InteratorD_dict_valuesG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_InteratorD_dict_values, B_dict);
    void (*__serialize__)(B_InteratorD_dict_values,$Serial$state);
    B_InteratorD_dict_values (*__deserialize__)(B_InteratorD_dict_values,$Serial$state);
    B_bool (*__bool__)(B_InteratorD_dict_values);
    B_str (*__str__)(B_InteratorD_dict_values);
    B_str (*__repr__)(B_InteratorD_dict_values);
    $WORD(*__next__)(B_InteratorD_dict_values);
};

struct B_InteratorD_dict_values {
    struct B_InteratorD_dict_valuesG_class *$class;
    B_dict src;
    int nxt;
};

extern struct B_InteratorD_dict_valuesG_class  B_InteratorD_dict_valuesG_methods;
B_InteratorD_dict_values B_InteratorD_dict_valuesG_new(B_dict);

// items iterator

typedef struct B_InteratorD_dict_items *B_InteratorD_dict_items;

struct B_InteratorD_dict_itemsG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_InteratorD_dict_items, B_dict);
    void (*__serialize__)(B_InteratorD_dict_items,$Serial$state);
    B_InteratorD_dict_items (*__deserialize__)(B_InteratorD_dict_items,$Serial$state);
    B_bool (*__bool__)(B_InteratorD_dict_items);
    B_str (*__str__)(B_InteratorD_dict_items);
    B_str (*__repr__)(B_InteratorD_dict_items);
    $WORD(*__next__)(B_InteratorD_dict_items);
};

struct B_InteratorD_dict_items {
    struct B_InteratorD_dict_itemsG_class *$class;
    B_dict src;
    int nxt;
};

extern struct B_InteratorD_dict_itemsG_class  B_InteratorD_dict_itemsG_methods;
B_InteratorD_dict_items B_InteratorD_dict_itemsG_new(B_dict);
