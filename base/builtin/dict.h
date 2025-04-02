typedef struct $table_struct *$table;

struct B_dict {
    struct B_dictG_class *$class;
    long numelements;               // nr of elements in dictionary
    $table table;                   // the hashtable
};

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

typedef struct B_IteratorD_dict_values *B_IteratorD_dict_values;

struct B_IteratorD_dict_valuesG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_IteratorD_dict_values, B_dict);
    void (*__serialize__)(B_IteratorD_dict_values,$Serial$state);
    B_IteratorD_dict_values (*__deserialize__)(B_IteratorD_dict_values,$Serial$state);
    $WORD(*__next__)(B_IteratorD_dict_values);
};

struct B_IteratorD_dict_values {
    struct B_IteratorD_dict_valuesG_class *$class;
    B_dict src;
    int nxt;
};

extern struct B_IteratorD_dict_valuesG_class  B_IteratorD_dict_valuesG_methods;
B_IteratorD_dict_values B_IteratorD_dict_valuesG_new(B_dict);

// items iterator

typedef struct B_IteratorD_dict_items *B_IteratorD_dict_items;

struct B_IteratorD_dict_itemsG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_IteratorD_dict_items, B_dict);
    void (*__serialize__)(B_IteratorD_dict_items,$Serial$state);
    B_IteratorD_dict_items (*__deserialize__)(B_IteratorD_dict_items,$Serial$state);
    $WORD(*__next__)(B_IteratorD_dict_items);
};

struct B_IteratorD_dict_items {
    struct B_IteratorD_dict_itemsG_class *$class;
    B_dict src;
    int nxt;
};

extern struct B_IteratorD_dict_itemsG_class  B_IteratorD_dict_itemsG_methods;
B_IteratorD_dict_items B_IteratorD_dict_itemsG_new(B_dict);


// Convenience methods used for (de)serialization
void B_dictD_setitem(B_dict dict, B_Hashable hashwit, $WORD key, $WORD value);
$WORD B_dictD_get(B_dict dict, B_Hashable hashwit, $WORD key, $WORD deflt);
