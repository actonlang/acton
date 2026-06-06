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

typedef struct B_IteratorD_dict_values *B_IteratorD_dict_values;

struct B_IteratorD_dict_valuesG_class {
    char *$GCINFO;
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

extern struct B_IteratorD_dict_itemsG_class  B_IteratorD_dict_itemsG_methods;
B_IteratorD_dict_items B_IteratorD_dict_itemsG_new(B_dict);


// Convenience methods used for (de)serialization
void B_dictD_setitem(B_dict dict, B_Hashable hashwit, $WORD key, $WORD value);
$WORD B_dictD_get(B_dict dict, B_Hashable hashwit, $WORD key, $WORD deflt);

B_bool B_OrdD_dictD___eq__ (B_OrdD_dict w, B_dict a, B_dict b);
B_bool B_OrdD_dictD___ne__ (B_OrdD_dict w, B_dict a, B_dict b);
B_bool B_OrdD_dictD___lt__ (B_OrdD_dict w, B_dict a, B_dict b);
B_bool B_OrdD_dictD___le__ (B_OrdD_dict w, B_dict a, B_dict b);
B_bool B_OrdD_dictD___ge__ (B_OrdD_dict w, B_dict a, B_dict b);
B_bool B_OrdD_dictD___gt__ (B_OrdD_dict w, B_dict a, B_dict b);
$WORD B_IndexedD_MappingD_dictD___getitem__(B_IndexedD_MappingD_dict wit, B_dict dict, $WORD key);
B_NoneType B_IndexedD_MappingD_dictD___setitem__ (B_IndexedD_MappingD_dict wit, B_dict dict, $WORD key, $WORD value);
B_NoneType B_IndexedD_MappingD_dictD___delitem__ (B_IndexedD_MappingD_dict wit, B_dict dict, $WORD key);
B_Iterator B_MappingD_dictD___iter__ (B_MappingD_dict wit, B_dict dict);
B_dict B_MappingD_dictD___fromiter__ (B_MappingD_dict wit, B_Iterable wit2, $WORD iter);
int64_t B_MappingD_dictD___len__ (B_MappingD_dict wit, B_dict dict);
B_bool B_MappingD_dictD___contains__ (B_MappingD_dict wit, B_dict dict, $WORD key);
B_bool B_MappingD_dictD___containsnot__ (B_MappingD_dict wit, B_dict dict, $WORD key);
$WORD B_MappingD_dictD_get (B_MappingD_dict wit, B_dict dict, $WORD key);
$WORD B_MappingD_dictD_get_def (B_MappingD_dict wit, B_dict dict, $WORD key, $WORD deflt);
$WORD B_MappingD_dictD_pop(B_MappingD_dict wit, B_dict dict, $WORD key);
$WORD B_MappingD_dictD_pop_def(B_MappingD_dict wit, B_dict dict, $WORD key, $WORD deflt);
B_Iterator B_MappingD_dictD_keys (B_MappingD_dict wit, B_dict dict);
B_Iterator B_MappingD_dictD_values (B_MappingD_dict wit, B_dict dict);
B_Iterator B_MappingD_dictD_items (B_MappingD_dict wit, B_dict dict);
B_NoneType B_MappingD_dictD_update (B_MappingD_dict wit, B_dict dict, B_Iterable wit2, $WORD other);
B_tuple B_MappingD_dictD_popitem (B_MappingD_dict wit, B_dict dict);
