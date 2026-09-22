typedef struct $table_struct *$table;

/*
 * Common storage shared by dict and idict.  Keep the concrete structures: C
 * runtime clients access dict fields directly.  dict.c uses B_dict_base so the
 * hash-table implementation is independent of the public mutability wrapper.
 */
typedef struct B_dict_base *B_dict_base;

struct __attribute__((__may_alias__)) B_dict_base {
    $SuperG_class $class;
    long numelements;
    $table table;
};

struct B_dict {
    struct B_dictG_class *$class;
    long numelements;               // nr of elements in dictionary
    $table table;                   // the hashtable
};

struct B_idict {
    struct B_idictG_class *$class;
    long numelements;
    $table table;
};

_Static_assert(sizeof(struct B_dict_base) == sizeof(struct B_dict),
               "generic dict and dict must have the same size");
_Static_assert(sizeof(struct B_dict_base) == sizeof(struct B_idict),
               "generic dict and idict must have the same size");
_Static_assert(offsetof(struct B_dict_base, numelements) == offsetof(struct B_dict, numelements),
               "generic dict and dict must have the same element-count offset");
_Static_assert(offsetof(struct B_dict_base, numelements) == offsetof(struct B_idict, numelements),
               "generic dict and idict must have the same element-count offset");
_Static_assert(offsetof(struct B_dict_base, table) == offsetof(struct B_dict, table),
               "generic dict and dict must have the same table offset");
_Static_assert(offsetof(struct B_dict_base, table) == offsetof(struct B_idict, table),
               "generic dict and idict must have the same table offset");

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
    bool (*__bool__)(B_IteratorD_dict);
    B_str (*__str__)(B_IteratorD_dict);
    B_str (*__repr__)(B_IteratorD_dict);
    bool (*__next__)(B_IteratorD_dict, $WORD *);
};

struct B_IteratorD_dict {
    struct B_IteratorD_dictG_class *$class;
    $WORD src;
    B_dict_base data;
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
    bool (*__bool__)(B_IteratorD_dict_values);
    B_str (*__str__)(B_IteratorD_dict_values);
    B_str (*__repr__)(B_IteratorD_dict_values);
    bool (*__next__)(B_IteratorD_dict_values, $WORD *);
};

struct B_IteratorD_dict_values {
    struct B_IteratorD_dict_valuesG_class *$class;
    $WORD src;
    B_dict_base data;
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
    bool (*__bool__)(B_IteratorD_dict_items);
    B_str (*__str__)(B_IteratorD_dict_items);
    B_str (*__repr__)(B_IteratorD_dict_items);
    bool (*__next__)(B_IteratorD_dict_items, $WORD *);
};

struct B_IteratorD_dict_items {
    struct B_IteratorD_dict_itemsG_class *$class;
    $WORD src;
    B_dict_base data;
    int nxt;
};

extern struct B_IteratorD_dict_itemsG_class  B_IteratorD_dict_itemsG_methods;
B_IteratorD_dict_items B_IteratorD_dict_itemsG_new(B_dict);


// Convenience methods used for (de)serialization
void B_dictD_setitem(B_dict dict, B_Hashable hashwit, $WORD key, $WORD value);
$WORD B_dictD_get(B_dict dict, B_Hashable hashwit, $WORD key, $WORD deflt);
