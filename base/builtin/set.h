
typedef struct {
    $WORD key;
    uint64_t hash;
} B_setentry;

typedef struct {
    B_Hashable hashwit;     // element hashing/equality policy, set at construction
    uint64_t numelements;    // nr of elements
    uint64_t fill;           // numelements + #dummy entries
    uint64_t mask;
    uint64_t finger;         // Search finger for pop()
    B_setentry *table;       // the hashtable
} B_set_table;

// Mutable and immutable sets share this storage view, including their policy.
typedef struct B_set_base *B_set_base;
struct __attribute__((__may_alias__)) B_set_base {
    $SuperG_class $class;
    B_set_table data;
};

struct B_set {
    struct B_setG_class *$class;
    B_set_table data;
};

struct B_iset {
    struct B_isetG_class *$class;
    B_set_table data;
};

_Static_assert(sizeof(struct B_set) == sizeof(struct B_iset),
               "set and iset must have the same size for freeze()");
_Static_assert(offsetof(struct B_set, data) == offsetof(struct B_iset, data),
               "set and iset must have the same table offset for freeze()");


_Static_assert(sizeof(struct B_set_base) == sizeof(struct B_set),
               "set base and set must have the same size");
_Static_assert(offsetof(struct B_set_base, data) == offsetof(struct B_set, data),
               "set base and set must have the same table offset");

// Direct table operations. Generated calls project the shared data field.
bool B_set_table_contains(B_set_table *, $WORD);
bool B_set_table_eq(B_set_table *, B_set_table *);
bool B_set_table_gt(B_set_table *, B_set_table *);
bool B_set_table_ge(B_set_table *, B_set_table *);
bool B_set_table_isdisjoint(B_set_table *, B_set_table *);
B_NoneType B_set_table_add(B_set_table *, $WORD);
B_NoneType B_set_table_discard(B_set_table *, $WORD);
$WORD B_set_table_pop(B_set_table *);
B_NoneType B_set_hash_table(B_set_table *, B_hasher);
B_Iterator B_set_base_iter(B_set_base);
B_NoneType B_set_base_update(B_set_base, B_Iterable, $WORD);
B_set_base B_set_base_union(B_set_table *, B_set_table *, $SuperG_class);
B_set_base B_set_base_intersection(B_set_table *, B_set_table *, $SuperG_class);
B_set_base B_set_base_difference(B_set_table *, B_set_table *, $SuperG_class);
B_set_base B_set_base_symmetric_difference(B_set_table *, B_set_table *, $SuperG_class);

// These adapters only retain the receiver result. Always inline them so even
// debug builds call the existing table operation without another call layer.
void B_set_table_ior(B_set_table *, B_set_table *);
void B_set_table_iand(B_set_table *, B_set_table *);
void B_set_table_isub(B_set_table *, B_set_table *);
void B_set_table_ixor(B_set_table *, B_set_table *);
static inline __attribute__((always_inline)) B_set_base B_set_base_ior(B_set_base self, B_set_base other) {
    B_set_table_ior(&self->data, &other->data);
    return self;
}
static inline __attribute__((always_inline)) B_set_base B_set_base_iand(B_set_base self, B_set_base other) {
    B_set_table_iand(&self->data, &other->data);
    return self;
}
static inline __attribute__((always_inline)) B_set_base B_set_base_isub(B_set_base self, B_set_base other) {
    B_set_table_isub(&self->data, &other->data);
    return self;
}
static inline __attribute__((always_inline)) B_set_base B_set_base_ixor(B_set_base self, B_set_base other) {
    B_set_table_ixor(&self->data, &other->data);
    return self;
}

// Iterators over sets ///////////////////////////////////////////////////////

typedef struct B_IteratorD_set *B_IteratorD_set; ;

struct B_IteratorD_setG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_IteratorD_set, B_set);
    void (*__serialize__)(B_IteratorD_set, $Serial$state);
    B_IteratorD_set (*__deserialize__)(B_IteratorD_set, $Serial$state);
    bool (*__bool__)(B_IteratorD_set);
    B_str (*__str__)(B_IteratorD_set);
    B_str (*__repr__)(B_IteratorD_set);
    bool (*__next__)(B_IteratorD_set, $WORD *);
};

struct B_IteratorD_set {
    struct B_IteratorD_setG_class *$class;
    $WORD src;
    B_set_table *data;
    uint64_t nxt;
};

extern struct  B_IteratorD_setG_class  B_IteratorD_setG_methods;
B_IteratorD_set B_IteratorD_setG_new(B_set);
B_Iterator $setD_U_iter_init(B_IteratorD_set iter, B_set src);
B_Iterator $isetD_U_iter_init(B_IteratorD_set iter, B_iset src);

void B_set_add_entry(B_set set, $WORD key, uint64_t hash);

// Witness-free length getters used by generated code.
static inline int64_t $setD_U__len(B_set self) {
    return (int64_t)self->data.numelements;
}
static inline int64_t $isetD_U__len(B_iset self) {
    return (int64_t)self->data.numelements;
}
