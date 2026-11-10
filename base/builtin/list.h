 
/*
 * Common prefix shared by list and ilist.  The concrete structs deliberately
 * retain their historical field layout: quite a bit of runtime C code reads
 * list fields directly. Shared base entry points serve both generic protocol
 * wrappers and concrete calls in generated C.
 */
typedef struct B_list_base *B_list_base;

struct __attribute__((__may_alias__)) B_list_base {
  $SuperG_class $class;
  $WORD *data;
  int length;
  int capacity;
};

struct B_list {
  struct B_listG_class *$class;
  $WORD *data;
  int length;
  int capacity;
};

struct B_ilist {
  struct B_ilistG_class *$class;
  $WORD *data;
  int length;
  int capacity;
};

_Static_assert(sizeof(struct B_list_base) == sizeof(struct B_list),
               "generic list and list must have the same size");
_Static_assert(sizeof(struct B_list_base) == sizeof(struct B_ilist),
               "generic list and ilist must have the same size");
_Static_assert(offsetof(struct B_list_base, data) == offsetof(struct B_list, data),
               "generic list and list must have the same data offset");
_Static_assert(offsetof(struct B_list_base, data) == offsetof(struct B_ilist, data),
               "generic list and ilist must have the same data offset");
_Static_assert(offsetof(struct B_list_base, length) == offsetof(struct B_list, length),
               "generic list and list must have the same length offset");
_Static_assert(offsetof(struct B_list_base, length) == offsetof(struct B_ilist, length),
               "generic list and ilist must have the same length offset");
_Static_assert(offsetof(struct B_list_base, capacity) == offsetof(struct B_list, capacity),
               "generic list and list must have the same capacity offset");
_Static_assert(offsetof(struct B_list_base, capacity) == offsetof(struct B_ilist, capacity),
               "generic list and ilist must have the same capacity offset");
/*
extern struct B_SequenceD_list *B_SequenceD_listG_witness;
extern struct B_CollectionD_SequenceD_list *B_CollectionD_SequenceD_listG_witness;
*/

// Iterators over lists ///////////////////////////////////////////////////////

typedef struct B_IteratorD_list *B_IteratorD_list; ;

struct B_IteratorD_listG_class {
  char *$GCINFO;
  int $class_id;
  $SuperG_class $superclass;
  void (*__init__)(B_IteratorD_list, B_list);
  void (*__serialize__)(B_IteratorD_list,$Serial$state);
  B_IteratorD_list (*__deserialize__)(B_IteratorD_list,$Serial$state);
  bool (*__bool__)(B_IteratorD_list);
  B_str (*__str__)(B_IteratorD_list);
  B_str (*__repr__)(B_IteratorD_list);
  bool (*__next__)(B_IteratorD_list, $WORD *);
};

struct B_IteratorD_list {
  struct B_IteratorD_listG_class *$class;
  $WORD src;
  B_list_base data;
  int nxt;
};

extern struct  B_IteratorD_listG_class  B_IteratorD_listG_methods;
B_IteratorD_list B_IteratorD_listG_new(B_list);
B_Iterator B_list_base_iter_init(B_IteratorD_list iter, $WORD src);

//convenience functions used at various places.
B_list B_listD_new(int capacity);
B_list B_listD_copy(B_list lst);
B_ilist B_ilistD_new(int capacity);

// Shared operations called directly by generated code and protocol wrappers.
B_list_base B_list_base_new(int capacity, $SuperG_class cls);
B_NoneType B_list_base_append(B_list_base lst, $WORD elem);
$WORD B_list_base_getitem(B_list_base lst, int64_t n);
B_NoneType B_list_base_setitem(B_list_base lst, int64_t n, $WORD val);
B_NoneType B_list_base_delitem(B_list_base lst, int64_t n);
B_list_base B_list_base_getslice(B_list_base lst, B_slice slc, $SuperG_class cls);
B_Iterator B_list_base_iter(B_list_base lst);
B_Iterator B_list_base_reversed(B_list_base lst, $SuperG_class cls);
B_list_base B_list_base_add(B_list_base lst, B_list_base other, $SuperG_class cls);
B_list_base B_list_base_mul(B_list_base lst, int64_t n, $SuperG_class cls);
B_NoneType B_list_base_insert(B_list_base lst, int64_t n, $WORD elem);
B_NoneType B_list_base_reverse(B_list_base lst);
B_NoneType B_list_base_delslice(B_list_base lst, B_slice slc);
B_NoneType B_list_base_setslice(B_list_base lst, B_Iterable wit, B_slice slc, $WORD src);

static inline int64_t B_list_base_len(B_list_base lst) {
    return (int64_t)lst->length;
}

// Retain the receiver result without adding an out-of-line adapter call.
B_NoneType B_list_base_extend(B_list_base lst, B_list_base other);
void B_list_base_imul(B_list_base lst, int64_t n);
static inline __attribute__((always_inline)) B_list_base B_list_base_iadd(B_list_base lst, B_list_base other) {
    B_list_base_extend(lst, other);
    return lst;
}
static inline __attribute__((always_inline)) B_list_base B_list_base_irepeat(B_list_base lst, int64_t n) {
    B_list_base_imul(lst, n);
    return lst;
}
