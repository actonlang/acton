 
/*
 * Common prefix shared by list and ilist.  The concrete structs deliberately
 * retain their historical field layout: quite a bit of runtime C code reads
 * list fields directly.  list.c operates on B_list_base and exposes only thin
 * concrete-class wrappers.
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

//convenience functions used at various places.
B_list B_listD_new(int capacity);
B_list B_listD_copy(B_list lst);
B_ilist B_ilistD_new(int capacity);

$WORD $listD_U__getitem__(B_list lst, int64_t n);
B_NoneType listD_U__setitem__(B_list lst, int64_t n, $WORD val);
