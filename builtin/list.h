 
struct B_list {
  struct B_listG_class *$class;
  $WORD *data;
  int length;
  int capacity;
};
extern GC_word B_listD_gcbm[GC_BITMAP_SIZE(struct B_list)];
/*
extern struct B_SequenceD_list *B_SequenceD_listG_witness;
extern struct B_CollectionD_SequenceD_list *B_CollectionD_SequenceD_listG_witness;
*/

// Iterators over lists ///////////////////////////////////////////////////////

typedef struct B_IteratorD_list *B_IteratorD_list; ;

struct B_IteratorD_listG_class {
  GC_descr $GCdescr;
  char *$name;
  int $class_id;
  $SuperG_class $superclass;
  void (*__init__)(B_IteratorD_list, B_list);
  void (*__serialize__)(B_IteratorD_list,$Serial$state);
  B_IteratorD_list (*__deserialize__)(B_IteratorD_list,$Serial$state);
  B_bool (*__bool__)(B_IteratorD_list);
  B_str (*__str__)(B_IteratorD_list);
  B_str (*__repr__)(B_IteratorD_list);
  $WORD(*__next__)(B_IteratorD_list);
};

struct B_IteratorD_list {
  struct B_IteratorD_listG_class *$class;
  B_list src;
  int nxt;
};
extern GC_word B_IteratorD_listD_gcbm[GC_BITMAP_SIZE(struct B_IteratorD_list)];

extern struct  B_IteratorD_listG_class  B_IteratorD_listG_methods;
B_IteratorD_list B_IteratorD_listG_new(B_list);

//convenience functions used at various places.
B_list B_listD_new(int capacity);
B_list B_listD_copy(B_list lst);
