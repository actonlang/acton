 
struct B_list {
  struct B_listG_class *$class;
  $WORD *data;
  int length;
  int capacity;
};
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
  $WORD(*__next__)(B_IteratorD_list);
};

struct B_IteratorD_list {
  struct B_IteratorD_listG_class *$class;
  B_list src;
  int nxt;
};

extern struct  B_IteratorD_listG_class  B_IteratorD_listG_methods;
B_IteratorD_list B_IteratorD_listG_new(B_list);

//convenience functions used at various places.
B_list B_listD_new(int capacity);
B_list B_listD_copy(B_list lst);
