struct B_listG_class {
  char *$GCINFO;
  int $class_id;
  $SuperG_class $superclass;
  void (*__init__)(B_list, B_Iterable, $WORD);
  void (*__serialize__)(B_list,$Serial$state);
  B_list (*__deserialize__)(B_list,$Serial$state);
  B_bool (*__bool__)(B_list);
  B_str (*__str__)(B_list);
  B_str (*__repr__)(B_list);
  B_list(*copy)(B_list);
  //  B_int (*sort)(B_list self, int (*cmp)($WORD,$WORD));
};

struct B_list {
  struct B_listG_class *$class;
  $WORD *data;
  int length;
  int capacity;
};

extern struct B_listG_class B_listG_methods;
B_list B_listG_new(B_Iterable, $WORD);

extern struct B_SequenceD_listG_class B_SequenceD_listG_methods;
B_SequenceD_list B_SequenceD_listG_new();
extern struct B_CollectionD_SequenceD_listG_class B_CollectionD_SequenceD_listG_methods;
B_CollectionD_SequenceD_list B_CollectionD_SequenceD_listG_new(B_Sequence);
extern struct B_TimesD_SequenceD_listG_class B_TimesD_SequenceD_listG_methods;
B_TimesD_SequenceD_list B_TimesD_SequenceD_listG_new(B_Sequence);
extern struct B_ContainerD_listG_class B_ContainerD_listG_methods;
B_ContainerD_list B_ContainerD_listG_new(B_Eq);
extern struct B_OrdD_listG_class B_OrdD_listG_methods;
B_OrdD_list B_OrdD_listG_new(B_Ord);

extern struct B_SequenceD_list *B_SequenceD_listG_witness;
extern struct B_CollectionD_SequenceD_list *B_CollectionD_SequenceD_listG_witness;

#define $SequenceD_listG_witness B_SequenceD_listG_witness
#define $CollectionD_listG_witness B_CollectionD_SequenceD_listG_witness

// void B_printlist(B_list list); //for debugging; only for lists of ints

// Iterators over lists ///////////////////////////////////////////////////////

typedef struct B_IteratorD_list *B_IteratorD_list; ;

struct B_IteratorD_listG_class {
  char *$GCINFO;
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

extern struct  B_IteratorD_listG_class  B_IteratorD_listG_methods;
B_IteratorD_list B_IteratorD_listG_new(B_list);

