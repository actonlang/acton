struct $list$class {
  char *$GCINFO;
  void (*__init__)($list);
  void (*__serialize__)($list, $Mapping$dict, $WORD*, int, $dict, $ROWLISTHEADER);
  $list (*__deserialize__)($Mapping$dict, $ROW*, $dict);
  $list(*copy)($list);
  //  $int (*sort)($list self, int (*cmp)($WORD,$WORD));
};


struct $list {
  struct $list$class *$class;
  $WORD *data;
  int length;
  int capacity;
};

extern struct $list$class $list$methods;


extern struct $Sequence$list$class $Sequence$list$methods;
extern struct $Container$list$class $Container$list$methods; 

extern struct $Sequence$list *$Sequence$list$witness;
extern struct $Container$list *$Container$list_new($Eq); // equality is for elements

void $printlist($list list); //for debugging; only for lists of ints

