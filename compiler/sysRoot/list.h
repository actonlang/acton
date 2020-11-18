struct $list$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($list, $Sequence, $WORD);
  void (*__serialize__)($list,$Serial$state);
  $list (*__deserialize__)($Serial$state);
  $bool (*__bool__)($list);
  $str (*__str__)($list);
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
$list $list$new($Sequence, $WORD);

extern struct $Sequence$list$class $Sequence$list$methods;
$Sequence$list $Sequence$list$new();
extern struct $Container$list$class $Container$list$methods;
$Container$list $Container$list$new($Eq);

extern struct $Sequence$list *$Sequence$list$witness;
extern struct $Container$list *$Container$list_new($Eq); // equality is for elements

// void $printlist($list list); //for debugging; only for lists of ints

// Iterators over lists ///////////////////////////////////////////////////////

typedef struct $Iterator$list *$Iterator$list; ;

struct $Iterator$list$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($Iterator$list, $list);
  void (*__serialize__)($Iterator$list,$Serial$state);
  $Iterator$list (*__deserialize__)($Serial$state);
  $bool (*__bool__)($Iterator$list);
  $str (*__str__)($Iterator$list);
  $WORD(*__next__)($Iterator$list);
};

struct $Iterator$list {
  struct $Iterator$list$class *$class;
  $list src;
  int nxt;
};

extern struct  $Iterator$list$class  $Iterator$list$methods;
$Iterator$list $Iterator$list$new($list);

