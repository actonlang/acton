struct $list$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($list, $Iterable, $WORD);
  void (*__serialize__)($list,$Serial$state);
  $list (*__deserialize__)($list,$Serial$state);
  $bool (*__bool__)($list);
  $str (*__str__)($list);
  $str (*__repr__)($list);
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
$list $list$new($Iterable, $WORD);

extern struct $Sequence$list$class $Sequence$list$methods;
$Sequence$list $Sequence$list$new();
extern struct $Collection$list$class $Collection$list$methods;
$Collection$list $Collection$list$new($Sequence);
extern struct $Times$list$class $Times$list$methods;
$Times$list $Times$list$new($Sequence);
extern struct $Container$list$class $Container$list$methods;
$Container$list $Container$list$new($Eq);
extern struct $Eq$list$class $Eq$list$methods;
$Eq$list $Eq$list$new($Eq);

extern struct $Sequence$list *$Sequence$list$witness;
extern struct $Collection$list *$Collection$list$witness;


// void $printlist($list list); //for debugging; only for lists of ints

// Iterators over lists ///////////////////////////////////////////////////////

typedef struct $Iterator$list *$Iterator$list; ;

struct $Iterator$list$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($Iterator$list, $list);
  void (*__serialize__)($Iterator$list,$Serial$state);
  $Iterator$list (*__deserialize__)($Iterator$list,$Serial$state);
  $bool (*__bool__)($Iterator$list);
  $str (*__str__)($Iterator$list);
  $str (*__repr__)($Iterator$list);
  $WORD(*__next__)($Iterator$list);
};

struct $Iterator$list {
  struct $Iterator$list$class *$class;
  $list src;
  int nxt;
};

extern struct  $Iterator$list$class  $Iterator$list$methods;
$Iterator$list $Iterator$list$new($list);

