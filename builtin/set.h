struct $set$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($set, $Hashable, $Iterable, $WORD);
  void (*__serialize__)($set, $Serial$state);
  $set (*__deserialize__)($set, $Serial$state);
  $bool (*__bool__)($set);
  $str (*__str__)($set);
  $set(*copy)($set, $Hashable);
};

typedef struct {
  $WORD key;
  long hash;    
} $setentry;

typedef struct $set {
  struct $set$class *$class;
  long numelements;    // nr of elements in $set
  long fill;           // numelements + #dummy entries
  long mask;
  long finger;                       // Search finger for pop() 
  $setentry *table;                  // the hashtable
} *$set;


extern struct $set$class $set$methods;
$set $set$new($Hashable, $Iterable, $WORD);

extern struct $Set$set$class $Set$set$methods;
$Set$set $Set$set$new($Hashable);
extern struct $Ord$set$class $Ord$set$methods;
//$Ord$set $Ord$set$new($Set$set);
extern struct $Minus$set$class $Minus$set$methods;
//$Minus$set $Minus$set$new($Set$set);
extern struct $Logical$set$class $Logical$set$methods;
//$Logical$set $Logical$set$new($Set$set);

extern struct $Set$set *$Set$set_new($Hashable);

// Iterators over sets ///////////////////////////////////////////////////////

typedef struct $Iterator$set *$Iterator$set; ;

struct $Iterator$set$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($Iterator$set, $set);
  void (*__serialize__)($Iterator$set, $Serial$state);
  $Iterator$set (*__deserialize__)($Iterator$set, $Serial$state);
  $bool (*__bool__)($Iterator$set);
  $str (*__str__)($Iterator$set);
  $WORD(*__next__)($Iterator$set);
};

struct $Iterator$set {
  struct $Iterator$set$class *$class;
  $set src;
  int nxt;
};

extern struct  $Iterator$set$class  $Iterator$set$methods;
$Iterator$set $Iterator$set$new($set);
