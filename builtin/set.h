struct $set$class {
  char *$GCINFO;
  $Super$class $superclass;
  void (*__init__)($set, $Hashable, $Iterable$opaque);
  void (*__serialize__)($set, $Serial$state);
  $set (*__deserialize__)($Serial$state);
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

extern struct $Set$set$class $Set$set$methods;
extern struct $Ord$set$class $Ord$set$methods;
extern struct $Minus$set$class $Minus$set$methods;
extern struct $Logical$set$class $Logical$set$methods;

extern struct $Set$set *$Set$set_new($Hashable);

// Iterators over sets ///////////////////////////////////////////////////////

typedef struct $Iterator$set *$Iterator$set; ;

struct $Iterator$set$class {
  char *$GCINFO;
  $Super$class $superclass;
  void (*__init__)($Iterator$set, $set);
  void (*__serialize__)($Iterator$set, $Serial$state);
  $Iterator$set (*__deserialize__)($Serial$state);
  $WORD(*__next__)($Iterator$set);
};

struct $Iterator$set {
  struct $Iterator$set$class *$class;
  $set src;
  int nxt;
};

extern struct  $Iterator$set$class  $Iterator$set$methods;
