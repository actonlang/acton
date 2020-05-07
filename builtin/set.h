struct $set$class {
  char *$GCINFO;
  $Super$class $superclass;
  void (*__init__)($set, $Hashable, $Iterable$opaque);
  void (*__serialize__)($set, $Mapping$dict, long*, $dict, struct $ROWLISTHEADER*);
  $set (*__deserialize__)($Mapping$dict, $ROW*, $dict);
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
