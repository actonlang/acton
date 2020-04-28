struct $set$class {
  char *$GCINFO;
  void (*__init__)($set);
  void (*__serialize__)($set, $Mapping$dict, long*, $dict, $ROWLISTHEADER);
  $set (*__deserialize__)($Mapping$dict, $ROW*, $dict);
  $set(*copy)($set);
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
