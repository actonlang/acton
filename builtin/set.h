struct $set$class {
  char *GCINFO;
  $None (*__serialize__)($set, $Mapping$dict, $WORD*, int, $dict, $ROWLISTHEADER);
  $set (*__deserialize__)($Mapping$dict, $ROW*, $dict);
  $set(*copy)($set);
};

typedef struct {
  $WORD key;
  long hash;    
} $setentry;

typedef struct $set {
  struct $set$class *class;
  long numelements;    // nr of elements in $set
  long fill;           // numelements + #dummy entries
    /* The table contains mask + 1 slots, and that's a power of 2.
     * We store the mask instead of the size because the mask is more
     * frequently needed.
     */
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
