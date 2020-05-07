struct $dict$class {
  char *$GCINFO;
  $Super$class $superclass;
  void(*__init__)($dict, $Hashable, $Iterable$opaque);
  void (*__serialize__)($dict, $Mapping$dict, long*, $dict, struct $ROWLISTHEADER*);
  $dict (*__deserialize__)($Mapping$dict, $ROW*, $dict);
};

typedef struct $table_struct *$table;

struct $dict {
  struct $dict$class *$class;
  long numelements;               // nr of elements in dictionary
  $table table;                   // the hashtable
};

extern struct $dict$class $dict$methods;

extern struct  $Mapping$dict$class $Mapping$dict$methods;
extern struct  $Indexed$dict$class $Indexed$dict$methods;


