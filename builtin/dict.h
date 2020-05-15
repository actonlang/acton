struct $dict$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void(*__init__)($dict, $Hashable, $Iterable$opaque);
  void (*__serialize__)($dict,$Serial$state);
  $dict (*__deserialize__)($Serial$state);
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

// Iterators over dicts ///////////////////////////////////////////////////////

// keys iterator

typedef struct $Iterator$dict *$Iterator$dict;

struct $Iterator$dict$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($Iterator$dict, $dict);
  void (*__serialize__)($Iterator$dict,$Serial$state);
  $Iterator$dict (*__deserialize__)($Serial$state);
  $WORD(*__next__)($Iterator$dict);
};

struct $Iterator$dict {
  struct $Iterator$dict$class *$class;
  $dict src;
  int nxt;
};

extern struct $Iterator$dict$class  $Iterator$dict$methods;

// values iterator

typedef struct $Iterator$dict$values *$Iterator$dict$values;

struct $Iterator$dict$values$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($Iterator$dict$values, $dict);
  void (*__serialize__)($Iterator$dict$values,$Serial$state);
  $Iterator$dict$values (*__deserialize__)($Serial$state);
  $WORD(*__next__)($Iterator$dict$values);
};

struct $Iterator$dict$values {
  struct $Iterator$dict$values$class *$class;
  $dict src;
  int nxt;
};

extern struct $Iterator$dict$values$class  $Iterator$dict$values$methods;

// items iterator

typedef struct $Iterator$dict$items *$Iterator$dict$items;

struct $Iterator$dict$items$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($Iterator$dict$items, $dict);
  void (*__serialize__)($Iterator$dict$items,$Serial$state);
  $Iterator$dict$items (*__deserialize__)($Serial$state);
  $WORD(*__next__)($Iterator$dict$items);
};

struct $Iterator$dict$items {
  struct $Iterator$dict$items$class *$class;
  $dict src;
  int nxt;
};

extern struct $Iterator$dict$items$class  $Iterator$dict$items$methods;
