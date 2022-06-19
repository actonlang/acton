struct $dict$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void(*__init__)($dict, $Hashable, $Iterable, $WORD);
  void (*__serialize__)($dict,$Serial$state);
  $dict (*__deserialize__)($dict,$Serial$state);
  $bool (*__bool__)($dict);
  $str (*__str__)($dict);
  $str (*__repr__)($dict);
};

typedef struct $table_struct *$table;

struct $dict {
  struct $dict$class *$class;
  long numelements;               // nr of elements in dictionary
  $table table;                   // the hashtable
};

extern struct $dict$class $dict$methods;
$dict $dict$new($Hashable, $Iterable, $WORD);

extern struct  $Mapping$dict$class $Mapping$dict$methods;
$Mapping$dict $Mapping$dict$new($Hashable);
extern struct  $Indexed$dict$class $Indexed$dict$methods;
$Indexed$dict $Indexed$dict$new($Mapping, $Eq);
extern struct  $Ord$dict$class $Ord$dict$methods;
$Ord$dict $Ord$dict$new($Hashable, $Eq);

// Iterators over dicts ///////////////////////////////////////////////////////

// keys iterator

typedef struct $Iterator$dict *$Iterator$dict;

struct $Iterator$dict$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($Iterator$dict, $dict);
  void (*__serialize__)($Iterator$dict,$Serial$state);
  $Iterator$dict (*__deserialize__)($Iterator$dict,$Serial$state);
  $bool (*__bool__)($Iterator$dict);
  $str (*__str__)($Iterator$dict);
  $str (*__repr__)($Iterator$dict);
  $WORD(*__next__)($Iterator$dict);
};

struct $Iterator$dict {
  struct $Iterator$dict$class *$class;
  $dict src;
  int nxt;
};

extern struct $Iterator$dict$class  $Iterator$dict$methods;
$Iterator$dict $Iterator$dict$new($dict);

// values iterator

typedef struct $Iterator$dict$values *$Iterator$dict$values;

struct $Iterator$dict$values$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($Iterator$dict$values, $dict);
  void (*__serialize__)($Iterator$dict$values,$Serial$state);
  $Iterator$dict$values (*__deserialize__)($Iterator$dict$values,$Serial$state);
  $bool (*__bool__)($Iterator$dict$values);
  $str (*__str__)($Iterator$dict$values);
  $str (*__repr__)($Iterator$dict$values);
  $WORD(*__next__)($Iterator$dict$values);
};

struct $Iterator$dict$values {
  struct $Iterator$dict$values$class *$class;
  $dict src;
  int nxt;
};

extern struct $Iterator$dict$values$class  $Iterator$dict$values$methods;
$Iterator$dict$values $Iterator$dict$values$new($dict);

// items iterator

typedef struct $Iterator$dict$items *$Iterator$dict$items;

struct $Iterator$dict$items$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($Iterator$dict$items, $dict);
  void (*__serialize__)($Iterator$dict$items,$Serial$state);
  $Iterator$dict$items (*__deserialize__)($Iterator$dict$items,$Serial$state);
  $bool (*__bool__)($Iterator$dict$items);
  $str (*__str__)($Iterator$dict$items);
  $str (*__repr__)($Iterator$dict$items);
  $WORD(*__next__)($Iterator$dict$items);
};

struct $Iterator$dict$items {
  struct $Iterator$dict$items$class *$class;
  $dict src;
  int nxt;
};

extern struct $Iterator$dict$items$class  $Iterator$dict$items$methods;
$Iterator$dict$items $Iterator$dict$items$new($dict);
