// print /////////////////////////////////////////////////////////////////

void print($tuple t);

// enumerate ////////////////////////////////////////////////////////////

struct $Iterator$enumerate;
typedef struct $Iterator$enumerate *$Iterator$enumerate;

struct $Iterator$enumerate$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($Iterator$enumerate, $Iterator,$int);
  $bool (*__bool__)($Iterator$enumerate);
  $str (*__str__)($Iterator$enumerate);
  void (*__serialize__)($Iterator$enumerate,$Serial$state);
  $Iterator$enumerate (*__deserialize__)($Serial$state);
  $WORD(*__next__)($Iterator$enumerate);
};

struct $Iterator$enumerate {
  struct $Iterator$enumerate$class *$class;
  $Iterator it;
  int nxt;
};

extern struct $Iterator$enumerate$class $Iterator$enumerate$methods;

$Iterator $enumerate($Iterable$opaque iter, $int start);

// filter ////////////////////////////////////////////////////////////

struct $Iterator$filter;
typedef struct $Iterator$filter *$Iterator$filter;

struct $Iterator$filter$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($Iterator$filter, $Iterator,$bool(*)($WORD));
  $bool (*__bool__)($Iterator$filter);
  $str (*__str__)($Iterator$filter);
  void (*__serialize__)($Iterator$filter,$Serial$state);
  $Iterator$filter (*__deserialize__)($Serial$state);
  $WORD(*__next__)($Iterator$filter);
};

struct $Iterator$filter {
  struct $Iterator$filter$class *$class;
  $Iterator it;
  $bool(*f)($WORD);
};

extern struct $Iterator$filter$class $Iterator$filter$methods;

$Iterator $filter($bool(*f)($WORD),$Iterable$opaque iter);

// map ////////////////////////////////////////////////////////////

struct $Iterator$map;
typedef struct $Iterator$map *$Iterator$map;

struct $Iterator$map$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($Iterator$map, $Iterator,$WORD(*)($WORD));
  $bool (*__bool__)($Iterator$map);
  $str (*__str__)($Iterator$map);
  void (*__serialize__)($Iterator$map,$Serial$state);
  $Iterator$map (*__deserialize__)($Serial$state);
  $WORD(*__next__)($Iterator$map);
};

struct $Iterator$map {
  struct $Iterator$map$class *$class;
  $Iterator it;
  $WORD(*f)($WORD);
};

extern struct $Iterator$map$class $Iterator$map$methods;

$Iterator $map($WORD(*f)($WORD),$Iterable$opaque iter);


