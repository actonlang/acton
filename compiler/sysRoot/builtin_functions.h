// print /////////////////////////////////////////////////////////////////

void $print(int size, ...);

// enumerate ////////////////////////////////////////////////////////////

struct $Iterator$enumerate;
typedef struct $Iterator$enumerate *$Iterator$enumerate;

struct $Iterator$enumerate$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($Iterator$enumerate, $Iterator,$int);
  void (*__serialize__)($Iterator$enumerate,$Serial$state);
  $Iterator$enumerate (*__deserialize__)($Serial$state);
  $bool (*__bool__)($Iterator$enumerate);
  $str (*__str__)($Iterator$enumerate);
  $WORD(*__next__)($Iterator$enumerate);
};

struct $Iterator$enumerate {
  struct $Iterator$enumerate$class *$class;
  $Iterator it;
  int nxt;
};

extern struct $Iterator$enumerate$class $Iterator$enumerate$methods;
$Iterator$enumerate $Iterator$enumerate$new($Iterator,$int);

$Iterator $enumerate($Iterable wit, $WORD iter, $int start);

// filter ////////////////////////////////////////////////////////////

struct $Iterator$filter;
typedef struct $Iterator$filter *$Iterator$filter;

struct $Iterator$filter$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($Iterator$filter, $Iterator,$bool(*)($WORD));
  void (*__serialize__)($Iterator$filter,$Serial$state);
  $Iterator$filter (*__deserialize__)($Serial$state);
  $bool (*__bool__)($Iterator$filter);
  $str (*__str__)($Iterator$filter);
  $WORD(*__next__)($Iterator$filter);
};

struct $Iterator$filter {
  struct $Iterator$filter$class *$class;
  $Iterator it;
  $bool(*f)($WORD);
};

extern struct $Iterator$filter$class $Iterator$filter$methods;
$Iterator$filter $Iterator$filter$new($Iterator,$bool(*)($WORD));

$Iterator $filter($Iterable wit, $bool(*f)($WORD), $WORD iter);

// map ////////////////////////////////////////////////////////////

struct $Iterator$map;
typedef struct $Iterator$map *$Iterator$map;

struct $Iterator$map$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($Iterator$map, $Iterator,$WORD(*)($WORD));
  void (*__serialize__)($Iterator$map,$Serial$state);
  $Iterator$map (*__deserialize__)($Serial$state);
  $bool (*__bool__)($Iterator$map);
  $str (*__str__)($Iterator$map);
  $WORD(*__next__)($Iterator$map);
};

struct $Iterator$map {
  struct $Iterator$map$class *$class;
  $Iterator it;
  $WORD(*f)($WORD);
};

extern struct $Iterator$map$class $Iterator$map$methods;
$Iterator$map $Iterator$map$new($Iterator,$WORD(*)($WORD));

$Iterator $map($Iterable wit, $WORD(*f)($WORD), $WORD iter);


// zip ////////////////////////////////////////////////////////////

struct $Iterator$zip;
typedef struct $Iterator$zip *$Iterator$zip;

struct $Iterator$zip$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($Iterator$zip, $Iterator, $Iterator);
  void (*__serialize__)($Iterator$zip,$Serial$state);
  $Iterator$zip (*__deserialize__)($Serial$state);
  $bool (*__bool__)($Iterator$zip);
  $str (*__str__)($Iterator$zip);
  $WORD(*__next__)($Iterator$zip);
};

struct $Iterator$zip {
  struct $Iterator$zip$class *$class;
  $Iterator it1;
  $Iterator it2;
};

extern struct $Iterator$zip$class $Iterator$zip$methods;
$Iterator$zip $Iterator$zip$new($Iterator, $Iterator);

$Iterator $zip($Iterable wit1, $Iterable wit2, $WORD iter1, $WORD iter2);


