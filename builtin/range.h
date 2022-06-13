struct $range$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($range, $int, $int, $int);
  void (*__serialize__)($range,$Serial$state);
  $range (*__deserialize__)($range,$Serial$state);
  $bool (*__bool__)($range);
  $str (*__str__)($range);
  $str (*__repr__)($range);
};

struct $range {
  struct $range$class *$class;
  long start;
  long stop;
  long step;
};


extern struct $range$class $range$methods;
$range $range$new($int, $int, $int);

extern struct $Iterable$range$class $Iterable$range$methods;
$Iterable$range $Iterable$range$new();
extern $Iterable$range $Iterable$range$witness;

// Iterators over ranges ///////////////////////////////////////////////////////

typedef struct $Iterator$range *$Iterator$range;

struct $Iterator$range$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($Iterator$range, $range);
  void (*__serialize__)($Iterator$range,$Serial$state);
  $Iterator$range (*__deserialize__)($Iterator$range,$Serial$state);
  $bool (*__bool__)($Iterator$range);
  $str (*__str__)($Iterator$range);
  $str (*__repr__)($Iterator$range);
  $WORD(*__next__)($Iterator$range);
};

struct $Iterator$range {
  struct $Iterator$range$class *$class;
  $range src;
  int nxt;
};

extern struct $Iterator$range$class  $Iterator$range$methods;
$Iterator$range $Iterator$range$new($range);
extern $Iterator$range $Iterator$range$witness;
