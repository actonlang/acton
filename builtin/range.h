struct $range$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($range, $int, $int, $int);
  void (*__serialize__)($range,$Serial$state);
  $range (*__deserialize__)($Serial$state);
  $bool (*__bool__)($range);
  $str (*__str__)($range);
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
$Iterable$range $Iterable$range$new($range);
extern $Iterable$range $Iterable$range$witness;

extern struct $Sequence$range$class $Sequence$range$methods;
$Sequence$range $Sequence$range$new();
//extern struct $Container$range$class $Container$range$methods; 

extern struct $Sequence$range *$Sequence$range$witness;
//extern struct $Container$range *$Container$range$witness;

// Iterators over ranges ///////////////////////////////////////////////////////

typedef struct $Iterator$range *$Iterator$range;

struct $Iterator$range$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($Iterator$range, $range);
  void (*__serialize__)($Iterator$range,$Serial$state);
  $Iterator$range (*__deserialize__)($Serial$state);
  $bool (*__bool__)($Iterator$range);
  $str (*__str__)($Iterator$range);
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
