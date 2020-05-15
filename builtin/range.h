struct $range$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($range, $int, $int, $int);
  void (*__serialize__)($range,$Serial$state);
  $range (*__deserialize__)($Serial$state);
};

struct $range {
  struct $range$class *$class;
  int start;
  int stop;
  int step;
};


extern struct $range$class $range$methods;

extern struct $Iterable$range$class $Iterable$range$methods;
extern $Iterable$range $Iterable$range$witness;

//extern struct $Sequence$range$class $Sequence$range$methods;
//extern struct $Container$range$class $Container$range$methods; 

//extern struct $Sequence$range *$Sequence$range$witness;
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
  $WORD(*__next__)($Iterator$range);
};

struct $Iterator$range {
  struct $Iterator$range$class *$class;
  $range src;
  int nxt;
};

extern struct $Iterator$range$class  $Iterator$range$methods;
extern $Iterator$range $Iterator$range$witness;
