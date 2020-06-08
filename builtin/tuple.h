struct $tuple$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($tuple,int,$WORD*);
  void (*__serialize__)($tuple,$Serial$state); 
  $tuple (*__deserialize__)($Serial$state);
  $bool (*__bool__)($tuple);
  $str (*__str__)($tuple);
};

struct $tuple {
  struct $tuple$class *$class;
  int size;
  $WORD *components;
};

extern struct $tuple$class $tuple$methods;

extern struct $Iterable$tuple$class $Iterable$tuple$methods; 
extern struct $Sliceable$tuple$class $Sliceable$tuple$methods;
extern struct $Hashable$tuple$class $Hashable$tuple$methods; 

extern struct $Iterable$tuple *$Iterable$tuple$witness;
extern struct $Sliceable$tuple *$Sliceable$tuple$witness;
extern struct $Hashable$tuple *$Hashable$tuple_new(int,$Hashable*);

// Iterators over tuples ///////////////////////////////////////////////////////

typedef struct $Iterator$tuple *$Iterator$tuple;

struct $Iterator$tuple$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($Iterator$tuple, $tuple);
  void (*__serialize__)($Iterator$tuple,$Serial$state);
  $Iterator$tuple (*__deserialize__)($Serial$state);
  $bool (*__bool__)($Iterator$tuple);
  $str (*__str__)($Iterator$tuple);
  $WORD(*__next__)($Iterator$tuple);
};

struct $Iterator$tuple {
  struct $Iterator$tuple$class *$class;
  $tuple src;
  int nxt;
};

extern struct $Iterator$tuple$class $Iterator$tuple$methods;

$tuple tup1($WORD a);
$tuple tup2($WORD a, $WORD b);
$tuple tup3($WORD a, $WORD b, $WORD c);
$tuple tup4($WORD a, $WORD b, $WORD c, $WORD d);
