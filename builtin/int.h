struct $int$class {
  char *$GCINFO;
  $Super$class $superclass;
  void (*__init__)($int, long);
  void (*__serialize__)($int, $Mapping$dict, long*, $dict, struct $ROWLISTHEADER*);
  $int (*__deserialize__)($Mapping$dict, $ROW*, $dict);
};

struct $int {
  struct $int$class *$class;
  long val;
};

extern struct $int$class $int$methods;

extern struct $Integral$int$class $Integral$int$methods;
extern struct $Logical$int$class $Logical$int$methods;
extern struct $Complex$int$class $Complex$int$methods;
extern struct $Plus$int$class $Plus$int$methods;
extern struct $Minus$int$class $Minus$int$methods;
extern struct $Hashable$int$class $Hashable$int$methods;

extern struct $Integral$int *$Integral$int$witness;
extern struct $Logical$int *$Logical$int$witness;
extern struct $Complex$int *$Complex$int$witness;
extern struct $Plus$int *$Plus$int$witness;
extern struct $Minus$int *$Minus$int$witness;
extern struct $Hashable$int *$Hashable$int$witness;

$int to$int(long n);
long from$int($int n);

$Iterator $range($int, $int, $int);
