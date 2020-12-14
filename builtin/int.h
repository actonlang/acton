struct $int$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($int, $atom);
  void (*__serialize__)($int,$Serial$state);
  $int (*__deserialize__)($Serial$state);
  $bool (*__bool__)($int);
  $str (*__str__)($int);
};

struct $int {
  struct $int$class *$class;
  long val;
};

extern struct $int$class $int$methods;
$int $int$new($atom);

extern struct $Integral$int$class $Integral$int$methods;
$Integral$int $Integral$int$new();
extern struct $Logical$int$class $Logical$int$methods;
$Logical$int $Logical$int$new($Integral);
extern struct $Minus$int$class $Minus$int$methods;
$Minus$int $Minus$int$new($Integral);
extern struct $Ord$int$class $Ord$int$methods;
$Ord$int $Ord$int$new();
extern struct $Hashable$int$class $Hashable$int$methods;
$Hashable$int $Hashable$int$new();

extern struct $Integral$int *$Integral$int$witness;
extern struct $Logical$int *$Logical$int$witness;
extern struct $Minus$int *$Minus$int$witness;
extern struct $Ord$int *$Ord$int$witness;
extern struct $Hashable$int *$Hashable$int$witness;


$int to$int(long n);
long from$int($int n);

/*
#define to$int(n)  ({$int $res = malloc(sizeof(struct $int)); \
                     $res->$class = &$int$methods; \
                     $res->val = n; \
                     $res;})

#define from$int(n)  ((($int)n)->val)
*/

$int $int$new($atom a);

// only called with e>=0.
long longpow(long a, long e); // used also for ndarrays

