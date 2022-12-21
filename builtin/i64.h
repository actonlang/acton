struct $i64$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($i64, $atom);
    void (*__serialize__)($i64,$Serial$state);
    $i64 (*__deserialize__)($i64,$Serial$state);
    $bool (*__bool__)($i64);
    $str (*__str__)($i64);
    $str (*__repr__)($i64);
};

struct $i64 {
    struct $i64$class *$class;
    long val;
};

extern struct $i64$class $i64$methods;
$i64 $i64$new($atom);

extern struct $Integral$i64$class $Integral$i64$methods;
$Integral$i64 $Integral$i64$new();
extern struct $Logical$i64$class $Logical$i64$methods;
$Logical$i64 $Logical$i64$new($Integral);
extern struct $Minus$i64$class $Minus$i64$methods;
$Minus$i64 $Minus$i64$new($Integral);
extern struct $Ord$i64$class $Ord$i64$methods;
$Ord$i64 $Ord$i64$new();
extern struct $Div$i64$class $Div$i64$methods;
$Div$i64 $Div$i64$new();
extern struct $Hashable$i64$class $Hashable$i64$methods;
$Hashable$i64 $Hashable$i64$new();

extern struct $Integral$i64 *$Integral$i64$witness;
extern struct $Logical$i64 *$Logical$i64$witness;
extern struct $Minus$i64 *$Minus$i64$witness;
extern struct $Div$i64 *$Div$i64$witness;
extern struct $Ord$i64 *$Ord$i64$witness;
extern struct $Hashable$i64 *$Hashable$i64$witness;


$i64 to$i64(long n);
long from$i64($i64 n);

/*
  #define to$i64(n)  ({$i64 $res = malloc(sizeof(struct $i64)); \
  $res->$class = &$i64$methods; \
  $res->val = n; \
  $res;})

  #define from$i64(n)  ((($i64)n)->val)
*/

$i64 $i64$new($atom a);

// only called with e>=0.
long longpow(long a, long e); // used also for ndarrays

