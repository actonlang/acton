struct $int$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($int, $WORD);
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

extern struct $Integral$int$class $Integral$int$methods;
extern struct $Logical$int$class $Logical$int$methods;
extern struct $Number$int$class $Number$int$methods;
extern struct $Plus$int$class $Plus$int$methods;
extern struct $Minus$int$class $Minus$int$methods;
extern struct $Hashable$int$class $Hashable$int$methods;

extern struct $Integral$int *$Integral$int$witness;
extern struct $Logical$int *$Logical$int$witness;
extern struct $Number$int *$Number$int$witness;
extern struct $Plus$int *$Plus$int$witness;
extern struct $Minus$int *$Minus$int$witness;
extern struct $Hashable$int *$Hashable$int$witness;

/*
$int to$int(long n);
long from$int($int n);
*/

#define to$int(n)  ({$int $res = malloc(sizeof(struct $int)); \
                     $res->$class = &$int$methods; \
                     $res->val = n; \
                     $res;})

#define from$int(n)  ((($int)n)->val)


$int $int_fromatom($Super a);
