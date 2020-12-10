struct $slice$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($slice,$int,$int,$int);
  void (*__serialize__)($slice,$Serial$state);
  $slice (*__deserialize__)($Serial$state);
  $bool (*__bool__)($float);
  $str (*__str__)($float);
};

typedef struct $slice {
  struct $slice$class *$class;
  int *start;
  int *stop;
  int *step;
} *$slice;

extern struct $slice$class $slice$methods;
$slice $slice$new($int,$int,$int);

void normalize_slice($slice slc, int len, int *slen, int *start, int *stop, int *step);
