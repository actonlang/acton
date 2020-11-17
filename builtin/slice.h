struct $Slice$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($Slice,$int,$int,$int);
  void (*__serialize__)($Slice,$Serial$state);
  $Slice (*__deserialize__)($Serial$state);
  $bool (*__bool__)($float);
  $str (*__str__)($float);
};

typedef struct $Slice {
  struct $Slice$class *$class;
  int *start;
  int *stop;
  int *step;
} *$Slice;

extern struct $Slice$class $Slice$methods;
$Slice $Slice$new($int,$int,$int);

void normalize_slice($Slice slc, int len, int *slen, int *start, int *stop, int *step);
