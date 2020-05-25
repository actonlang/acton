struct $bool$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($bool, long);
  $bool (*__bool__)($bool);
  $str (*__str__)($bool);
  void (*__serialize__)($bool, $Serial$state);
  $bool (*__deserialize__)($Serial$state);
};

struct $bool {
  struct $bool$class *$class;
  long val;
};

extern struct $bool$class $bool$methods;

$bool to$bool(long b);
long from$bool($bool b);

$bool $true, $false;
