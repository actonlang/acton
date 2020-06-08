struct $bool$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($bool, $struct);
  void (*__serialize__)($bool, $Serial$state);
  $bool (*__deserialize__)($Serial$state);
  $bool (*__bool__)($bool);
  $str (*__str__)($bool);
};

struct $bool {
  struct $bool$class *$class;
  long val;
};

extern struct $bool$class $bool$methods;

$bool to$bool(long b);
long from$bool($bool b);

$bool $True, $False;
