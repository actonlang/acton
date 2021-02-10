struct $bool$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($bool, $value);
  void (*__serialize__)($bool, $Serial$state);
  $bool (*__deserialize__)($bool, $Serial$state);
  $bool (*__bool__)($bool);
  $str (*__str__)($bool);
};

struct $bool {
  struct $bool$class *$class;
  long val;
};

extern struct $bool$class $bool$methods;
$bool $bool$new($value);

$bool to$bool(long b);
long from$bool($bool b);

$bool $True, $False;

$bool $default__bool__($value);
