struct $bool$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($bool, $value);
  void (*__serialize__)($bool, $Serial$state);
  $bool (*__deserialize__)($bool, $Serial$state);
  $bool (*__bool__)($bool);
  $str (*__str__)($bool);
  $str (*__repr__)($bool);
};

struct $bool {
  struct $bool$class *$class;
  long val;
};

extern struct $bool$class $bool$methods;
$bool $bool$new($value);

extern struct $Eq$bool$class $Eq$bool$methods;
$Eq$bool $Eq$bool$new();

extern struct $Hashable$bool$class $Hashable$bool$methods;
$Hashable$bool $Hashable$bool$new();

extern struct $Eq$bool *$Eq$bool$witness;
extern struct $Hashable$bool *$Hashable$bool$witness;

$bool to$bool(long b);
long from$bool($bool b);

extern $bool $True, $False;

$bool $default__bool__($value);
