struct $NoneType$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($NoneType);
  void (*__serialize__)($NoneType,$Serial$state);
  $NoneType (*__deserialize__)($NoneType,$Serial$state);
  $bool (*__bool__)($NoneType);
  $str (*__str__)($NoneType);
};

struct $NoneType {
  struct $NoneType$class *$class;
};

extern struct $NoneType$class $NoneType$methods;
$NoneType $NoneType$new();
