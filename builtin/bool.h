struct B_boolG_class {
  char *$GCINFO;
  int $class_id;
  $SuperG_class $superclass;
  void (*__init__)(B_bool, B_value);
  void (*__serialize__)(B_bool, $NoneType);
  B_bool (*__deserialize__)(B_bool, $NoneType);
  B_bool (*__bool__)(B_bool);
  B_str (*__str__)(B_bool);
  B_str (*__repr__)(B_bool);
};

struct B_bool {
  struct B_boolG_class *$class;
  long val;
};

extern struct B_boolG_class B_boolG_methods;
B_bool B_boolG_new(B_value);

//extern struct B_EqD_boolG_class B_EqD_boolG_methods;
//B_EqD_bool B_EqD_boolG_new();

extern struct B_HashableD_boolG_class B_HashableD_boolG_methods;
B_HashableD_bool B_HashableD_boolG_new();

//extern struct B_EqD_bool *B_EqD_boolG_witness;
extern struct B_HashableD_bool *B_HashableD_boolG_witness;

B_bool toB_bool(long b);
long fromB_bool(B_bool b);

extern B_bool $True, $False;

B_bool $default__bool__(B_value);
