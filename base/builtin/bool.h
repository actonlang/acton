struct B_bool {
  struct B_boolG_class *$class;
  long val;
};

B_bool toB_bool(long b);
long fromB_bool(B_bool b);

extern B_bool B_True, B_False;

B_bool $default__bool__(B_value);

B_bool B_HashableD_boolD___eq__(B_HashableD_bool wit, B_bool a, B_bool b);
B_bool B_HashableD_boolD___ne__(B_HashableD_bool wit, B_bool a, B_bool b);
B_NoneType B_HashableD_boolD_hash(B_HashableD_bool wit, B_bool a, B_hasher h);


