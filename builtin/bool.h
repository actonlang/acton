struct B_bool {
  struct B_boolG_class *$class;
  long val;
};

B_bool toB_bool(long b);
long fromB_bool(B_bool b);

extern B_bool B_True, B_False;

B_bool $default__bool__(B_value);
