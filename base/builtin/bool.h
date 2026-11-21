struct B_bool {
  struct B_boolG_class *$class;
  bool val;
};

B_bool toB_bool(bool b);
bool fromB_bool(B_bool b);

extern B_bool B_True, B_False;

bool $default__bool__(B_value);

// Witness-free formatting and truth workers.
bool B_bool_bool(B_bool self);
B_str B_bool_str(B_bool self);
B_str B_bool_repr(B_bool self);
