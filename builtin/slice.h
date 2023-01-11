struct B_sliceG_class {
  char *$GCINFO;
  int $class_id;
  $SuperG_class $superclass;
  void (*__init__)(B_slice,B_int,B_int,B_int);
  void (*__serialize__)(B_slice,$NoneType);
  B_slice (*__deserialize__)(B_slice,$NoneType);
  B_bool (*__bool__)(B_slice);
  B_str (*__str__)(B_slice);
  B_str (*__repr__)(B_slice);
};

typedef struct B_slice {
  struct B_sliceG_class *$class;
  int *start;
  int *stop;
  int *step;
} *B_slice;

extern struct B_sliceG_class B_sliceG_methods;
B_slice B_sliceG_new(B_int,B_int,B_int);

void normalize_slice(B_slice slc, int len, int *slen, int *start, int *stop, int *step);
