
struct B_slice {
  struct B_sliceG_class *$class;
  long *start;
  long *stop;
  long *step;
};

void normalize_slice(B_slice slc, long len, long *slen, long *start, long *stop, long *step);
