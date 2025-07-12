
struct B_slice {
  struct B_sliceG_class *$class;
  int64_t *start;
  int64_t *stop;
  int64_t *step;
};

void normalize_slice(B_slice slc, int64_t len, int64_t *slen, int64_t *start, int64_t *stop, int64_t *step);
