
struct B_slice {
  struct B_sliceG_class *$class;
  int64_t *start;
  int64_t *stop;
  int64_t *step;
};

void normalize_slice(B_slice slc, int64_t len, int64_t *slen, int64_t *start, int64_t *stop, int64_t *step);

// Witness-free formatting and truth workers.
bool B_slice_bool(B_slice self);
B_str B_slice_str(B_slice self);
B_str B_slice_repr(B_slice self);
