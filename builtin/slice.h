
struct B_slice {
  struct B_sliceG_class *$class;
  long *start;
  long *stop;
  long *step;
};
extern GC_word B_sliceD_gcbm[GC_BITMAP_SIZE(struct B_slice)];

void normalize_slice(B_slice slc, long len, long *slen, long *start, long *stop, long *step);
