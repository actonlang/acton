struct B_i64 {
    struct B_i64G_class *$class;
    long val;
};
extern GC_word B_i64D_gcbm[GC_BITMAP_SIZE(struct B_i64)];

 
B_i64 toB_i64(long n);
long fromB_i64(B_i64 n);

B_i64 B_i64G_new(B_atom a);

// only called with e>=0.
long longpow(long a, long e); // used also for ndarrays

