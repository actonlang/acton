struct B_u64 {
    struct B_u64G_class *$class;
    unsigned long val;
};

B_u64 toB_u64(unsigned long n);
unsigned long fromB_u64(B_u64 n);

B_u64 B_u64G_new(B_atom a);

