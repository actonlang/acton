struct B_u32 {
    struct B_u32G_class *$class;
    unsigned int val;
};
extern GC_word B_u32D_gcbm[GC_BITMAP_SIZE(struct B_u32)];

B_u32 toB_u32(unsigned int n);
unsigned int fromB_u32(B_u32 n);

B_u32 B_u32G_new(B_atom a);

