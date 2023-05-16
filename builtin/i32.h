struct B_i32 {
    struct B_i32G_class *$class;
    int val;
};
extern GC_word B_i32D_gcbm[GC_BITMAP_SIZE(struct B_i32)];

 
B_i32 toB_i32(int n);
int fromB_i32(B_i32 n);

B_i32 B_i32G_new(B_atom a);

