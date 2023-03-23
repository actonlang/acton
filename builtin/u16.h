struct B_u16 {
    struct B_u16G_class *$class;
    unsigned short val;
};

B_u16 toB_u16(unsigned short n);
unsigned short fromB_u16(B_u16 n);

B_u16 B_u16G_new(B_atom a);

