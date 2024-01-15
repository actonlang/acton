struct B_i16 {
    struct B_i16G_class *$class;
    short val;
};

 
B_i16 toB_i16(short n);
short fromB_i16(B_i16 n);

B_i16 B_i16G_new(B_atom a, B_int base);
 
