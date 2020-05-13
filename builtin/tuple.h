struct $tuple$class {
    char *$GCINFO;
    void (*__serialize__)($tuple,$Serial$state);
    $tuple (*__deserialize__)($Serial$state);
};

extern struct $tuple$class $tuple$methods;

#define $tup1_t$methods $tuple$methods
#define $tup2_t$methods $tuple$methods
#define $tup3_t$methods $tuple$methods
