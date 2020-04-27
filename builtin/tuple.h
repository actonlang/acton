struct $tuple$class {
    char *$GCINFO;
    void (*__serialize__)($tuple, $Mapping$dict, $WORD*, int, $dict, $ROWLISTHEADER);
    $tuple (*__deserialize__)($Mapping$dict, $ROW*, $dict);
};

extern struct $tuple$class $tuple$methods;

#define $tup1_t$methods $tuple$methods
#define $tup2_t$methods $tuple$methods
#define $tup3_t$methods $tuple$methods
