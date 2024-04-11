
typedef void *$WORD;
#define B_None ($WORD)0

#define $long long
#define $int64 int64_t

void $default__init__($WORD);


// void B_printobj(char *mess,$WORD obj);


#define $NEW($T, ...)       ({ $T $t = acton_malloc(sizeof(struct $T)); \
                               $t->$class = &$T ## G_methods; \
                               $t->$class->__init__($t, ##__VA_ARGS__); \
                               $t; })

#define $NEWCC($X, $c, ...) ({ $X $x = acton_malloc(sizeof(struct $X)); \
                               $x->$class = &$X ## G_methods; \
                               $x->$class->__init__($x, ##__VA_ARGS__, $CONSTCONT($x,$c)); })

#define $DNEW($T, $state)   ({ $T $t = acton_malloc(sizeof(struct $T)); \
                               $t->$class = &$T ## G_methods;                                     \
                               B_dictD_setitem($state->done,(B_Hashable)B_HashableD_intG_witness,to$int($state->row_no-1),$t); \
                               $t; })

#define $AND(T, a, b)       ({ T $a = (a); ($a && ((B_value)$a)->$class->__bool__((B_value)$a)->val) ? (b) : $a; })

#define $OR(T, a, b)        ({ T $a = (a); ($a && ((B_value)$a)->$class->__bool__((B_value)$a)->val) ? $a : (b); })

#define $NOT(T, a)          ({ T $a = (a); ($a && ((B_value)$a)->$class->__bool__((B_value)$a)->val) ? B_False : B_True; })

#define $ISINSTANCE($x,$T)  ({ $SuperG_class $c = (($Super)$x)->$class; \
                               while($c && $c != ($SuperG_class)&$T ## G_methods) $c = $c->$superclass; \
                               toB_bool($c != 0); })

#define $ISINSTANCE0($x,$T)  ({ $SuperG_class $c = (($Super)$x)->$class; \
                               while($c && $c != ($SuperG_class)&$T ## G_methods) $c = $c->$superclass; \
                               $c != 0; })

#define $ISNOTNONE(x)       ((x) != B_None ? B_True : B_False)

#define $ISNOTNONE0(x)      ((x) != B_None)

#define $ISNONE(x)          ((x) != B_None ? B_False : B_True)

#define $ISNONE0(x)         ((x) == B_None)

#define $SKIPRES(cont)      (cont)

#define $FORMAT($s, ...) ({                              \
    int sz = snprintf(NULL, 0, $s, ##__VA_ARGS__);       \
    char *$b = (char *)acton_malloc_atomic(sz + 1);      \
    if ($b != NULL) {                                    \
        snprintf($b, sz + 1, $s, ##__VA_ARGS__);         \
    }                                                    \
    to_str_noc($b);                                      \
})

char *unmangle_name(char *input);
