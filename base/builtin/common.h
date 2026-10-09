
typedef void *$WORD;
#define B_None ($WORD)0

#define $long long
#define $int64 int64_t

void $default__init__($WORD);

// Checked conversions used by generated code for fixed-width integer
// constructors.  Keeping the source value raw avoids the old atom -> bigint
// path; the caller casts the checked wide result to its destination type.
int64_t $checked_int_from_i64(int64_t value, int64_t min, int64_t max, const char *type_name);
uint64_t $checked_uint_from_i64(int64_t value, uint64_t max, const char *type_name);
int64_t $checked_int_from_u64(uint64_t value, int64_t max, const char *type_name);
uint64_t $checked_uint_from_u64(uint64_t value, uint64_t max, const char *type_name);


// void B_printobj(char *mess,$WORD obj);


#define $NEW($T, ...)       ({ $T $t = acton_malloc(sizeof(struct $T)); \
                               $t->$class = &$T ## G_methods; \
                               $t->$class->__init__($t, ##__VA_ARGS__); \
                               $t; })

// For a type whose only pointer is $class. The memory is not zeroed, so
// __init__ must write every other field.
#define $NEWATOMIC($T, ...) ({ $T $t = acton_malloc_atomic(sizeof(struct $T)); \
                               $t->$class = &$T ## G_methods; \
                               $t->$class->__init__($t, ##__VA_ARGS__); \
                               $t; })

#define $NEWCC($X, $c, ...) ({ $X $x = acton_malloc(sizeof(struct $X)); \
                               $x->$class = &$X ## G_methods; \
                               $x->$class->__init__($x, ##__VA_ARGS__, $CONSTCONT($x,$c)); })

#define $DNEW($T, $state)   ({ $T $t = acton_malloc(sizeof(struct $T)); \
                               $t->$class = &$T ## G_methods;                                     \
                               B_dictD_setitem($state->done,(B_Hashable)B_HashableD_intG_witness,toB_int($state->row_no-1),$t); \
                               $t; })

#define $AND(T, a, b)       ({ T $a = (a); ($a && ((B_value)$a)->$class->__bool__((B_value)$a)) ? (b) : $a; })

#define $OR(T, a, b)        ({ T $a = (a); ($a && ((B_value)$a)->$class->__bool__((B_value)$a)) ? $a : (b); })

#define $NOT(T, a)          ({ T $a = (a); ($a && ((B_value)$a)->$class->__bool__((B_value)$a)) ? B_False : B_True; })

#define $ISINSTANCE($x,$T)  ({ \
                               /* If object is NULL (None), return False */ \
                               ($x) == B_None ? B_False : ({ \
                                 $SuperG_class $c = (($Super)$x)->$class; \
                                 while($c && $c != ($SuperG_class)&$T ## G_methods) $c = $c->$superclass; \
                                 toB_bool($c != 0); \
                               }); \
                             })

#define $ISINSTANCE0($x,$T)  ({ \
                               /* If object is NULL (None), return 0 (False) */ \
                               ($x) == B_None ? 0 : ({ \
                                 $SuperG_class $c = (($Super)$x)->$class; \
                                 while($c && $c != ($SuperG_class)&$T ## G_methods) $c = $c->$superclass; \
                                 $c != 0; \
                               }); \
                             })

#define $ISNOTNONE(x)       ((x) != B_None ? B_True : B_False)

#define $ISNOTNONE0(x)      ((x) != B_None)

#define $ISNONE(x)          ((x) != B_None ? B_False : B_True)

#define $ISNONE0(x)         ((x) == B_None)

#define $SKIPRES(cont)      (cont)

// Raise an existing exception, or construct one with per-call arguments.
#define RAISE_EXC(exc)      $RAISE((B_BaseException)(exc))
#define RAISE($T, ...)      RAISE_EXC($NEW($T, ##__VA_ARGS__))

// $FORMAT is now a C function in str.c

char *unmangle_name(char *input);
