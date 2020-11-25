
typedef void *$WORD;
#define $None ($WORD)0

void $default__init__($WORD);

void $printobj(char *mess,$WORD obj);


#define $NEW($T, ...)       ({ $T $t = malloc(sizeof(struct $T)); \
                               $t->$class = &$T ## $methods; \
                               $t->$class->__init__($t, ##__VA_ARGS__); \
                               $t; })

#define $NEWCC($X, $c, ...) ({ $X $x = malloc(sizeof(struct $X)); \
                               $x->$class = &$X ## $methods; \
                               $x->$class->__init__($x, ##__VA_ARGS__, $CONSTCONT($x,$c)); })

#define $DNEW($T, $state)   ({ $T $t = malloc(sizeof(struct $T)); \
                               $t->$class = &$T ## $methods;                                     \
                               $dict_setitem($state->done,($Hashable)$Hashable$int$witness,to$int($state->row_no-1),$t); \
                               $t; })

#define $ISINSTANCE($x,$T)    ({ $Super$class $c = (($Super)$x)->$class; \
                                 while($c && $c != ($Super$class)&$T ## $methods) $c = $c->$superclass; \
                                 $c == ($Super$class)&$T ## $methods; })

#define $ISNOTNONE(x)         ((x) != $None ? $True : $False)

#define $SKIPRES(cont)        (cont)

#define $FORMAT($s, ...)      ({ char * $b; asprintf(&$b, $s, ##__VA_ARGS__); to$str($b); })