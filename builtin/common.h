#pragma once

#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

typedef void *$WORD;
#define $None ($WORD)0

void $default__init__($WORD);


#define $NEW($T, ...)       ({ $T $t = malloc(sizeof(struct $T)); \
                               $t->$class = &$T ## $methods; \
                               $t->$class->__init__($t, ##__VA_ARGS__); \
                               $t; })

#define $NEWCC($X, $c, ...) ({ $X $x = malloc(sizeof(struct $X)); \
                               $x->$class = &$X ## $methods; \
                               $x->$class->__init__($x, ##__VA_ARGS__, ($Cont)$NEW($RetNew,$c,($Actor)$x)); })

#define $DNEW($T, $state)      ({ $T $t = malloc(sizeof(struct $T)); \
                               $t->$class = &$T ## $methods; \
                               $dict_setitem($state->done,($Hashable)$Hashable$int$witness,to$int($state->row_no-1),$t); \
                               $t; })
