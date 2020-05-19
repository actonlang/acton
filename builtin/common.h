#pragma once

#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

typedef void *$WORD;
/*
struct exception;
typedef struct exception *exception;

void RAISE(exception e);

struct $tuple$class;

struct $tup1_t;
typedef struct $tup1_t *$tup1_t;

struct $tup2_t;
typedef struct $tup2_t *$tup2_t;

struct $tup3_t;
typedef struct $tup3_t *$tup3_t;

struct $tuple;
typedef struct $tuple *$tuple;

struct $tuple {
   struct $tuple$class *$class;
};

struct $tup1_t {
  struct $tuple$class *$class;
  $WORD a;
};

struct $tup2_t {
  struct $tuple$class *$class;
  $WORD a;
  $WORD b;
};

struct $tup3_t {
  struct $tuple$class *$class;
  $WORD a;
  $WORD b;
  $WORD c;
};

enum  exc {INDEXERROR, VALUEERROR, KEYERROR, STOPITERATION, TYPEERROR, MEMORYERROR, NOTIMPLEMENTED};

typedef enum exc exc;

#define MKEXCEPTION(e,sube) e=0;
*/
struct $Super$class;
typedef struct $Super$class *$Super$class;

struct $Super;
typedef struct $Super *$Super;

struct $Super$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
};

struct $Super {
  $Super$class $class;
};

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
