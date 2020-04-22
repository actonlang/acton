#pragma once

#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

typedef void *$WORD;
typedef void $None;

struct exception;
typedef struct exception *exception;

void RAISE(exception e);

struct $tup1_t;
typedef struct $tup1_t *$tup1_t;

struct $tup2_t;
typedef struct $tup2_t *$tup2_t;

struct $tup3_t;
typedef struct $tup3_t *$tup3_t;

struct $tuple$class {
    char *$GCINFO;
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

extern struct $tuple$class $tuple$methods;

#define $tup1_t$methods $tuple$methods
#define $tup2_t$methods $tuple$methods
#define $tup3_t$methods $tuple$methods

/*
typedef struct $pair_t {
  char *$GCINFO;
  $WORD fst;
  $WORD snd;
} *$pair_t;
*/

enum  exc {INDEXERROR, VALUEERROR, KEYERROR, STOPITERATION, TYPEERROR, MEMORYERROR, NOTIMPLEMENTED};

typedef enum exc exc;

#define MKEXCEPTION(e,sube) e=0;
