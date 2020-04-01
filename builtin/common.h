#pragma once

typedef void *$WORD;
typedef void None;

struct exception;
typedef struct exception *exception;

void RAISE(exception e);

struct $tup1_t;
typedef struct $tup1_t *$tup1_t;

struct $tup2_t;
typedef struct $tup2_t  *$tup2_t;

struct $tup2_t {
  char *$GCINFO;
  $WORD a;
  $WORD b;
};

struct $tup3_t;
typedef struct $tup3_t {
  char *$GCINFO;
  $WORD a;
  $WORD b;
  $WORD c;
}*$tup3_t;

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
