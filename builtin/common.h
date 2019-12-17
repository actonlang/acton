#pragma once

typedef void *$WORD;
typedef int $int;
typedef int $bool;

struct exception;

typedef struct exception *exception;

void RAISE(exception e);

typedef struct Slice {
  $int *start;
  $int *end;
  $int *step;
} *Slice;

struct Pair {
  $WORD fst;
  $WORD snd;
};

enum  exc { INDEXERROR, VALUEERROR, KEYERROR, STOPITERATION, TYPEERROR, MEMORYERROR};

typedef enum exc exc;

#define MKEXCEPTION(e,sube) e=NULL;

