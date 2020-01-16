#pragma once

typedef void *$WORD;
typedef long *$int;
typedef double *$float;
typedef int $bool;

struct exception;

typedef struct exception *exception;

void RAISE(exception e);

typedef struct Slice {
  $int start;
  $int stop;
  $int step;
} *Slice;

void normalize_slice(Slice slc, int len, int *slen, int *start, int *stop, int *step);

struct Pair {
  $WORD fst;
  $WORD snd;
};

enum  exc {INDEXERROR, VALUEERROR, KEYERROR, STOPITERATION, TYPEERROR, MEMORYERROR, NOTIMPLEMENTED};

typedef enum exc exc;

#define MKEXCEPTION(e,sube) e=NULL;

