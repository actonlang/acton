#pragma once
#include <complex.h>
#undef complex

typedef void *$WORD;
typedef long *$int;
typedef double *$float;
typedef int $bool;
typedef double _Complex $complex;

#define $true 1
#define $false 0

$complex to$complex(double, double);

struct exception;

typedef struct exception *exception;

void RAISE(exception e);

typedef struct Slice {
  $int start;
  $int stop;
  $int step;
} *Slice;

void normalize_slice(Slice slc, int len, int *slen, int *start, int *stop, int *step);

typedef struct $divmod_struct {
  char *$GCINFO;
  $WORD quotient;
  $WORD remainder;
} *$divmod_t;


enum  exc {INDEXERROR, VALUEERROR, KEYERROR, STOPITERATION, TYPEERROR, MEMORYERROR, NOTIMPLEMENTED};

typedef enum exc exc;

#define MKEXCEPTION(e,sube) e=NULL;

