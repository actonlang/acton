#pragma once
#include <complex.h>
#undef complex

typedef void *$WORD;
typedef long *$int;
typedef double *$float;
typedef int $bool;
typedef double _Complex $complex;
typedef void None;

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

typedef struct $pair_t {
  char *$GCINFO;
  $WORD fst;
  $WORD snd;
} *$pair_t;


enum  exc {INDEXERROR, VALUEERROR, KEYERROR, STOPITERATION, TYPEERROR, MEMORYERROR, NOTIMPLEMENTED};

typedef enum exc exc;

#define MKEXCEPTION(e,sube) e=NULL;


// abstract class Iterator  ////////////////////////////////////////////////////////////////////////////////////

struct Iterator;
typedef struct Iterator *Iterator;

struct Iterator$__class__;
typedef struct Iterator$__class__ *Iterator$__class__;

struct Iterator {
  char *$GCINFO;
  Iterator$__class__ __class__;
};

struct Iterator$__class__ {
  char *$GCINFO;
  $WORD (*__next__)($WORD);
};
