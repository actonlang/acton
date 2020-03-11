#pragma once
// #include <complex.h>
// #undef complex

#include <stdlib.h>

typedef void *$WORD;
//typedef long *$int;
//typedef double *$float;
//typedef int $bool;
//typedef double _Complex $complex;
typedef void None;

//$complex to$complex(double, double);

struct exception;
typedef struct exception *exception;

void RAISE(exception e);

struct $tup1_t;
typedef struct $tup1_t *$tup1_t;

struct $tup2_t;
typedef struct $tup2_t  *$tup2_t;

struct $tup2_t {
  $WORD a;
  $WORD b;
};

struct $tup3_t;
typedef struct $tup3_t {
  $WORD a;
  $WORD b;
  $WORD c;
}*$tup3_t;


typedef struct Slice {
  int *start;
  int *stop;
  int *step;
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

$WORD next(Iterator iter); 
