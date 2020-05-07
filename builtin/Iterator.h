#pragma once

#include "__builtin__.h"

typedef struct $Iterator$class *$Iterator$class;

struct $Iterator$class {
  char *$GCINFO;
  $Super$class $superclass;
  $WORD (*__next__)($WORD);
};

struct $Iterator {
  struct $Iterator$class *$class;
};

extern struct $Iterable$Iterator$class $Iterable$Iterator$methods;

extern struct $Iterable$Iterator *$Iterable$Iterator$witness;

extern struct $Iterator$class $Iterator$methods;

extern struct $Iterator *$Iterator$witness;

$WORD $next($Iterator);
