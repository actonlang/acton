#pragma once

#include "__builtin__.h"

typedef struct $Iterator$class *$Iterator$class;

struct $Iterator$class {
  char *GCINFO;
  $WORD (*__next__)($WORD);
};

struct $Iterator {
  struct $Iterator$class *$class;
};

extern struct $Iterable$Iterator$class $Iterable$Iterator$methods;

extern struct $Iterable$Iterator *$Iterable$Iterator$witness;

$WORD $next($Iterator);
