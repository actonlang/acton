#pragma once

#include "__builtin__.h"

struct Iterator$__class__ {
  char *$GCINFO;
  $WORD (*__next__)($WORD);
};

typedef struct Iterator$__class__ *Iterator$__class__;

struct Iterator {
  char *$GCINFO;
  Iterator$__class__ __class__;
};

Iterable$Iterator Iterable$Iterator_new();

