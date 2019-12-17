#pragma once

#include "common.h"
#include "protocols.c"

struct $list;
typedef struct $list *$list;

typedef struct list_internal_t {
  $WORD *data;
  int length;
  int capacity;
} *list_internal_t;
  

typedef struct $list$__methods__ {
  $list (*copy)($list self);
  //  $int (*sort)($list self, int (*cmp)($WORD,$WORD));
} *$list$__methods__;

struct $list {
  $list$__methods__ __class__;
  list_internal_t __internal__;
};

Sequence$__class__ Sequence_$list;
Sliceable$__class__ Sliceable_$list;
Indexed$__class__ Indexed_$list;
Container$__class__ Container_$list;
Collection$__class__ Collection_$list;
Iterable$__class__ Iterable_$list;
Plus$__class__ Plus_$list;
Collection$__class__ Collection_$list;
 
