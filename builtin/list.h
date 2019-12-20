#pragma once

#include "common.h"
#include "protocols.h"

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

// List methods ////////////////////////////////////////////////////////////////////////////

$list $list_copy($list lst);
$list $list_add($list lst, $list other);
$list $list_fromiter(Iterable it);
$int $list_len($list lst);
$bool $list_contains($list lst, $WORD elem, int (*eq)($WORD,$WORD));
$bool $list_containsnot($list lst, $WORD elem, int (*eq)($WORD,$WORD));
$WORD $list_getitem($list lst, int ix);
void $list_setitem($list lst, int ix, $WORD val);
void $list_delitem($list lst,int ix);
$list $list_getslice($list lst, Slice slc);
void $list_setslice($list lst, Slice slc, $list other);
void $list_delslice($list lst, Slice slc);
void $list_append($list lst, $WORD val);
Iterable $list_reversed($list lst);
void $list_insert($list lst, int ix, $WORD val);
void $list_reverse($list lst);

// Protocol instances ///////////////////////////////////////////////////////////////////////

Plus$__class__ Plus_$list_instance;
Collection$__class__ Collection_$list_instance;
Iterable$__class__ Iterable_$list_instance;
Container_Eq$__class__ Container_Eq$list_instance(Eq eqA);
Indexed$__class__ Indexed_$list_instance;
Sliceable$__class__ Sliceable_$list_instance;
Sequence$__class__ Sequence_$list_instance;

void list_instance_init();

