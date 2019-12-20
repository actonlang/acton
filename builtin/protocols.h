#pragma once

#include <stdlib.h>
#include "common.h"

// protocol Eq  ////////////////////////////////////////////////////////////////////////////////////

struct Eq;
typedef struct Eq *Eq;

struct Eq$__class__;
typedef struct Eq$__class__ *Eq$__class__;

struct Eq {
  char *$GCINFO;
  Eq$__class__ __class__;
  $WORD __impl__;
};

struct Eq$__class__ {
  char *$GCINFO;
  $bool (*__eq__)($WORD a, $WORD b);
  $bool (*__neq__)($WORD a, $WORD b);
};

Eq Eq$__pack__(Eq$__class__ __class__, $WORD __impl__);

// protocol Ord  ////////////////////////////////////////////////////////////////////////////////////

struct Ord;
typedef struct Ord *Ord;

struct Ord$__class__;
typedef struct Ord$__class__ *Ord$__class__;

struct Ord {
  char *$GCINFO;
  Ord$__class__ __class__;
  $WORD __impl__;
};

struct Ord$__class__ {
  char *$GCINFO;
  Eq$__class__ Eq$__methods__;
  $bool (*__lt__)($WORD a, $WORD b);
  $bool (*__le__)($WORD a, $WORD b);
  $bool (*__gt__)($WORD a, $WORD b);
  $bool (*__ge__)($WORD a, $WORD b);
};

Ord Ord$__pack__(Ord$__class__ __class__, $WORD __impl__);
  
// protocol Logical  ////////////////////////////////////////////////////////////////////////////////////

struct Logical;
typedef struct Logical *Logical;

struct Logical$__class__;
typedef struct Logical$__class__ *Logical$__class__;

struct Logical {
  char *$GCINFO;
  Logical$__class__ __class__;
  $WORD __impl__;
};

struct Logical$__class__ {
  char *$GCINFO;
  $bool (*and)($WORD a, $WORD b);
  $bool (*or)($WORD a, $WORD b);
  $bool (*xor)($WORD a, $WORD b);
};

Logical Logical$__pack__(Logical$__class__ __class__, $WORD __impl__);
 
// protocol Plus  ////////////////////////////////////////////////////////////////////////////////////

struct Plus;
typedef struct Plus *Plus;

struct Plus$__class__;
typedef struct Plus$__class__ *Plus$__class__;

struct Plus {
  char *$GCINFO;
  Plus$__class__ __class__;
  $WORD __impl__;
};

struct Plus$__class__ {
  char *$GCINFO;
  $WORD (*__add__)($WORD a, $WORD b);
};

Plus Plus$__pack__(Plus$__class__ __class__, $WORD __impl__);
 
// protocol Minus  ////////////////////////////////////////////////////////////////////////////////////

struct Minus;
typedef struct Minus *Minus;

struct Minus$__class__;
typedef struct Minus$__class__ *Minus$__class__;

struct Minus {
  char *$GCINFO;
  Minus$__class__ __class__;
  $WORD __impl__;
};

struct Minus$__class__ {
  char *$GCINFO;
  $WORD (*__sub__)($WORD a, $WORD b);
};

Minus Minus$__pack__(Minus$__class__ __class__, $WORD __impl__);
 
// protocol Iterator  ////////////////////////////////////////////////////////////////////////////////////

struct Iterator;
typedef struct Iterator *Iterator;

struct Iterator$__class__;
typedef struct Iterator$__class__ *Iterator$__class__;

struct Iterator {
  char *$GCINFO;
  Iterator$__class__ __class__;
  $WORD __impl__;
};

struct Iterator$__class__ {
  char *$GCINFO;
  $WORD (*__next__)(Iterator self);
};

Iterator Iterator$__pack__(Iterator$__class__ __class__, $WORD __impl__);
 
// protocol Iterable  ////////////////////////////////////////////////////////////////////////////////////

struct Iterable;
typedef struct Iterable *Iterable;

struct Iterable$__class__;
typedef struct Iterable$__class__ *Iterable$__class__;

struct Iterable {
  char *$GCINFO;
  Iterable$__class__ __class__;
  $WORD __impl__;
};

struct Iterable$__class__ {
  char *$GCINFO;
  Iterator (*__iter__)(Iterable self);
};

Iterable Iterable$__pack__(Iterable$__class__ __class__, $WORD __impl__);
 
// protocol Collection  ////////////////////////////////////////////////////////////////////////////////////

struct Collection;
typedef struct Collection *Collection;

struct Collection$__class__;
typedef struct Collection$__class__ *Collection$__class__;

struct Collection {
  char *$GCINFO;
  Collection$__class__ __class__;
  $WORD __impl__;
};

struct Collection$__class__ {
  char *$GCINFO;
  Iterable$__class__ Iterable$__methods__;
  Collection (*__fromiter__)(Iterable it);
  $int (*__len__)(Collection self);
};

Collection Collection$__pack__(Collection$__class__ __class__, $WORD __impl__);

// protocol Container  ////////////////////////////////////////////////////////////////////////////////////

struct Container_Eq;
typedef struct Container_Eq *Container_Eq;

struct Container_Eq$__class__;
typedef struct Container_Eq$__class__ *Container_Eq$__class__;

struct Container_Eq {
  char *$GCINFO;
  Container_Eq$__class__ __class__;
  $WORD __impl__;
};

struct Container_Eq$__class__ {
  char *$GCINFO;
  Collection$__class__ Collection$__methods__;
  $bool (*__contains__)(Container_Eq self, $WORD elem);
  $bool (*__containsnot__)(Container_Eq self, $WORD elem);
  Eq eqA;
};

Container_Eq Container_Eq$__pack__(Container_Eq$__class__ __class__, $WORD __impl__);
 
// protocol Indexed  ////////////////////////////////////////////////////////////////////////////////////

struct Indexed;
typedef struct Indexed *Indexed;

struct Indexed$__class__;
typedef struct Indexed$__class__ *Indexed$__class__;

struct Indexed {
  char *$GCINFO;
  Indexed$__class__ __class__;
  $WORD __impl__;
};

struct Indexed$__class__ {
  char *$GCINFO;
  $WORD (*__getitem__)(Indexed self, $WORD ix);
  void (*__setitem__)(Indexed self, $WORD ix, $WORD val);
  void (*__delitem__)(Indexed self, $WORD ix);
};

Indexed Indexed$__pack__(Indexed$__class__ __class__, $WORD __impl__);
 
// protocol Sliceable  ////////////////////////////////////////////////////////////////////////////////////

struct Sliceable;
typedef struct Sliceable *Sliceable;

struct Sliceable$__class__;
typedef struct Sliceable$__class__ *Sliceable$__class__;

struct Sliceable {
  char *$GCINFO;
  Sliceable$__class__ __class__;
  $WORD __impl__;
};

struct Sequence;
typedef struct Sequence *Sequence;

struct Sliceable$__class__ {
  char *$GCINFO;
  Indexed$__class__ Indexed$__methods__;
  Sequence (*__getslice__)(Sliceable self, Slice slice);
  void (*__setslice__)(Sliceable self, Slice slice, Iterable it);
  void (*__delslice__)(Sliceable self, Slice slice);
};

Sliceable Sliceable$__pack__(Sliceable$__class__ __class__, $WORD __impl__);
 
// protocol Sequence  ////////////////////////////////////////////////////////////////////////////////////

struct Sequence$__class__;
typedef struct Sequence$__class__ *Sequence$__class__;

struct Sequence {
  char *$GCINFO;
  Sequence$__class__ __class__;
  $WORD __impl__;
};

struct Sequence$__class__ {
  char *$GCINFO;
  Sliceable$__class__ Sliceable$__methods__;
  Container_Eq$__class__ Container_Eq$__methods__;
  Plus$__class__ Plus$__methods__;
  Iterable (*__reversed__)(Sequence self);
  void (*insert)(Sequence self, $int ix, $WORD elem);
  void (*append)(Sequence self, $WORD elem);
  void (*reverse)(Sequence self);
};

Sequence Sequence$__pack__(Sequence$__class__ __class__, $WORD __impl__);
 
// protocol Mapping  ////////////////////////////////////////////////////////////////////////////////////

struct Mapping;
typedef struct Mapping *Mapping;

struct Mapping$__class__;
typedef struct Mapping$__class__ *Mapping$__class__;

struct Mapping {
  char *$GCINFO;
  Mapping$__class__ __class__;
  $WORD __impl__;
};

struct Mapping$__class__ {
  char *$GCINFO;
  Collection$__class__ Collection$__methods__;
  Indexed$__class__ Indexed$__methods__;
  $WORD (*get)(Mapping self, $WORD key);
  Iterable (*keys)(Mapping self);
  Iterable (*values)(Mapping self);
  Iterable (*items)(Mapping self);
  void (*update)(Mapping self, Mapping other);
  struct $Pair (*popitem)(Mapping self);
  void (*setdefault)(Mapping self, $WORD key, $WORD value);
};

Mapping Mapping$__pack__(Mapping$__class__ __class__, $WORD __impl__);
 
// protocol Set  ////////////////////////////////////////////////////////////////////////////////////

struct Set;
typedef struct Set *Set;

struct Set$__class__;
typedef struct Set$__class__ *Set$__class__;

struct Set {
  char *$GCINFO;
  Set$__class__ __class__;
  $WORD __impl__;
};

struct Set$__class__ {
  char *$GCINFO;
  Collection$__class__ Collection$__methods__;
  Ord$__class__ Ord$__methods__;
  Logical$__class__ Logical$__methods__;
  Minus$__class__ Minus$__methods__;
  $bool (*isdisjoint)(Set self, Set other);
  void (*add)(Set self, $WORD elem);
  void (*discard)(Set self, $WORD elem);
  $WORD (*pop)(Set self);
};

Set Set$__pack__(Set$__class__ __class__, $WORD __impl__);
 
