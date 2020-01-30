#pragma once
#include <stdlib.h>

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
  $bool (*__eq__)( Eq$__class__, $WORD, $WORD);
  $bool (*__neq__)( Eq$__class__, $WORD, $WORD);
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
  $bool (*__lt__)(Ord$__class__, $WORD, $WORD);
  $bool (*__le__)(Ord$__class__, $WORD, $WORD);
  $bool (*__gt__)(Ord$__class__, $WORD, $WORD);
  $bool (*__ge__)(Ord$__class__, $WORD, $WORD);
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
  $WORD (*__and__)(Logical$__class__, $WORD, $WORD);
  $WORD (*__or__)(Logical$__class__, $WORD, $WORD);
  $WORD (*__xor__)(Logical$__class__, $WORD, $WORD);
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
  $WORD (*__add__)(Plus$__class__, $WORD, $WORD);
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
  $WORD (*__sub__)(Minus$__class__, $WORD, $WORD);
};

Minus Minus$__pack__(Minus$__class__ __class__, $WORD __impl__);

// protocol Complex  ////////////////////////////////////////////////////////////////////////////////////

struct Complex;
typedef struct Complex *Complex;

struct Complex$__class__;
typedef struct Complex$__class__ *Complex$__class__;

struct Complex {
  char *$GCINFO;
  Complex$__class__ __class__;
  $WORD __impl__;
};

struct Real;
typedef struct Real *Real;

struct Integral;
typedef struct Integral *Integral;

struct Complex$__class__ {
  char *$GCINFO;
  Eq$__class__ Eq$__methods__;
  Plus$__class__ Plus$__methods__;
  Minus$__class__ Minus$__methods__;
  double _Complex (*__complx__)(Complex$__class__, $WORD);  // spelling correctly correctly gives gcc error, in spite of macro complex being undef'ed in common.h???
  $bool (*__bool__)(Complex$__class__, $WORD);
  $WORD (*__mul__)(Complex$__class__, $WORD, $WORD);
  $WORD (*__truediv__)(Complex$__class__, $WORD, $WORD);
  $WORD (*__pow__)(Complex$__class__, $WORD, $WORD);
  $WORD (*__neg__)(Complex$__class__, $WORD);
  $WORD (*__pos__)(Complex$__class__, $WORD);
  Real (*real)(Complex$__class__, $WORD);
  Real (*imag)(Complex$__class__, $WORD);
  $WORD (*__abs__)(Complex$__class__, $WORD);
  $WORD (*conjugate)(Complex$__class__, $WORD);
};

Complex Complex$__pack__(Complex$__class__ __class__, $WORD __impl__);
 
// protocol Real  ////////////////////////////////////////////////////////////////////////////////////


struct Real$__class__;
typedef struct Real$__class__ *Real$__class__;

struct Real {
  char *$GCINFO;
  Real$__class__ __class__;
  $WORD __impl__;
};

struct Real$__class__ {
  char *$GCINFO;
  Ord$__class__ Ord$__methods__;
  Complex$__class__ Complex$__methods__;
  $float (*__float__)(Real$__class__, $WORD);
  Integral (*__trunc__)(Real$__class__, $WORD);
  Integral (*__floor__)(Real$__class__, $WORD);
  Integral (*__ceil__)(Real$__class__, $WORD);
  Integral (*__round__)(Real$__class__, $WORD);
};

Real Real$__pack__(Real$__class__ __class__, $WORD __impl__);
 
// protocol Rational  ////////////////////////////////////////////////////////////////////////////////////

struct Rational;
typedef struct Rational *Rational;

struct Rational$__class__;
typedef struct Rational$__class__ *Rational$__class__;

struct Rational {
  char *$GCINFO;
  Rational$__class__ __class__;
  $WORD __impl__;
};

struct Rational$__class__ {
  char *$GCINFO;
  Real$__class__ Real$__methods__;
  Integral (*__numerator__)(Rational$__class__, $WORD);
  Integral (*__denominator__)(Rational$__class__, $WORD);
};

Rational Rational$__pack__(Rational$__class__ __class__, $WORD __impl__);

// protocol Integral  ////////////////////////////////////////////////////////////////////////////////////

struct Real;
typedef struct Real *Real;
struct Integral$__class__;
typedef struct Integral$__class__ *Integral$__class__;

struct Integral {
  char *$GCINFO;
  Integral$__class__ __class__;
  $WORD __impl__;
};

struct Integral$__class__ {
  char *$GCINFO;
  Logical$__class__ Logical$__methods__;
  Rational$__class__ Rational$__methods__;
  $int (*__int__)(Integral$__class__, $WORD);
  $int (*__index__)(Integral$__class__, $WORD);
  $divmod_t (*__divmod__)(Integral$__class__, $WORD, $WORD);
  $WORD (*__floordiv__)(Integral$__class__, $WORD, $WORD);
  $WORD (*__mod__)(Integral$__class__, $WORD, $WORD);
  $WORD (*__lshift__)(Integral$__class__, $WORD, $WORD);
  $WORD (*__rshift__)(Integral$__class__, $WORD, $WORD);
  $WORD (*__invert__)(Integral$__class__, $WORD);
};

Integral Integral$__pack__(Integral$__class__ __class__, $WORD __impl__);

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
  $WORD (*__next__)(Iterator$__class__, $WORD);
};

Iterator Iterator$__pack__(Iterator$__class__ __class__, $WORD __impl__);
 
// protocol Hashable  ////////////////////////////////////////////////////////////////////////////////////

struct Hashable;
typedef struct Hashable *Hashable;

struct Hashable$__class__;
typedef struct Hashable$__class__ *Hashable$__class__;

struct Hashable {
  char *$GCINFO;
  Hashable$__class__ __class__;
  $WORD __impl__;
};

struct Hashable$__class__ {
  char *$GCINFO;
  Eq$__class__ Eq$__methods__;
  $int (*__hash__)(Hashable$__class__, $WORD);
};

Hashable Hashable$__pack__(Hashable$__class__ __class__, $WORD __impl__);
 
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
  Iterator (*__iter__)(Iterable$__class__, $WORD);
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
  Collection (*__fromiter__)(Collection$__class__, Iterable);  
  $int (*__len__)(Collection$__class__, $WORD);
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
  $bool (*__contains__)(Container_Eq$__class__, $WORD, $WORD);
  $bool (*__containsnot__)(Container_Eq$__class__, $WORD, $WORD);
  Eq$__class__ eqA;
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
  $WORD (*__getitem__)(Indexed$__class__, $WORD, $WORD);
  void (*__setitem__)(Indexed$__class__, $WORD, $WORD, $WORD);
  void (*__delitem__)(Indexed$__class__, $WORD, $WORD);
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
  $WORD (*__getslice__)(Sliceable$__class__, $WORD, Slice);
  // last param to setslice should be an Iterable!**********
  void (*__setslice__)(Sliceable$__class__, $WORD, Slice, Sequence);
  void (*__delslice__)(Sliceable$__class__, $WORD, Slice);
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
  Collection$__class__ Collection$__methods__;
  Plus$__class__ Plus$__methods__;
  Iterable (*__reversed__)(Sequence$__class__, $WORD);
  void (*insert)(Sequence$__class__, $WORD, $int, $WORD);
  void (*append)(Sequence$__class__, $WORD, $WORD);
  void (*reverse)(Sequence$__class__, $WORD);
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
  Container_Eq$__class__ Container_Eq$__methods__;
  Indexed$__class__ Indexed$__methods__;
  $WORD (*get)(Mapping$__class__, $WORD, $WORD, $WORD);
  Iterator (*keys)(Mapping$__class__,$WORD);
  Iterator (*values)(Mapping$__class__,$WORD);
  Iterator (*items)(Mapping$__class__,$WORD);
  void (*update)(Mapping$__class__,$WORD, Mapping);
  $WORD (*popitem)(Mapping$__class__,$WORD);
  $WORD (*setdefault)(Mapping$__class__,$WORD, $WORD, $WORD);
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
  Eq$__class__ Eq$__methods__;
  Ord$__class__ Ord$__methods__;
  Logical$__class__ Logical$__methods__;
  Minus$__class__ Minus$__methods__;
  Container_Eq$__class__ Container_Eq$__methods__;
  $bool (*isdisjoint)(Set$__class__, $WORD, $WORD);
  void (*add)(Set$__class__, $WORD, $WORD);
  void (*discard)(Set$__class__, $WORD, $WORD);
  $WORD (*pop)(Set$__class__, $WORD);
};

Set Set$__pack__(Set$__class__ __class__, $WORD __impl__);
 
// Convenience functions ////////////////////////////////////////

$WORD next(Iterator it);
