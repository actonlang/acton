#pragma once

#include "common.h"

struct $list;
typedef struct $list *$list;

struct $dict;
typedef struct $dict *$dict;

struct $set;
typedef struct $set *$set;

struct $str;
typedef struct $str *$str;

struct $int;
typedef struct $int *$int;

struct $float;
typedef struct $float *$float;

struct $complx;
typedef struct $complx *$complx;

struct $bool;
typedef struct $bool *$bool;

struct Iterator;
typedef struct Iterator *Iterator;

struct Slice;
typedef struct Slice *Slice;

struct Eq;
typedef struct Eq *Eq;

struct Eq$__class__;
typedef struct Eq$__class__ *Eq$__class__;

struct Eq$opaque;
typedef struct Eq$opaque *Eq$opaque;

struct Ord;
typedef struct Ord *Ord;

struct Ord$__class__;
typedef struct Ord$__class__ *Ord$__class__;

struct Ord$opaque;
typedef struct Ord$opaque *Ord$opaque;

struct Logical;
typedef struct Logical *Logical;

struct Logical$__class__;
typedef struct Logical$__class__ *Logical$__class__;

struct Logical$opaque;
typedef struct Logical$opaque *Logical$opaque;

struct Plus;
typedef struct Plus *Plus;

struct Plus$__class__;
typedef struct Plus$__class__ *Plus$__class__;

struct Plus$opaque;
typedef struct Plus$opaque *Plus$opaque;

struct Minus;
typedef struct Minus *Minus;

struct Minus$__class__;
typedef struct Minus$__class__ *Minus$__class__;

struct Minus$opaque;
typedef struct Minus$opaque *Minus$opaque;

struct Hashable;
typedef struct Hashable *Hashable;

struct Hashable$__class__;
typedef struct Hashable$__class__ *Hashable$__class__;

struct Hashable$opaque;
typedef struct Hashable$opaque *Hashable$opaque;

struct Boolean;
typedef struct Boolean *Boolean;

struct Boolean$__class__;
typedef struct Boolean$__class__ *Boolean$__class__;

struct Boolean$opaque;
typedef struct Boolean$opaque *Boolean$opaque;

struct Iterable;
typedef struct Iterable *Iterable;

struct Iterable$__class__;
typedef struct Iterable$__class__ *Iterable$__class__;

struct Iterable$opaque;
typedef struct Iterable$opaque *Iterable$opaque;

struct Collection;
typedef struct Collection *Collection;

struct Collection$__class__;
typedef struct Collection$__class__ *Collection$__class__;

struct Collection$opaque;
typedef struct Collection$opaque *Collection$opaque;

struct Indexed;
typedef struct Indexed *Indexed;

struct Indexed$__class__;
typedef struct Indexed$__class__ *Indexed$__class__;

struct Indexed$opaque;
typedef struct Indexed$opaque *Indexed$opaque;

struct Sliceable;
typedef struct Sliceable *Sliceable;

struct Sliceable$__class__;
typedef struct Sliceable$__class__ *Sliceable$__class__;

struct Sliceable$opaque;
typedef struct Sliceable$opaque *Sliceable$opaque;

struct Sequence;
typedef struct Sequence *Sequence;

struct Sequence$__class__;
typedef struct Sequence$__class__ *Sequence$__class__;

struct Sequence$opaque;
typedef struct Sequence$opaque *Sequence$opaque;

struct Container;
typedef struct Container *Container;

struct Container$__class__;
typedef struct Container$__class__ *Container$__class__;

struct Container$opaque;
typedef struct Container$opaque *Container$opaque;

struct Mapping;
typedef struct Mapping *Mapping;

struct Mapping$__class__;
typedef struct Mapping$__class__ *Mapping$__class__;

struct Mapping$opaque;
typedef struct Mapping$opaque *Mapping$opaque;

struct Set;
typedef struct Set *Set;

struct Set$__class__;
typedef struct Set$__class__ *Set$__class__;

struct Set$opaque;
typedef struct Set$opaque *Set$opaque;

struct Complx;
typedef struct Complx *Complx;

struct Complx$__class__;
typedef struct Complx$__class__ *Complx$__class__;

struct Complx$opaque;
typedef struct Complx$opaque *Complx$opaque;

struct Real;
typedef struct Real *Real;

struct Real$__class__;
typedef struct Real$__class__ *Real$__class__;

struct Real$opaque;
typedef struct Real$opaque *Real$opaque;

struct Rational;
typedef struct Rational *Rational;

struct Rational$__class__;
typedef struct Rational$__class__ *Rational$__class__;

struct Rational$opaque;
typedef struct Rational$opaque *Rational$opaque;

struct Integral;
typedef struct Integral *Integral;

struct Integral$__class__;
typedef struct Integral$__class__ *Integral$__class__;

struct Integral$opaque;
typedef struct Integral$opaque *Integral$opaque;

struct Boolean$int;
typedef struct Boolean$int *Boolean$int;

struct Boolean$int$__class__;
typedef struct Boolean$int$__class__ *Boolean$int$__class__;

struct Boolean$list;
typedef struct Boolean$list *Boolean$list;

struct Boolean$list$__class__;
typedef struct Boolean$list$__class__ *Boolean$list$__class__;

struct Sequence$list;
typedef struct Sequence$list *Sequence$list;

struct Sequence$list$__class__;
typedef struct Sequence$list$__class__ *Sequence$list$__class__;

struct Collection$list;
typedef struct Collection$list *Collection$list;

struct Collection$list$__class__;
typedef struct Collection$list$__class__ *Collection$list$__class__;

struct Plus$list;
typedef struct Plus$list *Plus$list;

struct Plus$list$__class__;
typedef struct Plus$list$__class__ *Plus$list$__class__;

struct Container$list;
typedef struct Container$list *Container$list;

struct Container$list$__class__;
typedef struct Container$list$__class__ *Container$list$__class__;

struct Mapping$dict;
typedef struct Mapping$dict *Mapping$dict;

struct Mapping$dict$__class__;
typedef struct Mapping$dict$__class__ *Mapping$dict$__class__;

struct Indexed$dict;
typedef struct Indexed$dict *Indexed$dict;

struct Indexed$dict$__class__;
typedef struct Indexed$dict$__class__ *Indexed$dict$__class__;

struct Set$set;
typedef struct Set$set *Set$set;

struct Set$set$__class__;
typedef struct Set$set$__class__ *Set$set$__class__;

struct Ord$set;
typedef struct Ord$set *Ord$set;

struct Ord$set$__class__;
typedef struct Ord$set$__class__ *Ord$set$__class__;

struct Logical$set;
typedef struct Logical$set *Logical$set;

struct Logical$set$__class__;
typedef struct Logical$set$__class__ *Logical$set$__class__;

struct Minus$set;
typedef struct Minus$set *Minus$set;

struct Minus$set$__class__;
typedef struct Minus$set$__class__ *Minus$set$__class__;

struct Iterable$Iterator;
typedef struct Iterable$Iterator *Iterable$Iterator;

struct Iterable$Iterator$__class__;
typedef struct Iterable$Iterator$__class__ *Iterable$Iterator$__class__;

struct Ord$str;
typedef struct Ord$str *Ord$str;

struct Ord$str$__class__;
typedef struct Ord$str$__class__ *Ord$str$__class__;

struct Container$str;
typedef struct Container$str *Container$str;

struct Container$str$__class__;
typedef struct Container$str$__class__ *Container$str$__class__;

struct Sliceable$str;
typedef struct Sliceable$str *Sliceable$str;

struct Sliceable$str$__class__;
typedef struct Sliceable$str$__class__ *Sliceable$str$__class__;

struct Plus$str;
typedef struct Plus$str *Plus$str;

struct Plus$str$__class__;
typedef struct Plus$str$__class__ *Plus$str$__class__;

struct Hashable$str;
typedef struct Hashable$str *Hashable$str;

struct Hashable$str$__class__;
typedef struct Hashable$str$__class__ *Hashable$str$__class__;

struct Integral$int;
typedef struct Integral$int *Integral$int;

struct Integral$int$__class__;
typedef struct Integral$int$__class__ *Integral$int$__class__;

struct Complx$int;
typedef struct Complx$int *Complx$int;

struct Complx$int$__class__;
typedef struct Complx$int$__class__ *Complx$int$__class__;

struct Plus$int;
typedef struct Plus$int *Plus$int;

struct Plus$int$__class__;
typedef struct Plus$int$__class__ *Plus$int$__class__;

struct Minus$int;
typedef struct Minus$int *Minus$int;

struct Minus$int$__class__;
typedef struct Minus$int$__class__ *Minus$int$__class__;

struct Logical$int;
typedef struct Logical$int *Logical$int;

struct Logical$int$__class__;
typedef struct Logical$int$__class__ *Logical$int$__class__;

struct Hashable$int;
typedef struct Hashable$int *Hashable$int;

struct Hashable$int$__class__;
typedef struct Hashable$int$__class__ *Hashable$int$__class__;

struct Real$float;
typedef struct Real$float *Real$float;

struct Real$float$__class__;
typedef struct Real$float$__class__ *Real$float$__class__;

struct Complx$float;
typedef struct Complx$float *Complx$float;

struct Complx$float$__class__;
typedef struct Complx$float$__class__ *Complx$float$__class__;

struct Plus$float;
typedef struct Plus$float *Plus$float;

struct Plus$float$__class__;
typedef struct Plus$float$__class__ *Plus$float$__class__;

struct Minus$float;
typedef struct Minus$float *Minus$float;

struct Minus$float$__class__;
typedef struct Minus$float$__class__ *Minus$float$__class__;

struct Hashable$float;
typedef struct Hashable$float *Hashable$float;

struct Hashable$float$__class__;
typedef struct Hashable$float$__class__ *Hashable$float$__class__;

// Eq ////////////////////////////////////////////////////////////

struct Eq {
    char *GCINFO;
    Eq$__class__  __class__;
};

struct Eq$__class__ {
    char *GCINFO;
    $bool (*__eq__)(Eq, $WORD, $WORD);
    $bool (*__ne__)(Eq, $WORD, $WORD);
};

struct Eq$opaque {
    char *GCINFO;
    Eq __proto__;
    $WORD __impl__;
};

Eq$opaque Eq$__pack__(Eq __proto__, $WORD __impl__);


// Ord ////////////////////////////////////////////////////////////

struct Ord {
    char *GCINFO;
    Ord$__class__  __class__;
};

struct Ord$__class__ {
    char *GCINFO;
    $bool (*__eq__)(Ord, $WORD, $WORD);
    $bool (*__ne__)(Ord, $WORD, $WORD);
    $bool (*__lt__)(Ord, $WORD, $WORD);
    $bool (*__le__)(Ord, $WORD, $WORD);
    $bool (*__gt__)(Ord, $WORD, $WORD);
    $bool (*__ge__)(Ord, $WORD, $WORD);
};

struct Ord$opaque {
    char *GCINFO;
    Ord __proto__;
    $WORD __impl__;
};

Ord$opaque Ord$__pack__(Ord __proto__, $WORD __impl__);


// Logical ////////////////////////////////////////////////////////////

struct Logical {
    char *GCINFO;
    Logical$__class__  __class__;
};

struct Logical$__class__ {
    char *GCINFO;
    $WORD (*__and__)(Logical, $WORD, $WORD);
    $WORD (*__or__)(Logical, $WORD, $WORD);
    $WORD (*__xor__)(Logical, $WORD, $WORD);
};

struct Logical$opaque {
    char *GCINFO;
    Logical __proto__;
    $WORD __impl__;
};

Logical$opaque Logical$__pack__(Logical __proto__, $WORD __impl__);


// Plus ////////////////////////////////////////////////////////////

struct Plus {
    char *GCINFO;
    Plus$__class__  __class__;
};

struct Plus$__class__ {
    char *GCINFO;
    $WORD (*__add__)(Plus, $WORD, $WORD);
};

struct Plus$opaque {
    char *GCINFO;
    Plus __proto__;
    $WORD __impl__;
};

Plus$opaque Plus$__pack__(Plus __proto__, $WORD __impl__);


// Minus ////////////////////////////////////////////////////////////

struct Minus {
    char *GCINFO;
    Minus$__class__  __class__;
};

struct Minus$__class__ {
    char *GCINFO;
    $WORD (*__sub__)(Minus, $WORD, $WORD);
};

struct Minus$opaque {
    char *GCINFO;
    Minus __proto__;
    $WORD __impl__;
};

Minus$opaque Minus$__pack__(Minus __proto__, $WORD __impl__);


// Hashable ////////////////////////////////////////////////////////////

struct Hashable {
    char *GCINFO;
    Hashable$__class__  __class__;
};

struct Hashable$__class__ {
    char *GCINFO;
    $bool (*__eq__)(Hashable, $WORD, $WORD);
    $bool (*__ne__)(Hashable, $WORD, $WORD);
    $int (*__hash__)(Hashable, $WORD);
};

struct Hashable$opaque {
    char *GCINFO;
    Hashable __proto__;
    $WORD __impl__;
};

Hashable$opaque Hashable$__pack__(Hashable __proto__, $WORD __impl__);


// Boolean ////////////////////////////////////////////////////////////

struct Boolean {
    char *GCINFO;
    Boolean$__class__  __class__;
};

struct Boolean$__class__ {
    char *GCINFO;
    $bool (*__bool__)(Boolean, $WORD);
};

struct Boolean$opaque {
    char *GCINFO;
    Boolean __proto__;
    $WORD __impl__;
};

Boolean$opaque Boolean$__pack__(Boolean __proto__, $WORD __impl__);


// Iterable ////////////////////////////////////////////////////////////

struct Iterable {
    char *GCINFO;
    Iterable$__class__  __class__;
};

struct Iterable$__class__ {
    char *GCINFO;
    Iterator (*__iter__)(Iterable, $WORD);
};

struct Iterable$opaque {
    char *GCINFO;
    Iterable __proto__;
    $WORD __impl__;
};

Iterable$opaque Iterable$__pack__(Iterable __proto__, $WORD __impl__);


// Collection ////////////////////////////////////////////////////////////

struct Collection {
    char *GCINFO;
    Collection$__class__  __class__;
};

struct Collection$__class__ {
    char *GCINFO;
    Iterator (*__iter__)(Collection, $WORD);
    $WORD (*__fromiter__)(Collection, Iterable$opaque);
    $int (*__len__)(Collection, $WORD);
};

struct Collection$opaque {
    char *GCINFO;
    Collection __proto__;
    $WORD __impl__;
};

Collection$opaque Collection$__pack__(Collection __proto__, $WORD __impl__);


// Indexed ////////////////////////////////////////////////////////////

struct Indexed {
    char *GCINFO;
    Indexed$__class__  __class__;
};

struct Indexed$__class__ {
    char *GCINFO;
    $WORD (*__getitem__)(Indexed, $WORD, $WORD);
    None (*__setitem__)(Indexed, $WORD, $WORD, $WORD);
    None (*__delitem__)(Indexed, $WORD, $WORD);
};

struct Indexed$opaque {
    char *GCINFO;
    Indexed __proto__;
    $WORD __impl__;
};

Indexed$opaque Indexed$__pack__(Indexed __proto__, $WORD __impl__);


// Sliceable ////////////////////////////////////////////////////////////

struct Sliceable {
    char *GCINFO;
    Sliceable$__class__  __class__;
};

struct Sliceable$__class__ {
    char *GCINFO;
    $WORD (*__getitem__)(Sliceable, $WORD, $int);
    None (*__setitem__)(Sliceable, $WORD, $int, $WORD);
    None (*__delitem__)(Sliceable, $WORD, $int);
    $WORD (*__getslice__)(Sliceable, $WORD, Slice);
    None (*__setslice__)(Sliceable, $WORD, Slice, Iterable$opaque);
    None (*__delslice__)(Sliceable, $WORD, Slice);
};

struct Sliceable$opaque {
    char *GCINFO;
    Sliceable __proto__;
    $WORD __impl__;
};

Sliceable$opaque Sliceable$__pack__(Sliceable __proto__, $WORD __impl__);


// Sequence ////////////////////////////////////////////////////////////

struct Sequence {
    char *GCINFO;
    Sequence$__class__  __class__;
    Collection _Collection;
    Plus _Plus;
};

struct Sequence$__class__ {
    char *GCINFO;
    $WORD (*__getitem__)(Sequence, $WORD, $int);
    None (*__setitem__)(Sequence, $WORD, $int, $WORD);
    None (*__delitem__)(Sequence, $WORD, $int);
    $WORD (*__getslice__)(Sequence, $WORD, Slice);
    None (*__setslice__)(Sequence, $WORD, Slice, Iterable$opaque);
    None (*__delslice__)(Sequence, $WORD, Slice);
    Iterator (*__reversed__)(Sequence, $WORD);
    None (*insert)(Sequence, $WORD, $int, $WORD);
    None (*append)(Sequence, $WORD, $WORD);
    None (*reverse)(Sequence, $WORD);
};

struct Sequence$opaque {
    char *GCINFO;
    Sequence __proto__;
    $WORD __impl__;
};

Sequence$opaque Sequence$__pack__(Sequence __proto__, $WORD __impl__);


// Container ////////////////////////////////////////////////////////////

struct Container {
    char *GCINFO;
    Container$__class__  __class__;
    Eq _Eq;
};

struct Container$__class__ {
    char *GCINFO;
    Iterator (*__iter__)(Container, $WORD);
    $WORD (*__fromiter__)(Container, Iterable$opaque);
    $int (*__len__)(Container, $WORD);
    $bool (*__contains__)(Container, $WORD, $WORD);
    $bool (*__containsnot__)(Container, $WORD, $WORD);
};

struct Container$opaque {
    char *GCINFO;
    Container __proto__;
    $WORD __impl__;
};

Container$opaque Container$__pack__(Container __proto__, $WORD __impl__);


// Mapping ////////////////////////////////////////////////////////////

struct Mapping {
    char *GCINFO;
    Mapping$__class__  __class__;
    Indexed _Indexed;
    Eq _Eq;
};

struct Mapping$__class__ {
    char *GCINFO;
    Iterator (*__iter__)(Mapping, $WORD);
    $WORD (*__fromiter__)(Mapping, Iterable$opaque);
    $int (*__len__)(Mapping, $WORD);
    $bool (*__contains__)(Mapping, $WORD, $WORD);
    $bool (*__containsnot__)(Mapping, $WORD, $WORD);
    $WORD (*get)(Mapping, $WORD, $WORD, $WORD);
    Iterator (*keys)(Mapping, $WORD);
    Iterator (*values)(Mapping, $WORD);
    Iterator (*items)(Mapping, $WORD);
    None (*update)(Mapping, $WORD, Iterable$opaque);
    $tup2_t (*popitem)(Mapping, $WORD);
    None (*setdefault)(Mapping, $WORD, $WORD, $WORD);
};

struct Mapping$opaque {
    char *GCINFO;
    Mapping __proto__;
    $WORD __impl__;
};

Mapping$opaque Mapping$__pack__(Mapping __proto__, $WORD __impl__);


// Set ////////////////////////////////////////////////////////////

struct Set {
    char *GCINFO;
    Set$__class__  __class__;
    Ord _Ord;
    Logical _Logical;
    Minus _Minus;
    Eq _Eq;
};

struct Set$__class__ {
    char *GCINFO;
    Iterator (*__iter__)(Set, $WORD);
    $WORD (*__fromiter__)(Set, Iterable$opaque);
    $int (*__len__)(Set, $WORD);
    $bool (*__contains__)(Set, $WORD, $WORD);
    $bool (*__containsnot__)(Set, $WORD, $WORD);
    $bool (*isdisjoint)(Set, $WORD, $WORD);
    None (*add)(Set, $WORD, $WORD);
    None (*discard)(Set, $WORD, $WORD);
    $WORD (*pop)(Set, $WORD);
};

struct Set$opaque {
    char *GCINFO;
    Set __proto__;
    $WORD __impl__;
};

Set$opaque Set$__pack__(Set __proto__, $WORD __impl__);


// Complx ////////////////////////////////////////////////////////////

struct Complx {
    char *GCINFO;
    Complx$__class__  __class__;
    Plus _Plus;
    Minus _Minus;
};

struct Complx$__class__ {
    char *GCINFO;
    $bool (*__eq__)(Complx, $WORD, $WORD);
    $bool (*__ne__)(Complx, $WORD, $WORD);
    $complx (*__complx__)(Complx, $WORD);
    $bool (*__bool__)(Complx, $WORD);
    $WORD (*__mul__)(Complx, $WORD, $WORD);
    $WORD (*__truediv__)(Complx, $WORD, $WORD);
    $WORD (*__pow__)(Complx, $WORD, $WORD);
    $WORD (*__neg__)(Complx, $WORD);
    $WORD (*__pos__)(Complx, $WORD);
    Real$opaque (*real)(Complx, $WORD);
    Real$opaque (*imag)(Complx, $WORD);
    Real$opaque (*__abs__)(Complx, $WORD);
    $WORD (*conjugate)(Complx, $WORD);
};

struct Complx$opaque {
    char *GCINFO;
    Complx __proto__;
    $WORD __impl__;
};

Complx$opaque Complx$__pack__(Complx __proto__, $WORD __impl__);


// Real ////////////////////////////////////////////////////////////

struct Real {
    char *GCINFO;
    Real$__class__  __class__;
    Complx _Complx;
    Plus _Plus;
    Minus _Minus;
};

struct Real$__class__ {
    char *GCINFO;
    $bool (*__eq__)(Real, $WORD, $WORD);
    $bool (*__ne__)(Real, $WORD, $WORD);
    $bool (*__lt__)(Real, $WORD, $WORD);
    $bool (*__le__)(Real, $WORD, $WORD);
    $bool (*__gt__)(Real, $WORD, $WORD);
    $bool (*__ge__)(Real, $WORD, $WORD);
    $float (*__float__)(Real, $WORD);
    Integral$opaque (*__trunc__)(Real, $WORD);
    Integral$opaque (*__floor__)(Real, $WORD);
    Integral$opaque (*__ceil__)(Real, $WORD);
    Integral$opaque (*__round__)(Real, $WORD);
};

struct Real$opaque {
    char *GCINFO;
    Real __proto__;
    $WORD __impl__;
};

Real$opaque Real$__pack__(Real __proto__, $WORD __impl__);


// Rational ////////////////////////////////////////////////////////////

struct Rational {
    char *GCINFO;
    Rational$__class__  __class__;
    Complx _Complx;
    Plus _Plus;
    Minus _Minus;
};

struct Rational$__class__ {
    char *GCINFO;
    $bool (*__eq__)(Rational, $WORD, $WORD);
    $bool (*__ne__)(Rational, $WORD, $WORD);
    $bool (*__lt__)(Rational, $WORD, $WORD);
    $bool (*__le__)(Rational, $WORD, $WORD);
    $bool (*__gt__)(Rational, $WORD, $WORD);
    $bool (*__ge__)(Rational, $WORD, $WORD);
    $float (*__float__)(Rational, $WORD);
    Integral$opaque (*__trunc__)(Rational, $WORD);
    Integral$opaque (*__floor__)(Rational, $WORD);
    Integral$opaque (*__ceil__)(Rational, $WORD);
    Integral$opaque (*__round__)(Rational, $WORD);
    Integral$opaque (*numerator)(Rational, $WORD);
    Integral$opaque (*denominator)(Rational, $WORD);
};

struct Rational$opaque {
    char *GCINFO;
    Rational __proto__;
    $WORD __impl__;
};

Rational$opaque Rational$__pack__(Rational __proto__, $WORD __impl__);


// Integral ////////////////////////////////////////////////////////////

struct Integral {
    char *GCINFO;
    Integral$__class__  __class__;
    Complx _Complx;
    Plus _Plus;
    Minus _Minus;
    Logical _Logical;
};

struct Integral$__class__ {
    char *GCINFO;
    $bool (*__eq__)(Integral, $WORD, $WORD);
    $bool (*__ne__)(Integral, $WORD, $WORD);
    $bool (*__lt__)(Integral, $WORD, $WORD);
    $bool (*__le__)(Integral, $WORD, $WORD);
    $bool (*__gt__)(Integral, $WORD, $WORD);
    $bool (*__ge__)(Integral, $WORD, $WORD);
    $float (*__float__)(Integral, $WORD);
    Integral$opaque (*__trunc__)(Integral, $WORD);
    Integral$opaque (*__floor__)(Integral, $WORD);
    Integral$opaque (*__ceil__)(Integral, $WORD);
    Integral$opaque (*__round__)(Integral, $WORD);
    Integral$opaque (*numerator)(Integral, $WORD);
    Integral$opaque (*denominator)(Integral, $WORD);
    $int (*__int__)(Integral, $WORD);
    $int (*__index__)(Integral, $WORD);
    $tup2_t (*__divmod__)(Integral, $WORD, $WORD);
    $WORD (*__floordiv__)(Integral, $WORD, $WORD);
    $WORD (*__mod__)(Integral, $WORD, $WORD);
    $WORD (*__lshift__)(Integral, $WORD, $WORD);
    $WORD (*__rshift__)(Integral, $WORD, $WORD);
    $WORD (*__invert__)(Integral, $WORD);
};

struct Integral$opaque {
    char *GCINFO;
    Integral __proto__;
    $WORD __impl__;
};

Integral$opaque Integral$__pack__(Integral __proto__, $WORD __impl__);


// Boolean$int ////////////////////////////////////////////////////////////

struct Boolean$int {
    char *GCINFO;
    Boolean$int$__class__  __class__;
};

struct Boolean$int$__class__ {
    char *GCINFO;
    $bool (*__bool__)(Boolean$int, $int);
};

$bool Boolean$int$__bool__ (Boolean$int, $int);

// Boolean$list ////////////////////////////////////////////////////////////

struct Boolean$list {
    char *GCINFO;
    Boolean$list$__class__  __class__;
};

struct Boolean$list$__class__ {
    char *GCINFO;
    $bool (*__bool__)(Boolean$list, $list);
};

$bool Boolean$list$__bool__ (Boolean$list, $list);

// Sequence$list ////////////////////////////////////////////////////////////

struct Sequence$list {
    char *GCINFO;
    Sequence$list$__class__  __class__;
    Collection _Collection;
    Plus _Plus;
};

struct Sequence$list$__class__ {
    char *GCINFO;
    $WORD (*__getitem__)(Sequence$list, $list, $int);
    None (*__setitem__)(Sequence$list, $list, $int, $WORD);
    None (*__delitem__)(Sequence$list, $list, $int);
    $list (*__getslice__)(Sequence$list, $list, Slice);
    None (*__setslice__)(Sequence$list, $list, Slice, Iterable$opaque);
    None (*__delslice__)(Sequence$list, $list, Slice);
    Iterator (*__reversed__)(Sequence$list, $list);
    None (*insert)(Sequence$list, $list, $int, $WORD);
    None (*append)(Sequence$list, $list, $WORD);
    None (*reverse)(Sequence$list, $list);
};

$WORD Sequence$list$__getitem__ (Sequence$list, $list, $int);
None Sequence$list$__setitem__ (Sequence$list, $list, $int, $WORD);
None Sequence$list$__delitem__ (Sequence$list, $list, $int);
$list Sequence$list$__getslice__ (Sequence$list, $list, Slice);
None Sequence$list$__setslice__ (Sequence$list, $list, Slice, Iterable$opaque);
None Sequence$list$__delslice__ (Sequence$list, $list, Slice);
Iterator Sequence$list$__reversed__ (Sequence$list, $list);
None Sequence$list$insert (Sequence$list, $list, $int, $WORD);
None Sequence$list$append (Sequence$list, $list, $WORD);
None Sequence$list$reverse (Sequence$list, $list);

// Collection$list ////////////////////////////////////////////////////////////

struct Collection$list {
    char *GCINFO;
    Collection$list$__class__  __class__;
    Sequence _Sequence;
};

struct Collection$list$__class__ {
    char *GCINFO;
    Iterator (*__iter__)(Collection$list, $list);
    $list (*__fromiter__)(Collection$list, Iterable$opaque);
    $int (*__len__)(Collection$list, $list);
};

Iterator Collection$list$__iter__ (Collection$list, $list);
$list Collection$list$__fromiter__ (Collection$list, Iterable$opaque);
$int Collection$list$__len__ (Collection$list, $list);

// Plus$list ////////////////////////////////////////////////////////////

struct Plus$list {
    char *GCINFO;
    Plus$list$__class__  __class__;
    Sequence _Sequence;
};

struct Plus$list$__class__ {
    char *GCINFO;
    $list (*__add__)(Plus$list, $list, $list);
};

$list Plus$list$__add__ (Plus$list, $list, $list);

// Container$list ////////////////////////////////////////////////////////////

struct Container$list {
    char *GCINFO;
    Container$list$__class__  __class__;
    Eq _Eq;
};

struct Container$list$__class__ {
    char *GCINFO;
    Iterator (*__iter__)(Container$list, $list);
    $list (*__fromiter__)(Container$list, Iterable$opaque);
    $int (*__len__)(Container$list, $list);
    $bool (*__contains__)(Container$list, $list, $WORD);
    $bool (*__containsnot__)(Container$list, $list, $WORD);
};

Iterator Container$list$__iter__ (Container$list, $list);
$list Container$list$__fromiter__ (Container$list, Iterable$opaque);
$int Container$list$__len__ (Container$list, $list);
$bool Container$list$__contains__ (Container$list, $list, $WORD);
$bool Container$list$__containsnot__ (Container$list, $list, $WORD);

// Mapping$dict ////////////////////////////////////////////////////////////

struct Mapping$dict {
    char *GCINFO;
    Mapping$dict$__class__  __class__;
    Hashable _Hashable;
    Indexed _Indexed;
    Eq _Eq;
};

struct Mapping$dict$__class__ {
    char *GCINFO;
    Iterator (*__iter__)(Mapping$dict, $dict);
    $dict (*__fromiter__)(Mapping$dict, Iterable$opaque);
    $int (*__len__)(Mapping$dict, $dict);
    $bool (*__contains__)(Mapping$dict, $dict, $WORD);
    $bool (*__containsnot__)(Mapping$dict, $dict, $WORD);
    $WORD (*get)(Mapping$dict, $dict, $WORD, $WORD);
    Iterator (*keys)(Mapping$dict, $dict);
    Iterator (*values)(Mapping$dict, $dict);
    Iterator (*items)(Mapping$dict, $dict);
    None (*update)(Mapping$dict, $dict, Iterable$opaque);
    $tup2_t (*popitem)(Mapping$dict, $dict);
    None (*setdefault)(Mapping$dict, $dict, $WORD, $WORD);
};

Iterator Mapping$dict$__iter__ (Mapping$dict, $dict);
$dict Mapping$dict$__fromiter__ (Mapping$dict, Iterable$opaque);
$int Mapping$dict$__len__ (Mapping$dict, $dict);
$bool Mapping$dict$__contains__ (Mapping$dict, $dict, $WORD);
$bool Mapping$dict$__containsnot__ (Mapping$dict, $dict, $WORD);
$WORD Mapping$dict$get (Mapping$dict, $dict, $WORD, $WORD);
Iterator Mapping$dict$keys (Mapping$dict, $dict);
Iterator Mapping$dict$values (Mapping$dict, $dict);
Iterator Mapping$dict$items (Mapping$dict, $dict);
None Mapping$dict$update (Mapping$dict, $dict, Iterable$opaque);
$tup2_t Mapping$dict$popitem (Mapping$dict, $dict);
None Mapping$dict$setdefault (Mapping$dict, $dict, $WORD, $WORD);

// Indexed$dict ////////////////////////////////////////////////////////////

struct Indexed$dict {
    char *GCINFO;
    Indexed$dict$__class__  __class__;
    Mapping _Mapping;
    Hashable _Hashable;
};

struct Indexed$dict$__class__ {
    char *GCINFO;
    $WORD (*__getitem__)(Indexed$dict, $dict, $WORD);
    None (*__setitem__)(Indexed$dict, $dict, $WORD, $WORD);
    None (*__delitem__)(Indexed$dict, $dict, $WORD);
};

$WORD Indexed$dict$__getitem__ (Indexed$dict, $dict, $WORD);
None Indexed$dict$__setitem__ (Indexed$dict, $dict, $WORD, $WORD);
None Indexed$dict$__delitem__ (Indexed$dict, $dict, $WORD);

// Set$set ////////////////////////////////////////////////////////////

struct Set$set {
    char *GCINFO;
    Set$set$__class__  __class__;
    Hashable _Hashable;
    Ord _Ord;
    Logical _Logical;
    Minus _Minus;
    Eq _Eq;
};

struct Set$set$__class__ {
    char *GCINFO;
    Iterator (*__iter__)(Set$set, $set);
    $set (*__fromiter__)(Set$set, Iterable$opaque);
    $int (*__len__)(Set$set, $set);
    $bool (*__contains__)(Set$set, $set, $WORD);
    $bool (*__containsnot__)(Set$set, $set, $WORD);
    $bool (*isdisjoint)(Set$set, $set, $set);
    None (*add)(Set$set, $set, $WORD);
    None (*discard)(Set$set, $set, $WORD);
    $WORD (*pop)(Set$set, $set);
};

Iterator Set$set$__iter__ (Set$set, $set);
$set Set$set$__fromiter__ (Set$set, Iterable$opaque);
$int Set$set$__len__ (Set$set, $set);
$bool Set$set$__contains__ (Set$set, $set, $WORD);
$bool Set$set$__containsnot__ (Set$set, $set, $WORD);
$bool Set$set$isdisjoint (Set$set, $set, $set);
None Set$set$add (Set$set, $set, $WORD);
None Set$set$discard (Set$set, $set, $WORD);
$WORD Set$set$pop (Set$set, $set);

// Ord$set ////////////////////////////////////////////////////////////

struct Ord$set {
    char *GCINFO;
    Ord$set$__class__  __class__;
    Set _Set;
    Hashable _Hashable;
};

struct Ord$set$__class__ {
    char *GCINFO;
    $bool (*__eq__)(Ord$set, $set, $set);
    $bool (*__ne__)(Ord$set, $set, $set);
    $bool (*__lt__)(Ord$set, $set, $set);
    $bool (*__le__)(Ord$set, $set, $set);
    $bool (*__gt__)(Ord$set, $set, $set);
    $bool (*__ge__)(Ord$set, $set, $set);
};

$bool Ord$set$__eq__ (Ord$set, $set, $set);
$bool Ord$set$__ne__ (Ord$set, $set, $set);
$bool Ord$set$__lt__ (Ord$set, $set, $set);
$bool Ord$set$__le__ (Ord$set, $set, $set);
$bool Ord$set$__gt__ (Ord$set, $set, $set);
$bool Ord$set$__ge__ (Ord$set, $set, $set);

// Logical$set ////////////////////////////////////////////////////////////

struct Logical$set {
    char *GCINFO;
    Logical$set$__class__  __class__;
    Set _Set;
    Hashable _Hashable;
};

struct Logical$set$__class__ {
    char *GCINFO;
    $set (*__and__)(Logical$set, $set, $set);
    $set (*__or__)(Logical$set, $set, $set);
    $set (*__xor__)(Logical$set, $set, $set);
};

$set Logical$set$__and__ (Logical$set, $set, $set);
$set Logical$set$__or__ (Logical$set, $set, $set);
$set Logical$set$__xor__ (Logical$set, $set, $set);

// Minus$set ////////////////////////////////////////////////////////////

struct Minus$set {
    char *GCINFO;
    Minus$set$__class__  __class__;
    Set _Set;
    Hashable _Hashable;
};

struct Minus$set$__class__ {
    char *GCINFO;
    $set (*__sub__)(Minus$set, $set, $set);
};

$set Minus$set$__sub__ (Minus$set, $set, $set);

// Iterable$Iterator ////////////////////////////////////////////////////////////

struct Iterable$Iterator {
    char *GCINFO;
    Iterable$Iterator$__class__  __class__;
};

struct Iterable$Iterator$__class__ {
    char *GCINFO;
    Iterator (*__iter__)(Iterable$Iterator, Iterator);
};

Iterator Iterable$Iterator$__iter__ (Iterable$Iterator, Iterator);

// Ord$str ////////////////////////////////////////////////////////////

struct Ord$str {
    char *GCINFO;
    Ord$str$__class__  __class__;
};

struct Ord$str$__class__ {
    char *GCINFO;
    $bool (*__eq__)(Ord$str, $str, $str);
    $bool (*__ne__)(Ord$str, $str, $str);
    $bool (*__lt__)(Ord$str, $str, $str);
    $bool (*__le__)(Ord$str, $str, $str);
    $bool (*__gt__)(Ord$str, $str, $str);
    $bool (*__ge__)(Ord$str, $str, $str);
};

$bool Ord$str$__eq__ (Ord$str, $str, $str);
$bool Ord$str$__ne__ (Ord$str, $str, $str);
$bool Ord$str$__lt__ (Ord$str, $str, $str);
$bool Ord$str$__le__ (Ord$str, $str, $str);
$bool Ord$str$__gt__ (Ord$str, $str, $str);
$bool Ord$str$__ge__ (Ord$str, $str, $str);

// Container$str ////////////////////////////////////////////////////////////

struct Container$str {
    char *GCINFO;
    Container$str$__class__  __class__;
    Eq _Eq;
};

struct Container$str$__class__ {
    char *GCINFO;
    Iterator (*__iter__)(Container$str, $str);
    $str (*__fromiter__)(Container$str, Iterable$opaque);
    $int (*__len__)(Container$str, $str);
    $bool (*__contains__)(Container$str, $str, $str);
    $bool (*__containsnot__)(Container$str, $str, $str);
};

Iterator Container$str$__iter__ (Container$str, $str);
$str Container$str$__fromiter__ (Container$str, Iterable$opaque);
$int Container$str$__len__ (Container$str, $str);
$bool Container$str$__contains__ (Container$str, $str, $str);
$bool Container$str$__containsnot__ (Container$str, $str, $str);

// Sliceable$str ////////////////////////////////////////////////////////////

struct Sliceable$str {
    char *GCINFO;
    Sliceable$str$__class__  __class__;
};

struct Sliceable$str$__class__ {
    char *GCINFO;
    $str (*__getitem__)(Sliceable$str, $str, $int);
    None (*__setitem__)(Sliceable$str, $str, $int, $str);
    None (*__delitem__)(Sliceable$str, $str, $int);
    $str (*__getslice__)(Sliceable$str, $str, Slice);
    None (*__setslice__)(Sliceable$str, $str, Slice, Iterable$opaque);
    None (*__delslice__)(Sliceable$str, $str, Slice);
};

$str Sliceable$str$__getitem__ (Sliceable$str, $str, $int);
None Sliceable$str$__setitem__ (Sliceable$str, $str, $int, $str);
None Sliceable$str$__delitem__ (Sliceable$str, $str, $int);
$str Sliceable$str$__getslice__ (Sliceable$str, $str, Slice);
None Sliceable$str$__setslice__ (Sliceable$str, $str, Slice, Iterable$opaque);
None Sliceable$str$__delslice__ (Sliceable$str, $str, Slice);

// Plus$str ////////////////////////////////////////////////////////////

struct Plus$str {
    char *GCINFO;
    Plus$str$__class__  __class__;
};

struct Plus$str$__class__ {
    char *GCINFO;
    $str (*__add__)(Plus$str, $str, $str);
};

$str Plus$str$__add__ (Plus$str, $str, $str);

// Hashable$str ////////////////////////////////////////////////////////////

struct Hashable$str {
    char *GCINFO;
    Hashable$str$__class__  __class__;
};

struct Hashable$str$__class__ {
    char *GCINFO;
    $bool (*__eq__)(Hashable$str, $str, $str);
    $bool (*__ne__)(Hashable$str, $str, $str);
    $int (*__hash__)(Hashable$str, $str);
};

$bool Hashable$str$__eq__ (Hashable$str, $str, $str);
$bool Hashable$str$__ne__ (Hashable$str, $str, $str);
$int Hashable$str$__hash__ (Hashable$str, $str);

// Integral$int ////////////////////////////////////////////////////////////

struct Integral$int {
    char *GCINFO;
    Integral$int$__class__  __class__;
    Complx _Complx;
    Plus _Plus;
    Minus _Minus;
    Logical _Logical;
};

struct Integral$int$__class__ {
    char *GCINFO;
    $bool (*__eq__)(Integral$int, $int, $int);
    $bool (*__ne__)(Integral$int, $int, $int);
    $bool (*__lt__)(Integral$int, $int, $int);
    $bool (*__le__)(Integral$int, $int, $int);
    $bool (*__gt__)(Integral$int, $int, $int);
    $bool (*__ge__)(Integral$int, $int, $int);
    $float (*__float__)(Integral$int, $int);
    Integral$opaque (*__trunc__)(Integral$int, $int);
    Integral$opaque (*__floor__)(Integral$int, $int);
    Integral$opaque (*__ceil__)(Integral$int, $int);
    Integral$opaque (*__round__)(Integral$int, $int);
    Integral$opaque (*numerator)(Integral$int, $int);
    Integral$opaque (*denominator)(Integral$int, $int);
    $int (*__int__)(Integral$int, $int);
    $int (*__index__)(Integral$int, $int);
    $tup2_t (*__divmod__)(Integral$int, $int, $int);
    $int (*__floordiv__)(Integral$int, $int, $int);
    $int (*__mod__)(Integral$int, $int, $int);
    $int (*__lshift__)(Integral$int, $int, $int);
    $int (*__rshift__)(Integral$int, $int, $int);
    $int (*__invert__)(Integral$int, $int);
};

$bool Integral$int$__eq__ (Integral$int, $int, $int);
$bool Integral$int$__ne__ (Integral$int, $int, $int);
$bool Integral$int$__lt__ (Integral$int, $int, $int);
$bool Integral$int$__le__ (Integral$int, $int, $int);
$bool Integral$int$__gt__ (Integral$int, $int, $int);
$bool Integral$int$__ge__ (Integral$int, $int, $int);
$float Integral$int$__float__ (Integral$int, $int);
Integral$opaque Integral$int$__trunc__ (Integral$int, $int);
Integral$opaque Integral$int$__floor__ (Integral$int, $int);
Integral$opaque Integral$int$__ceil__ (Integral$int, $int);
Integral$opaque Integral$int$__round__ (Integral$int, $int);
Integral$opaque Integral$int$numerator (Integral$int, $int);
Integral$opaque Integral$int$denominator (Integral$int, $int);
$int Integral$int$__int__ (Integral$int, $int);
$int Integral$int$__index__ (Integral$int, $int);
$tup2_t Integral$int$__divmod__ (Integral$int, $int, $int);
$int Integral$int$__floordiv__ (Integral$int, $int, $int);
$int Integral$int$__mod__ (Integral$int, $int, $int);
$int Integral$int$__lshift__ (Integral$int, $int, $int);
$int Integral$int$__rshift__ (Integral$int, $int, $int);
$int Integral$int$__invert__ (Integral$int, $int);

// Complx$int ////////////////////////////////////////////////////////////

struct Complx$int {
    char *GCINFO;
    Complx$int$__class__  __class__;
    Integral _Integral;
    Plus _Plus;
    Minus _Minus;
};

struct Complx$int$__class__ {
    char *GCINFO;
    $bool (*__eq__)(Complx$int, $int, $int);
    $bool (*__ne__)(Complx$int, $int, $int);
    $complx (*__complx__)(Complx$int, $int);
    $bool (*__bool__)(Complx$int, $int);
    $int (*__mul__)(Complx$int, $int, $int);
    $int (*__truediv__)(Complx$int, $int, $int);
    $int (*__pow__)(Complx$int, $int, $int);
    $int (*__neg__)(Complx$int, $int);
    $int (*__pos__)(Complx$int, $int);
    Real$opaque (*real)(Complx$int, $int);
    Real$opaque (*imag)(Complx$int, $int);
    Real$opaque (*__abs__)(Complx$int, $int);
    $int (*conjugate)(Complx$int, $int);
};

$bool Complx$int$__eq__ (Complx$int, $int, $int);
$bool Complx$int$__ne__ (Complx$int, $int, $int);
$complx Complx$int$__complx__ (Complx$int, $int);
$bool Complx$int$__bool__ (Complx$int, $int);
$int Complx$int$__mul__ (Complx$int, $int, $int);
$int Complx$int$__truediv__ (Complx$int, $int, $int);
$int Complx$int$__pow__ (Complx$int, $int, $int);
$int Complx$int$__neg__ (Complx$int, $int);
$int Complx$int$__pos__ (Complx$int, $int);
Real$opaque Complx$int$real (Complx$int, $int);
Real$opaque Complx$int$imag (Complx$int, $int);
Real$opaque Complx$int$__abs__ (Complx$int, $int);
$int Complx$int$conjugate (Complx$int, $int);

// Plus$int ////////////////////////////////////////////////////////////

struct Plus$int {
    char *GCINFO;
    Plus$int$__class__  __class__;
    Integral _Integral;
};

struct Plus$int$__class__ {
    char *GCINFO;
    $int (*__add__)(Plus$int, $int, $int);
};

$int Plus$int$__add__ (Plus$int, $int, $int);

// Minus$int ////////////////////////////////////////////////////////////

struct Minus$int {
    char *GCINFO;
    Minus$int$__class__  __class__;
    Integral _Integral;
};

struct Minus$int$__class__ {
    char *GCINFO;
    $int (*__sub__)(Minus$int, $int, $int);
};

$int Minus$int$__sub__ (Minus$int, $int, $int);

// Logical$int ////////////////////////////////////////////////////////////

struct Logical$int {
    char *GCINFO;
    Logical$int$__class__  __class__;
    Integral _Integral;
};

struct Logical$int$__class__ {
    char *GCINFO;
    $int (*__and__)(Logical$int, $int, $int);
    $int (*__or__)(Logical$int, $int, $int);
    $int (*__xor__)(Logical$int, $int, $int);
};

$int Logical$int$__and__ (Logical$int, $int, $int);
$int Logical$int$__or__ (Logical$int, $int, $int);
$int Logical$int$__xor__ (Logical$int, $int, $int);

// Hashable$int ////////////////////////////////////////////////////////////

struct Hashable$int {
    char *GCINFO;
    Hashable$int$__class__  __class__;
};

struct Hashable$int$__class__ {
    char *GCINFO;
    $bool (*__eq__)(Hashable$int, $int, $int);
    $bool (*__ne__)(Hashable$int, $int, $int);
    $int (*__hash__)(Hashable$int, $int);
};

$bool Hashable$int$__eq__ (Hashable$int, $int, $int);
$bool Hashable$int$__ne__ (Hashable$int, $int, $int);
$int Hashable$int$__hash__ (Hashable$int, $int);

// Real$float ////////////////////////////////////////////////////////////

struct Real$float {
    char *GCINFO;
    Real$float$__class__  __class__;
    Complx _Complx;
    Plus _Plus;
    Minus _Minus;
};

struct Real$float$__class__ {
    char *GCINFO;
    $bool (*__eq__)(Real$float, $float, $float);
    $bool (*__ne__)(Real$float, $float, $float);
    $bool (*__lt__)(Real$float, $float, $float);
    $bool (*__le__)(Real$float, $float, $float);
    $bool (*__gt__)(Real$float, $float, $float);
    $bool (*__ge__)(Real$float, $float, $float);
    $float (*__float__)(Real$float, $float);
    Integral$opaque (*__trunc__)(Real$float, $float);
    Integral$opaque (*__floor__)(Real$float, $float);
    Integral$opaque (*__ceil__)(Real$float, $float);
    Integral$opaque (*__round__)(Real$float, $float);
};

$bool Real$float$__eq__ (Real$float, $float, $float);
$bool Real$float$__ne__ (Real$float, $float, $float);
$bool Real$float$__lt__ (Real$float, $float, $float);
$bool Real$float$__le__ (Real$float, $float, $float);
$bool Real$float$__gt__ (Real$float, $float, $float);
$bool Real$float$__ge__ (Real$float, $float, $float);
$float Real$float$__float__ (Real$float, $float);
Integral$opaque Real$float$__trunc__ (Real$float, $float);
Integral$opaque Real$float$__floor__ (Real$float, $float);
Integral$opaque Real$float$__ceil__ (Real$float, $float);
Integral$opaque Real$float$__round__ (Real$float, $float);

// Complx$float ////////////////////////////////////////////////////////////

struct Complx$float {
    char *GCINFO;
    Complx$float$__class__  __class__;
    Real _Real;
    Plus _Plus;
    Minus _Minus;
};

struct Complx$float$__class__ {
    char *GCINFO;
    $bool (*__eq__)(Complx$float, $float, $float);
    $bool (*__ne__)(Complx$float, $float, $float);
    $complx (*__complx__)(Complx$float, $float);
    $bool (*__bool__)(Complx$float, $float);
    $float (*__mul__)(Complx$float, $float, $float);
    $float (*__truediv__)(Complx$float, $float, $float);
    $float (*__pow__)(Complx$float, $float, $float);
    $float (*__neg__)(Complx$float, $float);
    $float (*__pos__)(Complx$float, $float);
    Real$opaque (*real)(Complx$float, $float);
    Real$opaque (*imag)(Complx$float, $float);
    Real$opaque (*__abs__)(Complx$float, $float);
    $float (*conjugate)(Complx$float, $float);
};

$bool Complx$float$__eq__ (Complx$float, $float, $float);
$bool Complx$float$__ne__ (Complx$float, $float, $float);
$complx Complx$float$__complx__ (Complx$float, $float);
$bool Complx$float$__bool__ (Complx$float, $float);
$float Complx$float$__mul__ (Complx$float, $float, $float);
$float Complx$float$__truediv__ (Complx$float, $float, $float);
$float Complx$float$__pow__ (Complx$float, $float, $float);
$float Complx$float$__neg__ (Complx$float, $float);
$float Complx$float$__pos__ (Complx$float, $float);
Real$opaque Complx$float$real (Complx$float, $float);
Real$opaque Complx$float$imag (Complx$float, $float);
Real$opaque Complx$float$__abs__ (Complx$float, $float);
$float Complx$float$conjugate (Complx$float, $float);

// Plus$float ////////////////////////////////////////////////////////////

struct Plus$float {
    char *GCINFO;
    Plus$float$__class__  __class__;
    Real _Real;
};

struct Plus$float$__class__ {
    char *GCINFO;
    $float (*__add__)(Plus$float, $float, $float);
};

$float Plus$float$__add__ (Plus$float, $float, $float);

// Minus$float ////////////////////////////////////////////////////////////

struct Minus$float {
    char *GCINFO;
    Minus$float$__class__  __class__;
    Real _Real;
};

struct Minus$float$__class__ {
    char *GCINFO;
    $float (*__sub__)(Minus$float, $float, $float);
};

$float Minus$float$__sub__ (Minus$float, $float, $float);

// Hashable$float ////////////////////////////////////////////////////////////

struct Hashable$float {
    char *GCINFO;
    Hashable$float$__class__  __class__;
};

struct Hashable$float$__class__ {
    char *GCINFO;
    $bool (*__eq__)(Hashable$float, $float, $float);
    $bool (*__ne__)(Hashable$float, $float, $float);
    $int (*__hash__)(Hashable$float, $float);
};

$bool Hashable$float$__eq__ (Hashable$float, $float, $float);
$bool Hashable$float$__ne__ (Hashable$float, $float, $float);
$int Hashable$float$__hash__ (Hashable$float, $float);

$WORD next(Iterator it);
