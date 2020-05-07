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

struct $complex;
typedef struct $complex *$complex;

struct $bool;
typedef struct $bool *$bool;

struct $Iterator;
typedef struct $Iterator *$Iterator;

struct $Slice;
typedef struct $Slice *$Slice;

struct $Eq;
typedef struct $Eq *$Eq;

struct $Eq$class;
typedef struct $Eq$class *$Eq$class;

struct $Eq$opaque;
typedef struct $Eq$opaque *$Eq$opaque;

struct $Ord;
typedef struct $Ord *$Ord;

struct $Ord$class;
typedef struct $Ord$class *$Ord$class;

struct $Ord$opaque;
typedef struct $Ord$opaque *$Ord$opaque;

struct $Logical;
typedef struct $Logical *$Logical;

struct $Logical$class;
typedef struct $Logical$class *$Logical$class;

struct $Logical$opaque;
typedef struct $Logical$opaque *$Logical$opaque;

struct $Plus;
typedef struct $Plus *$Plus;

struct $Plus$class;
typedef struct $Plus$class *$Plus$class;

struct $Plus$opaque;
typedef struct $Plus$opaque *$Plus$opaque;

struct $Minus;
typedef struct $Minus *$Minus;

struct $Minus$class;
typedef struct $Minus$class *$Minus$class;

struct $Minus$opaque;
typedef struct $Minus$opaque *$Minus$opaque;

struct $Hashable;
typedef struct $Hashable *$Hashable;

struct $Hashable$class;
typedef struct $Hashable$class *$Hashable$class;

struct $Hashable$opaque;
typedef struct $Hashable$opaque *$Hashable$opaque;

struct $Boolean;
typedef struct $Boolean *$Boolean;

struct $Boolean$class;
typedef struct $Boolean$class *$Boolean$class;

struct $Boolean$opaque;
typedef struct $Boolean$opaque *$Boolean$opaque;

struct $Indexed;
typedef struct $Indexed *$Indexed;

struct $Indexed$class;
typedef struct $Indexed$class *$Indexed$class;

struct $Indexed$opaque;
typedef struct $Indexed$opaque *$Indexed$opaque;

struct $Sliceable;
typedef struct $Sliceable *$Sliceable;

struct $Sliceable$class;
typedef struct $Sliceable$class *$Sliceable$class;

struct $Sliceable$opaque;
typedef struct $Sliceable$opaque *$Sliceable$opaque;

struct $Iterable;
typedef struct $Iterable *$Iterable;

struct $Iterable$class;
typedef struct $Iterable$class *$Iterable$class;

struct $Iterable$opaque;
typedef struct $Iterable$opaque *$Iterable$opaque;

struct $Collection;
typedef struct $Collection *$Collection;

struct $Collection$class;
typedef struct $Collection$class *$Collection$class;

struct $Collection$opaque;
typedef struct $Collection$opaque *$Collection$opaque;

struct $Container;
typedef struct $Container *$Container;

struct $Container$class;
typedef struct $Container$class *$Container$class;

struct $Container$opaque;
typedef struct $Container$opaque *$Container$opaque;

struct $Sequence;
typedef struct $Sequence *$Sequence;

struct $Sequence$class;
typedef struct $Sequence$class *$Sequence$class;

struct $Sequence$opaque;
typedef struct $Sequence$opaque *$Sequence$opaque;

struct $Mapping;
typedef struct $Mapping *$Mapping;

struct $Mapping$class;
typedef struct $Mapping$class *$Mapping$class;

struct $Mapping$opaque;
typedef struct $Mapping$opaque *$Mapping$opaque;

struct $Set;
typedef struct $Set *$Set;

struct $Set$class;
typedef struct $Set$class *$Set$class;

struct $Set$opaque;
typedef struct $Set$opaque *$Set$opaque;

struct $Complex;
typedef struct $Complex *$Complex;

struct $Complex$class;
typedef struct $Complex$class *$Complex$class;

struct $Complex$opaque;
typedef struct $Complex$opaque *$Complex$opaque;

struct $Real;
typedef struct $Real *$Real;

struct $Real$class;
typedef struct $Real$class *$Real$class;

struct $Real$opaque;
typedef struct $Real$opaque *$Real$opaque;

struct $Rational;
typedef struct $Rational *$Rational;

struct $Rational$class;
typedef struct $Rational$class *$Rational$class;

struct $Rational$opaque;
typedef struct $Rational$opaque *$Rational$opaque;

struct $Integral;
typedef struct $Integral *$Integral;

struct $Integral$class;
typedef struct $Integral$class *$Integral$class;

struct $Integral$opaque;
typedef struct $Integral$opaque *$Integral$opaque;

struct $Boolean$int;
typedef struct $Boolean$int *$Boolean$int;

struct $Boolean$int$class;
typedef struct $Boolean$int$class *$Boolean$int$class;

struct $Boolean$list;
typedef struct $Boolean$list *$Boolean$list;

struct $Boolean$list$class;
typedef struct $Boolean$list$class *$Boolean$list$class;

struct $Sequence$list;
typedef struct $Sequence$list *$Sequence$list;

struct $Sequence$list$class;
typedef struct $Sequence$list$class *$Sequence$list$class;

struct $Collection$list;
typedef struct $Collection$list *$Collection$list;

struct $Collection$list$class;
typedef struct $Collection$list$class *$Collection$list$class;

struct $Plus$list;
typedef struct $Plus$list *$Plus$list;

struct $Plus$list$class;
typedef struct $Plus$list$class *$Plus$list$class;

struct $Container$list;
typedef struct $Container$list *$Container$list;

struct $Container$list$class;
typedef struct $Container$list$class *$Container$list$class;

struct $Mapping$dict;
typedef struct $Mapping$dict *$Mapping$dict;

struct $Mapping$dict$class;
typedef struct $Mapping$dict$class *$Mapping$dict$class;

struct $Indexed$dict;
typedef struct $Indexed$dict *$Indexed$dict;

struct $Indexed$dict$class;
typedef struct $Indexed$dict$class *$Indexed$dict$class;

struct $Set$set;
typedef struct $Set$set *$Set$set;

struct $Set$set$class;
typedef struct $Set$set$class *$Set$set$class;

struct $Ord$set;
typedef struct $Ord$set *$Ord$set;

struct $Ord$set$class;
typedef struct $Ord$set$class *$Ord$set$class;

struct $Logical$set;
typedef struct $Logical$set *$Logical$set;

struct $Logical$set$class;
typedef struct $Logical$set$class *$Logical$set$class;

struct $Minus$set;
typedef struct $Minus$set *$Minus$set;

struct $Minus$set$class;
typedef struct $Minus$set$class *$Minus$set$class;

struct $Iterable$Iterator;
typedef struct $Iterable$Iterator *$Iterable$Iterator;

struct $Iterable$Iterator$class;
typedef struct $Iterable$Iterator$class *$Iterable$Iterator$class;

struct $Ord$str;
typedef struct $Ord$str *$Ord$str;

struct $Ord$str$class;
typedef struct $Ord$str$class *$Ord$str$class;

struct $Container$str;
typedef struct $Container$str *$Container$str;

struct $Container$str$class;
typedef struct $Container$str$class *$Container$str$class;

struct $Sliceable$str;
typedef struct $Sliceable$str *$Sliceable$str;

struct $Sliceable$str$class;
typedef struct $Sliceable$str$class *$Sliceable$str$class;

struct $Plus$str;
typedef struct $Plus$str *$Plus$str;

struct $Plus$str$class;
typedef struct $Plus$str$class *$Plus$str$class;

struct $Hashable$str;
typedef struct $Hashable$str *$Hashable$str;

struct $Hashable$str$class;
typedef struct $Hashable$str$class *$Hashable$str$class;

struct $Integral$int;
typedef struct $Integral$int *$Integral$int;

struct $Integral$int$class;
typedef struct $Integral$int$class *$Integral$int$class;

struct $Logical$int;
typedef struct $Logical$int *$Logical$int;

struct $Logical$int$class;
typedef struct $Logical$int$class *$Logical$int$class;

struct $Complex$int;
typedef struct $Complex$int *$Complex$int;

struct $Complex$int$class;
typedef struct $Complex$int$class *$Complex$int$class;

struct $Plus$int;
typedef struct $Plus$int *$Plus$int;

struct $Plus$int$class;
typedef struct $Plus$int$class *$Plus$int$class;

struct $Minus$int;
typedef struct $Minus$int *$Minus$int;

struct $Minus$int$class;
typedef struct $Minus$int$class *$Minus$int$class;

struct $Hashable$int;
typedef struct $Hashable$int *$Hashable$int;

struct $Hashable$int$class;
typedef struct $Hashable$int$class *$Hashable$int$class;

struct $Real$float;
typedef struct $Real$float *$Real$float;

struct $Real$float$class;
typedef struct $Real$float$class *$Real$float$class;

struct $Complex$float;
typedef struct $Complex$float *$Complex$float;

struct $Complex$float$class;
typedef struct $Complex$float$class *$Complex$float$class;

struct $Plus$float;
typedef struct $Plus$float *$Plus$float;

struct $Plus$float$class;
typedef struct $Plus$float$class *$Plus$float$class;

struct $Minus$float;
typedef struct $Minus$float *$Minus$float;

struct $Minus$float$class;
typedef struct $Minus$float$class *$Minus$float$class;

struct $Hashable$float;
typedef struct $Hashable$float *$Hashable$float;

struct $Hashable$float$class;
typedef struct $Hashable$float$class *$Hashable$float$class;

// $Eq ////////////////////////////////////////////////////////////

struct $Eq {
    $Eq$class $class;
};

struct $Eq$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Eq);
    $bool (*__eq__)($Eq, $WORD, $WORD);
    $bool (*__ne__)($Eq, $WORD, $WORD);
};

struct $Eq$opaque {
    char *$GCINFO;
    $Eq proto;
    $WORD impl;
};

$Eq$opaque $Eq$pack($Eq proto, $WORD impl);


// $Ord ////////////////////////////////////////////////////////////

struct $Ord {
    $Ord$class $class;
};

struct $Ord$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Ord);
    $bool (*__eq__)($Ord, $WORD, $WORD);
    $bool (*__ne__)($Ord, $WORD, $WORD);
    $bool (*__lt__)($Ord, $WORD, $WORD);
    $bool (*__le__)($Ord, $WORD, $WORD);
    $bool (*__gt__)($Ord, $WORD, $WORD);
    $bool (*__ge__)($Ord, $WORD, $WORD);
};

struct $Ord$opaque {
    char *$GCINFO;
    $Ord proto;
    $WORD impl;
};

$Ord$opaque $Ord$pack($Ord proto, $WORD impl);


// $Logical ////////////////////////////////////////////////////////////

struct $Logical {
    $Logical$class $class;
};

struct $Logical$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Logical);
    $WORD (*__and__)($Logical, $WORD, $WORD);
    $WORD (*__or__)($Logical, $WORD, $WORD);
    $WORD (*__xor__)($Logical, $WORD, $WORD);
};

struct $Logical$opaque {
    char *$GCINFO;
    $Logical proto;
    $WORD impl;
};

$Logical$opaque $Logical$pack($Logical proto, $WORD impl);


// $Plus ////////////////////////////////////////////////////////////

struct $Plus {
    $Plus$class $class;
};

struct $Plus$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Plus);
    $WORD (*__add__)($Plus, $WORD, $WORD);
};

struct $Plus$opaque {
    char *$GCINFO;
    $Plus proto;
    $WORD impl;
};

$Plus$opaque $Plus$pack($Plus proto, $WORD impl);


// $Minus ////////////////////////////////////////////////////////////

struct $Minus {
    $Minus$class $class;
};

struct $Minus$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Minus);
    $WORD (*__sub__)($Minus, $WORD, $WORD);
};

struct $Minus$opaque {
    char *$GCINFO;
    $Minus proto;
    $WORD impl;
};

$Minus$opaque $Minus$pack($Minus proto, $WORD impl);


// $Hashable ////////////////////////////////////////////////////////////

struct $Hashable {
    $Hashable$class $class;
};

struct $Hashable$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Hashable);
    $bool (*__eq__)($Hashable, $WORD, $WORD);
    $bool (*__ne__)($Hashable, $WORD, $WORD);
    $int (*__hash__)($Hashable, $WORD);
};

struct $Hashable$opaque {
    char *$GCINFO;
    $Hashable proto;
    $WORD impl;
};

$Hashable$opaque $Hashable$pack($Hashable proto, $WORD impl);


// $Boolean ////////////////////////////////////////////////////////////

struct $Boolean {
    $Boolean$class $class;
};

struct $Boolean$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Boolean);
    $bool (*__bool__)($Boolean, $WORD);
};

struct $Boolean$opaque {
    char *$GCINFO;
    $Boolean proto;
    $WORD impl;
};

$Boolean$opaque $Boolean$pack($Boolean proto, $WORD impl);


// $Indexed ////////////////////////////////////////////////////////////

struct $Indexed {
    $Indexed$class $class;
    $Eq w$Eq$Indexed;
};

struct $Indexed$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Indexed, $Eq);
    $WORD (*__getitem__)($Indexed, $WORD, $WORD);
    void (*__setitem__)($Indexed, $WORD, $WORD, $WORD);
    void (*__delitem__)($Indexed, $WORD, $WORD);
};

struct $Indexed$opaque {
    char *$GCINFO;
    $Indexed proto;
    $WORD impl;
};

$Indexed$opaque $Indexed$pack($Indexed proto, $WORD impl);


// $Sliceable ////////////////////////////////////////////////////////////

struct $Sliceable {
    $Sliceable$class $class;
};

struct $Sliceable$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Sliceable);
    $WORD (*__getitem__)($Sliceable, $WORD, $int);
    void (*__setitem__)($Sliceable, $WORD, $int, $WORD);
    void (*__delitem__)($Sliceable, $WORD, $int);
    $WORD (*__getslice__)($Sliceable, $WORD, $Slice);
    void (*__setslice__)($Sliceable, $WORD, $Slice, $Iterable$opaque);
    void (*__delslice__)($Sliceable, $WORD, $Slice);
};

struct $Sliceable$opaque {
    char *$GCINFO;
    $Sliceable proto;
    $WORD impl;
};

$Sliceable$opaque $Sliceable$pack($Sliceable proto, $WORD impl);


// $Iterable ////////////////////////////////////////////////////////////

struct $Iterable {
    $Iterable$class $class;
};

struct $Iterable$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Iterable);
    $Iterator (*__iter__)($Iterable, $WORD);
};

struct $Iterable$opaque {
    char *$GCINFO;
    $Iterable proto;
    $WORD impl;
};

$Iterable$opaque $Iterable$pack($Iterable proto, $WORD impl);


// $Collection ////////////////////////////////////////////////////////////

struct $Collection {
    $Collection$class $class;
};

struct $Collection$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Collection);
    $Iterator (*__iter__)($Collection, $WORD);
    $WORD (*__fromiter__)($Collection, $Iterable$opaque);
    $int (*__len__)($Collection, $WORD);
};

struct $Collection$opaque {
    char *$GCINFO;
    $Collection proto;
    $WORD impl;
};

$Collection$opaque $Collection$pack($Collection proto, $WORD impl);


// $Container ////////////////////////////////////////////////////////////

struct $Container {
    $Container$class $class;
    $Eq w$Eq$A;
};

struct $Container$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Container, $Eq);
    $Iterator (*__iter__)($Container, $WORD);
    $WORD (*__fromiter__)($Container, $Iterable$opaque);
    $int (*__len__)($Container, $WORD);
    $bool (*__contains__)($Container, $WORD, $WORD);
    $bool (*__containsnot__)($Container, $WORD, $WORD);
};

struct $Container$opaque {
    char *$GCINFO;
    $Container proto;
    $WORD impl;
};

$Container$opaque $Container$pack($Container proto, $WORD impl);


// $Sequence ////////////////////////////////////////////////////////////

struct $Sequence {
    $Sequence$class $class;
    $Collection w$Collection$Sequence;
    $Plus w$Plus$Sequence;
};

struct $Sequence$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Sequence);
    $WORD (*__getitem__)($Sequence, $WORD, $int);
    void (*__setitem__)($Sequence, $WORD, $int, $WORD);
    void (*__delitem__)($Sequence, $WORD, $int);
    $WORD (*__getslice__)($Sequence, $WORD, $Slice);
    void (*__setslice__)($Sequence, $WORD, $Slice, $Iterable$opaque);
    void (*__delslice__)($Sequence, $WORD, $Slice);
    $Iterator (*__reversed__)($Sequence, $WORD);
    void (*insert)($Sequence, $WORD, $int, $WORD);
    void (*append)($Sequence, $WORD, $WORD);
    void (*reverse)($Sequence, $WORD);
};

struct $Sequence$opaque {
    char *$GCINFO;
    $Sequence proto;
    $WORD impl;
};

$Sequence$opaque $Sequence$pack($Sequence proto, $WORD impl);


// $Mapping ////////////////////////////////////////////////////////////

struct $Mapping {
    $Mapping$class $class;
    $Indexed w$Indexed$Mapping;
    $Eq w$Eq$A;
};

struct $Mapping$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Mapping, $Eq);
    $Iterator (*__iter__)($Mapping, $WORD);
    $WORD (*__fromiter__)($Mapping, $Iterable$opaque);
    $int (*__len__)($Mapping, $WORD);
    $bool (*__contains__)($Mapping, $WORD, $WORD);
    $bool (*__containsnot__)($Mapping, $WORD, $WORD);
    $WORD (*get)($Mapping, $WORD, $WORD, $WORD);
    $Iterator (*keys)($Mapping, $WORD);
    $Iterator (*values)($Mapping, $WORD);
    $Iterator (*items)($Mapping, $WORD);
    void (*update)($Mapping, $WORD, $Iterable$opaque);
    $tup2_t (*popitem)($Mapping, $WORD);
    void (*setdefault)($Mapping, $WORD, $WORD, $WORD);
};

struct $Mapping$opaque {
    char *$GCINFO;
    $Mapping proto;
    $WORD impl;
};

$Mapping$opaque $Mapping$pack($Mapping proto, $WORD impl);


// $Set ////////////////////////////////////////////////////////////

struct $Set {
    $Set$class $class;
    $Ord w$Ord$Set;
    $Logical w$Logical$Set;
    $Minus w$Minus$Set;
    $Eq w$Eq$Set;
};

struct $Set$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Set, $Eq);
    $Iterator (*__iter__)($Set, $WORD);
    $WORD (*__fromiter__)($Set, $Iterable$opaque);
    $int (*__len__)($Set, $WORD);
    $bool (*__contains__)($Set, $WORD, $WORD);
    $bool (*__containsnot__)($Set, $WORD, $WORD);
    $bool (*isdisjoint)($Set, $WORD, $WORD);
    void (*add)($Set, $WORD, $WORD);
    void (*discard)($Set, $WORD, $WORD);
    $WORD (*pop)($Set, $WORD);
};

struct $Set$opaque {
    char *$GCINFO;
    $Set proto;
    $WORD impl;
};

$Set$opaque $Set$pack($Set proto, $WORD impl);


// $Complex ////////////////////////////////////////////////////////////

struct $Complex {
    $Complex$class $class;
    $Plus w$Plus$Complex;
    $Minus w$Minus$Complex;
};

struct $Complex$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Complex);
    $bool (*__eq__)($Complex, $WORD, $WORD);
    $bool (*__ne__)($Complex, $WORD, $WORD);
    $complex (*__complx__)($Complex, $WORD);
    $bool (*__bool__)($Complex, $WORD);
    $WORD (*__mul__)($Complex, $WORD, $WORD);
    $WORD (*__truediv__)($Complex, $WORD, $WORD);
    $WORD (*__pow__)($Complex, $WORD, $WORD);
    $WORD (*__neg__)($Complex, $WORD);
    $WORD (*__pos__)($Complex, $WORD);
    $Real$opaque (*real)($Complex, $WORD);
    $Real$opaque (*imag)($Complex, $WORD);
    $Real$opaque (*__abs__)($Complex, $WORD);
    $WORD (*conjugate)($Complex, $WORD);
};

struct $Complex$opaque {
    char *$GCINFO;
    $Complex proto;
    $WORD impl;
};

$Complex$opaque $Complex$pack($Complex proto, $WORD impl);


// $Real ////////////////////////////////////////////////////////////

struct $Real {
    $Real$class $class;
    $Complex w$Complex$Real;
};

struct $Real$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Real);
    $bool (*__eq__)($Real, $WORD, $WORD);
    $bool (*__ne__)($Real, $WORD, $WORD);
    $bool (*__lt__)($Real, $WORD, $WORD);
    $bool (*__le__)($Real, $WORD, $WORD);
    $bool (*__gt__)($Real, $WORD, $WORD);
    $bool (*__ge__)($Real, $WORD, $WORD);
    $float (*__float__)($Real, $WORD);
    $Integral$opaque (*__trunc__)($Real, $WORD);
    $Integral$opaque (*__floor__)($Real, $WORD);
    $Integral$opaque (*__ceil__)($Real, $WORD);
    $Integral$opaque (*__round__)($Real, $WORD);
};

struct $Real$opaque {
    char *$GCINFO;
    $Real proto;
    $WORD impl;
};

$Real$opaque $Real$pack($Real proto, $WORD impl);


// $Rational ////////////////////////////////////////////////////////////

struct $Rational {
    $Rational$class $class;
};

struct $Rational$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Rational);
    $bool (*__eq__)($Rational, $WORD, $WORD);
    $bool (*__ne__)($Rational, $WORD, $WORD);
    $bool (*__lt__)($Rational, $WORD, $WORD);
    $bool (*__le__)($Rational, $WORD, $WORD);
    $bool (*__gt__)($Rational, $WORD, $WORD);
    $bool (*__ge__)($Rational, $WORD, $WORD);
    $float (*__float__)($Rational, $WORD);
    $Integral$opaque (*__trunc__)($Rational, $WORD);
    $Integral$opaque (*__floor__)($Rational, $WORD);
    $Integral$opaque (*__ceil__)($Rational, $WORD);
    $Integral$opaque (*__round__)($Rational, $WORD);
    $Integral$opaque (*numerator)($Rational, $WORD);
    $Integral$opaque (*denominator)($Rational, $WORD);
};

struct $Rational$opaque {
    char *$GCINFO;
    $Rational proto;
    $WORD impl;
};

$Rational$opaque $Rational$pack($Rational proto, $WORD impl);


// $Integral ////////////////////////////////////////////////////////////

struct $Integral {
    $Integral$class $class;
    $Logical w$Logical$Integral;
};

struct $Integral$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Integral);
    $bool (*__eq__)($Integral, $WORD, $WORD);
    $bool (*__ne__)($Integral, $WORD, $WORD);
    $bool (*__lt__)($Integral, $WORD, $WORD);
    $bool (*__le__)($Integral, $WORD, $WORD);
    $bool (*__gt__)($Integral, $WORD, $WORD);
    $bool (*__ge__)($Integral, $WORD, $WORD);
    $float (*__float__)($Integral, $WORD);
    $Integral$opaque (*__trunc__)($Integral, $WORD);
    $Integral$opaque (*__floor__)($Integral, $WORD);
    $Integral$opaque (*__ceil__)($Integral, $WORD);
    $Integral$opaque (*__round__)($Integral, $WORD);
    $Integral$opaque (*numerator)($Integral, $WORD);
    $Integral$opaque (*denominator)($Integral, $WORD);
    $int (*__int__)($Integral, $WORD);
    $int (*__index__)($Integral, $WORD);
    $tup2_t (*__divmod__)($Integral, $WORD, $WORD);
    $WORD (*__floordiv__)($Integral, $WORD, $WORD);
    $WORD (*__mod__)($Integral, $WORD, $WORD);
    $WORD (*__lshift__)($Integral, $WORD, $WORD);
    $WORD (*__rshift__)($Integral, $WORD, $WORD);
    $WORD (*__invert__)($Integral, $WORD);
};

struct $Integral$opaque {
    char *$GCINFO;
    $Integral proto;
    $WORD impl;
};

$Integral$opaque $Integral$pack($Integral proto, $WORD impl);


// $Boolean$int ////////////////////////////////////////////////////////////

struct $Boolean$int {
    $Boolean$int$class $class;
};

struct $Boolean$int$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Boolean$int);
    $bool (*__bool__)($Boolean$int, $int);
};

void $Boolean$int$__init__ ($Boolean$int);
$bool $Boolean$int$__bool__ ($Boolean$int, $int);

// $Boolean$list ////////////////////////////////////////////////////////////

struct $Boolean$list {
    $Boolean$list$class $class;
};

struct $Boolean$list$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Boolean$list);
    $bool (*__bool__)($Boolean$list, $list);
};

void $Boolean$list$__init__ ($Boolean$list);
$bool $Boolean$list$__bool__ ($Boolean$list, $list);

// $Sequence$list ////////////////////////////////////////////////////////////

struct $Sequence$list {
    $Sequence$list$class $class;
    $Collection$list w$Collection$Sequence;
    $Plus$list w$Plus$Sequence;
};

struct $Sequence$list$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Sequence$list);
    $WORD (*__getitem__)($Sequence$list, $list, $int);
    void (*__setitem__)($Sequence$list, $list, $int, $WORD);
    void (*__delitem__)($Sequence$list, $list, $int);
    $list (*__getslice__)($Sequence$list, $list, $Slice);
    void (*__setslice__)($Sequence$list, $list, $Slice, $Iterable$opaque);
    void (*__delslice__)($Sequence$list, $list, $Slice);
    $Iterator (*__reversed__)($Sequence$list, $list);
    void (*insert)($Sequence$list, $list, $int, $WORD);
    void (*append)($Sequence$list, $list, $WORD);
    void (*reverse)($Sequence$list, $list);
};

void $Sequence$list$__init__ ($Sequence$list);
$WORD $Sequence$list$__getitem__ ($Sequence$list, $list, $int);
void $Sequence$list$__setitem__ ($Sequence$list, $list, $int, $WORD);
void $Sequence$list$__delitem__ ($Sequence$list, $list, $int);
$list $Sequence$list$__getslice__ ($Sequence$list, $list, $Slice);
void $Sequence$list$__setslice__ ($Sequence$list, $list, $Slice, $Iterable$opaque);
void $Sequence$list$__delslice__ ($Sequence$list, $list, $Slice);
$Iterator $Sequence$list$__reversed__ ($Sequence$list, $list);
void $Sequence$list$insert ($Sequence$list, $list, $int, $WORD);
void $Sequence$list$append ($Sequence$list, $list, $WORD);
void $Sequence$list$reverse ($Sequence$list, $list);

// $Collection$list ////////////////////////////////////////////////////////////

struct $Collection$list {
    $Collection$list$class $class;
    $Sequence$list w$Sequence$list;
};

struct $Collection$list$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Collection$list, $Sequence$list);
    $Iterator (*__iter__)($Collection$list, $list);
    $list (*__fromiter__)($Collection$list, $Iterable$opaque);
    $int (*__len__)($Collection$list, $list);
};

void $Collection$list$__init__ ($Collection$list, $Sequence$list);
$Iterator $Collection$list$__iter__ ($Collection$list, $list);
$list $Collection$list$__fromiter__ ($Collection$list, $Iterable$opaque);
$int $Collection$list$__len__ ($Collection$list, $list);

// $Plus$list ////////////////////////////////////////////////////////////

struct $Plus$list {
    $Plus$list$class $class;
    $Sequence$list w$Sequence$list;
};

struct $Plus$list$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Plus$list, $Sequence$list);
    $list (*__add__)($Plus$list, $list, $list);
};

void $Plus$list$__init__ ($Plus$list, $Sequence$list);
$list $Plus$list$__add__ ($Plus$list, $list, $list);

// $Container$list ////////////////////////////////////////////////////////////

struct $Container$list {
    $Container$list$class $class;
    $Eq w$Eq$A;
};

struct $Container$list$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Container$list, $Eq);
    $Iterator (*__iter__)($Container$list, $list);
    $list (*__fromiter__)($Container$list, $Iterable$opaque);
    $int (*__len__)($Container$list, $list);
    $bool (*__contains__)($Container$list, $list, $WORD);
    $bool (*__containsnot__)($Container$list, $list, $WORD);
};

void $Container$list$__init__ ($Container$list, $Eq);
$Iterator $Container$list$__iter__ ($Container$list, $list);
$list $Container$list$__fromiter__ ($Container$list, $Iterable$opaque);
$int $Container$list$__len__ ($Container$list, $list);
$bool $Container$list$__contains__ ($Container$list, $list, $WORD);
$bool $Container$list$__containsnot__ ($Container$list, $list, $WORD);

// $Mapping$dict ////////////////////////////////////////////////////////////

struct $Mapping$dict {
    $Mapping$dict$class $class;
    $Indexed$dict w$Indexed$Mapping;
    $Hashable w$Hashable$Mapping;
};

struct $Mapping$dict$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Mapping$dict, $Hashable);
    $Iterator (*__iter__)($Mapping$dict, $dict);
    $dict (*__fromiter__)($Mapping$dict, $Iterable$opaque);
    $int (*__len__)($Mapping$dict, $dict);
    $bool (*__contains__)($Mapping$dict, $dict, $WORD);
    $bool (*__containsnot__)($Mapping$dict, $dict, $WORD);
    $WORD (*get)($Mapping$dict, $dict, $WORD, $WORD);
    $Iterator (*keys)($Mapping$dict, $dict);
    $Iterator (*values)($Mapping$dict, $dict);
    $Iterator (*items)($Mapping$dict, $dict);
    void (*update)($Mapping$dict, $dict, $Iterable$opaque);
    $tup2_t (*popitem)($Mapping$dict, $dict);
    void (*setdefault)($Mapping$dict, $dict, $WORD, $WORD);
};

void $Mapping$dict$__init__ ($Mapping$dict, $Hashable);
$Iterator $Mapping$dict$__iter__ ($Mapping$dict, $dict);
$dict $Mapping$dict$__fromiter__ ($Mapping$dict, $Iterable$opaque);
$int $Mapping$dict$__len__ ($Mapping$dict, $dict);
$bool $Mapping$dict$__contains__ ($Mapping$dict, $dict, $WORD);
$bool $Mapping$dict$__containsnot__ ($Mapping$dict, $dict, $WORD);
$WORD $Mapping$dict$get ($Mapping$dict, $dict, $WORD, $WORD);
$Iterator $Mapping$dict$keys ($Mapping$dict, $dict);
$Iterator $Mapping$dict$values ($Mapping$dict, $dict);
$Iterator $Mapping$dict$items ($Mapping$dict, $dict);
void $Mapping$dict$update ($Mapping$dict, $dict, $Iterable$opaque);
$tup2_t $Mapping$dict$popitem ($Mapping$dict, $dict);
void $Mapping$dict$setdefault ($Mapping$dict, $dict, $WORD, $WORD);

// $Indexed$dict ////////////////////////////////////////////////////////////

struct $Indexed$dict {
    $Indexed$dict$class $class;
    $Mapping$dict w$Mapping$dict;
    $Eq w$Eq$A;
};

struct $Indexed$dict$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Indexed$dict, $Mapping$dict, $Eq);
    $WORD (*__getitem__)($Indexed$dict, $dict, $WORD);
    void (*__setitem__)($Indexed$dict, $dict, $WORD, $WORD);
    void (*__delitem__)($Indexed$dict, $dict, $WORD);
};

void $Indexed$dict$__init__ ($Indexed$dict, $Mapping$dict, $Eq);
$WORD $Indexed$dict$__getitem__ ($Indexed$dict, $dict, $WORD);
void $Indexed$dict$__setitem__ ($Indexed$dict, $dict, $WORD, $WORD);
void $Indexed$dict$__delitem__ ($Indexed$dict, $dict, $WORD);

// $Set$set ////////////////////////////////////////////////////////////

struct $Set$set {
    $Set$set$class $class;
    $Ord$set w$Ord$Set;
    $Logical$set w$Logical$Set;
    $Minus$set w$Minus$Set;
    $Hashable w$Hashable$Set;
};

struct $Set$set$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Set$set, $Hashable);
    $Iterator (*__iter__)($Set$set, $set);
    $set (*__fromiter__)($Set$set, $Iterable$opaque);
    $int (*__len__)($Set$set, $set);
    $bool (*__contains__)($Set$set, $set, $WORD);
    $bool (*__containsnot__)($Set$set, $set, $WORD);
    $bool (*isdisjoint)($Set$set, $set, $set);
    void (*add)($Set$set, $set, $WORD);
    void (*discard)($Set$set, $set, $WORD);
    $WORD (*pop)($Set$set, $set);
};

void $Set$set$__init__ ($Set$set, $Hashable);
$Iterator $Set$set$__iter__ ($Set$set, $set);
$set $Set$set$__fromiter__ ($Set$set, $Iterable$opaque);
$int $Set$set$__len__ ($Set$set, $set);
$bool $Set$set$__contains__ ($Set$set, $set, $WORD);
$bool $Set$set$__containsnot__ ($Set$set, $set, $WORD);
$bool $Set$set$isdisjoint ($Set$set, $set, $set);
void $Set$set$add ($Set$set, $set, $WORD);
void $Set$set$discard ($Set$set, $set, $WORD);
$WORD $Set$set$pop ($Set$set, $set);

// $Ord$set ////////////////////////////////////////////////////////////

struct $Ord$set {
    $Ord$set$class $class;
    $Set$set w$Set$set;
};

struct $Ord$set$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Ord$set, $Set$set);
    $bool (*__eq__)($Ord$set, $set, $set);
    $bool (*__ne__)($Ord$set, $set, $set);
    $bool (*__lt__)($Ord$set, $set, $set);
    $bool (*__le__)($Ord$set, $set, $set);
    $bool (*__gt__)($Ord$set, $set, $set);
    $bool (*__ge__)($Ord$set, $set, $set);
};

void $Ord$set$__init__ ($Ord$set, $Set$set);
$bool $Ord$set$__eq__ ($Ord$set, $set, $set);
$bool $Ord$set$__ne__ ($Ord$set, $set, $set);
$bool $Ord$set$__lt__ ($Ord$set, $set, $set);
$bool $Ord$set$__le__ ($Ord$set, $set, $set);
$bool $Ord$set$__gt__ ($Ord$set, $set, $set);
$bool $Ord$set$__ge__ ($Ord$set, $set, $set);

// $Logical$set ////////////////////////////////////////////////////////////

struct $Logical$set {
    $Logical$set$class $class;
    $Set$set w$Set$set;
};

struct $Logical$set$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Logical$set, $Set$set);
    $set (*__and__)($Logical$set, $set, $set);
    $set (*__or__)($Logical$set, $set, $set);
    $set (*__xor__)($Logical$set, $set, $set);
};

void $Logical$set$__init__ ($Logical$set, $Set$set);
$set $Logical$set$__and__ ($Logical$set, $set, $set);
$set $Logical$set$__or__ ($Logical$set, $set, $set);
$set $Logical$set$__xor__ ($Logical$set, $set, $set);

// $Minus$set ////////////////////////////////////////////////////////////

struct $Minus$set {
    $Minus$set$class $class;
    $Set$set w$Set$set;
};

struct $Minus$set$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Minus$set, $Set$set);
    $set (*__sub__)($Minus$set, $set, $set);
};

void $Minus$set$__init__ ($Minus$set, $Set$set);
$set $Minus$set$__sub__ ($Minus$set, $set, $set);

// $Iterable$Iterator ////////////////////////////////////////////////////////////

struct $Iterable$Iterator {
    $Iterable$Iterator$class $class;
};

struct $Iterable$Iterator$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Iterable$Iterator);
    $Iterator (*__iter__)($Iterable$Iterator, $Iterator);
};

void $Iterable$Iterator$__init__ ($Iterable$Iterator);
$Iterator $Iterable$Iterator$__iter__ ($Iterable$Iterator, $Iterator);

// $Ord$str ////////////////////////////////////////////////////////////

struct $Ord$str {
    $Ord$str$class $class;
};

struct $Ord$str$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Ord$str);
    $bool (*__eq__)($Ord$str, $str, $str);
    $bool (*__ne__)($Ord$str, $str, $str);
    $bool (*__lt__)($Ord$str, $str, $str);
    $bool (*__le__)($Ord$str, $str, $str);
    $bool (*__gt__)($Ord$str, $str, $str);
    $bool (*__ge__)($Ord$str, $str, $str);
};

void $Ord$str$__init__ ($Ord$str);
$bool $Ord$str$__eq__ ($Ord$str, $str, $str);
$bool $Ord$str$__ne__ ($Ord$str, $str, $str);
$bool $Ord$str$__lt__ ($Ord$str, $str, $str);
$bool $Ord$str$__le__ ($Ord$str, $str, $str);
$bool $Ord$str$__gt__ ($Ord$str, $str, $str);
$bool $Ord$str$__ge__ ($Ord$str, $str, $str);

// $Container$str ////////////////////////////////////////////////////////////

struct $Container$str {
    $Container$str$class $class;
    $Eq w$Eq$A;
};

struct $Container$str$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Container$str, $Eq);
    $Iterator (*__iter__)($Container$str, $str);
    $str (*__fromiter__)($Container$str, $Iterable$opaque);
    $int (*__len__)($Container$str, $str);
    $bool (*__contains__)($Container$str, $str, $str);
    $bool (*__containsnot__)($Container$str, $str, $str);
};

void $Container$str$__init__ ($Container$str, $Eq);
$Iterator $Container$str$__iter__ ($Container$str, $str);
$str $Container$str$__fromiter__ ($Container$str, $Iterable$opaque);
$int $Container$str$__len__ ($Container$str, $str);
$bool $Container$str$__contains__ ($Container$str, $str, $str);
$bool $Container$str$__containsnot__ ($Container$str, $str, $str);

// $Sliceable$str ////////////////////////////////////////////////////////////

struct $Sliceable$str {
    $Sliceable$str$class $class;
};

struct $Sliceable$str$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Sliceable$str);
    $str (*__getitem__)($Sliceable$str, $str, $int);
    void (*__setitem__)($Sliceable$str, $str, $int, $str);
    void (*__delitem__)($Sliceable$str, $str, $int);
    $str (*__getslice__)($Sliceable$str, $str, $Slice);
    void (*__setslice__)($Sliceable$str, $str, $Slice, $Iterable$opaque);
    void (*__delslice__)($Sliceable$str, $str, $Slice);
};

void $Sliceable$str$__init__ ($Sliceable$str);
$str $Sliceable$str$__getitem__ ($Sliceable$str, $str, $int);
void $Sliceable$str$__setitem__ ($Sliceable$str, $str, $int, $str);
void $Sliceable$str$__delitem__ ($Sliceable$str, $str, $int);
$str $Sliceable$str$__getslice__ ($Sliceable$str, $str, $Slice);
void $Sliceable$str$__setslice__ ($Sliceable$str, $str, $Slice, $Iterable$opaque);
void $Sliceable$str$__delslice__ ($Sliceable$str, $str, $Slice);

// $Plus$str ////////////////////////////////////////////////////////////

struct $Plus$str {
    $Plus$str$class $class;
};

struct $Plus$str$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Plus$str);
    $str (*__add__)($Plus$str, $str, $str);
};

void $Plus$str$__init__ ($Plus$str);
$str $Plus$str$__add__ ($Plus$str, $str, $str);

// $Hashable$str ////////////////////////////////////////////////////////////

struct $Hashable$str {
    $Hashable$str$class $class;
};

struct $Hashable$str$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Hashable$str);
    $bool (*__eq__)($Hashable$str, $str, $str);
    $bool (*__ne__)($Hashable$str, $str, $str);
    $int (*__hash__)($Hashable$str, $str);
};

void $Hashable$str$__init__ ($Hashable$str);
$bool $Hashable$str$__eq__ ($Hashable$str, $str, $str);
$bool $Hashable$str$__ne__ ($Hashable$str, $str, $str);
$int $Hashable$str$__hash__ ($Hashable$str, $str);

// $Integral$int ////////////////////////////////////////////////////////////

struct $Integral$int {
    $Integral$int$class $class;
    $Logical$int w$Logical$Integral;
    $Complex$int w$Complex$Integral;
};

struct $Integral$int$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Integral$int);
    $bool (*__eq__)($Integral$int, $int, $int);
    $bool (*__ne__)($Integral$int, $int, $int);
    $bool (*__lt__)($Integral$int, $int, $int);
    $bool (*__le__)($Integral$int, $int, $int);
    $bool (*__gt__)($Integral$int, $int, $int);
    $bool (*__ge__)($Integral$int, $int, $int);
    $float (*__float__)($Integral$int, $int);
    $Integral$opaque (*__trunc__)($Integral$int, $int);
    $Integral$opaque (*__floor__)($Integral$int, $int);
    $Integral$opaque (*__ceil__)($Integral$int, $int);
    $Integral$opaque (*__round__)($Integral$int, $int);
    $Integral$opaque (*numerator)($Integral$int, $int);
    $Integral$opaque (*denominator)($Integral$int, $int);
    $int (*__int__)($Integral$int, $int);
    $int (*__index__)($Integral$int, $int);
    $tup2_t (*__divmod__)($Integral$int, $int, $int);
    $int (*__floordiv__)($Integral$int, $int, $int);
    $int (*__mod__)($Integral$int, $int, $int);
    $int (*__lshift__)($Integral$int, $int, $int);
    $int (*__rshift__)($Integral$int, $int, $int);
    $int (*__invert__)($Integral$int, $int);
};

void $Integral$int$__init__ ($Integral$int);
$bool $Integral$int$__eq__ ($Integral$int, $int, $int);
$bool $Integral$int$__ne__ ($Integral$int, $int, $int);
$bool $Integral$int$__lt__ ($Integral$int, $int, $int);
$bool $Integral$int$__le__ ($Integral$int, $int, $int);
$bool $Integral$int$__gt__ ($Integral$int, $int, $int);
$bool $Integral$int$__ge__ ($Integral$int, $int, $int);
$float $Integral$int$__float__ ($Integral$int, $int);
$Integral$opaque $Integral$int$__trunc__ ($Integral$int, $int);
$Integral$opaque $Integral$int$__floor__ ($Integral$int, $int);
$Integral$opaque $Integral$int$__ceil__ ($Integral$int, $int);
$Integral$opaque $Integral$int$__round__ ($Integral$int, $int);
$Integral$opaque $Integral$int$numerator ($Integral$int, $int);
$Integral$opaque $Integral$int$denominator ($Integral$int, $int);
$int $Integral$int$__int__ ($Integral$int, $int);
$int $Integral$int$__index__ ($Integral$int, $int);
$tup2_t $Integral$int$__divmod__ ($Integral$int, $int, $int);
$int $Integral$int$__floordiv__ ($Integral$int, $int, $int);
$int $Integral$int$__mod__ ($Integral$int, $int, $int);
$int $Integral$int$__lshift__ ($Integral$int, $int, $int);
$int $Integral$int$__rshift__ ($Integral$int, $int, $int);
$int $Integral$int$__invert__ ($Integral$int, $int);

// $Logical$int ////////////////////////////////////////////////////////////

struct $Logical$int {
    $Logical$int$class $class;
    $Integral$int w$Integral$int;
};

struct $Logical$int$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Logical$int, $Integral$int);
    $int (*__and__)($Logical$int, $int, $int);
    $int (*__or__)($Logical$int, $int, $int);
    $int (*__xor__)($Logical$int, $int, $int);
};

void $Logical$int$__init__ ($Logical$int, $Integral$int);
$int $Logical$int$__and__ ($Logical$int, $int, $int);
$int $Logical$int$__or__ ($Logical$int, $int, $int);
$int $Logical$int$__xor__ ($Logical$int, $int, $int);

// $Complex$int ////////////////////////////////////////////////////////////

struct $Complex$int {
    $Complex$int$class $class;
    $Integral$int w$Integral$int;
    $Plus$int w$Plus$Complex;
    $Minus$int w$Minus$Complex;
};

struct $Complex$int$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Complex$int, $Integral$int);
    $bool (*__eq__)($Complex$int, $int, $int);
    $bool (*__ne__)($Complex$int, $int, $int);
    $complex (*__complx__)($Complex$int, $int);
    $bool (*__bool__)($Complex$int, $int);
    $int (*__mul__)($Complex$int, $int, $int);
    $int (*__truediv__)($Complex$int, $int, $int);
    $int (*__pow__)($Complex$int, $int, $int);
    $int (*__neg__)($Complex$int, $int);
    $int (*__pos__)($Complex$int, $int);
    $Real$opaque (*real)($Complex$int, $int);
    $Real$opaque (*imag)($Complex$int, $int);
    $Real$opaque (*__abs__)($Complex$int, $int);
    $int (*conjugate)($Complex$int, $int);
};

void $Complex$int$__init__ ($Complex$int, $Integral$int);
$bool $Complex$int$__eq__ ($Complex$int, $int, $int);
$bool $Complex$int$__ne__ ($Complex$int, $int, $int);
$complex $Complex$int__complx__ ($Complex$int, $int);
$bool $Complex$int$__bool__ ($Complex$int, $int);
$int $Complex$int$__mul__ ($Complex$int, $int, $int);
$int $Complex$int$__truediv__ ($Complex$int, $int, $int);
$int $Complex$int$__pow__ ($Complex$int, $int, $int);
$int $Complex$int$__neg__ ($Complex$int, $int);
$int $Complex$int$__pos__ ($Complex$int, $int);
$Real$opaque $Complex$int$real ($Complex$int, $int);
$Real$opaque $Complex$int$imag ($Complex$int, $int);
$Real$opaque $Complex$int$__abs__ ($Complex$int, $int);
$int $Complex$int$conjugate ($Complex$int, $int);

// $Plus$int ////////////////////////////////////////////////////////////

struct $Plus$int {
    $Plus$int$class $class;
    $Complex$int w$Complex$int;
};

struct $Plus$int$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Plus$int, $Complex$int);
    $int (*__add__)($Plus$int, $int, $int);
};

void $Plus$int$__init__ ($Plus$int, $Complex$int);
$int $Plus$int$__add__ ($Plus$int, $int, $int);

// $Minus$int ////////////////////////////////////////////////////////////

struct $Minus$int {
    $Minus$int$class $class;
    $Complex$int w$Complex$int;
};

struct $Minus$int$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Minus$int, $Complex$int);
    $int (*__sub__)($Minus$int, $int, $int);
};

void $Minus$int$__init__ ($Minus$int, $Complex$int);
$int $Minus$int$__sub__ ($Minus$int, $int, $int);

// $Hashable$int ////////////////////////////////////////////////////////////

struct $Hashable$int {
    $Hashable$int$class $class;
};

struct $Hashable$int$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Hashable$int);
    $bool (*__eq__)($Hashable$int, $int, $int);
    $bool (*__ne__)($Hashable$int, $int, $int);
    $int (*__hash__)($Hashable$int, $int);
};

void $Hashable$int$__init__ ($Hashable$int);
$bool $Hashable$int$__eq__ ($Hashable$int, $int, $int);
$bool $Hashable$int$__ne__ ($Hashable$int, $int, $int);
$int $Hashable$int$__hash__ ($Hashable$int, $int);

// $Real$float ////////////////////////////////////////////////////////////

struct $Real$float {
    $Real$float$class $class;
    $Complex$float w$Complex$Real;
};

struct $Real$float$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Real$float);
    $bool (*__eq__)($Real$float, $float, $float);
    $bool (*__ne__)($Real$float, $float, $float);
    $bool (*__lt__)($Real$float, $float, $float);
    $bool (*__le__)($Real$float, $float, $float);
    $bool (*__gt__)($Real$float, $float, $float);
    $bool (*__ge__)($Real$float, $float, $float);
    $float (*__float__)($Real$float, $float);
    $Integral$opaque (*__trunc__)($Real$float, $float);
    $Integral$opaque (*__floor__)($Real$float, $float);
    $Integral$opaque (*__ceil__)($Real$float, $float);
    $Integral$opaque (*__round__)($Real$float, $float);
};

void $Real$float$__init__ ($Real$float);
$bool $Real$float$__eq__ ($Real$float, $float, $float);
$bool $Real$float$__ne__ ($Real$float, $float, $float);
$bool $Real$float$__lt__ ($Real$float, $float, $float);
$bool $Real$float$__le__ ($Real$float, $float, $float);
$bool $Real$float$__gt__ ($Real$float, $float, $float);
$bool $Real$float$__ge__ ($Real$float, $float, $float);
$float $Real$float$__float__ ($Real$float, $float);
$Integral$opaque $Real$float$__trunc__ ($Real$float, $float);
$Integral$opaque $Real$float$__floor__ ($Real$float, $float);
$Integral$opaque $Real$float$__ceil__ ($Real$float, $float);
$Integral$opaque $Real$float$__round__ ($Real$float, $float);

// $Complex$float ////////////////////////////////////////////////////////////

struct $Complex$float {
    $Complex$float$class $class;
    $Real$float w$Real$float;
    $Plus$float w$Plus$Complex;
    $Minus$float w$Minus$Complex;
};

struct $Complex$float$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Complex$float, $Real$float);
    $bool (*__eq__)($Complex$float, $float, $float);
    $bool (*__ne__)($Complex$float, $float, $float);
    $complex (*__complx__)($Complex$float, $float);
    $bool (*__bool__)($Complex$float, $float);
    $float (*__mul__)($Complex$float, $float, $float);
    $float (*__truediv__)($Complex$float, $float, $float);
    $float (*__pow__)($Complex$float, $float, $float);
    $float (*__neg__)($Complex$float, $float);
    $float (*__pos__)($Complex$float, $float);
    $Real$opaque (*real)($Complex$float, $float);
    $Real$opaque (*imag)($Complex$float, $float);
    $Real$opaque (*__abs__)($Complex$float, $float);
    $float (*conjugate)($Complex$float, $float);
};

void $Complex$float$__init__ ($Complex$float, $Real$float);
$bool $Complex$float$__eq__ ($Complex$float, $float, $float);
$bool $Complex$float$__ne__ ($Complex$float, $float, $float);
$complex $Complex$float__complx__ ($Complex$float, $float);
$bool $Complex$float$__bool__ ($Complex$float, $float);
$float $Complex$float$__mul__ ($Complex$float, $float, $float);
$float $Complex$float$__truediv__ ($Complex$float, $float, $float);
$float $Complex$float$__pow__ ($Complex$float, $float, $float);
$float $Complex$float$__neg__ ($Complex$float, $float);
$float $Complex$float$__pos__ ($Complex$float, $float);
$Real$opaque $Complex$float$real ($Complex$float, $float);
$Real$opaque $Complex$float$imag ($Complex$float, $float);
$Real$opaque $Complex$float$__abs__ ($Complex$float, $float);
$float $Complex$float$conjugate ($Complex$float, $float);

// $Plus$float ////////////////////////////////////////////////////////////

struct $Plus$float {
    $Plus$float$class $class;
    $Complex$float w$Complex$float;
};

struct $Plus$float$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Plus$float, $Complex$float);
    $float (*__add__)($Plus$float, $float, $float);
};

void $Plus$float$__init__ ($Plus$float, $Complex$float);
$float $Plus$float$__add__ ($Plus$float, $float, $float);

// $Minus$float ////////////////////////////////////////////////////////////

struct $Minus$float {
    $Minus$float$class $class;
    $Complex$float w$Complex$float;
};

struct $Minus$float$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Minus$float, $Complex$float);
    $float (*__sub__)($Minus$float, $float, $float);
};

void $Minus$float$__init__ ($Minus$float, $Complex$float);
$float $Minus$float$__sub__ ($Minus$float, $float, $float);

// $Hashable$float ////////////////////////////////////////////////////////////

struct $Hashable$float {
    $Hashable$float$class $class;
};

struct $Hashable$float$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Hashable$float);
    $bool (*__eq__)($Hashable$float, $float, $float);
    $bool (*__ne__)($Hashable$float, $float, $float);
    $int (*__hash__)($Hashable$float, $float);
};

void $Hashable$float$__init__ ($Hashable$float);
$bool $Hashable$float$__eq__ ($Hashable$float, $float, $float);
$bool $Hashable$float$__ne__ ($Hashable$float, $float, $float);
$int $Hashable$float$__hash__ ($Hashable$float, $float);

