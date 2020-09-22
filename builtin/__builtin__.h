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

struct $bytearray;
typedef struct $bytearray *$bytearray;

struct $NoneType;
typedef struct $NoneType *$NoneType;

struct $int;
typedef struct $int *$int;

struct $float;
typedef struct $float *$float;

struct $complex;
typedef struct $complex *$complex;

struct $bool;
typedef struct $bool *$bool;

struct $Serial$state;
typedef struct $Serial$state *$Serial$state;

#include "class_hierarchy.h"

struct $range;
typedef struct $range *$range;

struct $tuple;
typedef struct $tuple *$tuple;

struct $Iterator;
typedef struct $Iterator *$Iterator;

struct $Slice;
typedef struct $Slice *$Slice;

struct $BaseException;
typedef struct $BaseException *$BaseException;

struct $BaseException$class;
typedef struct $BaseException$class *$BaseException$class;

struct $SystemExit;
typedef struct $SystemExit *$SystemExit;

struct $SystemExit$class;
typedef struct $SystemExit$class *$SystemExit$class;

struct $KeyboardInterrupt;
typedef struct $KeyboardInterrupt *$KeyboardInterrupt;

struct $KeyboardInterrupt$class;
typedef struct $KeyboardInterrupt$class *$KeyboardInterrupt$class;

struct $Exception;
typedef struct $Exception *$Exception;

struct $Exception$class;
typedef struct $Exception$class *$Exception$class;

struct $AssertionError;
typedef struct $AssertionError *$AssertionError;

struct $AssertionError$class;
typedef struct $AssertionError$class *$AssertionError$class;

struct $LookupError;
typedef struct $LookupError *$LookupError;

struct $LookupError$class;
typedef struct $LookupError$class *$LookupError$class;

struct $IndexError;
typedef struct $IndexError *$IndexError;

struct $IndexError$class;
typedef struct $IndexError$class *$IndexError$class;

struct $KeyError;
typedef struct $KeyError *$KeyError;

struct $KeyError$class;
typedef struct $KeyError$class *$KeyError$class;

struct $MemoryError;
typedef struct $MemoryError *$MemoryError;

struct $MemoryError$class;
typedef struct $MemoryError$class *$MemoryError$class;

struct $OSError;
typedef struct $OSError *$OSError;

struct $OSError$class;
typedef struct $OSError$class *$OSError$class;

struct $RuntimeError;
typedef struct $RuntimeError *$RuntimeError;

struct $RuntimeError$class;
typedef struct $RuntimeError$class *$RuntimeError$class;

struct $NotImplementedError;
typedef struct $NotImplementedError *$NotImplementedError;

struct $NotImplementedError$class;
typedef struct $NotImplementedError$class *$NotImplementedError$class;

struct $ValueError;
typedef struct $ValueError *$ValueError;

struct $ValueError$class;
typedef struct $ValueError$class *$ValueError$class;

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

struct $Number;
typedef struct $Number *$Number;

struct $Number$class;
typedef struct $Number$class *$Number$class;

struct $Number$opaque;
typedef struct $Number$opaque *$Number$opaque;

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

struct $Sequence$range;
typedef struct $Sequence$range *$Sequence$range;

struct $Sequence$range$class;
typedef struct $Sequence$range$class *$Sequence$range$class;

struct $Collection$range;
typedef struct $Collection$range *$Collection$range;

struct $Collection$range$class;
typedef struct $Collection$range$class *$Collection$range$class;

struct $Plus$range;
typedef struct $Plus$range *$Plus$range;

struct $Plus$range$class;
typedef struct $Plus$range$class *$Plus$range$class;

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

struct $Minus$int;
typedef struct $Minus$int *$Minus$int;

struct $Minus$int$class;
typedef struct $Minus$int$class *$Minus$int$class;

struct $Ord$int;
typedef struct $Ord$int *$Ord$int;

struct $Ord$int$class;
typedef struct $Ord$int$class *$Ord$int$class;

struct $Hashable$int;
typedef struct $Hashable$int *$Hashable$int;

struct $Hashable$int$class;
typedef struct $Hashable$int$class *$Hashable$int$class;

struct $Real$float;
typedef struct $Real$float *$Real$float;

struct $Real$float$class;
typedef struct $Real$float$class *$Real$float$class;

struct $Minus$float;
typedef struct $Minus$float *$Minus$float;

struct $Minus$float$class;
typedef struct $Minus$float$class *$Minus$float$class;

struct $Ord$float;
typedef struct $Ord$float *$Ord$float;

struct $Ord$float$class;
typedef struct $Ord$float$class *$Ord$float$class;

struct $Hashable$float;
typedef struct $Hashable$float *$Hashable$float;

struct $Hashable$float$class;
typedef struct $Hashable$float$class *$Hashable$float$class;

struct $Number$complex;
typedef struct $Number$complex *$Number$complex;

struct $Number$complex$class;
typedef struct $Number$complex$class *$Number$complex$class;

struct $Minus$complex;
typedef struct $Minus$complex *$Minus$complex;

struct $Minus$complex$class;
typedef struct $Minus$complex$class *$Minus$complex$class;

struct $Eq$complex;
typedef struct $Eq$complex *$Eq$complex;

struct $Eq$complex$class;
typedef struct $Eq$complex$class *$Eq$complex$class;

struct $Hashable$complex;
typedef struct $Hashable$complex *$Hashable$complex;

struct $Hashable$complex$class;
typedef struct $Hashable$complex$class *$Hashable$complex$class;

struct $Iterable$range;
typedef struct $Iterable$range *$Iterable$range;

struct $Iterable$range$class;
typedef struct $Iterable$range$class *$Iterable$range$class;

struct $Iterable$tuple;
typedef struct $Iterable$tuple *$Iterable$tuple;

struct $Iterable$tuple$class;
typedef struct $Iterable$tuple$class *$Iterable$tuple$class;

struct $Sliceable$tuple;
typedef struct $Sliceable$tuple *$Sliceable$tuple;

struct $Sliceable$tuple$class;
typedef struct $Sliceable$tuple$class *$Sliceable$tuple$class;

struct $Hashable$tuple;
typedef struct $Hashable$tuple *$Hashable$tuple;

struct $Hashable$tuple$class;
typedef struct $Hashable$tuple$class *$Hashable$tuple$class;

struct $Ord$bytearray;
typedef struct $Ord$bytearray *$Ord$bytearray;

struct $Ord$bytearray$class;
typedef struct $Ord$bytearray$class *$Ord$bytearray$class;

struct $Sequence$bytearray;
typedef struct $Sequence$bytearray *$Sequence$bytearray;

struct $Sequence$bytearray$class;
typedef struct $Sequence$bytearray$class *$Sequence$bytearray$class;

struct $Collection$bytearray;
typedef struct $Collection$bytearray *$Collection$bytearray;

struct $Collection$bytearray$class;
typedef struct $Collection$bytearray$class *$Collection$bytearray$class;

struct $Plus$bytearray;
typedef struct $Plus$bytearray *$Plus$bytearray;

struct $Plus$bytearray$class;
typedef struct $Plus$bytearray$class *$Plus$bytearray$class;

struct $Container$bytearray;
typedef struct $Container$bytearray *$Container$bytearray;

struct $Container$bytearray$class;
typedef struct $Container$bytearray$class *$Container$bytearray$class;

// $Eq ////////////////////////////////////////////////////////////

struct $Eq {
    $Eq$class $class;
};

struct $Eq$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Eq);
    $bool (*__eq__)($Eq, $WORD, $WORD);
    $bool (*__ne__)($Eq, $WORD, $WORD);
};

struct $Eq$opaque {
    char *$GCINFO;
    int $class_id;
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
    int $class_id;
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
    int $class_id;
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
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Logical);
    $WORD (*__and__)($Logical, $WORD, $WORD);
    $WORD (*__or__)($Logical, $WORD, $WORD);
    $WORD (*__xor__)($Logical, $WORD, $WORD);
};

struct $Logical$opaque {
    char *$GCINFO;
    int $class_id;
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
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Plus);
    $WORD (*__add__)($Plus, $WORD, $WORD);
};

struct $Plus$opaque {
    char *$GCINFO;
    int $class_id;
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
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Minus);
    $WORD (*__sub__)($Minus, $WORD, $WORD);
};

struct $Minus$opaque {
    char *$GCINFO;
    int $class_id;
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
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Hashable);
    $bool (*__eq__)($Hashable, $WORD, $WORD);
    $bool (*__ne__)($Hashable, $WORD, $WORD);
    $int (*__hash__)($Hashable, $WORD);
};

struct $Hashable$opaque {
    char *$GCINFO;
    int $class_id;
    $Hashable proto;
    $WORD impl;
};

$Hashable$opaque $Hashable$pack($Hashable proto, $WORD impl);

// $Indexed ////////////////////////////////////////////////////////////

struct $Indexed {
    $Indexed$class $class;
    $Eq w$Eq$Indexed;
};

struct $Indexed$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Indexed, $Eq);
    $WORD (*__getitem__)($Indexed, $WORD, $WORD);
    void (*__setitem__)($Indexed, $WORD, $WORD, $WORD);
    void (*__delitem__)($Indexed, $WORD, $WORD);
};

struct $Indexed$opaque {
    char *$GCINFO;
    int $class_id;
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
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Sliceable);
    $WORD (*__getitem__)($Sliceable, $WORD, $int);
    void (*__setitem__)($Sliceable, $WORD, $int, $WORD);
    void (*__delitem__)($Sliceable, $WORD, $int);
    $WORD (*__getslice__)($Sliceable, $WORD, $Slice);
  void (*__setslice__)($Sliceable, $Iterable, $WORD, $Slice, $WORD);
    void (*__delslice__)($Sliceable, $WORD, $Slice);
};

struct $Sliceable$opaque {
    char *$GCINFO;
    int $class_id;
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
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Iterable);
    $Iterator (*__iter__)($Iterable, $WORD);
};

struct $Iterable$opaque {
    char *$GCINFO;
    int $class_id;
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
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Collection);
    $Iterator (*__iter__)($Collection, $WORD);
    $WORD (*__fromiter__)($Collection, $Iterable, $WORD);
    $int (*__len__)($Collection, $WORD);
};

struct $Collection$opaque {
    char *$GCINFO;
    int $class_id;
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
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Container, $Eq);
    $Iterator (*__iter__)($Container, $WORD);
    $WORD (*__fromiter__)($Container, $Iterable, $WORD);
    $int (*__len__)($Container, $WORD);
    $bool (*__contains__)($Container, $WORD, $WORD);
    $bool (*__containsnot__)($Container, $WORD, $WORD);
};

struct $Container$opaque {
    char *$GCINFO;
    int $class_id;
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
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Sequence);
    $WORD (*__getitem__)($Sequence, $WORD, $int);
    void (*__setitem__)($Sequence, $WORD, $int, $WORD);
    void (*__delitem__)($Sequence, $WORD, $int);
    $WORD (*__getslice__)($Sequence, $WORD, $Slice);
    void (*__setslice__)($Sequence, $Iterable, $WORD, $Slice, $WORD);
    void (*__delslice__)($Sequence, $WORD, $Slice);
    $Iterator (*__reversed__)($Sequence, $WORD);
    void (*insert)($Sequence, $WORD, $int, $WORD);
    void (*append)($Sequence, $WORD, $WORD);
    void (*reverse)($Sequence, $WORD);
};

struct $Sequence$opaque {
    char *$GCINFO;
    int $class_id;
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
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Mapping, $Eq);
    $Iterator (*__iter__)($Mapping, $WORD);
    $WORD (*__fromiter__)($Mapping, $Iterable, $WORD);
    $int (*__len__)($Mapping, $WORD);
    $bool (*__contains__)($Mapping, $WORD, $WORD);
    $bool (*__containsnot__)($Mapping, $WORD, $WORD);
    $WORD (*get)($Mapping, $WORD, $WORD, $WORD);
    $Iterator (*keys)($Mapping, $WORD);
    $Iterator (*values)($Mapping, $WORD);
    $Iterator (*items)($Mapping, $WORD);
    void (*update)($Mapping, $Iterable, $WORD, $WORD);
    $tuple (*popitem)($Mapping, $WORD);
    void (*setdefault)($Mapping, $WORD, $WORD, $WORD);
};

struct $Mapping$opaque {
    char *$GCINFO;
    int $class_id;
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
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Set, $Eq);
    $Iterator (*__iter__)($Set, $WORD);
    $WORD (*__fromiter__)($Set, $Iterable, $WORD);
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
    int $class_id;
    $Set proto;
    $WORD impl;
};

$Set$opaque $Set$pack($Set proto, $WORD impl);


// $Number ////////////////////////////////////////////////////////////

struct $Number {
    $Number$class $class;
    $Minus w$Minus$Number;
};

struct $Number$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Number);
    $WORD (*__add__)($Plus, $WORD, $WORD);
    $WORD (*__fromatom__)($Number,$WORD);
    $complex (*__complx__)($Number, $WORD);
    $WORD (*__mul__)($Number, $WORD, $WORD);
    $WORD (*__truediv__)($Number, $WORD, $WORD);
    $WORD (*__pow__)($Number, $WORD, $WORD);
    $WORD (*__neg__)($Number, $WORD);
    $WORD (*__pos__)($Number, $WORD);
    $WORD (*real)($Number, $Real, $WORD);
    $WORD (*imag)($Number, $Real, $WORD);
    $WORD (*__abs__)($Number, $Real, $WORD);
    $WORD (*conjugate)($Number, $WORD);
};

struct $Number$opaque {
    char *$GCINFO;
    int $class_id;
    $Number proto;
    $WORD impl;
};

$Number$opaque $Number$pack($Number proto, $WORD impl);


// $Real ////////////////////////////////////////////////////////////

struct $Real {
    $Real$class $class;
};

struct $Real$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Real);
    $WORD (*__add__)($Plus, $WORD, $WORD);
    $WORD (*__fromatom__)($Real,$WORD);
    $complex (*__complx__)($Real, $WORD);
    $WORD (*__mul__)($Real, $WORD, $WORD);
    $WORD (*__truediv__)($Real, $WORD, $WORD);
    $WORD (*__pow__)($Real, $WORD, $WORD);
    $WORD (*__neg__)($Real, $WORD);
    $WORD (*__pos__)($Real, $WORD);
    $WORD (*real)($Real, $Real, $WORD);
    $WORD (*imag)($Real, $Real, $WORD);
    $WORD (*__abs__)($Real, $Real, $WORD);
    $WORD (*conjugate)($Real, $WORD);
    $float (*__float__)($Real, $WORD);
    $WORD (*__trunc__)($Real, $Integral, $WORD);
    $WORD (*__floor__)($Real, $Integral, $WORD);
    $WORD (*__ceil__)($Real, $Integral, $WORD);
    $WORD (*__round__)($Real, $WORD, $int);
};

struct $Real$opaque {
    char *$GCINFO;
    int $class_id;
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
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Rational);
    $WORD (*__add__)($Rational, $WORD, $WORD);
    $WORD (*__fromatom__)($Rational,$WORD);
    $complex (*__complx__)($Rational, $WORD);
    $WORD (*__mul__)($Rational, $WORD, $WORD);
    $WORD (*__truediv__)($Rational, $WORD, $WORD);
    $WORD (*__pow__)($Rational, $WORD, $WORD);
    $WORD (*__neg__)($Rational, $WORD);
    $WORD (*__pos__)($Rational, $WORD);
    $WORD (*real)($Rational, $Real, $WORD);
    $WORD (*imag)($Rational, $Real, $WORD);
    $WORD (*__abs__)($Rational, $Real, $WORD);
    $WORD (*conjugate)($Rational, $WORD);
    $float (*__float__)($Rational, $WORD);
    $WORD (*__trunc__)($Rational, $Integral, $WORD);
    $WORD (*__floor__)($Rational, $Integral, $WORD);
    $WORD (*__ceil__)($Rational, $Integral, $WORD);
    $WORD (*__round__)($Rational, $WORD, $int);
    $WORD (*numerator)($Rational, $Integral, $WORD);
    $WORD (*denominator)($Rational, $Integral, $WORD);
};

struct $Rational$opaque {
    char *$GCINFO;
    int $class_id;
    $Rational proto;
    $WORD impl;
};

$Rational$opaque $Rational$pack($Rational proto, $WORD impl);


// $Integral ////////////////////////////////////////////////////////////

struct $Integral {
    $Integral$class $class;
    $Logical w$Logical$Integral;
    $Minus w$Minus$Integral;
};

struct $Integral$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Integral);
    $WORD (*__add__)($Integral, $WORD, $WORD);
    $WORD (*__fromatom__)($Integral,$WORD);
    $complex (*__complx__)($Integral, $WORD);
    $WORD (*__mul__)($Integral, $WORD, $WORD);
    $WORD (*__truediv__)($Integral, $WORD, $WORD);
    $WORD (*__pow__)($Integral, $WORD, $WORD);
    $WORD (*__neg__)($Integral, $WORD);
    $WORD (*__pos__)($Integral, $WORD);
    $WORD (*real)($Integral, $Real, $WORD);
    $WORD (*imag)($Integral, $Real, $WORD);
    $WORD (*__abs__)($Integral, $Real, $WORD);
    $WORD (*conjugate)($Integral, $WORD);
    $float (*__float__)($Integral, $WORD);
    $WORD (*__trunc__)($Integral, $Integral, $WORD);
    $WORD (*__floor__)($Integral, $Integral, $WORD);
    $WORD (*__ceil__)($Integral, $Integral, $WORD);
    $WORD (*__round__)($Integral, $WORD, $int);
    $WORD (*numerator)($Integral, $Integral, $WORD);
    $WORD (*denominator)($Integral, $Integral, $WORD);
    $int (*__int__)($Integral, $WORD);
    $int (*__index__)($Integral, $WORD);
    $tuple (*__divmod__)($Integral, $WORD, $WORD);
    $WORD (*__floordiv__)($Integral, $WORD, $WORD);
    $WORD (*__mod__)($Integral, $WORD, $WORD);
    $WORD (*__lshift__)($Integral, $WORD, $int);
    $WORD (*__rshift__)($Integral, $WORD, $int);
    $WORD (*__invert__)($Integral, $WORD);
};

struct $Integral$opaque {
    char *$GCINFO;
    int $class_id;
    $Integral proto;
    $WORD impl;
};

$Integral$opaque $Integral$pack($Integral proto, $WORD impl);

// $Sequence$list ////////////////////////////////////////////////////////////

struct $Sequence$list {
    $Sequence$list$class $class;
    $Collection$list w$Collection$Sequence;
    $Plus$list w$Plus$Sequence;
};

struct $Sequence$list$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Sequence$list);
    $WORD (*__getitem__)($Sequence$list, $list, $int);
    void (*__setitem__)($Sequence$list, $list, $int, $WORD);
    void (*__delitem__)($Sequence$list, $list, $int);
    $list (*__getslice__)($Sequence$list, $list, $Slice);
    void (*__setslice__)($Sequence$list, $Iterable, $list, $Slice, $WORD);
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
void $Sequence$list$__setslice__ ($Sequence$list, $Iterable, $list, $Slice, $WORD);
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
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Collection$list, $Sequence$list);
    $Iterator (*__iter__)($Collection$list, $list);
    $list (*__fromiter__)($Collection$list, $Iterable, $WORD);
    $int (*__len__)($Collection$list, $list);
};

void $Collection$list$__init__ ($Collection$list, $Sequence$list);
$Iterator $Collection$list$__iter__ ($Collection$list, $list);
$list $Collection$list$__fromiter__ ($Collection$list, $Iterable, $WORD);
$int $Collection$list$__len__ ($Collection$list, $list);

// $Plus$list ////////////////////////////////////////////////////////////

struct $Plus$list {
    $Plus$list$class $class;
    $Sequence$list w$Sequence$list;
};

struct $Plus$list$class {
    char *$GCINFO;
    int $class_id;
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
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Container$list, $Eq);
    $Iterator (*__iter__)($Container$list, $list);
    $list (*__fromiter__)($Container$list, $Iterable, $WORD);
    $int (*__len__)($Container$list, $list);
    $bool (*__contains__)($Container$list, $list, $WORD);
    $bool (*__containsnot__)($Container$list, $list, $WORD);
};

void $Container$list$__init__ ($Container$list, $Eq);
$Iterator $Container$list$__iter__ ($Container$list, $list);
$list $Container$list$__fromiter__ ($Container$list, $Iterable, $WORD);
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
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Mapping$dict, $Hashable);
    $Iterator (*__iter__)($Mapping$dict, $dict);
    $dict (*__fromiter__)($Mapping$dict, $Iterable, $WORD);
    $int (*__len__)($Mapping$dict, $dict);
    $bool (*__contains__)($Mapping$dict, $dict, $WORD);
    $bool (*__containsnot__)($Mapping$dict, $dict, $WORD);
    $WORD (*get)($Mapping$dict, $dict, $WORD, $WORD);
    $Iterator (*keys)($Mapping$dict, $dict);
    $Iterator (*values)($Mapping$dict, $dict);
    $Iterator (*items)($Mapping$dict, $dict);
    void (*update)($Mapping$dict, $Iterable, $dict, $WORD);
    $tuple (*popitem)($Mapping$dict, $dict);
    void (*setdefault)($Mapping$dict, $dict, $WORD, $WORD);
};

void $Mapping$dict$__init__ ($Mapping$dict, $Hashable);
$Iterator $Mapping$dict$__iter__ ($Mapping$dict, $dict);
$dict $Mapping$dict$__fromiter__ ($Mapping$dict, $Iterable, $WORD);
$int $Mapping$dict$__len__ ($Mapping$dict, $dict);
$bool $Mapping$dict$__contains__ ($Mapping$dict, $dict, $WORD);
$bool $Mapping$dict$__containsnot__ ($Mapping$dict, $dict, $WORD);
$WORD $Mapping$dict$get ($Mapping$dict, $dict, $WORD, $WORD);
$Iterator $Mapping$dict$keys ($Mapping$dict, $dict);
$Iterator $Mapping$dict$values ($Mapping$dict, $dict);
$Iterator $Mapping$dict$items ($Mapping$dict, $dict);
void $Mapping$dict$update ($Mapping$dict, $Iterable, $dict, $WORD);
$tuple $Mapping$dict$popitem ($Mapping$dict, $dict);
void $Mapping$dict$setdefault ($Mapping$dict, $dict, $WORD, $WORD);

// $Indexed$dict ////////////////////////////////////////////////////////////

struct $Indexed$dict {
    $Indexed$dict$class $class;
    $Mapping$dict w$Mapping$dict;
    $Eq w$Eq$A;
};

struct $Indexed$dict$class {
    char *$GCINFO;
    int $class_id;
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
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Set$set, $Hashable);
    $Iterator (*__iter__)($Set$set, $set);
    $set (*__fromiter__)($Set$set, $Iterable, $WORD);
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
$set $Set$set$__fromiter__ ($Set$set, $Iterable, $WORD);
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
    int $class_id;
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
    int $class_id;
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
    int $class_id;
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
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Iterable$Iterator);
    $Iterator (*__iter__)($Iterable$Iterator, $Iterator);
};

void $Iterable$Iterator$__init__ ($Iterable$Iterator);
$Iterator $Iterable$Iterator$__iter__ ($Iterable$Iterator, $Iterator);

// $Sequence$range ////////////////////////////////////////////////////////////

struct $Sequence$range {
    $Sequence$range$class $class;
    $Collection$range w$Collection$Sequence;
    $Plus$range w$Plus$Sequence;
};

struct $Sequence$range$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Sequence$range);
    $int (*__getitem__)($Sequence$range, $range, $int);
    void (*__setitem__)($Sequence$range, $range, $int, $int);
    void (*__delitem__)($Sequence$range, $range, $int);
    $range (*__getslice__)($Sequence$range, $range, $Slice);
    void (*__setslice__)($Sequence$range, $Iterable, $range, $Slice, $WORD);
    void (*__delslice__)($Sequence$range, $range, $Slice);
    $Iterator (*__reversed__)($Sequence$range, $range);
    void (*insert)($Sequence$range, $range, $int, $int);
    void (*append)($Sequence$range, $range, $int);
    void (*reverse)($Sequence$range, $range);
};

void $Sequence$range$__init__ ($Sequence$range);
$int $Sequence$range$__getitem__ ($Sequence$range, $range, $int);
void $Sequence$range$__setitem__ ($Sequence$range, $range, $int, $int);
void $Sequence$range$__delitem__ ($Sequence$range, $range, $int);
$range $Sequence$range$__getslice__ ($Sequence$range, $range, $Slice);
void $Sequence$range$__setslice__ ($Sequence$range, $Iterable, $range, $Slice, $WORD);
void $Sequence$range$__delslice__ ($Sequence$range, $range, $Slice);
$Iterator $Sequence$range$__reversed__ ($Sequence$range, $range);
void $Sequence$range$insert ($Sequence$range, $range, $int, $int);
void $Sequence$range$append ($Sequence$range, $range, $int);
void $Sequence$range$reverse ($Sequence$range, $range);

// $Collection$range ////////////////////////////////////////////////////////////

struct $Collection$range {
    $Collection$range$class $class;
    $Sequence$range w$Sequence$range;
};

struct $Collection$range$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Collection$range, $Sequence$range);
    $Iterator (*__iter__)($Collection$range, $range);
    $range (*__fromiter__)($Collection$range, $Iterable, $WORD);
    $int (*__len__)($Collection$range, $range);
};

void $Collection$range$__init__ ($Collection$range, $Sequence$range);
$Iterator $Collection$range$__iter__ ($Collection$range, $range);
$range $Collection$range$__fromiter__ ($Collection$range, $Iterable, $WORD);
$int $Collection$range$__len__ ($Collection$range, $range);

// $Plus$range ////////////////////////////////////////////////////////////

struct $Plus$range {
    $Plus$range$class $class;
    $Sequence$range w$Sequence$range;
};

struct $Plus$range$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Plus$range, $Sequence$range);
    $range (*__add__)($Plus$range, $range, $range);
};

void $Plus$range$__init__ ($Plus$range, $Sequence$range);
$range $Plus$range$__add__ ($Plus$range, $range, $range);

// $Ord$str ////////////////////////////////////////////////////////////

struct $Ord$str {
    $Ord$str$class $class;
};

struct $Ord$str$class {
    char *$GCINFO;
    int $class_id;
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
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Container$str, $Eq);
    $Iterator (*__iter__)($Container$str, $str);
    $int (*__len__)($Container$str, $str);
    $bool (*__contains__)($Container$str, $str, $str);
    $bool (*__containsnot__)($Container$str, $str, $str);
};

void $Container$str$__init__ ($Container$str, $Eq);
$Iterator $Container$str$__iter__ ($Container$str, $str);
$int $Container$str$__len__ ($Container$str, $str);
$bool $Container$str$__contains__ ($Container$str, $str, $str);
$bool $Container$str$__containsnot__ ($Container$str, $str, $str);

// $Sliceable$str ////////////////////////////////////////////////////////////

struct $Sliceable$str {
    $Sliceable$str$class $class;
};

struct $Sliceable$str$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Sliceable$str);
    $str (*__getitem__)($Sliceable$str, $str, $int);
    void (*__setitem__)($Sliceable$str, $str, $int, $str);
    void (*__delitem__)($Sliceable$str, $str, $int);
    $str (*__getslice__)($Sliceable$str, $str, $Slice);
    void (*__setslice__)($Sliceable$str, $Iterable, $str, $Slice, $WORD);
    void (*__delslice__)($Sliceable$str, $str, $Slice);
};

void $Sliceable$str$__init__ ($Sliceable$str);
$str $Sliceable$str$__getitem__ ($Sliceable$str, $str, $int);
void $Sliceable$str$__setitem__ ($Sliceable$str, $str, $int, $str);
void $Sliceable$str$__delitem__ ($Sliceable$str, $str, $int);
$str $Sliceable$str$__getslice__ ($Sliceable$str, $str, $Slice);
void $Sliceable$str$__setslice__ ($Sliceable$str, $Iterable, $str, $Slice, $WORD);
void $Sliceable$str$__delslice__ ($Sliceable$str, $str, $Slice);

// $Plus$str ////////////////////////////////////////////////////////////

struct $Plus$str {
    $Plus$str$class $class;
};

struct $Plus$str$class {
    char *$GCINFO;
    int $class_id;
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
    int $class_id;
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
    $Minus$int w$Minus$Integral;
};

struct $Integral$int$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Integral$int);
    $int (*__add__)($Integral$int, $int, $int);
    $int (*__fromatom__)($Integral$int,$WORD);
    $complex (*__complx__)($Integral$int, $int);
    $int (*__mul__)($Integral$int, $int, $int);
    $int (*__truediv__)($Integral$int, $int, $int);
    $int (*__pow__)($Integral$int, $int, $int);
    $int (*__neg__)($Integral$int, $int);
    $int (*__pos__)($Integral$int, $int);
    $WORD (*real)($Integral$int, $Real, $int);
    $WORD (*imag)($Integral$int, $Real, $int);
    $WORD (*__abs__)($Integral$int, $Real, $int);
    $int (*conjugate)($Integral$int, $int);
    $float (*__float__)($Integral$int, $int);
    $WORD (*__trunc__)($Integral$int, $Integral, $int);
    $WORD (*__floor__)($Integral$int, $Integral, $int);
    $WORD (*__ceil__)($Integral$int, $Integral, $int);
    $int (*__round__)($Integral$int, $int, $int);
    $WORD (*numerator)($Integral$int, $Integral, $int);
    $WORD (*denominator)($Integral$int, $Integral, $int);
    $int (*__int__)($Integral$int, $int);
    $int (*__index__)($Integral$int, $int);
    $tuple (*__divmod__)($Integral$int, $int, $int);
    $int (*__floordiv__)($Integral$int, $int, $int);
    $int (*__mod__)($Integral$int, $int, $int);
    $int (*__lshift__)($Integral$int, $int, $int);
    $int (*__rshift__)($Integral$int, $int, $int);
    $int (*__invert__)($Integral$int, $int);
};

void $Integral$int$__init__ ($Integral$int);
$int $Integral$int$__add__($Integral$int, $int, $int);
$int $Integral$int$__fromatom__($Integral$int,$WORD);
$complex $Integral$int$__complx__($Integral$int, $int);
$int $Integral$int$__mul__($Integral$int, $int, $int);
$int $Integral$int$__truediv__($Integral$int, $int, $int);
$int $Integral$int$__pow__($Integral$int, $int, $int);
$int $Integral$int$__neg__($Integral$int, $int);
$int $Integral$int$__pos__($Integral$int, $int);
$WORD $Integral$int$real($Integral$int, $Real, $int);
$WORD $Integral$int$imag($Integral$int, $Real, $int);
$WORD $Integral$int$__abs__($Integral$int, $Real, $int);
$int $Integral$int$conjugate($Integral$int, $int);
$float $Integral$int$__float__ ($Integral$int, $int);
$WORD $Integral$int$__trunc__ ($Integral$int, $Integral, $int);
$WORD $Integral$int$__floor__ ($Integral$int, $Integral, $int);
$WORD $Integral$int$__ceil__ ($Integral$int, $Integral, $int);
$int $Integral$int$__round__ ($Integral$int, $int, $int);
$WORD $Integral$int$numerator ($Integral$int, $Integral, $int);
$WORD $Integral$int$denominator ($Integral$int, $Integral, $int);
$int $Integral$int$__int__ ($Integral$int, $int);
$int $Integral$int$__index__ ($Integral$int, $int);
$tuple $Integral$int$__divmod__ ($Integral$int, $int, $int);
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
    int $class_id;
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

// $Minus$int ////////////////////////////////////////////////////////////

struct $Minus$int {
    $Minus$int$class $class;
    $Integral$int w$Integral$int;
};

struct $Minus$int$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Minus$int, $Integral$int);
    $int (*__sub__)($Minus$int, $int, $int);
};

void $Minus$int$__init__ ($Minus$int, $Integral$int);
$int $Minus$int$__sub__ ($Minus$int, $int, $int);

// $Ord$int /////////////////////////////////////////////////////////////////

struct $Ord$int {
    $Ord$int$class $class;
};

struct $Ord$int$class {  
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Ord$int);
    $bool (*__eq__)($Ord$int, $int, $int);
    $bool (*__ne__)($Ord$int, $int, $int);
    $bool (*__lt__)($Ord$int, $int, $int);
    $bool (*__le__)($Ord$int, $int, $int);
    $bool (*__gt__)($Ord$int, $int, $int);
    $bool (*__ge__)($Ord$int, $int, $int);
};

void $Ord$int$__init__ ($Ord$int);
$bool $Ord$int$__eq__ ($Ord$int, $int, $int);
$bool $Ord$int$__ne__ ($Ord$int, $int, $int);
$bool $Ord$int$__lt__ ($Ord$int, $int, $int);
$bool $Ord$int$__le__ ($Ord$int, $int, $int);
$bool $Ord$int$__gt__ ($Ord$int, $int, $int);
$bool $Ord$int$__ge__ ($Ord$int, $int, $int);
  

// $Hashable$int ////////////////////////////////////////////////////////////

struct $Hashable$int {
    $Hashable$int$class $class;
};

struct $Hashable$int$class {
    char *$GCINFO;
    int $class_id;
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
    $Minus$float w$Minus$Real;
};

struct $Real$float$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Real$float);
    $float (*__add__)($Real$float, $float, $float);
    $float (*__fromatom__)($Real$float,$WORD);
    $complex (*__complx__)($Real$float, $float);
    $float (*__mul__)($Real$float, $float, $float);
    $float (*__truediv__)($Real$float, $float, $float);
    $float (*__pow__)($Real$float, $float, $float);
    $float (*__neg__)($Real$float, $float);
    $float (*__pos__)($Real$float, $float);
    $WORD (*real)($Real$float, $Real, $float);
    $WORD (*imag)($Real$float, $Real, $float);
    $WORD (*__abs__)($Real$float, $Real, $float);
    $float (*conjugate)($Real$float, $float);
    $float (*__float__)($Real$float, $float);
    $WORD (*__trunc__)($Real$float, $Integral, $float);
    $WORD (*__floor__)($Real$float, $Integral, $float);
    $WORD (*__ceil__)($Real$float, $Integral, $float);
    $float (*__round__)($Real$float, $float, $int);
};

void $Real$float$__init__ ($Real$float);
$float $Real$float$__add__($Real$float, $float, $float);
$float $Real$float$__fromatom__($Real$float,$WORD);
$complex $Real$float$__complx__($Real$float, $float);
$float $Real$float$__mul__($Real$float, $float, $float);
$float $Real$float$__truediv__($Real$float, $float, $float);
$float $Real$float$__pow__($Real$float, $float, $float);
$float $Real$float$__neg__($Real$float, $float);
$float $Real$float$__pos__($Real$float, $float);
$WORD $Real$float$real($Real$float, $Real, $float);
$WORD $Real$float$imag($Real$float, $Real, $float);
$WORD $Real$float$__abs__($Real$float, $Real, $float);
$float $Real$float$conjugate($Real$float, $float);
$float $Real$float$__float__ ($Real$float, $float);
$WORD $Real$float$__trunc__ ($Real$float, $Integral, $float);
$WORD $Real$float$__floor__ ($Real$float, $Integral, $float);
$WORD $Real$float$__ceil__ ($Real$float, $Integral, $float);
$float $Real$float$__round__ ($Real$float, $float, $int);

// $Minus$float ////////////////////////////////////////////////////////////

struct $Minus$float {
    $Minus$float$class $class;
    $Real$float w$Real$float;
};

struct $Minus$float$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Minus$float, $Real$float);
    $float (*__sub__)($Minus$float, $float, $float);
};

void $Minus$float$__init__ ($Minus$float, $Real$float);
$float $Minus$float$__sub__ ($Minus$float, $float, $float);

// $Ord$float /////////////////////////////////////////////////////////////////

struct $Ord$float {
    $Ord$float$class $class;
};

struct $Ord$float$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Ord$float);
    $bool (*__eq__)($Ord$float, $float, $float);
    $bool (*__ne__)($Ord$float, $float, $float);
    $bool (*__lt__)($Ord$float, $float, $float);
    $bool (*__le__)($Ord$float, $float, $float);
    $bool (*__gt__)($Ord$float, $float, $float);
    $bool (*__ge__)($Ord$float, $float, $float);
};

void $Ord$float$__init__ ($Ord$float);
$bool $Ord$float$__eq__ ($Ord$float, $float, $float);
$bool $Ord$float$__ne__ ($Ord$float, $float, $float);
$bool $Ord$float$__lt__ ($Ord$float, $float, $float);
$bool $Ord$float$__le__ ($Ord$float, $float, $float);
$bool $Ord$float$__gt__ ($Ord$float, $float, $float);
$bool $Ord$float$__ge__ ($Ord$float, $float, $float);

// $Hashable$float ////////////////////////////////////////////////////////////

struct $Hashable$float {
    $Hashable$float$class $class;
};

struct $Hashable$float$class {
    char *$GCINFO;
    int $class_id;
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

// $Number$complex ////////////////////////////////////////////////////////////

struct $Number$complex {
    $Number$complex$class $class;
    $Minus$complex w$Minus$Number;
};

struct $Number$complex$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Number$complex);
    $complex (*__add__)($Number$complex, $complex, $complex);
    $complex (*__fromatom__)($Number$complex,$WORD);
    $complex (*__complx__)($Number$complex, $complex);
    $complex (*__mul__)($Number$complex, $complex, $complex);
    $complex (*__truediv__)($Number$complex, $complex, $complex);
    $complex (*__pow__)($Number$complex, $complex, $complex);
    $complex (*__neg__)($Number$complex, $complex);
    $complex (*__pos__)($Number$complex, $complex);
    $WORD (*real)($Number$complex, $Real, $complex);
    $WORD (*imag)($Number$complex, $Real, $complex);
    $WORD (*__abs__)($Number$complex, $Real, $complex);
    $complex (*conjugate)($Number$complex, $complex);
};

void $Number$complex$__init__ ($Number$complex);
$complex $Number$complex$__add__ ($Number$complex, $complex, $complex);
$complex $Number$complex$__fromatom__($Number$complex,$WORD);
$complex $Number$complex$__complx__ ($Number$complex, $complex);
$complex $Number$complex$__mul__ ($Number$complex, $complex, $complex);
$complex $Number$complex$__truediv__ ($Number$complex, $complex, $complex);
$complex $Number$complex$__pow__ ($Number$complex, $complex, $complex);
$complex $Number$complex$__neg__ ($Number$complex, $complex);
$complex $Number$complex$__pos__ ($Number$complex, $complex);
$WORD $Number$complex$real ($Number$complex, $Real, $complex);
$WORD $Number$complex$imag ($Number$complex, $Real, $complex);
$WORD $Number$complex$__abs__ ($Number$complex, $Real, $complex);
$complex $Number$complex$conjugate ($Number$complex, $complex);

// $Minus$complex ////////////////////////////////////////////////////////////

struct $Minus$complex {
    $Minus$complex$class $class;
    $Number$complex w$Number$complex;
};

struct $Minus$complex$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Minus$complex, $Number$complex);
    $complex (*__sub__)($Minus$complex, $complex, $complex);
};

void $Minus$complex$__init__ ($Minus$complex, $Number$complex);
$complex $Minus$complex$__sub__ ($Minus$complex, $complex, $complex);

// $Eq$complex ////////////////////////////////////////////////////////////

struct $Eq$complex {
    $Eq$complex$class $class;
};

struct $Eq$complex$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Eq$complex);
    $bool (*__eq__)($Eq$complex, $complex, $complex);
    $bool (*__ne__)($Eq$complex, $complex, $complex);
};

$bool $Eq$complex$__eq__ ($Eq$complex, $complex, $complex);
$bool $Eq$complex$__ne__ ($Eq$complex, $complex, $complex);

// $Hashable$complex ////////////////////////////////////////////////////////////

struct $Hashable$complex {
    $Hashable$complex$class $class;
};

struct $Hashable$complex$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Hashable$complex);
    $bool (*__eq__)($Hashable$complex, $complex, $complex);
    $bool (*__ne__)($Hashable$complex, $complex, $complex);
    $int (*__hash__)($Hashable$complex, $complex);
};

void $Hashable$complex$__init__ ($Hashable$complex);
$bool $Hashable$complex$__eq__ ($Hashable$complex, $complex, $complex);
$bool $Hashable$complex$__ne__ ($Hashable$complex, $complex, $complex);
$int $Hashable$complex$__hash__ ($Hashable$complex, $complex);

// $Iterable$range ////////////////////////////////////////////////////////////

struct $Iterable$range {
    $Iterable$range$class $class;
};

struct $Iterable$range$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Iterable$range);
    $Iterator (*__iter__)($Iterable$range, $range);
};

void $Iterable$range$__init__ ($Iterable$range);
$Iterator $Iterable$range$__iter__ ($Iterable$range, $range);

// $Iterable$tuple ////////////////////////////////////////////////////////////

struct $Iterable$tuple {
    $Iterable$tuple$class $class;
};

struct $Iterable$tuple$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Iterable$tuple);
    $Iterator (*__iter__)($Iterable$tuple, $tuple);
};

void $Iterable$tuple$__init__ ($Iterable$tuple);
$Iterator $Iterable$tuple$__iter__ ($Iterable$tuple, $tuple);

// $Sliceable$tuple ////////////////////////////////////////////////////////////

// all methods except getitem and getslice will raise NotImplementedError

struct $Sliceable$tuple {
    $Sliceable$tuple$class $class;
};

struct $Sliceable$tuple$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Sliceable$tuple);
    $WORD (*__getitem__)($Sliceable$tuple , $tuple, $int);
    void (*__setitem__)($Sliceable$tuple, $tuple, $int, $WORD);
    void (*__delitem__)($Sliceable$tuple, $tuple, $int);
    $tuple (*__getslice__)($Sliceable$tuple, $tuple, $Slice);
    void (*__setslice__)($Sliceable$tuple, $Iterable, $tuple, $Slice, $WORD);
    void (*__delslice__)($Sliceable$tuple, $tuple, $Slice);
};

void $Sliceable$tuple$__init__ ($Sliceable$tuple);
$WORD $Sliceable$tuple$__getitem__ ($Sliceable$tuple, $tuple, $int);
void $Sliceable$tuple$__setitem__ ($Sliceable$tuple, $tuple, $int, $WORD);
void $Sliceable$tuple$__delitem__ ($Sliceable$tuple, $tuple, $int);
$tuple $Sliceable$tuple$__getslice__ ($Sliceable$tuple, $tuple, $Slice);
void $Sliceable$tuple$__setslice__ ($Sliceable$tuple, $Iterable, $tuple, $Slice, $WORD);
void $Sliceable$tuple$__delslice__ ($Sliceable$tuple, $tuple, $Slice);

// $Hashable$tuple ////////////////////////////////////////////////////////////

struct $Hashable$tuple {
  $Hashable$tuple$class $class;
  int w$Hashable$tuple$size;
  $Hashable *w$Hashable$tuple;
};

struct $Hashable$tuple$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Hashable$tuple,int,$Hashable*);
    $bool (*__eq__)($Hashable$tuple, $tuple, $tuple);
    $bool (*__ne__)($Hashable$tuple, $tuple, $tuple);
    $int (*__hash__)($Hashable$tuple, $tuple);
};
  
void $Hashable$tuple$__init__ ($Hashable$tuple,int,$Hashable*);
$bool $Hashable$tuple$__eq__ ($Hashable$tuple, $tuple, $tuple);
$bool $Hashable$tuple$__ne__ ($Hashable$tuple, $tuple, $tuple);
$int $Hashable$tuple$__hash__ ($Hashable$tuple, $tuple);

// $Ord$bytearray ////////////////////////////////////////////////////////////

struct $Ord$bytearray {
    $Ord$bytearray$class $class;
};

struct $Ord$bytearray$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Ord$bytearray);
    $bool (*__eq__)($Ord$bytearray, $bytearray, $bytearray);
    $bool (*__ne__)($Ord$bytearray, $bytearray, $bytearray);
    $bool (*__lt__)($Ord$bytearray, $bytearray, $bytearray);
    $bool (*__le__)($Ord$bytearray, $bytearray, $bytearray);
    $bool (*__gt__)($Ord$bytearray, $bytearray, $bytearray);
    $bool (*__ge__)($Ord$bytearray, $bytearray, $bytearray);
};

void $Ord$bytearray$__init__ ($Ord$bytearray);
$bool $Ord$bytearray$__eq__ ($Ord$bytearray, $bytearray, $bytearray);
$bool $Ord$bytearray$__ne__ ($Ord$bytearray, $bytearray, $bytearray);
$bool $Ord$bytearray$__lt__ ($Ord$bytearray, $bytearray, $bytearray);
$bool $Ord$bytearray$__le__ ($Ord$bytearray, $bytearray, $bytearray);
$bool $Ord$bytearray$__gt__ ($Ord$bytearray, $bytearray, $bytearray);
$bool $Ord$bytearray$__ge__ ($Ord$bytearray, $bytearray, $bytearray);

// $Sequence$bytearray ////////////////////////////////////////////////////////////

struct $Sequence$bytearray {
    $Sequence$bytearray$class $class;
    $Collection$bytearray w$Collection$Sequence;
    $Plus$bytearray w$Plus$Sequence;
};

struct $Sequence$bytearray$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Sequence$bytearray);
    $int (*__getitem__)($Sequence$bytearray, $bytearray, $int);
    void (*__setitem__)($Sequence$bytearray, $bytearray, $int, $int);
    void (*__delitem__)($Sequence$bytearray, $bytearray, $int);
    $bytearray (*__getslice__)($Sequence$bytearray, $bytearray, $Slice);
    void (*__setslice__)($Sequence$bytearray, $Iterable, $bytearray, $Slice, $WORD);
    void (*__delslice__)($Sequence$bytearray, $bytearray, $Slice);
    $Iterator (*__reversed__)($Sequence$bytearray, $bytearray);
    void (*insert)($Sequence$bytearray, $bytearray, $int, $int);
    void (*append)($Sequence$bytearray, $bytearray, $int);
    void (*reverse)($Sequence$bytearray, $bytearray);
};

void $Sequence$bytearray$__init__ ($Sequence$bytearray);
$int $Sequence$bytearray$__getitem__ ($Sequence$bytearray, $bytearray, $int);
void $Sequence$bytearray$__setitem__ ($Sequence$bytearray, $bytearray, $int, $int);
void $Sequence$bytearray$__delitem__ ($Sequence$bytearray, $bytearray, $int);
$bytearray $Sequence$bytearray$__getslice__ ($Sequence$bytearray, $bytearray, $Slice);
void $Sequence$bytearray$__setslice__ ($Sequence$bytearray, $Iterable, $bytearray, $Slice, $WORD);
void $Sequence$bytearray$__delslice__ ($Sequence$bytearray, $bytearray, $Slice);
$Iterator $Sequence$bytearray$__reversed__ ($Sequence$bytearray, $bytearray);
void $Sequence$bytearray$insert ($Sequence$bytearray, $bytearray, $int, $int);
void $Sequence$bytearray$append ($Sequence$bytearray, $bytearray, $int);
void $Sequence$bytearray$reverse ($Sequence$bytearray, $bytearray);

// $Collection$bytearray ////////////////////////////////////////////////////////////

struct $Collection$bytearray {
    $Collection$bytearray$class $class;
    $Sequence$bytearray w$Sequence$bytearray;
};

struct $Collection$bytearray$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Collection$bytearray, $Sequence$bytearray);
    $Iterator (*__iter__)($Collection$bytearray, $bytearray);
    $bytearray (*__fromiter__)($Collection$bytearray, $Iterable, $WORD);
    $int (*__len__)($Collection$bytearray, $bytearray);
};

void $Collection$bytearray$__init__ ($Collection$bytearray, $Sequence$bytearray);
$Iterator $Collection$bytearray$__iter__ ($Collection$bytearray, $bytearray);
$bytearray $Collection$bytearray$__fromiter__ ($Collection$bytearray, $Iterable, $WORD);
$int $Collection$bytearray$__len__ ($Collection$bytearray, $bytearray);

// $Plus$bytearray ////////////////////////////////////////////////////////////

struct $Plus$bytearray {
    $Plus$bytearray$class $class;
    $Sequence$bytearray w$Sequence$bytearray;
};

struct $Plus$bytearray$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Plus$bytearray, $Sequence$bytearray);
    $bytearray (*__add__)($Plus$bytearray, $bytearray, $bytearray);
};

void $Plus$bytearray$__init__ ($Plus$bytearray, $Sequence$bytearray);
$bytearray $Plus$bytearray$__add__ ($Plus$bytearray, $bytearray, $bytearray);

// $Container$bytearray ////////////////////////////////////////////////////////////

struct $Container$bytearray {
    $Container$bytearray$class $class;
    $Eq w$Eq$A;
};

struct $Container$bytearray$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Container$bytearray, $Eq);
    $Iterator (*__iter__)($Container$bytearray, $bytearray);
    $int (*__len__)($Container$bytearray, $bytearray);
    $bool (*__contains__)($Container$bytearray, $bytearray, $int);
    $bool (*__containsnot__)($Container$bytearray, $bytearray, $int);
};

void $Container$bytearray$__init__ ($Container$bytearray, $Eq);
$Iterator $Container$bytearray$__iter__ ($Container$bytearray, $bytearray);
$int $Container$bytearray$__len__ ($Container$bytearray, $bytearray);
$bool $Container$bytearray$__contains__ ($Container$bytearray, $bytearray, $int);
$bool $Container$bytearray$__containsnot__ ($Container$bytearray, $bytearray, $int);

