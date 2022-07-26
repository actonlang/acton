#pragma once

#include "common.h"

struct $atom;
typedef struct $atom *$atom;

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

struct $bytes;
typedef struct $bytes *$bytes;

struct $NoneType;
typedef struct $NoneType *$NoneType;

struct $int;
typedef struct $int *$int;

struct $i64;
typedef struct $i64 *$i64;

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

struct $slice;
typedef struct $slice *$slice;

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

struct $Ord;
typedef struct $Ord *$Ord;

struct $Ord$class;
typedef struct $Ord$class *$Ord$class;

struct $Logical;
typedef struct $Logical *$Logical;

struct $Logical$class;
typedef struct $Logical$class *$Logical$class;

struct $Plus;
typedef struct $Plus *$Plus;

struct $Plus$class;
typedef struct $Plus$class *$Plus$class;

struct $Times;
typedef struct $Times *$Times;

struct $Times$class;
typedef struct $Times$class *$Times$class;

struct $Div;
typedef struct $Div *$Div;

struct $Div$class;
typedef struct $Div$class *$Div$class;

struct $Minus;
typedef struct $Minus *$Minus;

struct $Minus$class;
typedef struct $Minus$class *$Minus$class;

struct $Hashable;
typedef struct $Hashable *$Hashable;

struct $Hashable$class;
typedef struct $Hashable$class *$Hashable$class;

struct $Indexed;
typedef struct $Indexed *$Indexed;

struct $Indexed$class;
typedef struct $Indexed$class *$Indexed$class;

struct $Sliceable;
typedef struct $Sliceable *$Sliceable;

struct $Sliceable$class;
typedef struct $Sliceable$class *$Sliceable$class;

struct $Iterable;
typedef struct $Iterable *$Iterable;

struct $Iterable$class;
typedef struct $Iterable$class *$Iterable$class;

struct $Collection;
typedef struct $Collection *$Collection;

struct $Collection$class;
typedef struct $Collection$class *$Collection$class;

struct $Container;
typedef struct $Container *$Container;

struct $Container$class;
typedef struct $Container$class *$Container$class;

struct $Sequence;
typedef struct $Sequence *$Sequence;

struct $Sequence$class;
typedef struct $Sequence$class *$Sequence$class;

struct $Mapping;
typedef struct $Mapping *$Mapping;

struct $Mapping$class;
typedef struct $Mapping$class *$Mapping$class;

struct $Set;
typedef struct $Set *$Set;

struct $Set$class;
typedef struct $Set$class *$Set$class;

struct $Number;
typedef struct $Number *$Number;

struct $Number$class;
typedef struct $Number$class *$Number$class;

struct $Real;
typedef struct $Real *$Real;

struct $Real$class;
typedef struct $Real$class *$Real$class;

struct $Rational;
typedef struct $Rational *$Rational;

struct $Rational$class;
typedef struct $Rational$class *$Rational$class;

struct $Integral;
typedef struct $Integral *$Integral;

struct $Integral$class;
typedef struct $Integral$class *$Integral$class;

struct $Hashable$bool;
typedef struct $Hashable$bool *$Hashable$bool;

struct $Hashable$bool$class;
typedef struct $Hashable$bool$class *$Hashable$bool$class;

struct $Sequence$list;
typedef struct $Sequence$list *$Sequence$list;

struct $Sequence$list$class;
typedef struct $Sequence$list$class *$Sequence$list$class;

struct $Collection$list;
typedef struct $Collection$list *$Collection$list;

struct $Collection$list$class;
typedef struct $Collection$list$class *$Collection$list$class;

struct $Times$list;
typedef struct $Times$list *$Times$list;

struct $Times$list$class;
typedef struct $Times$list$class *$Times$list$class;

struct $Container$list;
typedef struct $Container$list *$Container$list;

struct $Container$list$class;
typedef struct $Container$list$class *$Container$list$class;

struct $Ord$list;
typedef struct $Ord$list *$Ord$list;

struct $Ord$list$class;
typedef struct $Ord$list$class *$Ord$list$class;

struct $Mapping$dict;
typedef struct $Mapping$dict *$Mapping$dict;

struct $Mapping$dict$class;
typedef struct $Mapping$dict$class *$Mapping$dict$class;

struct $Indexed$dict;
typedef struct $Indexed$dict *$Indexed$dict;

struct $Indexed$dict$class;
typedef struct $Indexed$dict$class *$Indexed$dict$class;

struct $Ord$dict;
typedef struct $Ord$dict *$Ord$dict;

struct $Ord$dict$class;
typedef struct $Ord$dict$class *$Ord$dict$class;

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

struct $Times$str;
typedef struct $Times$str *$Times$str;

struct $Times$str$class;
typedef struct $Times$str$class *$Times$str$class;

struct $Hashable$str;
typedef struct $Hashable$str *$Hashable$str;

struct $Hashable$str$class;
typedef struct $Hashable$str$class *$Hashable$str$class;

struct $Integral$i64;
typedef struct $Integral$i64 *$Integral$i64;

struct $Integral$i64$class;
typedef struct $Integral$i64$class *$Integral$i64$class;

struct $Logical$i64;
typedef struct $Logical$i64 *$Logical$i64;

struct $Logical$i64$class;
typedef struct $Logical$i64$class *$Logical$i64$class;

struct $Minus$i64;
typedef struct $Minus$i64 *$Minus$i64;

struct $Minus$i64$class;
typedef struct $Minus$i64$class *$Minus$i64$class;

struct $Div$i64;
typedef struct $Div$i64 *$Div$i64;

struct $Div$i64$class;
typedef struct $Div$i64$class *$Div$i64$class;

struct $Ord$i64;
typedef struct $Ord$i64 *$Ord$i64;

struct $Ord$i64$class;
typedef struct $Ord$i64$class *$Ord$i64$class;

struct $Hashable$i64;
typedef struct $Hashable$i64 *$Hashable$i64;

struct $Hashable$i64$class;
typedef struct $Hashable$i64$class *$Hashable$i64$class;

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

struct $Div$int;
typedef struct $Div$int *$Div$int;

struct $Div$int$class;
typedef struct $Div$int$class *$Div$int$class;

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

struct $Div$float;
typedef struct $Div$float *$Div$float;

struct $Div$float$class;
typedef struct $Div$float$class *$Div$float$class;

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

struct $Div$complex;
typedef struct $Div$complex *$Div$complex;

struct $Div$complex$class;
typedef struct $Div$complex$class *$Div$complex$class;

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

struct $Times$bytearray;
typedef struct $Times$bytearray *$Times$bytearray;

struct $Times$bytearray$class;
typedef struct $Times$bytearray$class *$Times$bytearray$class;

struct $Container$bytearray;
typedef struct $Container$bytearray *$Container$bytearray;

struct $Container$bytearray$class;
typedef struct $Container$bytearray$class *$Container$bytearray$class;

struct $Ord$bytes;
typedef struct $Ord$bytes *$Ord$bytes;

struct $Ord$bytes$class;
typedef struct $Ord$bytes$class *$Ord$bytes$class;

struct $Container$bytes;
typedef struct $Container$bytes *$Container$bytes;

struct $Container$bytes$class;
typedef struct $Container$bytes$class *$Container$bytes$class;

struct $Sliceable$bytes;
typedef struct $Sliceable$bytes *$Sliceable$bytes;

struct $Sliceable$bytes$class;
typedef struct $Sliceable$bytes$class *$Sliceable$bytes$class;

struct $Times$bytes;
typedef struct $Times$bytes *$Times$bytes;

struct $Times$bytes$class;
typedef struct $Times$bytes$class *$Times$bytes$class;

struct $Hashable$bytes;
typedef struct $Hashable$bytes *$Hashable$bytes;

struct $Hashable$bytes$class;
typedef struct $Hashable$bytes$class *$Hashable$bytes$class;

struct $Hashable$WORD;
typedef struct $Hashable$WORD *$Hashable$WORD;

struct $Hashable$WORD$class;
typedef struct $Hashable$WORD$class *$Hashable$WORD$class;


// $Eq ////////////////////////////////////////////////////////////

struct $Eq {
    $Eq$class $class;
};

struct $Eq$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Eq);
    void (*__serialize__)($Eq,$Serial$state);
    $Eq (*__deserialize__)($Eq,$Serial$state);
    $bool (*__bool__)($Eq);
    $str (*__str__)($Eq);
    $str (*__repr__)($Eq);
    $bool (*__eq__)($Eq, $WORD, $WORD);
    $bool (*__ne__)($Eq, $WORD, $WORD);
};

$WORD $Eq$__ne__($Eq wit, $WORD a, $WORD b);
extern struct $Eq$class $Eq$methods;
$Eq $Eq$new();

// $Ord ////////////////////////////////////////////////////////////

struct $Ord {
    $Ord$class $class;
};

struct $Ord$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Ord);
    void (*__serialize__)($Ord,$Serial$state);
    $Ord (*__deserialize__)($Ord,$Serial$state);
    $bool (*__bool__)($Ord);
    $str (*__str__)($Ord);
    $str (*__repr__)($Ord);
    $bool (*__eq__)($Ord, $WORD, $WORD);
    $bool (*__ne__)($Ord, $WORD, $WORD);
    $bool (*__lt__)($Ord, $WORD, $WORD);
    $bool (*__le__)($Ord, $WORD, $WORD);
    $bool (*__gt__)($Ord, $WORD, $WORD);
    $bool (*__ge__)($Ord, $WORD, $WORD);
};

$WORD $Ord$__gt__($Ord wit, $WORD a, $WORD b);
$WORD $Ord$__ge__($Ord wit, $WORD a, $WORD b);
extern struct $Ord$class $Ord$methods;
$Ord $Ord$new();

// $Logical ////////////////////////////////////////////////////////////

struct $Logical {
    $Logical$class $class;
};

struct $Logical$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Logical);
    void (*__serialize__)($Logical,$Serial$state);
    $Logical (*__deserialize__)($Logical,$Serial$state);
    $bool (*__bool__)($Logical);
    $str (*__str__)($Logical);
    $str (*__repr__)($Logical);
    $WORD (*__and__)($Logical, $WORD, $WORD);
    $WORD (*__or__)($Logical, $WORD, $WORD);
    $WORD (*__xor__)($Logical, $WORD, $WORD);
    $WORD (*__iand__)($Logical, $WORD, $WORD);
    $WORD (*__ior__)($Logical, $WORD, $WORD);
    $WORD (*__ixor__)($Logical, $WORD, $WORD);
};

$WORD $Logical$__iand__($Logical, $WORD, $WORD);
$WORD $Logical$__ior__($Logical, $WORD, $WORD);
$WORD $Logical$__ixor__($Logical, $WORD, $WORD);

extern struct $Logical$class $Logical$methods;
$Logical $Logical$new();

// $Plus ////////////////////////////////////////////////////////////

struct $Plus {
    $Plus$class $class;
};

struct $Plus$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Plus);
    void (*__serialize__)($Plus,$Serial$state);
    $Plus (*__deserialize__)($Plus,$Serial$state);
    $bool (*__bool__)($Plus);
    $str (*__str__)($Plus);
    $str (*__repr__)($Plus);
    $WORD (*__add__)($Plus, $WORD, $WORD);
    $WORD (*__iadd__)($Plus, $WORD, $WORD);
};

$WORD $Plus$__iadd__ ($Plus, $WORD, $WORD);

extern struct $Plus$class $Plus$methods;
$Plus $Plus$new();

// $Times ////////////////////////////////////////////////////////////

struct $Times {
    $Times$class $class;
};

struct $Times$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Times);
    void (*__serialize__)($Times,$Serial$state);
    $Times (*__deserialize__)($Times,$Serial$state);
    $bool (*__bool__)($Times);
    $str (*__str__)($Times);
    $str (*__repr__)($Times);
    $WORD (*__add__)($Times, $WORD, $WORD);
    $WORD (*__iadd__)($Times, $WORD, $WORD);
    $WORD (*__mul__)($Times, $WORD, $WORD);
    $WORD (*__imul__)($Times, $WORD, $WORD);
};

$WORD $Times$__imul__ ($Times, $WORD, $WORD);

extern struct $Times$class $Times$methods;
$Times $Times$new();

// $Div ////////////////////////////////////////////////////////////

struct $Div {
    $Div$class $class;
};

struct $Div$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Div);
    void (*__serialize__)($Div,$Serial$state);
    $Div (*__deserialize__)($Div,$Serial$state);
    $bool (*__bool__)($Div);
    $str (*__str__)($Div);
    $str (*__repr__)($Div);
    $WORD (*__truediv__)($Div, $WORD, $WORD);
    $WORD (*__itruediv__)($Div, $WORD, $WORD);
};

$WORD $Div$__itruediv__ ($Div, $WORD, $WORD);

extern struct $Div$class $Div$methods;
$Div $Div$new();

// $Minus ////////////////////////////////////////////////////////////

struct $Minus {
    $Minus$class $class;
};

struct $Minus$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Minus);
    void (*__serialize__)($Minus,$Serial$state);
    $Minus (*__deserialize__)($Minus,$Serial$state);
    $bool (*__bool__)($Minus);
    $str (*__str__)($Minus);
    $str (*__repr__)($Minus);
    $WORD (*__sub__)($Minus, $WORD, $WORD);
    $WORD (*__isub__)($Minus, $WORD, $WORD);
};

$WORD $Minus$__isub__ ($Minus, $WORD, $WORD);

extern struct $Minus$class $Minus$methods;
$Minus $Minus$new();

// $Hashable ////////////////////////////////////////////////////////////

struct $Hashable {
    $Hashable$class $class;
};

struct $Hashable$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Hashable);
    void (*__serialize__)($Hashable,$Serial$state);
    $Hashable (*__deserialize__)($Hashable,$Serial$state);
    $bool (*__bool__)($Hashable);
    $str (*__str__)($Hashable);
    $str (*__repr__)($Hashable);
    $bool (*__eq__)($Hashable, $WORD, $WORD);
    $bool (*__ne__)($Hashable, $WORD, $WORD);
    $int (*__hash__)($Hashable, $WORD);
};

extern struct $Hashable$class $Hashable$methods;
$Hashable $Hashable$new();

// $Indexed ////////////////////////////////////////////////////////////

struct $Indexed {
    $Indexed$class $class;
    $Eq w$Eq$A$Indexed;
};

struct $Indexed$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Indexed, $Eq);
    void (*__serialize__)($Indexed,$Serial$state);
    $Indexed (*__deserialize__)($Indexed,$Serial$state);
    $bool (*__bool__)($Indexed);
    $str (*__str__)($Indexed);
    $str (*__repr__)($Indexed);
    $WORD (*__getitem__)($Indexed, $WORD, $WORD);
    void (*__setitem__)($Indexed, $WORD, $WORD, $WORD);
    void (*__delitem__)($Indexed, $WORD, $WORD);
};

extern struct $Indexed$class $Indexed$methods;
$Indexed $Indexed$new($Eq);

// $Sliceable ////////////////////////////////////////////////////////////

struct $Sliceable {
    $Sliceable$class $class;
};

struct $Sliceable$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Sliceable);
    void (*__serialize__)($Sliceable,$Serial$state);
    $Sliceable (*__deserialize__)($Sliceable,$Serial$state);
    $bool (*__bool__)($Sliceable);
    $str (*__str__)($Sliceable);
    $str (*__repr__)($Sliceable);
    $WORD (*__getitem__)($Sliceable, $WORD, $int);
    void (*__setitem__)($Sliceable, $WORD, $int, $WORD);
    void (*__delitem__)($Sliceable, $WORD, $int);
    $WORD (*__getslice__)($Sliceable, $WORD, $slice);
    void (*__setslice__)($Sliceable, $WORD, $Iterable, $slice, $WORD);
    void (*__delslice__)($Sliceable, $WORD, $slice);
};

extern struct $Sliceable$class $Sliceable$methods;
$Sliceable $Sliceable$new();

// $Iterable ////////////////////////////////////////////////////////////

struct $Iterable {
    $Iterable$class $class;
};

struct $Iterable$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Iterable);
    void (*__serialize__)($Iterable,$Serial$state);
    $Iterable (*__deserialize__)($Iterable,$Serial$state);
    $bool (*__bool__)($Iterable);
    $str (*__str__)($Iterable);
    $str (*__repr__)($Iterable);
    $Iterator (*__iter__)($Iterable, $WORD);
};

extern struct $Iterable$class $Iterable$methods;
$Iterable $Iterable$new();

// $Collection ////////////////////////////////////////////////////////////

struct $Collection {
    $Collection$class $class;
};

struct $Collection$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Collection);
    void (*__serialize__)($Collection,$Serial$state);
    $Collection (*__deserialize__)($Collection,$Serial$state);
    $bool (*__bool__)($Collection);
    $str (*__str__)($Collection);
    $str (*__repr__)($Collection);
    $Iterator (*__iter__)($Collection, $WORD);
    $WORD (*__fromiter__)($Collection, $Iterable, $WORD);
    $int (*__len__)($Collection, $WORD);
};

extern struct $Collection$class $Collection$methods;
$Collection $Collection$new();

// $Container ////////////////////////////////////////////////////////////

struct $Container {
    $Container$class $class;
    $Eq w$Eq$A$Container;
};

struct $Container$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Container, $Eq);
    void (*__serialize__)($Container,$Serial$state);
    $Container (*__deserialize__)($Container,$Serial$state);
    $bool (*__bool__)($Container);
    $str (*__str__)($Container);
    $str (*__repr__)($Container);
    $Iterator (*__iter__)($Container, $WORD);
    $WORD (*__fromiter__)($Container, $Iterable, $WORD);
    $int (*__len__)($Container, $WORD);
    $bool (*__contains__)($Container, $WORD, $WORD);
    $bool (*__containsnot__)($Container, $WORD, $WORD);
};

extern struct $Container$class $Container$methods;

// $Sequence ////////////////////////////////////////////////////////////

struct $Sequence {
    $Sequence$class $class;
    $Collection w$Collection;
    $Times w$Times;
};

struct $Sequence$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Sequence);
    void (*__serialize__)($Sequence,$Serial$state);
    $Sequence (*__deserialize__)($Sequence,$Serial$state);
    $bool (*__bool__)($Sequence);
    $str (*__str__)($Sequence);
    $str (*__repr__)($Sequence);
    $WORD (*__getitem__)($Sequence, $WORD, $int);
    void (*__setitem__)($Sequence, $WORD, $int, $WORD);
    void (*__delitem__)($Sequence, $WORD, $int);
    $WORD (*__getslice__)($Sequence, $WORD, $slice);
    void (*__setslice__)($Sequence, $WORD, $Iterable, $slice, $WORD);
    void (*__delslice__)($Sequence, $WORD, $slice);
    $Iterator (*__reversed__)($Sequence, $WORD);
    void (*insert)($Sequence, $WORD, $int, $WORD);
    void (*append)($Sequence, $WORD, $WORD);
    void (*reverse)($Sequence, $WORD);
};

extern struct $Sequence$class $Sequence$methods;
$Sequence $Sequence$new();

// $Collection$Sequence ////////////////////////////////////////////////

#define $Collection$Sequence $Collection
#define $Collection$Sequence$class $Collection$class
#define $Collection$Sequence$methods $Collection$methods
#define $Collection$Sequence$new(...) $Collection$new(__VA_ARGS__)

// $Times$Sequence /////////////////////////////////////////////////////

#define $Times$Sequence $Times
#define $Times$Sequence$class $Times$class
#define $Times$Sequence$methods $Times$methods
#define $Times$Sequence$new(...) $Times$new(__VA_ARGS__)

// $Mapping ////////////////////////////////////////////////////////////

struct $Mapping {
    $Mapping$class $class;
    $Indexed w$Indexed;
    $Eq w$Eq$A$Mapping;
};

struct $Mapping$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Mapping, $Eq);
    void (*__serialize__)($Mapping,$Serial$state);
    $Mapping (*__deserialize__)($Mapping,$Serial$state);
    $bool (*__bool__)($Mapping);
    $str (*__str__)($Mapping);
    $str (*__repr__)($Mapping);
    $Iterator (*__iter__)($Mapping, $WORD);
    $WORD (*__fromiter__)($Mapping, $Iterable, $WORD);
    $int (*__len__)($Mapping, $WORD);
    $bool (*__contains__)($Mapping, $WORD, $WORD);
    $bool (*__containsnot__)($Mapping, $WORD, $WORD);
    $WORD (*get)($Mapping, $WORD, $WORD, $WORD);
    $Iterator (*keys)($Mapping, $WORD);
    $Iterator (*values)($Mapping, $WORD);
    $Iterator (*items)($Mapping, $WORD);
    void (*update)($Mapping, $WORD, $Iterable, $WORD);
    $tuple (*popitem)($Mapping, $WORD);
    void (*setdefault)($Mapping, $WORD, $WORD, $WORD);
};

extern struct $Mapping$class $Mapping$methods;
$Mapping $Mapping$new($Eq w$Eq$A$Mapping);

// $Indexed$Mapping ////////////////////////////////////////////////

#define $Indexed$Mapping $Indexed
#define $Indexed$Mapping$class $Indexed$class
#define $Indexed$Mapping$methods $Indexed$methods
#define $Indexed$Mapping$new(...) $Indexed$new(__VA_ARGS__)

// $Set ////////////////////////////////////////////////////////////

struct $Set {
    $Set$class $class;
    $Ord w$Ord;
    $Logical w$Logical;
    $Minus w$Minus;
    $Eq w$Eq$A$Set;
};

struct $Set$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Set, $Eq);
    void (*__serialize__)($Set,$Serial$state);
    $Set (*__deserialize__)($Set,$Serial$state);
    $bool (*__bool__)($Set);
    $str (*__str__)($Set);
    $str (*__repr__)($Set);
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

extern struct $Set$class $Set$methods;

// $Ord$Set ///////////////////////////////////////////////////////////

#define $Ord$Set $Ord
#define $Ord$Set$class $Ord$class
#define $Ord$Set$methods $Ord$methods
#define $Ord$Set$new(...) $Ord$new(__VA_ARGS__)

// $Logical$Set ///////////////////////////////////////////////////////

#define $Logical$Set $Logical
#define $Logical$Set$class $Logical$class
#define $Logical$Set$methods $Logical$methods
#define $Logical$Set$new(...) $Logical$new(__VA_ARGS__)

// $Minus$Set /////////////////////////////////////////////////////////

#define $Minus$Set $Minus
#define $Minus$Set$class $Minus$class
#define $Minus$Set$methods $Minus$methods
#define $Minus$Set$new(...) $Minus$new(__VA_ARGS__)

// $Number ////////////////////////////////////////////////////////////

struct $Number {
    $Number$class $class;
    $Minus w$Minus;
};

struct $Number$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Number);
    void (*__serialize__)($Number,$Serial$state);
    $Number (*__deserialize__)($Number,$Serial$state);
    $bool (*__bool__)($Number);
    $str (*__str__)($Number);
    $str (*__repr__)($Number);
    $WORD (*__add__)($Number, $WORD, $WORD);
    $WORD (*__iadd__)($Number, $WORD, $WORD);
    $WORD (*__mul__)($Number, $WORD, $WORD);
    $WORD (*__imul__)($Number, $WORD, $WORD);
    $WORD (*__fromatom__)($Number,$atom);
    $complex (*__complx__)($Number, $WORD);
    //    $WORD (*__truediv__)($Number, $WORD, $WORD);
    $WORD (*__pow__)($Number, $WORD, $WORD);
    //    $WORD (*__itruediv__)($Number, $WORD, $WORD);
    $WORD (*__ipow__)($Number, $WORD, $WORD);
    $WORD (*__neg__)($Number, $WORD);
    $WORD (*__pos__)($Number, $WORD);
    $WORD (*real)($Number, $WORD, $Real);
    $WORD (*imag)($Number, $WORD, $Real);
    $WORD (*__abs__)($Number, $WORD, $Real);
    $WORD (*conjugate)($Number, $WORD);
};

//$WORD $Number$__itruediv__($Number, $WORD, $WORD);
$WORD $Number$__ipow__($Number, $WORD, $WORD);

extern struct $Number$class $Number$methods;
$Number $Number$new();

// $Minus$Number ////////////////////////////////////////////////////

#define $Minus$Number $Minus
#define $Minus$Number$class $Minus$class
#define $Minus$Number$methods $Minus$methods
#define $Minus$Number$new(...) $Minus$new(__VA_ARGS__)

// $Real ////////////////////////////////////////////////////////////

struct $Real {
    $Real$class $class;
};

struct $Real$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Real);
    void (*__serialize__)($Real,$Serial$state);
    $Real (*__deserialize__)($Real,$Serial$state);
    $bool (*__bool__)($Real);
    $str (*__str__)($Real);
    $str (*__repr__)($Real);
    $WORD (*__add__)($Real, $WORD, $WORD);
    $WORD (*__iadd__)($Real, $WORD, $WORD);
    $WORD (*__mul__)($Real, $WORD, $WORD);
    $WORD (*__imul__)($Real, $WORD, $WORD);
    $WORD (*__fromatom__)($Real,$atom);
    $complex (*__complx__)($Real, $WORD);
    //    $WORD (*__truediv__)($Real, $WORD, $WORD);
    $WORD (*__pow__)($Real, $WORD, $WORD);
    //    $WORD (*__itruediv__)($Real, $WORD, $WORD);
    $WORD (*__ipow__)($Real, $WORD, $WORD);
    $WORD (*__neg__)($Real, $WORD);
    $WORD (*__pos__)($Real, $WORD);
    $WORD (*real)($Real, $WORD, $Real);
    $WORD (*imag)($Real, $WORD, $Real);
    $WORD (*__abs__)($Real, $WORD, $Real);
    $WORD (*conjugate)($Real, $WORD);
    $float (*__float__)($Real, $WORD);
    $WORD (*__trunc__)($Real, $WORD, $Integral);
    $WORD (*__floor__)($Real, $WORD, $Integral);
    $WORD (*__ceil__)($Real, $WORD, $Integral);
    $WORD (*__round__)($Real, $WORD, $int);
};

extern struct $Real$class $Real$methods;
$Real $Real$new();

// $Minus$Real //////////////////////////////////////////////////////////

#define $Minus$Real $Minus
#define $Minus$Real$class $Minus$class
#define $Minus$Real$methods $Minus$methods
#define $Minus$Real$new(...) $Minus$new(__VA_ARGS__)

// $RealFloat ///////////////////////////////////////////////////////////

#define $RealFloat $Real
#define $RealFloat$class $Real$class
#define $RealFloat$methods $Real$methods
#define $RealFloat$new(...) $Real$new(__VA_ARGS__)

// $Minus$RealFloat /////////////////////////////////////////////////////

#define $Minus$RealFloat $Minus
#define $Minus$RealFloat$class $Minus$class
#define $Minus$RealFloat$methods $Minus$methods
#define $Minus$RealFloat$new(...) $Minus$new(__VA_ARGS__)

// $Rational ////////////////////////////////////////////////////////////

struct $Rational {
    $Rational$class $class;
};

struct $Rational$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Rational);
    void (*__serialize__)($Rational,$Serial$state);
    $Rational (*__deserialize__)($Rational,$Serial$state);
    $bool (*__bool__)($Rational);
    $str (*__str__)($Rational);
    $str (*__repr__)($Rational);
    $WORD (*__add__)($Rational, $WORD, $WORD);
    $WORD (*__iadd__)($Rational, $WORD, $WORD);
    $WORD (*__mul__)($Rational, $WORD, $WORD);
    $WORD (*__imul__)($Rational, $WORD, $WORD);
    $WORD (*__fromatom__)($Rational,$atom);
    $complex (*__complx__)($Rational, $WORD);
    //    $WORD (*__truediv__)($Rational, $WORD, $WORD);
    $WORD (*__pow__)($Rational, $WORD, $WORD);
    //    $WORD (*__itruediv__)($Rational, $WORD, $WORD);
    $WORD (*__ipow__)($Rational, $WORD, $WORD);
    $WORD (*__neg__)($Rational, $WORD);
    $WORD (*__pos__)($Rational, $WORD);
    $WORD (*real)($Rational, $WORD, $Real);
    $WORD (*imag)($Rational, $WORD, $Real);
    $WORD (*__abs__)($Rational, $WORD, $Real);
    $WORD (*conjugate)($Rational, $WORD);
    $float (*__float__)($Rational, $WORD);
    $WORD (*__trunc__)($Rational, $WORD, $Integral);
    $WORD (*__floor__)($Rational, $WORD, $Integral);
    $WORD (*__ceil__)($Rational, $WORD, $Integral);
    $WORD (*__round__)($Rational, $WORD, $int);
    $WORD (*numerator)($Rational, $WORD, $Integral);
    $WORD (*denominator)($Rational, $WORD, $Integral);
};

extern struct $Rational$class $Rational$methods;
$Rational $Rational$new();

// $Minus$Rational //////////////////////////////////////////////////////

#define $Minus$Rational $Minus
#define $Minus$Rational$class $Minus$class
#define $Minus$Rational$methods $Minus$methods
#define $Minus$Rational$new(...) $Minus$new(__VA_ARGS__)

// $Integral ////////////////////////////////////////////////////////////

struct $Integral {
    $Integral$class $class;
    $Minus w$Minus;
    $Logical w$Logical;
};

struct $Integral$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Integral);
    void (*__serialize__)($Integral,$Serial$state);
    $Integral (*__deserialize__)($Integral,$Serial$state);
    $bool (*__bool__)($Integral);
    $str (*__str__)($Integral);
    $str (*__repr__)($Integral);
    $WORD (*__add__)($Integral, $WORD, $WORD);
    $WORD (*__iadd__)($Integral, $WORD, $WORD);
    $WORD (*__mul__)($Integral, $WORD, $WORD);
    $WORD (*__imul__)($Integral, $WORD, $WORD);
    $WORD (*__fromatom__)($Integral,$atom);
    $complex (*__complx__)($Integral, $WORD);
    //    $WORD (*__truediv__)($Integral, $WORD, $WORD);
    $WORD (*__pow__)($Integral, $WORD, $WORD);
    //    $WORD (*__itruediv__)($Integral, $WORD, $WORD);
    $WORD (*__ipow__)($Integral, $WORD, $WORD);
    $WORD (*__neg__)($Integral, $WORD);
    $WORD (*__pos__)($Integral, $WORD);
    $WORD (*real)($Integral, $WORD, $Real);
    $WORD (*imag)($Integral, $WORD, $Real);
    $WORD (*__abs__)($Integral, $WORD, $Real);
    $WORD (*conjugate)($Integral, $WORD);
    $float (*__float__)($Integral, $WORD);
    $WORD (*__trunc__)($Integral, $WORD, $Integral);
    $WORD (*__floor__)($Integral, $WORD, $Integral);
    $WORD (*__ceil__)($Integral, $WORD, $Integral);
    $WORD (*__round__)($Integral, $WORD, $int);
    $WORD (*numerator)($Integral, $WORD, $Integral);
    $WORD (*denominator)($Integral, $WORD, $Integral);
    $int (*__int__)($Integral, $WORD);
    $int (*__index__)($Integral, $WORD);
    $tuple (*__divmod__)($Integral, $WORD, $WORD);
    $WORD (*__floordiv__)($Integral, $WORD, $WORD);
    $WORD (*__mod__)($Integral, $WORD, $WORD);
    $WORD (*__lshift__)($Integral, $WORD, $int);
    $WORD (*__rshift__)($Integral, $WORD, $int);
    $WORD (*__ifloordiv__)($Integral, $WORD, $WORD);
    $WORD (*__imod__)($Integral, $WORD, $WORD);
    $WORD (*__ilshift__)($Integral, $WORD, $int);
    $WORD (*__irshift__)($Integral, $WORD, $int);
    $WORD (*__invert__)($Integral, $WORD);
};

$WORD $Integral$__ifloordiv__($Integral, $WORD, $WORD);
$WORD $Integral$__imod__($Integral, $WORD, $WORD);
$WORD $Integral$__ilshift__($Integral, $WORD, $int);
$WORD $Integral$__irshift__($Integral, $WORD, $int);

extern struct $Integral$class $Integral$methods;

// $Minus$Integral ///////////////////////////////////////////////////////////

#define $Minus$Integral $Minus
#define $Minus$Integral$class $Minus$class
#define $Minus$Integral$methods $Minus$methods
#define $Minus$Integral$new(...) $Minus$new(__VA_ARGS__)

// $Logical$Integral /////////////////////////////////////////////////////////

#define $Logical$Integral $Logical
#define $Logical$Integral$class $Logical$class
#define $Logical$Integral$methods $Logical$methods
#define $Logical$Integral$new(...) $Logical$new(__VA_ARGS__)

// $Hashable$bool ////////////////////////////////////////////////////////////

struct $Hashable$bool {
    $Hashable$bool$class $class;
};

struct $Hashable$bool$class {
    char *$GCINFO;
    bool $class_id;
    $Super$class $superclass;
    void (*__init__)($Hashable$bool);
    void (*__serialize__)($Hashable$bool,$Serial$state);
    $Hashable$bool (*__deserialize__)($Hashable$bool,$Serial$state);
    $bool (*__bool__)($Hashable$bool);
    $str (*__str__)($Hashable$bool);
    $str (*__repr__)($Hashable$bool);
    $bool (*__eq__)($Hashable$bool, $bool, $bool);
    $bool (*__ne__)($Hashable$bool, $bool, $bool);
    $int (*__hash__)($Hashable$bool, $bool);
};

void $Hashable$bool$__init__ ($Hashable$bool);
void $Hashable$bool$__serialize__($Hashable$bool, $Serial$state);
$Hashable$bool $Hashable$bool$__deserialize__($Hashable$bool, $Serial$state);
$bool $Hashable$bool$__eq__ ($Hashable$bool, $bool, $bool);
$bool $Hashable$bool$__ne__ ($Hashable$bool, $bool, $bool);
$int $Hashable$bool$__hash__ ($Hashable$bool, $bool);

// $Sequence$list ////////////////////////////////////////////////////////////

struct $Sequence$list {
    $Sequence$list$class $class;
    $Collection w$Collection;
    $Times w$Times;
};

struct $Sequence$list$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Sequence$list);
    void (*__serialize__)($Sequence$list,$Serial$state);
    $Sequence$list (*__deserialize__)($Sequence$list,$Serial$state);
    $bool (*__bool__)($Sequence$list);
    $str (*__str__)($Sequence$list);
    $str (*__repr__)($Sequence$list);
    $WORD (*__getitem__)($Sequence$list, $list, $int);
    void (*__setitem__)($Sequence$list, $list, $int, $WORD);
    void (*__delitem__)($Sequence$list, $list, $int);
    $list (*__getslice__)($Sequence$list, $list, $slice);
    void (*__setslice__)($Sequence$list, $list, $Iterable, $slice, $WORD);
    void (*__delslice__)($Sequence$list, $list, $slice);
    $Iterator (*__reversed__)($Sequence$list, $list);
    void (*insert)($Sequence$list, $list, $int, $WORD);
    void (*append)($Sequence$list, $list, $WORD);
    void (*reverse)($Sequence$list, $list);
};

void $Sequence$list$__init__ ($Sequence$list);
void $Sequence$list$__serialize__($Sequence$list, $Serial$state);
$Sequence$list $Sequence$list$__deserialize__($Sequence$list, $Serial$state);
$WORD $Sequence$list$__getitem__ ($Sequence$list, $list, $int);
void $Sequence$list$__setitem__ ($Sequence$list, $list, $int, $WORD);
void $Sequence$list$__delitem__ ($Sequence$list, $list, $int);
$list $Sequence$list$__getslice__ ($Sequence$list, $list, $slice);
void $Sequence$list$__setslice__ ($Sequence$list, $list, $Iterable, $slice, $WORD);
void $Sequence$list$__delslice__ ($Sequence$list, $list, $slice);
$Iterator $Sequence$list$__reversed__ ($Sequence$list, $list);
void $Sequence$list$insert ($Sequence$list, $list, $int, $WORD);
void $Sequence$list$append ($Sequence$list, $list, $WORD);
void $Sequence$list$reverse ($Sequence$list, $list);

// $Collection$list ////////////////////////////////////////////////////////////

struct $Collection$list {
    $Collection$list$class $class;
    $Sequence w$Sequence;
};

struct $Collection$list$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Collection$list, $Sequence);
    void (*__serialize__)($Collection$list,$Serial$state);
    $Collection$list (*__deserialize__)($Collection$list,$Serial$state);
    $bool (*__bool__)($Collection$list);
    $str (*__str__)($Collection$list);
    $str (*__repr__)($Collection$list);
    $Iterator (*__iter__)($Collection$list, $list);
    $list (*__fromiter__)($Collection$list, $Iterable, $WORD);
    $int (*__len__)($Collection$list, $list);
};

void $Collection$list$__init__ ($Collection$list, $Sequence);
void $Collection$list$__serialize__($Collection$list, $Serial$state);
$Collection$list $Collection$list$__deserialize__($Collection$list, $Serial$state);
$Iterator $Collection$list$__iter__ ($Collection$list, $list);
$list $Collection$list$__fromiter__ ($Collection$list, $Iterable, $WORD);
$int $Collection$list$__len__ ($Collection$list, $list);

// $Collection$Sequence ////////////////////////////////////////////////

#define $Collection$Sequence$list $Collection$list
#define $Collection$Sequence$list$class $Collection$list$class
#define $Collection$Sequence$list$methods $Collection$list$methods
#define $Collection$Sequence$list$new(...) $Collection$list$new(__VA_ARGS__)

// $Times$list ////////////////////////////////////////////////////////////

struct $Times$list {
    $Times$list$class $class;
    $Sequence w$Sequence;
};

struct $Times$list$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Times$list, $Sequence);
    void (*__serialize__)($Times$list,$Serial$state);
    $Times$list (*__deserialize__)($Times$list,$Serial$state);
    $bool (*__bool__)($Times$list);
    $str (*__str__)($Times$list);
    $str (*__repr__)($Times$list);
    $list (*__add__)($Times$list, $list, $list);
    $list (*__iadd__)($Times$list, $list, $list);
    $list (*__mul__)($Times$list, $list, $int);
    $list (*__imul__)($Times$list, $list, $int);
};

void $Times$list$__init__ ($Times$list, $Sequence);
void $Times$list$__serialize__($Times$list, $Serial$state);
$Times$list $Times$list$__deserialize__($Times$list, $Serial$state);
$list $Times$list$__add__ ($Times$list, $list, $list);
$list $Times$list$__mul__($Times$list, $list, $int);

// $Times$Sequence /////////////////////////////////////////////////////

#define $Times$Sequence$list $Times$list
#define $Times$Sequence$list$class $Times$list$class
#define $Times$Sequence$list$methods $Times$list$methods
#define $Times$Sequence$list$new(...) $Times$list$new(__VA_ARGS__)

// $Container$list ////////////////////////////////////////////////////////////

struct $Container$list {
    $Container$list$class $class;
    $Eq w$Eq$A$Container$list;
};

struct $Container$list$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Container$list, $Eq);
    void (*__serialize__)($Container$list,$Serial$state);
    $Container$list (*__deserialize__)($Container$list,$Serial$state);
    $bool (*__bool__)($Container$list);
    $str (*__str__)($Container$list);
    $str (*__repr__)($Container$list);
    $Iterator (*__iter__)($Container$list, $list);
    $list (*__fromiter__)($Container$list, $Iterable, $WORD);
    $int (*__len__)($Container$list, $list);
    $bool (*__contains__)($Container$list, $list, $WORD);
    $bool (*__containsnot__)($Container$list, $list, $WORD);
};

void $Container$list$__init__ ($Container$list, $Eq);
void $Container$list$__serialize__($Container$list, $Serial$state);
$Container$list $Container$list$__deserialize__($Container$list, $Serial$state);
$Iterator $Container$list$__iter__ ($Container$list, $list);
$list $Container$list$__fromiter__ ($Container$list, $Iterable, $WORD);
$int $Container$list$__len__ ($Container$list, $list);
$bool $Container$list$__contains__ ($Container$list, $list, $WORD);
$bool $Container$list$__containsnot__ ($Container$list, $list, $WORD);


// $Ord$list //////////////////////////////////////////////////////////////////////

struct $Ord$list {
    $Ord$list$class $class;
    $Ord w$Ord$A$Ord$list;
};

struct $Ord$list$class {  
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Ord$list,$Ord);
    void (*__serialize__)($Ord$list,$Serial$state);
    $Ord$list (*__deserialize__)($Ord$list,$Serial$state);
    $bool (*__bool__)($Ord$list);
    $str (*__str__)($Ord$list);
    $str (*__repr__)($Ord$list);
    $bool (*__eq__)($Ord$list, $list, $list);
    $bool (*__ne__)($Ord$list, $list, $list);
    $bool (*__lt__)($Ord$list, $list, $list);
    $bool (*__le__)($Ord$list, $list, $list);
    $bool (*__gt__)($Ord$list, $list, $list);
    $bool (*__ge__)($Ord$list, $list, $list);
};

void $Ord$list$__init__ ($Ord$list, $Ord);
void $Ord$list$__serialize__($Ord$list, $Serial$state);
$Ord$list $Ord$list$__deserialize__($Ord$list, $Serial$state);
$bool $Ord$list$__eq__ ($Ord$list, $list, $list);
$bool $Ord$list$__ne__ ($Ord$list, $list, $list);
$bool $Ord$list$__lt__ ($Ord$list, $list, $list);
$bool $Ord$list$__le__ ($Ord$list, $list, $list);
$bool $Ord$list$__gt__ ($Ord$list, $list, $list);
$bool $Ord$list$__ge__ ($Ord$list, $list, $list);

  
// $Mapping$dict ////////////////////////////////////////////////////////////

struct $Mapping$dict {
    $Mapping$dict$class $class;
    $Indexed w$Indexed;
    $Eq w$Eq$A$Mapping$dict;
    $Hashable w$Hashable$A$Mapping$dict;
};

struct $Mapping$dict$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Mapping$dict, $Hashable);
    void (*__serialize__)($Mapping$dict,$Serial$state);
    $Mapping$dict (*__deserialize__)($Mapping$dict,$Serial$state);
    $bool (*__bool__)($Mapping$dict);
    $str (*__str__)($Mapping$dict);
    $str (*__repr__)($Mapping$dict);
    $Iterator (*__iter__)($Mapping$dict, $dict);
    $dict (*__fromiter__)($Mapping$dict, $Iterable, $WORD);
    $int (*__len__)($Mapping$dict, $dict);
    $bool (*__contains__)($Mapping$dict, $dict, $WORD);
    $bool (*__containsnot__)($Mapping$dict, $dict, $WORD);
    $WORD (*get)($Mapping$dict, $dict, $WORD, $WORD);
    $Iterator (*keys)($Mapping$dict, $dict);
    $Iterator (*values)($Mapping$dict, $dict);
    $Iterator (*items)($Mapping$dict, $dict);
    void (*update)($Mapping$dict, $dict, $Iterable, $WORD);
    $tuple (*popitem)($Mapping$dict, $dict);
    void (*setdefault)($Mapping$dict, $dict, $WORD, $WORD);
};

void $Mapping$dict$__init__ ($Mapping$dict, $Hashable);
void $Mapping$dict$__serialize__($Mapping$dict, $Serial$state);
$Mapping$dict $Mapping$dict$__deserialize__($Mapping$dict, $Serial$state);
$Iterator $Mapping$dict$__iter__ ($Mapping$dict, $dict);
$dict $Mapping$dict$__fromiter__ ($Mapping$dict, $Iterable, $WORD);
$int $Mapping$dict$__len__ ($Mapping$dict, $dict);
$bool $Mapping$dict$__contains__ ($Mapping$dict, $dict, $WORD);
$bool $Mapping$dict$__containsnot__ ($Mapping$dict, $dict, $WORD);
$WORD $Mapping$dict$get ($Mapping$dict, $dict, $WORD, $WORD);
$Iterator $Mapping$dict$keys ($Mapping$dict, $dict);
$Iterator $Mapping$dict$values ($Mapping$dict, $dict);
$Iterator $Mapping$dict$items ($Mapping$dict, $dict);
void $Mapping$dict$update ($Mapping$dict, $dict, $Iterable, $WORD);
$tuple $Mapping$dict$popitem ($Mapping$dict, $dict);
void $Mapping$dict$setdefault ($Mapping$dict, $dict, $WORD, $WORD);

// $Indexed$dict ////////////////////////////////////////////////////////////

struct $Indexed$dict {
    $Indexed$dict$class $class;
    $Mapping w$Mapping;
    $Eq w$Eq$A$Mapping$dict;
    $Hashable w$Hashable$A$Mapping$dict;
};

struct $Indexed$dict$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Indexed$dict, $Mapping, $Eq);
    void (*__serialize__)($Indexed$dict,$Serial$state);
    $Indexed$dict (*__deserialize__)($Indexed$dict,$Serial$state);
    $bool (*__bool__)($Indexed$dict);
    $str (*__str__)($Indexed$dict);
    $str (*__repr__)($Indexed$dict);
    $WORD (*__getitem__)($Indexed$dict, $dict, $WORD);
    void (*__setitem__)($Indexed$dict, $dict, $WORD, $WORD);
    void (*__delitem__)($Indexed$dict, $dict, $WORD);
};

void $Indexed$dict$__init__ ($Indexed$dict, $Mapping, $Eq);
void $Indexed$dict$__serialize__($Indexed$dict, $Serial$state);
$Indexed$dict $Indexed$dict$__deserialize__($Indexed$dict, $Serial$state);
$WORD $Indexed$dict$__getitem__ ($Indexed$dict, $dict, $WORD);
void $Indexed$dict$__setitem__ ($Indexed$dict, $dict, $WORD, $WORD);
void $Indexed$dict$__delitem__ ($Indexed$dict, $dict, $WORD);

// $Indexed$Mapping$dict /////////////////////////////////////////////////////////

#define $Indexed$Mapping$dict $Indexed$dict
#define $Indexed$Mapping$dict$class $Indexed$dict$class
#define $Indexed$Mapping$dict$methods $Indexed$dict$methods
#define $Indexed$Mapping$dict$new(...) $Indexed$dict$new(__VA_ARGS__)

// $Ord$dict //////////////////////////////////////////////////////////////////////

struct $Ord$dict {
    $Ord$dict$class $class;
    $Eq w$Eq$A$Ord$dict;
    $Hashable w$Hashable$A$Ord$dict;
    $Eq w$Eq$B$Ord$dict;
};

struct $Ord$dict$class {  
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Ord$dict, $Hashable, $Eq);
    void (*__serialize__)($Ord$dict,$Serial$state);
    $Ord$dict (*__deserialize__)($Ord$dict,$Serial$state);
    $bool (*__bool__)($Ord$dict);
    $str (*__str__)($Ord$dict);
    $str (*__repr__)($Ord$dict);
    $bool (*__eq__)($Ord$dict, $dict, $dict);
    $bool (*__ne__)($Ord$dict, $dict, $dict);
    $bool (*__lt__)($Ord$dict, $dict, $dict);
    $bool (*__le__)($Ord$dict, $dict, $dict);
    $bool (*__gt__)($Ord$dict, $dict, $dict);
    $bool (*__ge__)($Ord$dict, $dict, $dict);
};

void $Ord$dict$__init__ ($Ord$dict, $Hashable, $Eq);
void $Ord$dict$__serialize__($Ord$dict, $Serial$state);
$Ord$dict $Ord$dict$__deserialize__($Ord$dict, $Serial$state);
$bool $Ord$dict$__eq__ ($Ord$dict, $dict, $dict);
$bool $Ord$dict$__ne__ ($Ord$dict, $dict, $dict);
$bool $Ord$dict$__lt__ ($Ord$dict, $dict, $dict);
$bool $Ord$dict$__le__ ($Ord$dict, $dict, $dict);
$bool $Ord$dict$__gt__ ($Ord$dict, $dict, $dict);
$bool $Ord$dict$__ge__ ($Ord$dict, $dict, $dict);
  
// $Set$set ////////////////////////////////////////////////////////////

struct $Set$set {
    $Set$set$class $class;
    $Ord w$Ord;
    $Logical w$Logical;
    $Minus w$Minus;
    $Eq w$Eq$A$Set$set;
    $Hashable w$Hashable$A$Set$set;
};

struct $Set$set$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Set$set, $Hashable);
    void (*__serialize__)($Set$set,$Serial$state);
    $Set$set (*__deserialize__)($Set$set,$Serial$state);
    $bool (*__bool__)($Set$set);
    $str (*__str__)($Set$set);
    $str (*__repr__)($Set$set);
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
void $Set$set$__serialize__($Set$set, $Serial$state);
$Set$set $Set$set$__deserialize__($Set$set, $Serial$state);
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
    $Set w$Set;
};

struct $Ord$set$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Ord$set, $Set);
    void (*__serialize__)($Ord$set,$Serial$state);
    $Ord$set (*__deserialize__)($Ord$set,$Serial$state);
    $bool (*__bool__)($Ord$set);
    $str (*__str__)($Ord$set);
    $str (*__repr__)($Ord$set);
    $bool (*__eq__)($Ord$set, $set, $set);
    $bool (*__ne__)($Ord$set, $set, $set);
    $bool (*__lt__)($Ord$set, $set, $set);
    $bool (*__le__)($Ord$set, $set, $set);
    $bool (*__gt__)($Ord$set, $set, $set);
    $bool (*__ge__)($Ord$set, $set, $set);
};

void $Ord$set$__init__ ($Ord$set, $Set);
void $Ord$set$__serialize__($Ord$set, $Serial$state);
$Ord$set $Ord$set$__deserialize__($Ord$set, $Serial$state);
$bool $Ord$set$__eq__ ($Ord$set, $set, $set);
$bool $Ord$set$__ne__ ($Ord$set, $set, $set);
$bool $Ord$set$__lt__ ($Ord$set, $set, $set);
$bool $Ord$set$__le__ ($Ord$set, $set, $set);
$bool $Ord$set$__gt__ ($Ord$set, $set, $set);
$bool $Ord$set$__ge__ ($Ord$set, $set, $set);

// $Ord$Set$set ///////////////////////////////////////////////////////

#define $Ord$Set$set $Ord
#define $Ord$Set$set$class $Ord$set$class
#define $Ord$Set$set$methods $Ord$set$methods
#define $Ord$Set$set$new(...) $Ord$set$new(__VA_ARGS__)

// $Logical$Set$set ///////////////////////////////////////////////////

#define $Logical$Set$set $Logical$set
#define $Logical$Set$set$class $Logical$set$class
#define $Logical$Set$set$methods $Logical$set$methods
#define $Logical$Set$set$new(...) $Logical$set$new(__VA_ARGS__)

// $Minus$Set$set /////////////////////////////////////////////////////

#define $Minus$Set$set $Minus$set
#define $Minus$Set$set$class $Minus$set$class
#define $Minus$Set$set$methods $Minus$set$methods
#define $Minus$Set$set$new(...) $Minus$set$new(__VA_ARGS__)

// $Logical$set ///////////////////////////////////////////////////////

struct $Logical$set {
    $Logical$set$class $class;
    $Set w$Set;
};

struct $Logical$set$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Logical$set, $Set);
    void (*__serialize__)($Logical$set,$Serial$state);
    $Logical$set (*__deserialize__)($Logical$set,$Serial$state);
    $bool (*__bool__)($Logical$set);
    $str (*__str__)($Logical$set);
    $str (*__repr__)($Logical$set);
    $set (*__and__)($Logical$set, $set, $set);
    $set (*__or__)($Logical$set, $set, $set);
    $set (*__xor__)($Logical$set, $set, $set);
    $set (*__iand__)($Logical$set, $set, $set);
    $set (*__ior__)($Logical$set, $set, $set);
    $set (*__ixor__)($Logical$set, $set, $set);
};

void $Logical$set$__init__ ($Logical$set, $Set);
void $Logical$set$__serialize__($Logical$set, $Serial$state);
$Logical$set $Logical$set$__deserialize__($Logical$set, $Serial$state);
$set $Logical$set$__and__ ($Logical$set, $set, $set);
$set $Logical$set$__or__ ($Logical$set, $set, $set);
$set $Logical$set$__xor__ ($Logical$set, $set, $set);

// $Minus$set ////////////////////////////////////////////////////////////

struct $Minus$set {
    $Minus$set$class $class;
    $Set w$Set;
};

struct $Minus$set$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Minus$set, $Set);
    void (*__serialize__)($Minus$set,$Serial$state);
    $Minus$set (*__deserialize__)($Minus$set,$Serial$state);
    $bool (*__bool__)($Minus$set);
    $str (*__str__)($Minus$set);
    $str (*__repr__)($Minus$set);
    $set (*__sub__)($Minus$set, $set, $set);
    $set (*__isub__)($Minus$set, $set, $set);
};

void $Minus$set$__init__ ($Minus$set, $Set);
void $Minus$set$__serialize__($Minus$set, $Serial$state);
$Minus$set $Minus$set$__deserialize__($Minus$set, $Serial$state);
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
    void (*__serialize__)($Iterable$Iterator,$Serial$state);
    $Iterable$Iterator (*__deserialize__)($Iterable$Iterator,$Serial$state);
    $bool (*__bool__)($Iterable$Iterator);
    $str (*__str__)($Iterable$Iterator);
    $str (*__repr__)($Iterable$Iterator);
    $Iterator (*__iter__)($Iterable$Iterator, $Iterator);
};

void $Iterable$Iterator$__init__ ($Iterable$Iterator);
void $Iterable$Iterator$__serialize__($Iterable$Iterator, $Serial$state);
$Iterable$Iterator $Iterable$Iterator$__deserialize__($Iterable$Iterator, $Serial$state);
$Iterator $Iterable$Iterator$__iter__ ($Iterable$Iterator, $Iterator);

 
// $Ord$str ////////////////////////////////////////////////////////////

struct $Ord$str {
    $Ord$str$class $class;
};

struct $Ord$str$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Ord$str);
    void (*__serialize__)($Ord$str,$Serial$state);
    $Ord$str (*__deserialize__)($Ord$str,$Serial$state);
    $bool (*__bool__)($Ord$str);
    $str (*__str__)($Ord$str);
    $str (*__repr__)($Ord$str);
    $bool (*__eq__)($Ord$str, $str, $str);
    $bool (*__ne__)($Ord$str, $str, $str);
    $bool (*__lt__)($Ord$str, $str, $str);
    $bool (*__le__)($Ord$str, $str, $str);
    $bool (*__gt__)($Ord$str, $str, $str);
    $bool (*__ge__)($Ord$str, $str, $str);
};

void $Ord$str$__init__ ($Ord$str);
void $Ord$str$__serialize__($Ord$str, $Serial$state);
$Ord$str $Ord$str$__deserialize__($Ord$str, $Serial$state);
$bool $Ord$str$__eq__ ($Ord$str, $str, $str);
$bool $Ord$str$__ne__ ($Ord$str, $str, $str);
$bool $Ord$str$__lt__ ($Ord$str, $str, $str);
$bool $Ord$str$__le__ ($Ord$str, $str, $str);
$bool $Ord$str$__gt__ ($Ord$str, $str, $str);
$bool $Ord$str$__ge__ ($Ord$str, $str, $str);

// $Container$str ////////////////////////////////////////////////////////////

struct $Container$str {
    $Container$str$class $class;
};

struct $Container$str$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Container$str);
    void (*__serialize__)($Container$str,$Serial$state);
    $Container$str (*__deserialize__)($Container$str,$Serial$state);
    $bool (*__bool__)($Container$str);
    $str (*__str__)($Container$str);
    $str (*__repr__)($Container$str);
    $Iterator (*__iter__)($Container$str, $str);
    $str (*__fromiter__)($Container$str, $Iterable, $WORD);
    $int (*__len__)($Container$str, $str);
    $bool (*__contains__)($Container$str, $str, $str);
    $bool (*__containsnot__)($Container$str, $str, $str);
};

void $Container$str$__init__ ($Container$str);
void $Container$str$__serialize__($Container$str, $Serial$state);
$Container$str $Container$str$__deserialize__($Container$str, $Serial$state);
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
    void (*__serialize__)($Sliceable$str,$Serial$state);
    $Sliceable$str (*__deserialize__)($Sliceable$str,$Serial$state);
    $bool (*__bool__)($Sliceable$str);
    $str (*__str__)($Sliceable$str);
    $str (*__repr__)($Sliceable$str);
    $str (*__getitem__)($Sliceable$str, $str, $int);
    void (*__setitem__)($Sliceable$str, $str, $int, $str);
    void (*__delitem__)($Sliceable$str, $str, $int);
    $str (*__getslice__)($Sliceable$str, $str, $slice);
    void (*__setslice__)($Sliceable$str, $str, $Iterable, $slice, $WORD);
    void (*__delslice__)($Sliceable$str, $str, $slice);
};

void $Sliceable$str$__init__ ($Sliceable$str);
void $Sliceable$str$__serialize__($Sliceable$str, $Serial$state);
$Sliceable$str $Sliceable$str$__deserialize__($Sliceable$str, $Serial$state);
$str $Sliceable$str$__getitem__ ($Sliceable$str, $str, $int);
void $Sliceable$str$__setitem__ ($Sliceable$str, $str, $int, $str);
void $Sliceable$str$__delitem__ ($Sliceable$str, $str, $int);
$str $Sliceable$str$__getslice__ ($Sliceable$str, $str, $slice);
void $Sliceable$str$__setslice__ ($Sliceable$str, $str, $Iterable, $slice, $WORD);
void $Sliceable$str$__delslice__ ($Sliceable$str, $str, $slice);

// $Times$str ////////////////////////////////////////////////////////////

struct $Times$str {
    $Times$str$class $class;
};

struct $Times$str$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Times$str);
    void (*__serialize__)($Times$str,$Serial$state);
    $Times$str (*__deserialize__)($Times$str,$Serial$state);
    $bool (*__bool__)($Times$str);
    $str (*__str__)($Times$str);
    $str (*__repr__)($Times$str);
    $str (*__add__)($Times$str, $str, $str);
    $str (*__iadd__)($Times$str, $str, $str);
    $str (*__mul__)($Times$str, $str, $int);
    $str (*__imul__)($Times$str, $str, $int);
};

void $Times$str$__init__ ($Times$str);
void $Times$str$__serialize__($Times$str, $Serial$state);
$Times$str $Times$str$__deserialize__($Times$str, $Serial$state);
$bool $Times$str$__bool__($Times$str);
$str $Times$str$__str__($Times$str);
$str $Times$str$__add__ ($Times$str, $str, $str);
$str $Times$str$__mul__($Times$str, $str, $int);

// $Hashable$str ////////////////////////////////////////////////////////////

struct $Hashable$str {
    $Hashable$str$class $class;
};

struct $Hashable$str$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Hashable$str);
    void (*__serialize__)($Hashable$str,$Serial$state);
    $Hashable$str (*__deserialize__)($Hashable$str,$Serial$state);
    $bool (*__bool__)($Hashable$str);
    $str (*__str__)($Hashable$str);
    $str (*__repr__)($Hashable$str);
    $bool (*__eq__)($Hashable$str, $str, $str);
    $bool (*__ne__)($Hashable$str, $str, $str);
    $int (*__hash__)($Hashable$str, $str);
};

void $Hashable$str$__init__ ($Hashable$str);
void $Hashable$str$__serialize__($Hashable$str, $Serial$state);
$Hashable$str $Hashable$str$__deserialize__($Hashable$str, $Serial$state);
$bool $Hashable$str$__eq__ ($Hashable$str, $str, $str);
$bool $Hashable$str$__ne__ ($Hashable$str, $str, $str);
$int $Hashable$str$__hash__ ($Hashable$str, $str);

// $Integral$i64 ////////////////////////////////////////////////////////////

struct $Integral$i64 {
    $Integral$i64$class $class;
    $Minus w$Minus;
    $Logical w$Logical;
};

struct $Integral$i64$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Integral$i64);
    void (*__serialize__)($Integral$i64,$Serial$state);
    $Integral$i64 (*__deserialize__)($Integral$i64,$Serial$state);
    $bool (*__bool__)($Integral$i64);
    $str (*__str__)($Integral$i64);
    $str (*__repr__)($Integral$i64);
    $i64 (*__add__)($Integral$i64, $i64, $i64);
    $i64 (*__iadd__)($Integral$i64, $i64, $i64);
    $i64 (*__mul__)($Integral$i64, $i64, $i64);
    $i64 (*__imul__)($Integral$i64, $i64, $i64);
    $i64 (*__fromatom__)($Integral$i64,$atom);
    $complex (*__complx__)($Integral$i64, $i64);
    //    $i64 (*__truediv__)($Integral$i64, $i64, $i64);
    $i64 (*__pow__)($Integral$i64, $i64, $i64);
    //    $i64 (*__itruediv__)($Integral$i64, $i64, $i64);
    $i64 (*__ipow__)($Integral$i64, $i64, $i64);
    $i64 (*__neg__)($Integral$i64, $i64);
    $i64 (*__pos__)($Integral$i64, $i64);
    $WORD (*real)($Integral$i64, $i64, $Real);
    $WORD (*imag)($Integral$i64, $i64, $Real);
    $WORD (*__abs__)($Integral$i64, $i64, $Real);
    $i64 (*conjugate)($Integral$i64, $i64);
    $float (*__float__)($Integral$i64, $i64);
    $WORD (*__trunc__)($Integral$i64, $i64, $Integral);
    $WORD (*__floor__)($Integral$i64, $i64, $Integral);
    $WORD (*__ceil__)($Integral$i64, $i64, $Integral);
    $i64 (*__round__)($Integral$i64, $i64, $i64);
    $WORD (*numerator)($Integral$i64, $i64, $Integral);
    $WORD (*denominator)($Integral$i64, $i64, $Integral);
    $i64 (*__int__)($Integral$i64, $int);
    $i64 (*__index__)($Integral$i64, $int);
    $tuple (*__divmod__)($Integral$i64, $i64, $i64);
    $i64 (*__floordiv__)($Integral$i64, $i64, $i64);
    $i64 (*__mod__)($Integral$i64, $i64, $i64);
    $i64 (*__lshift__)($Integral$i64, $i64, $int);
    $i64 (*__rshift__)($Integral$i64, $i64, $int);
    $i64 (*__ifloordiv__)($Integral$i64, $i64, $i64);
    $i64 (*__imod__)($Integral$i64, $i64, $i64);
    $i64 (*__ilshift__)($Integral$i64, $i64, $int);
    $i64 (*__irshift__)($Integral$i64, $i64, $int);
    $i64 (*__invert__)($Integral$i64, $i64);
};

void $Integral$i64$__init__ ($Integral$i64);
void $Integral$i64$__serialize__($Integral$i64, $Serial$state);
$Integral$i64 $Integral$i64$__deserialize__($Integral$i64, $Serial$state);
$i64 $Integral$i64$__add__($Integral$i64, $i64, $i64);
$i64 $Integral$i64$__fromatom__($Integral$i64,$atom);
$complex $Integral$i64$__complx__($Integral$i64, $i64);
$i64 $Integral$i64$__mul__($Integral$i64, $i64, $i64);
//$i64 $Integral$i64$__truediv__($Integral$i64, $i64, $i64);
$i64 $Integral$i64$__pow__($Integral$i64, $i64, $i64);
$i64 $Integral$i64$__neg__($Integral$i64, $i64);
$i64 $Integral$i64$__pos__($Integral$i64, $i64);
$WORD $Integral$i64$real($Integral$i64, $i64, $Real);
$WORD $Integral$i64$imag($Integral$i64, $i64, $Real);
$WORD $Integral$i64$__abs__($Integral$i64, $i64, $Real);
$i64 $Integral$i64$conjugate($Integral$i64, $i64);
$float $Integral$i64$__float__ ($Integral$i64, $i64);
$WORD $Integral$i64$__trunc__ ($Integral$i64, $i64, $Integral);
$WORD $Integral$i64$__floor__ ($Integral$i64, $i64, $Integral);
$WORD $Integral$i64$__ceil__ ($Integral$i64, $i64, $Integral);
$i64 $Integral$i64$__round__ ($Integral$i64, $i64, $i64);
$WORD $Integral$i64$numerator ($Integral$i64, $i64, $Integral);
$WORD $Integral$i64$denominator ($Integral$i64, $i64, $Integral);
$i64 $Integral$i64$__int__ ($Integral$i64, $int);
$i64 $Integral$i64$__index__ ($Integral$i64, $int);
$tuple $Integral$i64$__divmod__ ($Integral$i64, $i64, $i64);
$i64 $Integral$i64$__floordiv__ ($Integral$i64, $i64, $i64);
$i64 $Integral$i64$__mod__ ($Integral$i64, $i64, $i64);
$i64 $Integral$i64$__lshift__ ($Integral$i64, $i64, $int);
$i64 $Integral$i64$__rshift__ ($Integral$i64, $i64, $int);
$i64 $Integral$i64$__invert__ ($Integral$i64, $i64);

// $Logical$i64 ////////////////////////////////////////////////////////////

struct $Logical$i64 {
    $Logical$i64$class $class;
    $Integral w$Integral;
};

struct $Logical$i64$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Logical$i64, $Integral);
    void (*__serialize__)($Logical$i64,$Serial$state);
    $Logical$i64 (*__deserialize__)($Logical$i64,$Serial$state);
    $bool (*__bool__)($Logical$i64);
    $str (*__str__)($Logical$i64);
    $str (*__repr__)($Logical$i64);
    $i64 (*__and__)($Logical$i64, $i64, $i64);
    $i64 (*__or__)($Logical$i64, $i64, $i64);
    $i64 (*__xor__)($Logical$i64, $i64, $i64);
    $i64 (*__iand__)($Logical$i64, $i64, $i64);
    $i64 (*__ior__)($Logical$i64, $i64, $i64);
    $i64 (*__ixor__)($Logical$i64, $i64, $i64);
};

void $Logical$i64$__init__ ($Logical$i64, $Integral);
void $Logical$i64$__serialize__($Logical$i64, $Serial$state);
$Logical$i64 $Logical$i64$__deserialize__($Logical$i64, $Serial$state);
$i64 $Logical$i64$__and__ ($Logical$i64, $i64, $i64);
$i64 $Logical$i64$__or__ ($Logical$i64, $i64, $i64);
$i64 $Logical$i64$__xor__ ($Logical$i64, $i64, $i64);

// $Minus$i64 ////////////////////////////////////////////////////////////

struct $Minus$i64 {
    $Minus$i64$class $class;
    $Integral w$Integral;
};

struct $Minus$i64$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Minus$i64, $Integral);
    void (*__serialize__)($Minus$i64,$Serial$state);
    $Minus$i64 (*__deserialize__)($Minus$i64,$Serial$state);
    $bool (*__bool__)($Minus$i64);
    $str (*__str__)($Minus$i64);
    $str (*__repr__)($Minus$i64);
    $i64 (*__sub__)($Minus$i64, $i64, $i64);
    $i64 (*__isub__)($Minus$i64, $i64, $i64);
};

void $Minus$i64$__init__ ($Minus$i64, $Integral);
void $Minus$i64$__serialize__($Minus$i64, $Serial$state);
$Minus$i64 $Minus$i64$__deserialize__($Minus$i64, $Serial$state);
$i64 $Minus$i64$__sub__ ($Minus$i64, $i64, $i64);

// $Div$i64 ////////////////////////////////////////////////////////////

struct $Div$i64 {
    $Div$i64$class $class;
};

struct $Div$i64$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Div$i64);
    void (*__serialize__)($Div$i64,$Serial$state);
    $Div$i64 (*__deserialize__)($Div$i64,$Serial$state);
    $bool (*__bool__)($Div$i64);
    $str (*__str__)($Div$i64);
    $str (*__repr__)($Div$i64);
    $float (*__truediv__)($Div$i64, $i64, $i64);
    $float (*__itruediv__)($Div$i64, $i64, $i64);
};

$float $Div$i64$__truediv__ ($Div$i64, $i64, $i64);
$float $Div$i64$__itruediv__ ($Div$i64, $i64, $i64);

// $Ord$i64 /////////////////////////////////////////////////////////////////

struct $Ord$i64 {
    $Ord$i64$class $class;
};

struct $Ord$i64$class {  
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Ord$i64);
    void (*__serialize__)($Ord$i64,$Serial$state);
    $Ord$i64 (*__deserialize__)($Ord$i64,$Serial$state);
    $bool (*__bool__)($Ord$i64);
    $str (*__str__)($Ord$i64);
    $str (*__repr__)($Ord$i64);
    $bool (*__eq__)($Ord$i64, $i64, $i64);
    $bool (*__ne__)($Ord$i64, $i64, $i64);
    $bool (*__lt__)($Ord$i64, $i64, $i64);
    $bool (*__le__)($Ord$i64, $i64, $i64);
    $bool (*__gt__)($Ord$i64, $i64, $i64);
    $bool (*__ge__)($Ord$i64, $i64, $i64);
};

void $Ord$i64$__init__ ($Ord$i64);
void $Ord$i64$__serialize__($Ord$i64, $Serial$state);
$Ord$i64 $Ord$i64$__deserialize__($Ord$i64, $Serial$state);
$bool $Ord$i64$__eq__ ($Ord$i64, $i64, $i64);
$bool $Ord$i64$__ne__ ($Ord$i64, $i64, $i64);
$bool $Ord$i64$__lt__ ($Ord$i64, $i64, $i64);
$bool $Ord$i64$__le__ ($Ord$i64, $i64, $i64);
$bool $Ord$i64$__gt__ ($Ord$i64, $i64, $i64);
$bool $Ord$i64$__ge__ ($Ord$i64, $i64, $i64);
  

// $Hashable$i64 ////////////////////////////////////////////////////////////

struct $Hashable$i64 {
    $Hashable$i64$class $class;
};

struct $Hashable$i64$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Hashable$i64);
    void (*__serialize__)($Hashable$i64,$Serial$state);
    $Hashable$i64 (*__deserialize__)($Hashable$i64,$Serial$state);
    $bool (*__bool__)($Hashable$i64);
    $str (*__str__)($Hashable$i64);
    $str (*__repr__)($Hashable$i64);
    $bool (*__eq__)($Hashable$i64, $i64, $i64);
    $bool (*__ne__)($Hashable$i64, $i64, $i64);
    $int (*__hash__)($Hashable$i64, $i64);
};

void $Hashable$i64$__init__ ($Hashable$i64);
void $Hashable$i64$__serialize__($Hashable$i64, $Serial$state);
$Hashable$i64 $Hashable$i64$__deserialize__($Hashable$i64, $Serial$state);
$bool $Hashable$i64$__eq__ ($Hashable$i64, $i64, $i64);
$bool $Hashable$i64$__ne__ ($Hashable$i64, $i64, $i64);
$int $Hashable$i64$__hash__ ($Hashable$i64, $i64);

// $Integral$int ////////////////////////////////////////////////////////////

struct $Integral$int {
    $Integral$int$class $class;
    $Minus w$Minus;
    $Logical w$Logical;
};

struct $Integral$int$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Integral$int);
    void (*__serialize__)($Integral$int,$Serial$state);
    $Integral$int (*__deserialize__)($Integral$int,$Serial$state);
    $bool (*__bool__)($Integral$int);
    $str (*__str__)($Integral$int);
    $str (*__repr__)($Integral$int);
    $int (*__add__)($Integral$int, $int, $int);
    $int (*__iadd__)($Integral$int, $int, $int);
    $int (*__mul__)($Integral$int, $int, $int);
    $int (*__imul__)($Integral$int, $int, $int);
    $int (*__fromatom__)($Integral$int,$atom);
    $complex (*__complx__)($Integral$int, $int);
    //    $int (*__truediv__)($Integral$int, $int, $int);
    $int (*__pow__)($Integral$int, $int, $int);
    //    $int (*__itruediv__)($Integral$int, $int, $int);
    $int (*__ipow__)($Integral$int, $int, $int);
    $int (*__neg__)($Integral$int, $int);
    $int (*__pos__)($Integral$int, $int);
    $WORD (*real)($Integral$int, $int, $Real);
    $WORD (*imag)($Integral$int, $int, $Real);
    $WORD (*__abs__)($Integral$int, $int, $Real);
    $int (*conjugate)($Integral$int, $int);
    $float (*__float__)($Integral$int, $int);
    $WORD (*__trunc__)($Integral$int, $int, $Integral);
    $WORD (*__floor__)($Integral$int, $int, $Integral);
    $WORD (*__ceil__)($Integral$int, $int, $Integral);
    $int (*__round__)($Integral$int, $int, $int);
    $WORD (*numerator)($Integral$int, $int, $Integral);
    $WORD (*denominator)($Integral$int, $int, $Integral);
    $int (*__int__)($Integral$int, $int);
    $int (*__index__)($Integral$int, $int);
    $tuple (*__divmod__)($Integral$int, $int, $int);
    $int (*__floordiv__)($Integral$int, $int, $int);
    $int (*__mod__)($Integral$int, $int, $int);
    $int (*__lshift__)($Integral$int, $int, $int);
    $int (*__rshift__)($Integral$int, $int, $int);
    $int (*__ifloordiv__)($Integral$int, $int, $int);
    $int (*__imod__)($Integral$int, $int, $int);
    $int (*__ilshift__)($Integral$int, $int, $int);
    $int (*__irshift__)($Integral$int, $int, $int);
    $int (*__invert__)($Integral$int, $int);
};

void $Integral$int$__init__ ($Integral$int);
void $Integral$int$__serialize__($Integral$int, $Serial$state);
$Integral$int $Integral$int$__deserialize__($Integral$int, $Serial$state);
$int $Integral$int$__add__($Integral$int, $int, $int);
$int $Integral$int$__fromatom__($Integral$int,$atom);
$complex $Integral$int$__complx__($Integral$int, $int);
$int $Integral$int$__mul__($Integral$int, $int, $int);
//$int $Integral$int$__truediv__($Integral$int, $int, $int);
$int $Integral$int$__pow__($Integral$int, $int, $int);
$int $Integral$int$__neg__($Integral$int, $int);
$int $Integral$int$__pos__($Integral$int, $int);
$WORD $Integral$int$real($Integral$int, $int, $Real);
$WORD $Integral$int$imag($Integral$int, $int, $Real);
$WORD $Integral$int$__abs__($Integral$int, $int, $Real);
$int $Integral$int$conjugate($Integral$int, $int);
$float $Integral$int$__float__ ($Integral$int, $int);
$WORD $Integral$int$__trunc__ ($Integral$int, $int, $Integral);
$WORD $Integral$int$__floor__ ($Integral$int, $int, $Integral);
$WORD $Integral$int$__ceil__ ($Integral$int, $int, $Integral);
$int $Integral$int$__round__ ($Integral$int, $int, $int);
$WORD $Integral$int$numerator ($Integral$int, $int, $Integral);
$WORD $Integral$int$denominator ($Integral$int, $int, $Integral);
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
    $Integral w$Integral;
};

struct $Logical$int$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Logical$int, $Integral);
    void (*__serialize__)($Logical$int,$Serial$state);
    $Logical$int (*__deserialize__)($Logical$int,$Serial$state);
    $bool (*__bool__)($Logical$int);
    $str (*__str__)($Logical$int);
    $str (*__repr__)($Logical$int);
    $int (*__and__)($Logical$int, $int, $int);
    $int (*__or__)($Logical$int, $int, $int);
    $int (*__xor__)($Logical$int, $int, $int);
    $int (*__iand__)($Logical$int, $int, $int);
    $int (*__ior__)($Logical$int, $int, $int);
    $int (*__ixor__)($Logical$int, $int, $int);
};

void $Logical$int$__init__ ($Logical$int, $Integral);
void $Logical$int$__serialize__($Logical$int, $Serial$state);
$Logical$int $Logical$int$__deserialize__($Logical$int, $Serial$state);
$int $Logical$int$__and__ ($Logical$int, $int, $int);
$int $Logical$int$__or__ ($Logical$int, $int, $int);
$int $Logical$int$__xor__ ($Logical$int, $int, $int);

// $Logical$Integral$int /////////////////////////////////////////////////

#define $Logical$Integral$int $Logical$int
#define $Logical$Integral$int$class $Logical$int$class
#define $Logical$Integral$int$methods $Logical$int$methods
#define $Logical$Integral$int$new(...) $Logical$int$new(__VA_ARGS__)

// $Minus$int ////////////////////////////////////////////////////////////

struct $Minus$int {
    $Minus$int$class $class;
    $Integral w$Integral;
};

struct $Minus$int$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Minus$int, $Integral);
    void (*__serialize__)($Minus$int,$Serial$state);
    $Minus$int (*__deserialize__)($Minus$int,$Serial$state);
    $bool (*__bool__)($Minus$int);
    $str (*__str__)($Minus$int);
    $str (*__repr__)($Minus$int);
    $int (*__sub__)($Minus$int, $int, $int);
    $int (*__isub__)($Minus$int, $int, $int);
};

void $Minus$int$__init__ ($Minus$int, $Integral);
void $Minus$int$__serialize__($Minus$int, $Serial$state);
$Minus$int $Minus$int$__deserialize__($Minus$int, $Serial$state);
$int $Minus$int$__sub__ ($Minus$int, $int, $int);

// $Minus$Integral$int ///////////////////////////////////////////////////////////

#define $Minus$Integral$int $Minus$int
#define $Minus$Integral$int$class $Minus$int$class
#define $Minus$Integral$int$methods $Minus$int$methods
#define $Minus$Integral$int$new(...) $Minus$int$new(__VA_ARGS__)

// $Div$int ////////////////////////////////////////////////////////////

struct $Div$int {
    $Div$int$class $class;
};

struct $Div$int$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Div$int);
    void (*__serialize__)($Div$int,$Serial$state);
    $Div$int (*__deserialize__)($Div$int,$Serial$state);
    $bool (*__bool__)($Div$int);
    $str (*__str__)($Div$int);
    $str (*__repr__)($Div$int);
    $float (*__truediv__)($Div$int, $int, $int);
    $float (*__itruediv__)($Div$int, $int, $int);
};

$float $Div$int$__truediv__ ($Div$int, $int, $int);
$float $Div$int$__itruediv__ ($Div$int, $int, $int);

// $Ord$int /////////////////////////////////////////////////////////////////

struct $Ord$int {
    $Ord$int$class $class;
};

struct $Ord$int$class {  
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Ord$int);
    void (*__serialize__)($Ord$int,$Serial$state);
    $Ord$int (*__deserialize__)($Ord$int,$Serial$state);
    $bool (*__bool__)($Ord$int);
    $str (*__str__)($Ord$int);
    $str (*__repr__)($Ord$int);
    $bool (*__eq__)($Ord$int, $int, $int);
    $bool (*__ne__)($Ord$int, $int, $int);
    $bool (*__lt__)($Ord$int, $int, $int);
    $bool (*__le__)($Ord$int, $int, $int);
    $bool (*__gt__)($Ord$int, $int, $int);
    $bool (*__ge__)($Ord$int, $int, $int);
};

void $Ord$int$__init__ ($Ord$int);
void $Ord$int$__serialize__($Ord$int, $Serial$state);
$Ord$int $Ord$int$__deserialize__($Ord$int, $Serial$state);
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
    void (*__serialize__)($Hashable$int,$Serial$state);
    $Hashable$int (*__deserialize__)($Hashable$int,$Serial$state);
    $bool (*__bool__)($Hashable$int);
    $str (*__str__)($Hashable$int);
    $str (*__repr__)($Hashable$int);
    $bool (*__eq__)($Hashable$int, $int, $int);
    $bool (*__ne__)($Hashable$int, $int, $int);
    $int (*__hash__)($Hashable$int, $int);
};

void $Hashable$int$__init__ ($Hashable$int);
void $Hashable$int$__serialize__($Hashable$int, $Serial$state);
$Hashable$int $Hashable$int$__deserialize__($Hashable$int, $Serial$state);
$bool $Hashable$int$__eq__ ($Hashable$int, $int, $int);
$bool $Hashable$int$__ne__ ($Hashable$int, $int, $int);
$int $Hashable$int$__hash__ ($Hashable$int, $int);

// $Real$float ////////////////////////////////////////////////////////////

struct $Real$float {
    $Real$float$class $class;
    $Minus w$Minus;
};

struct $Real$float$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Real$float);
    void (*__serialize__)($Real$float,$Serial$state);
    $Real$float (*__deserialize__)($Real$float,$Serial$state);
    $bool (*__bool__)($Real$float);
    $str (*__str__)($Real$float);
    $str (*__repr__)($Real$float);
    $float (*__add__)($Real$float, $float, $float);
    $float (*__iadd__)($Real$float, $float, $float);
    $float (*__mul__)($Real$float, $float, $float);
    $float (*__imul__)($Real$float, $float, $float);
    $float (*__fromatom__)($Real$float,$atom);
    $complex (*__complx__)($Real$float, $float);
    //    $float (*__truediv__)($Real$float, $float, $float);
    $float (*__pow__)($Real$float, $float, $float);
    //    $float (*__itruediv__)($Real$float, $float, $float);
    $float (*__ipow__)($Real$float, $float, $float);
    $float (*__neg__)($Real$float, $float);
    $float (*__pos__)($Real$float, $float);
    $WORD (*real)($Real$float, $float, $Real);
    $WORD (*imag)($Real$float, $float, $Real);
    $WORD (*__abs__)($Real$float, $float, $Real);
    $float (*conjugate)($Real$float, $float);
    $float (*__float__)($Real$float, $float);
    $WORD (*__trunc__)($Real$float, $float, $Integral);
    $WORD (*__floor__)($Real$float, $float, $Integral);
    $WORD (*__ceil__)($Real$float, $float, $Integral);
    $float (*__round__)($Real$float, $float, $int);
};

void $Real$float$__init__ ($Real$float);
void $Real$float$__serialize__($Real$float, $Serial$state);
$Real$float $Real$float$__deserialize__($Real$float, $Serial$state);
$float $Real$float$__add__($Real$float, $float, $float);
$float $Real$float$__fromatom__($Real$float,$atom);
$complex $Real$float$__complx__($Real$float, $float);
$float $Real$float$__mul__($Real$float, $float, $float);
//$float $Real$float$__truediv__($Real$float, $float, $float);
$float $Real$float$__pow__($Real$float, $float, $float);
$float $Real$float$__neg__($Real$float, $float);
$float $Real$float$__pos__($Real$float, $float);
$WORD $Real$float$real($Real$float, $float, $Real);
$WORD $Real$float$imag($Real$float, $float, $Real);
$WORD $Real$float$__abs__($Real$float, $float, $Real);
$float $Real$float$conjugate($Real$float, $float);
$float $Real$float$__float__ ($Real$float, $float);
$WORD $Real$float$__trunc__ ($Real$float, $float, $Integral);
$WORD $Real$float$__floor__ ($Real$float, $float, $Integral);
$WORD $Real$float$__ceil__ ($Real$float, $float, $Integral);
$float $Real$float$__round__ ($Real$float, $float, $int);

// $Div$float ////////////////////////////////////////////////////////////

struct $Div$float {
    $Div$float$class $class;
};

struct $Div$float$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Div$float);
    void (*__serialize__)($Div$float,$Serial$state);
    $Div$float (*__deserialize__)($Div$float,$Serial$state);
    $bool (*__bool__)($Div$float);
    $str (*__str__)($Div$float);
    $str (*__repr__)($Div$float);
    $float (*__truediv__)($Div$float, $float, $float);
    $float (*__itruediv__)($Div$float, $float, $float);
};

$float $Div$float$__truediv__ ($Div$float, $float, $float);
$float $Div$float$__itruediv__ ($Div$float, $float, $float);

// $Minus$float ////////////////////////////////////////////////////////////

struct $Minus$float {
    $Minus$float$class $class;
    $Real w$Real;
};

struct $Minus$float$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Minus$float, $Real);
    void (*__serialize__)($Minus$float,$Serial$state);
    $Minus$float (*__deserialize__)($Minus$float,$Serial$state);
    $bool (*__bool__)($Minus$float);
    $str (*__str__)($Minus$float);
    $str (*__repr__)($Minus$float);
    $float (*__sub__)($Minus$float, $float, $float);
    $float (*__isub__)($Minus$float, $float, $float);
};

void $Minus$float$__init__ ($Minus$float, $Real);
void $Minus$float$__serialize__($Minus$float, $Serial$state);
$Minus$float $Minus$float$__deserialize__($Minus$float, $Serial$state);
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
    void (*__serialize__)($Ord$float,$Serial$state);
    $Ord$float (*__deserialize__)($Ord$float,$Serial$state);
    $bool (*__bool__)($Ord$float);
    $str (*__str__)($Ord$float);
    $str (*__repr__)($Ord$float);
    $bool (*__eq__)($Ord$float, $float, $float);
    $bool (*__ne__)($Ord$float, $float, $float);
    $bool (*__lt__)($Ord$float, $float, $float);
    $bool (*__le__)($Ord$float, $float, $float);
    $bool (*__gt__)($Ord$float, $float, $float);
    $bool (*__ge__)($Ord$float, $float, $float);
};

void $Ord$float$__init__ ($Ord$float);
void $Ord$float$__serialize__($Ord$float, $Serial$state);
$Ord$float $Ord$float$__deserialize__($Ord$float, $Serial$state);
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
    void (*__serialize__)($Hashable$float,$Serial$state);
    $Hashable$float (*__deserialize__)($Hashable$float,$Serial$state);
    $bool (*__bool__)($Hashable$float);
    $str (*__str__)($Hashable$float);
    $str (*__repr__)($Hashable$float);
    $bool (*__eq__)($Hashable$float, $float, $float);
    $bool (*__ne__)($Hashable$float, $float, $float);
    $int (*__hash__)($Hashable$float, $float);
};

void $Hashable$float$__init__ ($Hashable$float);
void $Hashable$float$__serialize__($Hashable$float, $Serial$state);
$Hashable$float $Hashable$float$__deserialize__($Hashable$float, $Serial$state);
$bool $Hashable$float$__eq__ ($Hashable$float, $float, $float);
$bool $Hashable$float$__ne__ ($Hashable$float, $float, $float);
$int $Hashable$float$__hash__ ($Hashable$float, $float);

// $Number$complex ////////////////////////////////////////////////////////////

struct $Number$complex {
    $Number$complex$class $class;
    $Minus w$Minus;
};

struct $Number$complex$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Number$complex);
    void (*__serialize__)($Number$complex,$Serial$state);
    $Number$complex (*__deserialize__)($Number$complex,$Serial$state);
    $bool (*__bool__)($Number$complex);
    $str (*__str__)($Number$complex);
    $str (*__repr__)($Number$complex);
    $complex (*__add__)($Number$complex, $complex, $complex);
    $complex (*__iadd__)($Number$complex, $complex, $complex);
    $complex (*__mul__)($Number$complex, $complex, $complex);
    $complex (*__imul__)($Number$complex, $complex, $complex);
    $complex (*__fromatom__)($Number$complex,$atom);
    $complex (*__complx__)($Number$complex, $complex);
    //    $complex (*__truediv__)($Number$complex, $complex, $complex);
    $complex (*__pow__)($Number$complex, $complex, $complex);
    //    $complex (*__itruediv__)($Number$complex, $complex, $complex);
    $complex (*__ipow__)($Number$complex, $complex, $complex);
    $complex (*__neg__)($Number$complex, $complex);
    $complex (*__pos__)($Number$complex, $complex);
    $WORD (*real)($Number$complex, $complex, $Real);
    $WORD (*imag)($Number$complex, $complex, $Real);
    $WORD (*__abs__)($Number$complex, $complex, $Real);
    $complex (*conjugate)($Number$complex, $complex);
};

void $Number$complex$__init__ ($Number$complex);
void $Number$complex$__serialize__($Number$complex, $Serial$state);
$Number$complex $Number$complex$__deserialize__($Number$complex, $Serial$state);
$complex $Number$complex$__add__ ($Number$complex, $complex, $complex);
$complex $Number$complex$__fromatom__($Number$complex,$atom);
$complex $Number$complex$__complx__ ($Number$complex, $complex);
$complex $Number$complex$__mul__ ($Number$complex, $complex, $complex);
//$complex $Number$complex$__truediv__ ($Number$complex, $complex, $complex);
$complex $Number$complex$__pow__ ($Number$complex, $complex, $complex);
$complex $Number$complex$__neg__ ($Number$complex, $complex);
$complex $Number$complex$__pos__ ($Number$complex, $complex);
$WORD $Number$complex$real ($Number$complex, $complex, $Real);
$WORD $Number$complex$imag ($Number$complex, $complex, $Real);
$WORD $Number$complex$__abs__ ($Number$complex, $complex, $Real);
$complex $Number$complex$conjugate ($Number$complex, $complex);

// $Div$complex ////////////////////////////////////////////////////////////

struct $Div$complex {
    $Div$complex$class $class;
};

struct $Div$complex$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Div$complex);
    void (*__serialize__)($Div$complex,$Serial$state);
    $Div$complex (*__deserialize__)($Div$complex,$Serial$state);
    $bool (*__bool__)($Div$complex);
    $str (*__str__)($Div$complex);
    $str (*__repr__)($Div$complex);
    $complex (*__truediv__)($Div$complex, $complex, $complex);
    $complex (*__itruediv__)($Div$complex, $complex, $complex);
};

$complex $Div$complex$__truediv__ ($Div$complex, $complex, $complex);
$complex $Div$complex$__itruediv__ ($Div$complex, $complex, $complex);

// $Minus$complex ////////////////////////////////////////////////////////////

struct $Minus$complex {
    $Minus$complex$class $class;
    $Number w$Number;
};

struct $Minus$complex$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Minus$complex, $Number);
    void (*__serialize__)($Minus$complex,$Serial$state);
    $Minus$complex (*__deserialize__)($Minus$complex,$Serial$state);
    $bool (*__bool__)($Minus$complex);
    $str (*__str__)($Minus$complex);
    $str (*__repr__)($Minus$complex);
    $complex (*__sub__)($Minus$complex, $complex, $complex);
    $complex (*__isub__)($Minus$complex, $complex, $complex);
};

void $Minus$complex$__init__ ($Minus$complex, $Number);
void $Minus$complex$__serialize__($Minus$complex, $Serial$state);
$Minus$complex $Minus$complex$__deserialize__($Minus$complex, $Serial$state);
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
    void (*__serialize__)($Eq$complex,$Serial$state);
    $Eq$complex (*__deserialize__)($Eq$complex,$Serial$state);
    $bool (*__bool__)($Eq$complex);
    $str (*__str__)($Eq$complex);
    $str (*__repr__)($Eq$complex);
    $bool (*__eq__)($Eq$complex, $complex, $complex);
    $bool (*__ne__)($Eq$complex, $complex, $complex);
};

void $Eq$complex$__init__($Eq$complex);
void $Eq$complex$__serialize__($Eq$complex, $Serial$state);
$Eq$complex $Eq$complex$__deserialize__($Eq$complex, $Serial$state);
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
    void (*__serialize__)($Hashable$complex,$Serial$state);
    $Hashable$complex (*__deserialize__)($Hashable$complex,$Serial$state);
    $bool (*__bool__)($Hashable$complex);
    $str (*__str__)($Hashable$complex);
    $str (*__repr__)($Hashable$complex);
    $bool (*__eq__)($Hashable$complex, $complex, $complex);
    $bool (*__ne__)($Hashable$complex, $complex, $complex);
    $int (*__hash__)($Hashable$complex, $complex);
};

void $Hashable$complex$__init__ ($Hashable$complex);
void $Hashable$complex$__serialize__($Hashable$complex, $Serial$state);
$Hashable$complex $Hashable$complex$__deserialize__($Hashable$complex, $Serial$state);
$bool $Hashable$complex$__bool__($Hashable$complex);
$str $Hashable$complex$__str__($Hashable$complex);
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
    void (*__serialize__)($Iterable$range,$Serial$state);
    $Iterable$range (*__deserialize__)($Iterable$range,$Serial$state);
    $bool (*__bool__)($Iterable$range);
    $str (*__str__)($Iterable$range);
    $str (*__repr__)($Iterable$range);
    $Iterator (*__iter__)($Iterable$range, $range);
};

void $Iterable$range$__init__ ($Iterable$range);
void $Iterable$range$__serialize__($Iterable$range, $Serial$state);
$Iterable$range $Iterable$range$__deserialize__($Iterable$range, $Serial$state);
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
    void (*__serialize__)($Iterable$tuple,$Serial$state);
    $Iterable$tuple (*__deserialize__)($Iterable$tuple,$Serial$state);
    $bool (*__bool__)($Iterable$tuple);
    $str (*__str__)($Iterable$tuple);
    $str (*__repr__)($Iterable$tuple);
    $Iterator (*__iter__)($Iterable$tuple, $tuple);
};

void $Iterable$tuple$__init__ ($Iterable$tuple);
void $Iterable$tuple$__serialize__($Iterable$tuple, $Serial$state);
$Iterable$tuple $Iterable$tuple$__deserialize__($Iterable$tuple, $Serial$state);
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
    void (*__serialize__)($Sliceable$tuple,$Serial$state);
    $Sliceable$tuple (*__deserialize__)($Sliceable$tuple,$Serial$state);
    $bool (*__bool__)($Sliceable$tuple);
    $str (*__str__)($Sliceable$tuple);
    $str (*__repr__)($Sliceable$tuple);
    $WORD (*__getitem__)($Sliceable$tuple, $tuple, $int);
    void (*__setitem__)($Sliceable$tuple, $tuple, $int, $WORD);
    void (*__delitem__)($Sliceable$tuple, $tuple, $int);
    $tuple (*__getslice__)($Sliceable$tuple, $tuple, $slice);
    void (*__setslice__)($Sliceable$tuple, $tuple, $Iterable, $slice, $WORD);
    void (*__delslice__)($Sliceable$tuple, $tuple, $slice);
};

void $Sliceable$tuple$__init__ ($Sliceable$tuple);
void $Sliceable$tuple$__serialize__($Sliceable$tuple, $Serial$state);
$Sliceable$tuple $Sliceable$tuple$__deserialize__($Sliceable$tuple, $Serial$state);
$WORD $Sliceable$tuple$__getitem__ ($Sliceable$tuple, $tuple, $int);
void $Sliceable$tuple$__setitem__ ($Sliceable$tuple, $tuple, $int, $WORD);
void $Sliceable$tuple$__delitem__ ($Sliceable$tuple, $tuple, $int);
$tuple $Sliceable$tuple$__getslice__ ($Sliceable$tuple, $tuple, $slice);
void $Sliceable$tuple$__setslice__ ($Sliceable$tuple, $tuple, $Iterable, $slice, $WORD);
void $Sliceable$tuple$__delslice__ ($Sliceable$tuple, $tuple, $slice);

// $Hashable$tuple ////////////////////////////////////////////////////////////

struct $Hashable$tuple {
    $Hashable$tuple$class $class;
    int w$Hashable$tuple$size;
    $Hashable *w$Hashable;
};

struct $Hashable$tuple$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Hashable$tuple,int,$Hashable*);
    void (*__serialize__)($Hashable$tuple,$Serial$state);
    $Hashable$tuple (*__deserialize__)($Hashable$tuple,$Serial$state);
    $bool (*__bool__)($Hashable$tuple);
    $str (*__str__)($Hashable$tuple);
    $str (*__repr__)($Hashable$tuple);
    $bool (*__eq__)($Hashable$tuple, $tuple, $tuple);
    $bool (*__ne__)($Hashable$tuple, $tuple, $tuple);
    $int (*__hash__)($Hashable$tuple, $tuple);
};
  
void $Hashable$tuple$__init__ ($Hashable$tuple,int,$Hashable*);
void $Hashable$tuple$__serialize__($Hashable$tuple, $Serial$state);
$Hashable$tuple $Hashable$tuple$__deserialize__($Hashable$tuple, $Serial$state);
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
    void (*__serialize__)($Ord$bytearray,$Serial$state);
    $Ord$bytearray (*__deserialize__)($Ord$bytearray,$Serial$state);
    $bool (*__bool__)($Ord$bytearray);
    $str (*__str__)($Ord$bytearray);
    $str (*__repr__)($Ord$bytearray);
    $bool (*__eq__)($Ord$bytearray, $bytearray, $bytearray);
    $bool (*__ne__)($Ord$bytearray, $bytearray, $bytearray);
    $bool (*__lt__)($Ord$bytearray, $bytearray, $bytearray);
    $bool (*__le__)($Ord$bytearray, $bytearray, $bytearray);
    $bool (*__gt__)($Ord$bytearray, $bytearray, $bytearray);
    $bool (*__ge__)($Ord$bytearray, $bytearray, $bytearray);
};

void $Ord$bytearray$__init__ ($Ord$bytearray);
void $Ord$bytearray$__serialize__($Ord$bytearray, $Serial$state);
$Ord$bytearray $Ord$bytearray$__deserialize__($Ord$bytearray, $Serial$state);
$bool $Ord$bytearray$__bool__($Ord$bytearray);
$str $Ord$bytearray$__str__($Ord$bytearray);
$bool $Ord$bytearray$__eq__ ($Ord$bytearray, $bytearray, $bytearray);
$bool $Ord$bytearray$__ne__ ($Ord$bytearray, $bytearray, $bytearray);
$bool $Ord$bytearray$__lt__ ($Ord$bytearray, $bytearray, $bytearray);
$bool $Ord$bytearray$__le__ ($Ord$bytearray, $bytearray, $bytearray);
$bool $Ord$bytearray$__gt__ ($Ord$bytearray, $bytearray, $bytearray);
$bool $Ord$bytearray$__ge__ ($Ord$bytearray, $bytearray, $bytearray);

// $Sequence$bytearray ////////////////////////////////////////////////////////////

struct $Sequence$bytearray {
    $Sequence$bytearray$class $class;
    $Collection w$Collection;
    $Times w$Times;
};

struct $Sequence$bytearray$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Sequence$bytearray);
    void (*__serialize__)($Sequence$bytearray,$Serial$state);
    $Sequence$bytearray (*__deserialize__)($Sequence$bytearray,$Serial$state);
    $bool (*__bool__)($Sequence$bytearray);
    $str (*__str__)($Sequence$bytearray);
    $str (*__repr__)($Sequence$bytearray);
    $int (*__getitem__)($Sequence$bytearray, $bytearray, $int);
    void (*__setitem__)($Sequence$bytearray, $bytearray, $int, $int);
    void (*__delitem__)($Sequence$bytearray, $bytearray, $int);
    $bytearray (*__getslice__)($Sequence$bytearray, $bytearray, $slice);
    void (*__setslice__)($Sequence$bytearray, $bytearray, $Iterable, $slice, $WORD);
    void (*__delslice__)($Sequence$bytearray, $bytearray, $slice);
    $Iterator (*__reversed__)($Sequence$bytearray, $bytearray);
    void (*insert)($Sequence$bytearray, $bytearray, $int, $int);
    void (*append)($Sequence$bytearray, $bytearray, $int);
    void (*reverse)($Sequence$bytearray, $bytearray);
};

void $Sequence$bytearray$__init__ ($Sequence$bytearray);
void $Sequence$bytearray$__serialize__($Sequence$bytearray, $Serial$state);
$Sequence$bytearray $Sequence$bytearray$__deserialize__($Sequence$bytearray, $Serial$state);
$bool $Sequence$bytearray$__bool__($Sequence$bytearray);
$str $Sequence$bytearray$__str__($Sequence$bytearray);
$int $Sequence$bytearray$__getitem__ ($Sequence$bytearray, $bytearray, $int);
void $Sequence$bytearray$__setitem__ ($Sequence$bytearray, $bytearray, $int, $int);
void $Sequence$bytearray$__delitem__ ($Sequence$bytearray, $bytearray, $int);
$bytearray $Sequence$bytearray$__getslice__ ($Sequence$bytearray, $bytearray, $slice);
void $Sequence$bytearray$__setslice__ ($Sequence$bytearray, $bytearray, $Iterable, $slice, $WORD);
void $Sequence$bytearray$__delslice__ ($Sequence$bytearray, $bytearray, $slice);
$Iterator $Sequence$bytearray$__reversed__ ($Sequence$bytearray, $bytearray);
void $Sequence$bytearray$insert ($Sequence$bytearray, $bytearray, $int, $int);
void $Sequence$bytearray$append ($Sequence$bytearray, $bytearray, $int);
void $Sequence$bytearray$reverse ($Sequence$bytearray, $bytearray);

// $Collection$bytearray ////////////////////////////////////////////////////////////

struct $Collection$bytearray {
    $Collection$bytearray$class $class;
    $Sequence w$Sequence;
};

struct $Collection$bytearray$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Collection$bytearray, $Sequence);
    void (*__serialize__)($Collection$bytearray,$Serial$state);
    $Collection$bytearray (*__deserialize__)($Collection$bytearray,$Serial$state);
    $bool (*__bool__)($Collection$bytearray);
    $str (*__str__)($Collection$bytearray);
    $str (*__repr__)($Collection$bytearray);
    $Iterator (*__iter__)($Collection$bytearray, $bytearray);
    $bytearray (*__fromiter__)($Collection$bytearray, $Iterable, $WORD);
    $int (*__len__)($Collection$bytearray, $bytearray);
};

void $Collection$bytearray$__init__ ($Collection$bytearray, $Sequence);
void $Collection$bytearray$__serialize__($Collection$bytearray, $Serial$state);
$Collection$bytearray $Collection$bytearray$__deserialize__($Collection$bytearray, $Serial$state);
$bool $Collection$bytearray$__bool__($Collection$bytearray);
$str $Collection$bytearray$__str__($Collection$bytearray);
$Iterator $Collection$bytearray$__iter__ ($Collection$bytearray, $bytearray);
$bytearray $Collection$bytearray$__fromiter__ ($Collection$bytearray, $Iterable, $WORD);
$int $Collection$bytearray$__len__ ($Collection$bytearray, $bytearray);

// $Collection$Sequence$bytearray //////////////////////////////////////////////

#define $Collection$Sequence$bytearray $Collection$bytearray
#define $Collection$Sequence$bytearray$class $Collection$bytearray$class
#define $Collection$Sequence$bytearray$methods $Collection$bytearray$methods
#define $Collection$Sequence$bytearray$new(...) $Collection$bytearray$new(__VA_ARGS__)

// $Times$bytearray ////////////////////////////////////////////////////////////

struct $Times$bytearray {
    $Times$bytearray$class $class;
    $Sequence w$Sequence;
};

struct $Times$bytearray$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Times$bytearray, $Sequence);
    void (*__serialize__)($Times$bytearray,$Serial$state);
    $Times$bytearray (*__deserialize__)($Times$bytearray,$Serial$state);
    $bool (*__bool__)($Times$bytearray);
    $str (*__str__)($Times$bytearray);
    $str (*__repr__)($Times$bytearray);
    $bytearray (*__add__)($Times$bytearray, $bytearray, $bytearray);
    $bytearray (*__iadd__)($Times$bytearray, $bytearray, $bytearray);
    $bytearray (*__mul__)($Times$bytearray, $bytearray, $int);
    $bytearray (*__imul__)($Times$bytearray, $bytearray, $int);
};

void $Times$bytearray$__init__ ($Times$bytearray, $Sequence);
void $Times$bytearray$__serialize__($Times$bytearray, $Serial$state);
$Times$bytearray $Times$bytearray$__deserialize__($Times$bytearray, $Serial$state);
$bool $Times$bytearray$__bool__($Times$bytearray);
$str $Times$bytearray$__str__($Times$bytearray);
$bytearray $Times$bytearray$__add__ ($Times$bytearray, $bytearray, $bytearray);
$bytearray $Times$bytearray$__mul__ ($Times$bytearray, $bytearray, $int);

// $Times$Sequence$bytearray ///////////////////////////////////////////////////////

#define $Times$Sequence$bytearray $Times$bytearray
#define $Times$Sequence$bytearray$class $Times$bytearray$class
#define $Times$Sequence$bytearray$methods $Times$bytearray$methods
#define $Times$Sequence$bytearray$new(...) $Times$bytearray$new(__VA_ARGS__)

// $Container$bytearray ////////////////////////////////////////////////////////////

struct $Container$bytearray {
    $Container$bytearray$class $class;
};

struct $Container$bytearray$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Container$bytearray);
    void (*__serialize__)($Container$bytearray,$Serial$state);
    $Container$bytearray (*__deserialize__)($Container$bytearray,$Serial$state);
    $bool (*__bool__)($Container$bytearray);
    $str (*__str__)($Container$bytearray);
    $str (*__repr__)($Container$bytearray);
    $Iterator (*__iter__)($Container$bytearray, $bytearray);
    $int (*__len__)($Container$bytearray, $bytearray);
    $bool (*__contains__)($Container$bytearray, $bytearray, $int);
    $bool (*__containsnot__)($Container$bytearray, $bytearray, $int);
};

void $Container$bytearray$__init__ ($Container$bytearray);
void $Container$bytearray$__serialize__($Container$bytearray, $Serial$state);
$Container$bytearray $Container$bytearray$__deserialize__($Container$bytearray, $Serial$state);
$bool $Container$bytearray$__bool__($Container$bytearray);
$str $Container$bytearray$__str__($Container$bytearray);
$Iterator $Container$bytearray$__iter__ ($Container$bytearray, $bytearray);
$int $Container$bytearray$__len__ ($Container$bytearray, $bytearray);
$bool $Container$bytearray$__contains__ ($Container$bytearray, $bytearray, $int);
$bool $Container$bytearray$__containsnot__ ($Container$bytearray, $bytearray, $int);

// $Ord$bytes ////////////////////////////////////////////////////////////

struct $Ord$bytes {
    $Ord$bytes$class $class;
};

struct $Ord$bytes$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Ord$bytes);
    void (*__serialize__)($Ord$bytes,$Serial$state);
    $Ord$bytes (*__deserialize__)($Ord$bytes,$Serial$state);
    $bool (*__bool__)($Ord$bytes);
    $str (*__str__)($Ord$bytes);
    $str (*__repr__)($Ord$bytes);
    $bool (*__eq__)($Ord$bytes, $bytes, $bytes);
    $bool (*__ne__)($Ord$bytes, $bytes, $bytes);
    $bool (*__lt__)($Ord$bytes, $bytes, $bytes);
    $bool (*__le__)($Ord$bytes, $bytes, $bytes);
    $bool (*__gt__)($Ord$bytes, $bytes, $bytes);
    $bool (*__ge__)($Ord$bytes, $bytes, $bytes);
};

void $Ord$bytes$__init__ ($Ord$bytes);
void $Ord$bytes$__serialize__($Ord$bytes, $Serial$state);
$Ord$bytes $Ord$bytes$__deserialize__($Ord$bytes, $Serial$state);
$bool $Ord$bytes$__eq__ ($Ord$bytes, $bytes, $bytes);
$bool $Ord$bytes$__ne__ ($Ord$bytes, $bytes, $bytes);
$bool $Ord$bytes$__lt__ ($Ord$bytes, $bytes, $bytes);
$bool $Ord$bytes$__le__ ($Ord$bytes, $bytes, $bytes);
$bool $Ord$bytes$__gt__ ($Ord$bytes, $bytes, $bytes);
$bool $Ord$bytes$__ge__ ($Ord$bytes, $bytes, $bytes);

// $Container$bytes ////////////////////////////////////////////////////////////

struct $Container$bytes {
    $Container$bytes$class $class;
};

struct $Container$bytes$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Container$bytes);
    void (*__serialize__)($Container$bytes,$Serial$state);
    $Container$bytes (*__deserialize__)($Container$bytes,$Serial$state);
    $bool (*__bool__)($Container$bytes);
    $str (*__str__)($Container$bytes);
    $str (*__repr__)($Container$bytes);
    $Iterator (*__iter__)($Container$bytes, $bytes);
    $bytes (*__fromiter__)($Container$bytes, $Iterable, $WORD);
    $int (*__len__)($Container$bytes, $bytes);
    $bool (*__contains__)($Container$bytes, $bytes, $bytes);
    $bool (*__containsnot__)($Container$bytes, $bytes, $bytes);
};

void $Container$bytes$__init__ ($Container$bytes);
void $Container$bytes$__serialize__($Container$bytes, $Serial$state);
$Container$bytes $Container$bytes$__deserialize__($Container$bytes, $Serial$state);
$Iterator $Container$bytes$__iter__ ($Container$bytes, $bytes);
$int $Container$bytes$__len__ ($Container$bytes, $bytes);
$bool $Container$bytes$__contains__ ($Container$bytes, $bytes, $bytes);
$bool $Container$bytes$__containsnot__ ($Container$bytes, $bytes, $bytes);

// $Sliceable$bytes ////////////////////////////////////////////////////////////

struct $Sliceable$bytes {
    $Sliceable$bytes$class $class;
};

struct $Sliceable$bytes$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Sliceable$bytes);
    void (*__serialize__)($Sliceable$bytes,$Serial$state);
    $Sliceable$bytes (*__deserialize__)($Sliceable$bytes,$Serial$state);
    $bool (*__bool__)($Sliceable$bytes);
    $str (*__str__)($Sliceable$bytes);
    $str (*__repr__)($Sliceable$bytes);
    $int (*__getitem__)($Sliceable$bytes, $bytes, $int);
    void (*__setitem__)($Sliceable$bytes, $bytes, $int, $bytes);
    void (*__delitem__)($Sliceable$bytes, $bytes, $int);
    $bytes (*__getslice__)($Sliceable$bytes, $bytes, $slice);
    void (*__setslice__)($Sliceable$bytes, $bytes, $Iterable, $slice, $WORD);
    void (*__delslice__)($Sliceable$bytes, $bytes, $slice);
};

void $Sliceable$bytes$__init__ ($Sliceable$bytes);
void $Sliceable$bytes$__serialize__($Sliceable$bytes, $Serial$state);
$Sliceable$bytes $Sliceable$bytes$__deserialize__($Sliceable$bytes, $Serial$state);
$int $Sliceable$bytes$__getitem__ ($Sliceable$bytes, $bytes, $int);
void $Sliceable$bytes$__setitem__ ($Sliceable$bytes, $bytes, $int, $bytes);
void $Sliceable$bytes$__delitem__ ($Sliceable$bytes, $bytes, $int);
$bytes $Sliceable$bytes$__getslice__ ($Sliceable$bytes, $bytes, $slice);
void $Sliceable$bytes$__setslice__ ($Sliceable$bytes, $bytes, $Iterable, $slice, $WORD);
void $Sliceable$bytes$__delslice__ ($Sliceable$bytes, $bytes, $slice);

// $Times$bytes ////////////////////////////////////////////////////////////

struct $Times$bytes {
    $Times$bytes$class $class;
};

struct $Times$bytes$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Times$bytes);
    void (*__serialize__)($Times$bytes,$Serial$state);
    $Times$bytes (*__deserialize__)($Times$bytes,$Serial$state);
    $bool (*__bool__)($Times$bytes);
    $str (*__str__)($Times$bytes);
    $str (*__repr__)($Times$bytes);
    $bytes (*__add__)($Times$bytes, $bytes, $bytes);
    $bytes (*__iadd__)($Times$bytes, $bytes, $bytes);
    $bytes (*__mul__)($Times$bytes, $bytes, $int);
    $bytes (*__imul__)($Times$bytes, $bytes, $int);
};

void $Times$bytes$__init__ ($Times$bytes);
void $Times$bytes$__serialize__($Times$bytes, $Serial$state);
$Times$bytes $Times$bytes$__deserialize__($Times$bytes, $Serial$state);
$bool $Times$bytes$__bool__($Times$bytes);
$bytes $Times$bytes$__str__($Times$bytes);
$bytes $Times$bytes$__add__ ($Times$bytes, $bytes, $bytes);
$bytes $Times$bytes$__mul__($Times$bytes, $bytes, $int);

// $Hashable$bytes ////////////////////////////////////////////////////////////

struct $Hashable$bytes {
    $Hashable$bytes$class $class;
};

struct $Hashable$bytes$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Hashable$bytes);
    void (*__serialize__)($Hashable$bytes,$Serial$state);
    $Hashable$bytes (*__deserialize__)($Hashable$bytes,$Serial$state);
    $bool (*__bool__)($Hashable$bytes);
    $str (*__str__)($Hashable$bytes);
    $str (*__repr__)($Hashable$bytes);
    $bool (*__eq__)($Hashable$bytes, $bytes, $bytes);
    $bool (*__ne__)($Hashable$bytes, $bytes, $bytes);
    $int (*__hash__)($Hashable$bytes, $bytes);
};

void $Hashable$bytes$__init__ ($Hashable$bytes);
void $Hashable$bytes$__serialize__($Hashable$bytes, $Serial$state);
$Hashable$bytes $Hashable$bytes$__deserialize__($Hashable$bytes, $Serial$state);
$bool $Hashable$bytes$__eq__ ($Hashable$bytes, $bytes, $bytes);
$bool $Hashable$bytes$__ne__ ($Hashable$bytes, $bytes, $bytes);
$int $Hashable$bytes$__hash__ ($Hashable$bytes, $bytes);

void $register_builtin_protocols();
