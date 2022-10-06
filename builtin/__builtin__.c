/*
 * Copyright (C) 2019-2021 Data Ductus AB
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "builtin.h"

$WORD $Eq$__ne__($Eq wit, $WORD a, $WORD b) {
  return to$bool(!wit->$class->__eq__(wit,a,b)->val);
}

$WORD $Ord$__gt__($Ord wit, $WORD a, $WORD b) {
  return wit->$class->__lt__(wit,b,a);
}

$WORD $Ord$__ge__($Ord wit, $WORD a, $WORD b) {
  return wit->$class->__le__(wit,b,a);
}

$WORD $Plus$__iadd__ ($Plus wit, $WORD a, $WORD b) {
    return wit->$class->__add__(wit, a, b);
}

$WORD $Minus$__isub__ ($Minus wit, $WORD a, $WORD b) {
    return wit->$class->__sub__(wit, a, b);
}

$WORD $Logical$__iand__ ($Logical wit, $WORD a, $WORD b) {
    return wit->$class->__and__(wit, a, b);
}

$WORD $Logical$__ior__ ($Logical wit, $WORD a, $WORD b) {
    return wit->$class->__or__(wit, a, b);
}

$WORD $Logical$__ixor__ ($Logical wit, $WORD a, $WORD b) {
    return wit->$class->__xor__(wit, a, b);
}

$WORD $Times$__imul__ ($Times wit, $WORD a, $WORD b) {
    return wit->$class->__mul__(wit, a, b);
}

$WORD $Div$__itruediv__ ($Div wit, $WORD a, $WORD b) {
    return wit->$class->__truediv__(wit, a, b);
}

$WORD $Number$__ipow__ ($Number wit, $WORD a, $WORD b) {
    return wit->$class->__pow__(wit, a, b);
}

$WORD $Integral$__ifloordiv__ ($Integral wit, $WORD a, $WORD b) {
    return wit->$class->__floordiv__(wit, a, b);
}

$WORD $Integral$__imod__ ($Integral wit, $WORD a, $WORD b) {
    return wit->$class->__mod__(wit, a, b);
}

$WORD $Integral$__ilshift__ ($Integral wit, $WORD a, $int b) {
    return wit->$class->__lshift__(wit, a, b);
}

$WORD $Integral$__irshift__ ($Integral wit, $WORD a, $int b) {
    return wit->$class->__rshift__(wit, a, b);
}

struct $Eq$class $Eq$methods = {"$Eq$class", UNASSIGNED, NULL, (void (*)($Eq))$default__init__, NULL, NULL, ($bool (*)($Eq))$default__bool__,  ($str (*)($Eq))$default__str__, ($str (*)($Eq))$default__str__,
                                NULL, NULL};

$Eq $Eq$new() {
  $Eq res = GC_MALLOC(sizeof(struct $Eq));
  res->$class = &$Eq$methods;
  return res;
}

struct $Ord$class $Ord$methods = {"$Ord$class", UNASSIGNED, ($Super$class)&$Eq$methods, (void (*)($Ord))$default__init__, NULL, NULL, ($bool (*)($Ord))$default__bool__,  ($str (*)($Ord))$default__str__, ($str (*)($Ord))$default__str__,
                                  NULL, NULL, NULL, NULL, NULL, NULL};

$Ord $Ord$new() {
  $Ord res = GC_MALLOC(sizeof(struct $Eq));
  res->$class = &$Ord$methods;
  return res;
}

struct $Logical$class $Logical$methods = {"$Logical$class", UNASSIGNED, NULL, (void (*)($Logical))$default__init__, NULL, NULL, ($bool (*)($Logical))$default__bool__,  ($str (*)($Logical))$default__str__, ($str (*)($Logical))$default__str__,
                                          NULL, NULL, NULL};

$Logical $Logical$new() {
  $Logical res = GC_MALLOC(sizeof(struct $Logical));
  res->$class = &$Logical$methods;
  return res;
}

struct $Plus$class $Plus$methods = {"$Plus$class", UNASSIGNED, NULL, (void (*)($Plus))$default__init__, NULL, NULL, ($bool (*)($Plus))$default__bool__,  ($str (*)($Plus))$default__str__, ($str (*)($Plus))$default__str__,
                                    NULL, $Plus$__iadd__};

$Plus $Plus$new() {
  $Plus res = GC_MALLOC(sizeof(struct $Plus));
  res->$class = &$Plus$methods;
  return res;
}

struct $Times$class $Times$methods = {"$Times$class", UNASSIGNED, NULL, (void (*)($Times))$default__init__, NULL, NULL, ($bool (*)($Times))$default__bool__,  ($str (*)($Times))$default__str__, ($str (*)($Times))$default__str__,
                                      NULL, ($WORD (*)($Times,$WORD,$WORD))$Plus$__iadd__, $Times$__imul__};

$Times $Times$new() {
  $Times res = GC_MALLOC(sizeof(struct $Times));
  res->$class = &$Times$methods;
  return res;
}

struct $Div$class $Div$methods = {"$Div$class", UNASSIGNED, NULL, (void (*)($Div))$default__init__, NULL, NULL, ($bool (*)($Div))$default__bool__,  ($str (*)($Div))$default__str__, ($str (*)($Div))$default__str__,
                                    NULL, $Div$__itruediv__};

$Div $Div$new() {
  $Div res = GC_MALLOC(sizeof(struct $Div));
  res->$class = &$Div$methods;
  return res;
}

struct $Minus$class $Minus$methods = {"$Minus$class", UNASSIGNED, NULL, (void (*)($Minus))$default__init__, NULL, NULL, ($bool (*)($Minus))$default__bool__,  ($str (*)($Minus))$default__str__, ($str (*)($Minus))$default__str__,
                                      NULL, $Minus$__isub__};

$Minus $Minus$new() {
  $Minus res = GC_MALLOC(sizeof(struct $Minus));
  res->$class = &$Minus$methods;
  return res;
}

struct $Hashable$class $Hashable$methods = {"$Hashable$class", UNASSIGNED, NULL, (void (*)($Hashable))$default__init__, NULL, NULL, ($bool (*)($Hashable))$default__bool__,  ($str (*)($Hashable))$default__str__, ($str (*)($Hashable))$default__str__,
                                            NULL, NULL, NULL};

$Hashable $Hashable$new() {
  $Hashable res = GC_MALLOC(sizeof(struct $Hashable));
  res->$class = &$Hashable$methods;
  return res;
}

static void $Indexed$__init__($Indexed self, $Eq w$Eq$A$Indexed) {
  self->w$Eq$A$Indexed = w$Eq$A$Indexed;
}

struct $Indexed$class $Indexed$methods = {"$Indexed$class", UNASSIGNED, NULL, $Indexed$__init__, NULL, NULL, ($bool (*)($Indexed))$default__bool__,  ($str (*)($Indexed))$default__str__, ($str (*)($Indexed))$default__str__, 
                                          NULL, NULL, NULL};

$Indexed $Indexed$new($Eq w$Eq$A$Indexed) {
  $Indexed res = GC_MALLOC(sizeof(struct $Indexed));
  res->$class = &$Indexed$methods;
  res->w$Eq$A$Indexed = w$Eq$A$Indexed;
  return res;
}

struct $Sliceable$class $Sliceable$methods = {"$Sliceable$class", UNASSIGNED, ($Super$class)&$Indexed$methods, (void (*)($Sliceable))$default__init__, NULL, NULL, ($bool (*)($Sliceable))$default__bool__,  ($str (*)($Sliceable))$default__str__,($str (*)($Sliceable))$default__str__,
                                              NULL, NULL, NULL, NULL, NULL, NULL};

$Sliceable $Sliceable$new() {
  $Sliceable res = GC_MALLOC(sizeof(struct $Sliceable));
  res->$class = &$Sliceable$methods;
  return res;
}

struct $Iterable$class $Iterable$methods = {"$Iterable$class", UNASSIGNED, NULL, (void (*)($Iterable))$default__init__, NULL, NULL, ($bool (*)($Iterable))$default__bool__,  ($str (*)($Iterable))$default__str__,($str (*)($Iterable))$default__str__,
                                            NULL};

$Iterable $Iterable$new() {
  $Iterable res = GC_MALLOC(sizeof(struct $Iterable));
  res->$class = &$Iterable$methods;
  return res;
}

struct $Collection$class $Collection$methods = {"$Collection$class", UNASSIGNED, ($Super$class)&$Iterable$methods, (void (*)($Collection))$default__init__, NULL, NULL, ($bool (*)($Collection))$default__bool__,  ($str (*)($Collection))$default__str__, ($str (*)($Collection))$default__str__,
                                                NULL, NULL, NULL};

$Collection $Collection$new() {
  $Collection res = GC_MALLOC(sizeof(struct $Collection));
  res->$class = &$Collection$methods;
  return res;
}

static void $Container$__init__($Container self, $Eq w$Eq$A$Container) {
  self->w$Eq$A$Container = w$Eq$A$Container;
}

struct $Container$class $Container$methods = {"$Container$class", UNASSIGNED, ($Super$class)&$Collection$methods, $Container$__init__, NULL, NULL, ($bool (*)($Container))$default__bool__,  ($str (*)($Container))$default__str__, ($str (*)($Container))$default__str__,
                                              NULL, NULL, NULL, NULL, NULL};

$Container $Container$new($Eq w$Eq$A$Container) {
  $Container res = GC_MALLOC(sizeof(struct $Container));
  res->$class = &$Container$methods;
  res->w$Eq$A$Container = w$Eq$A$Container;
  return res;
}

static void $Sequence$__init__($Sequence self) {
  self->w$Collection = $Collection$new();
  self->w$Times = $Times$new();
}

struct $Sequence$class $Sequence$methods = {"$Sequence$class", UNASSIGNED, ($Super$class)&$Sliceable$methods, $Sequence$__init__, NULL, NULL, ($bool (*)($Sequence))$default__bool__,  ($str (*)($Sequence))$default__str__,($str (*)($Sequence))$default__str__,
                                            NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL};

$Sequence $Sequence$new() {
  $Sequence res = GC_MALLOC(sizeof(struct $Sequence));
  res->$class = &$Sequence$methods;
  res->w$Collection = $Collection$new();
  res->w$Times = $Times$new();
  return res;
}

void $Mapping$__init__($Mapping self, $Eq w$Eq$A$Mapping) {
  self->w$Indexed = $Indexed$new(w$Eq$A$Mapping);
  self->w$Eq$A$Mapping = w$Eq$A$Mapping;
}

struct $Mapping$class $Mapping$methods = {"$Mapping$class", UNASSIGNED, ($Super$class)&$Container$methods, $Mapping$__init__, NULL, NULL, ($bool (*)($Mapping))$default__bool__,  ($str (*)($Mapping))$default__str__, ($str (*)($Mapping))$default__str__,
                                          NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL};

$Mapping $Mapping$new($Eq w$Eq$A$Mapping) {
  $Mapping res = GC_MALLOC(sizeof(struct $Mapping));
  res->$class = &$Mapping$methods;
  res->w$Indexed = $Indexed$new(w$Eq$A$Mapping);
  res->w$Eq$A$Mapping = w$Eq$A$Mapping;
  return res;
}

void $Set$__init__($Set self, $Eq w$Eq$A$Set) {
  self->w$Ord = $Ord$new();
  self->w$Logical = $Logical$new();
  self->w$Minus = $Minus$new();
  self->w$Eq$A$Set = w$Eq$A$Set;
}
  
struct $Set$class $Set$methods = {"$Set$class", UNASSIGNED, ($Super$class)&$Container$methods, $Set$__init__, NULL, NULL, ($bool (*)($Set))$default__bool__,  ($str (*)($Set))$default__str__, ($str (*)($Set))$default__str__,
                                NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL};

$Set $Set$new($Eq w$Eq$A$Set) {
  $Set res = GC_MALLOC(sizeof(struct $Set));
  res->$class = &$Set$methods;
  res->w$Ord = $Ord$new();
  res->w$Logical = $Logical$new();
  res->w$Minus = $Minus$new();
  res->w$Eq$A$Set = w$Eq$A$Set;
 return res;
}

void $Number$__init__($Number self) {
  self->w$Minus = $Minus$new();
}

struct $Number$class $Number$methods = {"$Number$class", UNASSIGNED, ($Super$class)&$Times$methods, $Number$__init__, NULL, NULL, ($bool (*)($Number))$default__bool__,  ($str (*)($Number))$default__str__,($str (*)($Number))$default__str__,
                                        NULL, ($WORD (*)($Number,$WORD,$WORD))$Plus$__iadd__, NULL, ($WORD (*)($Number,$WORD,$WORD))$Plus$__iadd__, NULL, NULL, NULL, $Number$__ipow__, NULL, NULL, NULL, NULL, NULL, NULL};


$Number $Number$new() {
  $Number res = GC_MALLOC(sizeof(struct $Number));
  res->$class = &$Number$methods;
  res->w$Minus = $Minus$new();
  return res;
}


struct $Real$class $Real$methods = {"$Real$class", UNASSIGNED, ($Super$class)&$Number$methods, (void (*)($Real))$default__init__, NULL, NULL, ($bool (*)($Real))$default__bool__,  ($str (*)($Real))$default__str__, ($str (*)($Real))$default__str__,
                                    NULL, ($WORD (*)($Real,$WORD,$WORD))$Plus$__iadd__, NULL, ($WORD (*)($Real,$WORD,$WORD))$Times$__imul__, NULL, NULL, NULL, ($WORD (*)($Real,$WORD,$WORD))$Number$__ipow__, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL};


$Real $Real$new() {
  $Real res = GC_MALLOC(sizeof(struct $Real));
  res->$class = &$Real$methods;
  return res;
}

struct $Rational$class $Rational$methods = {"$Rational$class", UNASSIGNED, ($Super$class)&$Real$methods, (void (*)($Rational))$default__init__, NULL, NULL, ($bool (*)($Rational))$default__bool__,  ($str (*)($Rational))$default__str__, ($str (*)($Rational))$default__str__,
                                            NULL, ($WORD (*)($Rational,$WORD,$WORD))$Plus$__iadd__, NULL,  ($WORD (*)($Rational,$WORD,$WORD))$Times$__imul__, NULL, NULL, NULL, ($WORD (*)($Rational,$WORD,$WORD))$Number$__ipow__, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL};


$Rational $Rational$new() {
  $Rational res = GC_MALLOC(sizeof(struct $Rational));
  res->$class = &$Rational$methods;
  return res;
}

void $Integral$__init__($Integral self) {
  self->w$Logical = $Logical$new();
  self->w$Minus = $Minus$new();
}
  
struct $Integral$class $Integral$methods = {"$Integral$class", UNASSIGNED, ($Super$class)&$Rational$methods, $Integral$__init__, NULL, NULL, ($bool (*)($Integral))$default__bool__,  ($str (*)($Integral))$default__str__, ($str (*)($Integral))$default__str__,
                                            NULL,  ($WORD (*)($Integral,$WORD,$WORD))$Plus$__iadd__, NULL,  ($WORD (*)($Integral,$WORD,$WORD))$Times$__imul__, NULL, NULL, NULL,  ($WORD (*)($Integral,$WORD,$WORD))$Number$__ipow__, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, $Integral$__ifloordiv__, $Integral$__imod__, $Integral$__ilshift__, $Integral$__irshift__, NULL};

$Integral $Integral$new() {
  $Integral res = GC_MALLOC(sizeof(struct $Integral));
  res->$class = &$Integral$methods;
  res->w$Logical = $Logical$new();
  res->w$Minus = $Minus$new();
 return res;
}

void $register_builtin_protocols() {
  $register(&$Eq$methods);
  $register(&$Ord$methods);
  $register(&$Logical$methods);
  $register(&$Plus$methods);
  $register(&$Times$methods);
  $register(&$Div$methods);
  $register(&$Minus$methods);
  $register(&$Hashable$methods);
  $register(&$Indexed$methods);
  $register(&$Sliceable$methods);
  $register(&$Iterable$methods);
  $register(&$Collection$methods);
  $register(&$Container$methods);
  $register(&$Sequence$methods);
  $register(&$Mapping$methods);
  $register(&$Set$methods);
  $register(&$Number$methods);
  $register(&$Real$methods);
  $register(&$Rational$methods);
  $register(&$Hashable$bool$methods);
  $register(&$Sequence$list$methods);
  $register(&$Collection$list$methods);
  $register(&$Times$list$methods);
  $register(&$Ord$list$methods);
  $register(&$Container$list$methods);
  $register(&$Mapping$dict$methods);
  $register(&$Indexed$dict$methods);
  $register(&$Ord$dict$methods);
  $register(&$Set$set$methods);
  $register(&$Ord$set$methods);
  $register(&$Logical$set$methods);
  $register(&$Minus$set$methods);
  $register(&$Iterable$Iterator$methods);
  $register(&$Ord$str$methods);
  $register(&$Container$str$methods);
  $register(&$Sliceable$str$methods);
  $register(&$Times$str$methods);
  $register(&$Hashable$str$methods);
  $register(&$Integral$int$methods);
  $register(&$Logical$int$methods);
  $register(&$Minus$int$methods);
  $register(&$Ord$int$methods);
  $register(&$Hashable$int$methods);
  $register(&$Real$float$methods);
  $register(&$Div$float$methods);
  $register(&$Minus$float$methods);
  $register(&$Ord$float$methods);
  $register(&$Hashable$float$methods);
  $register(&$Number$complex$methods);
  $register(&$Div$complex$methods);
  $register(&$Minus$complex$methods);
  $register(&$Eq$complex$methods);
  $register(&$Hashable$complex$methods);
  $register(&$Iterable$range$methods);
  $register(&$Iterable$tuple$methods);
  $register(&$Sliceable$tuple$methods);
  $register(&$Hashable$tuple$methods);
  $register(&$Ord$bytearray$methods);
  $register(&$Sequence$bytearray$methods);
  $register(&$Collection$bytearray$methods);
  $register(&$Times$bytearray$methods);
  $register(&$Container$bytearray$methods);
  $register(&$Hashable$WORD$methods);
  
}
