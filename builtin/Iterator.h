#pragma once

#include "__builtin__.h"

struct $Iterator$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Iterator);
    void (*__serialize__)($Iterator,$Serial$state);
    $Iterator (*__deserialize__)($Iterator,$Serial$state);
    $bool (*__bool__)($Iterator);
    $str (*__str__)($Iterator);
    $str (*__repr__)($Iterator);
    $WORD (*__next__)($Iterator);
};

struct $Iterator {
    struct $Iterator$class *$class;
};

extern struct $Iterable$Iterator$class $Iterable$Iterator$methods;
$Iterable$Iterator $Iterable$Iterator$new();
extern struct $Iterable$Iterator *$Iterable$Iterator$witness;

extern struct $Iterator$class $Iterator$methods;
$Iterator $Iterator$new();
extern struct $Iterator *$Iterator$witness;

$WORD $next($Iterator);
