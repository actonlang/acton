#pragma once

#include "../../rts/rts.h"

struct lambda$1;
struct lambda$2;
struct Act;
struct lambda$3;
struct lambda$4;
struct Root;

typedef struct lambda$1 *lambda$1;
typedef struct lambda$2 *lambda$2;
typedef struct Act *Act;
typedef struct lambda$3 *lambda$3;
typedef struct lambda$4 *lambda$4;
typedef struct Root *Root;

struct lambda$1$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)(lambda$1, $Cont);
    $bool (*__bool__)(lambda$1);
    $str (*__str__)(lambda$1);
    void (*__serialize__)(lambda$1, $Serial$state);
    lambda$1 (*__deserialize__)($Serial$state);
    $R (*enter)(lambda$1, $Msg);    
};
struct lambda$1 {
    struct lambda$1$class *$class;
    $Cont cont$0;    
};

struct lambda$2$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)(lambda$2, Act, $int, $list);
    $bool (*__bool__)(lambda$2);
  $str (*__str__)(lambda$2);
    void (*__serialize__)(lambda$2, $Serial$state);
    lambda$2 (*__deserialize__)($Serial$state);
    $R (*enter)(lambda$2, $Cont);    
};
struct lambda$2 {
    struct lambda$2$class *$class;
    Act self;
    $int from;
    $list table;
};

struct Act$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    $R (*__init__)(Act, $int, $Cont);
    $bool (*__bool__)(Act);
    $str (*__str__)(Act);
    void (*__serialize__)(Act, $Serial$state);
    Act (*__deserialize__)($Serial$state);
    $R (*act$local)(Act, $int, $list, $Cont);
    $R (*act)(Act, $int, $list, $Cont);
};
struct Act {
    union {
        struct Act$class *$class;
        struct $Actor super;
    };
    $int i;
    $int count;
    $dict rcv_dict;
    $dict snd_dict;
};

struct lambda$3$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)(lambda$3, Root, $Iterator, $Cont);
    $bool (*__bool__)(lambda$3);
    $str (*__str__)(lambda$3);
    void (*__serialize__)(lambda$3, $Serial$state);
    lambda$3 (*__deserialize__)($Serial$state);
    $R (*enter)(lambda$3, Act);
};
struct lambda$3 {
    struct lambda$3$class *$class;
    Root self;
    $Iterator iter$1;
    $Cont cont$0;
};

struct lambda$4$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)(lambda$4, $Cont);
    $bool (*__bool__)(lambda$4);
    $str (*__str__)(lambda$4);
    void (*__serialize__)(lambda$4, $Serial$state);
    lambda$4 (*__deserialize__)($Serial$state);
    $R (*enter)(lambda$4, $WORD);
};
struct lambda$4 {
    struct lambda$4$class *$class;
    $Cont cont$0;
};

struct Root$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    $R (*__init__)(Root, $int, $Cont);
    $bool (*__bool__)(Root);
  $str (*__str__)(Root);
    void (*__serialize__)(Root, $Serial$state);
    Root (*__deserialize__)($Serial$state);
};
struct Root {
    union {
        struct Root$class *$class;
        struct $Actor super;
    };
    $list table;
};

$R loop$1(Root, $Iterator, $Cont, $WORD);
$R cont$1(Root, $Iterator, $Cont, Act);
$R join$1(Root, $Cont, $WORD);

extern struct lambda$1$class lambda$1$methods;
extern struct lambda$2$class lambda$2$methods;
extern struct Act$class Act$methods;
extern struct lambda$3$class lambda$3$methods;
extern struct lambda$4$class lambda$4$methods;
extern struct Root$class Root$methods;

extern $Mapping $Mapping$dict$witness;
extern $Indexed $Indexed$dict$witness;

extern $int no_actors;
extern $int total_msgs;
