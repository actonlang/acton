#pragma once
#ifdef __linux__
#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif
#endif

#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <stdatomic.h>

#include "../rts/common.h"
#include "common.h"

struct B_NoneType;
typedef struct B_NoneType *B_NoneType;

struct B_tuple;
typedef struct B_tuple *B_tuple;

struct $Serial$state;
typedef struct $Serial$state *$Serial$state;

struct $SuperG_class;
typedef struct $SuperG_class *$SuperG_class;

struct $Super;
typedef struct $Super *$Super;

struct $SerializableG_class;
typedef struct $SerializableG_class *$SerializableG_class;

struct $Serializable;
typedef struct $Serializable  *$Serializable;

// The following declarations moved here from function.h

struct $proc;
struct $action;
struct $mut;
struct $pure;
struct $Cont;

typedef struct $proc *$proc;
typedef struct $action *$action;
typedef struct $mut *$mut;
typedef struct $pure *$pure;
typedef struct $Cont *$Cont;

enum $RTAG { $RDONE, $RFAIL, $RCONT, $RWAIT };
typedef enum $RTAG $RTAG;

struct $R {
    $RTAG tag;
    $Cont cont;
    $WORD value;
};
typedef struct $R $R;

#define $R_CONT(cont, arg)      ($R){$RCONT, (cont), ($WORD)(arg)}
#define $R_DONE(value)          ($R){$RDONE, NULL,   (value)}
#define $R_FAIL(value)          ($R){$RFAIL, NULL,   (value)}
#define $R_WAIT(cont, value)    ($R){$RWAIT, (cont), (value)}

/////////////////////////////////////////////////////////

// And the following from rts.h /////////////////////////

struct $Actor;
struct $Catcher;
typedef struct $Actor *$Actor;
typedef struct $Catcher *$Catcher;

#define $Lock                   volatile atomic_flag

///////////////////////////////////////////////////////////

#include "../out/types/__builtin__.h"
#include "class_hierarchy.h"
#include "serialize.h"
#include "registration.h"
#include "Iterator.h"
#include "complx.h"
#include "box.h"
#include "none.h"
#include "int.h"
#include "slice.h"
#include "float.h"
#include "bool.h"
#include "list.h"
#include "dict.h"
#include "str.h"
#include "set.h"
#include "tuple.h"
#include "hash.h"
#include "i64.h"
#include "i32.h"
#include "i16.h"
#include "u64.h"
#include "u32.h"
#include "u16.h"
#include "range.h"
#include "exceptions.h"
#include "function.h"
#include "builtin_functions.h"
#include "env.h"
#include "staticWitnesses.h"
#include "utils.h"

