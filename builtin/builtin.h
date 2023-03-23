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

#include "__builtin__.h"
#include "class_hierarchy.h"
#include "serialize.h"
#include "registration.h"
#include "Iterator.h"
#include "complx.h"
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


