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

#include "common.h"
#include "__builtin__.h"
#include "serialize.h"
#include "registration.h"
#include "Iterator.h"
#include "complx.h"
#include "none.h"
#include "atom.h"
#include "int.h"
#include "slice.h"
#include "float.h"
#include "bool.h"
#include "hash.h"
#include "list.h"
#include "list_impl.h"
#include "dict.h"
#include "dict_impl.h"
#include "str.h"
#include "set.h"
#include "set_impl.h"
#include "tuple.h"
#include "range.h"
#include "exceptions.h"
#include "function.h"
#include "builtin_functions.h"
