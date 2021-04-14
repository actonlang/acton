#ifdef __linux__
#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif
#endif

#include "builtin.h"

#include "common.c"
#include "__builtin__.c"
#include "class_hierarchy.c"
#include "none.c"
#include "atom.c"
#include "int.c"
#include "float.c"
#include "bool.c"
#include "complex.c"
#include "Iterator.c"
#include "slice.c"
#include "hash.c"
#include "list.c"
#include "list_impl.c"
#include "dict.c"
#include "dict_impl.c"
#include "str.c"
#include "set.c"
#include "set_impl.c"
#include "tuple.c"
#include "range.c"
#include "exceptions.c"
#include "serialize.c"
#include "registration.c"
#include "function.c"
#include "builtin_functions.c"

