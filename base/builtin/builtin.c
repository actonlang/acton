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

#ifdef __linux__
#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif
#endif

#include "builtin.h"

#include "common.c"
#include "class_hierarchy.c"
#include "none.c"
#include "box.c"
// #include "atom.c"
#include "int.c"
#include "i32.c"
#include "i16.c"
#include "i8.c"
#include "u64.c"
#include "u32.c"
#include "u16.c"
#include "u8.c"
#include "u1.c"
#include "bigint.c"
#include "float.c"
#include "bool.c"
#include "complex.c"
#include "Iterator.c"
#include "slice.c"
//#include "hash.c"
#include "timsort.c"
#include "list.c"
#include "dict.c"
#include "str.c"
#include "set.c"
#include "tuple.c"
#include "range.c"
#include "exceptions.c"
#include "serialize.c"
#include "registration.c"
#include "function.c"
#include "builtin_functions.c"
#include "env.c"
#include "staticWitnesses.c"
#include "utils.c"
#include "hasher.c"
