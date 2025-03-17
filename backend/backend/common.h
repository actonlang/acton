/*
 * Copyright (C) 2019-2021 Deutsche Telekom AG
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

/*
 * common.h
 *      Author: aagapi
 */

#ifndef BACKEND_COMMON_H_
#define BACKEND_COMMON_H_

// Avoid conflict with Windows WORD type
#if !defined(_WIN32) && !defined(_WIN64) && !defined(__MINGW32__) && !defined(__MINGW64__) && !defined(__CYGWIN__)
typedef void *WORD;
#endif

#define DB_TYPE_CHAR 0
#define DB_TYPE_INT16 1
#define DB_TYPE_INT32 2
#define DB_TYPE_INT64 3
#define DB_TYPE_FLOAT32 4
#define DB_TYPE_FLOAT64 5
#define DB_TYPE_BLOB 6

#define VERBOSE_BACKEND 0

#include <stdlib.h>
#if !defined(_WIN32) && !defined(_WIN64) && !defined(__MINGW32__) && !defined(__MINGW64__) && !defined(__CYGWIN__)
#include <pthread.h>
#include <unistd.h>
#endif
#include <inttypes.h>
#include <string.h>
#include <assert.h>

#endif /* BACKEND_COMMON_H_ */
