/*
 * common.h
 *      Author: aagapi
 */

#ifndef BACKEND_COMMON_H_
#define BACKEND_COMMON_H_


typedef void *WORD;

#define DB_TYPE_CHAR 0
#define DB_TYPE_INT16 1
#define DB_TYPE_INT32 2
#define DB_TYPE_INT64 3
#define DB_TYPE_FLOAT32 4
#define DB_TYPE_FLOAT64 5
#define DB_TYPE_BLOB 6

#define VERBOSE_BACKEND 0

#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <inttypes.h>
#include <string.h>
#include <assert.h>

#endif /* BACKEND_COMMON_H_ */
