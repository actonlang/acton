#pragma once

#include "hash.h"
#include "iterator.h"

typedef void *WORD;

struct set_struct;

typedef struct set_struct *set_t;

set_t set_new(Hashable h);
void set_add(set_t s, WORD elem);     // never fails
int set_remove(set_t s, WORD elem);   // returns KEYERROR if elem not in s
void set_discard(set_t s, WORD elem); // does not fail if elem not in s
int set_pop(set_t s, WORD *res);      // removes an unspecified element and returns it; returns KEYERROR if empty
int set_contains(set_t s, WORD elem);
int set_isdisjoint(set_t s, set_t other);
iterator_t set_iter(set_t s); 
int set_len(set_t s);

int set_eq(set_t s, set_t other);
int set_ne(set_t s, set_t other);
int set_lt(set_t s, set_t other);
int set_le(set_t s, set_t other);
int set_gt(set_t s, set_t other);
int set_ge(set_t s, set_t other);

int set_sub(set_t s, set_t other);

set_t set_union(set_t s, set_t other);
set_t set_intersection(set_t s, set_t other);
set_t set_symmetric_difference(set_t s, set_t other);

