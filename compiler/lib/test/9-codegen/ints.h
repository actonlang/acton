/* Acton codegen hash: test-hash */
#pragma once
#include "builtin/builtin.h"
#include "rts/rts.h"
extern int64_t intsQ_int64_min;
extern int64_t intsQ_int64_max;
extern int32_t intsQ_int32_min;
extern int32_t intsQ_int32_max;
extern int16_t intsQ_int16_min;
extern int16_t intsQ_int16_max;
extern int8_t intsQ_int8_min;
extern int8_t intsQ_int8_max;
extern uint64_t intsQ_uint64_min;
extern uint64_t intsQ_uint64_max;
extern uint32_t intsQ_uint32_min;
extern uint32_t intsQ_uint32_max;
extern uint16_t intsQ_uint16_min;
extern uint16_t intsQ_uint16_max;
extern uint8_t intsQ_uint8_min;
extern uint8_t intsQ_uint8_max;
extern B_bigint intsQ_bigint_small;
extern B_bigint intsQ_bigint_neg_small;
extern B_bigint intsQ_bigint_i64_max;
extern B_bigint intsQ_bigint_i64_min;
extern B_bigint intsQ_bigint_i64_underflow;
extern B_bigint intsQ_bigint_u64_edge;
extern B_bigint intsQ_bigint_u64_edge_minus1;
extern B_bigint intsQ_bigint_u64_overflow;
extern B_bigint intsQ_xint_i64_min_minus1;
void intsQ___init__ ();