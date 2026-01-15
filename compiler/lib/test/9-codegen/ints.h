/* Acton impl hash: test-hash */
#pragma once
#include "builtin/builtin.h"
#include "rts/rts.h"
extern int64_t intsQ_U_int64_min;
extern B_int intsQ_int64_min;
extern int64_t intsQ_U_1int64_max;
extern B_int intsQ_int64_max;
extern int32_t intsQ_U_2int32_min;
extern B_i32 intsQ_int32_min;
extern int32_t intsQ_U_3int32_max;
extern B_i32 intsQ_int32_max;
extern int16_t intsQ_U_4int16_min;
extern B_i16 intsQ_int16_min;
extern int16_t intsQ_U_5int16_max;
extern B_i16 intsQ_int16_max;
extern B_Number intsQ_W_20;
extern B_i8 intsQ_int8_min;
extern B_Number intsQ_W_23;
extern B_i8 intsQ_int8_max;
extern uint64_t intsQ_U_6uint64_min;
extern B_u64 intsQ_uint64_min;
extern uint64_t intsQ_U_7uint64_max;
extern B_u64 intsQ_uint64_max;
extern uint32_t intsQ_U_8uint32_min;
extern B_u32 intsQ_uint32_min;
extern uint32_t intsQ_U_9uint32_max;
extern B_u32 intsQ_uint32_max;
extern uint16_t intsQ_U_10uint16_min;
extern B_u16 intsQ_uint16_min;
extern uint16_t intsQ_U_11uint16_max;
extern B_u16 intsQ_uint16_max;
extern B_Number intsQ_W_42;
extern B_u8 intsQ_uint8_min;
extern B_Number intsQ_W_45;
extern B_u8 intsQ_uint8_max;
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