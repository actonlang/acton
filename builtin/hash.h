#pragma once
#include "common.h"

long $int_hash($int n);
long $float_hash($float v);

long $string_hash(void *s, int len);
