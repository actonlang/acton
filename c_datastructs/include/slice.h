#pragma once

#define HAS_START 1
#define HAS_STOP  1 << 1
#define HAS_STEP  1 << 2

typedef struct slice_struct {
  int has;
  int start;
  int stop;
  int step;
} *slice_t;

