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

/* Normalize slice notation, so that
- if step == 0, VALUEERROR is returned

- Otherwise, 
   - on input, nchars must be the # of elements in the sequence being sliced-
   - on output 
       - 0 <= start < nchars is the starting position
       - 0 <= stop < nchars is the ending position (*non-inclusive*!)
       - step is the step size
       - nchars is the # of elements in *the slice*. 
*/
int normalize_slice(slice_t slc, int *len, int *start, int *stop, int *step);
