#include "slice.h"
#include "acterror.h"

int normalize_slice(slice_t slc, int *len, int *start, int *stop, int *step) {
  *step = slc->step;
  if (slc->has & HAS_STEP) {
    if (*step == 0)
      return VALUEERROR;
  } else
    *step = 1;
  *start = slc->start;
  *start = slc->has & HAS_START ? (*start >=0 ? (*start < *len  ? *start : *len-1 + (*step > 0))
                                              : (*start > -*len ? *len+*start : 0))
                                : (*step > 0 ? 0 : *len-1);
  *stop = slc->stop;
  *stop = slc->has & HAS_STOP ? (*stop >= 0 ? (*stop < *len  ? *stop : *len-1 + (*step > 0)) 
                                            : (*stop > -*len ? *len+*stop : 0))
                              : (*step > 0 ? *len : -1);
  if ((*step > 0 && *start >= *stop) || (*step < 0 && *start <= *stop))
    *len = 0;
  else
    *len = (*stop-*start)/ *step + ((*stop-*start)%*step != 0);
  return 0;
}
