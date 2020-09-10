
/* Normalize slice notation, so that
- if step == 0, VALUEERROR is raised

- Otherwise, 
   - on input, len must be the # of elements in the sequence being sliced
   - on output 
       - 0 <= *start < len is the starting position
       - 0 <= *stop < len is the ending position (*non-inclusive*!)
       - *step is the step size
       - *slen is the # of elements in the slice. 
*/


void normalize_slice($Slice slc, int len, int *slen, int *start, int *stop, int *step) {
  if (slc->step == NULL)
    *step = 1;
  else
    *step = *slc->step;
  if (*step == 0) {
    RAISE(($BaseException)$NEW($ValueError,to$str("step size 0 in slice")));
  }
  if (slc->start == NULL)
    *start = *step > 0 ? 0 : len-1;
  else {
    *start = *slc->start;
    *start = *start >=0 ? (*start < len  ? *start : len-1 + (*step > 0))
                        : (*start > -len ? len+*start : 0);
  }
  if (slc->stop == NULL)
    *stop = *step > 0 ? len : -1;
  else {
    *stop = *slc->stop;
    *stop = *stop >= 0 ? (*stop < len  ? *stop : len-1 + (*step > 0)) 
                       : (*stop > -len ? len+*stop : 0);
  }
  
  if ((*step > 0 && *start >= *stop) || (*step < 0 && *start <= *stop))
    *slen = 0;
  else
    *slen = (*stop-*start)/ *step + ((*stop-*start)%*step != 0);
}

void $Slice__init__($Slice s, $int start, $int stop, $int step) {
  if (start) {
    s->start = malloc(sizeof(int));
    *s->start = start->val;
  } else
    s->start = NULL;
 if (stop) {
    s->stop = malloc(sizeof(int));
    *s->stop = stop->val;
  } else
   s->stop = NULL;
 if (step) {
    s->step = malloc(sizeof(int));
    *s->step = step->val;
  } else
   s->step = NULL;
}

struct $Slice$class $Slice$methods = {"",UNASSIGNED,($Super$class)&$struct$methods,$Slice__init__,NULL,NULL,NULL,NULL};
