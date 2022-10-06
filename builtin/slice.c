/*
 * Copyright (C) 2019-2021 Data Ductus AB
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


void normalize_slice($slice slc, int len, int *slen, int *start, int *stop, int *step) {
  if (slc->step == NULL)
    *step = 1;
  else
    *step = *slc->step;
  if (*step == 0) {
    $RAISE(($BaseException)$NEW($ValueError,to$str("step size 0 in slice")));
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

$slice $slice$new($int start,$int stop,$int step) {
  return $NEW($slice,start,stop,step);
}

void $slice__init__($slice s, $int start, $int stop, $int step) {
  if (start) {
    s->start = GC_MALLOC(sizeof(int));
    *s->start = start->val;
  } else
    s->start = NULL;
 if (stop) {
    s->stop = GC_MALLOC(sizeof(int));
    *s->stop = stop->val;
  } else
   s->stop = NULL;
 if (step) {
    s->step = GC_MALLOC(sizeof(int));
    *s->step = step->val;
  } else
   s->step = NULL;
}

struct $slice$class $slice$methods = {"$slice",UNASSIGNED,($Super$class)&$value$methods,$slice__init__,NULL,NULL,NULL,NULL};
