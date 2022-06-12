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

$range $range$new($int start, $int stop, $int step) {
  return $NEW($range, start, stop, step);
}

void $range$__init__($range self, $int start, $int stop, $int step) {
  if (stop) {
      self->start = from$int(start);
      self->stop = from$int(stop);
  } else {
      self->start = 0;
      self->stop = from$int(start);
  }
  if (step) {
    int stp = from$int(step);
    if (stp==0) {
      $RAISE(($BaseException)$NEW($ValueError,to$str("step size zero in range")));
    }
    else
      self->step = stp;
  } else
    self->step = 1;
}


$bool $range$__bool__($range self) {
  return to$bool ((self->step > 0 && self->stop > self->start) ||
                  (self->start > self->stop));
}

$str $range$__str__($range self) {
  char *s;
  asprintf(&s,"range(%ld,%ld,%ld)",self->start,self->stop,self->step);
  return to$str(s);
}

void $range$__serialize__($range self, $Serial$state state) {
  $ROW row = $add_header(RANGE_ID,3,state);
  row->blob[0] = ($WORD)self->start;
  row->blob[1] = ($WORD)self->stop;
  row->blob[2] = ($WORD)self->step;
}

$range $range$__deserialize__($range self, $Serial$state state) {
  $ROW this = state->row;
  state->row = this->next;
  state->row_no++;
  $range res = malloc(sizeof(struct $range));
  res->$class = &$range$methods;
  res->start = (long)this->blob[0];
  res->stop = (long)this->blob[1];
  res->step = (long)this->blob[2];
  return res;
}

static $WORD $Iterator$range_next($Iterator$range self) {
  int res = self->src->start + self->nxt++*self->src->step;
  if (self->src->step>0)
    return res < self->src->stop ? to$int(res) : NULL;
  else
    return res > self->src->stop ? to$int(res) : NULL;
}

$Iterator$range $Iterator$range$new($range rng) {
  return $NEW($Iterator$range,rng);
}

void $Iterator$range_init($Iterator$range self, $range rng) {
  self->src = rng;
  self->nxt = 0;
}                                    

$bool $Iterator$range_bool($Iterator$range self) {
  return $True;
}

$str $Iterator$range_str($Iterator$range self) {
  char *s;
  asprintf(&s,"<range iterator object at %p>",self);
  return to$str(s);
}

void $Iterator$range_serialize($Iterator$range self, $Serial$state state) {
  $step_serialize(self->src,state);
  $step_serialize(to$int(self->nxt),state);
}

$Iterator$range $Iterator$range$_deserialize($Iterator$range self, $Serial$state state) {
   $Iterator$range res = $DNEW($Iterator$range,state);
   res->src = ($range)$step_deserialize(state);
   res->nxt = from$int(($int)$step_deserialize(state));
   return res;
}


struct $range$class $range$methods = {
   "$range",
   UNASSIGNED,
   ($Super$class)&$value$methods,
   $range$__init__,
   $range$__serialize__,
   $range$__deserialize__,
   $range$__bool__,
   $range$__str__,
   $range$__str__
};


struct $Iterator$range$class $Iterator$range$methods = {
    "$Iterator$range",
    UNASSIGNED,
    ($Super$class)&$Iterator$methods,
    $Iterator$range_init,
    $Iterator$range_serialize,
    $Iterator$range$_deserialize,
    $Iterator$range_bool,
    $Iterator$range_str,
    $Iterator$range_str,
    $Iterator$range_next
};

//$Iterator $range_iter($range rng) {
//  return ($Iterator)$NEW($Iterator$range,rng);
//}

$Iterable$range $Iterable$range$new() {
  return $NEW($Iterable$range);
}

void $Iterable$range$__init__ ($Iterable$range wit){
  return;
}

void $Iterable$range$__serialize__($Iterable$range self, $Serial$state state) {
}

$Iterable$range $Iterable$range$__deserialize__($Iterable$range res, $Serial$state state) {
   if(!res)
      res = $DNEW($Iterable$range,state);
   return res;
}

$Iterator $Iterable$range$__iter__ ($Iterable$range wit, $range rng) {
  return ($Iterator)$NEW($Iterator$range,rng);
}

struct $Iterable$range$class $Iterable$range$methods = {
    "$Iterable$range",
    UNASSIGNED,
    ($Super$class)&$Iterable$methods,
    $Iterable$range$__init__,
    $Iterable$range$__serialize__,
    $Iterable$range$__deserialize__,
    ($bool (*)($Iterable$range))$default__bool__,
    ($str (*)($Iterable$range))$default__str__,
    ($str (*)($Iterable$range))$default__str__,
    $Iterable$range$__iter__
};

struct $Iterable$range $Iterable$range_instance = {&$Iterable$range$methods};
$Iterable$range $Iterable$range$witness = &$Iterable$range_instance;

 
