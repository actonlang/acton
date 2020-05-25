
void $range$__init__($range self, $int start, $int stop, $int step) {
  if (start) 
      self->start = from$int(start);
  else 
      self->start = 0;
  self->stop = from$int(stop);
  if (step) {
    int stp = from$int(step);
    if (stp==0) {
    RAISE(($BaseException)$NEW($ValueError,from$UTF8("step size zero in range")));
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
  asprintf(&s,"range(%d,%d,%d)",self->start,self->stop,self->step);
  return from$UTF8(s);
}

void $range$__serialize__($range self, $Serial$state state) {
  $ROW row = $add_header(RANGE_ID,3,state);
  row->blob[0] = ($WORD)(long)self->start;
  row->blob[1] = ($WORD)(long)self->stop;
  row->blob[2] = ($WORD)(long)self->step;
}

$range $range$__deserialize__($Serial$state state) {
  $ROW this = state->row;
  state->row = this->next;
  state->row_no++;
  $range res = malloc(sizeof(struct $range));
  res->$class = &$range$methods;
  res->start = (int)this->blob[0];
  res->stop = (int)this->blob[1];
  res->step = (int)this->blob[2];
  return res;
}

static $WORD $Iterator$range_next($Iterator$range self) {
  int res = self->src->start + self->nxt++*self->src->step;
  if (self->src->step>0)
    return res < self->src->stop ? to$int(res) : NULL;
  else
    return res > self->src->stop ? to$int(res) : NULL;
}

void $Iterator$range_init($Iterator$range self, $range rng) {
  self->src = rng;
  self->nxt = 0;
}                                    

$bool $Iterator$range_bool($Iterator$range self) {
  return $true;
}

$str $Iterator$range_str($Iterator$range self) {
  char *s;
  asprintf(&s,"<range iterator object at %p>",self);
  return from$UTF8(s);
}

void $Iterator$range_serialize($Iterator$range self, $Serial$state state) {
  $step_serialize(self->src,state);
  $step_serialize(to$int(self->nxt),state);
}

$Iterator$range $Iterator$range$_deserialize($Serial$state state) {
   $Iterator$range res = $DNEW($Iterator$range,state);
   res->src = ($range)$step_deserialize(state);
   res->nxt = from$int(($int)$step_deserialize(state));
   return res;
}


struct $range$class $range$methods = {"",UNASSIGNED,NULL,$range$__init__,$range$__bool__,$range$__str__,$range$__serialize__,$range$__deserialize__};


struct $Iterator$range$class $Iterator$range$methods = {"",UNASSIGNED,($Super$class)&$Iterator$methods, $Iterator$range_init,$Iterator$range_bool,
                                                        $Iterator$range_str,$Iterator$range_serialize, $Iterator$range$_deserialize, $Iterator$range_next};

//$Iterator $range_iter($range rng) {
//  return ($Iterator)$NEW($Iterator$range,rng);
//}

void $Iterable$range$__init__ ($Iterable$range wit){
  return;
}


$Iterator $Iterable$range$__iter__ ($Iterable$range wit, $range rng) {
  return ($Iterator)$NEW($Iterator$range,rng);
}

struct $Iterable$range$class $Iterable$range$methods = {"",UNASSIGNED,NULL,$Iterable$range$__init__,$Iterable$range$__iter__};

struct $Iterable$range $Iterable$range_instance = {&$Iterable$range$methods};
$Iterable$range $Iterable$range$witness = &$Iterable$range_instance;

