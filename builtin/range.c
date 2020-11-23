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
    RAISE(($BaseException)$NEW($ValueError,to$str("step size zero in range")));
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

$range $range$__deserialize__($Serial$state state) {
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

$Iterator$range $Iterator$range$_deserialize($Serial$state state) {
   $Iterator$range res = $DNEW($Iterator$range,state);
   res->src = ($range)$step_deserialize(state);
   res->nxt = from$int(($int)$step_deserialize(state));
   return res;
}


struct $range$class $range$methods = {"",UNASSIGNED,($Super$class)&$struct$methods,$range$__init__,$range$__serialize__,$range$__deserialize__,$range$__bool__,$range$__str__};


struct $Iterator$range$class $Iterator$range$methods = {"",UNASSIGNED,($Super$class)&$Iterator$methods, $Iterator$range_init,
                                                       $Iterator$range_serialize, $Iterator$range$_deserialize,$Iterator$range_bool,$Iterator$range_str,$Iterator$range_next};

//$Iterator $range_iter($range rng) {
//  return ($Iterator)$NEW($Iterator$range,rng);
//}

$Iterable$range $Iterable$range$new() {
  return $NEW($Iterable$range);
}

void $Iterable$range$__init__ ($Iterable$range wit){
  return;
}


$Iterator $Iterable$range$__iter__ ($Iterable$range wit, $range rng) {
  return ($Iterator)$NEW($Iterator$range,rng);
}

struct $Iterable$range$class $Iterable$range$methods = {"",UNASSIGNED,NULL,$Iterable$range$__init__,$Iterable$range$__iter__};

struct $Iterable$range $Iterable$range_instance = {&$Iterable$range$methods};
$Iterable$range $Iterable$range$witness = &$Iterable$range_instance;


// $Sequence$range ////////////////////////////////////////////////////////////

struct $Collection$range$class $Collection$range$methods = {"",UNASSIGNED,NULL,$Collection$range$__init__,
                                                            $Collection$range$__iter__,$Collection$range$__fromiter__,
                                                            $Collection$range$__len__};

struct $Plus$range$class $Plus$range$methods = {"",UNASSIGNED,NULL,$Plus$range$__init__,$Plus$range$__add__};

$Sequence$range $Sequence$range$new() {
  return $NEW($Sequence$range);
}

void $Sequence$range$__init__($Sequence$range self) {
  self->w$Collection = ($Collection)$NEW($Collection$range, self);
  self->w$Plus = ($Plus)$NEW($Plus$range, self);
}

$int $Sequence$range$__getitem__ ($Sequence$range wit, $range self, $int n) {
  long res = self->start + n->val*self->step;
  if ((self->step>0 && res>self->stop) || (self->step<0 && res<self->stop))
    RAISE(($BaseException)$NEW($IndexError,to$str("getitem: indexing outside range")));
  return to$int(res);
}
    
  
void $Sequence$range$__setitem__ ($Sequence$range wit, $range self, $int m, $int n) {
  fprintf(stderr,"Internal error: call to mutating method setitem on range");
  exit(-1);
}

void $Sequence$range$__delitem__ ($Sequence$range wit, $range self, $int n) {
  fprintf(stderr,"Internal error: call to mutating method delitem on range");
  exit(-1);
}

$range $Sequence$range$__getslice__ ($Sequence$range wit, $range self, $Slice slc) {
  int len = wit->w$Collection->$class->__len__(wit->w$Collection,self)->val;
  int start, stop, step, slen;
  normalize_slice(slc, len, &slen, &start, &stop, &step);
  $int sstart = $Sequence$range$__getitem__(wit,self,to$int(start));
  int sstep = (self->step)*step;
  int sstop = sstart->val + slen * sstep;
  return $NEW($range,sstart,to$int(sstop),to$int(sstep));
}
void $Sequence$range$__setslice__ ($Sequence$range wit, $Iterable wit2, $range self, $Slice slc, $WORD iter) {
  fprintf(stderr,"Internal error: call to mutating method setslice on range");
  exit(-1);
}

void $Sequence$range$__delslice__ ($Sequence$range wit, $range self, $Slice slc) {
  fprintf(stderr,"Internal error: call to mutating method delslice on range");
  exit(-1);
}

//$Iterator $Sequence$range$__reversed__ ($Sequence$range, $range);
//void $Sequence$range$insert ($Sequence$range, $range, $int, $int);
//void $Sequence$range$append ($Sequence$range, $range, $int);
//void $Sequence$range$reverse ($Sequence$range, $range);

// $Collection$range ////////////////////////////////////////////////////////////

void $Collection$range$__init__($Collection$range self, $Sequence$range master) {
  self->w$Sequence$range = master;
}

$Iterator $Collection$range$__iter__ ($Collection$range wit, $range self) {
  return ($Iterator)$NEW($Iterator$range,self);
}
  
  
$range $Collection$range$__fromiter__ ($Collection$range wit, $Iterable wit2, $WORD iter) {
    RAISE(($BaseException)$NEW($NotImplementedError,to$str("__fromiter__ cannot be used to build range")));
    return NULL;
}
  
$int $Collection$range$__len__ ($Collection$range wit, $range self) {
  if (self->step < 0) 
    return to$int(self->stop >= self->start ? 0 : 1+(self->stop+1-self->start)/self->step);
  return to$int(self->stop <= self->start ? 0 : 1+(self->stop-1-self->start)/self->step);
}

// $Plus$range ////////////////////////////////////////////////////////////

void $Plus$range$__init__($Plus$range self, $Sequence$range master) {
  self->w$Sequence$range = master;
}

$range $Plus$range$__add__ ($Plus$range wit, $range a, $range b) {
    RAISE(($BaseException)$NEW($NotImplementedError,to$str("__add__ not implemented for range arguments")));
    return NULL;
}
  

struct $Sequence$range  $Sequence$range_instance;
struct $Collection$range $Collection$range_instance;
struct $Plus$range $Plus$range_instance;

struct $Sequence$range$class $Sequence$range$methods = {"",UNASSIGNED,NULL,$Sequence$range$__init__,
                                                        $Sequence$range$__getitem__, $Sequence$range$__setitem__, $Sequence$range$__delitem__,
                                                        $Sequence$range$__getslice__, $Sequence$range$__setslice__, $Sequence$range$__delslice__,
                                                        NULL,NULL,NULL,NULL};
// $Sequence$range$__reversed__,$Sequence$range$insert,$Sequence$range$append,$Sequence$range$reverse};
struct $Sequence$range $Sequence$range_instance = {
    &$Sequence$range$methods,
    ($Collection)&$Collection$range_instance,
    ($Plus)&$Plus$range_instance
};
struct $Sequence$range *$Sequence$range$witness = &$Sequence$range_instance;

struct $Collection$range $Collection$range_instance = {&$Collection$range$methods,&$Sequence$range_instance};
$Collection$range $Collection$range$witness = &$Collection$range_instance;


struct $Plus$range $Plus$range_instance = {&$Plus$range$methods, &$Sequence$range_instance};
$Plus$range $Plus$range$witness = &$Plus$range_instance;
