
void $range$__init__($range self, $int start, $int stop, $int step) {
  self->start = start != NULL ? from$int(start) : 0;
  self->stop = from$int(stop);
  if (step) {
    int stp = from$int(step);
    if (stp==0) {
     exception e;
     MKEXCEPTION(e,INDEXERROR);
     RAISE(e);
    }
    else
      self->step = stp;
  } else
    self->step = 1;
}

void $range$__serialize__($range self, $Mapping$dict wit, long *start_no, $dict done, struct $ROWLISTHEADER *accum) {
  $int prevkey = ($int)$dict_get(done,wit->w$Hashable$Mapping,self,NULL);
  if (prevkey) {
    $val_serialize(-RANGE_ID,&prevkey->val,start_no,accum);
  } else {
    $dict_setitem(done,wit->w$Hashable$Mapping,self,to$int(*start_no));
    $enqueue(accum,$new_row(RANGE_ID,start_no,0,NULL));
    $val_serialize(INT_ID,&self->start,start_no,accum);
    $val_serialize(INT_ID,&self->stop,start_no,accum);
    $val_serialize(INT_ID,&self->step,start_no,accum);
  }
}

$range $range$__deserialize__($Mapping$dict wit, $ROW* row, $dict done) {
  $ROW this = *row;
  *row = this->next;
  if (this->class_id < 0) {
    return $dict_get(done,wit->w$Hashable$Mapping,to$int((long)this->blob[0]),NULL);
  } else {
    $range res = malloc(sizeof(struct $range));
    $dict_setitem(done,wit->w$Hashable$Mapping,to$int(this->row_no),res);
    res->$class = &$range$methods;
    res->start = (int)$val_deserialize(row);
    res->stop = (int)$val_deserialize(row);
    res->step = (int)$val_deserialize(row);
    return res;
  }
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

void $Iterator$range_serialize($Iterator$range self, $Mapping$dict wit, long* start_no, $dict done, struct $ROWLISTHEADER* accum) {
  $int prevkey = ($int)$dict_get(done,wit->w$Hashable$Mapping,self,NULL);
  if (prevkey) {
    $val_serialize(-RANGEITERATOR_ID,&prevkey->val,start_no,accum);
    return;
  }
  $dict_setitem(done,wit->w$Hashable$Mapping,self,to$int(*start_no));
  $enqueue(accum,$new_row(RANGEITERATOR_ID,start_no,0,NULL));
  $step_serialize(($Serializable)self->src,wit,start_no,done,accum);
  $val_serialize(INT_ID,&self->nxt,start_no,accum);
}

struct $Iterator$range$class $Iterator$range$methods;

$Iterator$range $Iterator$range$_deserialize($Mapping$dict wit, $ROW* row, $dict done) {
  $ROW this = *row;
  *row = this->next;
  if (this->class_id < 0) {
    return $dict_get(done,wit->w$Hashable$Mapping,to$int((long)this->blob[0]),NULL);
  } else {
    $Iterator$range res = malloc(sizeof(struct $Iterator$range));
    $dict_setitem(done,wit->w$Hashable$Mapping,to$int(this->row_no),res);
    res->$class = &$Iterator$range$methods;
    res->src = ($range)$step_deserialize(wit,row,done);
    res->nxt = (int)$val_deserialize(row);
    return res;
  }
}

struct $range$class $range$methods = {"",NULL,$range$__init__,$range$__serialize__,$range$__deserialize__};


struct $Iterator$range$class $Iterator$range$methods = {"",($Super$class)&$Iterator$methods, $Iterator$range_init,
                                                        $Iterator$range_serialize, $Iterator$range$_deserialize, $Iterator$range_next};

struct $Iterator$range $Iterator$range$instance = {&$Iterator$range$methods};
struct $Iterator$range *$Iterator$range$witness = &$Iterator$range$instance;

void $Iterable$range$__init__ ($Iterable$range wit){
  return;
}

$Iterator $Iterable$range$__iter__ ($Iterable$range wit, $range rng) {
  return ($Iterator)$NEW($Iterator$range,rng);
}

struct $Iterable$range$class $Iterable$range$methods = {"",NULL,$Iterable$range$__init__,$Iterable$range$__iter__};

struct $Iterable$range $Iterable$range_instance = {&$Iterable$range$methods};
$Iterable$range $Iterable$range$witness = &$Iterable$range_instance;

