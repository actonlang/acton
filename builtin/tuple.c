void $tuple_serialize($tuple self, $Serial$state state) {
  $int prevkey = ($int)$dict_get(state->done,($Hashable)$Hashable$WORD$witness,self,NULL);
  if (prevkey) {
    $val_serialize(-TUPLE_ID,&prevkey->val,state);
    return;
  }
  $dict_setitem(state->done,($Hashable)$Hashable$WORD$witness,self,to$int(state->row_no));
  long len = (long)self->size;
  $val_serialize(TUPLE_ID,&len,state);
  for (int i=0; i<self->size; i++) {
    $step_serialize(self->components[i],state);
  }
}


$tuple $tuple_deserialize($Serial$state state) {
  $ROW this = state->row;
  state->row = this->next;
  state->row_no++;
  if (this->class_id < 0) {
    return ($tuple)$dict_get(state->done,($Hashable)$Hashable$int$witness,to$int((long)this->blob[0]),NULL);
  } else {
    int len = (int)(long)this->blob[0];
    $tuple res = malloc(sizeof(struct $tuple));
    $dict_setitem(state->done,($Hashable)$Hashable$int$witness,to$int(state->row_no-1),res);
    $WORD *comps = malloc(len * sizeof($WORD));
    res->$class = &$tuple$methods;
    res->size = len;
    for (int i = 0; i < len; i++) 
      res->components[i] = $step_deserialize(state);
    return res;
  }
}

void $tuple_init($tuple self,int size ,$WORD *comps) {
  self->size = size;
  self->components = comps;
}

struct $tuple$class $tuple$methods = {
    "tuple",
    UNASSIGNED,
    NULL,
    $tuple_init,
    $tuple_serialize,
    $tuple_deserialize
};

// Iterators over tuples ///////////////////////////////////////////////////////

static $WORD $Iterator$tuple_next($Iterator$tuple self) {
  return self->nxt >= self->src->size ? NULL : self->src->components[self->nxt++];
}

void $Iterator$tuple_init($Iterator$tuple self, $tuple lst) {
  self->src = lst;
  self->nxt = 0;
}

void $Iterator$tuple_serialize($Iterator$tuple self,$Serial$state state) {
  $step_serialize(self->src,state);
  $step_serialize(to$int(self->nxt),state);
}

$Iterator$tuple $Iterator$tuple$_deserialize($Serial$state state) {
   $Iterator$tuple res = $DNEW($Iterator$tuple,state);
   res->src = $step_deserialize(state);
   res->nxt = from$int(($int)$step_deserialize(state));
   return res;
}

struct $Iterator$tuple$class $Iterator$tuple$methods = {"",UNASSIGNED,($Super$class)&$Iterator$methods, $Iterator$tuple_init,
                                                      $Iterator$tuple_serialize, $Iterator$tuple$_deserialize, $Iterator$tuple_next};


// Iterable ///////////////////////////////////////////////////////////////

$Iterator $Iterable$tuple$__iter__($Iterable$tuple wit, $tuple self) {
  return ($Iterator)$NEW($Iterator$tuple,self);
}

void $Iterable$tuple$__init__($Iterable$tuple self) {
  return;
}

struct $Iterable$tuple$class $Iterable$tuple$methods = {"",UNASSIGNED, NULL,$Iterable$tuple$__init__,$Iterable$tuple$__iter__};
struct $Iterable$tuple $Iterable$tuple$instance = {&$Iterable$tuple$methods};
struct $Iterable$tuple *$Iterable$tuple$witness = &$Iterable$tuple$instance;

// Sliceable ///////////////////////////////////////////////////////////////

void $Sliceable$tuple$__init__ ($Sliceable$tuple wit) {
  return;
}

$WORD $Sliceable$tuple$__getitem__ ($Sliceable$tuple wit, $tuple self, $int n) {
  int size = self->size;
  int ix = (int)from$int(n);
  int ix0 = ix < 0 ? size + ix : ix;
  if (ix0 < 0 || ix0 >= size) {
    RAISE(($BaseException)$NEW($IndexError,from$UTF8("getitem: indexing outside tuple")));
  }
  return self->components[ix0];
}


void $Sliceable$tuple$__setitem__ ($Sliceable$tuple wit, $tuple self, $int ix, $WORD elem) {
  fprintf(stderr,"%s\n","internal error: setitem on immutable tuple");
  exit(-1);
}

void $Sliceable$tuple$__delitem__ ($Sliceable$tuple wit, $tuple self, $int ix) {
  fprintf(stderr,"%s\n","internal error: delitem on immutable tuple");
  exit(-1);
}
  

$tuple $Sliceable$tuple$__getslice__ ($Sliceable$tuple wit, $tuple self, $Slice slc) {
  int size = self->size;
  int start, stop, step, slen;
  normalize_slice(slc, size, &slen, &start, &stop, &step);
  //slice notation have been eliminated and default values applied.
  // slen now is the length of the slice
  $tuple res = malloc(sizeof(struct $tuple));
  res->$class = self->$class;
  res->size = slen;
  res->components = malloc(slen * sizeof($WORD));
  int t = start;
  for (int i=0; i<slen; i++) {
    res->components[i] = self->components[t];
    t += step;
  }
  return res;
}

void $Sliceable$tuple$__setslice__ ($Sliceable$tuple wit, $tuple self, $Slice slc, $Iterable$opaque it) {
    fprintf(stderr,"%s\n","internal error: setslice on immutable tuple");
  exit(-1);
}

void $Sliceable$tuple$__delslice__ ($Sliceable$tuple wit, $tuple self, $Slice slc) {
    fprintf(stderr,"%s\n","internal error: delslice on immutable tuple");
  exit(-1);
}

struct $Sliceable$tuple$class $Sliceable$tuple$methods = {"",UNASSIGNED, NULL,$Sliceable$tuple$__init__,
                                                          $Sliceable$tuple$__getitem__,$Sliceable$tuple$__setitem__,$Sliceable$tuple$__delitem__,
                                                          $Sliceable$tuple$__getslice__, $Sliceable$tuple$__setslice__, $Sliceable$tuple$__delslice__};
struct  $Sliceable$tuple $Sliceable$tuple$instance = {&$Sliceable$tuple$methods};
struct $Sliceable$tuple *$Sliceable$tuple$witness = &$Sliceable$tuple$instance;

// Hashable ///////////////////////////////////////////////////////////////

void $Hashable$tuple$__init__ ($Hashable$tuple wit, int n, $Hashable *comps) {
  wit->w$Hashable$tuple$size = n;
  wit->w$Hashable$tuple = comps;
}

$bool $Hashable$tuple$__eq__ ($Hashable$tuple wit, $tuple tup1, $tuple tup2) {
  //type-checking guarantees that sizes are equal
  for (int i=0; i<tup1->size; i++)
    if (!wit->w$Hashable$tuple[i]->$class->__eq__(wit->w$Hashable$tuple[i],tup1->components[i],tup1->components[i]))
      return $false;
  return $true;
}

$bool $Hashable$tuple$__ne__ ($Hashable$tuple wit, $tuple tup1, $tuple tup2) {
  return to$bool(!from$bool($Hashable$tuple$__eq__(wit,tup1,tup2)));
}
    
  
$int $Hashable$tuple$__hash__ ($Hashable$tuple wit, $tuple tup) {
  return to$int($tuple_hash(wit,tup));
}

struct $Hashable$tuple$class $Hashable$tuple$methods = {"",UNASSIGNED, NULL,$Hashable$tuple$__init__,
                                                          $Hashable$tuple$__eq__,$Hashable$tuple$__ne__,$Hashable$tuple$__hash__};
