
static struct Sequence$list  Sequence$list_instance;
static struct Collection$list Collection$list_instance;
static struct Plus$list Plus$list_instance;

static struct Sequence$list$__class__ Sequence$list_methods = {"",Sequence$list$__getitem__, Sequence$list$__setitem__, Sequence$list$__delitem__,
                                                                  Sequence$list$__getslice__, Sequence$list$__setslice__, Sequence$list$__delslice__,
                                                               Sequence$list$__reversed__,Sequence$list$insert,Sequence$list$append,Sequence$list$reverse};
                                                       
static struct Sequence$list Sequence$list_instance = {"",&Sequence$list_methods, (Collection)&Collection$list_instance,(Plus)&Plus$list_instance};
static Sequence$list Sequence$list_witness = &Sequence$list_instance;

static struct Collection$list$__class__ Collection$list_methods = {"",Collection$list$__iter__,Collection$list$__fromiter__,Collection$list$__len__};
static struct Collection$list Collection$list_instance = {"",&Collection$list_methods,(Sequence)&Sequence$list_instance};
static Collection$list Collection$list_witness = &Collection$list_instance;


static struct Plus$list$__class__ Plus$list_methods = {"", Plus$list$__add__};
static struct Plus$list Plus$list_instance = {"", &Plus$list_methods, (Sequence)&Sequence$list_instance};
static Plus$list Plus$list_witness = &Plus$list_instance;

static struct Container$list$__class__ Container$list_methods = {"",(Iterator (*)(Container$list, $list))Collection$list$__iter__,
                                                                 ($list (*)(Container$list, Iterable$opaque))Collection$list$__fromiter__,
                                                                 ($int (*)(Container$list, $list))Collection$list$__len__,
                                                                  Container$list$__contains__,Container$list$__containsnot__};

Sequence$list Sequence$list_new() {
  return Sequence$list_witness;
}

Container$list Container$list_new(Eq _EqA) {
  Container$list res = malloc(sizeof(struct Container$list));
  res->__class__ = &Container$list_methods;
  res->_Eq = _EqA;
  return res;
}



$list Plus$list$__add__ (Plus$list wit, $list a, $list b) {
  return $list_add(a,b);
}

Iterator Collection$list$__iter__(Collection$list wit, $list self) {
  return $list_iter(self);
}

$list Collection$list$__fromiter__(Collection$list wit, Iterable$opaque it) {
  Iterator iter;
  if (it == NULL)
    iter = NULL;
  else
    iter = it->__proto__->__class__->__iter__(it->__proto__, it->__impl__);
  return $list_fromiter(iter);
}
 
$int Collection$list$__len__(Collection$list wit, $list self) {
  return to$int($list_len(self));
}

$WORD Sequence$list$__getitem__(Sequence$list wit, $list self, $int ix) {
  return $list_getitem(self,from$int(ix));
}

None Sequence$list$__setitem__(Sequence$list wit, $list self, $int ix, $WORD val) {
  $list_setitem(self,from$int(ix),val);
}

None Sequence$list$__delitem__(Sequence$list wit, $list self, $int ix) {
  $list_delitem(self,from$int(ix));
}

$list Sequence$list$__getslice__(Sequence$list wit, $list self, Slice slice) {
  return $list_getslice(self,slice);
}

None Sequence$list$__setslice__(Sequence$list wit, $list self, Slice slice, Iterable$opaque it) {
  $list_setslice(self,slice,it->__proto__->__class__->__iter__(it->__proto__,it->__impl__));
}

None Sequence$list$__delslice__(Sequence$list wit, $list self, Slice slice) {
  $list_delslice(($list)self,slice);
}  

$bool Container$list$__contains__(Container$list wit, $list self, $WORD elem) {
  return to$bool($list_contains(wit->_Eq,self,elem));
}
                 
$bool Container$list$__containsnot__(Container$list wit, $list self, $WORD elem) {
  return to$bool($list_containsnot(wit->_Eq,self,elem));
}

Iterator Iterable$list$reversed$__iter__(Iterable wit, $WORD lst) {
  return $list_reversed(lst);
}

Iterator Sequence$list$__reversed__(Sequence$list wit, $list self) {
  return $list_reversed(self);
}

void Sequence$list$insert(Sequence$list wit, $list self, $int ix, $WORD elem) {
  $list_insert(self,from$int(ix),elem);
}

void Sequence$list$append(Sequence$list wit, $list self, $WORD elem) {
  $list_append(self,elem);
}

void Sequence$list$reverse(Sequence$list wit, $list self) {
  $list_reverse(self);
}

