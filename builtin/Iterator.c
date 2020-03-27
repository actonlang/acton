Iterator Iterable$Iterator$__iter__(Iterable$Iterator wit, Iterator self) {
  return self;
}

static struct Iterable$Iterator$__class__ Iterable$Iterator_methods = {"", Iterable$Iterator$__iter__};
static struct Iterable$Iterator Iterable$Iterator_instance = {"",&Iterable$Iterator_methods};
static Iterable$Iterator Iterable$Iterator_witness = &Iterable$Iterator_instance;

Iterable$Iterator Iterable$Iterator_new() {
  return Iterable$Iterator_witness;
}



  
$WORD next(Iterator it) {
  return it->__class__->__next__(it);
}
