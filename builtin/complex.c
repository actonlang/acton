$complex to$complex(complex double c) {
  $complex res = malloc(sizeof(struct $complex));
  res->$class = &$complex$methods;
  res->val = c;
  return res;
}

void $complex_init($complex self, $Number$opaque c){
  self->val = c->proto->$class->__complx__(c->proto,c->impl)->val;
}

void $complex_serialize($complex c,$Serial$state state) {
  $ROW row = $add_header(STR_ID,2,state);
  double re = creal(c->val);
  double im = cimag(c->val);
  memcpy(row->blob,&re,sizeof(double));
  memcpy(row->blob+1,&im,sizeof(double));
}

$complex $complex_deserialize($Serial$state state) {
  $ROW this = state->row;
  state->row =this->next;
  state->row_no++;
  double re, im;
  memcpy(&re,this->blob,sizeof(double));
  memcpy(&im,this->blob+1,sizeof(double));
  return to$complex(re + im * _Complex_I);
}

$bool $complex_bool($complex n) {
  return to$bool(n->val != 0.0);
}

$str $complex_str($complex c) {
  char *s;
  asprintf(&s,"%f + %f*I",creal(c->val),cimag(c->val));
  return to$str(s);
}
  
struct $complex$class $complex$methods = {"",UNASSIGNED,($Super$class)&$struct$methods,$complex_init,$complex_serialize,$complex_deserialize,$complex_bool,$complex_str};

// $Number$complex  ////////////////////////////////////////////////////////////////////////////////////////

$bool $Number$complex$__eq__ ($Number$complex wit, $complex a, $complex b) {
  return to$bool(creal(a->val) == creal(b->val) && cimag(a->val) == cimag(b->val));
}

$bool $Number$complex$__ne__ ($Number$complex wit, $complex a, $complex b) {
  return to$bool(!from$bool($Number$complex$__eq__(wit,a,b)));
}

$complex $Number$complex$__complx__ ($Number$complex wit, $complex c) {
  return c;
}

$complex $Number$complex$__mul__ ($Number$complex wit, $complex a, $complex b){
  return to$complex(a->val * b->val);
}

$complex $Number$complex$__truediv__ ($Number$complex wit, $complex a, $complex b) {
  return to$complex(a->val/b->val);
}

$complex $Number$complex$__pow__ ($Number$complex wit, $complex a, $complex b) {
  return to$complex(cpow(a->val,b->val));
}

$complex $Number$complex$__neg__ ($Number$complex wit, $complex c){
  return to$complex(-c->val);
}

$complex $Number$complex$__pos__ ($Number$complex wit, $complex c) {
  return c;
}

$WORD $Number$complex$real ($Number$complex wit, $Real wit2, $complex c) {
  $Number wit3 = wit2->w$Number$Real;
  return wit3->$class->__fromatom__(wit3,to$float(creal(c->val)));
}

$WORD $Number$complex$imag ($Number$complex wit, $Real wit2, $complex c) {
  $Number wit3 = wit2->w$Number$Real;
  return wit3->$class->__fromatom__(wit3,to$float(cimag(c->val)));
}

$WORD $Number$complex$__abs__ ($Number$complex wit, $Real wit2, $complex c) {
  $Number wit3 = wit2->w$Number$Real;
  return wit3->$class->__fromatom__(wit3,to$float(cabs(c->val)));
}

$complex $Number$complex$conjugate ($Number$complex wit, $complex c) {
  return to$complex(conj(c->val));
}

// $Plus$complex  ////////////////////////////////////////////////////////////////////////////////////////

$complex $Plus$complex$__add__($Plus$complex wit, $complex a, $complex b) {
  return to$complex(a->val + b->val);
}  

// $Minus$complex  ////////////////////////////////////////////////////////////////////////////////////////

$complex $Minus$complex$__sub__($Minus$complex wit, $complex a, $complex b) {
  return to$complex(a->val - b->val);
}  

// $Hashable$complex  ////////////////////////////////////////////////////////////////////////////////////////

$bool $Hashable$complex$__eq__($Hashable$complex wit, $complex a, $complex b) {
  return to$bool(creal(a->val) == creal(b->val) && cimag(a->val) == cimag(b->val));
}

$bool $Hashable$complex$__ne__($Hashable$complex wit, $complex a, $complex b) {
  return to$bool(!from$bool($Hashable$complex$__eq__(wit,a,b)));
}

$int $Hashable$complex$__hash__($Hashable$complex wit, $complex a) {
  return to$int($complex_hash(a));
}

// init methods ////////////////////////////////////////////////////////////////////////////////////////////////

void $Number$complex_init ($Number$complex wit) {
  wit-> w$Plus$Number = $NEW($Plus$complex,wit);
  wit-> w$Minus$Number = $NEW($Minus$complex,wit);
}

void $Plus$complex_init($Plus$complex wit, $Number$complex w$Number$complex) {
  wit->w$Number$complex =  w$Number$complex;
}

void $Minus$complex_init($Minus$complex wit, $Number$complex w$Number$complex) {
  wit->w$Number$complex =  w$Number$complex;
}

struct $Number$complex $Number$complex_instance;
struct $Plus$complex $Plus$complex_instance;
struct $Minus$complex $Minus$complex_instance;
struct $Hashable$complex $Hashable$complex_instance;

struct $Number$complex$class $Number$complex$methods = {"", UNASSIGNED,NULL, $Number$complex_init,$Number$complex$__eq__,$Number$complex$__ne__,$Number$complex$__complx__,
                                               $Number$complex$__mul__,$Number$complex$__truediv__,$Number$complex$__pow__,$Number$complex$__neg__,
                                               $Number$complex$__pos__,$Number$complex$real,$Number$complex$imag,$Number$complex$__abs__,$Number$complex$conjugate};
 struct $Number$complex $Number$complex_instance = {&$Number$complex$methods, &$Plus$complex_instance, &$Minus$complex_instance};
 $Number$complex $Number$complex$witness = &$Number$complex_instance;

struct $Plus$complex$class $Plus$complex$methods = {"", UNASSIGNED,NULL, $Plus$complex_init,$Plus$complex$__add__};
 struct $Plus$complex $Plus$complex_instance = {&$Plus$complex$methods, &$Number$complex_instance};
 $Plus$complex $Plus$complex$witness = &$Plus$complex_instance;

struct $Minus$complex$class $Minus$complex$methods = {"", UNASSIGNED,NULL, $Minus$complex_init,$Minus$complex$__sub__};
 struct $Minus$complex $Minus$complex_instance = {&$Minus$complex$methods, &$Number$complex_instance};
 $Minus$complex $Minus$complex$witness = &$Minus$complex_instance;

struct $Hashable$complex$class $Hashable$complex$methods = {"",UNASSIGNED, NULL, (void (*)($Hashable$complex))$default__init__,$Hashable$complex$__eq__,$Hashable$complex$__ne__,$Hashable$complex$__hash__};
 struct $Hashable$complex $Hashable$complex_instance = {&$Hashable$complex$methods};
 $Hashable$complex $Hashable$complex$witness = &$Hashable$complex_instance;
