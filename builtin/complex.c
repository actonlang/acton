$complex to$complex(complex double c) {
  $complex res = malloc(sizeof(struct $complex));
  res->$class = &$complex$methods;
  res->val = c;
  return res;
}

void $complex_init($complex self, $Complex$opaque c){
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

// $Complex$complex  ////////////////////////////////////////////////////////////////////////////////////////

$bool $Complex$complex$__eq__ ($Complex$complex wit, $complex a, $complex b) {
  return to$bool(creal(a->val) == creal(b->val) && cimag(a->val) == cimag(b->val));
}

$bool $Complex$complex$__ne__ ($Complex$complex wit, $complex a, $complex b) {
  return to$bool(!from$bool($Complex$complex$__eq__(wit,a,b)));
}

$complex $Complex$complex$__complx__ ($Complex$complex wit, $complex c) {
  return c;
}

$complex $Complex$complex$__mul__ ($Complex$complex wit, $complex a, $complex b){
  return to$complex(a->val * b->val);
}

$complex $Complex$complex$__truediv__ ($Complex$complex wit, $complex a, $complex b) {
  return to$complex(a->val/b->val);
}

$complex $Complex$complex$__pow__ ($Complex$complex wit, $complex a, $complex b) {
  return to$complex(cpow(a->val,b->val));
}

$complex $Complex$complex$__neg__ ($Complex$complex wit, $complex c){
  return to$complex(-c->val);
}

$complex $Complex$complex$__pos__ ($Complex$complex wit, $complex c) {
  return c;
}

$Real$opaque $Complex$complex$real ($Complex$complex wit, $complex c) {
  return $Real$pack(($Real)$Real$float$witness,to$float(creal(c->val)));
}

$Real$opaque $Complex$complex$imag ($Complex$complex wit, $complex c) {
  return $Real$pack(($Real)$Real$float$witness,to$float(cimag(c->val)));
}

$Real$opaque $Complex$complex$__abs__ ($Complex$complex wit, $complex c) {
  return $Real$pack(($Real)$Real$float$witness,to$float(cabs(c->val)));
}

$complex $Complex$complex$conjugate ($Complex$complex wit, $complex c) {
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

void $Complex$complex_init ($Complex$complex wit) {
  wit-> w$Plus$Complex = $NEW($Plus$complex,wit);
  wit-> w$Minus$Complex = $NEW($Minus$complex,wit);
}

void $Plus$complex_init($Plus$complex wit, $Complex$complex w$Complex$complex) {
  wit->w$Complex$complex =  w$Complex$complex;
}

void $Minus$complex_init($Minus$complex wit, $Complex$complex w$Complex$complex) {
  wit->w$Complex$complex =  w$Complex$complex;
}

struct $Complex$complex $Complex$complex_instance;
struct $Plus$complex $Plus$complex_instance;
struct $Minus$complex $Minus$complex_instance;
struct $Hashable$complex $Hashable$complex_instance;

struct $Complex$complex$class $Complex$complex$methods = {"", UNASSIGNED,NULL, $Complex$complex_init,$Complex$complex$__eq__,$Complex$complex$__ne__,$Complex$complex$__complx__,
                                               $Complex$complex$__mul__,$Complex$complex$__truediv__,$Complex$complex$__pow__,$Complex$complex$__neg__,
                                               $Complex$complex$__pos__,$Complex$complex$real,$Complex$complex$imag,$Complex$complex$__abs__,$Complex$complex$conjugate};
 struct $Complex$complex $Complex$complex_instance = {&$Complex$complex$methods, &$Plus$complex_instance, &$Minus$complex_instance};
 $Complex$complex $Complex$complex$witness = &$Complex$complex_instance;

struct $Plus$complex$class $Plus$complex$methods = {"", UNASSIGNED,NULL, $Plus$complex_init,$Plus$complex$__add__};
 struct $Plus$complex $Plus$complex_instance = {&$Plus$complex$methods, &$Complex$complex_instance};
 $Plus$complex $Plus$complex$witness = &$Plus$complex_instance;

struct $Minus$complex$class $Minus$complex$methods = {"", UNASSIGNED,NULL, $Minus$complex_init,$Minus$complex$__sub__};
 struct $Minus$complex $Minus$complex_instance = {&$Minus$complex$methods, &$Complex$complex_instance};
 $Minus$complex $Minus$complex$witness = &$Minus$complex_instance;

struct $Hashable$complex$class $Hashable$complex$methods = {"",UNASSIGNED, NULL, (void (*)($Hashable$complex))$default__init__,$Hashable$complex$__eq__,$Hashable$complex$__ne__,$Hashable$complex$__hash__};
 struct $Hashable$complex $Hashable$complex_instance = {&$Hashable$complex$methods};
 $Hashable$complex $Hashable$complex$witness = &$Hashable$complex_instance;
