$complex to$complex(complex double c) {
  $complex res = malloc(sizeof(struct $complex));
  res->$class = &$complex$methods;
  res->val = c;
  return res;
}

$complex $complex$new($Number wit, $WORD c) {
  return $NEW($complex,wit,c);
}

void $complex_init($complex self, $Number wit, $WORD c){
  self->val = wit->$class->__complx__(wit,c)->val;
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

void $Number$complex$__serialize__($Number$complex self, $Serial$state state) {
  $step_serialize(self->w$Minus, state);
}

$Number$complex $Number$complex$__deserialize__($Serial$state state) {
   $Number$complex res = $DNEW($Number$complex,state);
   res->w$Minus = ($Minus)$step_deserialize(state);
   return res;
}

$complex $Number$complex$__add__($Number$complex wit, $complex a, $complex b) {
  return to$complex(a->val + b->val);
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
  return wit2->$class->__fromatom__(wit2,($atom)to$float(creal(c->val)));
}

$WORD $Number$complex$imag ($Number$complex wit, $Real wit2, $complex c) {
  return wit2->$class->__fromatom__(wit2,($atom)to$float(cimag(c->val)));
}

$WORD $Number$complex$__abs__ ($Number$complex wit, $Real wit2, $complex c) {
  return wit2->$class->__fromatom__(wit2,($atom)to$float(cabs(c->val)));
}

$complex $Number$complex$conjugate ($Number$complex wit, $complex c) {
  return to$complex(conj(c->val));
}

// $Minus$complex  ////////////////////////////////////////////////////////////////////////////////////////

void $Minus$complex$__serialize__($Minus$complex self, $Serial$state state) {
  $step_serialize(self->w$Number, state);
}

$Minus$complex $Minus$complex$__deserialize__($Serial$state state) {
   $Minus$complex res = $DNEW($Minus$complex,state);
   res->w$Number = ($Number)$step_deserialize(state);
   return res;
}

$complex $Minus$complex$__sub__($Minus$complex wit, $complex a, $complex b) {
  return to$complex(a->val - b->val);
}  
// $Eq$complex  ////////////////////////////////////////////////////////////////////////////////////////

void $Eq$complex$__serialize__($Eq$complex self, $Serial$state state) {
}

$Eq$complex $Eq$complex$__deserialize__($Serial$state state) {
   $Eq$complex res = $DNEW($Eq$complex,state);
   return res;
}

$bool $Eq$complex$__eq__ ($Eq$complex wit, $complex a, $complex b) {
  return to$bool(creal(a->val) == creal(b->val) && cimag(a->val) == cimag(b->val));
}

$bool $Eq$complex$__ne__ ($Eq$complex wit, $complex a, $complex b) {
  return to$bool(!from$bool($Eq$complex$__eq__(wit,a,b)));
}


// $Hashable$complex  ////////////////////////////////////////////////////////////////////////////////////////

void $Hashable$complex$__serialize__($Hashable$complex self, $Serial$state state) {
}

$Hashable$complex $Hashable$complex$__deserialize__($Serial$state state) {
   $Hashable$complex res = $DNEW($Hashable$complex,state);
   return res;
}

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
  wit-> w$Minus = ($Minus)$NEW($Minus$complex,($Number)wit);
}

void $Minus$complex_init($Minus$complex wit, $Number w$Number) {
  wit->w$Number =  w$Number;
}

void $Eq$complex_init($Eq$complex wit) {
  return;
}

void $Hashable$complex_init($Hashable$complex wit) {
  return;
}

$Number$complex $Number$complex$new() {
  return $NEW($Number$complex);
}

$Minus$complex $Minus$complex$new($Number wit) {
  return $NEW($Minus$complex,wit);
}
  
$Eq$complex $Eq$complex$new() {
  return $NEW($Eq$complex);
}

$Hashable$complex $Hashable$complex$new() {
  return $NEW($Hashable$complex);
}


struct $Number$complex $Number$complex_instance;
struct $Minus$complex $Minus$complex_instance;
struct $Eq$complex $Eq$complex_instance;
struct $Hashable$complex $Hashable$complex_instance;

struct $Number$complex$class $Number$complex$methods = {
    "$Number$complex",
    UNASSIGNED,
    ($Super$class)&$Number$methods,
    $Number$complex_init,
    $Number$complex$__serialize__,
    $Number$complex$__deserialize__,
    ($bool (*)($Number$complex))$default__bool__,
    ($str (*)($Number$complex))$default__str__,
    $Number$complex$__add__,
    ($complex (*)($Number$complex, $complex, $complex))$Plus$__iadd__,
    NULL,        // fromatom
    $Number$complex$__complx__,
    $Number$complex$__mul__,
    $Number$complex$__truediv__,
    $Number$complex$__pow__,
    $Number$complex$__neg__,
    $Number$complex$__pos__,
    $Number$complex$real,
    $Number$complex$imag,
    $Number$complex$__abs__,
    $Number$complex$conjugate
};
struct $Number$complex $Number$complex_instance = {&$Number$complex$methods, ($Minus)&$Minus$complex_instance};
$Number$complex $Number$complex$witness = &$Number$complex_instance;

struct $Minus$complex$class $Minus$complex$methods = {
    "$Minus$complex",
    UNASSIGNED,
    ($Super$class)&$Minus$methods,
    $Minus$complex_init,
    $Minus$complex$__serialize__,
    $Minus$complex$__deserialize__,
    ($bool (*)($Minus$complex))$default__bool__,
    ($str (*)($Minus$complex))$default__str__,
    $Minus$complex$__sub__
};
struct $Minus$complex $Minus$complex_instance = {&$Minus$complex$methods, ($Number)&$Number$complex_instance};
$Minus$complex $Minus$complex$witness = &$Minus$complex_instance;

struct $Eq$complex$class $Eq$complex$methods = {
    "$Eq$complex",
    UNASSIGNED,
    ($Super$class)&$Eq$methods,
    $Eq$complex_init,
    $Eq$complex$__serialize__,
    $Eq$complex$__deserialize__,
    ($bool (*)($Eq$complex))$default__bool__,
    ($str (*)($Eq$complex))$default__str__,
    $Eq$complex$__eq__,
    $Eq$complex$__ne__
};
struct $Eq$complex $Eq$complex_instance = {&$Eq$complex$methods};
$Eq$complex $Eq$complex$witness = &$Eq$complex_instance;

struct $Hashable$complex$class $Hashable$complex$methods = {
    "$Hashable$complex",
    UNASSIGNED,
    ($Super$class)&$Hashable$methods,
    $Hashable$complex_init,
    $Hashable$complex$__serialize__,
    $Hashable$complex$__deserialize__,
    ($bool (*)($Hashable$complex))$default__bool__,
    ($str (*)($Hashable$complex))$default__str__,
    $Hashable$complex$__eq__,
    $Hashable$complex$__ne__,
    $Hashable$complex$__hash__
};
 struct $Hashable$complex $Hashable$complex_instance = {&$Hashable$complex$methods};
 $Hashable$complex $Hashable$complex$witness = &$Hashable$complex_instance;
