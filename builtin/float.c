#include <math.h>

// General methods ///////////////////////////////////////////////////////////////////////

void $float_init($float self, $Real$opaque x){
  self->val = x->proto->$class->__float__(x->proto,x->impl)->val;
}

void $float_serialize($float self, $Serial$state state) {
  $val_serialize(FLOAT_ID,&self->val,state);
}

$float $float_deserialize($Serial$state state) {
 $WORD w = $val_deserialize(state);
 double x;
 memcpy(&x,&w,sizeof($WORD));
 return to$float(x);
}

$bool $float_bool($float x) {
  return to$bool(x->val != 0.0);
}

$str $float_str($float x) {
  char *s;
  asprintf(&s,"%g",x->val);
  return to$str(s);
}

struct $float$class $float$methods = {"",UNASSIGNED,($Super$class)&$struct$methods,$float_init,$float_serialize, $float_deserialize,$float_bool,$float_str};
  
$float to$float(double x) {
  $float res = malloc(sizeof(struct $float));
  res->$class = &$float$methods;
  res->val = x;
  return res;
}

double from$float($float x) {
  return x->val;
}

// $Real$float /////////////////////////////////////////////////////////////////////////

$bool $Real$float$__eq__ ($Real$float wit, $float a, $float b) {
  return to$bool(a->val == b->val);
}

$bool $Real$float$__ne__ ($Real$float wit, $float a, $float b) {
  return to$bool(a->val != b->val);
}

$bool $Real$float$__lt__ ($Real$float wit, $float a, $float b) {
  return to$bool(a->val < b->val);
}

$bool $Real$float$__le__ ($Real$float wit, $float a, $float b) {
  return to$bool(a->val <= b->val);
}

$bool $Real$float$__gt__ ($Real$float wit, $float a, $float b) {
  return to$bool(a->val > b->val);
}

$bool $Real$float$__ge__ ($Real$float wit, $float a, $float b) {
  return to$bool(a->val == b->val);
}

$float $Real$float$__float__ ($Real$float wit, $float x) {
  return x;
}

$Integral$opaque $Real$float$__trunc__ ($Real$float wit, $float x) {
  return $Integral$pack(($Integral)$Integral$int$witness,to$int((long)trunc(from$float(x))));
}
  
$Integral$opaque $Real$float$__floor__ ($Real$float wit, $float x) {
  return $Integral$pack(($Integral)$Integral$int$witness,to$int((long)floor(from$float(x))));
}
  
$Integral$opaque $Real$float$__ceil__ ($Real$float wit, $float x) {
  return $Integral$pack(($Integral)$Integral$int$witness,to$int((long)ceil(from$float(x))));
}
  
$float $Real$float$__round__ ($Real$float wit, $float x, $int p) {
  double pval = p==NULL ? 0.0 : (double)p->val;
  double p10 = pow(10.0,pval);
  return to$float(round(x->val * p10)/p10);
}
    

// $Complex$float //////////////////////////////////////////////////////////////////////////////////////

$bool $Complex$float$__eq__ ($Complex$float wit, $float a, $float b) {
  return to$bool(a->val == b->val);
}

$bool $Complex$float$__ne__ ($Complex$float wit, $float a, $float b) {
  return to$bool(a->val != b->val);
}

$complex $Complex$float$__complx__($Complex$float wit, $float a) {
  return to$complex(to$float(from$float(a)),to$float(0.0));
}

$float $Complex$float$__mul__($Complex$float wit,  $float a, $float b) {
  return to$float(from$float(a) * from$float(b));
}  

// The typechecker will reject true division between two integers.
$float $Complex$float$__truediv__($Complex$float wit,  $float a, $float b) {
  return to$float(from$float(a) / from$float(b));
}  

 
$float $Complex$float$__pow__($Complex$float wit,  $float a, $float b) {
  return to$float(exp(from$float(b) * log(from$float(a))));
  }

$float $Complex$float$__neg__($Complex$float wit,  $float a) {
  return to$float(-from$float(a));
}

$float $Complex$float$__pos__($Complex$float wit,  $float a) {
  return a;
}

$Real$opaque $Complex$float$real($Complex$float wit,  $float a) {
  return $Real$pack(($Real)wit,a);
}

$Real$opaque $Complex$float$imag($Complex$float wit,  $float a) {
  return  $Real$pack(($Real)wit,to$float(0.0));
}

$Real$opaque $Complex$float$__abs__($Complex$float wit,  $float a) {
  return  $Real$pack(($Real)wit,to$float(fabs(from$float(a))));
}

$float $Complex$float$__conjugate__($Complex$float wit,  $float a) {
  return a;
}

// $Plus$float  ////////////////////////////////////////////////////////////////////////////////////////

$float $Plus$float$__add__($Plus$float wit,  $float a, $float b) {
  return to$float(from$float(a) + from$float(b));
}  
 
// $Minus$float  ////////////////////////////////////////////////////////////////////////////////////////

$float $Minus$float$__sub__($Minus$float wit,  $float a, $float b) {
  return to$float(from$float(a) - from$float(b));
}  

// $Hashable$float ///////////////////////////////////////////////////////////////////////////////////////////////////////

$bool $Hashable$float$__eq__($Hashable$float wit, $float a, $float b) {
  return to$bool(a->val == b->val);
}

$bool $Hashable$float$__neq__($Hashable$float wit, $float a, $float b) {
  return to$bool(a->val != b->val);
}

$int $Hashable$float$__hash__($Hashable$float wit, $float a) {
  return to$int($float_hash(a));
}

void $Real$float_init($Real$float wit) {
  wit-> w$Complex$Real = $NEW($Complex$float,wit);
};

void $Complex$float_init($Complex$float wit, $Real$float w$Real$float) {
  wit->w$Real$float = w$Real$float;
  wit-> w$Plus$Complex = $NEW($Plus$float,wit);
  wit-> w$Minus$Complex = $NEW($Minus$float,wit);
}

void $Plus$float_init($Plus$float wit, $Complex$float w$Complex$float) {
  wit->w$Complex$float =  w$Complex$float;
}

void $Minus$float_init($Minus$float wit, $Complex$float w$Complex$float) {
  wit->w$Complex$float =  w$Complex$float;
}

 struct $Real$float $Real$float_instance;
 struct $Complex$float $Complex$float_instance;
 struct $Plus$float $Plus$float_instance;
 struct $Minus$float $Minus$float_instance;
 struct $Hashable$float $Hashable$float_instance;

struct $Real$float$class $Real$float$methods = {"", UNASSIGNED,NULL, $Real$float_init,$Real$float$__eq__ , $Real$float$__ne__ , $Real$float$__lt__ , $Real$float$__le__ ,
                                                     $Real$float$__gt__ , $Real$float$__ge__ , $Real$float$__float__ , $Real$float$__trunc__ , $Real$float$__floor__ ,
                                                     $Real$float$__ceil__ , $Real$float$__round__};
 struct $Real$float $Real$float_instance = {&$Real$float$methods};
 $Real$float $Real$float$witness = &$Real$float_instance;


struct $Complex$float$class $Complex$float$methods = {"", UNASSIGNED,NULL, $Complex$float_init,$Complex$float$__eq__,$Complex$float$__ne__,$Complex$float$__complx__,
                                               $Complex$float$__mul__,$Complex$float$__truediv__,$Complex$float$__pow__,$Complex$float$__neg__,
                                               $Complex$float$__pos__,$Complex$float$real,$Complex$float$imag,$Complex$float$__abs__,$Complex$float$__conjugate__};
 struct $Complex$float $Complex$float_instance = {&$Complex$float$methods, &$Real$float_instance, &$Plus$float_instance, &$Minus$float_instance};
 $Complex$float $Complex$float$witness = &$Complex$float_instance;

struct $Plus$float$class $Plus$float$methods = {"", UNASSIGNED,NULL, $Plus$float_init,$Plus$float$__add__};
 struct $Plus$float $Plus$float_instance = {&$Plus$float$methods, &$Complex$float_instance};
 $Plus$float $Plus$float$witness = &$Plus$float_instance;

struct $Minus$float$class $Minus$float$methods = {"", UNASSIGNED,NULL, $Minus$float_init,$Minus$float$__sub__};
 struct $Minus$float $Minus$float_instance = {&$Minus$float$methods, &$Complex$float_instance};
 $Minus$float $Minus$float$witness = &$Minus$float_instance;

struct $Hashable$float$class $Hashable$float$methods = {"",UNASSIGNED, NULL, (void (*)($Hashable$float))$default__init__,$Hashable$float$__eq__,$Hashable$float$__neq__,$Hashable$float$__hash__};
 struct $Hashable$float $Hashable$float_instance = {&$Hashable$float$methods};
 $Hashable$float $Hashable$float$witness = &$Hashable$float_instance;
 
