#include <math.h>

// General methods ///////////////////////////////////////////////////////////////////////

void $float_init($float self, $WORD x){
  self->val = $float_fromatom(x)->val;
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

$float $float_fromatom($Super a) {
  if ($ISINSTANCE(a,$int)) return to$float((double)(($int)a)->val);
  if ($ISINSTANCE(a,$float)) return ($float)a;
  if ($ISINSTANCE(a,$bool)) return to$float((double)(($bool)a)->val);
  if ($ISINSTANCE(a,$str)) {
    double x;
    int c;
    sscanf((char *)(($str)a)->str,"%lf%n",&x,&c);
    if (c==(($str)a)->nbytes)
      return to$float(x);
    else
      RAISE(($BaseException)$NEW($ValueError,to$str("float_fromatom(): invalid str literal for type float")));
  }
  fprintf(stderr,"float_fromatom: argument not of atomic type");
  exit(1);
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
    

// $Number$float //////////////////////////////////////////////////////////////////////////////////////

$bool $Number$float$__eq__ ($Number$float wit, $float a, $float b) {
  return to$bool(a->val == b->val);
}

$bool $Number$float$__ne__ ($Number$float wit, $float a, $float b) {
  return to$bool(a->val != b->val);
}

$float $Number$float$__fromatom__($Number$float wit, $WORD w) {
  return $float_fromatom(w);
}

$complex $Number$float$__complx__($Number$float wit, $float a) {
  return to$complex(a->val);
}

$float $Number$float$__mul__($Number$float wit,  $float a, $float b) {
  return to$float(from$float(a) * from$float(b));
}  

$float $Number$float$__truediv__($Number$float wit,  $float a, $float b) {
  return to$float(from$float(a) / from$float(b));
}  

$float $Number$float$__pow__($Number$float wit,  $float a, $float b) {
  return to$float(exp(from$float(b) * log(from$float(a))));
  }

$float $Number$float$__neg__($Number$float wit, $float a) {
  return to$float(-from$float(a));
}

$float $Number$float$__pos__($Number$float wit, $float a) {
  return a;
}

$Real$opaque $Number$float$real($Number$float wit, $float a) {
  return $Real$pack(($Real)$Real$float$witness,a);
}

$Real$opaque $Number$float$imag($Number$float wit,  $float a) {
  return  $Real$pack(($Real)$Real$float$witness,to$float(0.0));
}

$Real$opaque $Number$float$__abs__($Number$float wit,  $float a) {
  return  $Real$pack(($Real)$Real$float$witness,to$float(fabs(from$float(a))));
}

$float $Number$float$conjugate($Number$float wit,  $float a) {
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

// init methods ////////////////////////////////////////////////////////////////////////////////////////////////

void $Real$float_init($Real$float wit) {
  wit-> w$Number$Real = $NEW($Number$float,wit);
};

void $Number$float_init($Number$float wit, $Real$float w$Real$float) {
  wit->w$Real$float = w$Real$float;
  wit-> w$Plus$Number = $NEW($Plus$float,wit);
  wit-> w$Minus$Number = $NEW($Minus$float,wit);
}

void $Plus$float_init($Plus$float wit, $Number$float w$Number$float) {
  wit->w$Number$float =  w$Number$float;
}

void $Minus$float_init($Minus$float wit, $Number$float w$Number$float) {
  wit->w$Number$float =  w$Number$float;
}

 struct $Real$float $Real$float_instance;
 struct $Number$float $Number$float_instance;
 struct $Plus$float $Plus$float_instance;
 struct $Minus$float $Minus$float_instance;
 struct $Hashable$float $Hashable$float_instance;

struct $Real$float$class $Real$float$methods = {"", UNASSIGNED,NULL, $Real$float_init,$Real$float$__eq__ , $Real$float$__ne__ , $Real$float$__lt__ , $Real$float$__le__ ,
                                                     $Real$float$__gt__ , $Real$float$__ge__ , $Real$float$__float__ , $Real$float$__trunc__ , $Real$float$__floor__ ,
                                                     $Real$float$__ceil__ , $Real$float$__round__};
 struct $Real$float $Real$float_instance = {&$Real$float$methods};
 $Real$float $Real$float$witness = &$Real$float_instance;


struct $Number$float$class $Number$float$methods = {"", UNASSIGNED,NULL, $Number$float_init,$Number$float$__eq__,$Number$float$__ne__,$Number$float$__fromatom__,$Number$float$__complx__,
                                               $Number$float$__mul__,$Number$float$__truediv__,$Number$float$__pow__,$Number$float$__neg__,
                                               $Number$float$__pos__,$Number$float$real,$Number$float$imag,$Number$float$__abs__,$Number$float$conjugate};
 struct $Number$float $Number$float_instance = {&$Number$float$methods, &$Real$float_instance, &$Plus$float_instance, &$Minus$float_instance};
 $Number$float $Number$float$witness = &$Number$float_instance;

struct $Plus$float$class $Plus$float$methods = {"", UNASSIGNED,NULL, $Plus$float_init,$Plus$float$__add__};
 struct $Plus$float $Plus$float_instance = {&$Plus$float$methods, &$Number$float_instance};
 $Plus$float $Plus$float$witness = &$Plus$float_instance;

struct $Minus$float$class $Minus$float$methods = {"", UNASSIGNED,NULL, $Minus$float_init,$Minus$float$__sub__};
 struct $Minus$float $Minus$float_instance = {&$Minus$float$methods, &$Number$float_instance};
 $Minus$float $Minus$float$witness = &$Minus$float_instance;

struct $Hashable$float$class $Hashable$float$methods = {"",UNASSIGNED, NULL, (void (*)($Hashable$float))$default__init__,$Hashable$float$__eq__,$Hashable$float$__neq__,$Hashable$float$__hash__};
 struct $Hashable$float $Hashable$float_instance = {&$Hashable$float$methods};
 $Hashable$float $Hashable$float$witness = &$Hashable$float_instance;
 
