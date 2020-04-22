#include <math.h>

$None $float_serialize($float self, $Mapping$dict notused,$WORD *prefix, int prefix_size, $dict done, $ROWLISTHEADER accum);
$float $float_deserialize($Mapping$dict notused, $ROW *row, $dict done);

struct $float$class $float$methods = {"",$float_serialize, $float_deserialize};


// Serialization ///////////////////////////////////////////////////////////////////////

$None $float_serialize($float x, $Mapping$dict notused, $WORD *prefix, int prefix_size, $dict done, $ROWLISTHEADER accum) {
  $ROW row = $new_row(FLOAT_ID,prefix_size,1,prefix);
  double dx = from$float(x);
  memcpy(row->data+prefix_size,&dx,sizeof(double)); //Here we rely on sizeof(double) = sizeof($WORD)...
  $enqueue(accum,row);
}

$float $float_deserialize($Mapping$dict notused, $ROW *row, $dict done) {
  $ROW this = *row;
  *row =this->next;
  return to$float((long)this->data[this->prefix_size]);
}

  
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
  return to$float((double)x->val);
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
  
$Integral$opaque $Real$float$__round__ ($Real$float wit, $float x) {
  return $Integral$pack(($Integral)$Integral$int$witness,to$int((long)round(from$float(x))));
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

$bool $Complex$float$__bool__($Complex$float wit, $float a) {
  return from$float(a)==0.0 ? $true : $false;
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
 
 struct $Real$float $Real$float_instance;
 struct $Complex$float $Complex$float_instance;
 struct $Plus$float $Plus$float_instance;
 struct $Minus$float $Minus$float_instance;
 struct $Hashable$float $Hashable$float_instance;

 struct $Real$float$class $Real$float$methods = {"", $Real$float$__eq__ , $Real$float$__ne__ , $Real$float$__lt__ , $Real$float$__le__ ,
                                                     $Real$float$__gt__ , $Real$float$__ge__ , $Real$float$__float__ , $Real$float$__trunc__ , $Real$float$__floor__ ,
                                                     $Real$float$__ceil__ , $Real$float$__round__};
 struct $Real$float $Real$float_instance = {&$Real$float$methods};
 $Real$float $Real$float$witness = &$Real$float_instance;


 struct $Complex$float$class $Complex$float$methods = {"",$Complex$float$__eq__,$Complex$float$__ne__,$Complex$float$__complx__,
                                               $Complex$float$__bool__,$Complex$float$__mul__,$Complex$float$__truediv__,$Complex$float$__pow__,$Complex$float$__neg__,
                                               $Complex$float$__pos__,$Complex$float$real,$Complex$float$imag,$Complex$float$__abs__,$Complex$float$__conjugate__};
 struct $Complex$float $Complex$float_instance = {&$Complex$float$methods, ($Real)&$Real$float_instance, ($Plus)&$Plus$float_instance, ($Minus)&$Minus$float_instance};
 $Complex$float $Complex$float$witness = &$Complex$float_instance;

 struct $Plus$float$class $Plus$float$methods = {"",$Plus$float$__add__};
 struct $Plus$float $Plus$float_instance = {&$Plus$float$methods, ($Real)&$Real$float_instance};
 $Plus$float $Plus$float$witness = &$Plus$float_instance;

 struct $Minus$float$class $Minus$float$methods = {"",$Minus$float$__sub__};
 struct $Minus$float $Minus$float_instance = {&$Minus$float$methods, ($Real)&$Real$float_instance};
 $Minus$float $Minus$float$witness = &$Minus$float_instance;

 struct $Hashable$float$class $Hashable$float$methods = {"",$Hashable$float$__eq__,$Hashable$float$__neq__,$Hashable$float$__hash__};
 struct $Hashable$float $Hashable$float_instance = {&$Hashable$float$methods};
 $Hashable$float $Hashable$float$witness = &$Hashable$float_instance;
 
