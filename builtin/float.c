#include <math.h>

struct $float$__methods__ $float_table = {$float_serialize, $float_deserialize};
$float$__methods__ $float_methods = &$float_table;

// Serialization ///////////////////////////////////////////////////////////////////////

None $float_serialize($float x, $WORD *prefix, int prefix_size, $dict done, $ROWLISTHEADER accum) {
  $ROW row = new_row(FLOAT_ID,prefix_size,1,prefix);
  double dx = from$float(x);
  memcpy(row->data+prefix_size,&dx,sizeof(double)); //Here we rely on sizeof(double) = sizeof($WORD)...
  enqueue(accum,row);
}

$float $float_deserialize($ROW *row, $dict done) {
  $ROW this = *row;
  *row =this->next;
  return to$float((long)this->data[this->prefix_size]);
}

  
$float to$float(double x) {
  $float res = malloc(sizeof(struct $float));
  res->__class__ = $float_methods;
  res->val = x;
  return res;
}

double from$float($float x) {
  return x->val;
}

// Real$float /////////////////////////////////////////////////////////////////////////

$bool Real$float$__eq__ (Real$float wit, $float a, $float b) {
  return to$bool(a->val == b->val);
}

$bool Real$float$__ne__ (Real$float wit, $float a, $float b) {
  return to$bool(a->val != b->val);
}

$bool Real$float$__lt__ (Real$float wit, $float a, $float b) {
  return to$bool(a->val < b->val);
}

$bool Real$float$__le__ (Real$float wit, $float a, $float b) {
  return to$bool(a->val <= b->val);
}

$bool Real$float$__gt__ (Real$float wit, $float a, $float b) {
  return to$bool(a->val > b->val);
}

$bool Real$float$__ge__ (Real$float wit, $float a, $float b) {
  return to$bool(a->val == b->val);
}

$float Real$float$__float__ (Real$float wit, $float x) {
  return to$float((double)x->val);
}

Integral$opaque Real$float$__trunc__ (Real$float wit, $float x) {
  return Integral$__pack__((Integral)Integral$int_new(),to$int((long)trunc(from$float(x))));
}
  
Integral$opaque Real$float$__floor__ (Real$float wit, $float x) {
  return Integral$__pack__((Integral)Integral$int_new(),to$int((long)floor(from$float(x))));
}
  
Integral$opaque Real$float$__ceil__ (Real$float wit, $float x) {
  return Integral$__pack__((Integral)Integral$int_new(),to$int((long)ceil(from$float(x))));
}
  
Integral$opaque Real$float$__round__ (Real$float wit, $float x) {
  return Integral$__pack__((Integral)Integral$int_new(),to$int((long)round(from$float(x))));
}
    

// Complex$float //////////////////////////////////////////////////////////////////////////////////////

$bool Complex$float$__eq__ (Complex$float wit, $float a, $float b) {
  return to$bool(a->val == b->val);
}

$bool Complex$float$__ne__ (Complex$float wit, $float a, $float b) {
  return to$bool(a->val != b->val);
}

$complex Complex$float$__complx__(Complex$float wit, $float a) {
  return to$complex(to$float(from$float(a)),to$float(0.0));
}

$bool Complex$float$__bool__(Complex$float wit, $float a) {
  return from$float(a)==0.0 ? $true : $false;
}

$float Complex$float$__mul__(Complex$float wit,  $float a, $float b) {
  return to$float(from$float(a) * from$float(b));
}  

// The typechecker will reject true division between two integers.
$float Complex$float$__truediv__(Complex$float wit,  $float a, $float b) {
  return to$float(from$float(a) / from$float(b));
}  

 
$float Complex$float$__pow__(Complex$float wit,  $float a, $float b) {
  return to$float(exp(from$float(b) * log(from$float(a))));
  }

$float Complex$float$__neg__(Complex$float wit,  $float a) {
  return to$float(-from$float(a));
}

$float Complex$float$__pos__(Complex$float wit,  $float a) {
  return a;
}

Real$opaque Complex$float$real(Complex$float wit,  $float a) {
  return Real$__pack__((Real)wit,a);
}

Real$opaque Complex$float$imag(Complex$float wit,  $float a) {
  return  Real$__pack__((Real)wit,to$float(0.0));
}

Real$opaque Complex$float$__abs__(Complex$float wit,  $float a) {
  return  Real$__pack__((Real)wit,to$float(fabs(from$float(a))));
}

$float Complex$float$__conjugate__(Complex$float wit,  $float a) {
  return a;
}

// Plus$float  ////////////////////////////////////////////////////////////////////////////////////////

$float Plus$float$__add__(Plus$float wit,  $float a, $float b) {
  return to$float(from$float(a) + from$float(b));
}  
 
// Minus$float  ////////////////////////////////////////////////////////////////////////////////////////

$float Minus$float$__sub__(Minus$float wit,  $float a, $float b) {
  return to$float(from$float(a) - from$float(b));
}  

// Hashable$float ///////////////////////////////////////////////////////////////////////////////////////////////////////

$bool Hashable$float$__eq__(Hashable$float wit, $float a, $float b) {
  return to$bool(a->val == b->val);
}

$bool Hashable$float$__neq__(Hashable$float wit, $float a, $float b) {
  return to$bool(a->val != b->val);
}

$int Hashable$float$__hash__(Hashable$float wit, $float a) {
  return to$int($float_hash(a));
}

$int Hashable$float$__keyinfo__(Hashable$float wit) {
  return to$int(FLOAT_ID);
}

static struct Real$float Real$float_instance;
static struct Complex$float Complex$float_instance;
static struct Plus$float Plus$float_instance;
static struct Minus$float Minus$float_instance;
static struct Hashable$float Hashable$float_instance;

static struct Real$float$__class__ Real$float_methods = {"", Real$float$__eq__ , Real$float$__ne__ , Real$float$__lt__ , Real$float$__le__ ,
                                                     Real$float$__gt__ , Real$float$__ge__ , Real$float$__float__ , Real$float$__trunc__ , Real$float$__floor__ ,
                                                     Real$float$__ceil__ , Real$float$__round__};
static struct Real$float Real$float_instance = {"",&Real$float_methods};
static Real$float Real$float_witness = &Real$float_instance;


static struct Complex$float$__class__ Complex$float_methods = {"",Complex$float$__eq__,Complex$float$__ne__,Complex$float$__complx__,
                                               Complex$float$__bool__,Complex$float$__mul__,Complex$float$__truediv__,Complex$float$__pow__,Complex$float$__neg__,
                                               Complex$float$__pos__,Complex$float$real,Complex$float$imag,Complex$float$__abs__,Complex$float$__conjugate__};
static struct Complex$float Complex$float_instance = {"",&Complex$float_methods, (Real)&Real$float_instance, (Plus)&Plus$float_instance, (Minus)&Minus$float_instance};
static Complex$float Complex$float_witness = &Complex$float_instance;

static struct Plus$float$__class__ Plus$float_methods = {"",Plus$float$__add__};
static struct Plus$float Plus$float_instance = {"",&Plus$float_methods, (Real)&Real$float_instance};
static Plus$float Plus$float_witness = &Plus$float_instance;

static struct Minus$float$__class__ Minus$float_methods = {"",Minus$float$__sub__};
static struct Minus$float Minus$float_instance = {"",&Minus$float_methods, (Real)&Real$float_instance};
static Minus$float Minus$float_witness = &Minus$float_instance;

static struct Hashable$float$__class__ Hashable$float_methods = {"",Hashable$float$__eq__,Hashable$float$__neq__,Hashable$float$__hash__,Hashable$float$__keyinfo__};
static struct Hashable$float Hashable$float_instance = {"",&Hashable$float_methods};
static Hashable$float Hashable$float_witness = &Hashable$float_instance;

 
Real$float Real$float_new() {
  return Real$float_witness;
}
 
Complex$float Complex$float_new() {
  return Complex$float_witness;
}

Plus$float Plus$float_new() {
  return Plus$float_witness;
}

Minus$float Minus$float_new() {
  return Minus$float_witness;
}

Hashable$float Hashable$float_new() {
    return Hashable$float_witness;
}

 
