
struct $int {
  char *GCINFO;
  long val;
};

  
$int to$int(long i) {
  $int res = malloc(sizeof(struct $int));
  res->val = i;
  return res;
}

long from$int($int w) {
  return w->val;
}

// Integral$int /////////////////////////////////////////////////////////////////////////

$bool Integral$int$__eq__ (Integral$int wit, $int a, $int b) {
  return to$bool(a->val == b->val);
}

$bool Integral$int$__ne__ (Integral$int wit, $int a, $int b) {
  return to$bool(a->val != b->val);
}

$bool Integral$int$__lt__ (Integral$int wit, $int a, $int b) {
  return to$bool(a->val < b->val);
}

$bool Integral$int$__le__ (Integral$int wit, $int a, $int b) {
  return to$bool(a->val <= b->val);
}

$bool Integral$int$__gt__ (Integral$int wit, $int a, $int b) {
  return to$bool(a->val > b->val);
}

$bool Integral$int$__ge__ (Integral$int wit, $int a, $int b) {
  return to$bool(a->val == b->val);
}

$float Integral$int$__float__ (Integral$int wit, $int n) {
  return to$float((double)n->val);
}

Integral$opaque Integral$int$__trunc__ (Integral$int wit, $int n) {
  return Integral$__pack__((Integral)wit,n);
}
  
Integral$opaque Integral$int$__floor__ (Integral$int wit, $int n) {
  return Integral$__pack__((Integral)wit,n);
}
  
Integral$opaque Integral$int$__ceil__ (Integral$int wit, $int n) {
  return Integral$__pack__((Integral)wit,n);
}
  
Integral$opaque Integral$int$__round__ (Integral$int wit, $int n) {
  return Integral$__pack__((Integral)wit,n);
}
  
Integral$opaque Integral$int$numerator (Integral$int wit, $int n) {
  return Integral$__pack__((Integral)wit,n);
}
  
Integral$opaque Integral$int$denominator (Integral$int wit, $int n) {
  return Integral$__pack__((Integral)wit,to$int(1L));
}
  
$int Integral$int$__int__ (Integral$int wit, $int n) {
  return n;
}

$int Integral$int$__index__(Integral$int wit, $int n) {
  return n;
}

$tup2_t Integral$int$__divmod__(Integral$int wit, $int a, $int b) {
  int n = from$int(a);
  int d = from$int(b);
  $tup2_t res = malloc(sizeof(struct $tup2_t));
  res->$GCINFO = "";
  res->a = to$int(n/d);
  res->b = to$int(n%d);
  return res;
}

$int Integral$int$__floordiv__(Integral$int wit, $int a, $int b) {
  return to$int(from$int(a) / from$int(b));
}

$int Integral$int$__mod__(Integral$int wit, $int a, $int b) {
  return to$int(from$int(a) % from$int(b));
}

$int Integral$int$__lshift__(Integral$int wit,  $int a, $int b) {
  return to$int(from$int(a) << from$int(b));
}

$int Integral$int$__rshift__(Integral$int wit,  $int a, $int b) {
  return to$int(from$int(a) >> from$int(b));
}
 
$int Integral$int$__invert__(Integral$int wit,  $int a) {
  return to$int(~from$int(a));
}


// Logical$int  ////////////////////////////////////////////////////////////////////////////////////////

$int Logical$int$__and__(Logical$int wit,  $int a, $int b) {
  return to$int(from$int(a) & from$int(b));
}
                                                 
$int Logical$int$__or__(Logical$int wit,  $int a, $int b) {
  return to$int(from$int(a) | from$int(b));
}
                                                 
$int Logical$int$__xor__(Logical$int wit,  $int a, $int b) {
  return to$int(from$int(a) ^ from$int(b));
}  

// Complex$int //////////////////////////////////////////////////////////////////////////////////////

$bool Complex$int$__eq__ (Complex$int wit, $int a, $int b) {
  return to$bool(a->val == b->val);
}

$bool Complex$int$__ne__ (Complex$int wit, $int a, $int b) {
  return to$bool(a->val != b->val);
}

$complex Complex$int$__complx__(Complex$int wit, $int a) {
  return to$complex(to$float((double)from$int(a)),to$float(0.0));
}

$bool Complex$int$__bool__(Complex$int wit, $int a) {
  return from$int(a)==0L ? $true : $false;
}

$int Complex$int$__mul__(Complex$int wit,  $int a, $int b) {
  return to$int(from$int(a) * from$int(b));
}  

// The typechecker will reject true division between two integers.
$int Complex$int$__truediv__(Complex$int wit,  $int a, $int b) {
  // raise NOTIMPLEMENTED
  return NULL;
}  

// only called with e>=0.
static int intpow(int a, int e) {
  if (e == 0) return 1;
  if (e == 1) return a;
  if (e % 2 == 0) return intpow(a*a,e/2);
  return a * intpow(a*a,e/2);
}
  
$int Complex$int$__pow__(Complex$int wit,  $int a, $int b) {
  if ( from$int(b) < 0) {
    // raise VALUEERROR;
    return NULL;
  }
  return to$int(intpow(from$int(a),from$int(b)));
}

$int Complex$int$__neg__(Complex$int wit,  $int a) {
  return to$int(-from$int(a));
}

$int Complex$int$__pos__(Complex$int wit,  $int a) {
  return a;
}

Real$opaque Complex$int$real(Complex$int wit,  $int a) {
  return Real$__pack__((Real)wit,a);
}

Real$opaque Complex$int$imag(Complex$int wit,  $int a) {
  return  Real$__pack__((Real)wit,to$int(0L));
}

Real$opaque Complex$int$__abs__(Complex$int wit,  $int a) {
  return  Real$__pack__((Real)wit,to$int(labs(from$int(a))));
}

$int Complex$int$__conjugate__(Complex$int wit,  $int a) {
  return a;
}

// Plus$int  ////////////////////////////////////////////////////////////////////////////////////////

$int Plus$int$__add__(Plus$int wit,  $int a, $int b) {
  return to$int(from$int(a) + from$int(b));
}  
 
// Minus$int  ////////////////////////////////////////////////////////////////////////////////////////

$int Minus$int$__sub__(Minus$int wit,  $int a, $int b) {
  return to$int(from$int(a) - from$int(b));
}  

// Hashable$int ///////////////////////////////////////////////////////////////////////////////////////////////////////

$bool Hashable$int$__eq__(Hashable$int wit, $int a, $int b) {
  return to$bool(a->val == b->val);
}

$bool Hashable$int$__neq__(Hashable$int wit, $int a, $int b) {
  return to$bool(a->val != b->val);
}

$int Hashable$int$__hash__(Hashable$int wit, $int a) {
  return to$int($int_hash(a));
}


static struct Integral$int Integral$int_instance;
static struct Logical$int Logical$int_instance;
static struct Complex$int Complex$int_instance;
static struct Plus$int Plus$int_instance;
static struct Minus$int Minus$int_instance;
static struct Hashable$int Hashable$int_instance;

static struct Integral$int$__class__ Integral$int_methods = {"", Integral$int$__eq__ , Integral$int$__ne__ , Integral$int$__lt__ , Integral$int$__le__ ,
                                                     Integral$int$__gt__ , Integral$int$__ge__ , Integral$int$__float__ , Integral$int$__trunc__ , Integral$int$__floor__ ,
                                                     Integral$int$__ceil__ , Integral$int$__round__ , Integral$int$numerator , Integral$int$denominator ,
                                                     Integral$int$__int__ , Integral$int$__index__ , Integral$int$__divmod__ , Integral$int$__floordiv__ ,
                                                     Integral$int$__mod__ , Integral$int$__lshift__ , Integral$int$__rshift__ , Integral$int$__invert__};

static struct Integral$int Integral$int_instance = {"",&Integral$int_methods, (Logical)&Logical$int_instance};
static Integral$int Integral$int_witness = &Integral$int_instance;


static struct Logical$int$__class__ Logical$int_methods =  {"", Logical$int$__and__ , Logical$int$__or__ , Logical$int$__xor__};
static struct Logical$int Logical$int_instance = {"",&Logical$int_methods, (Integral)&Integral$int_instance};
static Logical$int Logical$int_witness = &Logical$int_instance;


static struct Complex$int$__class__ Complex$int_methods = {"",Complex$int$__eq__,Complex$int$__ne__,Complex$int$__complx__,
                                               Complex$int$__bool__,Complex$int$__mul__,Complex$int$__truediv__,Complex$int$__pow__,Complex$int$__neg__,
                                               Complex$int$__pos__,Complex$int$real,Complex$int$imag,Complex$int$__abs__,Complex$int$__conjugate__};
static struct Complex$int Complex$int_instance = {"",&Complex$int_methods, (Integral)&Integral$int_instance, (Plus)&Plus$int_instance, (Minus)&Minus$int_instance};
static Complex$int Complex$int_witness = &Complex$int_instance;

static struct Plus$int$__class__ Plus$int_methods = {"",Plus$int$__add__};
static struct Plus$int Plus$int_instance = {"",&Plus$int_methods, (Integral)&Integral$int_instance};
static Plus$int Plus$int_witness = &Plus$int_instance;

static struct Minus$int$__class__ Minus$int_methods = {"",Minus$int$__sub__};
static struct Minus$int Minus$int_instance = {"",&Minus$int_methods, (Integral)&Integral$int_instance};
static Minus$int Minus$int_witness = &Minus$int_instance;

static struct Hashable$int$__class__ Hashable$int_methods = {"",Hashable$int$__eq__,Hashable$int$__neq__,Hashable$int$__hash__};
static struct Hashable$int Hashable$int_instance = {"",&Hashable$int_methods};
static Hashable$int Hashable$int_witness = &Hashable$int_instance;

 
Integral$int Integral$int_new() {
  return Integral$int_witness;
}

Logical$int Logical$int_new() {
  return Logical$int_witness;
}

Complex$int Complex$int_new() {
  return Complex$int_witness;
}


Plus$int Plus$int_new() {
  return Plus$int_witness;
}

Minus$int Minus$int_new() {
  return Minus$int_witness;
}

Hashable$int Hashable$int_new() {
    return Hashable$int_witness;
}

 
