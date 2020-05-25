// General methods ///////////////////////////////////////////////////////////////////////

void $int_init($int self, long val){
  self->val = val;
}

void $int_serialize($int n,$Serial$state state) {
  $val_serialize(INT_ID,&n->val,state);
}

$int $int_deserialize($Serial$state state) {
  return to$int((long)$val_deserialize(state));
}

$bool $int_bool($int n) {
  return to$bool(n->val != 0);
}

$str $int_str($int n) {
  char *s;
  asprintf(&s,"%ld",n->val);
  return from$UTF8(s);
}
  
struct $int$class $int$methods = {"",UNASSIGNED,NULL,$int_init,$int_bool,$int_str,$int_serialize,$int_deserialize};

$int to$int(long i) {
  $int res = malloc(sizeof(struct $int));
  res->$class = &$int$methods;
  res->val = i;
  return res;
}

long from$int($int w) {
  return w->val;
}


// $Integral$int /////////////////////////////////////////////////////////////////////////

$bool $Integral$int$__eq__ ($Integral$int wit, $int a, $int b) {
  return to$bool(a->val == b->val);
}

$bool $Integral$int$__ne__ ($Integral$int wit, $int a, $int b) {
  return to$bool(a->val != b->val);
}

$bool $Integral$int$__lt__ ($Integral$int wit, $int a, $int b) {
  return to$bool(a->val < b->val);
}

$bool $Integral$int$__le__ ($Integral$int wit, $int a, $int b) {
  return to$bool(a->val <= b->val);
}

$bool $Integral$int$__gt__ ($Integral$int wit, $int a, $int b) {
  return to$bool(a->val > b->val);
}

$bool $Integral$int$__ge__ ($Integral$int wit, $int a, $int b) {
  return to$bool(a->val == b->val);
}

$float $Integral$int$__float__ ($Integral$int wit, $int n) {
  return to$float((double)n->val);
}

$Integral$opaque $Integral$int$__trunc__ ($Integral$int wit, $int n) {
  return $Integral$pack(($Integral)wit,n);
}
  
$Integral$opaque $Integral$int$__floor__ ($Integral$int wit, $int n) {
  return $Integral$pack(($Integral)wit,n);
}
  
$Integral$opaque $Integral$int$__ceil__ ($Integral$int wit, $int n) {
  return $Integral$pack(($Integral)wit,n);
}
  
$Integral$opaque $Integral$int$__round__ ($Integral$int wit, $int n) {
  return $Integral$pack(($Integral)wit,n);
}
  
$Integral$opaque $Integral$int$numerator ($Integral$int wit, $int n) {
  return $Integral$pack(($Integral)wit,n);
}
  
$Integral$opaque $Integral$int$denominator ($Integral$int wit, $int n) {
  return $Integral$pack(($Integral)wit,to$int(1L));
}
  
$int $Integral$int$__int__ ($Integral$int wit, $int n) {
  return n;
}

$int $Integral$int$__index__($Integral$int wit, $int n) {
  return n;
}

$tuple $Integral$int$__divmod__($Integral$int wit, $int a, $int b) {
  int n = from$int(a);
  int d = from$int(b);
  $WORD *comps = malloc(2*sizeof($WORD));
  comps[0] = to$int(n/d);
  comps[1] = to$int(n%d);
  return $NEW($tuple,2,comps);
}

$int $Integral$int$__floordiv__($Integral$int wit, $int a, $int b) {
  return to$int(from$int(a) / from$int(b));
}

$int $Integral$int$__mod__($Integral$int wit, $int a, $int b) {
  return to$int(from$int(a) % from$int(b));
}

$int $Integral$int$__lshift__($Integral$int wit,  $int a, $int b) {
  return to$int(from$int(a) << from$int(b));
}

$int $Integral$int$__rshift__($Integral$int wit,  $int a, $int b) {
  return to$int(from$int(a) >> from$int(b));
}
 
$int $Integral$int$__invert__($Integral$int wit,  $int a) {
  return to$int(~from$int(a));
}


// Logical$int  ////////////////////////////////////////////////////////////////////////////////////////

$int $Logical$int$__and__($Logical$int wit,  $int a, $int b) {
  return to$int(from$int(a) & from$int(b));
}
                                                 
$int $Logical$int$__or__($Logical$int wit,  $int a, $int b) {
  return to$int(from$int(a) | from$int(b));
}
                                                 
$int $Logical$int$__xor__($Logical$int wit,  $int a, $int b) {
  return to$int(from$int(a) ^ from$int(b));
}  

// $Complex$int //////////////////////////////////////////////////////////////////////////////////////

$bool $Complex$int$__eq__ ($Complex$int wit, $int a, $int b) {
  return to$bool(a->val == b->val);
}

$bool $Complex$int$__ne__ ($Complex$int wit, $int a, $int b) {
  return to$bool(a->val != b->val);
}

$complex $Complex$int$__complx__($Complex$int wit, $int a) {
  return to$complex(to$float((double)from$int(a)),to$float(0.0));
}

$bool $Complex$int$__bool__($Complex$int wit, $int a) {
  return from$int(a)==0L ? $true : $false;
}

$int $Complex$int$__mul__($Complex$int wit,  $int a, $int b) {
  return to$int(from$int(a) * from$int(b));
}  

// The typechecker will reject true division between two integers.
$int $Complex$int$__truediv__($Complex$int wit,  $int a, $int b) {
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
  
$int $Complex$int$__pow__($Complex$int wit,  $int a, $int b) {
  if ( from$int(b) < 0) {
    // raise VALUEERROR;
    return NULL;
  }
  return to$int(intpow(from$int(a),from$int(b)));
}

$int $Complex$int$__neg__($Complex$int wit,  $int a) {
  return to$int(-from$int(a));
}

$int $Complex$int$__pos__($Complex$int wit,  $int a) {
  return a;
}

$Real$opaque $Complex$int$real($Complex$int wit,  $int a) {
  return $Real$pack(($Real)wit,a);
}

$Real$opaque $Complex$int$imag($Complex$int wit,  $int a) {
  return  $Real$pack(($Real)wit,to$int(0L));
}

$Real$opaque $Complex$int$__abs__($Complex$int wit,  $int a) {
  return  $Real$pack(($Real)wit,to$int(labs(from$int(a))));
}

$int $Complex$int$__conjugate__($Complex$int wit,  $int a) {
  return a;
}

// $Plus$int  ////////////////////////////////////////////////////////////////////////////////////////

$int $Plus$int$__add__($Plus$int wit,  $int a, $int b) {
  return to$int(from$int(a) + from$int(b));
}  
 
// $Minus$int  ////////////////////////////////////////////////////////////////////////////////////////

$int $Minus$int$__sub__($Minus$int wit,  $int a, $int b) {
  return to$int(from$int(a) - from$int(b));
}  

// $Hashable$int ///////////////////////////////////////////////////////////////////////////////////////////////////////

$bool $Hashable$int$__eq__($Hashable$int wit, $int a, $int b) {
  return to$bool(a->val == b->val);
}

$bool $Hashable$int$__neq__($Hashable$int wit, $int a, $int b) {
  return to$bool(a->val != b->val);
}

$int $Hashable$int$__hash__($Hashable$int wit, $int a) {
  return to$int($int_hash(a));
}

void $Integral$int_init($Integral$int wit) {
  wit-> w$Complex$Integral = $NEW($Complex$int,wit);
  wit-> w$Logical$Integral = $NEW($Logical$int,wit);
};

void $Complex$int_init($Complex$int wit, $Integral$int w$Integral$int) {
  wit->w$Integral$int = w$Integral$int;
  wit-> w$Plus$Complex = $NEW($Plus$int,wit);
  wit-> w$Minus$Complex = $NEW($Minus$int,wit);
}

void $Logical$int_init($Logical$int wit, $Integral$int w$Integral$int) {
  wit->w$Integral$int =  w$Integral$int;
}

void $Plus$int_init($Plus$int wit, $Complex$int w$Complex$int) {
  wit->w$Complex$int =  w$Complex$int;
}

void $Minus$int_init($Minus$int wit, $Complex$int w$Complex$int) {
  wit->w$Complex$int =  w$Complex$int;
}

struct $Integral$int $Integral$int_instance;
struct $Logical$int $Logical$int_instance;
struct $Complex$int $Complex$int_instance;
struct $Plus$int $Plus$int_instance;
struct $Minus$int $Minus$int_instance;
struct $Hashable$int $Hashable$int_instance;

struct $Integral$int$class $Integral$int$methods = {"",UNASSIGNED,NULL,$Integral$int_init, $Integral$int$__eq__ , $Integral$int$__ne__ , $Integral$int$__lt__ , $Integral$int$__le__ ,
                                                     $Integral$int$__gt__ , $Integral$int$__ge__ , $Integral$int$__float__ , $Integral$int$__trunc__ , $Integral$int$__floor__ ,
                                                     $Integral$int$__ceil__ , $Integral$int$__round__ , $Integral$int$numerator , $Integral$int$denominator ,
                                                     $Integral$int$__int__ , $Integral$int$__index__ , $Integral$int$__divmod__ , $Integral$int$__floordiv__ ,
                                                     $Integral$int$__mod__ , $Integral$int$__lshift__ , $Integral$int$__rshift__ , $Integral$int$__invert__};
struct $Integral$int $Integral$int_instance = {&$Integral$int$methods, &$Logical$int_instance};
$Integral$int $Integral$int$witness = &$Integral$int_instance;


struct $Logical$int$class $Logical$int$methods =  {"", UNASSIGNED,NULL,$Logical$int_init, $Logical$int$__and__ , $Logical$int$__or__ , $Logical$int$__xor__};
struct $Logical$int $Logical$int_instance = {&$Logical$int$methods, &$Integral$int_instance};
$Logical$int $Logical$int$witness = &$Logical$int_instance;


struct $Complex$int$class $Complex$int$methods = {"", UNASSIGNED,NULL,$Complex$int_init, $Complex$int$__eq__,$Complex$int$__ne__,$Complex$int$__complx__,
                                               $Complex$int$__bool__,$Complex$int$__mul__,$Complex$int$__truediv__,$Complex$int$__pow__,$Complex$int$__neg__,
                                               $Complex$int$__pos__,$Complex$int$real,$Complex$int$imag,$Complex$int$__abs__,$Complex$int$__conjugate__};
struct $Complex$int $Complex$int_instance = {&$Complex$int$methods, &$Integral$int_instance, &$Plus$int_instance, &$Minus$int_instance};
$Complex$int $Complex$int$witness = &$Complex$int_instance;

struct $Plus$int$class $Plus$int$methods = {"",UNASSIGNED,NULL,$Plus$int_init, $Plus$int$__add__};
struct $Plus$int $Plus$int_instance = {&$Plus$int$methods, &$Complex$int_instance};
$Plus$int $Plus$int$witness = &$Plus$int_instance;

struct $Minus$int$class $Minus$int$methods = {"",UNASSIGNED, NULL,$Minus$int_init, $Minus$int$__sub__};
struct $Minus$int $Minus$int_instance = {&$Minus$int$methods, &$Complex$int_instance};
$Minus$int $Minus$int$witness = &$Minus$int_instance;

struct $Hashable$int$class $Hashable$int$methods = {"",UNASSIGNED, NULL,(void (*)($Hashable$int))$default__init__, $Hashable$int$__eq__,$Hashable$int$__neq__,$Hashable$int$__hash__};
struct $Hashable$int $Hashable$int_instance = {&$Hashable$int$methods};
$Hashable$int $Hashable$int$witness = &$Hashable$int_instance;
