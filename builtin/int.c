
$int to$int(long i) {
  $int res = malloc(sizeof(long));
  *res = i;
  return res;
}

long from$int($int w) {
  return *w;
}

$bool $int_eq_instance(Eq$__class__ cl, $WORD a, $WORD b);
$bool $int_neq_instance(Eq$__class__ cl, $WORD a, $WORD b);

$int $int_hash_instance(Hashable$__class__ cl, $WORD a);

$bool $int_lt_instance(Ord$__class__ cl, $WORD a, $WORD b);
$bool $int_le_instance(Ord$__class__ cl, $WORD a, $WORD b);
$bool $int_gt_instance(Ord$__class__ cl, $WORD a, $WORD b);
$bool $int_ge_instance(Ord$__class__ cl, $WORD a, $WORD b);

$WORD $int_and_instance(Logical$__class__ cl, $WORD a, $WORD b);
$WORD $int_or_instance(Logical$__class__ cl, $WORD a, $WORD b);
$WORD $int_xor_instance(Logical$__class__ cl, $WORD a, $WORD b);

$WORD $int_add_instance(Plus$__class__ cl, $WORD a, $WORD b);

$WORD $int_sub_instance(Minus$__class__ cl, $WORD a, $WORD b);

$complex $int_complex_instance(Complex$__class__ cl, $WORD a);
$bool $int_bool_instance(Complex$__class__ cl, $WORD a);
$WORD $int_mul_instance(Complex$__class__ cl,  $WORD a, $WORD b);
$WORD $int_truediv_instance(Complex$__class__ cl,  $WORD a, $WORD b);
$WORD $int_pow_instance(Complex$__class__ cl,  $WORD a, $WORD b);
$WORD $int_neg_instance(Complex$__class__ cl,  $WORD a);
$WORD $int_pos_instance(Complex$__class__ cl,  $WORD a);
Real $int_real_instance(Complex$__class__ cl,  $WORD a);
Real $int_imag_instance(Complex$__class__ cl,  $WORD a);
$WORD $int_conjugate_instance(Complex$__class__ cl,  $WORD a);

$float $int_float_instance(Real$__class__ cl, $WORD a);
Integral $int_trunc_instance(Real$__class__ cl, $WORD a);
Integral $int_floor_instance(Real$__class__ cl, $WORD a);
Integral $int_ceil_instance(Real$__class__ cl, $WORD a);
Integral $int_round_instance(Real$__class__ cl, $WORD a);
                               
Integral $int_numerator_instance(Rational$__class__ cl, $WORD a);
Integral $int_denominator_instance(Rational$__class__ cl, $WORD a);

$int $int_int_instance(Integral$__class__ cl, $WORD a);
$int $int_index_instance(Integral$__class__ cl, $WORD a);
$divmod_t $int_divmod_instance(Integral$__class__ cl, $WORD a, $WORD b);
$WORD $int_floordiv_instance(Integral$__class__ cl, $WORD a, $WORD b);
$WORD $int_mod_instance(Integral$__class__ cl, $WORD a, $WORD b);
$WORD $int_lshift_instance(Integral$__class__ cl,  $WORD a, $WORD b);
$WORD $int_rshift_instance(Integral$__class__ cl,  $WORD a, $WORD b);
$WORD $int_invert_instance(Integral$__class__ cl,  $WORD a);

struct Eq$__class__ Eq$int_struct = {"GC_Eq$int",$int_eq_instance,$int_neq_instance};
Eq$__class__ Eq$int_instance = &Eq$int_struct;

struct Hashable$__class__ Hashable$int_struct = {"GC_Hashable_$int",&Eq$int_struct,$int_hash_instance};
Hashable$__class__ Hashable$int_instance = &Hashable$int_struct;

struct Ord$__class__ Ord$int_struct = {"GC_Ord$int",&Eq$int_struct, $int_lt_instance,$int_le_instance, $int_gt_instance, $int_ge_instance};
Ord$__class__ Ord$int_instance = &Ord$int_struct;

struct Logical$__class__ Logical$int_struct = {"GC_Logical$int", $int_and_instance,$int_or_instance, $int_xor_instance};
Logical$__class__ Logical$int_instance = &Logical$int_struct;

struct Plus$__class__ Plus$int_struct = {"GC_Plus$int",$int_add_instance};
Plus$__class__ Plus$int_instance = &Plus$int_struct;

struct Minus$__class__ Minus$int_struct = {"GC_Minus$int",$int_sub_instance};
Minus$__class__ Minus$int_instance = &Minus$int_struct;

struct Complex$__class__ Complex$int_struct = {"GC_Complex$int",&Eq$int_struct,&Plus$int_struct,&Minus$int_struct,$int_complex_instance,
                                               $int_bool_instance,$int_mul_instance,$int_truediv_instance,$int_pow_instance,$int_neg_instance,
                                               $int_pos_instance,$int_real_instance,$int_imag_instance,$int_conjugate_instance};
Complex$__class__ Complex$int_instance = &Complex$int_struct;

struct Real$__class__ Real$int_struct = {"GC_Real$int",&Ord$int_struct,&Complex$int_struct,$int_float_instance,$int_trunc_instance,
                                         $int_floor_instance,$int_ceil_instance,$int_round_instance};
Real$__class__ Real$int_instance = &Real$int_struct;

struct Rational$__class__ Rational$int_struct = {"GC_Rational$int", &Real$int_struct,$int_numerator_instance,$int_denominator_instance};
Rational$__class__ Rational$int_instance = &Rational$int_struct;

struct Integral$__class__ Integral$int_struct = {"GC_Integral",&Logical$int_struct,&Rational$int_struct,$int_int_instance,$int_index_instance,$int_divmod_instance,$int_floordiv_instance,$int_mod_instance,$int_lshift_instance,$int_rshift_instance,$int_invert_instance};
Integral$__class__ Integral$int_instance = &Integral$int_struct;

// Eq ////////////////////////////////////////////////////////////////////////////////////////

$bool $int_eq_instance(Eq$__class__ cl, $WORD a, $WORD b) {
  return from$int(a) == from$int(b);
}

$bool $int_neq_instance(Eq$__class__ cl, $WORD a, $WORD b) {
  return from$int(a) != from$int(b);
}

// Hashable ///////////////////////////////////////////////////////////////////////////////

$int $int_hash_instance(Hashable$__class__ cl, $WORD a) {
  return to$int($int_hash(a));
}

// Ord  ////////////////////////////////////////////////////////////////////////////////////////

$bool $int_lt_instance(Ord$__class__ cl, $WORD a, $WORD b) {
  return from$int(a) < from$int(b);
}
$bool $int_le_instance(Ord$__class__ cl, $WORD a, $WORD b) {
  return from$int(a) <= from$int(b);
}
$bool $int_gt_instance(Ord$__class__ cl, $WORD a, $WORD b) {
  return from$int(a) > from$int(b);
}
$bool $int_ge_instance(Ord$__class__ cl, $WORD a, $WORD b) {
  return from$int(a) >= from$int(b);
}
                                                 
// Logical  ////////////////////////////////////////////////////////////////////////////////////////

$WORD $int_and_instance(Logical$__class__ cl,  $WORD a, $WORD b) {
  return to$int(from$int(a) & from$int(b));
}
                                                 
$WORD $int_or_instance(Logical$__class__ cl,  $WORD a, $WORD b) {
  return to$int(from$int(a) | from$int(b));
}
                                                 
$WORD $int_xor_instance(Logical$__class__ cl,  $WORD a, $WORD b) {
  return to$int(from$int(a) ^ from$int(b));
}  
                                                 
// Plus  ////////////////////////////////////////////////////////////////////////////////////////

$WORD $int_add_instance(Plus$__class__ cl,  $WORD a, $WORD b) {
  return to$int(from$int(a) + from$int(b));
}  
                                                 
// Minus ////////////////////////////////////////////////////////////////////////////////////////

$WORD $int_sub_instance(Minus$__class__ cl,  $WORD a, $WORD b) {
  return to$int(from$int(a) - from$int(b));
}  
                                                 
// Complex //////////////////////////////////////////////////////////////////////////////////////

$complex $int_complex_instance(Complex$__class__ cl, $WORD a) {
  return to$complex((double)from$int(a),0.0);
}

$bool $int_bool_instance(Complex$__class__ cl, $WORD a) {
  return from$int(a)==0L ? $true : $false;
}

$WORD $int_mul_instance(Complex$__class__ cl,  $WORD a, $WORD b) {
  return to$int(from$int(a) * from$int(b));
}  

// The typechecker will reject true division between two integers.
$WORD $int_truediv_instance(Complex$__class__ cl,  $WORD a, $WORD b) {
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
  
$WORD $int_pow_instance(Complex$__class__ cl,  $WORD a, $WORD b) {
  if ( from$int(b) < 0) {
    // raise VALUEERROR;
    return NULL;
  }
  return to$int(intpow(from$int(a),from$int(b)));
}

$WORD $int_neg_instance(Complex$__class__ cl,  $WORD a) {
  return to$int(-from$int(a));
}

$WORD $int_pos_instance(Complex$__class__ cl,  $WORD a) {
  return a;
}

Real $int_real_instance(Complex$__class__ cl,  $WORD a) {
  return Real$__pack__(Real$int_instance,a);
}

Real $int_imag_instance(Complex$__class__ cl,  $WORD a) {
  return  Real$__pack__(Real$int_instance,to$int(0L));
}

$WORD $int_conjugate_instance(Complex$__class__ cl,  $WORD a) {
  return a;
}

// Real //////////////////////////////////////////////////////////////////////////////////////////

$float $int_float_instance(Real$__class__ cl, $WORD a) {
  return to$float((double)from$int(a));
}

Integral $int_trunc_instance(Real$__class__ cl, $WORD a) {
  return Integral$__pack__(Integral$int_instance,a);
}

Integral $int_floor_instance(Real$__class__ cl, $WORD a) {
  return Integral$__pack__(Integral$int_instance,a);
}

Integral $int_ceil_instance(Real$__class__ cl, $WORD a) {
  return Integral$__pack__(Integral$int_instance,a);
}

Integral $int_round_instance(Real$__class__ cl, $WORD a) {
  return Integral$__pack__(Integral$int_instance,a);
}

// Rational //////////////////////////////////////////////////////////////////////////////////////

Integral $int_numerator_instance(Rational$__class__ cl, $WORD a) {
  return Integral$__pack__(Integral$int_instance,a);
}

Integral $int_denominator_instance(Rational$__class__ cl, $WORD a) {
  return Integral$__pack__(Integral$int_instance,to$int(1L));
}

// Integral //////////////////////////////////////////////////////////////////////////////////////

$int $int_int_instance(Integral$__class__ cl, $WORD a) {
  return ($int)a;
}

$int $int_index_instance(Integral$__class__ cl, $WORD a) {
  return ($int)a;
}

$divmod_t $int_divmod_instance(Integral$__class__ cl, $WORD a, $WORD b) {
  int n = from$int(a);
  int d = from$int(b);
  $divmod_t res = malloc(sizeof(struct $divmod_struct));
  res->$GCINFO = "GC";
  res->quotient = to$int(n/d);
  res->remainder = to$int(n%d);
  return res;
}

$WORD $int_floordiv_instance(Integral$__class__ cl, $WORD a, $WORD b) {
  return to$int(from$int(a) / from$int(b));
}

$WORD $int_mod_instance(Integral$__class__ cl, $WORD a, $WORD b) {
  return to$int(from$int(a) % from$int(b));
}

$WORD $int_lshift_instance(Integral$__class__ cl,  $WORD a, $WORD b) {
  return to$int(from$int(a) << from$int(b));
}

$WORD $int_rshift_instance(Integral$__class__ cl,  $WORD a, $WORD b) {
  return to$int(from$int(a) >> from$int(b));
}  

$WORD $int_invert_instance(Integral$__class__ cl,  $WORD a) {
  return to$int(~from$int(a));
}
