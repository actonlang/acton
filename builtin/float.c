
struct $float  {
  char *GCINFO;
  float val;
};
  
$float to$float(double x) {
  $float res = malloc(sizeof(struct $float));
  res->val = x;
  return res;
}

double from$float($float x) {
  return x->val;
}
/*
$bool $float_eq_instance(Eq$__class__ cl, $WORD a, $WORD b);
$bool $float_neq_instance(Eq$__class__ cl, $WORD a, $WORD b);

$int $float_hash_instance(Hashable$__class__ cl, $WORD a);

$bool $float_lt_instance(Ord$__class__ cl, $WORD a, $WORD b);
$bool $float_le_instance(Ord$__class__ cl, $WORD a, $WORD b);
$bool $float_gt_instance(Ord$__class__ cl, $WORD a, $WORD b);
$bool $float_ge_instance(Ord$__class__ cl, $WORD a, $WORD b);

$WORD $float_and_instance(Logical$__class__ cl, $WORD a, $WORD b);
$WORD $float_or_instance(Logical$__class__ cl, $WORD a, $WORD b);
$WORD $float_xor_instance(Logical$__class__ cl, $WORD a, $WORD b);

$WORD $float_add_instance(Plus$__class__ cl, $WORD a, $WORD b);

$WORD $float_sub_instance(Minus$__class__ cl, $WORD a, $WORD b);

$complex $float_complex_instance(Complex$__class__ cl, $WORD a);
$bool $float_bool_instance(Complex$__class__ cl, $WORD a);
$WORD $float_mul_instance(Complex$__class__ cl,  $WORD a, $WORD b);
$WORD $float_truediv_instance(Complex$__class__ cl,  $WORD a, $WORD b);
$WORD $float_pow_instance(Complex$__class__ cl,  $WORD a, $WORD b);
$WORD $float_neg_instance(Complex$__class__ cl,  $WORD a);
$WORD $float_pos_instance(Complex$__class__ cl,  $WORD a);
Real $float_real_instance(Complex$__class__ cl,  $WORD a);
Real $float_imag_instance(Complex$__class__ cl,  $WORD a);
$WORD $float_conjugate_instance(Complex$__class__ cl,  $WORD a);

$float $float_float_instance(Real$__class__ cl, $WORD a);
Integral $float_trunc_instance(Real$__class__ cl, $WORD a);
Integral $float_floor_instance(Real$__class__ cl, $WORD a);
Integral $float_ceil_instance(Real$__class__ cl, $WORD a);
Integral $float_round_instance(Real$__class__ cl, $WORD a);

struct Eq$__class__ Eq$float_struct = {"GC_Eq$float",$float_eq_instance,$float_neq_instance};
Eq$__class__ Eq$float_instance = &Eq$float_struct;

struct Hashable$__class__ Hashable$float_struct = {"GC_Hashable_$float",&Eq$float_struct,$float_hash_instance};
Hashable$__class__ Hashable$float_instance = &Hashable$float_struct;

struct Ord$__class__ Ord$float_struct = {"GC_Ord$float",&Eq$float_struct, $float_lt_instance,$float_le_instance, $float_gt_instance, $float_ge_instance};
Ord$__class__ Ord$float_instance = &Ord$float_struct;

struct Plus$__class__ Plus$float_struct = {"GC_Plus$float",$float_add_instance};
Plus$__class__ Plus$float_instance = &Plus$float_struct;

struct Minus$__class__ Minus$float_struct = {"GC_Minus$float",$float_sub_instance};
Minus$__class__ Minus$float_instance = &Minus$float_struct;

struct Complex$__class__ Complex$float_struct = {"GC_Complex$float",&Eq$float_struct,&Plus$float_struct,&Minus$float_struct,$float_complex_instance,
                                               $float_bool_instance,$float_mul_instance,$float_truediv_instance,$float_pow_instance,$float_neg_instance,
                                               $float_pos_instance,$float_real_instance,$float_imag_instance,$float_conjugate_instance};
Complex$__class__ Complex$float_instance = &Complex$float_struct;

struct Real$__class__ Real$float_struct = {"GC_Real$float",&Ord$float_struct,&Complex$float_struct,$float_float_instance,$float_trunc_instance,
                                         $float_floor_instance,$float_ceil_instance,$float_round_instance};
Real$__class__ Real$float_instance = &Real$float_struct;

// Eq ////////////////////////////////////////////////////////////////////////////////////////

$bool $float_eq_instance(Eq$__class__ cl, $WORD a, $WORD b) {
  return from$float(a) == from$float(b);
}

$bool $float_neq_instance(Eq$__class__ cl, $WORD a, $WORD b) {
  return from$float(a) != from$float(b);
}

// Hashable ///////////////////////////////////////////////////////////////////////////////

$int $float_hash_instance(Hashable$__class__ cl, $WORD a) {
  return to$int($float_hash(a));
}

// Ord  ////////////////////////////////////////////////////////////////////////////////////////

$bool $float_lt_instance(Ord$__class__ cl, $WORD a, $WORD b) {
  return from$float(a) < from$float(b);
}
$bool $float_le_instance(Ord$__class__ cl, $WORD a, $WORD b) {
  return from$float(a) <= from$float(b);
}
$bool $float_gt_instance(Ord$__class__ cl, $WORD a, $WORD b) {
  return from$float(a) > from$float(b);
}
$bool $float_ge_instance(Ord$__class__ cl, $WORD a, $WORD b) {
  return from$float(a) >= from$float(b);
}
                                                                                              
// Plus  ////////////////////////////////////////////////////////////////////////////////////////

$WORD $float_add_instance(Plus$__class__ cl,  $WORD a, $WORD b) {
  return to$float(from$float(a) + from$float(b));
}  
                                                 
// Minus ////////////////////////////////////////////////////////////////////////////////////////

$WORD $float_sub_instance(Minus$__class__ cl,  $WORD a, $WORD b) {
  return to$float(from$float(a) - from$float(b));
}  
                                                 
// Complex //////////////////////////////////////////////////////////////////////////////////////

$complex $float_complex_instance(Complex$__class__ cl, $WORD a) {
  return to$complex((double)from$float(a),0.0);
}

$bool $float_bool_instance(Complex$__class__ cl, $WORD a) {
  return from$float(a)==0.0 ? $true : $false;
}

$WORD $float_mul_instance(Complex$__class__ cl,  $WORD a, $WORD b) {
  return to$float(from$float(a) * from$float(b));
}  

$WORD $float_truediv_instance(Complex$__class__ cl,  $WORD a, $WORD b) {
  return to$float(from$float(a) / from$float(b));
}  
 
$WORD $float_pow_instance(Complex$__class__ cl,  $WORD a, $WORD b) {
  return to$float(pow(from$float(a),from$float(b)));
}

$WORD $float_neg_instance(Complex$__class__ cl,  $WORD a) {
  return to$float(-from$float(a));
}

$WORD $float_pos_instance(Complex$__class__ cl,  $WORD a) {
  return a;
}

Real $float_real_instance(Complex$__class__ cl,  $WORD a) {
  return Real$__pack__(Real$float_instance,a);
}

Real $float_imag_instance(Complex$__class__ cl,  $WORD a) {
  return  Real$__pack__(Real$float_instance,to$float(0.0));
}

$WORD $float_conjugate_instance(Complex$__class__ cl,  $WORD a) {
  return a;
}

// Real //////////////////////////////////////////////////////////////////////////////////////////

$float $float_float_instance(Real$__class__ cl, $WORD a) {
  return a;
}

Integral $float_trunc_instance(Real$__class__ cl, $WORD a) {
  //trunc returns a double
  double x = trunc(from$float(a));
  // next line may overflow; needs to be fixed
  long n = (long)x;
  return Integral$__pack__(Integral$int_instance,to$int(n));
}

Integral $float_floor_instance(Real$__class__ cl, $WORD a) {
  //floor returns a double
  double x = floor(from$float(a));
  // next line may overflow; needs to be fixed
  long n = (long)x;
  return Integral$__pack__(Integral$int_instance,to$int(n));
}

Integral $float_ceil_instance(Real$__class__ cl, $WORD a) {
  //ceil returns a double
  double x = ceil(from$float(a));
  // next line may overflow; needs to be fixed
  long n = (long)x;
  return Integral$__pack__(Integral$int_instance,to$int(n));
}

Integral $float_round_instance(Real$__class__ cl, $WORD a) {
  //round returns a double
  double x = round(from$float(a));
  // next line may overflow; needs to be fixed
  long n = (long)x;
  return Integral$__pack__(Integral$int_instance,to$int(n));
}
*/
