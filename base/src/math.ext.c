double mathQ_sqrt(double x) {
  return sqrt(x);
}
double mathQ_exp(double x) {
  return exp(x);
}
double mathQ_log(double x) {
  return log(x);
}
double mathQ_sin(double x) {
  return sin(x);
}
double mathQ_cos(double x) {
  return cos(x);
}
double mathQ_tan(double x) {
  return tan(x);
}
double mathQ_asin(double x) {
  return asin(x);
}
double mathQ_acos(double x) {
  return acos(x);
}
double mathQ_atan(double x) {
  return atan(x);
}
double mathQ_sinh(double x) {
  return sinh(x);
}
double mathQ_cosh(double x) {
  return cosh(x);
}
double mathQ_tanh(double x) {
  return tanh(x);
}
double mathQ_asinh(double x) {
  return asinh(x);
}
double mathQ_acosh(double x) {
  return acosh(x);
}
double mathQ_atanh(double x) {
  return atanh(x);
}
double mathQ_ldexp(double x, int64_t exp) {
  if (exp > (int64_t)INT_MAX || exp < (int64_t)INT_MIN) {
    $RAISE(((B_BaseException)B_ValueErrorG_new($FORMAT("Exponent out of valid range for ldexp: %lld", (long long)exp))));
  }

  return ldexp(x, (int)exp);
}

void mathQ___ext_init__() {
    //NOP
};
