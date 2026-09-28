#include <limits.h>

double stdQ_mathQ_sqrt(double x) {
  return sqrt(x);
}
double stdQ_mathQ_exp(double x) {
  return exp(x);
}
double stdQ_mathQ_log(double x) {
  return log(x);
}
double stdQ_mathQ_sin(double x) {
  return sin(x);
}
double stdQ_mathQ_cos(double x) {
  return cos(x);
}
double stdQ_mathQ_tan(double x) {
  return tan(x);
}
double stdQ_mathQ_asin(double x) {
  return asin(x);
}
double stdQ_mathQ_acos(double x) {
  return acos(x);
}
double stdQ_mathQ_atan(double x) {
  return atan(x);
}
double stdQ_mathQ_sinh(double x) {
  return sinh(x);
}
double stdQ_mathQ_cosh(double x) {
  return cosh(x);
}
double stdQ_mathQ_tanh(double x) {
  return tanh(x);
}
double stdQ_mathQ_asinh(double x) {
  return asinh(x);
}
double stdQ_mathQ_acosh(double x) {
  return acosh(x);
}
double stdQ_mathQ_atanh(double x) {
  return atanh(x);
}
double stdQ_mathQ_ldexp(double x, int64_t exp) {
  // The C int bounds exceed the range of finite nonzero double results.
  if (exp > INT_MAX) return ldexp(x, INT_MAX);
  if (exp < INT_MIN) return ldexp(x, INT_MIN);
  return ldexp(x, (int)exp);
}

void stdQ_mathQ___ext_init__() {
    //NOP
};
