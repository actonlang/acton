#include <complex.h>

struct $complex$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($complex, $Complex$opaque);
  void (*__serialize__)($complex,$Serial$state);
  $complex (*__deserialize__)($Serial$state);
  $bool (*__bool__)($complex);
  $str (*__str__)($complex);
};

struct $complex {
  struct $complex$class *$class;
  complex double val;
};

extern struct $complex$class $complex$methods;

$complex to$complex(complex double c);

extern struct $Complex$complex$class $Complex$complex$methods;
extern struct $Plus$complex$class $Plus$complex$methods;
extern struct $Minus$complex$class $Minus$complex$methods;
extern struct $Hashable$complex$class $Hashable$complex$methods;

extern struct $Complex$complex *$Complex$complex$witness;
extern struct $Plus$complex *$Plus$complex$witness;
extern struct $Minus$complex *$Minus$complex$witness;
extern struct $Hashable$complex *$Hashable$complex$witness;
