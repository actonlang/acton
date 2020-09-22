#include <complex.h>

struct $complex$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($complex, $Number$opaque);
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

extern struct $Number$complex$class $Number$complex$methods;
extern struct $Minus$complex$class $Minus$complex$methods;
extern struct $Eq$complex$class $Eq$complex$methods;
extern struct $Hashable$complex$class $Hashable$complex$methods;

extern struct $Number$complex *$Number$complex$witness;
extern struct $Minus$complex *$Minus$complex$witness;
extern struct $Eq$complex *$Eq$complex$witness;
extern struct $Hashable$complex *$Hashable$complex$witness;
