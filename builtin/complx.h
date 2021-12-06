/*
 * This file is deliberately named complx.h to avoid collisions under gcc or
 * clang. Details are murky, is it with the standard complex.h? (Path should be
 * different, no?) Björn should know the details..
 */
#include <complex.h>

struct $complex$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($complex, $Number, $WORD);
  void (*__serialize__)($complex,$Serial$state);
  $complex (*__deserialize__)($complex,$Serial$state);
  $bool (*__bool__)($complex);
  $str (*__str__)($complex);
};

struct $complex {
  struct $complex$class *$class;
  complex double val;
};

extern struct $complex$class $complex$methods;
$complex $complex$new($Number, $WORD);

$complex to$complex(complex double c);

extern struct $Number$complex$class $Number$complex$methods;
$Number$complex $Number$complex$new();
extern struct $Div$complex$class $Div$complex$methods;
$Div$complex $Div$complex$new();
extern struct $Minus$complex$class $Minus$complex$methods;
$Minus$complex $Minus$complex$new($Number);
extern struct $Eq$complex$class $Eq$complex$methods;
$Eq$complex $Eq$complex$new();
extern struct $Hashable$complex$class $Hashable$complex$methods;
$Hashable$complex $Hashable$complex$new();

extern struct $Number$complex *$Number$complex$witness;
extern struct $Minus$complex *$Minus$complex$witness;
extern struct $Eq$complex *$Eq$complex$witness;
extern struct $Hashable$complex *$Hashable$complex$witness;
