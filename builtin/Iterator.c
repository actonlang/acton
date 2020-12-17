$Iterator $Iterable$Iterator_iter($Iterable$Iterator wit, $Iterator self) {
  return self;
}

$Iterable$Iterator $Iterable$Iterator$new() {
  return $NEW($Iterable$Iterator);
}

struct $Iterable$Iterator$class $Iterable$Iterator$methods = {
    "$Iterable$Iterator",
    UNASSIGNED,
    ($Super$class)&$Iterable$methods,
    (void (*)($Iterable$Iterator))$default__init__,
    $Iterable$Iterator$__serialize__,
    $Iterable$Iterator$__deserialize__,
    ($bool (*)($Iterable$Iterator))$default__bool__,
    ($str (*)($Iterable$Iterator))$default__str__,
    $Iterable$Iterator_iter
};

struct $Iterable$Iterator $Iterable$Iterator_instance = {&$Iterable$Iterator$methods};
$Iterable$Iterator $Iterable$Iterator$witness = &$Iterable$Iterator_instance;


struct $Iterator$class $Iterator$methods = {"$Iterator",UNASSIGNED,NULL,NULL,NULL,NULL,NULL,NULL,NULL}; // $Iterator is an abstract class
struct $Iterator $Iterator_instance = {&$Iterator$methods};
struct $Iterator *$Iterator$witness = &$Iterator_instance;

void $Iterable$Iterator$__serialize__( $Iterable$Iterator self, $Serial$state state) {
}

$Iterable$Iterator $Iterable$Iterator$__deserialize__($Serial$state state) {
   $Iterable$Iterator res = $DNEW($Iterable$Iterator,state);
   return res;
}

$bool $Iterable$Iterator$__eq__($Iterable$Iterator wit, $complex a, $complex b) {
  return to$bool(creal(a->val) == creal(b->val) && cimag(a->val) == cimag(b->val));
}

$WORD $next($Iterator it) {
  return it->$class->__next__(it);
}
