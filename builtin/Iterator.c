$Iterator $Iterable$Iterator_iter($Iterable$Iterator wit, $Iterator self) {
  return self;
}

$Iterable$Iterator $Iterable$Iterator$new() {
  return $NEW($Iterable$Iterator);
}

struct $Iterable$Iterator$class $Iterable$Iterator$methods = {"", UNASSIGNED,NULL, (void (*)($Iterable$Iterator))$default__init__,$Iterable$Iterator_iter};
struct $Iterable$Iterator $Iterable$Iterator_instance = {&$Iterable$Iterator$methods};
$Iterable$Iterator $Iterable$Iterator$witness = &$Iterable$Iterator_instance;


struct $Iterator$class $Iterator$methods = {"",UNASSIGNED,NULL,NULL,NULL,NULL,NULL,NULL,NULL}; // $Iterator is an abstract class
struct $Iterator $Iterator_instance = {&$Iterator$methods};
struct $Iterator *$Iterator$witness = &$Iterator_instance;

$WORD $next($Iterator it) {
  return it->$class->__next__(it);
}
