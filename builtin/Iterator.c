$Iterator $Iterable$Iterator__iter__($Iterable$Iterator wit, $Iterator self) {
  return self;
}

struct $Iterable$Iterator$class $Iterable$Iterator$methods = {"", NULL, (void (*)($Iterable$Iterator))$default__init__,$Iterable$Iterator__iter__};
struct $Iterable$Iterator $Iterable$Iterator_instance = {&$Iterable$Iterator$methods};
$Iterable$Iterator $Iterable$Iterator$witness = &$Iterable$Iterator_instance;

struct $Iterator$class $Iterator$methods = {"",NULL,NULL,NULL,NULL,NULL}; // $Iterator is an abstract class
struct $Iterator $Iterator_instance = {&$Iterator$methods};
struct $Iterator *$Iterator$witness = &$Iterator_instance;

$WORD $next($Iterator it) {
  return it->$class->__next__(it);
}
