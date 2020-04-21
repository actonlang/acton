$Iterator $Iterable$Iterator__iter__($Iterable$Iterator wit, $Iterator self) {
  return self;
}

struct $Iterable$Iterator$class $Iterable$Iterator$methods = {"", $Iterable$Iterator__iter__};
struct $Iterable$Iterator $Iterable$Iterator_instance = {&$Iterable$Iterator$methods};
$Iterable$Iterator $Iterable$Iterator$witness = &$Iterable$Iterator_instance;
  
$WORD $next($Iterator it) {
  return it->class->__next__(it);
}
