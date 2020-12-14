

// Serialization ///////////////////////////////////////////////////////////////////////

void $bool_init($bool self, $struct s){
  self->val = (s->$class->__bool__(s))->val;
}

$bool $bool_bool($bool self) {
  return self;
}

$str $bool_str($bool self) {
  if (self->val)
    return to$str("True");
  else
    return to$str("False");
}

void $bool_serialize($bool self, $Serial$state state) {
  $val_serialize(BOOL_ID,&self->val,state);
}

$bool $bool_deserialize($Serial$state state) {
  return to$bool((long)$val_deserialize(state));
}

struct $bool$class $bool$methods = {
    "$bool",
    UNASSIGNED,
    ($Super$class)&$atom$methods,
    $bool_init,
    $bool_serialize,
    $bool_deserialize,
    $bool_bool,
    $bool_str
};

$bool $bool$new($struct s) {
    return $NEW($bool, s);
}

$bool to$bool(long b) {
  $bool res = malloc(sizeof(struct $bool));
  res->$class = &$bool$methods;
  res->val = b;
  return res;
}
    
long from$bool($bool b) {
  return b->val;
}

struct $bool $t = {&$bool$methods,1L};
struct $bool $f = {&$bool$methods,0L};

$bool $True = &$t;
$bool $False = &$f;


$bool $default__bool__($struct self) {
  return $True;
}
