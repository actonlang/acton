

// Serialization ///////////////////////////////////////////////////////////////////////

void $bool_init($bool self, long val){
  self->val = val;
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

struct $bool$class $bool$methods = {"",UNASSIGNED,NULL,$bool_init, $bool_bool, $bool_str, $bool_serialize, $bool_deserialize};

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

$bool $true = &$t;
$bool $false = &$f;
