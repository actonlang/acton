void $bool_init($bool self, long val);
void $bool_serialize($bool self, $Serial$state);
$bool $bool_deserialize($Serial$state);

struct $bool$class $bool$methods = {"",UNASSIGNED,NULL,$bool_init, $bool_serialize, $bool_deserialize};


// Serialization ///////////////////////////////////////////////////////////////////////

void $bool_init($bool self, long val){
  self->val = val;
}

void $bool_serialize($bool n, $Serial$state state) {
  $val_serialize(BOOL_ID,&n->val,state);
}

$bool $bool_deserialize($Serial$state state) {
  return to$bool((long)$val_deserialize(state));
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

$bool $true = &$t;
$bool $false = &$f;
