void $bool_init($bool self, long val);
void $bool_serialize($bool self, $Mapping$dict notused, long *start_no, $dict done, $ROWLISTHEADER accum);
$bool $bool_deserialize($Mapping$dict notused, $ROW *row, $dict done);

struct $bool$class $bool$methods = {"", $bool_init, $bool_serialize, $bool_deserialize};


// Serialization ///////////////////////////////////////////////////////////////////////

void $bool_init($bool self, long val){
  self->val = val;
}

void $bool_serialize($bool n,  $Mapping$dict notused, long *start_no, $dict done, $ROWLISTHEADER accum) {
  $enqueue(accum,$new_row(BOOL_ID,start_no,1,($WORD)&n->val));
}

$bool $bool_deserialize( $Mapping$dict notused, $ROW *row, $dict done) {
  $ROW this = *row;
  *row =this->next;
  long res;
  memcpy(&res,this->blob,sizeof(long));
  return to$bool(res);
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
