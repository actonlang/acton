$None $bool_serialize($bool self, $Mapping$dict notused, $WORD *prefix, int prefix_size, $dict done, $ROWLISTHEADER accum);
$bool $bool_deserialize($Mapping$dict notused, $ROW *row, $dict done);

struct $bool$class $bool$methods = {"",$bool_serialize, $bool_deserialize};

// Serialization ///////////////////////////////////////////////////////////////////////

$None $bool_serialize($bool n,  $Mapping$dict notused, $WORD *prefix, int prefix_size, $dict done, $ROWLISTHEADER accum) {
  $ROW row = $new_row(BOOL_ID,prefix_size,1,prefix);
  row->data[prefix_size] = ($WORD)from$bool(n);
  $enqueue(accum,row);
}

$bool $bool_deserialize( $Mapping$dict notused, $ROW *row, $dict done) {
  $ROW this = *row;
  *row =this->next;
  return to$bool((long)this->data[this->prefix_size]);
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
