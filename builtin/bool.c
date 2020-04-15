

struct $bool$__methods__ $bool_table = {$bool_serialize, $bool_deserialize};
$bool$__methods__ $bool_methods = &$bool_table;

// Serialization ///////////////////////////////////////////////////////////////////////

None $bool_serialize($bool n, $WORD *prefix, int prefix_size, $dict done, $ROWLISTHEADER accum) {
  $ROW row = new_row(BOOL_ID,prefix_size,1,prefix);
  row->data[prefix_size] = ($WORD)from$bool(n);
  enqueue(accum,row);
}

$bool $bool_deserialize($ROW *row, $dict done) {
  $ROW this = *row;
  *row =this->next;
  return to$bool((long)this->data[this->prefix_size]);
}


$bool to$bool(long b) {
  $bool res = malloc(sizeof(struct $bool));
  res->__class__ = $bool_methods;
  res->val = b;
  return res;
}
    
long from$bool($bool b) {
  return b->val;
}

struct $bool $t = {"",&$bool_table,1L};
struct $bool $f = {"",&$bool_table,0L};

$bool $true = &$t;
$bool $false = &$f;
