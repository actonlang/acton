struct $bool {
  char *GCINFO;
  int val;
};
  
$bool to$bool(int b) {
  $bool res = malloc(sizeof(struct $bool));
  res->val = b;
  return res;
}
    
int from$bool($bool b) {
  return b->val;
}

struct $bool $t = {"",1};
struct $bool $f = {"",0};

$bool $true = &$t;
$bool $false = &$f;
