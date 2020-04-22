struct $bool$class {
  char *GCINFO;
  $None (*__serialize__)($bool, $Mapping$dict, $WORD*, int, $dict, $ROWLISTHEADER);
  $bool (*__deserialize__)($Mapping$dict, $ROW*, $dict);
};

struct $bool {
  struct $bool$class *$class;
  long val;
};

extern struct $bool$class $bool$methods;

$bool to$bool(long b);
long from$bool($bool b);

$bool $true, $false;
