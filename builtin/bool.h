struct $bool$class {
  char *$GCINFO;
  void (*__init__)($bool, long);
  void (*__serialize__)($bool, $Mapping$dict, long*, $dict, struct $ROWLISTHEADER*);
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
