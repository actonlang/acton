typedef struct $bool$__methods__ {
  None (*__serialize__)($bool, $WORD*, int, $dict, $ROWLISTHEADER);
  $bool (*__deserialize__)($ROW*, $dict);
} *$bool$__methods__;

None $bool_serialize($bool self, $WORD *prefix, int prefix_size, $dict done, $ROWLISTHEADER accum);
$bool $bool_deserialize($ROW *row, $dict done);

struct $bool {
  char *GCINFO;
  $bool$__methods__ __class__;
  long val;
};

struct $bool$__methods__ $bool_table;

$bool to$bool(long b);
long from$bool($bool b);

$bool $true, $false;
