typedef struct $int$__methods__ {
  None (*__serialize__)($int, $WORD*, int, $dict, $ROWLISTHEADER);
  $int (*__deserialize__)($ROW*, $dict);
} *$int$__methods__;

None $int_serialize($int self, $WORD *prefix, int prefix_size, $dict done, $ROWLISTHEADER accum);
$int $int_deserialize($ROW *row, $dict done);

struct $int {
  char *GCINFO;
  $int$__methods__ __class__;
  long val;
};

struct $int$__methods__ $int_table;

$int to$int(long n);
long from$int($int n);

Integral$int Integral$int_new();
Logical$int Logical$int_new();
Complex$int Complex$int_new();
Plus$int Plus$int_new();
Minus$int Minus$int_new();
Hashable$int Hashable$int_new();


