typedef struct $float$__methods__ {
  None (*__serialize__)($float, $WORD*, int, $dict, $ROWLISTHEADER);
  $float (*__deserialize__)($ROW*, $dict);
} *$float$__methods__;

None $float_serialize($float self, $WORD *prefix, int prefix_size, $dict done, $ROWLISTHEADER accum);
$float $float_deserialize($ROW *row, $dict done);

struct $float {
  char *GCINFO;
  $float$__methods__ __class__;
  long val;
};

struct $float$__methods__ $float_table;

$float to$float(double x);
double from$float($float x);

Real$float Real$float_new();
Complex$float Complex$float_new();
Plus$float Plus$float_new();
Minus$float Minus$float_new();
Hashable$float Hashable$float_new();

