struct $float$class {
  char *$GCINFO;
  void (*__init__)($float, double);
  void (*__serialize__)($float, $Mapping$dict, long*, $dict, struct $ROWLISTHEADER*);
  $float (*__deserialize__)($Mapping$dict,$ROW*, $dict);
};


struct $float {
  struct $float$class *$class;
  double val;
};

extern struct $float$class $float$methods;

extern struct $Real$float$class $Real$float$methods;
extern struct $Complex$float$class $Complex$float$methods;
extern struct $Plus$float$class $Plus$float$methods;
extern struct $Minus$float$class $Minus$float$methods;
extern struct $Hashable$float$class $Hashable$float$methods;

extern struct $Real$float *$Real$float$witness;
extern struct $Complex$float *$Complex$float$witness;
extern struct $Plus$float *$Plus$float$witness;
extern struct $Minus$float *$Minus$float$witness;
extern struct $Hashable$float *$Hashable$float$witness;

$float to$float(double x);
double from$float($float x);
