typedef struct $Slice {
  int *start;
  int *stop;
  int *step;
} *$Slice;

void normalize_slice($Slice slc, int len, int *slen, int *start, int *stop, int *step);
