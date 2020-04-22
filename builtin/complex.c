struct $complex {
  char *$GCINFO;
  double re;
  double im;
};

$complex to$complex($float re,$float im) {
  $complex res = malloc(sizeof(struct $complex));
  res->re = from$float(re);
  res->im = from$float(im);
  return res;
}
