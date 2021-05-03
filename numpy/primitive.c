
int $elem_size(enum ElemType typ) {
  switch (typ) {
  case LongType:
    return 1;
  case DblType:
    return 1;
  }
}

#define LONGOP(op,sym) static union $Bytes8 op(union $Bytes8 a, union $Bytes8 b) { \
  union $Bytes8 res; \
  res.l = a.l sym b.l; \
  return res; \
} 

LONGOP(l$add,+)
LONGOP(l$sub,-)
LONGOP(l$mul,*)
LONGOP(l$floordiv,/)
LONGOP(l$mod,%)
LONGOP(l$land,&&)
LONGOP(l$lor,||)
LONGOP(l$band,&)
LONGOP(l$bor,|)
LONGOP(l$bxor,^)
LONGOP(l$lsh,<<)
LONGOP(l$rsh,>>)

static union $Bytes8 l$truediv(union $Bytes8 a, union $Bytes8 b) {
  union $Bytes8 res;
  res.d = (double)a.l/(double)b.l;
  return res;
} 

#define LONGBOOLOP(op,sym) static bool op(union $Bytes8 a, union $Bytes8 b) { \
  return a.l sym b.l; \
} 

LONGBOOLOP(l$eq,==)
LONGBOOLOP(l$neq,!=)
LONGBOOLOP(l$lt,<)
LONGBOOLOP(l$le,<=)
LONGBOOLOP(l$gt,>)
LONGBOOLOP(l$ge,>=)
  
#define DBLOP(op,sym) static union $Bytes8 op(union $Bytes8 a, union $Bytes8 b) { \
  union $Bytes8 res; \
  res.d = a.d sym b.d; \
  return res; \
} 

DBLOP(d$add,+)
DBLOP(d$sub,-)
DBLOP(d$mul,*)
DBLOP(d$truediv,/)

#define DBLBOOLOP(op,sym) static bool op(union $Bytes8 a, union $Bytes8 b) { \
  return a.d sym b.d; \
} 
DBLBOOLOP(d$eq,==)
DBLBOOLOP(d$neq,!=)
DBLBOOLOP(d$lt,<)
DBLBOOLOP(d$le,<=)
DBLBOOLOP(d$gt,>)
DBLBOOLOP(d$ge,>=)

#define LONGINCROP(op,sym) static void op(union $Bytes8 *a, union $Bytes8 b) { (*a).l sym b.l;}

LONGINCROP(l$iadd,+=)
LONGINCROP(l$isub,-=)
LONGINCROP(l$imul,*=)
LONGINCROP(l$ifloordiv,/=)
LONGINCROP(l$imod,%=)
LONGINCROP(l$iband,&=)
LONGINCROP(l$ibor,|=)
LONGINCROP(l$ibxor,^=)
LONGINCROP(l$ilsh,<<=)
LONGINCROP(l$irsh,>>=)

static void l$itruediv(union $Bytes8 *a, union $Bytes8 b) {
  fprintf(stderr,"Internal error: executing numpy.l$itruediv\n");
  exit(-1);
} 

#define DBLINCROP(op,sym) static void op(union $Bytes8 *a, union $Bytes8 b) { (*a).d sym b.d;}

DBLINCROP(d$iadd,+=)
DBLINCROP(d$isub,-=)
DBLINCROP(d$imul,*=)
DBLINCROP(d$itruediv,/=)

#define LONGUNARY(op,sym) static union $Bytes8 op(union $Bytes8 a) {union $Bytes8 res; res.l = sym(a.l); return res;}

LONGUNARY(l$abs,labs)
LONGUNARY(l$neg,-)
LONGUNARY(l$lnot,!)
LONGUNARY(l$bnot,~)

#define DBLUNARY(op,sym)  static union $Bytes8 op(union $Bytes8 a) {union $Bytes8 res; res.d = sym(a.d); return res;}

DBLUNARY(d$abs,fabs)
DBLUNARY(d$neg,-)

$str l$prim_str(union $Bytes8 n) {
  char *s;
  asprintf(&s,"%ld",n.l);
  return to$str(s);
}

$str d$prim_str(union $Bytes8 x) {
  char *s;
  asprintf(&s,"%g",x.d);
  return to$str(s);
}

union $Bytes8 l$pow(union $Bytes8 a, union $Bytes8 b) {
  union $Bytes8 res;
  if (b.l < 0)
    RAISE(($BaseException)$NEW($ValueError,to$str("pow for ndarray[int]: negative value in exponent array")));
  res.l = longpow(a.l,b.l);
  return res;
}

union $Bytes8 d$pow(union $Bytes8  a, union $Bytes8 b) {
  union $Bytes8 res;
  res.d = exp(b.d * log(a.d));
  return res;
}

$WORD to$obj$int(union $Bytes8 x) {
  return to$int(x.l);
}

union $Bytes8 from$obj$int($WORD x) {
  union $Bytes8 res;
  res.l = (($int)x)->val;
  return res;
}

$WORD to$obj$float(union $Bytes8 x) {
  return to$float(x.d);
}

union $Bytes8 from$obj$float($WORD x) {
  union $Bytes8 res;
  res.d = (($float)x)->val;
  return res;
}

void numpy$$Primitive$int$serialize(numpy$$Primitive$int self, $Serial$state state) {
}

numpy$$Primitive$int numpy$$Primitive$int$deserialize(numpy$$Primitive$int self, $Serial$state state) {
  numpy$$Primitive$int res = (numpy$$Primitive$int)$DNEW(numpy$$Primitive$int,state);
  return res;
}

void numpy$$Primitive$float$serialize(numpy$$Primitive$float self, $Serial$state state) {
}

numpy$$Primitive$float numpy$$Primitive$float$deserialize(numpy$$Primitive$float self, $Serial$state state) {
  numpy$$Primitive$float res = (numpy$$Primitive$float)$DNEW(numpy$$Primitive$float,state);
  return res;
}


struct numpy$$Primitive$int$class numpy$$Primitive$int$methods = {"numpy$$Primitive$int",UNASSIGNED,NULL,(void (*)(numpy$$Primitive$int))$default__init__,
                                                                  numpy$$Primitive$int$serialize,numpy$$Primitive$int$deserialize,NULL,NULL,
                                                                  LongType,to$obj$int,from$obj$int,l$prim_str,
                                                                  l$add,l$sub,l$mul,l$truediv,l$floordiv,l$mod,l$land,l$lor,l$band,l$bor,l$bxor,l$lsh,l$rsh,l$pow,
                                                                  l$iadd,l$isub,l$imul,l$itruediv,l$ifloordiv,l$imod,l$iband,l$ibor,l$ibxor,l$ilsh,l$irsh,
                                                                  l$eq,l$neq,l$lt,l$le,l$gt,l$ge,l$abs,l$neg,l$lnot,l$bnot};


struct numpy$$Primitive$float$class numpy$$Primitive$float$methods = {"numpy$$Primitive$float",UNASSIGNED,NULL,(void (*)(numpy$$Primitive$float))$default__init__, 
                                                                      numpy$$Primitive$float$serialize,numpy$$Primitive$float$deserialize,NULL,NULL,
                                                                      DblType,to$obj$float,from$obj$float,d$prim_str,
                                                                      d$add,d$sub,d$mul,d$truediv,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,d$pow,
                                                                      d$iadd,d$isub,d$imul,d$itruediv,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                                                                      d$eq,d$neq,d$lt,d$le,d$gt,d$ge,d$abs,d$neg,NULL,NULL};

struct numpy$$Primitive$int numpy$$Primitive$int_instance = {&numpy$$Primitive$int$methods};
numpy$$Primitive$int numpy$$Primitive$int$witness = &numpy$$Primitive$int_instance;

struct numpy$$Primitive$float numpy$$Primitive$float_instance = {&numpy$$Primitive$float$methods};
numpy$$Primitive$float numpy$$Primitive$float$witness = &numpy$$Primitive$float_instance;

numpy$$Primitive$int numpy$$Primitive$int$new() {
  numpy$$Primitive$int res = malloc(sizeof(struct numpy$$Primitive$int));
  res->$class = &numpy$$Primitive$int$methods;
  return res;
}
numpy$$Primitive$float numpy$$Primitive$float$new() {
  numpy$$Primitive$float res = malloc(sizeof(struct numpy$$Primitive$float));
  res->$class = &numpy$$Primitive$float$methods;
  return res;
}
