/*
 * Copyright (C) 2019-2021 Data Ductus AB
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#pragma once


int $elem_size(enum ElemType typ) {
    switch (typ) {
    case LongType:
        return 1;
    case DblType:
        return 1;
    }
}

#define LONGOP(op,sym) static union $Bytes8 op(union $Bytes8 a, union $Bytes8 b) { \
        union $Bytes8 res;                                              \
        res.l = a.l sym b.l;                                            \
        return res;                                                     \
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
        return a.l sym b.l;                                             \
    } 

LONGBOOLOP(l$eq,==)
LONGBOOLOP(l$neq,!=)
LONGBOOLOP(l$lt,<)
LONGBOOLOP(l$le,<=)
LONGBOOLOP(l$gt,>)
LONGBOOLOP(l$ge,>=)
  
#define DBLOP(op,sym) static union $Bytes8 op(union $Bytes8 a, union $Bytes8 b) { \
        union $Bytes8 res;                                              \
        res.d = a.d sym b.d;                                            \
        return res;                                                     \
    } 

DBLOP(d$add,+)
DBLOP(d$sub,-)
DBLOP(d$mul,*)
DBLOP(d$truediv,/)

#define DBLBOOLOP(op,sym) static bool op(union $Bytes8 a, union $Bytes8 b) { \
        return a.d sym b.d;                                             \
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

B_str l$prim_str(union $Bytes8 n) {
    char *s;
    asprintf(&s,"%ld",n.l);
    return to$str(s);
}

B_str d$prim_str(union $Bytes8 x) {
    char *s;
    asprintf(&s,"%g",x.d);
    return to$str(s);
}

union $Bytes8 l$pow(union $Bytes8 a, union $Bytes8 b) {
    union $Bytes8 res;
    if (b.l < 0)
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("pow for ndarray[int]: negative value in exponent array")));
    res.l = longpow(a.l,b.l);
    return res;
}

union $Bytes8 d$pow(union $Bytes8  a, union $Bytes8 b) {
    union $Bytes8 res;
    res.d = exp(b.d * log(a.d));
    return res;
}

$WORD to$objB_int(union $Bytes8 x) {
    return  toB_int(x.l);
}

union $Bytes8 from$objB_int($WORD x) {
    union $Bytes8 res;
    res.l = fromB_int((B_int)x);
    return res;
}

$WORD to$objB_float(union $Bytes8 x) {
    return toB_float(x.d);
}

union $Bytes8 from$objB_float($WORD x) {
    union $Bytes8 res;
    res.d = ((B_float)x)->val;
    return res;
}

void numpy$$PrimitiveB_int$serialize(numpy$$PrimitiveB_int self, $Serial$state state) {
}

numpy$$PrimitiveB_int numpy$$PrimitiveB_int$deserialize(numpy$$PrimitiveB_int self, $Serial$state state) {
    numpy$$PrimitiveB_int res = (numpy$$PrimitiveB_int)$DNEW(numpy$$PrimitiveB_int,state);
    return res;
}

void numpy$$PrimitiveB_float$serialize(numpy$$PrimitiveB_float self, $Serial$state state) {
}

numpy$$PrimitiveB_float numpy$$PrimitiveB_float$deserialize(numpy$$PrimitiveB_float self, $Serial$state state) {
    numpy$$PrimitiveB_float res = (numpy$$PrimitiveB_float)$DNEW(numpy$$PrimitiveB_float,state);
    return res;
}


struct numpy$$PrimitiveB_intG_class numpy$$PrimitiveB_intG_methods = {"numpy$$PrimitiveB_int",UNASSIGNED,NULL,(void (*)(numpy$$PrimitiveB_int))$default__init__,
                                                                  numpy$$PrimitiveB_int$serialize,numpy$$PrimitiveB_int$deserialize,NULL,NULL,NULL,
                                                                  LongType,to$objB_int,from$objB_int,l$prim_str,
                                                                  l$add,l$sub,l$mul,l$truediv,l$floordiv,l$mod,l$land,l$lor,l$band,l$bor,l$bxor,l$lsh,l$rsh,l$pow,
                                                                  l$iadd,l$isub,l$imul,l$itruediv,l$ifloordiv,l$imod,l$iband,l$ibor,l$ibxor,l$ilsh,l$irsh,
                                                                  l$eq,l$neq,l$lt,l$le,l$gt,l$ge,l$abs,l$neg,l$lnot,l$bnot};


struct numpy$$PrimitiveB_floatG_class numpy$$PrimitiveB_floatG_methods = {"numpy$$PrimitiveB_float",UNASSIGNED,NULL,(void (*)(numpy$$PrimitiveB_float))$default__init__, 
                                                                      numpy$$PrimitiveB_float$serialize,numpy$$PrimitiveB_float$deserialize,NULL,NULL,NULL,
                                                                      DblType,to$objB_float,from$objB_float,d$prim_str,
                                                                      d$add,d$sub,d$mul,d$truediv,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,d$pow,
                                                                      d$iadd,d$isub,d$imul,d$itruediv,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                                                                      d$eq,d$neq,d$lt,d$le,d$gt,d$ge,d$abs,d$neg,NULL,NULL};

struct numpy$$PrimitiveB_int numpy$$PrimitiveB_intD_instance = {&numpy$$PrimitiveB_intG_methods};
numpy$$PrimitiveB_int numpy$$PrimitiveB_intG_witness = &numpy$$PrimitiveB_intD_instance;

struct numpy$$PrimitiveB_float numpy$$PrimitiveB_floatD_instance = {&numpy$$PrimitiveB_floatG_methods};
numpy$$PrimitiveB_float numpy$$PrimitiveB_floatG_witness = &numpy$$PrimitiveB_floatD_instance;

numpy$$PrimitiveB_int numpy$$PrimitiveB_intG_new() {
    numpy$$PrimitiveB_int res = malloc(sizeof(struct numpy$$PrimitiveB_int));
    res->$class = &numpy$$PrimitiveB_intG_methods;
    return res;
}
numpy$$PrimitiveB_float numpy$$PrimitiveB_floatG_new() {
    numpy$$PrimitiveB_float res = malloc(sizeof(struct numpy$$PrimitiveB_float));
    res->$class = &numpy$$PrimitiveB_floatG_methods;
    return res;
}
