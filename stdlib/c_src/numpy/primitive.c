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

LONGOP(B_l_add,+)
LONGOP(B_l_sub,-)
LONGOP(B_l_mul,*)
LONGOP(B_l_floordiv,/)
LONGOP(B_l_mod,%)
LONGOP(B_l_land,&&)
LONGOP(B_l_lor,||)
LONGOP(B_l_band,&)
LONGOP(B_l_bor,|)
LONGOP(B_l_bxor,^)
LONGOP(B_l_lsh,<<)
LONGOP(B_l_rsh,>>)

static union $Bytes8 B_l_truediv(union $Bytes8 a, union $Bytes8 b) {
    union $Bytes8 res;
    res.d = (double)a.l/(double)b.l;
    return res;
} 

#define LONGBOOLOP(op,sym) static bool op(union $Bytes8 a, union $Bytes8 b) { \
        return a.l sym b.l;                                             \
    } 

LONGBOOLOP(B_l_eq,==)
LONGBOOLOP(B_l_neq,!=)
LONGBOOLOP(B_l_lt,<)
LONGBOOLOP(B_l_le,<=)
LONGBOOLOP(B_l_gt,>)
LONGBOOLOP(B_l_ge,>=)
  
#define DBLOP(op,sym) static union $Bytes8 op(union $Bytes8 a, union $Bytes8 b) { \
        union $Bytes8 res;                                              \
        res.d = a.d sym b.d;                                            \
        return res;                                                     \
    } 

DBLOP(B_d_add,+)
DBLOP(B_d_sub,-)
DBLOP(B_d_mul,*)
DBLOP(B_d_truediv,/)

#define DBLBOOLOP(op,sym) static bool op(union $Bytes8 a, union $Bytes8 b) { \
        return a.d sym b.d;                                             \
    } 
DBLBOOLOP(B_d_eq,==)
DBLBOOLOP(B_d_neq,!=)
DBLBOOLOP(B_d_lt,<)
DBLBOOLOP(B_d_le,<=)
DBLBOOLOP(B_d_gt,>)
DBLBOOLOP(B_d_ge,>=)

#define LONGINCROP(op,sym) static void op(union $Bytes8 *a, union $Bytes8 b) { (*a).l sym b.l;}

LONGINCROP(B_l_iadd,+=)
LONGINCROP(B_l_isub,-=)
LONGINCROP(B_l_imul,*=)
LONGINCROP(B_l_ifloordiv,/=)
LONGINCROP(B_l_imod,%=)
LONGINCROP(B_l_iband,&=)
LONGINCROP(B_l_ibor,|=)
LONGINCROP(B_l_ibxor,^=)
LONGINCROP(B_l_ilsh,<<=)
LONGINCROP(B_l_irsh,>>=)

static void B_l_itruediv(union $Bytes8 *a, union $Bytes8 b) {
    fprintf(stderr,"Internal error: executing numpy.B_l_itruediv\n");
    exit(-1);
} 

#define DBLINCROP(op,sym) static void op(union $Bytes8 *a, union $Bytes8 b) { (*a).d sym b.d;}

DBLINCROP(B_d_iadd,+=)
DBLINCROP(B_d_isub,-=)
DBLINCROP(B_d_imul,*=)
DBLINCROP(B_d_itruediv,/=)

#define LONGUNARY(op,sym) static union $Bytes8 op(union $Bytes8 a) {union $Bytes8 res; res.l = sym(a.l); return res;}

LONGUNARY(lB_abs,labs)
LONGUNARY(B_l_neg,-)
LONGUNARY(B_l_lnot,!)
LONGUNARY(B_l_bnot,~)

#define DBLUNARY(op,sym)  static union $Bytes8 op(union $Bytes8 a) {union $Bytes8 res; res.d = sym(a.d); return res;}

DBLUNARY(dB_abs,fabs)
DBLUNARY(B_d_neg,-)

B_str B_l_prim_str(union $Bytes8 n) {
    char *s;
    asprintf(&s,"%ld",n.l);
    return to$str(s);
}

B_str B_d_prim_str(union $Bytes8 x) {
    char *s;
    asprintf(&s,"%g",x.d);
    return to$str(s);
}

union $Bytes8 lB_pow(union $Bytes8 a, union $Bytes8 b) {
    union $Bytes8 res;
    if (b.l < 0)
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("pow for ndarray[int]: negative value in exponent array")));
    res.l = longpow(a.l,b.l);
    return res;
}

union $Bytes8 dB_pow(union $Bytes8  a, union $Bytes8 b) {
    union $Bytes8 res;
    res.d = exp(b.d * log(a.d));
    return res;
}

$WORD to$objB_int(union $Bytes8 x) {
    return  to$int(x.l);
}

union $Bytes8 from$objB_int($WORD x) {
    union $Bytes8 res;
    res.l = from$int((B_int)x);
    return res;
}

$WORD to$objB_float(union $Bytes8 x) {
    return to$float(x.d);
}

union $Bytes8 from$objB_float($WORD x) {
    union $Bytes8 res;
    res.d = ((B_float)x)->val;
    return res;
}

void numpyQ_PrimitiveD_intD_serialize(numpyQ_PrimitiveD_int self, $Serial$state state) {
}

numpyQ_PrimitiveD_int numpyQ_PrimitiveD_intD_deserialize(numpyQ_PrimitiveD_int self, $Serial$state state) {
    numpyQ_PrimitiveD_int res = (numpyQ_PrimitiveD_int)$DNEW(numpyQ_PrimitiveD_int,state);
    return res;
}

void numpyQ_PrimitiveD_floatD_serialize(numpyQ_PrimitiveD_float self, $Serial$state state) {
}

numpyQ_PrimitiveD_float numpyQ_PrimitiveD_floatD_deserialize(numpyQ_PrimitiveD_float self, $Serial$state state) {
    numpyQ_PrimitiveD_float res = (numpyQ_PrimitiveD_float)$DNEW(numpyQ_PrimitiveD_float,state);
    return res;
}


struct numpyQ_PrimitiveD_intG_class numpyQ_PrimitiveD_intG_methods = {0,"numpyQ_PrimitiveD_int",UNASSIGNED,NULL,(B_NoneType (*)(numpyQ_PrimitiveD_int))$default__init__,                                                                  numpyQ_PrimitiveD_intD_serialize,numpyQ_PrimitiveD_intD_deserialize,NULL,NULL,NULL,
                                                                  LongType,to$objB_int,from$objB_int,B_l_prim_str,
                                                                  B_l_add,B_l_sub,B_l_mul,B_l_truediv,B_l_floordiv,B_l_mod,B_l_land,B_l_lor,B_l_band,B_l_bor,B_l_bxor,B_l_lsh,B_l_rsh,lB_pow,
                                                                  B_l_iadd,B_l_isub,B_l_imul,B_l_itruediv,B_l_ifloordiv,B_l_imod,B_l_iband,B_l_ibor,B_l_ibxor,B_l_ilsh,B_l_irsh,
                                                                  B_l_eq,B_l_neq,B_l_lt,B_l_le,B_l_gt,B_l_ge,lB_abs,B_l_neg,B_l_lnot,B_l_bnot};


struct numpyQ_PrimitiveD_floatG_class numpyQ_PrimitiveD_floatG_methods = {0,"numpyQ_PrimitiveD_float",UNASSIGNED,NULL,(B_NoneType (*)(numpyQ_PrimitiveD_float))$default__init__,                                                                       numpyQ_PrimitiveD_floatD_serialize,numpyQ_PrimitiveD_floatD_deserialize,NULL,NULL,NULL,
                                                                      DblType,to$objB_float,from$objB_float,B_d_prim_str,
                                                                      B_d_add,B_d_sub,B_d_mul,B_d_truediv,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,dB_pow,
                                                                      B_d_iadd,B_d_isub,B_d_imul,B_d_itruediv,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                                                                      B_d_eq,B_d_neq,B_d_lt,B_d_le,B_d_gt,B_d_ge,dB_abs,B_d_neg,NULL,NULL};

struct numpyQ_PrimitiveD_int numpyQ_PrimitiveD_intD_instance = {&numpyQ_PrimitiveD_intG_methods};
numpyQ_PrimitiveD_int numpyQ_PrimitiveD_intG_witness = &numpyQ_PrimitiveD_intD_instance;

struct numpyQ_PrimitiveD_float numpyQ_PrimitiveD_floatD_instance = {&numpyQ_PrimitiveD_floatG_methods};
numpyQ_PrimitiveD_float numpyQ_PrimitiveD_floatG_witness = &numpyQ_PrimitiveD_floatD_instance;

numpyQ_PrimitiveD_int numpyQ_PrimitiveD_intG_new() {
    numpyQ_PrimitiveD_int res = malloc(sizeof(numpyQ_PrimitiveD_int));
    res->$class = &numpyQ_PrimitiveD_intG_methods;
    return res;
}
numpyQ_PrimitiveD_float numpyQ_PrimitiveD_floatG_new() {
    numpyQ_PrimitiveD_float res = malloc(sizeof(numpyQ_PrimitiveD_float));
    res->$class = &numpyQ_PrimitiveD_floatG_methods;
    return res;
}
