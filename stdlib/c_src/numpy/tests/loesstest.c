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

#include "../../builtin/builtin.h"
#include "../numpy.h"

numpyQ_ndarray B_pow3(numpyQ_IntegralD_ndarray wit, numpyQ_ndarray x) {
  numpyQ_ndarray tmp =  wit->$class->__mul__(wit,x,x);
  return wit->$class->__mul__(wit,tmp,x);
}

numpyQ_ndarray loess_simple(numpyQ_ndarray x, numpyQ_ndarray y, numpyQ_ndarray xin, B_int win) {
  B_list ix = $NEW(B_list,NULL,NULL);
  B_slice s = $NEW(B_slice,NULL,NULL,NULL);
  B_listD_append(ix,numpyQ_ndsliceG_new(s));
  B_listD_append(ix,numpyQ_ndindexG_new(numpy$G_newaxis));
  numpyQ_Primitive witp = (numpyQ_Primitive)numpyQ_PrimitiveB_floatG_witness;
  numpyQ_IntegralD_ndarray wit = $NEW(numpyQ_IntegralD_ndarray,witp);
  numpyQ_MinusD_ndarray wit2 = (numpyQ_MinusD_ndarray)wit-> W_Minus;
  numpyQ_ndarray tmp1 = wit2->$class->__sub__(wit2,xin,numpyQ_ndarrayD___ndgetslice__(x,ix));
  numpyQ_ndarray xd = numpyQ_abs(witp,tmp1);
  numpyQ_ndarray tmp2 = numpyQ_partition(witp,xd,win);
  B_list ix2 = $NEW(B_list,NULL,NULL);
  B_listD_append(ix2,numpyQ_ndsliceG_new(s));
  B_listD_append(ix2,numpyQ_ndindexG_new(win));
  numpyQ_ndarray tmp3 = numpyQ_ndarrayD___ndgetslice__(tmp2,ix2);
  numpyQ_ndarray tmp4 = numpyQ_ndarrayD___ndgetslice__(tmp3,ix);
  numpyQ_ndarray tmp5 = wit->$class->__truediv__(wit,xd,tmp4);
  numpyQ_ndarray w = numpyQ_clip(witp,tmp5,to$float(0.0),to$float(1.0));
  numpyQ_ndarray tmp6 = B_pow3(wit,w);
  numpyQ_ndarray tmp7 = wit2->$class->__sub__(wit2,numpyQ_fromatom(to$float(1.0)),tmp6);
  numpyQ_ndarray ws = B_pow3(wit,tmp7);
  numpyQ_ndarray a00 = numpyQ_sum(witp,ws,toB_int(1));
  numpyQ_ndarray a01 = numpyQ_dot(witp,ws,x);
  numpyQ_ndarray a11 = numpyQ_dot(witp,ws,wit->$class->__mul__(wit,x,x));
  numpyQ_ndarray b0 = numpyQ_dot(witp,ws,y);
  numpyQ_ndarray b1 = numpyQ_dot(witp,ws,wit->$class->__mul__(wit,x,y));
  numpyQ_ndarray det = wit2->$class->__sub__(wit2,wit->$class->__mul__(wit,a00,a11),wit->$class->__mul__(wit,a01,a01));
  numpyQ_ndarray tmp8 = wit2->$class->__sub__(wit2,wit->$class->__mul__(wit,a11,b0),wit->$class->__mul__(wit,a01,b1));
  numpyQ_ndarray tmp9 = wit2->$class->__sub__(wit2,wit->$class->__mul__(wit,a00,b1),wit->$class->__mul__(wit,a01,b0));
  numpyQ_ndarray tmp10 = wit->$class->__add__(wit,tmp8,wit->$class->__mul__(wit,tmp9,xin));
  return wit->$class->__truediv__(wit,tmp10,det);
}

numpyQ_ndarray mkarray(double elems[], int len){
  B_list lst = $NEW(B_list,NULL,NULL);
  for (int i =0; i< len; i++)
    B_listD_append(lst,to$float(elems[i]));
  numpyQ_Primitive wit0 = (numpyQ_Primitive)numpyQ_PrimitiveB_floatG_witness;
  return numpyQ_array(wit0,lst);
}
  
int main(int argc, char *argv[]) {
  long n;
  sscanf(argv[1],"%ld",&n);
  numpyQ_ndarray xx,yy;
  long win;
  $register_builtin();
  numpy$D___init__();
  if (n <= 21) {
    
    double xx0[] = {0.5578196, 2.0217271, 2.5773252, 3.4140288, 4.3014084,
                    4.7448394, 5.1073781, 6.5411662, 6.7216176, 7.2600583,
                    8.1335874, 9.1224379, 11.9296663, 12.3797674, 13.2728619,
                    14.2767453, 15.3731026, 15.6476637, 18.5605355, 18.5866354,
                    18.7572812};
    double yy0[] = {18.63654, 103.49646, 150.35391, 190.51031, 208.70115,
                    213.71135, 228.49353, 233.55387, 234.55054, 223.89225,
                    227.68339, 223.91982, 168.01999, 164.95750, 152.61107,
                    160.78742, 168.55567, 152.42658, 221.70702, 222.69040,
                    243.18828};
    n = 21;
    win = 6;
    /*

    double xx0[] = {0.0,0.25,0.5,0.75,1.0};
    double yy0[] = {0.9,1.0,1.2,1.3,1.3};

    n = 5;
    win = 2;
    */
    xx = mkarray(xx0,n);
    yy = mkarray(yy0,n);
  } else {
    double xx0[n], yy0[n];
    double step = 2*3.1415926535/(n-1);
    for (int i=0; i<n; i++) {
      xx0[i] = i*step;
      yy0[i] = sin(i*step);
    }
    numpyQ_IntegralD_ndarray wit = numpyQ_IntegralD_ndarrayG_new((numpyQ_Primitive)numpyQ_PrimitiveB_floatG_witness);
    xx = mkarray(xx0,n);
    yy = wit->$class->__add__(wit,mkarray(yy0,n),numpyQ_unirand(to$float(-0.5),to$float(0.5),toB_int(n)));
    win = n/4-1;
  }
  numpyQ_ndarray res = loess_simple(xx,yy,xx,toB_int(win));
  printf("set term aqua title \"Loess\"\n");
  printf("set multiplot\nplot  [0:6.3][-1.5:1.5] 0\nclear\n");
  printf("$scatterplot <<EOD\n");
  for (int i=0; i<n; i++)
    printf("%0.3f %0.3f %0.3f\n",xx->data[i].d, yy->data[i].d, res->data[i].d);
  printf("EOD\n");
  printf("plot [0:6.3][-1.5:1.5] \"$scatterplot\" using 1:2 with points title \"scatterplot, \\\"\n");
  printf("plot  [0:6.3][-1.5:1.5] \"$scatterplot\"using 1:3 with lines title \"smoothed\"\n");
}

