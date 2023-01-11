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

numpy$$ndarray $pow3(numpy$B_IntegralD_ndarray wit, numpy$$ndarray x) {
  numpy$$ndarray tmp =  wit->$class->__mul__(wit,x,x);
  return wit->$class->__mul__(wit,tmp,x);
}

numpy$$ndarray loess_simple(numpy$$ndarray x, numpy$$ndarray y, numpy$$ndarray xin, B_int win) {
  B_list ix = $NEW(B_list,NULL,NULL);
  B_slice s = $NEW(B_slice,NULL,NULL,NULL);
  B_listD_append(ix,numpy$$ndsliceG_new(s));
  B_listD_append(ix,numpy$$ndindexG_new(numpy$G_newaxis));
  numpy$$Primitive witp = (numpy$$Primitive)numpy$$PrimitiveB_floatG_witness;
  numpy$B_IntegralD_ndarray wit = $NEW(numpy$B_IntegralD_ndarray,witp);
  numpy$B_MinusD_ndarray wit2 = (numpy$B_MinusD_ndarray)wit-> W_Minus;
  numpy$$ndarray tmp1 = wit2->$class->__sub__(wit2,xin,numpy$$ndarrayD___ndgetslice__(x,ix));
  numpy$$ndarray xd = numpy$$abs(witp,tmp1);
  numpy$$ndarray tmp2 = numpy$$partition(witp,xd,win);
  B_list ix2 = $NEW(B_list,NULL,NULL);
  B_listD_append(ix2,numpy$$ndsliceG_new(s));
  B_listD_append(ix2,numpy$$ndindexG_new(win));
  numpy$$ndarray tmp3 = numpy$$ndarrayD___ndgetslice__(tmp2,ix2);
  numpy$$ndarray tmp4 = numpy$$ndarrayD___ndgetslice__(tmp3,ix);
  numpy$$ndarray tmp5 = wit->$class->__truediv__(wit,xd,tmp4);
  numpy$$ndarray w = numpy$$clip(witp,tmp5,toB_float(0.0),toB_float(1.0));
  numpy$$ndarray tmp6 = $pow3(wit,w);
  numpy$$ndarray tmp7 = wit2->$class->__sub__(wit2,numpy$$fromatom(toB_float(1.0)),tmp6);
  numpy$$ndarray ws = $pow3(wit,tmp7);
  numpy$$ndarray a00 = numpy$$sum(witp,ws,toB_int(1));
  numpy$$ndarray a01 = numpy$$dot(witp,ws,x);
  numpy$$ndarray a11 = numpy$$dot(witp,ws,wit->$class->__mul__(wit,x,x));
  numpy$$ndarray b0 = numpy$$dot(witp,ws,y);
  numpy$$ndarray b1 = numpy$$dot(witp,ws,wit->$class->__mul__(wit,x,y));
  numpy$$ndarray det = wit2->$class->__sub__(wit2,wit->$class->__mul__(wit,a00,a11),wit->$class->__mul__(wit,a01,a01));
  numpy$$ndarray tmp8 = wit2->$class->__sub__(wit2,wit->$class->__mul__(wit,a11,b0),wit->$class->__mul__(wit,a01,b1));
  numpy$$ndarray tmp9 = wit2->$class->__sub__(wit2,wit->$class->__mul__(wit,a00,b1),wit->$class->__mul__(wit,a01,b0));
  numpy$$ndarray tmp10 = wit->$class->__add__(wit,tmp8,wit->$class->__mul__(wit,tmp9,xin));
  return wit->$class->__truediv__(wit,tmp10,det);
}

numpy$$ndarray mkarray(double elems[], int len){
  B_list lst = $NEW(B_list,NULL,NULL);
  for (int i =0; i< len; i++)
    B_listD_append(lst,toB_float(elems[i]));
  numpy$$Primitive wit0 = (numpy$$Primitive)numpy$$PrimitiveB_floatG_witness;
  return numpy$$array(wit0,lst);
}
  
int main(int argc, char *argv[]) {
  long n;
  sscanf(argv[1],"%ld",&n);
  numpy$$ndarray xx,yy;
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
    numpy$B_IntegralD_ndarray wit = numpy$B_IntegralD_ndarrayG_new((numpy$$Primitive)numpy$$PrimitiveB_floatG_witness);
    xx = mkarray(xx0,n);
    yy = wit->$class->__add__(wit,mkarray(yy0,n),numpy$$unirand(toB_float(-0.5),toB_float(0.5),toB_int(n)));
    win = n/4-1;
  }
  numpy$$ndarray res = loess_simple(xx,yy,xx,toB_int(win));
  printf("set term aqua title \"Loess\"\n");
  printf("set multiplot\nplot  [0:6.3][-1.5:1.5] 0\nclear\n");
  printf("$scatterplot <<EOD\n");
  for (int i=0; i<n; i++)
    printf("%0.3f %0.3f %0.3f\n",xx->data[i].d, yy->data[i].d, res->data[i].d);
  printf("EOD\n");
  printf("plot [0:6.3][-1.5:1.5] \"$scatterplot\" using 1:2 with points title \"scatterplot, \\\"\n");
  printf("plot  [0:6.3][-1.5:1.5] \"$scatterplot\"using 1:3 with lines title \"smoothed\"\n");
}

