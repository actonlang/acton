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

#include "../builtin.h"
#include <stdio.h>

int main() {
  B_float a = toB_float(7.0);
  B_float b = toB_float(5.0);
  B_RealD_float wit = B_RealD_floatG_witness;
  B_float c = wit->$class->__truediv__(wit,a,b);
  printf("7.0/5.0=%f\n",fromB_float(c));
  B_int d = wit->$class->__floor__(wit,(B_Integral)B_IntegralD_intG_witness,c);
  printf("floor(7/5)=%ld\n",fromB_int(d));
  printf("round(1234567.14,-5)=%f\n",B_RealD_floatD___round__(B_RealD_floatG_witness,toB_float(1234567.14),toB_int(-5))->val);
  printf("round(1.2345678,5)=%f\n",B_RealD_floatD___round__(B_RealD_floatG_witness,toB_float(1.2345678),toB_int(5))->val);
  printf("%f\n",fromB_float(wit->$class->__fromatom__(wit,(B_atom)$PlusB_strG_witness->$class->__add__($PlusB_strG_witness,to$str("3."),to$str("14")))));
}
