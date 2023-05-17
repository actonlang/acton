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

B_NoneType B_NoneTypeG_new() {
  return $NEW(B_NoneType);
}


void B_NoneTypeD__serialize__(B_NoneType self, $Serial$state state) {
  $add_header(NONE_ID,0,state);
}

B_NoneType B_NoneTypeD__deserialize__(B_NoneType self, $Serial$state state) {
  state->row = state->row->next;
  state->row_no++;
  return NULL;
}

B_bool B_NoneTypeD__bool__(B_NoneType self) {
  return B_False;
}

B_str B_NoneTypeD__str__(B_NoneType self) {
  return to$str("None");
}

struct B_NoneTypeG_class B_NoneTypeG_methods = {0,"B_NoneType",UNASSIGNED,($SuperG_class)&B_valueG_methods,(void (*)(B_NoneType))$default__init__,                                            B_NoneTypeD__serialize__,  B_NoneTypeD__deserialize__, B_NoneTypeD__bool__, B_NoneTypeD__str__, B_NoneTypeD__str__};
