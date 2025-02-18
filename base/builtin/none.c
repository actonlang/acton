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

struct B_NoneTypeG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_NoneType);
    void (*__serialize__)(B_NoneType,$Serial$state);
    B_NoneType (*__deserialize__)(B_NoneType,$Serial$state);
};
struct B_NoneType {
    struct B_NoneTypeG_class *$class;
};

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

struct B_NoneTypeG_class B_NoneTypeG_methods = {"B_NoneType",UNASSIGNED,($SuperG_class)&B_valueG_methods,(void (*)(B_NoneType))$default__init__,
                                            B_NoneTypeD__serialize__,  B_NoneTypeD__deserialize__};


// Bool /////////////////////////////////////////////////////////////////////

struct B_BoolD_NoneTypeG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__) (B_BoolD_NoneType);
    void (*__serialize__) (B_BoolD_NoneType, $Serial$state);
    B_BoolD_NoneType (*__deserialize__) (B_BoolD_NoneType, $Serial$state);
    B_bool (*__bool__) (B_BoolD_NoneType, B_NoneType);
};

struct B_BoolD_NoneType {
    struct B_BoolD_NoneTypeG_class *$class;
};

void B_BoolD_NoneTypeD___serialize__(B_BoolD_NoneType self, $Serial$state state) {
}

B_BoolD_NoneType B_BoolD_NoneTypeD___deserialize__(B_BoolD_NoneType self, $Serial$state state) {
    B_BoolD_NoneType res = $DNEW(B_BoolD_NoneType,state);
    return res;
}

B_bool B_BoolD_NoneTypeD___bool__(B_BoolD_NoneType wit, B_NoneType self) {
  return B_False;
}

struct B_BoolD_NoneTypeG_class B_BoolD_NoneTypeG_methods = {"B_BoolD_NoneType", UNASSIGNED, ($SuperG_class)&B_BoolG_methods,(B_NoneType (*)(B_BoolD_NoneType))$default__init__, B_BoolD_NoneTypeD___serialize__, B_BoolD_NoneTypeD___deserialize__,B_BoolD_NoneTypeD___bool__};
   
// Show /////////////////////////////////////////////////////////////////////////////////////

struct B_ShowD_NoneTypeG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__) (B_ShowD_NoneType);
    void (*__serialize__) (B_ShowD_NoneType, $Serial$state);
    B_ShowD_NoneType (*__deserialize__) (B_ShowD_NoneType, $Serial$state);
    B_str (*__str__) (B_ShowD_NoneType, B_NoneType);
    B_str (*__repr__) (B_ShowD_NoneType, B_NoneType);
};

struct B_ShowD_NoneType {
    struct B_ShowD_NoneTypeG_class *$class;
};

void B_ShowD_NoneTypeD___serialize__ (B_ShowD_NoneType self, $Serial$state state) {
}

B_ShowD_NoneType B_ShowD_NoneTypeD___deserialize__ (B_ShowD_NoneType self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct B_ShowD_NoneType));
            self->$class = &B_ShowD_NoneTypeG_methods;
            return self;
        }
        self = $DNEW(B_ShowD_NoneType, state);
    }
    return self;
}

B_str B_ShowD_NoneTypeD___str__(B_ShowD_NoneType wit, B_NoneType self) {
  return to$str("None");
}

B_str B_ShowD_NoneTypeD___repr__(B_ShowD_NoneType wit, B_NoneType self) {
    return to$str("None");
}

struct B_ShowD_NoneTypeG_class B_ShowD_NoneTypeG_methods = {"B_ShowD_NoneType", UNASSIGNED, ($SuperG_class)&B_ShowG_methods,(B_NoneType (*)(B_ShowD_NoneType))$default__init__, B_ShowD_NoneTypeD___serialize__, B_ShowD_NoneTypeD___deserialize__,B_ShowD_NoneTypeD___str__, B_ShowD_NoneTypeD___repr__};
   
