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

$Serializable $SerializableG_new() {
    return $NEW($Serializable);
}

B_NoneType $SerializableD___init__ ($Serializable self) {
    return B_None;
}
/*
B_value B_valueG_new() {
    return $NEW(B_value);
}

B_NoneType B_valueD___init__ (B_value self) {
    return B_None;
}

B_object B_objectG_new() {
    return $NEW(B_object);
}

B_NoneType B_objectD___init__ (B_object self) {
    return B_None;
}
*/
B_str B_valueD___str__(B_value self) {
    return $FORMAT("<%s object at %p>", unmangle_name(self->$class->$GCINFO), self);
}

B_str B_valueD___repr__(B_value self) {
    return $FORMAT("<%s object at %p>", unmangle_name(self->$class->$GCINFO), self);
}
/*
B_str B_objectD___str__(B_object self) {
    return $FORMAT("<%s object at %p>", unmangle_name(self->$class->$GCINFO), self);
}
*/
B_bool B_valueD___bool__(B_value self) {
    return B_True;
}
/*
B_bool B_objectD___bool__(B_object self) {
    return B_True;
}
*/

// Bool ///////////////////////////////////////////////////////////////

/* struct B_BoolD_valueG_class { */
/*     char *$GCINFO; */
/*     int $class_id; */
/*     $SuperG_class $superclass; */
/*     B_NoneType (*__init__)(B_BoolD_value); */
/*     void (*__serialize__)(B_BoolD_value,$Serial$state); */
/*     B_BoolD_value (*__deserialize__)(B_BoolD_value,$Serial$state); */
/*     B_bool (*__bool__)(B_BoolD_value, B_value); */
/* }; */

/* struct B_BoolD_value { */
/*     B_BoolD_valueG_class $class; */
/* }; */

B_bool B_BoolD_valueD___bool__(B_BoolD_value wit, B_value self) {
    return B_True;
}

/* B_NoneType B_BoolD_valueD___init__(B_BoolD_value self) { */
/*     return B_None; */
/* } */

/* void B_BoolD_valueD___serialize__(B_BoolD_value self, $Serial$state state) { */
/* } */

/* B_BoolD_value B_BoolD_valueD___deserialize__(B_BoolD_value self, $Serial$state state) { */
/*     B_BoolD_value res = $DNEW(B_BoolD_value,state); */
/*     return res; */
/* } */
/* struct B_BoolD_valueG_class B_BoolD_valueG_methods = { */
/*     "B_BoolD_value", */
/*     UNASSIGNED, */
/*     ($SuperG_class)&B_BoolG_methods, */
/*     B_BoolD_valueD___init__, */
/*     B_BoolD_valueD___serialize__, */
/*     B_BoolD_valueD___deserialize__, */
/*     B_BoolD_valueD___bool__ */
/* }; */

// Show ///////////////////////////////////////////////////////////////

/* struct B_ShowD_valueG_class { */
/*     char *$GCINFO; */
/*     int $class_id; */
/*     $SuperG_class $superclass; */
/*     B_NoneType (*__init__)(B_ShowD_value); */
/*     void (*__serialize__)(B_ShowD_value,$Serial$state); */
/*     B_ShowD_value (*__deserialize__)(B_ShowD_value,$Serial$state); */
/*     B_str (*__str__)(B_ShowD_value, B_value); */
/*     B_str (*__repr__)(B_ShowD_value, B_value); */
/* }; */

/* struct B_ShowD_value { */
/*     B_ShowD_valueG_class $class; */
/* }; */

B_str B_ShowD_valueD___str__( B_ShowD_value wit, B_value self) {
    return $FORMAT("<%s object at %p>", unmangle_name(self->$class->$GCINFO), self);
}

B_str B_ShowD_valueD___repr__(B_ShowD_value wit, B_value self) {
    return B_ShowD_valueD___str__(wit, self);
}

/* B_NoneType B_ShowD_valueD___init__(B_ShowD_value self) { */
/*     return B_None; */
/* } */

/* void B_ShowD_valueD___serialize__(B_ShowD_value self, $Serial$state state) { */
/* } */

/* B_ShowD_value B_ShowD_valueD___deserialize__(B_ShowD_value self, $Serial$state state) { */
/*     B_ShowD_value res = $DNEW(B_ShowD_value,state); */
/*     return res; */
/* } */
/* struct B_ShowD_valueG_class B_ShowD_valueG_methods = { */
/*     "B_ShowD_value", */
/*     UNASSIGNED, */
/*     ($SuperG_class)&B_ShowG_methods, */
/*     B_ShowD_valueD___init__, */
/*     B_ShowD_valueD___serialize__, */
/*     B_ShowD_valueD___deserialize__, */
/*     B_ShowD_valueD___str__, */
/*     B_ShowD_valueD___repr__ */
/* }; */

struct $SerializableG_class $SerializableG_methods = {"$Serializable",UNASSIGNED,NULL, $SerializableD___init__,NULL,NULL};

//struct B_valueG_class B_valueG_methods = {"B_value",UNASSIGNED,($SuperG_class)&$SerializableG_methods,B_valueD___init__,NULL,NULL, B_valueD___bool__,B_valueD___str__,B_valueD___str__};

//struct B_objectG_class B_objectG_methods = {"B_object",UNASSIGNED,($SuperG_class)&B_valueG_methods,B_objectD___init__,NULL,NULL,B_objectD___bool__,B_objectD___str__,B_objectD___str__};
