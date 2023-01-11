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

void $SerializableD___init__ ($Serializable self) {
    return;
}

B_value B_valueG_new() {
    return $NEW(B_value);
}

void B_valueD___init__ (B_value self) {
    return;
}

B_object B_objectG_new() {
    return $NEW(B_object);
}

void B_objectD___init__ (B_object self) {
    return;
}

B_str B_valueD___str__(B_value self) {
    char *s;
    asprintf(&s,"<%s object at %p>",self->$class->$GCINFO,self);
    return to$str(s);
}

B_str B_objectD___str__(B_object self) {
    char *s;
    asprintf(&s,"<%s object at %p>",self->$class->$GCINFO,self);
    return to$str(s);
}

B_bool B_valueD___bool__(B_value self) {
    return $True;
}
B_bool B_objectD___bool__(B_object self) {
    return $True;
}


struct $InitializableG_class $InitializableG_methods = {"$Initializable",UNASSIGNED,NULL,NULL};

struct $SerializableG_class $SerializableG_methods = {"$Serializable",UNASSIGNED,($SuperG_class)&$InitializableG_methods, $SerializableD___init__,NULL,NULL};

struct B_valueG_class B_valueG_methods = {"B_value",UNASSIGNED,($SuperG_class)&$SerializableG_methods,B_valueD___init__,NULL,NULL, B_valueD___bool__,B_valueD___str__,B_valueD___str__};

struct B_objectG_class B_objectG_methods = {"B_value",UNASSIGNED,($SuperG_class)&B_valueG_methods,B_objectD___init__,NULL,NULL,B_objectD___bool__,B_objectD___str__,B_objectD___str__};
