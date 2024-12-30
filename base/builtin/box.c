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

B_NoneType $BoxD___init__($Box self, $WORD val) {
    self->val = val;
    return B_None;
}

B_bool $BoxD___bool__($Box self) {
    B_value it = (B_value)self->val;
    return it->$class->__bool__(it);
}

B_str $BoxD___str__($Box self) {
    B_value it = (B_value)self->val;
    return it->$class->__str__(it);
}

B_str $BoxD___repr__($Box self) {
    B_value it = (B_value)self->val;
    return it->$class->__repr__(it);
}

void $BoxD___serialize__ ($Box self, $Serial$state state) {
    $step_serialize(self->val, state);
}

$Box $BoxD___deserialize__ ($Box self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct $Box));
            self->$class = &$BoxG_methods;
            return self;
        }
        self = $DNEW($Box, state);
    }
    self->val = $step_deserialize(state);
    return self;
}

$Box $BoxG_new($WORD G_1) {
    $Box $tmp = acton_malloc(sizeof(struct $Box));
    $tmp->$class = &$BoxG_methods;
    $BoxG_methods.__init__($tmp, G_1);
    return $tmp;
}

struct $BoxG_class $BoxG_methods = {
    .$GCINFO            = "$Box",
    .$superclass        = ($SuperG_class)&B_ExceptionG_methods,
    .__init__           = $BoxD___init__,
    .__bool__           = $BoxD___bool__,
    .__str__            = $BoxD___str__,
    .__repr__           = $BoxD___repr__,
    .__serialize__      = $BoxD___serialize__,
    .__deserialize__    = $BoxD___deserialize__
};
