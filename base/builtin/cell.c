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

B_NoneType $CellD___init__($Cell self, $WORD cell) {
    self->cell = cell;
    return B_None;
}

bool $CellD___bool__($Cell self) {
    B_value it = (B_value)self->cell;
    return it->$class->__bool__(it);
}

B_str $CellD___str__($Cell self) {
    B_value it = (B_value)self->cell;
    return it->$class->__str__(it);
}

B_str $CellD___repr__($Cell self) {
    B_value it = (B_value)self->cell;
    return it->$class->__repr__(it);
}

void $CellD___serialize__ ($Cell self, $Serial$state state) {
    $step_serialize(self->cell, state);
}

$Cell $CellD___deserialize__ ($Cell self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct $Cell));
            self->$class = &$CellG_methods;
            return self;
        }
        self = $DNEW($Cell, state);
    }
    self->cell = $step_deserialize(state);
    return self;
}

$Cell $CellG_new($WORD G_1) {
    $Cell $tmp = acton_malloc(sizeof(struct $Cell));
    $tmp->$class = &$CellG_methods;
    $CellG_methods.__init__($tmp, G_1);
    return $tmp;
}

struct $CellG_class $CellG_methods = {
    .$GCINFO            = "$Cell",
    .$superclass        = ($SuperG_class)&B_ExceptionG_methods,
    .__init__           = $CellD___init__,
    .__bool__           = $CellD___bool__,
    .__str__            = $CellD___str__,
    .__repr__           = $CellD___repr__,
    .__serialize__      = $CellD___serialize__,
    .__deserialize__    = $CellD___deserialize__
};
