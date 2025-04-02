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

void B_BaseExceptionD___serialize__ (B_BaseException, $Serial$state);
B_bool B_valueD___bool__ (B_value);
B_str B_valueD___str__ (B_value);
B_str B_valueD___repr__ (B_value);


B_NoneType $SEQD___init__ ($SEQ self) {
    self->error_message = NULL;
    return B_None;
}
$SEQ $SEQD___deserialize__ ($SEQ self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct $SEQ));
            self->$class = &$SEQG_methods;
            return self;
        }
        self = $DNEW($SEQ, state);
    }
    self->error_message = $step_deserialize(state);
    return self;
}
$SEQ $SEQG_new() {
    $SEQ $tmp = acton_malloc(sizeof(struct $SEQ));
    $tmp->$class = &$SEQG_methods;
    $SEQG_methods.__init__($tmp);
    return $tmp;
}
struct $SEQG_class $SEQG_methods = {
    .$GCINFO            = "$SEQ",
    .$superclass        = ($SuperG_class)&B_ExceptionG_methods,
    .__init__           = $SEQD___init__,
    .__serialize__      = (void (*) ($SEQ, $Serial$state))B_BaseExceptionD___serialize__,
    .__deserialize__    = $SEQD___deserialize__
};


$BRK $BRKD___deserialize__ ($BRK self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct $BRK));
            self->$class = &$BRKG_methods;
            return self;
        }
        self = $DNEW($BRK, state);
    }
    self->error_message = NULL;
    return self;
}
$BRK $BRKG_new() {
    $BRK $tmp = acton_malloc(sizeof(struct $BRK));
    $tmp->$class = &$BRKG_methods;
    $BRKG_methods.__init__($tmp);
    return $tmp;
}
struct $BRKG_class $BRKG_methods = {
    .$GCINFO            = "$BRK",
    .$superclass        = ($SuperG_class)&B_ExceptionG_methods,
    .__init__           = (B_NoneType (*) ($BRK))$SEQD___init__,
    .__serialize__      = (void (*) ($BRK, $Serial$state))B_BaseExceptionD___serialize__,
    .__deserialize__    = $BRKD___deserialize__
};


$CNT $CNTD___deserialize__ ($CNT self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct $CNT));
            self->$class = &$CNTG_methods;
            return self;
        }
        self = $DNEW($CNT, state);
    }
    self->error_message = $step_deserialize(state);
    return self;
}
$CNT $CNTG_new() {
    $CNT $tmp = acton_malloc(sizeof(struct $CNT));
    $tmp->$class = &$CNTG_methods;
    $CNTG_methods.__init__($tmp);
    return $tmp;
}
struct $CNTG_class $CNTG_methods = {
    .$GCINFO            = "$CNT",
    .$superclass        = ($SuperG_class)&B_ExceptionG_methods,
    .__init__           = (B_NoneType (*) ($CNT))$SEQD___init__,
    .__serialize__      = (void (*) ($CNT, $Serial$state))B_BaseExceptionD___serialize__,
    .__deserialize__    = $CNTD___deserialize__
};


B_NoneType $RETD___init__ ($RET self, B_value val) {
    self->error_message = NULL;
    self->val = val;
    return B_None;
}
void $RETD___serialize__ ($RET self, $Serial$state state) {
    $step_serialize(self->val, state);
}
$RET $RETD___deserialize__ ($RET self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct $RET));
            self->$class = &$RETG_methods;
            return self;
        }
        self = $DNEW($RET, state);
    }
    self->error_message = $step_deserialize(state);
    self->val = $step_deserialize(state);
    return self;
}
$RET $RETG_new(B_value G_1) {
    $RET $tmp = acton_malloc(sizeof(struct $RET));
    $tmp->$class = &$RETG_methods;
    $RETG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct $RETG_class $RETG_methods = {
    .$GCINFO            = "$RET",
    .$superclass        = ($SuperG_class)&B_ExceptionG_methods,
    .__init__           = $RETD___init__,
    .__serialize__      = $RETD___serialize__,
    .__deserialize__    = $RETD___deserialize__
};
