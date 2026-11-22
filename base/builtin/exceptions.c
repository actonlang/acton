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
B_BaseException B_BaseExceptionD___deserialize__ (B_BaseException, $Serial$state);
bool B_valueD___bool__ (B_value);
B_str B_valueD___str__ (B_value);
B_str B_valueD___repr__ (B_value);


// Native exception Show witnesses are stateless. KeyError retains its key's
// rendering policy in the exception, rather than in the Show witness.
// STATIC_EXCEPTION values and the Show witnesses both bypass constructors.
B_str B_BaseException_str(B_BaseException self) {
    return $FORMAT_len("%s: %s", 6, self->$class->_name(self), self->error_message);
}

B_str B_BaseException_repr(B_BaseException self) {
    return $FORMAT_len("%s(%s)", 6, self->$class->_name(self), B_str_repr(self->error_message));
}

B_str B_IndexError_str(B_IndexError self) {
    return $FORMAT_len("%s: %s, index: %lld", 19, self->$class->_name(self), self->error_message, self->index);
}

B_str B_IndexError_repr(B_IndexError self) {
    return $FORMAT_len("%s(%lld, %s)", 12, self->$class->_name(self), self->index, B_str_repr(self->error_message));
}

// The captured renderer owns the original typed key and its Show evidence.
// These calls remain lazy and never consult the key's legacy class slots.
B_str B_KeyError_str(B_KeyError self) {
    B_str text = self->key == B_None ? B_None_str :
        (B_str)self->_key_render->$class->__eval__(self->_key_render, toB_bool(false));
    return $FORMAT_len("%s: %s, key: %s", 15, self->$class->_name(self), self->error_message, text);
}

B_str B_KeyError_repr(B_KeyError self) {
    B_str text = self->key == B_None ? B_None_str :
        (B_str)self->_key_render->$class->__eval__(self->_key_render, toB_bool(true));
    return $FORMAT_len("%s(%s, %s)", 10, self->$class->_name(self), text, B_str_repr(self->error_message));
}

B_str B_FileNotFoundError_str(B_FileNotFoundError self) {
    return $FORMAT_len("%s: No such file or directory: '%s'", 35, self->$class->_name(self), self->filename);
}

B_str B_FileNotFoundError_repr(B_FileNotFoundError self) {
    return $FORMAT_len("%s(%s)", 6, self->$class->_name(self), B_str_repr(self->filename));
}

// Keep the first renderer: a later catch can have erased the concrete type.
// A shared exception can be raised by several actors, so publish the witness
// atomically. Static native exceptions already contain their fixed witness.
B_BaseException $exceptionShow(B_Show show, B_BaseException self) {
    B_value expected = NULL;
    if (atomic_compare_exchange_strong_explicit(&self->_exception_show, &expected,
            (B_value)show, memory_order_release, memory_order_relaxed))
        ACTON_WBAR(self, &self->_exception_show, show);
    return self;
}

B_str B_exception_str(B_BaseException self) {
    B_Show show = (B_Show)atomic_load_explicit(&self->_exception_show, memory_order_acquire);
    return show && show->$class != (struct B_ShowG_class *)&B_ShowD_ExceptionG_methods
        ? show->$class->__str__(show, self) : B_BaseException_str(self);
}

B_str B_exception_repr(B_BaseException self) {
    B_Show show = (B_Show)atomic_load_explicit(&self->_exception_show, memory_order_acquire);
    return show && show->$class != (struct B_ShowG_class *)&B_ShowD_ExceptionG_methods
        ? show->$class->__repr__(show, self) : B_BaseException_repr(self);
}

// Catches expose Exception, while its saved evidence can still render the
// concrete error. A plain Exception stores this policy itself: stop there
// rather than recursively redispatching to the same policy.
B_str B_ShowD_ExceptionD___str__(B_ShowD_Exception witness, B_Exception self) {
    return B_exception_str((B_BaseException)self);
}

B_str B_ShowD_ExceptionD___repr__(B_ShowD_Exception witness, B_Exception self) {
    return B_exception_repr((B_BaseException)self);
}

B_DEFINE_SHOW(BaseException)
B_DEFINE_SHOW(IndexError)
B_DEFINE_SHOW(KeyError)
B_DEFINE_SHOW(FileNotFoundError)

B_NoneType $SEQD___init__ ($SEQ self) {
    self->error_message = NULL;
    self->_exception_show = NULL;
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
    B_BaseExceptionD___deserialize__((B_BaseException)self, state);
    return self;
}
static struct $SEQ $SEQ_exception = { .$class = &$SEQG_methods };

$SEQ $SEQG_new() {
    return &$SEQ_exception;
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
    B_BaseExceptionD___deserialize__((B_BaseException)self, state);
    return self;
}
static struct $BRK $BRK_exception = { .$class = &$BRKG_methods };

$BRK $BRKG_new() {
    return &$BRK_exception;
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
    B_BaseExceptionD___deserialize__((B_BaseException)self, state);
    return self;
}
static struct $CNT $CNT_exception = { .$class = &$CNTG_methods };

$CNT $CNTG_new() {
    return &$CNT_exception;
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
    self->_exception_show = NULL;
    self->val = val;
    return B_None;
}
void $RETD___serialize__ ($RET self, $Serial$state state) {
    B_BaseExceptionD___serialize__((B_BaseException)self, state);
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
    B_BaseExceptionD___deserialize__((B_BaseException)self, state);
    ACTON_STORE(self, self->val, $step_deserialize(state));
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


$WORD $raiseValueError(B_str msg) {
    RAISE(B_ValueError, msg);
    return ($WORD)0;
}
