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

////////////////////////////////////////////////////////////////////////////////////////

bool $procD___bool__($proc self) {
  return true;
}
B_str $procD___str__($proc self) {
  return $FORMAT("<proc closure at %p>", self);
}

bool $actionD___bool__($action self) {
  return true;
}
B_str $actionD___str__($action self) {
  return $FORMAT("<action closure at %p>", self);
}

bool $mutD___bool__($mut self) {
  return true;
}
B_str $mutD___str__($mut self) {
  return $FORMAT("<mut closure at %p>", self);
}

bool $pureD___bool__($pure self) {
  return true;
}
B_str $pureD___str__($pure self) {
  return $FORMAT("<pure closure at %p>", self);
}

void $ContD___init__($Cont $this) {
    // Empty
}
bool $ContD___bool__($Cont self) {
  return true;
}
B_str $ContD___str__($Cont self) {
  return $FORMAT("<$Cont closure at %p>", self);
}
void $ContD___serialize__($Cont self, $Serial$state state) {
    // Empty
}
$Cont $ContD___deserialize__($Cont self, $Serial$state state) {
    return $DNEW($Cont,state);
}

struct $ContG_class $ContG_methods = {
    "$Cont",
    UNASSIGNED,
    NULL,
    $ContD___init__,
    $ContD___serialize__,
    $ContD___deserialize__,
    $ContD___bool__,
    $ContD___str__,
    $ContD___str__,
    NULL                /* __call__ */
};
struct $procG_class $procG_methods = {
    "$proc",
    UNASSIGNED,
    NULL,
    NULL,               /* __init__ */
    NULL,               /* __serialize__ */
    NULL,               /* __deserialize__ */
    $procD___bool__,
    $procD___str__,
    $procD___str__,
    NULL,               /* __call__ */
    NULL                /* __exec__ */
};
struct $actionG_class $actionG_methods = {
    "$action",
    UNASSIGNED,
    NULL,
    NULL,               /* __init__ */
    NULL,               /* __serialize__ */
    NULL,               /* __deserialize__ */
    $actionD___bool__,
    $actionD___str__,
    $actionD___str__,
    NULL,               /* __call__ */
    NULL,               /* __exec__ */
    NULL                /* __asyn__ */
};
struct $mutG_class $mutG_methods = {
    "$mut",
    UNASSIGNED,
    NULL,
    NULL,               /* __init__ */
    NULL,               /* __serialize__ */
    NULL,               /* __deserialize__ */
    $mutD___bool__,
    $mutD___str__,
    $mutD___str__,
    NULL,               /* __call__ */
    NULL,               /* __exec__ */
    NULL                /* __eval__ */
};
struct $pureG_class $pureG_methods = {
    "$pure",
    UNASSIGNED,
    NULL,
    NULL,               /* __init__ */
    NULL,               /* __serialize__ */
    NULL,               /* __deserialize__ */
    $pureD___bool__,
    $pureD___str__,
    $pureD___str__,
    NULL,               /* __call__ */
    NULL,               /* __exec__ */
    NULL                /* __eval__ */
};

// R1-R8 closure support ////////////////////////////////////////////////////////////////
// There is one registered descriptor per resume body. Shapes and cold support
// are compiled only in the runtime; constructors are inline in function.h.
// Objects gain no extra fields.

void $cont_class_init(struct $ContClass *cls, char *name,
                      const struct $ClosureShape *shape,
                      $R (*call)($Cont, $WORD),
                      $Cont (*restore)($Cont, $Serial$state)) {
    cls->shape = shape;
    cls->methods = (struct $ContG_class) {
        .$GCINFO = name,
        .$class_id = UNASSIGNED,
        .$superclass = ($SuperG_class)&$ContG_methods,
        .__init__ = NULL,
        .__serialize__ = $cont_serialize,
        .__deserialize__ = restore,
        .__bool__ = (bool (*)($Cont))B_valueG_methods.__bool__,
        .__str__ = (B_str (*)($Cont))B_valueG_methods.__str__,
        .__repr__ = (B_str (*)($Cont))B_valueG_methods.__repr__,
        .__call__ = call
    };
    $register(&cls->methods);
}

static struct $ClosureLayout $cont_layout(const struct $ContClass *cls) {
    return (struct $ClosureLayout) {
        .size = cls->shape->size,
        .methods = ($SerializableG_class)&cls->methods,
        .field_count = cls->shape->field_count,
        .fields = cls->shape->fields
    };
}

void $cont_serialize($Cont self, $Serial$state state) {
    const struct $ContClass *cls = (const struct $ContClass *)self->$class;
    struct $ClosureLayout layout = $cont_layout(cls);
    $closure_serialize(self, &layout, state);
}

$Cont $cont_restore(const struct $ContClass *cls, $Cont self, $Serial$state state) {
    // self may be NULL, including the allocation-only (state == NULL) phase.
    // The existing codec registers the object before restoring its captures,
    // preserving back references, cycles, and external actor references.
    struct $ClosureLayout layout = $cont_layout(cls);
    return $closure_deserialize(self, &layout, state);
}

static const struct $ClosureField $Cont_R1_fields[] = {
    {offsetof(struct $Cont_R1, ref0), sizeof($WORD), $CLOSURE_REF},
};
const struct $ClosureShape $Cont_R1_shape = {
    sizeof(struct $Cont_R1), 1, $Cont_R1_fields
};

static const struct $ClosureField $Cont_R2_fields[] = {
    {offsetof(struct $Cont_R2, ref0), sizeof($WORD), $CLOSURE_REF},
    {offsetof(struct $Cont_R2, ref1), sizeof($WORD), $CLOSURE_REF},
};
const struct $ClosureShape $Cont_R2_shape = {
    sizeof(struct $Cont_R2), 2, $Cont_R2_fields
};

static const struct $ClosureField $Cont_R3_fields[] = {
    {offsetof(struct $Cont_R3, ref0), sizeof($WORD), $CLOSURE_REF},
    {offsetof(struct $Cont_R3, ref1), sizeof($WORD), $CLOSURE_REF},
    {offsetof(struct $Cont_R3, ref2), sizeof($WORD), $CLOSURE_REF},
};
const struct $ClosureShape $Cont_R3_shape = {
    sizeof(struct $Cont_R3), 3, $Cont_R3_fields
};

static const struct $ClosureField $Cont_R4_fields[] = {
    {offsetof(struct $Cont_R4, ref0), sizeof($WORD), $CLOSURE_REF},
    {offsetof(struct $Cont_R4, ref1), sizeof($WORD), $CLOSURE_REF},
    {offsetof(struct $Cont_R4, ref2), sizeof($WORD), $CLOSURE_REF},
    {offsetof(struct $Cont_R4, ref3), sizeof($WORD), $CLOSURE_REF},
};
const struct $ClosureShape $Cont_R4_shape = {
    sizeof(struct $Cont_R4), 4, $Cont_R4_fields
};

static const struct $ClosureField $Cont_R5_fields[] = {
    {offsetof(struct $Cont_R5, ref0), sizeof($WORD), $CLOSURE_REF},
    {offsetof(struct $Cont_R5, ref1), sizeof($WORD), $CLOSURE_REF},
    {offsetof(struct $Cont_R5, ref2), sizeof($WORD), $CLOSURE_REF},
    {offsetof(struct $Cont_R5, ref3), sizeof($WORD), $CLOSURE_REF},
    {offsetof(struct $Cont_R5, ref4), sizeof($WORD), $CLOSURE_REF},
};
const struct $ClosureShape $Cont_R5_shape = {
    sizeof(struct $Cont_R5), 5, $Cont_R5_fields
};

static const struct $ClosureField $Cont_R6_fields[] = {
    {offsetof(struct $Cont_R6, ref0), sizeof($WORD), $CLOSURE_REF},
    {offsetof(struct $Cont_R6, ref1), sizeof($WORD), $CLOSURE_REF},
    {offsetof(struct $Cont_R6, ref2), sizeof($WORD), $CLOSURE_REF},
    {offsetof(struct $Cont_R6, ref3), sizeof($WORD), $CLOSURE_REF},
    {offsetof(struct $Cont_R6, ref4), sizeof($WORD), $CLOSURE_REF},
    {offsetof(struct $Cont_R6, ref5), sizeof($WORD), $CLOSURE_REF},
};
const struct $ClosureShape $Cont_R6_shape = {
    sizeof(struct $Cont_R6), 6, $Cont_R6_fields
};

static const struct $ClosureField $Cont_R7_fields[] = {
    {offsetof(struct $Cont_R7, ref0), sizeof($WORD), $CLOSURE_REF},
    {offsetof(struct $Cont_R7, ref1), sizeof($WORD), $CLOSURE_REF},
    {offsetof(struct $Cont_R7, ref2), sizeof($WORD), $CLOSURE_REF},
    {offsetof(struct $Cont_R7, ref3), sizeof($WORD), $CLOSURE_REF},
    {offsetof(struct $Cont_R7, ref4), sizeof($WORD), $CLOSURE_REF},
    {offsetof(struct $Cont_R7, ref5), sizeof($WORD), $CLOSURE_REF},
    {offsetof(struct $Cont_R7, ref6), sizeof($WORD), $CLOSURE_REF},
};
const struct $ClosureShape $Cont_R7_shape = {
    sizeof(struct $Cont_R7), 7, $Cont_R7_fields
};

static const struct $ClosureField $Cont_R8_fields[] = {
    {offsetof(struct $Cont_R8, ref0), sizeof($WORD), $CLOSURE_REF},
    {offsetof(struct $Cont_R8, ref1), sizeof($WORD), $CLOSURE_REF},
    {offsetof(struct $Cont_R8, ref2), sizeof($WORD), $CLOSURE_REF},
    {offsetof(struct $Cont_R8, ref3), sizeof($WORD), $CLOSURE_REF},
    {offsetof(struct $Cont_R8, ref4), sizeof($WORD), $CLOSURE_REF},
    {offsetof(struct $Cont_R8, ref5), sizeof($WORD), $CLOSURE_REF},
    {offsetof(struct $Cont_R8, ref6), sizeof($WORD), $CLOSURE_REF},
    {offsetof(struct $Cont_R8, ref7), sizeof($WORD), $CLOSURE_REF},
};
const struct $ClosureShape $Cont_R8_shape = {
    sizeof(struct $Cont_R8), 8, $Cont_R8_fields
};

// Proc0 R1-R4 use the same field codec as Cont, with a Proc-specific descriptor.
_Static_assert(offsetof(struct $Proc0G_class, __call__) == offsetof(struct $procG_class, __call__),
               "Proc0 call slot mismatch");
_Static_assert(offsetof(struct $Proc0G_class, __exec__) == offsetof(struct $procG_class, __exec__),
               "Proc0 exec slot mismatch");

void $proc0_class_init(struct $Proc0Class *cls, char *name,
                       const struct $ClosureShape *shape,
                       $R (*call)($proc, $Cont),
                       $proc (*restore)($proc, $Serial$state)) {
    cls->shape = shape;
    cls->methods = (struct $Proc0G_class) {
        .$GCINFO = name,
        .$class_id = UNASSIGNED,
        .$superclass = ($SuperG_class)&$procG_methods,
        .__init__ = NULL,
        .__serialize__ = $proc0_serialize,
        .__deserialize__ = restore,
        .__bool__ = (bool (*)($proc))B_valueG_methods.__bool__,
        .__str__ = (B_str (*)($proc))B_valueG_methods.__str__,
        .__repr__ = (B_str (*)($proc))B_valueG_methods.__repr__,
        .__call__ = call,
        .__exec__ = call
    };
    $register(&cls->methods);
}

static struct $ClosureLayout $proc0_layout(const struct $Proc0Class *cls) {
    return (struct $ClosureLayout) {
        .size = cls->shape->size,
        .methods = ($SerializableG_class)&cls->methods,
        .field_count = cls->shape->field_count,
        .fields = cls->shape->fields
    };
}

void $proc0_serialize($proc self, $Serial$state state) {
    const struct $Proc0Class *cls = (const struct $Proc0Class *)self->$class;
    struct $ClosureLayout layout = $proc0_layout(cls);
    $closure_serialize(self, &layout, state);
}

$proc $proc0_restore(const struct $Proc0Class *cls, $proc self, $Serial$state state) {
    struct $ClosureLayout layout = $proc0_layout(cls);
    return $closure_deserialize(self, &layout, state);
}

static const struct $ClosureField $Proc0_R1_fields[] = {
    {offsetof(struct $Proc0_R1, ref0), sizeof($WORD), $CLOSURE_REF},
};
const struct $ClosureShape $Proc0_R1_shape = {
    sizeof(struct $Proc0_R1), 1, $Proc0_R1_fields
};

static const struct $ClosureField $Proc0_R2_fields[] = {
    {offsetof(struct $Proc0_R2, ref0), sizeof($WORD), $CLOSURE_REF},
    {offsetof(struct $Proc0_R2, ref1), sizeof($WORD), $CLOSURE_REF},
};
const struct $ClosureShape $Proc0_R2_shape = {
    sizeof(struct $Proc0_R2), 2, $Proc0_R2_fields
};

static const struct $ClosureField $Proc0_R3_fields[] = {
    {offsetof(struct $Proc0_R3, ref0), sizeof($WORD), $CLOSURE_REF},
    {offsetof(struct $Proc0_R3, ref1), sizeof($WORD), $CLOSURE_REF},
    {offsetof(struct $Proc0_R3, ref2), sizeof($WORD), $CLOSURE_REF},
};
const struct $ClosureShape $Proc0_R3_shape = {
    sizeof(struct $Proc0_R3), 3, $Proc0_R3_fields
};

static const struct $ClosureField $Proc0_R4_fields[] = {
    {offsetof(struct $Proc0_R4, ref0), sizeof($WORD), $CLOSURE_REF},
    {offsetof(struct $Proc0_R4, ref1), sizeof($WORD), $CLOSURE_REF},
    {offsetof(struct $Proc0_R4, ref2), sizeof($WORD), $CLOSURE_REF},
    {offsetof(struct $Proc0_R4, ref3), sizeof($WORD), $CLOSURE_REF},
};
const struct $ClosureShape $Proc0_R4_shape = {
    sizeof(struct $Proc0_R4), 4, $Proc0_R4_fields
};

// Shared fixed R1 codec. Method-table arity affects invocation, not storage.
// __call__ returns $AWAIT's single R_WAIT step; __exec__ returns the message.
static const struct $ClosureField $Action_R1_fields[] = {
    {offsetof(struct $Action_R1, ref0), sizeof($WORD), $CLOSURE_REF},
};
static struct $ClosureLayout $action_r1_layout(const void *methods) {
    return (struct $ClosureLayout) {
        .size = sizeof(struct $Action_R1),
        .methods = ($SerializableG_class)methods,
        .field_count = 1,
        .fields = $Action_R1_fields
    };
}
void $action_r1_serialize($action self, $Serial$state state) {
    struct $ClosureLayout layout = $action_r1_layout(self->$class);
    $closure_serialize(self, &layout, state);
}
$action $action_r1_restore(const void *methods, $action self, $Serial$state state) {
    struct $ClosureLayout layout = $action_r1_layout(methods);
    return $closure_deserialize(self, &layout, state);
}

_Static_assert(offsetof(struct $Action0G_class, __asyn__) == offsetof(struct $actionG_class, __asyn__),
               "Action0 async slot mismatch");
static $R $action0_call($action self, $Cont cont) {
    return $R_WAIT(cont, ((struct $Action0G_class *)self->$class)->__asyn__(self));
}
static $R $action0_exec($action self, $Cont cont) {
    return $R_CONT(cont, ((struct $Action0G_class *)self->$class)->__asyn__(self));
}
void $action0_class_init(struct $Action0G_class *methods, char *name,
                         B_Msg (*asyn)($action),
                         $action (*restore)($action, $Serial$state)) {
    *methods = (struct $Action0G_class) {
        .$GCINFO = name,
        .$class_id = UNASSIGNED,
        .$superclass = ($SuperG_class)&$actionG_methods,
        .__init__ = NULL,
        .__serialize__ = $action_r1_serialize,
        .__deserialize__ = restore,
        .__bool__ = (bool (*)($action))B_valueG_methods.__bool__,
        .__str__ = (B_str (*)($action))B_valueG_methods.__str__,
        .__repr__ = (B_str (*)($action))B_valueG_methods.__repr__,
        .__call__ = $action0_call,
        .__exec__ = $action0_exec,
        .__asyn__ = asyn
    };
    $register(methods);
}

_Static_assert(offsetof(struct $Action1G_class, __asyn__) == offsetof(struct $actionG_class, __asyn__),
               "Action1 async slot mismatch");
static $R $action1_call($action self, $Cont cont, $WORD arg1) {
    return $R_WAIT(cont, ((struct $Action1G_class *)self->$class)->__asyn__(self, arg1));
}
static $R $action1_exec($action self, $Cont cont, $WORD arg1) {
    return $R_CONT(cont, ((struct $Action1G_class *)self->$class)->__asyn__(self, arg1));
}
void $action1_class_init(struct $Action1G_class *methods, char *name,
                         B_Msg (*asyn)($action, $WORD),
                         $action (*restore)($action, $Serial$state)) {
    *methods = (struct $Action1G_class) {
        .$GCINFO = name,
        .$class_id = UNASSIGNED,
        .$superclass = ($SuperG_class)&$actionG_methods,
        .__init__ = NULL,
        .__serialize__ = $action_r1_serialize,
        .__deserialize__ = restore,
        .__bool__ = (bool (*)($action))B_valueG_methods.__bool__,
        .__str__ = (B_str (*)($action))B_valueG_methods.__str__,
        .__repr__ = (B_str (*)($action))B_valueG_methods.__repr__,
        .__call__ = $action1_call,
        .__exec__ = $action1_exec,
        .__asyn__ = asyn
    };
    $register(methods);
}

_Static_assert(offsetof(struct $Action2G_class, __asyn__) == offsetof(struct $actionG_class, __asyn__),
               "Action2 async slot mismatch");
static $R $action2_call($action self, $Cont cont, $WORD arg1, $WORD arg2) {
    return $R_WAIT(cont, ((struct $Action2G_class *)self->$class)->__asyn__(self, arg1, arg2));
}
static $R $action2_exec($action self, $Cont cont, $WORD arg1, $WORD arg2) {
    return $R_CONT(cont, ((struct $Action2G_class *)self->$class)->__asyn__(self, arg1, arg2));
}
void $action2_class_init(struct $Action2G_class *methods, char *name,
                         B_Msg (*asyn)($action, $WORD, $WORD),
                         $action (*restore)($action, $Serial$state)) {
    *methods = (struct $Action2G_class) {
        .$GCINFO = name,
        .$class_id = UNASSIGNED,
        .$superclass = ($SuperG_class)&$actionG_methods,
        .__init__ = NULL,
        .__serialize__ = $action_r1_serialize,
        .__deserialize__ = restore,
        .__bool__ = (bool (*)($action))B_valueG_methods.__bool__,
        .__str__ = (B_str (*)($action))B_valueG_methods.__str__,
        .__repr__ = (B_str (*)($action))B_valueG_methods.__repr__,
        .__call__ = $action2_call,
        .__exec__ = $action2_exec,
        .__asyn__ = asyn
    };
    $register(methods);
}

_Static_assert(offsetof(struct $Action3G_class, __asyn__) == offsetof(struct $actionG_class, __asyn__),
               "Action3 async slot mismatch");
static $R $action3_call($action self, $Cont cont, $WORD arg1, $WORD arg2, $WORD arg3) {
    return $R_WAIT(cont, ((struct $Action3G_class *)self->$class)->__asyn__(self, arg1, arg2, arg3));
}
static $R $action3_exec($action self, $Cont cont, $WORD arg1, $WORD arg2, $WORD arg3) {
    return $R_CONT(cont, ((struct $Action3G_class *)self->$class)->__asyn__(self, arg1, arg2, arg3));
}
void $action3_class_init(struct $Action3G_class *methods, char *name,
                         B_Msg (*asyn)($action, $WORD, $WORD, $WORD),
                         $action (*restore)($action, $Serial$state)) {
    *methods = (struct $Action3G_class) {
        .$GCINFO = name,
        .$class_id = UNASSIGNED,
        .$superclass = ($SuperG_class)&$actionG_methods,
        .__init__ = NULL,
        .__serialize__ = $action_r1_serialize,
        .__deserialize__ = restore,
        .__bool__ = (bool (*)($action))B_valueG_methods.__bool__,
        .__str__ = (B_str (*)($action))B_valueG_methods.__str__,
        .__repr__ = (B_str (*)($action))B_valueG_methods.__repr__,
        .__call__ = $action3_call,
        .__exec__ = $action3_exec,
        .__asyn__ = asyn
    };
    $register(methods);
}
