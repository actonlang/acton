#pragma once

struct $ContG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($Cont);
    void (*__serialize__)($Cont, $Serial$state);
    $Cont (*__deserialize__)($Cont, $Serial$state);
    bool (*__bool__)($Cont);
    B_str (*__str__)($Cont);
    B_str (*__repr__)($Cont);
    $R (*__call__)($Cont, $WORD);
};
struct $Cont {
    struct $ContG_class *$class;
};
extern struct $ContG_class $ContG_methods;

void $ContD___init__($Cont);
bool $ContD___bool__($Cont);
B_str $ContD___str__($Cont);
void $ContD___serialize__($Cont, $Serial$state);
$Cont $ContD___deserialize__($Cont, $Serial$state);


struct $procG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($proc);
    void (*__serialize__)($proc, $Serial$state);
    $proc (*__deserialize__)($proc, $Serial$state);
    bool (*__bool__)($proc);
    B_str (*__str__)($proc);
    B_str (*__repr__)($proc);
    $R (*__call__)($proc, $Cont, $WORD);
    $R (*__exec__)($proc, $Cont, $WORD);
};
struct $proc {
    struct $procG_class *$class;
};
extern struct $procG_class $procG_methods;


struct $actionG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($action);
    void (*__serialize__)($action, $Serial$state);
    $action (*__deserialize__)($action, $Serial$state);
    bool (*__bool__)($action);
    B_str (*__str__)($action);
    B_str (*__repr__)($action);
    $R (*__call__)($action, $Cont, $WORD);
    $R (*__exec__)($action, $Cont, $WORD);
    B_Msg (*__asyn__)($action, $WORD);
};
struct $action {
    struct $actionG_class *$class;
};
extern struct $actionG_class $actionG_methods;


struct $mutG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($mut);
    void (*__serialize__)($mut, $Serial$state);
    $mut (*__deserialize__)($mut, $Serial$state);
    bool (*__bool__)($mut);
    B_str (*__str__)($mut);
    B_str (*__repr__)($mut);
    $R (*__call__)($mut, $Cont, $WORD);
    $R (*__exec__)($mut, $Cont, $WORD);
    $WORD (*__eval__)($mut, $WORD);
};
struct $mut {
    struct $mutG_class *$class;
};
extern struct $mutG_class $mutG_methods;


struct $pureG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($pure);
    void (*__serialize__)($pure, $Serial$state);
    $pure (*__deserialize__)($pure, $Serial$state);
    bool (*__bool__)($pure);
    B_str (*__str__)($pure);
    B_str (*__repr__)($pure);
    $R (*__call__)($pure, $Cont, $WORD);
    $R (*__exec__)($pure, $Cont, $WORD);
    $WORD (*__eval__)($pure, $WORD);
};
struct $pure {
    struct $pureG_class *$class;
};
extern struct $pureG_class $pureG_methods;


//////////////////////////////////////////////////////////////////////////////////
struct $action2;
typedef struct $action2 *$action2;
struct $action2G_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($action2);
    void (*__serialize__)($action2, $Serial$state);
    $action2 (*__deserialize__)($action2, $Serial$state);
    bool (*__bool__)($action2);
    B_str (*__str__)($action2);
    B_str (*__repr__)($action2);
    $R (*__call__)($action2, $Cont, $WORD, $WORD);
    $R (*__exec__)($action2, $Cont, $WORD, $WORD);
    B_Msg (*__asyn__)($action2, $WORD, $WORD);
};
struct $action2 {
    struct $action2G_class *$class;
};

struct $action3;
typedef struct $action3 *$action3;
struct $action3G_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($action3);
    void (*__serialize__)($action3, $Serial$state);
    $action3 (*__deserialize__)($action3, $Serial$state);
    bool (*__bool__)($action3);
    B_str (*__str__)($action3);
    B_str (*__repr__)($action3);
    $R (*__call__)($action3, $Cont, $WORD, $WORD, $WORD);
    $R (*__exec__)($action3, $WORD, $WORD, $WORD);
    B_Msg (*__asyn__)($action3, $WORD, $WORD, $WORD);
};
struct $action3 {
    struct $action3G_class *$class;
};

// Shared compiler-generated continuation payloads. The normal method table is
// the first member of each descriptor, so $class retains its existing meaning.
// A shape describes storage only; __call__ identifies the module's resume body.
struct $ClosureShape {
    size_t size;
    size_t field_count;
    const struct $ClosureField *fields;
};

struct $ContClass {
    struct $ContG_class methods;
    const struct $ClosureShape *shape;
};

void $cont_class_init(struct $ContClass *cls, char *name,
                      const struct $ClosureShape *shape,
                      $R (*call)($Cont, $WORD),
                      $Cont (*restore)($Cont, $Serial$state));
void $cont_serialize($Cont self, $Serial$state state);
$Cont $cont_restore(const struct $ContClass *cls, $Cont self, $Serial$state state);

// Keep these small allocation-and-store bodies visible to the C optimizer.
// Shapes, serialization and class initialization remain in function.c.
struct $Cont_R1 {
    struct $ContG_class *$class;
    $WORD ref0;
};
extern const struct $ClosureShape $Cont_R1_shape;
static inline $Cont $Cont_R1_new(struct $ContClass *cls, $WORD ref0) {
    struct $Cont_R1 *self = acton_malloc(sizeof(*self));
    self->$class = &cls->methods;
    self->ref0 = ref0;
    return ($Cont)self;
}

struct $Cont_R2 {
    struct $ContG_class *$class;
    $WORD ref0;
    $WORD ref1;
};
extern const struct $ClosureShape $Cont_R2_shape;
static inline $Cont $Cont_R2_new(struct $ContClass *cls, $WORD ref0, $WORD ref1) {
    struct $Cont_R2 *self = acton_malloc(sizeof(*self));
    self->$class = &cls->methods;
    self->ref0 = ref0;
    self->ref1 = ref1;
    return ($Cont)self;
}

struct $Cont_R3 {
    struct $ContG_class *$class;
    $WORD ref0;
    $WORD ref1;
    $WORD ref2;
};
extern const struct $ClosureShape $Cont_R3_shape;
static inline $Cont $Cont_R3_new(struct $ContClass *cls, $WORD ref0, $WORD ref1, $WORD ref2) {
    struct $Cont_R3 *self = acton_malloc(sizeof(*self));
    self->$class = &cls->methods;
    self->ref0 = ref0;
    self->ref1 = ref1;
    self->ref2 = ref2;
    return ($Cont)self;
}

struct $Cont_R4 {
    struct $ContG_class *$class;
    $WORD ref0;
    $WORD ref1;
    $WORD ref2;
    $WORD ref3;
};
extern const struct $ClosureShape $Cont_R4_shape;
static inline $Cont $Cont_R4_new(struct $ContClass *cls, $WORD ref0, $WORD ref1, $WORD ref2, $WORD ref3) {
    struct $Cont_R4 *self = acton_malloc(sizeof(*self));
    self->$class = &cls->methods;
    self->ref0 = ref0;
    self->ref1 = ref1;
    self->ref2 = ref2;
    self->ref3 = ref3;
    return ($Cont)self;
}

struct $Cont_R5 {
    struct $ContG_class *$class;
    $WORD ref0;
    $WORD ref1;
    $WORD ref2;
    $WORD ref3;
    $WORD ref4;
};
extern const struct $ClosureShape $Cont_R5_shape;
static inline $Cont $Cont_R5_new(struct $ContClass *cls,
                                $WORD ref0, $WORD ref1, $WORD ref2, $WORD ref3,
                                $WORD ref4) {
    struct $Cont_R5 *self = acton_malloc(sizeof(*self));
    self->$class = &cls->methods;
    self->ref0 = ref0;
    self->ref1 = ref1;
    self->ref2 = ref2;
    self->ref3 = ref3;
    self->ref4 = ref4;
    return ($Cont)self;
}

struct $Cont_R6 {
    struct $ContG_class *$class;
    $WORD ref0;
    $WORD ref1;
    $WORD ref2;
    $WORD ref3;
    $WORD ref4;
    $WORD ref5;
};
extern const struct $ClosureShape $Cont_R6_shape;
static inline $Cont $Cont_R6_new(struct $ContClass *cls,
                                $WORD ref0, $WORD ref1, $WORD ref2, $WORD ref3,
                                $WORD ref4, $WORD ref5) {
    struct $Cont_R6 *self = acton_malloc(sizeof(*self));
    self->$class = &cls->methods;
    self->ref0 = ref0;
    self->ref1 = ref1;
    self->ref2 = ref2;
    self->ref3 = ref3;
    self->ref4 = ref4;
    self->ref5 = ref5;
    return ($Cont)self;
}

struct $Cont_R7 {
    struct $ContG_class *$class;
    $WORD ref0;
    $WORD ref1;
    $WORD ref2;
    $WORD ref3;
    $WORD ref4;
    $WORD ref5;
    $WORD ref6;
};
extern const struct $ClosureShape $Cont_R7_shape;
static inline $Cont $Cont_R7_new(struct $ContClass *cls,
                                $WORD ref0, $WORD ref1, $WORD ref2, $WORD ref3,
                                $WORD ref4, $WORD ref5, $WORD ref6) {
    struct $Cont_R7 *self = acton_malloc(sizeof(*self));
    self->$class = &cls->methods;
    self->ref0 = ref0;
    self->ref1 = ref1;
    self->ref2 = ref2;
    self->ref3 = ref3;
    self->ref4 = ref4;
    self->ref5 = ref5;
    self->ref6 = ref6;
    return ($Cont)self;
}

struct $Cont_R8 {
    struct $ContG_class *$class;
    $WORD ref0;
    $WORD ref1;
    $WORD ref2;
    $WORD ref3;
    $WORD ref4;
    $WORD ref5;
    $WORD ref6;
    $WORD ref7;
};
extern const struct $ClosureShape $Cont_R8_shape;
static inline $Cont $Cont_R8_new(struct $ContClass *cls,
                                $WORD ref0, $WORD ref1, $WORD ref2, $WORD ref3,
                                $WORD ref4, $WORD ref5, $WORD ref6, $WORD ref7) {
    struct $Cont_R8 *self = acton_malloc(sizeof(*self));
    self->$class = &cls->methods;
    self->ref0 = ref0;
    self->ref1 = ref1;
    self->ref2 = ref2;
    self->ref3 = ref3;
    self->ref4 = ref4;
    self->ref5 = ref5;
    self->ref6 = ref6;
    self->ref7 = ref7;
    return ($Cont)self;
}

// Zero-argument Procs still receive the continuation supplied by their caller.
// Their table has the ordinary Proc prefix and both invocation slots. Keep a
// separate descriptor: the shape follows a different table size than in Cont.
struct $Proc0G_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($proc);
    void (*__serialize__)($proc, $Serial$state);
    $proc (*__deserialize__)($proc, $Serial$state);
    bool (*__bool__)($proc);
    B_str (*__str__)($proc);
    B_str (*__repr__)($proc);
    $R (*__call__)($proc, $Cont);
    $R (*__exec__)($proc, $Cont);
};
struct $Proc0Class {
    struct $Proc0G_class methods;
    const struct $ClosureShape *shape;
};

void $proc0_class_init(struct $Proc0Class *cls, char *name,
                       const struct $ClosureShape *shape,
                       $R (*call)($proc, $Cont),
                       $proc (*restore)($proc, $Serial$state));
void $proc0_serialize($proc self, $Serial$state state);
$proc $proc0_restore(const struct $Proc0Class *cls, $proc self, $Serial$state state);

struct $Proc0_R1 {
    struct $Proc0G_class *$class;
    $WORD ref0;
};
extern const struct $ClosureShape $Proc0_R1_shape;
static inline $proc $Proc0_R1_new(struct $Proc0Class *cls, $WORD ref0) {
    struct $Proc0_R1 *self = acton_malloc(sizeof(*self));
    self->$class = &cls->methods;
    self->ref0 = ref0;
    return ($proc)self;
}

struct $Proc0_R2 {
    struct $Proc0G_class *$class;
    $WORD ref0;
    $WORD ref1;
};
extern const struct $ClosureShape $Proc0_R2_shape;
static inline $proc $Proc0_R2_new(struct $Proc0Class *cls, $WORD ref0, $WORD ref1) {
    struct $Proc0_R2 *self = acton_malloc(sizeof(*self));
    self->$class = &cls->methods;
    self->ref0 = ref0;
    self->ref1 = ref1;
    return ($proc)self;
}

struct $Proc0_R3 {
    struct $Proc0G_class *$class;
    $WORD ref0;
    $WORD ref1;
    $WORD ref2;
};
extern const struct $ClosureShape $Proc0_R3_shape;
static inline $proc $Proc0_R3_new(struct $Proc0Class *cls, $WORD ref0, $WORD ref1, $WORD ref2) {
    struct $Proc0_R3 *self = acton_malloc(sizeof(*self));
    self->$class = &cls->methods;
    self->ref0 = ref0;
    self->ref1 = ref1;
    self->ref2 = ref2;
    return ($proc)self;
}

struct $Proc0_R4 {
    struct $Proc0G_class *$class;
    $WORD ref0;
    $WORD ref1;
    $WORD ref2;
    $WORD ref3;
};
extern const struct $ClosureShape $Proc0_R4_shape;
static inline $proc $Proc0_R4_new(struct $Proc0Class *cls, $WORD ref0, $WORD ref1, $WORD ref2, $WORD ref3) {
    struct $Proc0_R4 *self = acton_malloc(sizeof(*self));
    self->$class = &cls->methods;
    self->ref0 = ref0;
    self->ref1 = ref1;
    self->ref2 = ref2;
    self->ref3 = ref3;
    return ($proc)self;
}

// One reference capture, independent of invocation arity. These objects need no
// shape pointer: all Action tables use the same fixed field codec.
struct $Action_R1 {
    struct $actionG_class *$class;
    $WORD ref0;
};
static inline $action $Action_R1_new(void *methods, $WORD ref0) {
    struct $Action_R1 *self = acton_malloc(sizeof(*self));
    self->$class = methods;
    self->ref0 = ref0;
    return ($action)self;
}
void $action_r1_serialize($action self, $Serial$state state);
$action $action_r1_restore(const void *methods, $action self, $Serial$state state);

struct $Action0G_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($action);
    void (*__serialize__)($action, $Serial$state);
    $action (*__deserialize__)($action, $Serial$state);
    bool (*__bool__)($action);
    B_str (*__str__)($action);
    B_str (*__repr__)($action);
    $R (*__call__)($action, $Cont);
    $R (*__exec__)($action, $Cont);
    B_Msg (*__asyn__)($action);
};
void $action0_class_init(struct $Action0G_class *methods, char *name,
                         B_Msg (*asyn)($action),
                         $action (*restore)($action, $Serial$state));

struct $Action1G_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($action);
    void (*__serialize__)($action, $Serial$state);
    $action (*__deserialize__)($action, $Serial$state);
    bool (*__bool__)($action);
    B_str (*__str__)($action);
    B_str (*__repr__)($action);
    $R (*__call__)($action, $Cont, $WORD);
    $R (*__exec__)($action, $Cont, $WORD);
    B_Msg (*__asyn__)($action, $WORD);
};
void $action1_class_init(struct $Action1G_class *methods, char *name,
                         B_Msg (*asyn)($action, $WORD),
                         $action (*restore)($action, $Serial$state));

struct $Action2G_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($action);
    void (*__serialize__)($action, $Serial$state);
    $action (*__deserialize__)($action, $Serial$state);
    bool (*__bool__)($action);
    B_str (*__str__)($action);
    B_str (*__repr__)($action);
    $R (*__call__)($action, $Cont, $WORD, $WORD);
    $R (*__exec__)($action, $Cont, $WORD, $WORD);
    B_Msg (*__asyn__)($action, $WORD, $WORD);
};
void $action2_class_init(struct $Action2G_class *methods, char *name,
                         B_Msg (*asyn)($action, $WORD, $WORD),
                         $action (*restore)($action, $Serial$state));

struct $Action3G_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($action);
    void (*__serialize__)($action, $Serial$state);
    $action (*__deserialize__)($action, $Serial$state);
    bool (*__bool__)($action);
    B_str (*__str__)($action);
    B_str (*__repr__)($action);
    $R (*__call__)($action, $Cont, $WORD, $WORD, $WORD);
    $R (*__exec__)($action, $Cont, $WORD, $WORD, $WORD);
    B_Msg (*__asyn__)($action, $WORD, $WORD, $WORD);
};
void $action3_class_init(struct $Action3G_class *methods, char *name,
                         B_Msg (*asyn)($action, $WORD, $WORD, $WORD),
                         $action (*restore)($action, $Serial$state));
