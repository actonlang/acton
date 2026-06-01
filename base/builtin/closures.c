/* Shared runtime classes for common lambda-lifted closure shapes.
 *
 * CodeGen aliases selected generated closure classes to these structs and emits
 * only a small typed trampoline plus a constructor wrapper.  Serialization is
 * deliberately unsupported here until generic closures can record enough
 * generated-code identity to deserialize safely.
 */

B_bool $procD___bool__($proc self);
B_str $procD___str__($proc self);
B_bool $mutD___bool__($mut self);
B_str $mutD___str__($mut self);

#define B_CLOSURE_ARGS_0
#define B_CLOSURE_ARGS_1 , $WORD p0
#define B_CLOSURE_ARGS_2 , $WORD p0, $WORD p1
#define B_CLOSURE_ARGS_3 , $WORD p0, $WORD p1, $WORD p2
#define B_CLOSURE_ARGS_4 , $WORD p0, $WORD p1, $WORD p2, $WORD p3

#define B_CLOSURE_CALL_ARGS_0
#define B_CLOSURE_CALL_ARGS_1 , p0
#define B_CLOSURE_CALL_ARGS_2 , p0, p1
#define B_CLOSURE_CALL_ARGS_3 , p0, p1, p2
#define B_CLOSURE_CALL_ARGS_4 , p0, p1, p2, p3

#define B_CLOSURE_INIT_FIELDS_0
#define B_CLOSURE_INIT_FIELDS_1 self->p0 = p0;
#define B_CLOSURE_INIT_FIELDS_2 self->p0 = p0; self->p1 = p1;
#define B_CLOSURE_INIT_FIELDS_3 self->p0 = p0; self->p1 = p1; self->p2 = p2;
#define B_CLOSURE_INIT_FIELDS_4 self->p0 = p0; self->p1 = p1; self->p2 = p2; self->p3 = p3;

static void B_Closure_serialization_unsupported(char *name) {
    fprintf(stderr, "Serialization of generic closure %s is not implemented\n", name);
    abort();
}

#define B_DEFINE_CLOSURE_CONT(N) \
struct B_ClosureCont##N##G_class B_ClosureCont##N##G_methods; \
B_NoneType B_ClosureCont##N##D___init__(B_ClosureCont##N self, B_ClosureCont##N##Fn f B_CLOSURE_ARGS_##N) { \
    self->f = f; \
    B_CLOSURE_INIT_FIELDS_##N \
    return B_None; \
} \
$R B_ClosureCont##N##D___call__(B_ClosureCont##N self, $WORD arg) { \
    return self->f(self, arg); \
} \
void B_ClosureCont##N##D___serialize__(B_ClosureCont##N self, $Serial$state state) { \
    B_Closure_serialization_unsupported("B_ClosureCont" #N); \
} \
B_ClosureCont##N B_ClosureCont##N##D___deserialize__(B_ClosureCont##N self, $Serial$state state) { \
    B_Closure_serialization_unsupported("B_ClosureCont" #N); \
    return NULL; \
} \
B_ClosureCont##N B_ClosureCont##N##G_new(B_ClosureCont##N##Fn f B_CLOSURE_ARGS_##N) { \
    B_ClosureCont##N tmp = acton_malloc(sizeof(struct B_ClosureCont##N)); \
    tmp->$class = &B_ClosureCont##N##G_methods; \
    B_ClosureCont##N##G_methods.__init__(tmp, f B_CLOSURE_CALL_ARGS_##N); \
    return tmp; \
}

#define B_DEFINE_CLOSURE_PROC(N) \
struct B_ClosureProc##N##G_class B_ClosureProc##N##G_methods; \
B_NoneType B_ClosureProc##N##D___init__(B_ClosureProc##N self, B_ClosureProc##N##Fn f B_CLOSURE_ARGS_##N) { \
    self->f = f; \
    B_CLOSURE_INIT_FIELDS_##N \
    return B_None; \
} \
$R B_ClosureProc##N##D___call__(B_ClosureProc##N self, $Cont cont) { \
    return self->f(self, cont); \
} \
$R B_ClosureProc##N##D___exec__(B_ClosureProc##N self, $Cont cont) { \
    return self->f(self, cont); \
} \
void B_ClosureProc##N##D___serialize__(B_ClosureProc##N self, $Serial$state state) { \
    B_Closure_serialization_unsupported("B_ClosureProc" #N); \
} \
B_ClosureProc##N B_ClosureProc##N##D___deserialize__(B_ClosureProc##N self, $Serial$state state) { \
    B_Closure_serialization_unsupported("B_ClosureProc" #N); \
    return NULL; \
} \
B_ClosureProc##N B_ClosureProc##N##G_new(B_ClosureProc##N##Fn f B_CLOSURE_ARGS_##N) { \
    B_ClosureProc##N tmp = acton_malloc(sizeof(struct B_ClosureProc##N)); \
    tmp->$class = &B_ClosureProc##N##G_methods; \
    B_ClosureProc##N##G_methods.__init__(tmp, f B_CLOSURE_CALL_ARGS_##N); \
    return tmp; \
}

B_DEFINE_CLOSURE_CONT(0)
B_DEFINE_CLOSURE_CONT(1)
B_DEFINE_CLOSURE_CONT(2)
B_DEFINE_CLOSURE_CONT(3)
B_DEFINE_CLOSURE_CONT(4)

B_DEFINE_CLOSURE_PROC(0)
B_DEFINE_CLOSURE_PROC(1)
B_DEFINE_CLOSURE_PROC(2)
B_DEFINE_CLOSURE_PROC(3)
B_DEFINE_CLOSURE_PROC(4)

struct B_ClosureMut0G_class B_ClosureMut0G_methods;

B_NoneType B_ClosureMut0D___init__(B_ClosureMut0 self, B_ClosureMut0Fn f) {
    self->f = f;
    return B_None;
}
$WORD B_ClosureMut0D___eval__(B_ClosureMut0 self) {
    return self->f(self);
}
$R B_ClosureMut0D___call__(B_ClosureMut0 self, $Cont cont) {
    return $R_CONT(cont, self->f(self));
}
$R B_ClosureMut0D___exec__(B_ClosureMut0 self, $Cont cont) {
    return $R_CONT(cont, self->f(self));
}
void B_ClosureMut0D___serialize__(B_ClosureMut0 self, $Serial$state state) {
    B_Closure_serialization_unsupported("B_ClosureMut0");
}
B_ClosureMut0 B_ClosureMut0D___deserialize__(B_ClosureMut0 self, $Serial$state state) {
    B_Closure_serialization_unsupported("B_ClosureMut0");
    return NULL;
}
B_ClosureMut0 B_ClosureMut0G_new(B_ClosureMut0Fn f) {
    B_ClosureMut0 tmp = acton_malloc(sizeof(struct B_ClosureMut0));
    tmp->$class = &B_ClosureMut0G_methods;
    B_ClosureMut0G_methods.__init__(tmp, f);
    return tmp;
}

#define B_INIT_CLOSURE_CONT(N) \
    B_ClosureCont##N##G_methods.$GCINFO = "B_ClosureCont" #N; \
    B_ClosureCont##N##G_methods.$superclass = ($SuperG_class)&$ContG_methods; \
    B_ClosureCont##N##G_methods.__init__ = B_ClosureCont##N##D___init__; \
    B_ClosureCont##N##G_methods.__serialize__ = B_ClosureCont##N##D___serialize__; \
    B_ClosureCont##N##G_methods.__deserialize__ = B_ClosureCont##N##D___deserialize__; \
    B_ClosureCont##N##G_methods.__bool__ = (B_bool (*)(B_ClosureCont##N))$ContD___bool__; \
    B_ClosureCont##N##G_methods.__str__ = (B_str (*)(B_ClosureCont##N))$ContD___str__; \
    B_ClosureCont##N##G_methods.__repr__ = (B_str (*)(B_ClosureCont##N))$ContD___str__; \
    B_ClosureCont##N##G_methods.__call__ = B_ClosureCont##N##D___call__; \
    $register(&B_ClosureCont##N##G_methods);

#define B_INIT_CLOSURE_PROC(N) \
    B_ClosureProc##N##G_methods.$GCINFO = "B_ClosureProc" #N; \
    B_ClosureProc##N##G_methods.$superclass = ($SuperG_class)&$procG_methods; \
    B_ClosureProc##N##G_methods.__init__ = B_ClosureProc##N##D___init__; \
    B_ClosureProc##N##G_methods.__serialize__ = B_ClosureProc##N##D___serialize__; \
    B_ClosureProc##N##G_methods.__deserialize__ = B_ClosureProc##N##D___deserialize__; \
    B_ClosureProc##N##G_methods.__bool__ = (B_bool (*)(B_ClosureProc##N))$procD___bool__; \
    B_ClosureProc##N##G_methods.__str__ = (B_str (*)(B_ClosureProc##N))$procD___str__; \
    B_ClosureProc##N##G_methods.__repr__ = (B_str (*)(B_ClosureProc##N))$procD___str__; \
    B_ClosureProc##N##G_methods.__call__ = B_ClosureProc##N##D___call__; \
    B_ClosureProc##N##G_methods.__exec__ = B_ClosureProc##N##D___exec__; \
    $register(&B_ClosureProc##N##G_methods);

void B_ClosureQ___init__() {
    B_INIT_CLOSURE_CONT(0)
    B_INIT_CLOSURE_CONT(1)
    B_INIT_CLOSURE_CONT(2)
    B_INIT_CLOSURE_CONT(3)
    B_INIT_CLOSURE_CONT(4)

    B_INIT_CLOSURE_PROC(0)
    B_INIT_CLOSURE_PROC(1)
    B_INIT_CLOSURE_PROC(2)
    B_INIT_CLOSURE_PROC(3)
    B_INIT_CLOSURE_PROC(4)

    B_ClosureMut0G_methods.$GCINFO = "B_ClosureMut0";
    B_ClosureMut0G_methods.$superclass = ($SuperG_class)&$mutG_methods;
    B_ClosureMut0G_methods.__init__ = B_ClosureMut0D___init__;
    B_ClosureMut0G_methods.__serialize__ = B_ClosureMut0D___serialize__;
    B_ClosureMut0G_methods.__deserialize__ = B_ClosureMut0D___deserialize__;
    B_ClosureMut0G_methods.__bool__ = (B_bool (*)(B_ClosureMut0))$mutD___bool__;
    B_ClosureMut0G_methods.__str__ = (B_str (*)(B_ClosureMut0))$mutD___str__;
    B_ClosureMut0G_methods.__repr__ = (B_str (*)(B_ClosureMut0))$mutD___str__;
    B_ClosureMut0G_methods.__call__ = B_ClosureMut0D___call__;
    B_ClosureMut0G_methods.__exec__ = B_ClosureMut0D___exec__;
    B_ClosureMut0G_methods.__eval__ = B_ClosureMut0D___eval__;
    $register(&B_ClosureMut0G_methods);
}

#undef B_INIT_CLOSURE_CONT
#undef B_INIT_CLOSURE_PROC
#undef B_DEFINE_CLOSURE_CONT
#undef B_DEFINE_CLOSURE_PROC
#undef B_CLOSURE_ARGS_0
#undef B_CLOSURE_ARGS_1
#undef B_CLOSURE_ARGS_2
#undef B_CLOSURE_ARGS_3
#undef B_CLOSURE_ARGS_4
#undef B_CLOSURE_CALL_ARGS_0
#undef B_CLOSURE_CALL_ARGS_1
#undef B_CLOSURE_CALL_ARGS_2
#undef B_CLOSURE_CALL_ARGS_3
#undef B_CLOSURE_CALL_ARGS_4
#undef B_CLOSURE_INIT_FIELDS_0
#undef B_CLOSURE_INIT_FIELDS_1
#undef B_CLOSURE_INIT_FIELDS_2
#undef B_CLOSURE_INIT_FIELDS_3
#undef B_CLOSURE_INIT_FIELDS_4
