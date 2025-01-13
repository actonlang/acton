#include "rts/common.h"
#include "out/types/test.h"
$R testQ_L_1C_1cont ($Cont C_cont, testQ_main G_act, B_NoneType C_2res) {
    return $R_CONT(C_cont, G_act);
}
B_NoneType testQ_L_2ContD___init__ (testQ_L_2Cont L_self, $Cont C_cont, testQ_main G_act) {
    L_self->C_cont = C_cont;
    L_self->G_act = G_act;
    return B_None;
}
$R testQ_L_2ContD___call__ (testQ_L_2Cont L_self, B_NoneType G_1) {
    $Cont C_cont = L_self->C_cont;
    testQ_main G_act = L_self->G_act;
    return testQ_L_1C_1cont(C_cont, G_act, G_1);
}
void testQ_L_2ContD___serialize__ (testQ_L_2Cont self, $Serial$state state) {
    $step_serialize(self->C_cont, state);
    $step_serialize(self->G_act, state);
}
testQ_L_2Cont testQ_L_2ContD___deserialize__ (testQ_L_2Cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct testQ_L_2Cont));
            self->$class = &testQ_L_2ContG_methods;
            return self;
        }
        self = $DNEW(testQ_L_2Cont, state);
    }
    self->C_cont = $step_deserialize(state);
    self->G_act = $step_deserialize(state);
    return self;
}
testQ_L_2Cont testQ_L_2ContG_new($Cont G_1, testQ_main G_2) {
    testQ_L_2Cont $tmp = acton_malloc(sizeof(struct testQ_L_2Cont));
    $tmp->$class = &testQ_L_2ContG_methods;
    testQ_L_2ContG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct testQ_L_2ContG_class testQ_L_2ContG_methods;
B_NoneType testQ_L_3procD___init__ (testQ_L_3proc L_self, testQ_main G_act, B_Env env) {
    L_self->G_act = G_act;
    L_self->env = env;
    return B_None;
}
$R testQ_L_3procD___call__ (testQ_L_3proc L_self, $Cont C_cont) {
    testQ_main G_act = L_self->G_act;
    B_Env env = L_self->env;
    return (($R (*) (testQ_main, $Cont, B_Env))G_act->$class->__init__)(G_act, C_cont, env);
}
$R testQ_L_3procD___exec__ (testQ_L_3proc L_self, $Cont C_cont) {
    return (($R (*) (testQ_L_3proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void testQ_L_3procD___serialize__ (testQ_L_3proc self, $Serial$state state) {
    $step_serialize(self->G_act, state);
    $step_serialize(self->env, state);
}
testQ_L_3proc testQ_L_3procD___deserialize__ (testQ_L_3proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct testQ_L_3proc));
            self->$class = &testQ_L_3procG_methods;
            return self;
        }
        self = $DNEW(testQ_L_3proc, state);
    }
    self->G_act = $step_deserialize(state);
    self->env = $step_deserialize(state);
    return self;
}
testQ_L_3proc testQ_L_3procG_new(testQ_main G_1, B_Env G_2) {
    testQ_L_3proc $tmp = acton_malloc(sizeof(struct testQ_L_3proc));
    $tmp->$class = &testQ_L_3procG_methods;
    testQ_L_3procG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct testQ_L_3procG_class testQ_L_3procG_methods;
$R testQ_mainD___init__ (testQ_main self, $Cont C_cont, B_Env env) {
    bQ_foo();
    ((B_Msg (*) (B_Env, B_int))env->$class->exit)(env, to$int(0));
    return $R_CONT(C_cont, B_None);
}
void testQ_mainD___serialize__ (testQ_main self, $Serial$state state) {
    $ActorG_methods.__serialize__(($Actor)self, state);
}
testQ_main testQ_mainD___deserialize__ (testQ_main self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct testQ_main));
            self->$class = &testQ_mainG_methods;
            return self;
        }
        self = $DNEW(testQ_main, state);
    }
    $ActorG_methods.__deserialize__(($Actor)self, state);
    return self;
}
void testQ_mainD__GC_finalizer (void *obj, void *cdata) {
    testQ_main self = (testQ_main)obj;
    self->$class->__cleanup__(self);
}
$R testQ_mainG_new($Cont G_1, B_Env G_2) {
    testQ_main $tmp = acton_malloc(sizeof(struct testQ_main));
    $tmp->$class = &testQ_mainG_methods;
    return testQ_mainG_methods.__init__($tmp, $CONSTCONT($tmp, G_1), G_2);
}
struct testQ_mainG_class testQ_mainG_methods;
$R testQ_mainG_newact ($Cont C_cont, B_Env env) {
    testQ_main G_act = $NEWACTOR(testQ_main);
    if (G_act->$class->__cleanup__ != $ActorD___cleanup__) $GCfinalizer(G_act, testQ_mainD__GC_finalizer);
    return $AWAIT((($Cont)testQ_L_2ContG_new(C_cont, G_act)), $ASYNC((($Actor)G_act), (($Cont)testQ_L_3procG_new(G_act, env))));
}
int testQ_done$ = 0;
void testQ___init__ () {
    if (testQ_done$) return;
    testQ_done$ = 1;
    bQ___init__();
    {
        testQ_L_2ContG_methods.$GCINFO = "testQ_L_2Cont";
        testQ_L_2ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        testQ_L_2ContG_methods.__bool__ = (B_bool (*) (testQ_L_2Cont))B_valueG_methods.__bool__;
        testQ_L_2ContG_methods.__str__ = (B_str (*) (testQ_L_2Cont))B_valueG_methods.__str__;
        testQ_L_2ContG_methods.__repr__ = (B_str (*) (testQ_L_2Cont))B_valueG_methods.__repr__;
        testQ_L_2ContG_methods.__init__ = testQ_L_2ContD___init__;
        testQ_L_2ContG_methods.__call__ = testQ_L_2ContD___call__;
        testQ_L_2ContG_methods.__serialize__ = testQ_L_2ContD___serialize__;
        testQ_L_2ContG_methods.__deserialize__ = testQ_L_2ContD___deserialize__;
        $register(&testQ_L_2ContG_methods);
    }
    {
        testQ_L_3procG_methods.$GCINFO = "testQ_L_3proc";
        testQ_L_3procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        testQ_L_3procG_methods.__bool__ = (B_bool (*) (testQ_L_3proc))B_valueG_methods.__bool__;
        testQ_L_3procG_methods.__str__ = (B_str (*) (testQ_L_3proc))B_valueG_methods.__str__;
        testQ_L_3procG_methods.__repr__ = (B_str (*) (testQ_L_3proc))B_valueG_methods.__repr__;
        testQ_L_3procG_methods.__init__ = testQ_L_3procD___init__;
        testQ_L_3procG_methods.__call__ = testQ_L_3procD___call__;
        testQ_L_3procG_methods.__exec__ = testQ_L_3procD___exec__;
        testQ_L_3procG_methods.__serialize__ = testQ_L_3procD___serialize__;
        testQ_L_3procG_methods.__deserialize__ = testQ_L_3procD___deserialize__;
        $register(&testQ_L_3procG_methods);
    }
    {
        testQ_mainG_methods.$GCINFO = "testQ_main";
        testQ_mainG_methods.$superclass = ($SuperG_class)&$ActorG_methods;
        testQ_mainG_methods.__bool__ = (B_bool (*) (testQ_main))$ActorG_methods.__bool__;
        testQ_mainG_methods.__str__ = (B_str (*) (testQ_main))$ActorG_methods.__str__;
        testQ_mainG_methods.__repr__ = (B_str (*) (testQ_main))$ActorG_methods.__repr__;
        testQ_mainG_methods.__resume__ = (B_NoneType (*) (testQ_main))$ActorG_methods.__resume__;
        testQ_mainG_methods.__cleanup__ = (B_NoneType (*) (testQ_main))$ActorG_methods.__cleanup__;
        testQ_mainG_methods.__init__ = testQ_mainD___init__;
        testQ_mainG_methods.__serialize__ = testQ_mainD___serialize__;
        testQ_mainG_methods.__deserialize__ = testQ_mainD___deserialize__;
        $register(&testQ_mainG_methods);
    }
}