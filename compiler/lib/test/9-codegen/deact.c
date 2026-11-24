/* Acton impl hash: test-hash */
#include "rts/common.h"
#include "out/types/deact.h"
$R deactQ_L_1C_2cont ($action cb, $Cont C_cont, int64_t C_3res) {
    int64_t C_1pre = C_3res;
    #line 7 "test/src/deact.act"
    int64_t v = C_1pre;
    #line 8 "test/src/deact.act"
    B_Msg m = ((B_Msg)((B_Msg (*) ($WORD, B_int))(($action)(cb))->$class->__asyn__)(cb, toB_int(2LL)));
    int64_t N_1tmp = (((int64_t)(v * 10LL)));
    return $R_CONT(C_cont, toB_int(N_1tmp));
}
$R deactQ_L_2ContD___call__($Cont $closure, $WORD $result) {
    deactQ_L_2Cont L_self = (deactQ_L_2Cont)$closure;
    B_int G_1 = (B_int)$result;
    $action cb = ($action)(((deactQ_L_2Cont)(L_self))->ref0);
    $Cont C_cont = ($Cont)(((deactQ_L_2Cont)(L_self))->ref1);
    return deactQ_L_1C_2cont(cb, C_cont, ((B_int)G_1)->val);
}
$Cont deactQ_L_2ContD___deserialize__($Cont self, $Serial$state state) {
    return $cont_restore(&deactQ_L_2ContG_methods, self, state);
}
struct $ContClass deactQ_L_2ContG_methods;
$R deactQ_L_3C_4cont ($Cont C_cont, B_NoneType C_5res) {
    #line 16 "test/src/deact.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromLiteral("Apa", 3, 3)), actStrFromLiteral(" ", 1, 1), actStrFromLiteral("\n", 1, 1), false, false);
    return $R_CONT(C_cont, B_None);
}
$R deactQ_L_4ContD___call__($Cont $closure, $WORD $result) {
    deactQ_L_4Cont L_self = (deactQ_L_4Cont)$closure;
    B_NoneType G_1 = (B_NoneType)$result;
    $Cont C_cont = ($Cont)(((deactQ_L_4Cont)(L_self))->ref0);
    return deactQ_L_3C_4cont(C_cont, G_1);
}
$Cont deactQ_L_4ContD___deserialize__($Cont self, $Serial$state state) {
    return $cont_restore(&deactQ_L_4ContG_methods, self, state);
}
struct $ContClass deactQ_L_4ContG_methods;
B_Msg deactQ_L_6actionD___asyn__($action $closure, $WORD $arg1) {
    deactQ_L_6action L_self = (deactQ_L_6action)$closure;
    B_int G_1 = (B_int)$arg1;
    deactQ_Apa L_5obj = (deactQ_Apa)(((deactQ_L_6action)(L_self))->ref0);
    return ((B_Msg)((B_Msg (*) ($WORD, int64_t))((deactQ_Apa)(L_5obj))->$class->notice)(L_5obj, ((B_int)G_1)->val));
}
$action deactQ_L_6actionD___deserialize__($action self, $Serial$state state) {
    return $action_r1_restore(&deactQ_L_6actionG_methods, self, state);
}
struct $Action1G_class deactQ_L_6actionG_methods;
$R deactQ_L_7procD___call__($proc $closure, $Cont $continuation) {
    deactQ_L_7proc L_self = (deactQ_L_7proc)$closure;
    $Cont C_cont = ($Cont)$continuation;
    deactQ_Apa self = (deactQ_Apa)(((deactQ_L_7proc)(L_self))->ref0);
    $action cb = ($action)(((deactQ_L_7proc)(L_self))->ref1);
    return (($R (*) ($WORD, $Cont, $action))((deactQ_Apa)(self))->$class->setupG_local)(self, C_cont, cb);
}
$proc deactQ_L_7procD___deserialize__($proc self, $Serial$state state) {
    return $proc0_restore(&deactQ_L_7procG_methods, self, state);
}
struct $Proc0Class deactQ_L_7procG_methods;
$R deactQ_L_8procD___call__($proc $closure, $Cont $continuation) {
    deactQ_L_8proc L_self = (deactQ_L_8proc)$closure;
    $Cont C_cont = ($Cont)$continuation;
    deactQ_Apa self = (deactQ_Apa)(((deactQ_L_8proc)(L_self))->ref0);
    $action cb = ($action)(((deactQ_L_8proc)(L_self))->ref1);
    return (($R (*) ($WORD, $Cont, $action))((deactQ_Apa)(self))->$class->computeG_local)(self, C_cont, cb);
}
$proc deactQ_L_8procD___deserialize__($proc self, $Serial$state state) {
    return $proc0_restore(&deactQ_L_8procG_methods, self, state);
}
struct $Proc0Class deactQ_L_8procG_methods;
$R deactQ_L_9procD___call__ (deactQ_L_9proc L_self, $Cont C_cont) {
    deactQ_Apa self = ((deactQ_L_9proc)(L_self))->self;
    int64_t i = ((int64_t)((deactQ_L_9proc)(L_self))->i);
    return (($R (*) ($WORD, $Cont, int64_t))((deactQ_Apa)(self))->$class->noticeG_local)(self, C_cont, i);
}
static const struct $ClosureField deactQ_L_9procD_fields[] = {
    {offsetof(struct deactQ_L_9proc, self), sizeof(((deactQ_L_9proc)0)->self), $CLOSURE_REF},
    {offsetof(struct deactQ_L_9proc, i), sizeof(((deactQ_L_9proc)0)->i), I64_ID},
};
static const struct $ClosureLayout deactQ_L_9procD_layout = {
    sizeof(struct deactQ_L_9proc), ($SerializableG_class)&deactQ_L_9procG_methods, 2, deactQ_L_9procD_fields
};
void deactQ_L_9procD___serialize__(deactQ_L_9proc self, $Serial$state state) {
    $closure_serialize(self, &deactQ_L_9procD_layout, state);
}
deactQ_L_9proc deactQ_L_9procD___deserialize__(deactQ_L_9proc self, $Serial$state state) {
    return $closure_deserialize(self, &deactQ_L_9procD_layout, state);
}
deactQ_L_9proc deactQ_L_9procG_new(deactQ_Apa G_1, int64_t G_2) {
    deactQ_L_9proc $tmp = acton_malloc(sizeof(struct deactQ_L_9proc));
    $tmp->$class = &deactQ_L_9procG_methods;
    $tmp->self = G_1;
    $tmp->i = G_2;
    return $tmp;
}
struct deactQ_L_9procG_class deactQ_L_9procG_methods;
$R deactQ_L_10procD___call__($proc $closure, $Cont $continuation) {
    deactQ_L_10proc L_self = (deactQ_L_10proc)$closure;
    $Cont C_cont = ($Cont)$continuation;
    deactQ_Apa self = (deactQ_Apa)(((deactQ_L_10proc)(L_self))->ref0);
    return (($R (*) ($WORD, $Cont))((deactQ_Apa)(self))->$class->__startup__G_local)(self, C_cont);
}
$proc deactQ_L_10procD___deserialize__($proc self, $Serial$state state) {
    return $proc0_restore(&deactQ_L_10procG_methods, self, state);
}
struct $Proc0Class deactQ_L_10procG_methods;
$R deactQ_L_11procD___call__ (deactQ_L_11proc L_self, $Cont C_cont) {
    deactQ_Bepa self = ((deactQ_L_11proc)(L_self))->self;
    int64_t i = ((int64_t)((deactQ_L_11proc)(L_self))->i);
    return (($R (*) ($WORD, $Cont, int64_t))((deactQ_Bepa)(self))->$class->callbackG_local)(self, C_cont, i);
}
_Static_assert($CLOSURE_FIELD_EQ(deactQ_L_11proc, self, deactQ_L_9proc, self) && $CLOSURE_FIELD_EQ(deactQ_L_11proc, i, deactQ_L_9proc, i), "Closure capture layout mismatch");
static const struct $ClosureLayout deactQ_L_11procD_layout = {
    sizeof(struct deactQ_L_11proc), ($SerializableG_class)&deactQ_L_11procG_methods, 2, deactQ_L_9procD_fields
};
void deactQ_L_11procD___serialize__(deactQ_L_11proc self, $Serial$state state) {
    $closure_serialize(self, &deactQ_L_11procD_layout, state);
}
deactQ_L_11proc deactQ_L_11procD___deserialize__(deactQ_L_11proc self, $Serial$state state) {
    return $closure_deserialize(self, &deactQ_L_11procD_layout, state);
}
deactQ_L_11proc deactQ_L_11procG_new(deactQ_Bepa G_1, int64_t G_2) {
    deactQ_L_11proc $tmp = acton_malloc(sizeof(struct deactQ_L_11proc));
    $tmp->$class = &deactQ_L_11procG_methods;
    $tmp->self = G_1;
    $tmp->i = G_2;
    return $tmp;
}
struct deactQ_L_11procG_class deactQ_L_11procG_methods;
$R deactQ_L_12procD___call__($proc $closure, $Cont $continuation) {
    deactQ_L_12proc L_self = (deactQ_L_12proc)$closure;
    $Cont C_cont = ($Cont)$continuation;
    deactQ_Bepa self = (deactQ_Bepa)(((deactQ_L_12proc)(L_self))->ref0);
    return (($R (*) ($WORD, $Cont))((deactQ_Bepa)(self))->$class->__startup__G_local)(self, C_cont);
}
$proc deactQ_L_12procD___deserialize__($proc self, $Serial$state state) {
    return $proc0_restore(&deactQ_L_12procG_methods, self, state);
}
struct $Proc0Class deactQ_L_12procG_methods;
#line 28 "test/src/deact.act"
$R deactQ_L_13myproc (deactQ_main self, $Cont C_cont, int64_t i) {
    #line 29 "test/src/deact.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(2, actStrFromLiteral("myproc", 6, 6), toB_int(i)), actStrFromLiteral(" ", 1, 1), actStrFromLiteral("\n", 1, 1), false, false);
    #line 30 "test/src/deact.act"
    if (i == 2LL) {
        #line 31 "test/src/deact.act"
        ((B_Msg (*) ($WORD, int64_t))((B_Env)(((deactQ_main)(self))->env))->$class->exit)(((deactQ_main)(self))->env, 0LL);
    }
    return $R_CONT(C_cont, toB_int(i));
}
B_Msg deactQ_L_15actionD___asyn__($action $closure, $WORD $arg1) {
    deactQ_L_15action L_self = (deactQ_L_15action)$closure;
    B_int G_1 = (B_int)$arg1;
    deactQ_Apa L_14obj = (deactQ_Apa)(((deactQ_L_15action)(L_self))->ref0);
    return ((B_Msg)((B_Msg (*) ($WORD, int64_t))((deactQ_Apa)(L_14obj))->$class->notice)(L_14obj, ((B_int)G_1)->val));
}
$action deactQ_L_15actionD___deserialize__($action self, $Serial$state state) {
    return $action_r1_restore(&deactQ_L_15actionG_methods, self, state);
}
struct $Action1G_class deactQ_L_15actionG_methods;
B_Msg deactQ_L_17actionD___asyn__($action $closure, $WORD $arg1) {
    deactQ_L_17action L_self = (deactQ_L_17action)$closure;
    B_int G_1 = (B_int)$arg1;
    deactQ_Bepa L_16obj = (deactQ_Bepa)(((deactQ_L_17action)(L_self))->ref0);
    return ((B_Msg)((B_Msg (*) ($WORD, int64_t))((deactQ_Bepa)(L_16obj))->$class->callback)(L_16obj, ((B_int)G_1)->val));
}
$action deactQ_L_17actionD___deserialize__($action self, $Serial$state state) {
    return $action_r1_restore(&deactQ_L_17actionG_methods, self, state);
}
struct $Action1G_class deactQ_L_17actionG_methods;
$R deactQ_L_19procD___call__ (deactQ_L_19proc L_self, $Cont C_cont) {
    int64_t G_1 = ((int64_t)((deactQ_L_19proc)(L_self))->G_1);
    deactQ_main self = ((deactQ_L_19proc)(L_self))->self;
    return deactQ_L_13myproc(self, C_cont, G_1);
}
static const struct $ClosureField deactQ_L_19procD_fields[] = {
    {offsetof(struct deactQ_L_19proc, G_1), sizeof(((deactQ_L_19proc)0)->G_1), I64_ID},
    {offsetof(struct deactQ_L_19proc, self), sizeof(((deactQ_L_19proc)0)->self), $CLOSURE_REF},
};
static const struct $ClosureLayout deactQ_L_19procD_layout = {
    sizeof(struct deactQ_L_19proc), ($SerializableG_class)&deactQ_L_19procG_methods, 2, deactQ_L_19procD_fields
};
void deactQ_L_19procD___serialize__(deactQ_L_19proc self, $Serial$state state) {
    $closure_serialize(self, &deactQ_L_19procD_layout, state);
}
deactQ_L_19proc deactQ_L_19procD___deserialize__(deactQ_L_19proc self, $Serial$state state) {
    return $closure_deserialize(self, &deactQ_L_19procD_layout, state);
}
deactQ_L_19proc deactQ_L_19procG_new(int64_t G_1, deactQ_main G_2) {
    deactQ_L_19proc $tmp = acton_malloc(sizeof(struct deactQ_L_19proc));
    $tmp->$class = &deactQ_L_19procG_methods;
    $tmp->G_1 = G_1;
    $tmp->self = G_2;
    return $tmp;
}
struct deactQ_L_19procG_class deactQ_L_19procG_methods;
B_Msg deactQ_L_20actionD___asyn__($action $closure, $WORD $arg1) {
    deactQ_L_20action L_self = (deactQ_L_20action)$closure;
    B_int G_1 = (B_int)$arg1;
    deactQ_main self = (deactQ_main)(((deactQ_L_20action)(L_self))->ref0);
    return ((B_Msg)$ASYNC((($Actor)self), (($Cont)deactQ_L_19procG_new(((B_int)G_1)->val, self))));
}
$action deactQ_L_20actionD___deserialize__($action self, $Serial$state state) {
    return $action_r1_restore(&deactQ_L_20actionG_methods, self, state);
}
struct $Action1G_class deactQ_L_20actionG_methods;
$R deactQ_L_18C_6cont (deactQ_Apa a, deactQ_main self, $Cont C_cont, int64_t C_7res) {
    #line 38 "test/src/deact.act"
    int64_t r = C_7res;
    #line 39 "test/src/deact.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(2, actStrFromLiteral("r =", 3, 3), toB_int(r)), actStrFromLiteral(" ", 1, 1), actStrFromLiteral("\n", 1, 1), false, false);
    #line 40 "test/src/deact.act"
    ((B_Msg (*) ($WORD, $action))((deactQ_Apa)(a))->$class->compute)(a, (($action)((deactQ_L_20action)$Action_R1_new(&deactQ_L_20actionG_methods, self))));
    #line 41 "test/src/deact.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromLiteral("main", 4, 4)), actStrFromLiteral(" ", 1, 1), actStrFromLiteral("\n", 1, 1), false, false);
    return $R_CONT(C_cont, B_None);
}
$R deactQ_L_21ContD___call__($Cont $closure, $WORD $result) {
    deactQ_L_21Cont L_self = (deactQ_L_21Cont)$closure;
    B_int G_1 = (B_int)$result;
    deactQ_Apa a = (deactQ_Apa)(((deactQ_L_21Cont)(L_self))->ref0);
    deactQ_main self = (deactQ_main)(((deactQ_L_21Cont)(L_self))->ref1);
    $Cont C_cont = ($Cont)(((deactQ_L_21Cont)(L_self))->ref2);
    return deactQ_L_18C_6cont(a, self, C_cont, ((B_int)G_1)->val);
}
$Cont deactQ_L_21ContD___deserialize__($Cont self, $Serial$state state) {
    return $cont_restore(&deactQ_L_21ContG_methods, self, state);
}
struct $ContClass deactQ_L_21ContG_methods;
$R deactQ_L_22procD___call__($proc $closure, $Cont $continuation) {
    deactQ_L_22proc L_self = (deactQ_L_22proc)$closure;
    $Cont C_cont = ($Cont)$continuation;
    deactQ_main self = (deactQ_main)(((deactQ_L_22proc)(L_self))->ref0);
    return (($R (*) ($WORD, $Cont))((deactQ_main)(self))->$class->__startup__G_local)(self, C_cont);
}
$proc deactQ_L_22procD___deserialize__($proc self, $Serial$state state) {
    return $proc0_restore(&deactQ_L_22procG_methods, self, state);
}
struct $Proc0Class deactQ_L_22procG_methods;
B_NoneType deactQ_ApaD___init__ (deactQ_Apa self) {
    $MarkStartup(((B_Msg (*) ($WORD))((deactQ_Apa)(self))->$class->__startup__)(self));
    return B_None;
}
#line 2 "test/src/deact.act"
$R deactQ_ApaD_setupG_local (deactQ_Apa self, $Cont C_cont, $action cb) {
    #line 3 "test/src/deact.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromLiteral("setup", 5, 5)), actStrFromLiteral(" ", 1, 1), actStrFromLiteral("\n", 1, 1), false, false);
    #line 4 "test/src/deact.act"
    ((B_Msg (*) ($WORD, B_int))(($action)(cb))->$class->__asyn__)(cb, toB_int(0LL));
    return $R_CONT(C_cont, B_None);
}
#line 5 "test/src/deact.act"
$R deactQ_ApaD_computeG_local (deactQ_Apa self, $Cont C_cont, $action cb) {
    #line 6 "test/src/deact.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromLiteral("compute", 7, 7)), actStrFromLiteral(" ", 1, 1), actStrFromLiteral("\n", 1, 1), false, false);
    $action N_function = cb;
    return $AWAIT((($Cont)((deactQ_L_2Cont)$Cont_R2_new(&deactQ_L_2ContG_methods, cb, C_cont))), ((B_Msg)((B_Msg (*) ($WORD, B_int))(($action)(N_function))->$class->__asyn__)(N_function, toB_int(1LL))));
}
#line 10 "test/src/deact.act"
$R deactQ_ApaD_noticeG_local (deactQ_Apa self, $Cont C_cont, int64_t i) {
    #line 11 "test/src/deact.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromLiteral("notice", 6, 6)), actStrFromLiteral(" ", 1, 1), actStrFromLiteral("\n", 1, 1), false, false);
    int64_t N_2tmp = (((int64_t)(i + 1LL)));
    return $R_CONT(C_cont, toB_int(N_2tmp));
}
#line 14 "test/src/deact.act"
$R deactQ_ApaD___startup__G_local (deactQ_Apa self, $Cont C_cont) {
    return (($R (*) ($WORD, $Cont, $action))((deactQ_Apa)(self))->$class->setupG_local)(self, $SKIPRES((($Cont)((deactQ_L_4Cont)$Cont_R1_new(&deactQ_L_4ContG_methods, C_cont)))), (($action)((deactQ_L_6action)$Action_R1_new(&deactQ_L_6actionG_methods, self))));
}
B_Msg deactQ_ApaD_setup (deactQ_Apa self, $action cb) {
    return $ASYNC((($Actor)self), (($Cont)((deactQ_L_7proc)$Proc0_R2_new(&deactQ_L_7procG_methods, self, cb))));
}
B_Msg deactQ_ApaD_compute (deactQ_Apa self, $action cb) {
    return ((B_Msg)$ASYNC((($Actor)self), (($Cont)((deactQ_L_8proc)$Proc0_R2_new(&deactQ_L_8procG_methods, self, cb)))));
}
B_Msg deactQ_ApaD_notice (deactQ_Apa self, int64_t i) {
    return ((B_Msg)$ASYNC((($Actor)self), (($Cont)deactQ_L_9procG_new(self, i))));
}
B_Msg deactQ_ApaD___startup__ (deactQ_Apa self) {
    return $ASYNC((($Actor)self), (($Cont)((deactQ_L_10proc)$Proc0_R1_new(&deactQ_L_10procG_methods, self))));
}
void deactQ_ApaD___serialize__ (deactQ_Apa self, $Serial$state state) {
    $ActorG_methods.__serialize__(($Actor)self, state);
}
deactQ_Apa deactQ_ApaD___deserialize__ (deactQ_Apa self, $Serial$state state) {
    $WORD $tmp;
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_Apa));
            self->$class = &deactQ_ApaG_methods;
            return self;
        }
        self = $DNEW(deactQ_Apa, state);
    }
    $ActorG_methods.__deserialize__(($Actor)self, state);
    return self;
}
void deactQ_ApaD_GCfinalizer (void *obj, void *cdata) {
    deactQ_Apa self = (deactQ_Apa)obj;
    self->$class->__cleanup__(self);
}
deactQ_Apa deactQ_ApaG_new() {
    deactQ_Apa $tmp = acton_malloc(sizeof(struct deactQ_Apa));
    $tmp->$class = &deactQ_ApaG_methods;
    deactQ_ApaG_methods.__init__($tmp);
    return $tmp;
}
struct deactQ_ApaG_class deactQ_ApaG_methods;
B_NoneType deactQ_BepaD___init__ (deactQ_Bepa self) {
    $MarkStartup(((B_Msg (*) ($WORD))((deactQ_Bepa)(self))->$class->__startup__)(self));
    return B_None;
}
#line 19 "test/src/deact.act"
$R deactQ_BepaD_callbackG_local (deactQ_Bepa self, $Cont C_cont, int64_t i) {
    #line 20 "test/src/deact.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(2, actStrFromLiteral("callback", 8, 8), toB_int(i)), actStrFromLiteral(" ", 1, 1), actStrFromLiteral("\n", 1, 1), false, false);
    int64_t N_4tmp = (((int64_t)(i + 1LL)));
    return $R_CONT(C_cont, toB_int(N_4tmp));
}
#line 23 "test/src/deact.act"
$R deactQ_BepaD___startup__G_local (deactQ_Bepa self, $Cont C_cont) {
    #line 24 "test/src/deact.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromLiteral("Bepa", 4, 4)), actStrFromLiteral(" ", 1, 1), actStrFromLiteral("\n", 1, 1), false, false);
    return $R_CONT(C_cont, B_None);
}
B_Msg deactQ_BepaD_callback (deactQ_Bepa self, int64_t i) {
    return ((B_Msg)$ASYNC((($Actor)self), (($Cont)deactQ_L_11procG_new(self, i))));
}
B_Msg deactQ_BepaD___startup__ (deactQ_Bepa self) {
    return $ASYNC((($Actor)self), (($Cont)((deactQ_L_12proc)$Proc0_R1_new(&deactQ_L_12procG_methods, self))));
}
void deactQ_BepaD___serialize__ (deactQ_Bepa self, $Serial$state state) {
    $ActorG_methods.__serialize__(($Actor)self, state);
}
deactQ_Bepa deactQ_BepaD___deserialize__ (deactQ_Bepa self, $Serial$state state) {
    $WORD $tmp;
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_Bepa));
            self->$class = &deactQ_BepaG_methods;
            return self;
        }
        self = $DNEW(deactQ_Bepa, state);
    }
    $ActorG_methods.__deserialize__(($Actor)self, state);
    return self;
}
void deactQ_BepaD_GCfinalizer (void *obj, void *cdata) {
    deactQ_Bepa self = (deactQ_Bepa)obj;
    self->$class->__cleanup__(self);
}
deactQ_Bepa deactQ_BepaG_new() {
    deactQ_Bepa $tmp = acton_malloc(sizeof(struct deactQ_Bepa));
    $tmp->$class = &deactQ_BepaG_methods;
    deactQ_BepaG_methods.__init__($tmp);
    return $tmp;
}
struct deactQ_BepaG_class deactQ_BepaG_methods;
B_NoneType deactQ_mainD___init__ (deactQ_main self, B_Env env) {
    ACTON_STORE_ATTR(((deactQ_main)(self)), env, env);
    $MarkStartup(((B_Msg (*) ($WORD))((deactQ_main)(self))->$class->__startup__)(self));
    return B_None;
}
#line 27 "test/src/deact.act"
$R deactQ_mainD___startup__G_local (deactQ_main self, $Cont C_cont) {
    #line 33 "test/src/deact.act"
    deactQ_Apa a = deactQ_ApaG_newact();
    #line 34 "test/src/deact.act"
    deactQ_Bepa b = deactQ_BepaG_newact();
    #line 35 "test/src/deact.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromLiteral("-----", 5, 5)), actStrFromLiteral(" ", 1, 1), actStrFromLiteral("\n", 1, 1), false, false);
    #line 36 "test/src/deact.act"
    ((B_Msg (*) ($WORD, $action))((deactQ_Apa)(a))->$class->setup)(a, (($action)((deactQ_L_15action)$Action_R1_new(&deactQ_L_15actionG_methods, a))));
    #line 37 "test/src/deact.act"
    B_Msg x = ((B_Msg (*) ($WORD, $action))((deactQ_Apa)(a))->$class->compute)(a, (($action)((deactQ_L_17action)$Action_R1_new(&deactQ_L_17actionG_methods, b))));
    return $AWAIT((($Cont)((deactQ_L_21Cont)$Cont_R3_new(&deactQ_L_21ContG_methods, a, self, C_cont))), x);
}
B_Msg deactQ_mainD___startup__ (deactQ_main self) {
    return $ASYNC((($Actor)self), (($Cont)((deactQ_L_22proc)$Proc0_R1_new(&deactQ_L_22procG_methods, self))));
}
void deactQ_mainD___serialize__ (deactQ_main self, $Serial$state state) {
    $ActorG_methods.__serialize__(($Actor)self, state);
    $step_serialize(self->env, state);
}
deactQ_main deactQ_mainD___deserialize__ (deactQ_main self, $Serial$state state) {
    $WORD $tmp;
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_main));
            self->$class = &deactQ_mainG_methods;
            return self;
        }
        self = $DNEW(deactQ_main, state);
    }
    $ActorG_methods.__deserialize__(($Actor)self, state);
    ACTON_STORE_ATTR(self, env, $step_deserialize(state));
    return self;
}
void deactQ_mainD_GCfinalizer (void *obj, void *cdata) {
    deactQ_main self = (deactQ_main)obj;
    self->$class->__cleanup__(self);
}
deactQ_main deactQ_mainG_new(B_Env G_1) {
    deactQ_main $tmp = acton_malloc(sizeof(struct deactQ_main));
    $tmp->$class = &deactQ_mainG_methods;
    deactQ_mainG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct deactQ_mainG_class deactQ_mainG_methods;
deactQ_Apa deactQ_ApaG_newact () {
    $Actor N_construction_parent = $ConstructionBegin();
    deactQ_Apa G_act;
    {
        struct JumpBuf $frame;
        if ($PUSH(&$frame)) {
            G_act = $NEWACTOR(deactQ_Apa);
            $ConstructionEnter((($Actor)G_act));
            if ((void*)G_act->$class->__cleanup__ != (void*)$ActorD___cleanup__) $InstallFinalizer(G_act, deactQ_ApaD_GCfinalizer);
            ((B_NoneType (*) ($WORD))((deactQ_Apa)(G_act))->$class->__init__)(G_act);
            $DROP();
        }
        else {
            B_BaseException N_construction_exception = $POP();
            $ConstructionAbort(N_construction_parent, N_construction_exception);
            $RAISE(N_construction_exception);
            __builtin_unreachable();
        }
    }
    $ConstructionEnd(N_construction_parent);
    return G_act;
}
deactQ_Bepa deactQ_BepaG_newact () {
    $Actor N_construction_parent = $ConstructionBegin();
    deactQ_Bepa G_act;
    {
        struct JumpBuf $frame;
        if ($PUSH(&$frame)) {
            G_act = $NEWACTOR(deactQ_Bepa);
            $ConstructionEnter((($Actor)G_act));
            if ((void*)G_act->$class->__cleanup__ != (void*)$ActorD___cleanup__) $InstallFinalizer(G_act, deactQ_BepaD_GCfinalizer);
            ((B_NoneType (*) ($WORD))((deactQ_Bepa)(G_act))->$class->__init__)(G_act);
            $DROP();
        }
        else {
            B_BaseException N_construction_exception = $POP();
            $ConstructionAbort(N_construction_parent, N_construction_exception);
            $RAISE(N_construction_exception);
            __builtin_unreachable();
        }
    }
    $ConstructionEnd(N_construction_parent);
    return G_act;
}
deactQ_main deactQ_mainG_newact (B_Env env) {
    $Actor N_construction_parent = $ConstructionBegin();
    deactQ_main G_act;
    {
        struct JumpBuf $frame;
        if ($PUSH(&$frame)) {
            G_act = $NEWACTOR(deactQ_main);
            $ConstructionEnter((($Actor)G_act));
            if ((void*)G_act->$class->__cleanup__ != (void*)$ActorD___cleanup__) $InstallFinalizer(G_act, deactQ_mainD_GCfinalizer);
            ((B_NoneType (*) ($WORD, B_Env))((deactQ_main)(G_act))->$class->__init__)(G_act, env);
            $DROP();
        }
        else {
            B_BaseException N_construction_exception = $POP();
            $ConstructionAbort(N_construction_parent, N_construction_exception);
            $RAISE(N_construction_exception);
            __builtin_unreachable();
        }
    }
    $ConstructionEnd(N_construction_parent);
    return G_act;
}
int deactQ_done$ = 0;
void deactQ___init__ () {
    if (deactQ_done$) return;
    deactQ_done$ = 1;
    $cont_class_init(&deactQ_L_2ContG_methods, "deactQ_L_2Cont", &$Cont_R2_shape, deactQ_L_2ContD___call__, deactQ_L_2ContD___deserialize__);
    $cont_class_init(&deactQ_L_4ContG_methods, "deactQ_L_4Cont", &$Cont_R1_shape, deactQ_L_4ContD___call__, deactQ_L_4ContD___deserialize__);
    $action1_class_init(&deactQ_L_6actionG_methods, "deactQ_L_6action", deactQ_L_6actionD___asyn__, deactQ_L_6actionD___deserialize__);
    $proc0_class_init(&deactQ_L_7procG_methods, "deactQ_L_7proc", &$Proc0_R2_shape, deactQ_L_7procD___call__, deactQ_L_7procD___deserialize__);
    $proc0_class_init(&deactQ_L_8procG_methods, "deactQ_L_8proc", &$Proc0_R2_shape, deactQ_L_8procD___call__, deactQ_L_8procD___deserialize__);
    {
        deactQ_L_9procG_methods.$GCINFO = "deactQ_L_9proc";
        deactQ_L_9procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        deactQ_L_9procG_methods.__bool__ = (bool (*) (deactQ_L_9proc))B_valueG_methods.__bool__;
        deactQ_L_9procG_methods.__str__ = (B_str (*) (deactQ_L_9proc))B_valueG_methods.__str__;
        deactQ_L_9procG_methods.__repr__ = (B_str (*) (deactQ_L_9proc))B_valueG_methods.__repr__;
        deactQ_L_9procG_methods.__init__ = NULL;
        deactQ_L_9procG_methods.__call__ = ($R (*) (deactQ_L_9proc, $Cont))deactQ_L_9procD___call__;
        deactQ_L_9procG_methods.__exec__ = ($R (*) (deactQ_L_9proc, $Cont))deactQ_L_9procD___call__;
        deactQ_L_9procG_methods.__serialize__ = deactQ_L_9procD___serialize__;
        deactQ_L_9procG_methods.__deserialize__ = deactQ_L_9procD___deserialize__;
        $register(&deactQ_L_9procG_methods);
    }
    $proc0_class_init(&deactQ_L_10procG_methods, "deactQ_L_10proc", &$Proc0_R1_shape, deactQ_L_10procD___call__, deactQ_L_10procD___deserialize__);
    {
        deactQ_L_11procG_methods.$GCINFO = "deactQ_L_11proc";
        deactQ_L_11procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        deactQ_L_11procG_methods.__bool__ = (bool (*) (deactQ_L_11proc))B_valueG_methods.__bool__;
        deactQ_L_11procG_methods.__str__ = (B_str (*) (deactQ_L_11proc))B_valueG_methods.__str__;
        deactQ_L_11procG_methods.__repr__ = (B_str (*) (deactQ_L_11proc))B_valueG_methods.__repr__;
        deactQ_L_11procG_methods.__init__ = NULL;
        deactQ_L_11procG_methods.__call__ = ($R (*) (deactQ_L_11proc, $Cont))deactQ_L_11procD___call__;
        deactQ_L_11procG_methods.__exec__ = ($R (*) (deactQ_L_11proc, $Cont))deactQ_L_11procD___call__;
        deactQ_L_11procG_methods.__serialize__ = deactQ_L_11procD___serialize__;
        deactQ_L_11procG_methods.__deserialize__ = deactQ_L_11procD___deserialize__;
        $register(&deactQ_L_11procG_methods);
    }
    $proc0_class_init(&deactQ_L_12procG_methods, "deactQ_L_12proc", &$Proc0_R1_shape, deactQ_L_12procD___call__, deactQ_L_12procD___deserialize__);
    $action1_class_init(&deactQ_L_15actionG_methods, "deactQ_L_15action", deactQ_L_15actionD___asyn__, deactQ_L_15actionD___deserialize__);
    $action1_class_init(&deactQ_L_17actionG_methods, "deactQ_L_17action", deactQ_L_17actionD___asyn__, deactQ_L_17actionD___deserialize__);
    {
        deactQ_L_19procG_methods.$GCINFO = "deactQ_L_19proc";
        deactQ_L_19procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        deactQ_L_19procG_methods.__bool__ = (bool (*) (deactQ_L_19proc))B_valueG_methods.__bool__;
        deactQ_L_19procG_methods.__str__ = (B_str (*) (deactQ_L_19proc))B_valueG_methods.__str__;
        deactQ_L_19procG_methods.__repr__ = (B_str (*) (deactQ_L_19proc))B_valueG_methods.__repr__;
        deactQ_L_19procG_methods.__init__ = NULL;
        deactQ_L_19procG_methods.__call__ = ($R (*) (deactQ_L_19proc, $Cont))deactQ_L_19procD___call__;
        deactQ_L_19procG_methods.__exec__ = ($R (*) (deactQ_L_19proc, $Cont))deactQ_L_19procD___call__;
        deactQ_L_19procG_methods.__serialize__ = deactQ_L_19procD___serialize__;
        deactQ_L_19procG_methods.__deserialize__ = deactQ_L_19procD___deserialize__;
        $register(&deactQ_L_19procG_methods);
    }
    $action1_class_init(&deactQ_L_20actionG_methods, "deactQ_L_20action", deactQ_L_20actionD___asyn__, deactQ_L_20actionD___deserialize__);
    $cont_class_init(&deactQ_L_21ContG_methods, "deactQ_L_21Cont", &$Cont_R3_shape, deactQ_L_21ContD___call__, deactQ_L_21ContD___deserialize__);
    $proc0_class_init(&deactQ_L_22procG_methods, "deactQ_L_22proc", &$Proc0_R1_shape, deactQ_L_22procD___call__, deactQ_L_22procD___deserialize__);
    {
        deactQ_ApaG_methods.$GCINFO = "deactQ_Apa";
        deactQ_ApaG_methods.$superclass = ($SuperG_class)&$ActorG_methods;
        deactQ_ApaG_methods.__bool__ = (bool (*) (deactQ_Apa))$ActorG_methods.__bool__;
        deactQ_ApaG_methods.__str__ = (B_str (*) (deactQ_Apa))$ActorG_methods.__str__;
        deactQ_ApaG_methods.__repr__ = (B_str (*) (deactQ_Apa))$ActorG_methods.__repr__;
        deactQ_ApaG_methods.__resume__ = (B_NoneType (*) (deactQ_Apa))$ActorG_methods.__resume__;
        deactQ_ApaG_methods.__cleanup__ = (B_NoneType (*) (deactQ_Apa))$ActorG_methods.__cleanup__;
        deactQ_ApaG_methods.__init__ = (B_NoneType (*) (deactQ_Apa))deactQ_ApaD___init__;
        deactQ_ApaG_methods.setupG_local = ($R (*) (deactQ_Apa, $Cont, $action))deactQ_ApaD_setupG_local;
        deactQ_ApaG_methods.computeG_local = ($R (*) (deactQ_Apa, $Cont, $action))deactQ_ApaD_computeG_local;
        deactQ_ApaG_methods.noticeG_local = ($R (*) (deactQ_Apa, $Cont, int64_t))deactQ_ApaD_noticeG_local;
        deactQ_ApaG_methods.__startup__G_local = ($R (*) (deactQ_Apa, $Cont))deactQ_ApaD___startup__G_local;
        deactQ_ApaG_methods.setup = (B_Msg (*) (deactQ_Apa, $action))deactQ_ApaD_setup;
        deactQ_ApaG_methods.compute = (B_Msg (*) (deactQ_Apa, $action))deactQ_ApaD_compute;
        deactQ_ApaG_methods.notice = (B_Msg (*) (deactQ_Apa, int64_t))deactQ_ApaD_notice;
        deactQ_ApaG_methods.__startup__ = (B_Msg (*) (deactQ_Apa))deactQ_ApaD___startup__;
        deactQ_ApaG_methods.__serialize__ = deactQ_ApaD___serialize__;
        deactQ_ApaG_methods.__deserialize__ = deactQ_ApaD___deserialize__;
        $register(&deactQ_ApaG_methods);
    }
    {
        deactQ_BepaG_methods.$GCINFO = "deactQ_Bepa";
        deactQ_BepaG_methods.$superclass = ($SuperG_class)&$ActorG_methods;
        deactQ_BepaG_methods.__bool__ = (bool (*) (deactQ_Bepa))$ActorG_methods.__bool__;
        deactQ_BepaG_methods.__str__ = (B_str (*) (deactQ_Bepa))$ActorG_methods.__str__;
        deactQ_BepaG_methods.__repr__ = (B_str (*) (deactQ_Bepa))$ActorG_methods.__repr__;
        deactQ_BepaG_methods.__resume__ = (B_NoneType (*) (deactQ_Bepa))$ActorG_methods.__resume__;
        deactQ_BepaG_methods.__cleanup__ = (B_NoneType (*) (deactQ_Bepa))$ActorG_methods.__cleanup__;
        deactQ_BepaG_methods.__init__ = (B_NoneType (*) (deactQ_Bepa))deactQ_BepaD___init__;
        deactQ_BepaG_methods.callbackG_local = ($R (*) (deactQ_Bepa, $Cont, int64_t))deactQ_BepaD_callbackG_local;
        deactQ_BepaG_methods.__startup__G_local = ($R (*) (deactQ_Bepa, $Cont))deactQ_BepaD___startup__G_local;
        deactQ_BepaG_methods.callback = (B_Msg (*) (deactQ_Bepa, int64_t))deactQ_BepaD_callback;
        deactQ_BepaG_methods.__startup__ = (B_Msg (*) (deactQ_Bepa))deactQ_BepaD___startup__;
        deactQ_BepaG_methods.__serialize__ = deactQ_BepaD___serialize__;
        deactQ_BepaG_methods.__deserialize__ = deactQ_BepaD___deserialize__;
        $register(&deactQ_BepaG_methods);
    }
    {
        deactQ_mainG_methods.$GCINFO = "deactQ_main";
        deactQ_mainG_methods.$superclass = ($SuperG_class)&$ActorG_methods;
        deactQ_mainG_methods.__bool__ = (bool (*) (deactQ_main))$ActorG_methods.__bool__;
        deactQ_mainG_methods.__str__ = (B_str (*) (deactQ_main))$ActorG_methods.__str__;
        deactQ_mainG_methods.__repr__ = (B_str (*) (deactQ_main))$ActorG_methods.__repr__;
        deactQ_mainG_methods.__resume__ = (B_NoneType (*) (deactQ_main))$ActorG_methods.__resume__;
        deactQ_mainG_methods.__cleanup__ = (B_NoneType (*) (deactQ_main))$ActorG_methods.__cleanup__;
        deactQ_mainG_methods.__init__ = (B_NoneType (*) (deactQ_main, B_Env))deactQ_mainD___init__;
        deactQ_mainG_methods.__startup__G_local = ($R (*) (deactQ_main, $Cont))deactQ_mainD___startup__G_local;
        deactQ_mainG_methods.__startup__ = (B_Msg (*) (deactQ_main))deactQ_mainD___startup__;
        deactQ_mainG_methods.__serialize__ = deactQ_mainD___serialize__;
        deactQ_mainG_methods.__deserialize__ = deactQ_mainD___deserialize__;
        $register(&deactQ_mainG_methods);
    }
}