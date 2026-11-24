/* Acton impl hash: test-hash */
#include "rts/common.h"
#include "out/types/lines.h"
$R linesQ_L_1C_2cont ($action cb, $Cont C_cont, int64_t C_3res) {
    int64_t C_1pre = C_3res;
    #line 9 "test/src/lines.act"
    int64_t v = C_1pre;
    #line 10 "test/src/lines.act"
    B_Msg m = ((B_Msg)((B_Msg (*) ($WORD, B_int))(($action)(cb))->$class->__asyn__)(cb, toB_int(2LL)));
    int64_t N_1tmp = (((int64_t)(v * 10LL)));
    return $R_CONT(C_cont, toB_int(N_1tmp));
}
$R linesQ_L_2ContD___call__($Cont $closure, $WORD $result) {
    linesQ_L_2Cont L_self = (linesQ_L_2Cont)$closure;
    B_int G_1 = (B_int)$result;
    $action cb = ($action)(((linesQ_L_2Cont)(L_self))->ref0);
    $Cont C_cont = ($Cont)(((linesQ_L_2Cont)(L_self))->ref1);
    return linesQ_L_1C_2cont(cb, C_cont, ((B_int)G_1)->val);
}
$Cont linesQ_L_2ContD___deserialize__($Cont self, $Serial$state state) {
    return $cont_restore(&linesQ_L_2ContG_methods, self, state);
}
struct $ContClass linesQ_L_2ContG_methods;
$R linesQ_L_3C_4cont ($Cont C_cont, B_NoneType C_5res) {
    #line 20 "test/src/lines.act"
    int64_t z = 1LL;
    #line 21 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromLiteral("Apa", 3, 3)), actStrFromLiteral(" ", 1, 1), actStrFromLiteral("\n", 1, 1), false, false);
    return $R_CONT(C_cont, B_None);
}
$R linesQ_L_4ContD___call__($Cont $closure, $WORD $result) {
    linesQ_L_4Cont L_self = (linesQ_L_4Cont)$closure;
    B_NoneType G_1 = (B_NoneType)$result;
    $Cont C_cont = ($Cont)(((linesQ_L_4Cont)(L_self))->ref0);
    return linesQ_L_3C_4cont(C_cont, G_1);
}
$Cont linesQ_L_4ContD___deserialize__($Cont self, $Serial$state state) {
    return $cont_restore(&linesQ_L_4ContG_methods, self, state);
}
struct $ContClass linesQ_L_4ContG_methods;
B_Msg linesQ_L_6actionD___asyn__($action $closure, $WORD $arg1) {
    linesQ_L_6action L_self = (linesQ_L_6action)$closure;
    B_int G_1 = (B_int)$arg1;
    linesQ_Apa L_5obj = (linesQ_Apa)(((linesQ_L_6action)(L_self))->ref0);
    return ((B_Msg)((B_Msg (*) ($WORD, int64_t))((linesQ_Apa)(L_5obj))->$class->notice)(L_5obj, ((B_int)G_1)->val));
}
$action linesQ_L_6actionD___deserialize__($action self, $Serial$state state) {
    return $action_r1_restore(&linesQ_L_6actionG_methods, self, state);
}
struct $Action1G_class linesQ_L_6actionG_methods;
$R linesQ_L_7procD___call__($proc $closure, $Cont $continuation) {
    linesQ_L_7proc L_self = (linesQ_L_7proc)$closure;
    $Cont C_cont = ($Cont)$continuation;
    linesQ_Apa self = (linesQ_Apa)(((linesQ_L_7proc)(L_self))->ref0);
    $action cb = ($action)(((linesQ_L_7proc)(L_self))->ref1);
    return (($R (*) ($WORD, $Cont, $action))((linesQ_Apa)(self))->$class->setupG_local)(self, C_cont, cb);
}
$proc linesQ_L_7procD___deserialize__($proc self, $Serial$state state) {
    return $proc0_restore(&linesQ_L_7procG_methods, self, state);
}
struct $Proc0Class linesQ_L_7procG_methods;
$R linesQ_L_8procD___call__($proc $closure, $Cont $continuation) {
    linesQ_L_8proc L_self = (linesQ_L_8proc)$closure;
    $Cont C_cont = ($Cont)$continuation;
    linesQ_Apa self = (linesQ_Apa)(((linesQ_L_8proc)(L_self))->ref0);
    $action cb = ($action)(((linesQ_L_8proc)(L_self))->ref1);
    return (($R (*) ($WORD, $Cont, $action))((linesQ_Apa)(self))->$class->computeG_local)(self, C_cont, cb);
}
$proc linesQ_L_8procD___deserialize__($proc self, $Serial$state state) {
    return $proc0_restore(&linesQ_L_8procG_methods, self, state);
}
struct $Proc0Class linesQ_L_8procG_methods;
$R linesQ_L_9procD___call__ (linesQ_L_9proc L_self, $Cont C_cont) {
    linesQ_Apa self = ((linesQ_L_9proc)(L_self))->self;
    int64_t i = ((int64_t)((linesQ_L_9proc)(L_self))->i);
    return (($R (*) ($WORD, $Cont, int64_t))((linesQ_Apa)(self))->$class->noticeG_local)(self, C_cont, i);
}
static const struct $ClosureField linesQ_L_9procD_fields[] = {
    {offsetof(struct linesQ_L_9proc, self), sizeof(((linesQ_L_9proc)0)->self), $CLOSURE_REF},
    {offsetof(struct linesQ_L_9proc, i), sizeof(((linesQ_L_9proc)0)->i), I64_ID},
};
static const struct $ClosureLayout linesQ_L_9procD_layout = {
    sizeof(struct linesQ_L_9proc), ($SerializableG_class)&linesQ_L_9procG_methods, 2, linesQ_L_9procD_fields
};
void linesQ_L_9procD___serialize__(linesQ_L_9proc self, $Serial$state state) {
    $closure_serialize(self, &linesQ_L_9procD_layout, state);
}
linesQ_L_9proc linesQ_L_9procD___deserialize__(linesQ_L_9proc self, $Serial$state state) {
    return $closure_deserialize(self, &linesQ_L_9procD_layout, state);
}
linesQ_L_9proc linesQ_L_9procG_new(linesQ_Apa G_1, int64_t G_2) {
    linesQ_L_9proc $tmp = acton_malloc(sizeof(struct linesQ_L_9proc));
    $tmp->$class = &linesQ_L_9procG_methods;
    $tmp->self = G_1;
    $tmp->i = G_2;
    return $tmp;
}
struct linesQ_L_9procG_class linesQ_L_9procG_methods;
$R linesQ_L_10procD___call__($proc $closure, $Cont $continuation) {
    linesQ_L_10proc L_self = (linesQ_L_10proc)$closure;
    $Cont C_cont = ($Cont)$continuation;
    linesQ_Apa self = (linesQ_Apa)(((linesQ_L_10proc)(L_self))->ref0);
    return (($R (*) ($WORD, $Cont))((linesQ_Apa)(self))->$class->__startup__G_local)(self, C_cont);
}
$proc linesQ_L_10procD___deserialize__($proc self, $Serial$state state) {
    return $proc0_restore(&linesQ_L_10procG_methods, self, state);
}
struct $Proc0Class linesQ_L_10procG_methods;
$R linesQ_L_11procD___call__ (linesQ_L_11proc L_self, $Cont C_cont) {
    linesQ_Bepa self = ((linesQ_L_11proc)(L_self))->self;
    int64_t i = ((int64_t)((linesQ_L_11proc)(L_self))->i);
    return (($R (*) ($WORD, $Cont, int64_t))((linesQ_Bepa)(self))->$class->callbackG_local)(self, C_cont, i);
}
_Static_assert($CLOSURE_FIELD_EQ(linesQ_L_11proc, self, linesQ_L_9proc, self) && $CLOSURE_FIELD_EQ(linesQ_L_11proc, i, linesQ_L_9proc, i), "Closure capture layout mismatch");
static const struct $ClosureLayout linesQ_L_11procD_layout = {
    sizeof(struct linesQ_L_11proc), ($SerializableG_class)&linesQ_L_11procG_methods, 2, linesQ_L_9procD_fields
};
void linesQ_L_11procD___serialize__(linesQ_L_11proc self, $Serial$state state) {
    $closure_serialize(self, &linesQ_L_11procD_layout, state);
}
linesQ_L_11proc linesQ_L_11procD___deserialize__(linesQ_L_11proc self, $Serial$state state) {
    return $closure_deserialize(self, &linesQ_L_11procD_layout, state);
}
linesQ_L_11proc linesQ_L_11procG_new(linesQ_Bepa G_1, int64_t G_2) {
    linesQ_L_11proc $tmp = acton_malloc(sizeof(struct linesQ_L_11proc));
    $tmp->$class = &linesQ_L_11procG_methods;
    $tmp->self = G_1;
    $tmp->i = G_2;
    return $tmp;
}
struct linesQ_L_11procG_class linesQ_L_11procG_methods;
$R linesQ_L_12procD___call__($proc $closure, $Cont $continuation) {
    linesQ_L_12proc L_self = (linesQ_L_12proc)$closure;
    $Cont C_cont = ($Cont)$continuation;
    linesQ_Bepa self = (linesQ_Bepa)(((linesQ_L_12proc)(L_self))->ref0);
    return (($R (*) ($WORD, $Cont))((linesQ_Bepa)(self))->$class->__startup__G_local)(self, C_cont);
}
$proc linesQ_L_12procD___deserialize__($proc self, $Serial$state state) {
    return $proc0_restore(&linesQ_L_12procG_methods, self, state);
}
struct $Proc0Class linesQ_L_12procG_methods;
#line 33 "test/src/lines.act"
$R linesQ_L_13myproc (linesQ_main self, $Cont C_cont, int64_t i) {
    #line 34 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(2, actStrFromLiteral("myproc", 6, 6), toB_int(i)), actStrFromLiteral(" ", 1, 1), actStrFromLiteral("\n", 1, 1), false, false);
    #line 35 "test/src/lines.act"
    if (i == 2LL) {
        #line 36 "test/src/lines.act"
        ((B_Msg (*) ($WORD, int64_t))((B_Env)(((linesQ_main)(self))->env))->$class->exit)(((linesQ_main)(self))->env, 0LL);
    }
    return $R_CONT(C_cont, toB_int(i));
}
B_Msg linesQ_L_15actionD___asyn__($action $closure, $WORD $arg1) {
    linesQ_L_15action L_self = (linesQ_L_15action)$closure;
    B_int G_1 = (B_int)$arg1;
    linesQ_Apa L_14obj = (linesQ_Apa)(((linesQ_L_15action)(L_self))->ref0);
    return ((B_Msg)((B_Msg (*) ($WORD, int64_t))((linesQ_Apa)(L_14obj))->$class->notice)(L_14obj, ((B_int)G_1)->val));
}
$action linesQ_L_15actionD___deserialize__($action self, $Serial$state state) {
    return $action_r1_restore(&linesQ_L_15actionG_methods, self, state);
}
struct $Action1G_class linesQ_L_15actionG_methods;
B_Msg linesQ_L_17actionD___asyn__($action $closure, $WORD $arg1) {
    linesQ_L_17action L_self = (linesQ_L_17action)$closure;
    B_int G_1 = (B_int)$arg1;
    linesQ_Bepa L_16obj = (linesQ_Bepa)(((linesQ_L_17action)(L_self))->ref0);
    return ((B_Msg)((B_Msg (*) ($WORD, int64_t))((linesQ_Bepa)(L_16obj))->$class->callback)(L_16obj, ((B_int)G_1)->val));
}
$action linesQ_L_17actionD___deserialize__($action self, $Serial$state state) {
    return $action_r1_restore(&linesQ_L_17actionG_methods, self, state);
}
struct $Action1G_class linesQ_L_17actionG_methods;
$R linesQ_L_19procD___call__ (linesQ_L_19proc L_self, $Cont C_cont) {
    int64_t G_1 = ((int64_t)((linesQ_L_19proc)(L_self))->G_1);
    linesQ_main self = ((linesQ_L_19proc)(L_self))->self;
    return linesQ_L_13myproc(self, C_cont, G_1);
}
static const struct $ClosureField linesQ_L_19procD_fields[] = {
    {offsetof(struct linesQ_L_19proc, G_1), sizeof(((linesQ_L_19proc)0)->G_1), I64_ID},
    {offsetof(struct linesQ_L_19proc, self), sizeof(((linesQ_L_19proc)0)->self), $CLOSURE_REF},
};
static const struct $ClosureLayout linesQ_L_19procD_layout = {
    sizeof(struct linesQ_L_19proc), ($SerializableG_class)&linesQ_L_19procG_methods, 2, linesQ_L_19procD_fields
};
void linesQ_L_19procD___serialize__(linesQ_L_19proc self, $Serial$state state) {
    $closure_serialize(self, &linesQ_L_19procD_layout, state);
}
linesQ_L_19proc linesQ_L_19procD___deserialize__(linesQ_L_19proc self, $Serial$state state) {
    return $closure_deserialize(self, &linesQ_L_19procD_layout, state);
}
linesQ_L_19proc linesQ_L_19procG_new(int64_t G_1, linesQ_main G_2) {
    linesQ_L_19proc $tmp = acton_malloc(sizeof(struct linesQ_L_19proc));
    $tmp->$class = &linesQ_L_19procG_methods;
    $tmp->G_1 = G_1;
    $tmp->self = G_2;
    return $tmp;
}
struct linesQ_L_19procG_class linesQ_L_19procG_methods;
B_Msg linesQ_L_20actionD___asyn__($action $closure, $WORD $arg1) {
    linesQ_L_20action L_self = (linesQ_L_20action)$closure;
    B_int G_1 = (B_int)$arg1;
    linesQ_main self = (linesQ_main)(((linesQ_L_20action)(L_self))->ref0);
    return ((B_Msg)$ASYNC((($Actor)self), (($Cont)linesQ_L_19procG_new(((B_int)G_1)->val, self))));
}
$action linesQ_L_20actionD___deserialize__($action self, $Serial$state state) {
    return $action_r1_restore(&linesQ_L_20actionG_methods, self, state);
}
struct $Action1G_class linesQ_L_20actionG_methods;
$R linesQ_L_21procD___call__($proc $closure, $Cont $continuation) {
    linesQ_L_21proc L_self = (linesQ_L_21proc)$closure;
    $Cont C_cont = ($Cont)$continuation;
    linesQ_main self = (linesQ_main)(((linesQ_L_21proc)(L_self))->ref0);
    return linesQ_L_13myproc(self, C_cont, 0LL);
}
$proc linesQ_L_21procD___deserialize__($proc self, $Serial$state state) {
    return $proc0_restore(&linesQ_L_21procG_methods, self, state);
}
struct $Proc0Class linesQ_L_21procG_methods;
#line 97 "test/src/lines.act"
$R linesQ_L_22nop ($Cont C_cont) {
    #line 98 "test/src/lines.act"
    return $R_CONT(C_cont, B_None);
}
$R linesQ_L_23C_8cont ($Cont C_cont, B_NoneType C_9res) {
    return $R_CONT(C_cont, B_None);
}
$R linesQ_L_24ContD___call__($Cont $closure, $WORD $result) {
    linesQ_L_24Cont L_self = (linesQ_L_24Cont)$closure;
    B_NoneType G_1 = (B_NoneType)$result;
    $Cont C_cont = ($Cont)(((linesQ_L_24Cont)(L_self))->ref0);
    return linesQ_L_23C_8cont(C_cont, G_1);
}
$Cont linesQ_L_24ContD___deserialize__($Cont self, $Serial$state state) {
    return $cont_restore(&linesQ_L_24ContG_methods, self, state);
}
struct $ContClass linesQ_L_24ContG_methods;
$R linesQ_L_18C_6cont (linesQ_Apa a, linesQ_main self, B_Iterable W_Apa_851, B_Number W_Apa_925, $Cont C_cont, int64_t C_7res) {
    #line 43 "test/src/lines.act"
    int64_t r = C_7res;
    #line 44 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(2, actStrFromLiteral("r =", 3, 3), toB_int(r)), actStrFromLiteral(" ", 1, 1), actStrFromLiteral("\n", 1, 1), false, false);
    #line 45 "test/src/lines.act"
    ((B_Msg (*) ($WORD, $action))((linesQ_Apa)(a))->$class->compute)(a, (($action)((linesQ_L_20action)$Action_R1_new(&linesQ_L_20actionG_methods, self))));
    #line 46 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromLiteral("main", 4, 4)), actStrFromLiteral(" ", 1, 1), actStrFromLiteral("\n", 1, 1), false, false);
    #line 49 "test/src/lines.act"
    int64_t v = 0LL;
    #line 50 "test/src/lines.act"
    if (v == 0LL) {
        #line 51 "test/src/lines.act"
        ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromLiteral("if branch", 9, 9)), actStrFromLiteral(" ", 1, 1), actStrFromLiteral("\n", 1, 1), false, false);
        #line 52 "test/src/lines.act"
        if (v < 1LL) {
            #line 53 "test/src/lines.act"
            ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromLiteral("nested if", 9, 9)), actStrFromLiteral(" ", 1, 1), actStrFromLiteral("\n", 1, 1), false, false);
        }
        else if (v == -1LL) {
            #line 55 "test/src/lines.act"
            ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromLiteral("nested elif", 11, 11)), actStrFromLiteral(" ", 1, 1), actStrFromLiteral("\n", 1, 1), false, false);
        }
        else {
            #line 57 "test/src/lines.act"
            ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromLiteral("nested else", 11, 11)), actStrFromLiteral(" ", 1, 1), actStrFromLiteral("\n", 1, 1), false, false);
        }
    }
    else if (v == 1LL) {
        #line 59 "test/src/lines.act"
        ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromLiteral("outer elif", 10, 10)), actStrFromLiteral(" ", 1, 1), actStrFromLiteral("\n", 1, 1), false, false);
    }
    else {
        #line 61 "test/src/lines.act"
        ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromLiteral("outer else", 10, 10)), actStrFromLiteral(" ", 1, 1), actStrFromLiteral("\n", 1, 1), false, false);
    }
    #line 64 "test/src/lines.act"
    int64_t i = 0LL;
    #line 65 "test/src/lines.act"
    while (true) {
        if (i < 3LL) {
        }
        else {
            #line 75 "test/src/lines.act"
            ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromLiteral("while else", 10, 10)), actStrFromLiteral(" ", 1, 1), actStrFromLiteral("\n", 1, 1), false, false);
            break;
        }
        #line 66 "test/src/lines.act"
        i = (((int64_t)(i + 1LL)));
        #line 67 "test/src/lines.act"
        if (i == 1LL) {
            #line 68 "test/src/lines.act"
            ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromLiteral("continue path", 13, 13)), actStrFromLiteral(" ", 1, 1), actStrFromLiteral("\n", 1, 1), false, false);
            #line 69 "test/src/lines.act"
            continue;
        }
        #line 70 "test/src/lines.act"
        if (i == 2LL) {
            #line 71 "test/src/lines.act"
            ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromLiteral("break path", 10, 10)), actStrFromLiteral(" ", 1, 1), actStrFromLiteral("\n", 1, 1), false, false);
            #line 72 "test/src/lines.act"
            break;
        }
        #line 73 "test/src/lines.act"
        ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(2, actStrFromLiteral("loop body", 9, 9), toB_int(i)), actStrFromLiteral(" ", 1, 1), actStrFromLiteral("\n", 1, 1), false, false);
    }
    B_Iterator N_12iter = ((B_Iterator (*) ($WORD, B_list))((B_Iterable)(W_Apa_851))->$class->__iter__)(W_Apa_851, B_mk_list(3, toB_int(1LL) , toB_int(2LL) , toB_int(3LL)));
    #line 78 "test/src/lines.act"
    while (true) {
        $MaybeWord N_13maybe;
        B_Iterator N_13maybeG_next_iter = N_12iter;
        N_13maybe.just = N_13maybeG_next_iter->$class->__next__(N_13maybeG_next_iter, &N_13maybe.val);
        int64_t j;
        if (N_13maybe.just) {
            j = ((B_int)N_13maybe.val)->val;
            #line 79 "test/src/lines.act"
            if (j == 2LL) {
                #line 80 "test/src/lines.act"
                continue;
            }
            #line 81 "test/src/lines.act"
            ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(2, actStrFromLiteral("for j", 5, 5), toB_int(j)), actStrFromLiteral(" ", 1, 1), actStrFromLiteral("\n", 1, 1), false, false);
        }
        else {
            break;
        }
    }
    B_BaseException N_17xx;
    {
        struct JumpBuf $frame;
        if ($PUSHF(&$frame)) {
            {
                struct JumpBuf $frame;
                if ($PUSH(&$frame)) {
                    #line 85 "test/src/lines.act"
                    if (v == 0LL) {
                        #line 86 "test/src/lines.act"
                        $RAISE(((B_BaseException)B_ValueErrorG_new(actStrFromLiteral("boom", 4, 4))));
                        __builtin_unreachable();
                    }
                    #line 87 "test/src/lines.act"
                    ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromLiteral("unreached", 9, 9)), actStrFromLiteral(" ", 1, 1), actStrFromLiteral("\n", 1, 1), false, false);
                    $DROP();
                }
                else {
                    B_BaseException N_16x = $POP();
                    B_ValueError e;
                    if ($ISINSTANCE0(N_16x, B_ValueError)) {
                        e = ((B_ValueError)N_16x);
                        #line 89 "test/src/lines.act"
                        ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(2, actStrFromLiteral("caught", 6, 6), actStrFromLiteral("ValueError", 10, 10)), actStrFromLiteral(" ", 1, 1), actStrFromLiteral("\n", 1, 1), false, false);
                    }
                    else {
                        $RAISE(N_16x);
                        __builtin_unreachable();
                    }
                }
            }
            $RAISE(((B_BaseException)$SEQG_new()));
            __builtin_unreachable();
        }
        else {
            N_17xx = $POP();
            #line 91 "test/src/lines.act"
            ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromLiteral("finally", 7, 7)), actStrFromLiteral(" ", 1, 1), actStrFromLiteral("\n", 1, 1), false, false);
            if ($ISINSTANCE0(N_17xx, $SEQ)) {
            }
            else {
                $RAISE(N_17xx);
                __builtin_unreachable();
            }
        }
    }
    #line 94 "test/src/lines.act"
    $AFTER(toB_float(1), (($Cont)((linesQ_L_21proc)$Proc0_R1_new(&linesQ_L_21procG_methods, self))));
    return linesQ_L_22nop($SKIPRES((($Cont)((linesQ_L_24Cont)$Cont_R1_new(&linesQ_L_24ContG_methods, C_cont)))));
}
$R linesQ_L_25ContD___call__($Cont $closure, $WORD $result) {
    linesQ_L_25Cont L_self = (linesQ_L_25Cont)$closure;
    B_int G_1 = (B_int)$result;
    linesQ_Apa a = (linesQ_Apa)(((linesQ_L_25Cont)(L_self))->ref0);
    linesQ_main self = (linesQ_main)(((linesQ_L_25Cont)(L_self))->ref1);
    B_Iterable W_Apa_851 = (B_Iterable)(((linesQ_L_25Cont)(L_self))->ref2);
    B_Number W_Apa_925 = (B_Number)(((linesQ_L_25Cont)(L_self))->ref3);
    $Cont C_cont = ($Cont)(((linesQ_L_25Cont)(L_self))->ref4);
    return linesQ_L_18C_6cont(a, self, W_Apa_851, W_Apa_925, C_cont, ((B_int)G_1)->val);
}
$Cont linesQ_L_25ContD___deserialize__($Cont self, $Serial$state state) {
    return $cont_restore(&linesQ_L_25ContG_methods, self, state);
}
struct $ContClass linesQ_L_25ContG_methods;
$R linesQ_L_26procD___call__($proc $closure, $Cont $continuation) {
    linesQ_L_26proc L_self = (linesQ_L_26proc)$closure;
    $Cont C_cont = ($Cont)$continuation;
    linesQ_main self = (linesQ_main)(((linesQ_L_26proc)(L_self))->ref0);
    return (($R (*) ($WORD, $Cont))((linesQ_main)(self))->$class->__startup__G_local)(self, C_cont);
}
$proc linesQ_L_26procD___deserialize__($proc self, $Serial$state state) {
    return $proc0_restore(&linesQ_L_26procG_methods, self, state);
}
struct $Proc0Class linesQ_L_26procG_methods;
B_NoneType linesQ_ApaD___init__ (linesQ_Apa self) {
    #line 2 "test/src/lines.act"
    ((linesQ_Apa)(self))->apa = 2001LL;
    #line 6 "test/src/lines.act"
    ((linesQ_Apa)(self))->apb = 2002LL;
    #line 16 "test/src/lines.act"
    ((linesQ_Apa)(self))->y = 123LL;
    $MarkStartup(((B_Msg (*) ($WORD))((linesQ_Apa)(self))->$class->__startup__)(self));
    return B_None;
}
#line 3 "test/src/lines.act"
$R linesQ_ApaD_setupG_local (linesQ_Apa self, $Cont C_cont, $action cb) {
    #line 4 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromLiteral("setup", 5, 5)), actStrFromLiteral(" ", 1, 1), actStrFromLiteral("\n", 1, 1), false, false);
    #line 5 "test/src/lines.act"
    ((B_Msg (*) ($WORD, B_int))(($action)(cb))->$class->__asyn__)(cb, toB_int(0LL));
    return $R_CONT(C_cont, B_None);
}
#line 7 "test/src/lines.act"
$R linesQ_ApaD_computeG_local (linesQ_Apa self, $Cont C_cont, $action cb) {
    #line 8 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromLiteral("compute", 7, 7)), actStrFromLiteral(" ", 1, 1), actStrFromLiteral("\n", 1, 1), false, false);
    $action N_function = cb;
    return $AWAIT((($Cont)((linesQ_L_2Cont)$Cont_R2_new(&linesQ_L_2ContG_methods, cb, C_cont))), ((B_Msg)((B_Msg (*) ($WORD, B_int))(($action)(N_function))->$class->__asyn__)(N_function, toB_int(1LL))));
}
#line 12 "test/src/lines.act"
$R linesQ_ApaD_noticeG_local (linesQ_Apa self, $Cont C_cont, int64_t i) {
    #line 13 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromLiteral("notice", 6, 6)), actStrFromLiteral(" ", 1, 1), actStrFromLiteral("\n", 1, 1), false, false);
    int64_t N_2tmp = (((int64_t)(i + 1LL)));
    return $R_CONT(C_cont, toB_int(N_2tmp));
}
#line 18 "test/src/lines.act"
$R linesQ_ApaD___startup__G_local (linesQ_Apa self, $Cont C_cont) {
    return (($R (*) ($WORD, $Cont, $action))((linesQ_Apa)(self))->$class->setupG_local)(self, $SKIPRES((($Cont)((linesQ_L_4Cont)$Cont_R1_new(&linesQ_L_4ContG_methods, C_cont)))), (($action)((linesQ_L_6action)$Action_R1_new(&linesQ_L_6actionG_methods, self))));
}
B_Msg linesQ_ApaD_setup (linesQ_Apa self, $action cb) {
    return $ASYNC((($Actor)self), (($Cont)((linesQ_L_7proc)$Proc0_R2_new(&linesQ_L_7procG_methods, self, cb))));
}
B_Msg linesQ_ApaD_compute (linesQ_Apa self, $action cb) {
    return ((B_Msg)$ASYNC((($Actor)self), (($Cont)((linesQ_L_8proc)$Proc0_R2_new(&linesQ_L_8procG_methods, self, cb)))));
}
B_Msg linesQ_ApaD_notice (linesQ_Apa self, int64_t i) {
    return ((B_Msg)$ASYNC((($Actor)self), (($Cont)linesQ_L_9procG_new(self, i))));
}
B_Msg linesQ_ApaD___startup__ (linesQ_Apa self) {
    return $ASYNC((($Actor)self), (($Cont)((linesQ_L_10proc)$Proc0_R1_new(&linesQ_L_10procG_methods, self))));
}
void linesQ_ApaD___serialize__ (linesQ_Apa self, $Serial$state state) {
    $ActorG_methods.__serialize__(($Actor)self, state);
    $val_serialize(I64_ID, &self->apa, state);
    $val_serialize(I64_ID, &self->apb, state);
    $val_serialize(I64_ID, &self->y, state);
}
linesQ_Apa linesQ_ApaD___deserialize__ (linesQ_Apa self, $Serial$state state) {
    $WORD $tmp;
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_Apa));
            self->$class = &linesQ_ApaG_methods;
            return self;
        }
        self = $DNEW(linesQ_Apa, state);
    }
    $ActorG_methods.__deserialize__(($Actor)self, state);
    $tmp = $val_deserialize(state);
    memcpy(&self->apa, &$tmp, sizeof(self->apa));
    $tmp = $val_deserialize(state);
    memcpy(&self->apb, &$tmp, sizeof(self->apb));
    $tmp = $val_deserialize(state);
    memcpy(&self->y, &$tmp, sizeof(self->y));
    return self;
}
void linesQ_ApaD_GCfinalizer (void *obj, void *cdata) {
    linesQ_Apa self = (linesQ_Apa)obj;
    self->$class->__cleanup__(self);
}
linesQ_Apa linesQ_ApaG_new() {
    linesQ_Apa $tmp = acton_malloc(sizeof(struct linesQ_Apa));
    $tmp->$class = &linesQ_ApaG_methods;
    linesQ_ApaG_methods.__init__($tmp);
    return $tmp;
}
struct linesQ_ApaG_class linesQ_ApaG_methods;
B_NoneType linesQ_BepaD___init__ (linesQ_Bepa self) {
    $MarkStartup(((B_Msg (*) ($WORD))((linesQ_Bepa)(self))->$class->__startup__)(self));
    return B_None;
}
#line 24 "test/src/lines.act"
$R linesQ_BepaD_callbackG_local (linesQ_Bepa self, $Cont C_cont, int64_t i) {
    #line 25 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(2, actStrFromLiteral("callback", 8, 8), toB_int(i)), actStrFromLiteral(" ", 1, 1), actStrFromLiteral("\n", 1, 1), false, false);
    int64_t N_4tmp = (((int64_t)(i + 1LL)));
    return $R_CONT(C_cont, toB_int(N_4tmp));
}
#line 28 "test/src/lines.act"
$R linesQ_BepaD___startup__G_local (linesQ_Bepa self, $Cont C_cont) {
    #line 29 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromLiteral("Bepa", 4, 4)), actStrFromLiteral(" ", 1, 1), actStrFromLiteral("\n", 1, 1), false, false);
    return $R_CONT(C_cont, B_None);
}
B_Msg linesQ_BepaD_callback (linesQ_Bepa self, int64_t i) {
    return ((B_Msg)$ASYNC((($Actor)self), (($Cont)linesQ_L_11procG_new(self, i))));
}
B_Msg linesQ_BepaD___startup__ (linesQ_Bepa self) {
    return $ASYNC((($Actor)self), (($Cont)((linesQ_L_12proc)$Proc0_R1_new(&linesQ_L_12procG_methods, self))));
}
void linesQ_BepaD___serialize__ (linesQ_Bepa self, $Serial$state state) {
    $ActorG_methods.__serialize__(($Actor)self, state);
}
linesQ_Bepa linesQ_BepaD___deserialize__ (linesQ_Bepa self, $Serial$state state) {
    $WORD $tmp;
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_Bepa));
            self->$class = &linesQ_BepaG_methods;
            return self;
        }
        self = $DNEW(linesQ_Bepa, state);
    }
    $ActorG_methods.__deserialize__(($Actor)self, state);
    return self;
}
void linesQ_BepaD_GCfinalizer (void *obj, void *cdata) {
    linesQ_Bepa self = (linesQ_Bepa)obj;
    self->$class->__cleanup__(self);
}
linesQ_Bepa linesQ_BepaG_new() {
    linesQ_Bepa $tmp = acton_malloc(sizeof(struct linesQ_Bepa));
    $tmp->$class = &linesQ_BepaG_methods;
    linesQ_BepaG_methods.__init__($tmp);
    return $tmp;
}
struct linesQ_BepaG_class linesQ_BepaG_methods;
B_NoneType linesQ_mainD___init__ (linesQ_main self, B_Env env) {
    ACTON_STORE_ATTR(((linesQ_main)(self)), env, env);
    $MarkStartup(((B_Msg (*) ($WORD))((linesQ_main)(self))->$class->__startup__)(self));
    return B_None;
}
#line 32 "test/src/lines.act"
$R linesQ_mainD___startup__G_local (linesQ_main self, $Cont C_cont) {
    B_Iterable W_Apa_851 = (B_Iterable)B_SequenceD_listG_witness->W_Collection;
    B_Number W_Apa_925 = (B_Number)B_RealFloatD_floatG_witness;
    #line 38 "test/src/lines.act"
    linesQ_Apa a = linesQ_ApaG_newact();
    #line 39 "test/src/lines.act"
    linesQ_Bepa b = linesQ_BepaG_newact();
    #line 40 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromLiteral("-----", 5, 5)), actStrFromLiteral(" ", 1, 1), actStrFromLiteral("\n", 1, 1), false, false);
    #line 41 "test/src/lines.act"
    ((B_Msg (*) ($WORD, $action))((linesQ_Apa)(a))->$class->setup)(a, (($action)((linesQ_L_15action)$Action_R1_new(&linesQ_L_15actionG_methods, a))));
    #line 42 "test/src/lines.act"
    B_Msg x = ((B_Msg (*) ($WORD, $action))((linesQ_Apa)(a))->$class->compute)(a, (($action)((linesQ_L_17action)$Action_R1_new(&linesQ_L_17actionG_methods, b))));
    return $AWAIT((($Cont)((linesQ_L_25Cont)$Cont_R5_new(&linesQ_L_25ContG_methods, a, self, W_Apa_851, W_Apa_925, C_cont))), x);
}
B_Msg linesQ_mainD___startup__ (linesQ_main self) {
    return $ASYNC((($Actor)self), (($Cont)((linesQ_L_26proc)$Proc0_R1_new(&linesQ_L_26procG_methods, self))));
}
void linesQ_mainD___serialize__ (linesQ_main self, $Serial$state state) {
    $ActorG_methods.__serialize__(($Actor)self, state);
    $step_serialize(self->env, state);
}
linesQ_main linesQ_mainD___deserialize__ (linesQ_main self, $Serial$state state) {
    $WORD $tmp;
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_main));
            self->$class = &linesQ_mainG_methods;
            return self;
        }
        self = $DNEW(linesQ_main, state);
    }
    $ActorG_methods.__deserialize__(($Actor)self, state);
    ACTON_STORE_ATTR(self, env, $step_deserialize(state));
    return self;
}
void linesQ_mainD_GCfinalizer (void *obj, void *cdata) {
    linesQ_main self = (linesQ_main)obj;
    self->$class->__cleanup__(self);
}
linesQ_main linesQ_mainG_new(B_Env G_1) {
    linesQ_main $tmp = acton_malloc(sizeof(struct linesQ_main));
    $tmp->$class = &linesQ_mainG_methods;
    linesQ_mainG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct linesQ_mainG_class linesQ_mainG_methods;
linesQ_Apa linesQ_ApaG_newact () {
    $Actor N_construction_parent = $ConstructionBegin();
    linesQ_Apa G_act;
    {
        struct JumpBuf $frame;
        if ($PUSH(&$frame)) {
            G_act = $NEWACTOR(linesQ_Apa);
            $ConstructionEnter((($Actor)G_act));
            if ((void*)G_act->$class->__cleanup__ != (void*)$ActorD___cleanup__) $InstallFinalizer(G_act, linesQ_ApaD_GCfinalizer);
            ((B_NoneType (*) ($WORD))((linesQ_Apa)(G_act))->$class->__init__)(G_act);
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
linesQ_Bepa linesQ_BepaG_newact () {
    $Actor N_construction_parent = $ConstructionBegin();
    linesQ_Bepa G_act;
    {
        struct JumpBuf $frame;
        if ($PUSH(&$frame)) {
            G_act = $NEWACTOR(linesQ_Bepa);
            $ConstructionEnter((($Actor)G_act));
            if ((void*)G_act->$class->__cleanup__ != (void*)$ActorD___cleanup__) $InstallFinalizer(G_act, linesQ_BepaD_GCfinalizer);
            ((B_NoneType (*) ($WORD))((linesQ_Bepa)(G_act))->$class->__init__)(G_act);
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
linesQ_main linesQ_mainG_newact (B_Env env) {
    $Actor N_construction_parent = $ConstructionBegin();
    linesQ_main G_act;
    {
        struct JumpBuf $frame;
        if ($PUSH(&$frame)) {
            G_act = $NEWACTOR(linesQ_main);
            $ConstructionEnter((($Actor)G_act));
            if ((void*)G_act->$class->__cleanup__ != (void*)$ActorD___cleanup__) $InstallFinalizer(G_act, linesQ_mainD_GCfinalizer);
            ((B_NoneType (*) ($WORD, B_Env))((linesQ_main)(G_act))->$class->__init__)(G_act, env);
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
int linesQ_done$ = 0;
void linesQ___init__ () {
    if (linesQ_done$) return;
    linesQ_done$ = 1;
    $cont_class_init(&linesQ_L_2ContG_methods, "linesQ_L_2Cont", &$Cont_R2_shape, linesQ_L_2ContD___call__, linesQ_L_2ContD___deserialize__);
    $cont_class_init(&linesQ_L_4ContG_methods, "linesQ_L_4Cont", &$Cont_R1_shape, linesQ_L_4ContD___call__, linesQ_L_4ContD___deserialize__);
    $action1_class_init(&linesQ_L_6actionG_methods, "linesQ_L_6action", linesQ_L_6actionD___asyn__, linesQ_L_6actionD___deserialize__);
    $proc0_class_init(&linesQ_L_7procG_methods, "linesQ_L_7proc", &$Proc0_R2_shape, linesQ_L_7procD___call__, linesQ_L_7procD___deserialize__);
    $proc0_class_init(&linesQ_L_8procG_methods, "linesQ_L_8proc", &$Proc0_R2_shape, linesQ_L_8procD___call__, linesQ_L_8procD___deserialize__);
    {
        linesQ_L_9procG_methods.$GCINFO = "linesQ_L_9proc";
        linesQ_L_9procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        linesQ_L_9procG_methods.__bool__ = (bool (*) (linesQ_L_9proc))B_valueG_methods.__bool__;
        linesQ_L_9procG_methods.__str__ = (B_str (*) (linesQ_L_9proc))B_valueG_methods.__str__;
        linesQ_L_9procG_methods.__repr__ = (B_str (*) (linesQ_L_9proc))B_valueG_methods.__repr__;
        linesQ_L_9procG_methods.__init__ = NULL;
        linesQ_L_9procG_methods.__call__ = ($R (*) (linesQ_L_9proc, $Cont))linesQ_L_9procD___call__;
        linesQ_L_9procG_methods.__exec__ = ($R (*) (linesQ_L_9proc, $Cont))linesQ_L_9procD___call__;
        linesQ_L_9procG_methods.__serialize__ = linesQ_L_9procD___serialize__;
        linesQ_L_9procG_methods.__deserialize__ = linesQ_L_9procD___deserialize__;
        $register(&linesQ_L_9procG_methods);
    }
    $proc0_class_init(&linesQ_L_10procG_methods, "linesQ_L_10proc", &$Proc0_R1_shape, linesQ_L_10procD___call__, linesQ_L_10procD___deserialize__);
    {
        linesQ_L_11procG_methods.$GCINFO = "linesQ_L_11proc";
        linesQ_L_11procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        linesQ_L_11procG_methods.__bool__ = (bool (*) (linesQ_L_11proc))B_valueG_methods.__bool__;
        linesQ_L_11procG_methods.__str__ = (B_str (*) (linesQ_L_11proc))B_valueG_methods.__str__;
        linesQ_L_11procG_methods.__repr__ = (B_str (*) (linesQ_L_11proc))B_valueG_methods.__repr__;
        linesQ_L_11procG_methods.__init__ = NULL;
        linesQ_L_11procG_methods.__call__ = ($R (*) (linesQ_L_11proc, $Cont))linesQ_L_11procD___call__;
        linesQ_L_11procG_methods.__exec__ = ($R (*) (linesQ_L_11proc, $Cont))linesQ_L_11procD___call__;
        linesQ_L_11procG_methods.__serialize__ = linesQ_L_11procD___serialize__;
        linesQ_L_11procG_methods.__deserialize__ = linesQ_L_11procD___deserialize__;
        $register(&linesQ_L_11procG_methods);
    }
    $proc0_class_init(&linesQ_L_12procG_methods, "linesQ_L_12proc", &$Proc0_R1_shape, linesQ_L_12procD___call__, linesQ_L_12procD___deserialize__);
    $action1_class_init(&linesQ_L_15actionG_methods, "linesQ_L_15action", linesQ_L_15actionD___asyn__, linesQ_L_15actionD___deserialize__);
    $action1_class_init(&linesQ_L_17actionG_methods, "linesQ_L_17action", linesQ_L_17actionD___asyn__, linesQ_L_17actionD___deserialize__);
    {
        linesQ_L_19procG_methods.$GCINFO = "linesQ_L_19proc";
        linesQ_L_19procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        linesQ_L_19procG_methods.__bool__ = (bool (*) (linesQ_L_19proc))B_valueG_methods.__bool__;
        linesQ_L_19procG_methods.__str__ = (B_str (*) (linesQ_L_19proc))B_valueG_methods.__str__;
        linesQ_L_19procG_methods.__repr__ = (B_str (*) (linesQ_L_19proc))B_valueG_methods.__repr__;
        linesQ_L_19procG_methods.__init__ = NULL;
        linesQ_L_19procG_methods.__call__ = ($R (*) (linesQ_L_19proc, $Cont))linesQ_L_19procD___call__;
        linesQ_L_19procG_methods.__exec__ = ($R (*) (linesQ_L_19proc, $Cont))linesQ_L_19procD___call__;
        linesQ_L_19procG_methods.__serialize__ = linesQ_L_19procD___serialize__;
        linesQ_L_19procG_methods.__deserialize__ = linesQ_L_19procD___deserialize__;
        $register(&linesQ_L_19procG_methods);
    }
    $action1_class_init(&linesQ_L_20actionG_methods, "linesQ_L_20action", linesQ_L_20actionD___asyn__, linesQ_L_20actionD___deserialize__);
    $proc0_class_init(&linesQ_L_21procG_methods, "linesQ_L_21proc", &$Proc0_R1_shape, linesQ_L_21procD___call__, linesQ_L_21procD___deserialize__);
    $cont_class_init(&linesQ_L_24ContG_methods, "linesQ_L_24Cont", &$Cont_R1_shape, linesQ_L_24ContD___call__, linesQ_L_24ContD___deserialize__);
    $cont_class_init(&linesQ_L_25ContG_methods, "linesQ_L_25Cont", &$Cont_R5_shape, linesQ_L_25ContD___call__, linesQ_L_25ContD___deserialize__);
    $proc0_class_init(&linesQ_L_26procG_methods, "linesQ_L_26proc", &$Proc0_R1_shape, linesQ_L_26procD___call__, linesQ_L_26procD___deserialize__);
    {
        linesQ_ApaG_methods.$GCINFO = "linesQ_Apa";
        linesQ_ApaG_methods.$superclass = ($SuperG_class)&$ActorG_methods;
        linesQ_ApaG_methods.__bool__ = (bool (*) (linesQ_Apa))$ActorG_methods.__bool__;
        linesQ_ApaG_methods.__str__ = (B_str (*) (linesQ_Apa))$ActorG_methods.__str__;
        linesQ_ApaG_methods.__repr__ = (B_str (*) (linesQ_Apa))$ActorG_methods.__repr__;
        linesQ_ApaG_methods.__resume__ = (B_NoneType (*) (linesQ_Apa))$ActorG_methods.__resume__;
        linesQ_ApaG_methods.__cleanup__ = (B_NoneType (*) (linesQ_Apa))$ActorG_methods.__cleanup__;
        linesQ_ApaG_methods.__init__ = (B_NoneType (*) (linesQ_Apa))linesQ_ApaD___init__;
        linesQ_ApaG_methods.setupG_local = ($R (*) (linesQ_Apa, $Cont, $action))linesQ_ApaD_setupG_local;
        linesQ_ApaG_methods.computeG_local = ($R (*) (linesQ_Apa, $Cont, $action))linesQ_ApaD_computeG_local;
        linesQ_ApaG_methods.noticeG_local = ($R (*) (linesQ_Apa, $Cont, int64_t))linesQ_ApaD_noticeG_local;
        linesQ_ApaG_methods.__startup__G_local = ($R (*) (linesQ_Apa, $Cont))linesQ_ApaD___startup__G_local;
        linesQ_ApaG_methods.setup = (B_Msg (*) (linesQ_Apa, $action))linesQ_ApaD_setup;
        linesQ_ApaG_methods.compute = (B_Msg (*) (linesQ_Apa, $action))linesQ_ApaD_compute;
        linesQ_ApaG_methods.notice = (B_Msg (*) (linesQ_Apa, int64_t))linesQ_ApaD_notice;
        linesQ_ApaG_methods.__startup__ = (B_Msg (*) (linesQ_Apa))linesQ_ApaD___startup__;
        linesQ_ApaG_methods.__serialize__ = linesQ_ApaD___serialize__;
        linesQ_ApaG_methods.__deserialize__ = linesQ_ApaD___deserialize__;
        $register(&linesQ_ApaG_methods);
    }
    {
        linesQ_BepaG_methods.$GCINFO = "linesQ_Bepa";
        linesQ_BepaG_methods.$superclass = ($SuperG_class)&$ActorG_methods;
        linesQ_BepaG_methods.__bool__ = (bool (*) (linesQ_Bepa))$ActorG_methods.__bool__;
        linesQ_BepaG_methods.__str__ = (B_str (*) (linesQ_Bepa))$ActorG_methods.__str__;
        linesQ_BepaG_methods.__repr__ = (B_str (*) (linesQ_Bepa))$ActorG_methods.__repr__;
        linesQ_BepaG_methods.__resume__ = (B_NoneType (*) (linesQ_Bepa))$ActorG_methods.__resume__;
        linesQ_BepaG_methods.__cleanup__ = (B_NoneType (*) (linesQ_Bepa))$ActorG_methods.__cleanup__;
        linesQ_BepaG_methods.__init__ = (B_NoneType (*) (linesQ_Bepa))linesQ_BepaD___init__;
        linesQ_BepaG_methods.callbackG_local = ($R (*) (linesQ_Bepa, $Cont, int64_t))linesQ_BepaD_callbackG_local;
        linesQ_BepaG_methods.__startup__G_local = ($R (*) (linesQ_Bepa, $Cont))linesQ_BepaD___startup__G_local;
        linesQ_BepaG_methods.callback = (B_Msg (*) (linesQ_Bepa, int64_t))linesQ_BepaD_callback;
        linesQ_BepaG_methods.__startup__ = (B_Msg (*) (linesQ_Bepa))linesQ_BepaD___startup__;
        linesQ_BepaG_methods.__serialize__ = linesQ_BepaD___serialize__;
        linesQ_BepaG_methods.__deserialize__ = linesQ_BepaD___deserialize__;
        $register(&linesQ_BepaG_methods);
    }
    {
        linesQ_mainG_methods.$GCINFO = "linesQ_main";
        linesQ_mainG_methods.$superclass = ($SuperG_class)&$ActorG_methods;
        linesQ_mainG_methods.__bool__ = (bool (*) (linesQ_main))$ActorG_methods.__bool__;
        linesQ_mainG_methods.__str__ = (B_str (*) (linesQ_main))$ActorG_methods.__str__;
        linesQ_mainG_methods.__repr__ = (B_str (*) (linesQ_main))$ActorG_methods.__repr__;
        linesQ_mainG_methods.__resume__ = (B_NoneType (*) (linesQ_main))$ActorG_methods.__resume__;
        linesQ_mainG_methods.__cleanup__ = (B_NoneType (*) (linesQ_main))$ActorG_methods.__cleanup__;
        linesQ_mainG_methods.__init__ = (B_NoneType (*) (linesQ_main, B_Env))linesQ_mainD___init__;
        linesQ_mainG_methods.__startup__G_local = ($R (*) (linesQ_main, $Cont))linesQ_mainD___startup__G_local;
        linesQ_mainG_methods.__startup__ = (B_Msg (*) (linesQ_main))linesQ_mainD___startup__;
        linesQ_mainG_methods.__serialize__ = linesQ_mainD___serialize__;
        linesQ_mainG_methods.__deserialize__ = linesQ_mainD___deserialize__;
        $register(&linesQ_mainG_methods);
    }
}