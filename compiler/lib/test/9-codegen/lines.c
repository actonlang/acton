/* Acton impl hash: test-hash */
#include "rts/common.h"
#include "out/types/lines.h"
static $R linesQ_V_1closure_adapter ($action V_adapter, $Cont V_1adapter, $WORD V_2adapter) {
    return $AWAIT(V_1adapter, ((B_Msg)((B_Msg (*) ($WORD, $WORD))(($action)(V_adapter))->$class->__asyn__)(V_adapter, V_2adapter)));
}
static $R linesQ_V_2closure_adapter ($action V_adapter, $Cont V_1adapter, $WORD V_2adapter) {
    return $R_CONT(V_1adapter, ((B_value)((B_Msg (*) ($WORD, $WORD))(($action)(V_adapter))->$class->__asyn__)(V_adapter, V_2adapter)));
}
B_Eq linesQ_W_Apa_1097;
B_Eq linesQ_W_Apa_759;
B_Eq linesQ_W_Apa_331;
B_Eq linesQ_W_Apa_785;
$R linesQ_L_1C_1cont (linesQ_Apa self, $Cont C_cont, B_NoneType C_2res) {
    #line 18 "test/src/lines.act"
    ((linesQ_Apa)(self))->z = 1LL;
    #line 19 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromCStringLength("Apa", 3)), actStrFromCStringLength(" ", 1), actStrFromCStringLength("\n", 1), false, false);
    return $R_CONT(C_cont, B_None);
}
$R linesQ_L_2ContD___call__ (linesQ_L_2Cont L_self, B_NoneType G_1) {
    linesQ_Apa self = ((linesQ_L_2Cont)(L_self))->self;
    $Cont C_cont = ((linesQ_L_2Cont)(L_self))->C_cont;
    return linesQ_L_1C_1cont(self, C_cont, G_1);
}
static const struct $ClosureField linesQ_L_2ContD_fields[] = {
    {offsetof(struct linesQ_L_2Cont, self), sizeof(((linesQ_L_2Cont)0)->self), $CLOSURE_REF},
    {offsetof(struct linesQ_L_2Cont, C_cont), sizeof(((linesQ_L_2Cont)0)->C_cont), $CLOSURE_REF},
};
static const struct $ClosureLayout linesQ_L_2ContD_layout = {
    sizeof(struct linesQ_L_2Cont), ($SerializableG_class)&linesQ_L_2ContG_methods, 2, linesQ_L_2ContD_fields
};
void linesQ_L_2ContD___serialize__(linesQ_L_2Cont self, $Serial$state state) {
    $closure_serialize(self, &linesQ_L_2ContD_layout, state);
}
linesQ_L_2Cont linesQ_L_2ContD___deserialize__(linesQ_L_2Cont self, $Serial$state state) {
    return $closure_deserialize(self, &linesQ_L_2ContD_layout, state);
}
linesQ_L_2Cont linesQ_L_2ContG_new(linesQ_Apa G_1, $Cont G_2) {
    linesQ_L_2Cont $tmp = acton_malloc(sizeof(struct linesQ_L_2Cont));
    $tmp->$class = &linesQ_L_2ContG_methods;
    $tmp->self = G_1;
    $tmp->C_cont = G_2;
    return $tmp;
}
struct linesQ_L_2ContG_class linesQ_L_2ContG_methods;
B_Msg linesQ_L_4actionD___asyn__ (linesQ_L_4action L_self, B_int G_1) {
    linesQ_Apa L_3obj = ((linesQ_L_4action)(L_self))->L_3obj;
    return ((B_Msg)((B_Msg (*) ($WORD, int64_t))((linesQ_Apa)(L_3obj))->$class->notice)(L_3obj, ((B_int)G_1)->val));
}
_Static_assert(offsetof(struct linesQ_L_4actionG_class, __asyn__) == offsetof(struct $actionG_class, __asyn__) && sizeof(((linesQ_L_4action)0)->$class->__asyn__) == sizeof((($action)0)->$class->__asyn__), "Closure adapter slot mismatch");
static const struct $ClosureField linesQ_L_4actionD_fields[] = {
    {offsetof(struct linesQ_L_4action, L_3obj), sizeof(((linesQ_L_4action)0)->L_3obj), $CLOSURE_REF},
};
static const struct $ClosureLayout linesQ_L_4actionD_layout = {
    sizeof(struct linesQ_L_4action), ($SerializableG_class)&linesQ_L_4actionG_methods, 1, linesQ_L_4actionD_fields
};
void linesQ_L_4actionD___serialize__(linesQ_L_4action self, $Serial$state state) {
    $closure_serialize(self, &linesQ_L_4actionD_layout, state);
}
linesQ_L_4action linesQ_L_4actionD___deserialize__(linesQ_L_4action self, $Serial$state state) {
    return $closure_deserialize(self, &linesQ_L_4actionD_layout, state);
}
linesQ_L_4action linesQ_L_4actionG_new(linesQ_Apa G_1) {
    linesQ_L_4action $tmp = acton_malloc(sizeof(struct linesQ_L_4action));
    $tmp->$class = &linesQ_L_4actionG_methods;
    $tmp->L_3obj = G_1;
    return $tmp;
}
struct linesQ_L_4actionG_class linesQ_L_4actionG_methods;
$R linesQ_L_5C_3cont ($action cb, $Cont C_cont, int64_t C_4res) {
    #line 9 "test/src/lines.act"
    int64_t v = C_4res;
    #line 10 "test/src/lines.act"
    B_Msg m = ((B_Msg)((B_Msg (*) ($WORD, B_int))(($action)(cb))->$class->__asyn__)(cb, toB_int(2LL)));
    int64_t N_tmp = (((int64_t)(v * 10LL)));
    return $R_CONT(C_cont, toB_int(N_tmp));
}
$R linesQ_L_6ContD___call__ (linesQ_L_6Cont L_self, B_int G_1) {
    $action cb = ((linesQ_L_6Cont)(L_self))->cb;
    $Cont C_cont = ((linesQ_L_6Cont)(L_self))->C_cont;
    return linesQ_L_5C_3cont(cb, C_cont, ((B_int)G_1)->val);
}
_Static_assert($CLOSURE_FIELD_EQ(linesQ_L_6Cont, cb, linesQ_L_2Cont, self) && $CLOSURE_FIELD_EQ(linesQ_L_6Cont, C_cont, linesQ_L_2Cont, C_cont), "Closure capture layout mismatch");
static const struct $ClosureLayout linesQ_L_6ContD_layout = {
    sizeof(struct linesQ_L_6Cont), ($SerializableG_class)&linesQ_L_6ContG_methods, 2, linesQ_L_2ContD_fields
};
void linesQ_L_6ContD___serialize__(linesQ_L_6Cont self, $Serial$state state) {
    $closure_serialize(self, &linesQ_L_6ContD_layout, state);
}
linesQ_L_6Cont linesQ_L_6ContD___deserialize__(linesQ_L_6Cont self, $Serial$state state) {
    return $closure_deserialize(self, &linesQ_L_6ContD_layout, state);
}
linesQ_L_6Cont linesQ_L_6ContG_new($action G_1, $Cont G_2) {
    linesQ_L_6Cont $tmp = acton_malloc(sizeof(struct linesQ_L_6Cont));
    $tmp->$class = &linesQ_L_6ContG_methods;
    $tmp->cb = G_1;
    $tmp->C_cont = G_2;
    return $tmp;
}
struct linesQ_L_6ContG_class linesQ_L_6ContG_methods;
$R linesQ_L_7procD___call__ (linesQ_L_7proc L_self, $Cont C_cont) {
    linesQ_Apa self = ((linesQ_L_7proc)(L_self))->self;
    $action cb = ((linesQ_L_7proc)(L_self))->cb;
    return (($R (*) ($WORD, $Cont, $action))((linesQ_Apa)(self))->$class->setupG_local)(self, C_cont, cb);
}
_Static_assert($CLOSURE_FIELD_EQ(linesQ_L_7proc, self, linesQ_L_2Cont, self) && $CLOSURE_FIELD_EQ(linesQ_L_7proc, cb, linesQ_L_2Cont, C_cont), "Closure capture layout mismatch");
static const struct $ClosureLayout linesQ_L_7procD_layout = {
    sizeof(struct linesQ_L_7proc), ($SerializableG_class)&linesQ_L_7procG_methods, 2, linesQ_L_2ContD_fields
};
void linesQ_L_7procD___serialize__(linesQ_L_7proc self, $Serial$state state) {
    $closure_serialize(self, &linesQ_L_7procD_layout, state);
}
linesQ_L_7proc linesQ_L_7procD___deserialize__(linesQ_L_7proc self, $Serial$state state) {
    return $closure_deserialize(self, &linesQ_L_7procD_layout, state);
}
linesQ_L_7proc linesQ_L_7procG_new(linesQ_Apa G_1, $action G_2) {
    linesQ_L_7proc $tmp = acton_malloc(sizeof(struct linesQ_L_7proc));
    $tmp->$class = &linesQ_L_7procG_methods;
    $tmp->self = G_1;
    $tmp->cb = G_2;
    return $tmp;
}
struct linesQ_L_7procG_class linesQ_L_7procG_methods;
$R linesQ_L_8procD___call__ (linesQ_L_8proc L_self, $Cont C_cont) {
    linesQ_Apa self = ((linesQ_L_8proc)(L_self))->self;
    $action cb = ((linesQ_L_8proc)(L_self))->cb;
    return (($R (*) ($WORD, $Cont, $action))((linesQ_Apa)(self))->$class->computeG_local)(self, C_cont, cb);
}
_Static_assert($CLOSURE_FIELD_EQ(linesQ_L_8proc, self, linesQ_L_2Cont, self) && $CLOSURE_FIELD_EQ(linesQ_L_8proc, cb, linesQ_L_2Cont, C_cont), "Closure capture layout mismatch");
static const struct $ClosureLayout linesQ_L_8procD_layout = {
    sizeof(struct linesQ_L_8proc), ($SerializableG_class)&linesQ_L_8procG_methods, 2, linesQ_L_2ContD_fields
};
void linesQ_L_8procD___serialize__(linesQ_L_8proc self, $Serial$state state) {
    $closure_serialize(self, &linesQ_L_8procD_layout, state);
}
linesQ_L_8proc linesQ_L_8procD___deserialize__(linesQ_L_8proc self, $Serial$state state) {
    return $closure_deserialize(self, &linesQ_L_8procD_layout, state);
}
linesQ_L_8proc linesQ_L_8procG_new(linesQ_Apa G_1, $action G_2) {
    linesQ_L_8proc $tmp = acton_malloc(sizeof(struct linesQ_L_8proc));
    $tmp->$class = &linesQ_L_8procG_methods;
    $tmp->self = G_1;
    $tmp->cb = G_2;
    return $tmp;
}
struct linesQ_L_8procG_class linesQ_L_8procG_methods;
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
$R linesQ_L_10procD___call__ (linesQ_L_10proc L_self, $Cont C_cont) {
    linesQ_Bepa self = ((linesQ_L_10proc)(L_self))->self;
    int64_t i = ((int64_t)((linesQ_L_10proc)(L_self))->i);
    return (($R (*) ($WORD, $Cont, int64_t))((linesQ_Bepa)(self))->$class->callbackG_local)(self, C_cont, i);
}
_Static_assert($CLOSURE_FIELD_EQ(linesQ_L_10proc, self, linesQ_L_9proc, self) && $CLOSURE_FIELD_EQ(linesQ_L_10proc, i, linesQ_L_9proc, i), "Closure capture layout mismatch");
static const struct $ClosureLayout linesQ_L_10procD_layout = {
    sizeof(struct linesQ_L_10proc), ($SerializableG_class)&linesQ_L_10procG_methods, 2, linesQ_L_9procD_fields
};
void linesQ_L_10procD___serialize__(linesQ_L_10proc self, $Serial$state state) {
    $closure_serialize(self, &linesQ_L_10procD_layout, state);
}
linesQ_L_10proc linesQ_L_10procD___deserialize__(linesQ_L_10proc self, $Serial$state state) {
    return $closure_deserialize(self, &linesQ_L_10procD_layout, state);
}
linesQ_L_10proc linesQ_L_10procG_new(linesQ_Bepa G_1, int64_t G_2) {
    linesQ_L_10proc $tmp = acton_malloc(sizeof(struct linesQ_L_10proc));
    $tmp->$class = &linesQ_L_10procG_methods;
    $tmp->self = G_1;
    $tmp->i = G_2;
    return $tmp;
}
struct linesQ_L_10procG_class linesQ_L_10procG_methods;
B_Msg linesQ_L_14actionD___asyn__ (linesQ_L_14action L_self, B_int G_1) {
    linesQ_Apa L_13obj = ((linesQ_L_14action)(L_self))->L_13obj;
    return ((B_Msg)((B_Msg (*) ($WORD, int64_t))((linesQ_Apa)(L_13obj))->$class->notice)(L_13obj, ((B_int)G_1)->val));
}
_Static_assert(offsetof(struct linesQ_L_14actionG_class, __asyn__) == offsetof(struct $actionG_class, __asyn__) && sizeof(((linesQ_L_14action)0)->$class->__asyn__) == sizeof((($action)0)->$class->__asyn__), "Closure adapter slot mismatch");
_Static_assert($CLOSURE_FIELD_EQ(linesQ_L_14action, L_13obj, linesQ_L_4action, L_3obj), "Closure capture layout mismatch");
static const struct $ClosureLayout linesQ_L_14actionD_layout = {
    sizeof(struct linesQ_L_14action), ($SerializableG_class)&linesQ_L_14actionG_methods, 1, linesQ_L_4actionD_fields
};
void linesQ_L_14actionD___serialize__(linesQ_L_14action self, $Serial$state state) {
    $closure_serialize(self, &linesQ_L_14actionD_layout, state);
}
linesQ_L_14action linesQ_L_14actionD___deserialize__(linesQ_L_14action self, $Serial$state state) {
    return $closure_deserialize(self, &linesQ_L_14actionD_layout, state);
}
linesQ_L_14action linesQ_L_14actionG_new(linesQ_Apa G_1) {
    linesQ_L_14action $tmp = acton_malloc(sizeof(struct linesQ_L_14action));
    $tmp->$class = &linesQ_L_14actionG_methods;
    $tmp->L_13obj = G_1;
    return $tmp;
}
struct linesQ_L_14actionG_class linesQ_L_14actionG_methods;
B_Msg linesQ_L_16actionD___asyn__ (linesQ_L_16action L_self, B_int G_1) {
    linesQ_Bepa L_15obj = ((linesQ_L_16action)(L_self))->L_15obj;
    return ((B_Msg)((B_Msg (*) ($WORD, int64_t))((linesQ_Bepa)(L_15obj))->$class->callback)(L_15obj, ((B_int)G_1)->val));
}
_Static_assert(offsetof(struct linesQ_L_16actionG_class, __asyn__) == offsetof(struct $actionG_class, __asyn__) && sizeof(((linesQ_L_16action)0)->$class->__asyn__) == sizeof((($action)0)->$class->__asyn__), "Closure adapter slot mismatch");
_Static_assert($CLOSURE_FIELD_EQ(linesQ_L_16action, L_15obj, linesQ_L_4action, L_3obj), "Closure capture layout mismatch");
static const struct $ClosureLayout linesQ_L_16actionD_layout = {
    sizeof(struct linesQ_L_16action), ($SerializableG_class)&linesQ_L_16actionG_methods, 1, linesQ_L_4actionD_fields
};
void linesQ_L_16actionD___serialize__(linesQ_L_16action self, $Serial$state state) {
    $closure_serialize(self, &linesQ_L_16actionD_layout, state);
}
linesQ_L_16action linesQ_L_16actionD___deserialize__(linesQ_L_16action self, $Serial$state state) {
    return $closure_deserialize(self, &linesQ_L_16actionD_layout, state);
}
linesQ_L_16action linesQ_L_16actionG_new(linesQ_Bepa G_1) {
    linesQ_L_16action $tmp = acton_malloc(sizeof(struct linesQ_L_16action));
    $tmp->$class = &linesQ_L_16actionG_methods;
    $tmp->L_15obj = G_1;
    return $tmp;
}
struct linesQ_L_16actionG_class linesQ_L_16actionG_methods;
B_Msg linesQ_L_19actionD___asyn__ (linesQ_L_19action L_self, B_int G_1) {
    linesQ_main L_18obj = ((linesQ_L_19action)(L_self))->L_18obj;
    return ((B_Msg)((B_Msg (*) ($WORD, int64_t))((linesQ_main)(L_18obj))->$class->myproc)(L_18obj, ((B_int)G_1)->val));
}
_Static_assert(offsetof(struct linesQ_L_19actionG_class, __asyn__) == offsetof(struct $actionG_class, __asyn__) && sizeof(((linesQ_L_19action)0)->$class->__asyn__) == sizeof((($action)0)->$class->__asyn__), "Closure adapter slot mismatch");
_Static_assert($CLOSURE_FIELD_EQ(linesQ_L_19action, L_18obj, linesQ_L_4action, L_3obj), "Closure capture layout mismatch");
static const struct $ClosureLayout linesQ_L_19actionD_layout = {
    sizeof(struct linesQ_L_19action), ($SerializableG_class)&linesQ_L_19actionG_methods, 1, linesQ_L_4actionD_fields
};
void linesQ_L_19actionD___serialize__(linesQ_L_19action self, $Serial$state state) {
    $closure_serialize(self, &linesQ_L_19actionD_layout, state);
}
linesQ_L_19action linesQ_L_19actionD___deserialize__(linesQ_L_19action self, $Serial$state state) {
    return $closure_deserialize(self, &linesQ_L_19actionD_layout, state);
}
linesQ_L_19action linesQ_L_19actionG_new(linesQ_main G_1) {
    linesQ_L_19action $tmp = acton_malloc(sizeof(struct linesQ_L_19action));
    $tmp->$class = &linesQ_L_19actionG_methods;
    $tmp->L_18obj = G_1;
    return $tmp;
}
struct linesQ_L_19actionG_class linesQ_L_19actionG_methods;
$R linesQ_L_20procD___call__ (linesQ_L_20proc L_self, $Cont C_cont) {
    linesQ_main self = ((linesQ_L_20proc)(L_self))->self;
    return (($R (*) ($WORD, $Cont, int64_t))((linesQ_main)(self))->$class->myprocG_local)(self, C_cont, 0LL);
}
_Static_assert($CLOSURE_FIELD_EQ(linesQ_L_20proc, self, linesQ_L_4action, L_3obj), "Closure capture layout mismatch");
static const struct $ClosureLayout linesQ_L_20procD_layout = {
    sizeof(struct linesQ_L_20proc), ($SerializableG_class)&linesQ_L_20procG_methods, 1, linesQ_L_4actionD_fields
};
void linesQ_L_20procD___serialize__(linesQ_L_20proc self, $Serial$state state) {
    $closure_serialize(self, &linesQ_L_20procD_layout, state);
}
linesQ_L_20proc linesQ_L_20procD___deserialize__(linesQ_L_20proc self, $Serial$state state) {
    return $closure_deserialize(self, &linesQ_L_20procD_layout, state);
}
linesQ_L_20proc linesQ_L_20procG_new(linesQ_main G_1) {
    linesQ_L_20proc $tmp = acton_malloc(sizeof(struct linesQ_L_20proc));
    $tmp->$class = &linesQ_L_20procG_methods;
    $tmp->self = G_1;
    return $tmp;
}
struct linesQ_L_20procG_class linesQ_L_20procG_methods;
$R linesQ_L_17C_9cont (linesQ_main self, $Cont C_cont, int64_t C_10res) {
    #line 38 "test/src/lines.act"
    ((linesQ_main)(self))->r = C_10res;
    #line 39 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(2, actStrFromCStringLength("r =", 3), toB_int(((int64_t)((linesQ_main)(self))->r))), actStrFromCStringLength(" ", 1), actStrFromCStringLength("\n", 1), false, false);
    #line 40 "test/src/lines.act"
    ((B_Msg (*) ($WORD, $action))((linesQ_Apa)(((linesQ_main)(self))->a))->$class->compute)(((linesQ_main)(self))->a, (($action)linesQ_L_19actionG_new(self)));
    #line 41 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromCStringLength("main", 4)), actStrFromCStringLength(" ", 1), actStrFromCStringLength("\n", 1), false, false);
    #line 44 "test/src/lines.act"
    ((linesQ_main)(self))->v = 0LL;
    #line 45 "test/src/lines.act"
    if (((int64_t)((linesQ_main)(self))->v) == 0LL) {
        #line 46 "test/src/lines.act"
        ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromCStringLength("if branch", 9)), actStrFromCStringLength(" ", 1), actStrFromCStringLength("\n", 1), false, false);
        #line 47 "test/src/lines.act"
        if (((int64_t)((linesQ_main)(self))->v) < 1LL) {
            #line 48 "test/src/lines.act"
            ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromCStringLength("nested if", 9)), actStrFromCStringLength(" ", 1), actStrFromCStringLength("\n", 1), false, false);
        }
        else if (((int64_t)((linesQ_main)(self))->v) == -1LL) {
            #line 50 "test/src/lines.act"
            ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromCStringLength("nested elif", 11)), actStrFromCStringLength(" ", 1), actStrFromCStringLength("\n", 1), false, false);
        }
        else {
            #line 52 "test/src/lines.act"
            ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromCStringLength("nested else", 11)), actStrFromCStringLength(" ", 1), actStrFromCStringLength("\n", 1), false, false);
        }
    }
    else if (((int64_t)((linesQ_main)(self))->v) == 1LL) {
        #line 54 "test/src/lines.act"
        ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromCStringLength("outer elif", 10)), actStrFromCStringLength(" ", 1), actStrFromCStringLength("\n", 1), false, false);
    }
    else {
        #line 56 "test/src/lines.act"
        ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromCStringLength("outer else", 10)), actStrFromCStringLength(" ", 1), actStrFromCStringLength("\n", 1), false, false);
    }
    #line 59 "test/src/lines.act"
    ((linesQ_main)(self))->i = 0LL;
    #line 60 "test/src/lines.act"
    while (true) {
        if (((int64_t)((linesQ_main)(self))->i) < 3LL) {
        }
        else {
            #line 70 "test/src/lines.act"
            ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromCStringLength("while else", 10)), actStrFromCStringLength(" ", 1), actStrFromCStringLength("\n", 1), false, false);
            break;
        }
        #line 61 "test/src/lines.act"
        ((linesQ_main)(self))->i = (((int64_t)(((int64_t)((linesQ_main)(self))->i) + 1LL)));
        #line 62 "test/src/lines.act"
        if (((int64_t)((linesQ_main)(self))->i) == 1LL) {
            #line 63 "test/src/lines.act"
            ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromCStringLength("continue path", 13)), actStrFromCStringLength(" ", 1), actStrFromCStringLength("\n", 1), false, false);
            #line 64 "test/src/lines.act"
            continue;
        }
        #line 65 "test/src/lines.act"
        if (((int64_t)((linesQ_main)(self))->i) == 2LL) {
            #line 66 "test/src/lines.act"
            ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromCStringLength("break path", 10)), actStrFromCStringLength(" ", 1), actStrFromCStringLength("\n", 1), false, false);
            #line 67 "test/src/lines.act"
            break;
        }
        #line 68 "test/src/lines.act"
        ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(2, actStrFromCStringLength("loop body", 9), toB_int(((int64_t)((linesQ_main)(self))->i))), actStrFromCStringLength(" ", 1), actStrFromCStringLength("\n", 1), false, false);
    }
    B_Iterator N_3iter = B_CollectionD_SequenceD_listD___iter__((B_CollectionD_SequenceD_list)B_SequenceD_listG_witness->W_Collection, B_mk_list(3, toB_int(1LL) , toB_int(2LL) , toB_int(3LL)));
    #line 73 "test/src/lines.act"
    while (true) {
        $MaybeWord N_4maybe;
        B_Iterator N_4maybeG_next_iter = N_3iter;
        N_4maybe.just = N_4maybeG_next_iter->$class->__next__(N_4maybeG_next_iter, &N_4maybe.val);
        B_int j;
        if (N_4maybe.just) {
            j = N_4maybe.val;
            #line 74 "test/src/lines.act"
            if (((bool (*) ($WORD, B_int, B_int))((B_Eq)(linesQ_W_Apa_759))->$class->__eq__)(linesQ_W_Apa_759, j, toB_int(2LL))) {
                #line 75 "test/src/lines.act"
                continue;
            }
            #line 76 "test/src/lines.act"
            ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(2, actStrFromCStringLength("for j", 5), j), actStrFromCStringLength(" ", 1), actStrFromCStringLength("\n", 1), false, false);
        }
        else {
            break;
        }
    }
    B_BaseException N_8xx;
    {
        struct JumpBuf $frame;
        if ($PUSHF(&$frame)) {
            {
                struct JumpBuf $frame;
                if ($PUSH(&$frame)) {
                    #line 80 "test/src/lines.act"
                    if (((bool (*) ($WORD, B_int, B_int))((B_Eq)(linesQ_W_Apa_785))->$class->__eq__)(linesQ_W_Apa_785, toB_int(((int64_t)((linesQ_main)(self))->v)), toB_int(0LL))) {
                        #line 81 "test/src/lines.act"
                        $RAISE(((B_BaseException)B_ValueErrorG_new(actStrFromCStringLength("boom", 4))));
                        __builtin_unreachable();
                    }
                    #line 82 "test/src/lines.act"
                    ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromCStringLength("unreached", 9)), actStrFromCStringLength(" ", 1), actStrFromCStringLength("\n", 1), false, false);
                    $DROP();
                }
                else {
                    B_BaseException N_7x = $POP();
                    B_ValueError e;
                    if ($ISINSTANCE0(N_7x, B_ValueError)) {
                        e = ((B_ValueError)N_7x);
                        #line 84 "test/src/lines.act"
                        ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(2, actStrFromCStringLength("caught", 6), actStrFromCStringLength("ValueError", 10)), actStrFromCStringLength(" ", 1), actStrFromCStringLength("\n", 1), false, false);
                    }
                    else {
                        $RAISE(N_7x);
                        __builtin_unreachable();
                    }
                }
            }
            $RAISE(((B_BaseException)$SEQG_new()));
            __builtin_unreachable();
        }
        else {
            N_8xx = $POP();
            #line 86 "test/src/lines.act"
            ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromCStringLength("finally", 7)), actStrFromCStringLength(" ", 1), actStrFromCStringLength("\n", 1), false, false);
            if ($ISINSTANCE0(N_8xx, $SEQ)) {
            }
            else {
                $RAISE(N_8xx);
                __builtin_unreachable();
            }
        }
    }
    #line 89 "test/src/lines.act"
    $AFTER(toB_float(1), (($Cont)linesQ_L_20procG_new(self)));
    return (($R (*) ($WORD, $Cont))((linesQ_main)(self))->$class->nopG_local)(self, $SKIPRES(C_cont));
}
$R linesQ_L_21ContD___call__ (linesQ_L_21Cont L_self, B_int G_1) {
    linesQ_main self = ((linesQ_L_21Cont)(L_self))->self;
    $Cont C_cont = ((linesQ_L_21Cont)(L_self))->C_cont;
    return linesQ_L_17C_9cont(self, C_cont, ((B_int)G_1)->val);
}
_Static_assert($CLOSURE_FIELD_EQ(linesQ_L_21Cont, self, linesQ_L_2Cont, self) && $CLOSURE_FIELD_EQ(linesQ_L_21Cont, C_cont, linesQ_L_2Cont, C_cont), "Closure capture layout mismatch");
static const struct $ClosureLayout linesQ_L_21ContD_layout = {
    sizeof(struct linesQ_L_21Cont), ($SerializableG_class)&linesQ_L_21ContG_methods, 2, linesQ_L_2ContD_fields
};
void linesQ_L_21ContD___serialize__(linesQ_L_21Cont self, $Serial$state state) {
    $closure_serialize(self, &linesQ_L_21ContD_layout, state);
}
linesQ_L_21Cont linesQ_L_21ContD___deserialize__(linesQ_L_21Cont self, $Serial$state state) {
    return $closure_deserialize(self, &linesQ_L_21ContD_layout, state);
}
linesQ_L_21Cont linesQ_L_21ContG_new(linesQ_main G_1, $Cont G_2) {
    linesQ_L_21Cont $tmp = acton_malloc(sizeof(struct linesQ_L_21Cont));
    $tmp->$class = &linesQ_L_21ContG_methods;
    $tmp->self = G_1;
    $tmp->C_cont = G_2;
    return $tmp;
}
struct linesQ_L_21ContG_class linesQ_L_21ContG_methods;
$R linesQ_L_12C_7cont (linesQ_main self, $Cont C_cont, linesQ_Bepa C_8res) {
    #line 34 "test/src/lines.act"
    ((linesQ_main)(self))->b = C_8res;
    #line 35 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromCStringLength("-----", 5)), actStrFromCStringLength(" ", 1), actStrFromCStringLength("\n", 1), false, false);
    #line 36 "test/src/lines.act"
    ((B_Msg (*) ($WORD, $action))((linesQ_Apa)(((linesQ_main)(self))->a))->$class->setup)(((linesQ_main)(self))->a, (($action)linesQ_L_14actionG_new(((linesQ_main)(self))->a)));
    #line 37 "test/src/lines.act"
    ((linesQ_main)(self))->x = ((B_Msg (*) ($WORD, $action))((linesQ_Apa)(((linesQ_main)(self))->a))->$class->compute)(((linesQ_main)(self))->a, (($action)linesQ_L_16actionG_new(((linesQ_main)(self))->b)));
    return $AWAIT((($Cont)linesQ_L_21ContG_new(self, C_cont)), ((linesQ_main)(self))->x);
}
$R linesQ_L_22ContD___call__ (linesQ_L_22Cont L_self, linesQ_Bepa G_1) {
    linesQ_main self = ((linesQ_L_22Cont)(L_self))->self;
    $Cont C_cont = ((linesQ_L_22Cont)(L_self))->C_cont;
    return linesQ_L_12C_7cont(self, C_cont, G_1);
}
_Static_assert($CLOSURE_FIELD_EQ(linesQ_L_22Cont, self, linesQ_L_2Cont, self) && $CLOSURE_FIELD_EQ(linesQ_L_22Cont, C_cont, linesQ_L_2Cont, C_cont), "Closure capture layout mismatch");
static const struct $ClosureLayout linesQ_L_22ContD_layout = {
    sizeof(struct linesQ_L_22Cont), ($SerializableG_class)&linesQ_L_22ContG_methods, 2, linesQ_L_2ContD_fields
};
void linesQ_L_22ContD___serialize__(linesQ_L_22Cont self, $Serial$state state) {
    $closure_serialize(self, &linesQ_L_22ContD_layout, state);
}
linesQ_L_22Cont linesQ_L_22ContD___deserialize__(linesQ_L_22Cont self, $Serial$state state) {
    return $closure_deserialize(self, &linesQ_L_22ContD_layout, state);
}
linesQ_L_22Cont linesQ_L_22ContG_new(linesQ_main G_1, $Cont G_2) {
    linesQ_L_22Cont $tmp = acton_malloc(sizeof(struct linesQ_L_22Cont));
    $tmp->$class = &linesQ_L_22ContG_methods;
    $tmp->self = G_1;
    $tmp->C_cont = G_2;
    return $tmp;
}
struct linesQ_L_22ContG_class linesQ_L_22ContG_methods;
$R linesQ_L_11C_5cont (linesQ_main self, $Cont C_cont, linesQ_Apa C_6res) {
    #line 33 "test/src/lines.act"
    ((linesQ_main)(self))->a = C_6res;
    return linesQ_BepaG_newact((($Cont)linesQ_L_22ContG_new(self, C_cont)));
}
$R linesQ_L_23ContD___call__ (linesQ_L_23Cont L_self, linesQ_Apa G_1) {
    linesQ_main self = ((linesQ_L_23Cont)(L_self))->self;
    $Cont C_cont = ((linesQ_L_23Cont)(L_self))->C_cont;
    return linesQ_L_11C_5cont(self, C_cont, G_1);
}
_Static_assert($CLOSURE_FIELD_EQ(linesQ_L_23Cont, self, linesQ_L_2Cont, self) && $CLOSURE_FIELD_EQ(linesQ_L_23Cont, C_cont, linesQ_L_2Cont, C_cont), "Closure capture layout mismatch");
static const struct $ClosureLayout linesQ_L_23ContD_layout = {
    sizeof(struct linesQ_L_23Cont), ($SerializableG_class)&linesQ_L_23ContG_methods, 2, linesQ_L_2ContD_fields
};
void linesQ_L_23ContD___serialize__(linesQ_L_23Cont self, $Serial$state state) {
    $closure_serialize(self, &linesQ_L_23ContD_layout, state);
}
linesQ_L_23Cont linesQ_L_23ContD___deserialize__(linesQ_L_23Cont self, $Serial$state state) {
    return $closure_deserialize(self, &linesQ_L_23ContD_layout, state);
}
linesQ_L_23Cont linesQ_L_23ContG_new(linesQ_main G_1, $Cont G_2) {
    linesQ_L_23Cont $tmp = acton_malloc(sizeof(struct linesQ_L_23Cont));
    $tmp->$class = &linesQ_L_23ContG_methods;
    $tmp->self = G_1;
    $tmp->C_cont = G_2;
    return $tmp;
}
struct linesQ_L_23ContG_class linesQ_L_23ContG_methods;
$R linesQ_L_24procD___call__ (linesQ_L_24proc L_self, $Cont C_cont) {
    linesQ_main self = ((linesQ_L_24proc)(L_self))->self;
    int64_t i = ((int64_t)((linesQ_L_24proc)(L_self))->i);
    return (($R (*) ($WORD, $Cont, int64_t))((linesQ_main)(self))->$class->myprocG_local)(self, C_cont, i);
}
_Static_assert($CLOSURE_FIELD_EQ(linesQ_L_24proc, self, linesQ_L_9proc, self) && $CLOSURE_FIELD_EQ(linesQ_L_24proc, i, linesQ_L_9proc, i), "Closure capture layout mismatch");
static const struct $ClosureLayout linesQ_L_24procD_layout = {
    sizeof(struct linesQ_L_24proc), ($SerializableG_class)&linesQ_L_24procG_methods, 2, linesQ_L_9procD_fields
};
void linesQ_L_24procD___serialize__(linesQ_L_24proc self, $Serial$state state) {
    $closure_serialize(self, &linesQ_L_24procD_layout, state);
}
linesQ_L_24proc linesQ_L_24procD___deserialize__(linesQ_L_24proc self, $Serial$state state) {
    return $closure_deserialize(self, &linesQ_L_24procD_layout, state);
}
linesQ_L_24proc linesQ_L_24procG_new(linesQ_main G_1, int64_t G_2) {
    linesQ_L_24proc $tmp = acton_malloc(sizeof(struct linesQ_L_24proc));
    $tmp->$class = &linesQ_L_24procG_methods;
    $tmp->self = G_1;
    $tmp->i = G_2;
    return $tmp;
}
struct linesQ_L_24procG_class linesQ_L_24procG_methods;
$R linesQ_L_25procD___call__ (linesQ_L_25proc L_self, $Cont C_cont) {
    linesQ_main self = ((linesQ_L_25proc)(L_self))->self;
    return (($R (*) ($WORD, $Cont))((linesQ_main)(self))->$class->nopG_local)(self, C_cont);
}
_Static_assert($CLOSURE_FIELD_EQ(linesQ_L_25proc, self, linesQ_L_4action, L_3obj), "Closure capture layout mismatch");
static const struct $ClosureLayout linesQ_L_25procD_layout = {
    sizeof(struct linesQ_L_25proc), ($SerializableG_class)&linesQ_L_25procG_methods, 1, linesQ_L_4actionD_fields
};
void linesQ_L_25procD___serialize__(linesQ_L_25proc self, $Serial$state state) {
    $closure_serialize(self, &linesQ_L_25procD_layout, state);
}
linesQ_L_25proc linesQ_L_25procD___deserialize__(linesQ_L_25proc self, $Serial$state state) {
    return $closure_deserialize(self, &linesQ_L_25procD_layout, state);
}
linesQ_L_25proc linesQ_L_25procG_new(linesQ_main G_1) {
    linesQ_L_25proc $tmp = acton_malloc(sizeof(struct linesQ_L_25proc));
    $tmp->$class = &linesQ_L_25procG_methods;
    $tmp->self = G_1;
    return $tmp;
}
struct linesQ_L_25procG_class linesQ_L_25procG_methods;
$R linesQ_L_26C_11cont ($Cont C_cont, linesQ_Apa G_act, B_NoneType C_12res) {
    return $R_CONT(C_cont, G_act);
}
$R linesQ_L_27procD___call__ (linesQ_L_27proc L_self, $Cont C_cont) {
    linesQ_Apa G_act = ((linesQ_L_27proc)(L_self))->G_act;
    return (($R (*) ($WORD, $Cont))((linesQ_Apa)(G_act))->$class->__init__)(G_act, C_cont);
}
_Static_assert($CLOSURE_FIELD_EQ(linesQ_L_27proc, G_act, linesQ_L_4action, L_3obj), "Closure capture layout mismatch");
static const struct $ClosureLayout linesQ_L_27procD_layout = {
    sizeof(struct linesQ_L_27proc), ($SerializableG_class)&linesQ_L_27procG_methods, 1, linesQ_L_4actionD_fields
};
void linesQ_L_27procD___serialize__(linesQ_L_27proc self, $Serial$state state) {
    $closure_serialize(self, &linesQ_L_27procD_layout, state);
}
linesQ_L_27proc linesQ_L_27procD___deserialize__(linesQ_L_27proc self, $Serial$state state) {
    return $closure_deserialize(self, &linesQ_L_27procD_layout, state);
}
linesQ_L_27proc linesQ_L_27procG_new(linesQ_Apa G_1) {
    linesQ_L_27proc $tmp = acton_malloc(sizeof(struct linesQ_L_27proc));
    $tmp->$class = &linesQ_L_27procG_methods;
    $tmp->G_act = G_1;
    return $tmp;
}
struct linesQ_L_27procG_class linesQ_L_27procG_methods;
$R linesQ_L_28C_13cont ($Cont C_cont, linesQ_Bepa G_act, B_NoneType C_14res) {
    return $R_CONT(C_cont, G_act);
}
$R linesQ_L_29procD___call__ (linesQ_L_29proc L_self, $Cont C_cont) {
    linesQ_Bepa G_act = ((linesQ_L_29proc)(L_self))->G_act;
    return (($R (*) ($WORD, $Cont))((linesQ_Bepa)(G_act))->$class->__init__)(G_act, C_cont);
}
_Static_assert($CLOSURE_FIELD_EQ(linesQ_L_29proc, G_act, linesQ_L_4action, L_3obj), "Closure capture layout mismatch");
static const struct $ClosureLayout linesQ_L_29procD_layout = {
    sizeof(struct linesQ_L_29proc), ($SerializableG_class)&linesQ_L_29procG_methods, 1, linesQ_L_4actionD_fields
};
void linesQ_L_29procD___serialize__(linesQ_L_29proc self, $Serial$state state) {
    $closure_serialize(self, &linesQ_L_29procD_layout, state);
}
linesQ_L_29proc linesQ_L_29procD___deserialize__(linesQ_L_29proc self, $Serial$state state) {
    return $closure_deserialize(self, &linesQ_L_29procD_layout, state);
}
linesQ_L_29proc linesQ_L_29procG_new(linesQ_Bepa G_1) {
    linesQ_L_29proc $tmp = acton_malloc(sizeof(struct linesQ_L_29proc));
    $tmp->$class = &linesQ_L_29procG_methods;
    $tmp->G_act = G_1;
    return $tmp;
}
struct linesQ_L_29procG_class linesQ_L_29procG_methods;
$R linesQ_L_30C_15cont ($Cont C_cont, linesQ_main G_act, B_NoneType C_16res) {
    return $R_CONT(C_cont, G_act);
}
$R linesQ_L_31procD___call__ (linesQ_L_31proc L_self, $Cont C_cont) {
    linesQ_main G_act = ((linesQ_L_31proc)(L_self))->G_act;
    B_Env env = ((linesQ_L_31proc)(L_self))->env;
    return (($R (*) ($WORD, $Cont, B_Env))((linesQ_main)(G_act))->$class->__init__)(G_act, C_cont, env);
}
_Static_assert($CLOSURE_FIELD_EQ(linesQ_L_31proc, G_act, linesQ_L_2Cont, self) && $CLOSURE_FIELD_EQ(linesQ_L_31proc, env, linesQ_L_2Cont, C_cont), "Closure capture layout mismatch");
static const struct $ClosureLayout linesQ_L_31procD_layout = {
    sizeof(struct linesQ_L_31proc), ($SerializableG_class)&linesQ_L_31procG_methods, 2, linesQ_L_2ContD_fields
};
void linesQ_L_31procD___serialize__(linesQ_L_31proc self, $Serial$state state) {
    $closure_serialize(self, &linesQ_L_31procD_layout, state);
}
linesQ_L_31proc linesQ_L_31procD___deserialize__(linesQ_L_31proc self, $Serial$state state) {
    return $closure_deserialize(self, &linesQ_L_31procD_layout, state);
}
linesQ_L_31proc linesQ_L_31procG_new(linesQ_main G_1, B_Env G_2) {
    linesQ_L_31proc $tmp = acton_malloc(sizeof(struct linesQ_L_31proc));
    $tmp->$class = &linesQ_L_31procG_methods;
    $tmp->G_act = G_1;
    $tmp->env = G_2;
    return $tmp;
}
struct linesQ_L_31procG_class linesQ_L_31procG_methods;
$R linesQ_ApaD___init__ (linesQ_Apa self, $Cont C_cont) {
    #line 2 "test/src/lines.act"
    ((linesQ_Apa)(self))->apa = 2001LL;
    #line 6 "test/src/lines.act"
    ((linesQ_Apa)(self))->apb = 2002LL;
    #line 16 "test/src/lines.act"
    ((linesQ_Apa)(self))->y = 123LL;
    return (($R (*) ($WORD, $Cont, $action))((linesQ_Apa)(self))->$class->setupG_local)(self, $SKIPRES((($Cont)linesQ_L_2ContG_new(self, C_cont))), (($action)linesQ_L_4actionG_new(self)));
}
#line 3 "test/src/lines.act"
$R linesQ_ApaD_setupG_local (linesQ_Apa self, $Cont C_cont, $action cb) {
    #line 4 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromCStringLength("setup", 5)), actStrFromCStringLength(" ", 1), actStrFromCStringLength("\n", 1), false, false);
    #line 5 "test/src/lines.act"
    ((B_Msg (*) ($WORD, B_int))(($action)(cb))->$class->__asyn__)(cb, toB_int(0LL));
    return $R_CONT(C_cont, B_None);
}
#line 7 "test/src/lines.act"
$R linesQ_ApaD_computeG_local (linesQ_Apa self, $Cont C_cont, $action cb) {
    #line 8 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromCStringLength("compute", 7)), actStrFromCStringLength(" ", 1), actStrFromCStringLength("\n", 1), false, false);
    return $AWAIT((($Cont)linesQ_L_6ContG_new(cb, C_cont)), ((B_Msg)((B_Msg (*) ($WORD, B_int))(($action)(cb))->$class->__asyn__)(cb, toB_int(1LL))));
}
#line 12 "test/src/lines.act"
$R linesQ_ApaD_noticeG_local (linesQ_Apa self, $Cont C_cont, int64_t i) {
    #line 13 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromCStringLength("notice", 6)), actStrFromCStringLength(" ", 1), actStrFromCStringLength("\n", 1), false, false);
    int64_t N_1tmp = (((int64_t)(i + 1LL)));
    return $R_CONT(C_cont, toB_int(N_1tmp));
}
B_Msg linesQ_ApaD_setup (linesQ_Apa self, $action cb) {
    return $ASYNC((($Actor)self), (($Cont)linesQ_L_7procG_new(self, cb)));
}
B_Msg linesQ_ApaD_compute (linesQ_Apa self, $action cb) {
    return ((B_Msg)$ASYNC((($Actor)self), (($Cont)linesQ_L_8procG_new(self, cb))));
}
B_Msg linesQ_ApaD_notice (linesQ_Apa self, int64_t i) {
    return ((B_Msg)$ASYNC((($Actor)self), (($Cont)linesQ_L_9procG_new(self, i))));
}
void linesQ_ApaD___serialize__ (linesQ_Apa self, $Serial$state state) {
    $ActorG_methods.__serialize__(($Actor)self, state);
    $val_serialize(I64_ID, &self->apa, state);
    $val_serialize(I64_ID, &self->apb, state);
    $val_serialize(I64_ID, &self->y, state);
    $val_serialize(I64_ID, &self->z, state);
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
    $tmp = $val_deserialize(state);
    memcpy(&self->z, &$tmp, sizeof(self->z));
    return self;
}
void linesQ_ApaD_GCfinalizer (void *obj, void *cdata) {
    linesQ_Apa self = (linesQ_Apa)obj;
    self->$class->__cleanup__(self);
}
$R linesQ_ApaG_new($Cont G_1) {
    linesQ_Apa $tmp = acton_malloc(sizeof(struct linesQ_Apa));
    $tmp->$class = &linesQ_ApaG_methods;
    return linesQ_ApaG_methods.__init__($tmp, $CONSTCONT($tmp, G_1));
}
struct linesQ_ApaG_class linesQ_ApaG_methods;
$R linesQ_BepaD___init__ (linesQ_Bepa self, $Cont C_cont) {
    #line 25 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(1, actStrFromCStringLength("Bepa", 4)), actStrFromCStringLength(" ", 1), actStrFromCStringLength("\n", 1), false, false);
    return $R_CONT(C_cont, B_None);
}
#line 22 "test/src/lines.act"
$R linesQ_BepaD_callbackG_local (linesQ_Bepa self, $Cont C_cont, int64_t i) {
    #line 23 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(2, actStrFromCStringLength("callback", 8), toB_int(i)), actStrFromCStringLength(" ", 1), actStrFromCStringLength("\n", 1), false, false);
    int64_t N_2tmp = (((int64_t)(i + 1LL)));
    return $R_CONT(C_cont, toB_int(N_2tmp));
}
B_Msg linesQ_BepaD_callback (linesQ_Bepa self, int64_t i) {
    return ((B_Msg)$ASYNC((($Actor)self), (($Cont)linesQ_L_10procG_new(self, i))));
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
$R linesQ_BepaG_new($Cont G_1) {
    linesQ_Bepa $tmp = acton_malloc(sizeof(struct linesQ_Bepa));
    $tmp->$class = &linesQ_BepaG_methods;
    return linesQ_BepaG_methods.__init__($tmp, $CONSTCONT($tmp, G_1));
}
struct linesQ_BepaG_class linesQ_BepaG_methods;
$R linesQ_mainD___init__ (linesQ_main self, $Cont C_cont, B_Env env) {
    ((linesQ_main)(self))->env = env;
    return linesQ_ApaG_newact((($Cont)linesQ_L_23ContG_new(self, C_cont)));
}
#line 28 "test/src/lines.act"
$R linesQ_mainD_myprocG_local (linesQ_main self, $Cont C_cont, int64_t i) {
    #line 29 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, bool, bool))B_print)($NEWTUPLE(2, actStrFromCStringLength("myproc", 6), toB_int(i)), actStrFromCStringLength(" ", 1), actStrFromCStringLength("\n", 1), false, false);
    #line 30 "test/src/lines.act"
    if (((bool (*) ($WORD, B_int, B_int))((B_Eq)(linesQ_W_Apa_331))->$class->__eq__)(linesQ_W_Apa_331, toB_int(i), toB_int(2LL))) {
        #line 31 "test/src/lines.act"
        ((B_Msg (*) ($WORD, int64_t))((B_Env)(((linesQ_main)(self))->env))->$class->exit)(((linesQ_main)(self))->env, 0LL);
    }
    return $R_CONT(C_cont, toB_int(i));
}
#line 92 "test/src/lines.act"
$R linesQ_mainD_nopG_local (linesQ_main self, $Cont C_cont) {
    #line 93 "test/src/lines.act"
    return $R_CONT(C_cont, B_None);
}
B_Msg linesQ_mainD_myproc (linesQ_main self, int64_t i) {
    return ((B_Msg)$ASYNC((($Actor)self), (($Cont)linesQ_L_24procG_new(self, i))));
}
B_Msg linesQ_mainD_nop (linesQ_main self) {
    return $ASYNC((($Actor)self), (($Cont)linesQ_L_25procG_new(self)));
}
void linesQ_mainD___serialize__ (linesQ_main self, $Serial$state state) {
    $ActorG_methods.__serialize__(($Actor)self, state);
    $step_serialize(self->env, state);
    $step_serialize(self->a, state);
    $step_serialize(self->b, state);
    $step_serialize(self->x, state);
    $val_serialize(I64_ID, &self->r, state);
    $val_serialize(I64_ID, &self->v, state);
    $val_serialize(I64_ID, &self->i, state);
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
    self->env = $step_deserialize(state);
    self->a = $step_deserialize(state);
    self->b = $step_deserialize(state);
    self->x = $step_deserialize(state);
    $tmp = $val_deserialize(state);
    memcpy(&self->r, &$tmp, sizeof(self->r));
    $tmp = $val_deserialize(state);
    memcpy(&self->v, &$tmp, sizeof(self->v));
    $tmp = $val_deserialize(state);
    memcpy(&self->i, &$tmp, sizeof(self->i));
    return self;
}
void linesQ_mainD_GCfinalizer (void *obj, void *cdata) {
    linesQ_main self = (linesQ_main)obj;
    self->$class->__cleanup__(self);
}
$R linesQ_mainG_new($Cont G_1, B_Env G_2) {
    linesQ_main $tmp = acton_malloc(sizeof(struct linesQ_main));
    $tmp->$class = &linesQ_mainG_methods;
    return linesQ_mainG_methods.__init__($tmp, $CONSTCONT($tmp, G_1), G_2);
}
struct linesQ_mainG_class linesQ_mainG_methods;
$R linesQ_ApaG_newact ($Cont C_cont) {
    linesQ_Apa G_act = $NEWACTOR(linesQ_Apa);
    if ((void*)G_act->$class->__cleanup__ != (void*)$ActorD___cleanup__) $InstallFinalizer(G_act, linesQ_ApaD_GCfinalizer);
    return $AWAIT($SKIPRES($CONSTCONT(G_act, C_cont)), $ASYNC((($Actor)G_act), (($Cont)linesQ_L_27procG_new(G_act))));
}
$R linesQ_BepaG_newact ($Cont C_cont) {
    linesQ_Bepa G_act = $NEWACTOR(linesQ_Bepa);
    if ((void*)G_act->$class->__cleanup__ != (void*)$ActorD___cleanup__) $InstallFinalizer(G_act, linesQ_BepaD_GCfinalizer);
    return $AWAIT($SKIPRES($CONSTCONT(G_act, C_cont)), $ASYNC((($Actor)G_act), (($Cont)linesQ_L_29procG_new(G_act))));
}
$R linesQ_mainG_newact ($Cont C_cont, B_Env env) {
    linesQ_main G_act = $NEWACTOR(linesQ_main);
    if ((void*)G_act->$class->__cleanup__ != (void*)$ActorD___cleanup__) $InstallFinalizer(G_act, linesQ_mainD_GCfinalizer);
    return $AWAIT($SKIPRES($CONSTCONT(G_act, C_cont)), $ASYNC((($Actor)G_act), (($Cont)linesQ_L_31procG_new(G_act, env))));
}
static void linesQ___init__$g0 () {
    linesQ_W_Apa_1097 = (B_Eq)B_OrdD_intG_witness;
    linesQ_W_Apa_759 = ((B_Eq)$EqOptG_new(linesQ_W_Apa_1097));
    linesQ_W_Apa_331 = ((B_Eq)$EqOptG_new(linesQ_W_Apa_1097));
    linesQ_W_Apa_785 = ((B_Eq)$EqOptG_new(linesQ_W_Apa_1097));
}
int linesQ_done$ = 0;
void linesQ___init__ () {
    if (linesQ_done$) return;
    linesQ_done$ = 1;
    {
        linesQ_L_2ContG_methods.$GCINFO = "linesQ_L_2Cont";
        linesQ_L_2ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        linesQ_L_2ContG_methods.__bool__ = (bool (*) (linesQ_L_2Cont))B_valueG_methods.__bool__;
        linesQ_L_2ContG_methods.__str__ = (B_str (*) (linesQ_L_2Cont))B_valueG_methods.__str__;
        linesQ_L_2ContG_methods.__repr__ = (B_str (*) (linesQ_L_2Cont))B_valueG_methods.__repr__;
        linesQ_L_2ContG_methods.__init__ = NULL;
        linesQ_L_2ContG_methods.__call__ = ($R (*) (linesQ_L_2Cont, B_NoneType))linesQ_L_2ContD___call__;
        linesQ_L_2ContG_methods.__serialize__ = linesQ_L_2ContD___serialize__;
        linesQ_L_2ContG_methods.__deserialize__ = linesQ_L_2ContD___deserialize__;
        $register(&linesQ_L_2ContG_methods);
    }
    {
        linesQ_L_4actionG_methods.$GCINFO = "linesQ_L_4action";
        linesQ_L_4actionG_methods.$superclass = ($SuperG_class)&$actionG_methods;
        linesQ_L_4actionG_methods.__bool__ = (bool (*) (linesQ_L_4action))B_valueG_methods.__bool__;
        linesQ_L_4actionG_methods.__str__ = (B_str (*) (linesQ_L_4action))B_valueG_methods.__str__;
        linesQ_L_4actionG_methods.__repr__ = (B_str (*) (linesQ_L_4action))B_valueG_methods.__repr__;
        linesQ_L_4actionG_methods.__init__ = NULL;
        linesQ_L_4actionG_methods.__call__ = ($R (*) (linesQ_L_4action, $Cont, B_int))linesQ_V_1closure_adapter;
        linesQ_L_4actionG_methods.__exec__ = ($R (*) (linesQ_L_4action, $Cont, B_int))linesQ_V_2closure_adapter;
        linesQ_L_4actionG_methods.__asyn__ = (B_Msg (*) (linesQ_L_4action, B_int))linesQ_L_4actionD___asyn__;
        linesQ_L_4actionG_methods.__serialize__ = linesQ_L_4actionD___serialize__;
        linesQ_L_4actionG_methods.__deserialize__ = linesQ_L_4actionD___deserialize__;
        $register(&linesQ_L_4actionG_methods);
    }
    {
        linesQ_L_6ContG_methods.$GCINFO = "linesQ_L_6Cont";
        linesQ_L_6ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        linesQ_L_6ContG_methods.__bool__ = (bool (*) (linesQ_L_6Cont))B_valueG_methods.__bool__;
        linesQ_L_6ContG_methods.__str__ = (B_str (*) (linesQ_L_6Cont))B_valueG_methods.__str__;
        linesQ_L_6ContG_methods.__repr__ = (B_str (*) (linesQ_L_6Cont))B_valueG_methods.__repr__;
        linesQ_L_6ContG_methods.__init__ = NULL;
        linesQ_L_6ContG_methods.__call__ = ($R (*) (linesQ_L_6Cont, B_int))linesQ_L_6ContD___call__;
        linesQ_L_6ContG_methods.__serialize__ = linesQ_L_6ContD___serialize__;
        linesQ_L_6ContG_methods.__deserialize__ = linesQ_L_6ContD___deserialize__;
        $register(&linesQ_L_6ContG_methods);
    }
    {
        linesQ_L_7procG_methods.$GCINFO = "linesQ_L_7proc";
        linesQ_L_7procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        linesQ_L_7procG_methods.__bool__ = (bool (*) (linesQ_L_7proc))B_valueG_methods.__bool__;
        linesQ_L_7procG_methods.__str__ = (B_str (*) (linesQ_L_7proc))B_valueG_methods.__str__;
        linesQ_L_7procG_methods.__repr__ = (B_str (*) (linesQ_L_7proc))B_valueG_methods.__repr__;
        linesQ_L_7procG_methods.__init__ = NULL;
        linesQ_L_7procG_methods.__call__ = ($R (*) (linesQ_L_7proc, $Cont))linesQ_L_7procD___call__;
        linesQ_L_7procG_methods.__exec__ = ($R (*) (linesQ_L_7proc, $Cont))linesQ_L_7procD___call__;
        linesQ_L_7procG_methods.__serialize__ = linesQ_L_7procD___serialize__;
        linesQ_L_7procG_methods.__deserialize__ = linesQ_L_7procD___deserialize__;
        $register(&linesQ_L_7procG_methods);
    }
    {
        linesQ_L_8procG_methods.$GCINFO = "linesQ_L_8proc";
        linesQ_L_8procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        linesQ_L_8procG_methods.__bool__ = (bool (*) (linesQ_L_8proc))B_valueG_methods.__bool__;
        linesQ_L_8procG_methods.__str__ = (B_str (*) (linesQ_L_8proc))B_valueG_methods.__str__;
        linesQ_L_8procG_methods.__repr__ = (B_str (*) (linesQ_L_8proc))B_valueG_methods.__repr__;
        linesQ_L_8procG_methods.__init__ = NULL;
        linesQ_L_8procG_methods.__call__ = ($R (*) (linesQ_L_8proc, $Cont))linesQ_L_8procD___call__;
        linesQ_L_8procG_methods.__exec__ = ($R (*) (linesQ_L_8proc, $Cont))linesQ_L_8procD___call__;
        linesQ_L_8procG_methods.__serialize__ = linesQ_L_8procD___serialize__;
        linesQ_L_8procG_methods.__deserialize__ = linesQ_L_8procD___deserialize__;
        $register(&linesQ_L_8procG_methods);
    }
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
    {
        linesQ_L_10procG_methods.$GCINFO = "linesQ_L_10proc";
        linesQ_L_10procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        linesQ_L_10procG_methods.__bool__ = (bool (*) (linesQ_L_10proc))B_valueG_methods.__bool__;
        linesQ_L_10procG_methods.__str__ = (B_str (*) (linesQ_L_10proc))B_valueG_methods.__str__;
        linesQ_L_10procG_methods.__repr__ = (B_str (*) (linesQ_L_10proc))B_valueG_methods.__repr__;
        linesQ_L_10procG_methods.__init__ = NULL;
        linesQ_L_10procG_methods.__call__ = ($R (*) (linesQ_L_10proc, $Cont))linesQ_L_10procD___call__;
        linesQ_L_10procG_methods.__exec__ = ($R (*) (linesQ_L_10proc, $Cont))linesQ_L_10procD___call__;
        linesQ_L_10procG_methods.__serialize__ = linesQ_L_10procD___serialize__;
        linesQ_L_10procG_methods.__deserialize__ = linesQ_L_10procD___deserialize__;
        $register(&linesQ_L_10procG_methods);
    }
    {
        linesQ_L_14actionG_methods.$GCINFO = "linesQ_L_14action";
        linesQ_L_14actionG_methods.$superclass = ($SuperG_class)&$actionG_methods;
        linesQ_L_14actionG_methods.__bool__ = (bool (*) (linesQ_L_14action))B_valueG_methods.__bool__;
        linesQ_L_14actionG_methods.__str__ = (B_str (*) (linesQ_L_14action))B_valueG_methods.__str__;
        linesQ_L_14actionG_methods.__repr__ = (B_str (*) (linesQ_L_14action))B_valueG_methods.__repr__;
        linesQ_L_14actionG_methods.__init__ = NULL;
        linesQ_L_14actionG_methods.__call__ = ($R (*) (linesQ_L_14action, $Cont, B_int))linesQ_V_1closure_adapter;
        linesQ_L_14actionG_methods.__exec__ = ($R (*) (linesQ_L_14action, $Cont, B_int))linesQ_V_2closure_adapter;
        linesQ_L_14actionG_methods.__asyn__ = (B_Msg (*) (linesQ_L_14action, B_int))linesQ_L_14actionD___asyn__;
        linesQ_L_14actionG_methods.__serialize__ = linesQ_L_14actionD___serialize__;
        linesQ_L_14actionG_methods.__deserialize__ = linesQ_L_14actionD___deserialize__;
        $register(&linesQ_L_14actionG_methods);
    }
    {
        linesQ_L_16actionG_methods.$GCINFO = "linesQ_L_16action";
        linesQ_L_16actionG_methods.$superclass = ($SuperG_class)&$actionG_methods;
        linesQ_L_16actionG_methods.__bool__ = (bool (*) (linesQ_L_16action))B_valueG_methods.__bool__;
        linesQ_L_16actionG_methods.__str__ = (B_str (*) (linesQ_L_16action))B_valueG_methods.__str__;
        linesQ_L_16actionG_methods.__repr__ = (B_str (*) (linesQ_L_16action))B_valueG_methods.__repr__;
        linesQ_L_16actionG_methods.__init__ = NULL;
        linesQ_L_16actionG_methods.__call__ = ($R (*) (linesQ_L_16action, $Cont, B_int))linesQ_V_1closure_adapter;
        linesQ_L_16actionG_methods.__exec__ = ($R (*) (linesQ_L_16action, $Cont, B_int))linesQ_V_2closure_adapter;
        linesQ_L_16actionG_methods.__asyn__ = (B_Msg (*) (linesQ_L_16action, B_int))linesQ_L_16actionD___asyn__;
        linesQ_L_16actionG_methods.__serialize__ = linesQ_L_16actionD___serialize__;
        linesQ_L_16actionG_methods.__deserialize__ = linesQ_L_16actionD___deserialize__;
        $register(&linesQ_L_16actionG_methods);
    }
    {
        linesQ_L_19actionG_methods.$GCINFO = "linesQ_L_19action";
        linesQ_L_19actionG_methods.$superclass = ($SuperG_class)&$actionG_methods;
        linesQ_L_19actionG_methods.__bool__ = (bool (*) (linesQ_L_19action))B_valueG_methods.__bool__;
        linesQ_L_19actionG_methods.__str__ = (B_str (*) (linesQ_L_19action))B_valueG_methods.__str__;
        linesQ_L_19actionG_methods.__repr__ = (B_str (*) (linesQ_L_19action))B_valueG_methods.__repr__;
        linesQ_L_19actionG_methods.__init__ = NULL;
        linesQ_L_19actionG_methods.__call__ = ($R (*) (linesQ_L_19action, $Cont, B_int))linesQ_V_1closure_adapter;
        linesQ_L_19actionG_methods.__exec__ = ($R (*) (linesQ_L_19action, $Cont, B_int))linesQ_V_2closure_adapter;
        linesQ_L_19actionG_methods.__asyn__ = (B_Msg (*) (linesQ_L_19action, B_int))linesQ_L_19actionD___asyn__;
        linesQ_L_19actionG_methods.__serialize__ = linesQ_L_19actionD___serialize__;
        linesQ_L_19actionG_methods.__deserialize__ = linesQ_L_19actionD___deserialize__;
        $register(&linesQ_L_19actionG_methods);
    }
    {
        linesQ_L_20procG_methods.$GCINFO = "linesQ_L_20proc";
        linesQ_L_20procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        linesQ_L_20procG_methods.__bool__ = (bool (*) (linesQ_L_20proc))B_valueG_methods.__bool__;
        linesQ_L_20procG_methods.__str__ = (B_str (*) (linesQ_L_20proc))B_valueG_methods.__str__;
        linesQ_L_20procG_methods.__repr__ = (B_str (*) (linesQ_L_20proc))B_valueG_methods.__repr__;
        linesQ_L_20procG_methods.__init__ = NULL;
        linesQ_L_20procG_methods.__call__ = ($R (*) (linesQ_L_20proc, $Cont))linesQ_L_20procD___call__;
        linesQ_L_20procG_methods.__exec__ = ($R (*) (linesQ_L_20proc, $Cont))linesQ_L_20procD___call__;
        linesQ_L_20procG_methods.__serialize__ = linesQ_L_20procD___serialize__;
        linesQ_L_20procG_methods.__deserialize__ = linesQ_L_20procD___deserialize__;
        $register(&linesQ_L_20procG_methods);
    }
    {
        linesQ_L_21ContG_methods.$GCINFO = "linesQ_L_21Cont";
        linesQ_L_21ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        linesQ_L_21ContG_methods.__bool__ = (bool (*) (linesQ_L_21Cont))B_valueG_methods.__bool__;
        linesQ_L_21ContG_methods.__str__ = (B_str (*) (linesQ_L_21Cont))B_valueG_methods.__str__;
        linesQ_L_21ContG_methods.__repr__ = (B_str (*) (linesQ_L_21Cont))B_valueG_methods.__repr__;
        linesQ_L_21ContG_methods.__init__ = NULL;
        linesQ_L_21ContG_methods.__call__ = ($R (*) (linesQ_L_21Cont, B_int))linesQ_L_21ContD___call__;
        linesQ_L_21ContG_methods.__serialize__ = linesQ_L_21ContD___serialize__;
        linesQ_L_21ContG_methods.__deserialize__ = linesQ_L_21ContD___deserialize__;
        $register(&linesQ_L_21ContG_methods);
    }
    {
        linesQ_L_22ContG_methods.$GCINFO = "linesQ_L_22Cont";
        linesQ_L_22ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        linesQ_L_22ContG_methods.__bool__ = (bool (*) (linesQ_L_22Cont))B_valueG_methods.__bool__;
        linesQ_L_22ContG_methods.__str__ = (B_str (*) (linesQ_L_22Cont))B_valueG_methods.__str__;
        linesQ_L_22ContG_methods.__repr__ = (B_str (*) (linesQ_L_22Cont))B_valueG_methods.__repr__;
        linesQ_L_22ContG_methods.__init__ = NULL;
        linesQ_L_22ContG_methods.__call__ = ($R (*) (linesQ_L_22Cont, linesQ_Bepa))linesQ_L_22ContD___call__;
        linesQ_L_22ContG_methods.__serialize__ = linesQ_L_22ContD___serialize__;
        linesQ_L_22ContG_methods.__deserialize__ = linesQ_L_22ContD___deserialize__;
        $register(&linesQ_L_22ContG_methods);
    }
    {
        linesQ_L_23ContG_methods.$GCINFO = "linesQ_L_23Cont";
        linesQ_L_23ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        linesQ_L_23ContG_methods.__bool__ = (bool (*) (linesQ_L_23Cont))B_valueG_methods.__bool__;
        linesQ_L_23ContG_methods.__str__ = (B_str (*) (linesQ_L_23Cont))B_valueG_methods.__str__;
        linesQ_L_23ContG_methods.__repr__ = (B_str (*) (linesQ_L_23Cont))B_valueG_methods.__repr__;
        linesQ_L_23ContG_methods.__init__ = NULL;
        linesQ_L_23ContG_methods.__call__ = ($R (*) (linesQ_L_23Cont, linesQ_Apa))linesQ_L_23ContD___call__;
        linesQ_L_23ContG_methods.__serialize__ = linesQ_L_23ContD___serialize__;
        linesQ_L_23ContG_methods.__deserialize__ = linesQ_L_23ContD___deserialize__;
        $register(&linesQ_L_23ContG_methods);
    }
    {
        linesQ_L_24procG_methods.$GCINFO = "linesQ_L_24proc";
        linesQ_L_24procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        linesQ_L_24procG_methods.__bool__ = (bool (*) (linesQ_L_24proc))B_valueG_methods.__bool__;
        linesQ_L_24procG_methods.__str__ = (B_str (*) (linesQ_L_24proc))B_valueG_methods.__str__;
        linesQ_L_24procG_methods.__repr__ = (B_str (*) (linesQ_L_24proc))B_valueG_methods.__repr__;
        linesQ_L_24procG_methods.__init__ = NULL;
        linesQ_L_24procG_methods.__call__ = ($R (*) (linesQ_L_24proc, $Cont))linesQ_L_24procD___call__;
        linesQ_L_24procG_methods.__exec__ = ($R (*) (linesQ_L_24proc, $Cont))linesQ_L_24procD___call__;
        linesQ_L_24procG_methods.__serialize__ = linesQ_L_24procD___serialize__;
        linesQ_L_24procG_methods.__deserialize__ = linesQ_L_24procD___deserialize__;
        $register(&linesQ_L_24procG_methods);
    }
    {
        linesQ_L_25procG_methods.$GCINFO = "linesQ_L_25proc";
        linesQ_L_25procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        linesQ_L_25procG_methods.__bool__ = (bool (*) (linesQ_L_25proc))B_valueG_methods.__bool__;
        linesQ_L_25procG_methods.__str__ = (B_str (*) (linesQ_L_25proc))B_valueG_methods.__str__;
        linesQ_L_25procG_methods.__repr__ = (B_str (*) (linesQ_L_25proc))B_valueG_methods.__repr__;
        linesQ_L_25procG_methods.__init__ = NULL;
        linesQ_L_25procG_methods.__call__ = ($R (*) (linesQ_L_25proc, $Cont))linesQ_L_25procD___call__;
        linesQ_L_25procG_methods.__exec__ = ($R (*) (linesQ_L_25proc, $Cont))linesQ_L_25procD___call__;
        linesQ_L_25procG_methods.__serialize__ = linesQ_L_25procD___serialize__;
        linesQ_L_25procG_methods.__deserialize__ = linesQ_L_25procD___deserialize__;
        $register(&linesQ_L_25procG_methods);
    }
    {
        linesQ_L_27procG_methods.$GCINFO = "linesQ_L_27proc";
        linesQ_L_27procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        linesQ_L_27procG_methods.__bool__ = (bool (*) (linesQ_L_27proc))B_valueG_methods.__bool__;
        linesQ_L_27procG_methods.__str__ = (B_str (*) (linesQ_L_27proc))B_valueG_methods.__str__;
        linesQ_L_27procG_methods.__repr__ = (B_str (*) (linesQ_L_27proc))B_valueG_methods.__repr__;
        linesQ_L_27procG_methods.__init__ = NULL;
        linesQ_L_27procG_methods.__call__ = ($R (*) (linesQ_L_27proc, $Cont))linesQ_L_27procD___call__;
        linesQ_L_27procG_methods.__exec__ = ($R (*) (linesQ_L_27proc, $Cont))linesQ_L_27procD___call__;
        linesQ_L_27procG_methods.__serialize__ = linesQ_L_27procD___serialize__;
        linesQ_L_27procG_methods.__deserialize__ = linesQ_L_27procD___deserialize__;
        $register(&linesQ_L_27procG_methods);
    }
    {
        linesQ_L_29procG_methods.$GCINFO = "linesQ_L_29proc";
        linesQ_L_29procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        linesQ_L_29procG_methods.__bool__ = (bool (*) (linesQ_L_29proc))B_valueG_methods.__bool__;
        linesQ_L_29procG_methods.__str__ = (B_str (*) (linesQ_L_29proc))B_valueG_methods.__str__;
        linesQ_L_29procG_methods.__repr__ = (B_str (*) (linesQ_L_29proc))B_valueG_methods.__repr__;
        linesQ_L_29procG_methods.__init__ = NULL;
        linesQ_L_29procG_methods.__call__ = ($R (*) (linesQ_L_29proc, $Cont))linesQ_L_29procD___call__;
        linesQ_L_29procG_methods.__exec__ = ($R (*) (linesQ_L_29proc, $Cont))linesQ_L_29procD___call__;
        linesQ_L_29procG_methods.__serialize__ = linesQ_L_29procD___serialize__;
        linesQ_L_29procG_methods.__deserialize__ = linesQ_L_29procD___deserialize__;
        $register(&linesQ_L_29procG_methods);
    }
    {
        linesQ_L_31procG_methods.$GCINFO = "linesQ_L_31proc";
        linesQ_L_31procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        linesQ_L_31procG_methods.__bool__ = (bool (*) (linesQ_L_31proc))B_valueG_methods.__bool__;
        linesQ_L_31procG_methods.__str__ = (B_str (*) (linesQ_L_31proc))B_valueG_methods.__str__;
        linesQ_L_31procG_methods.__repr__ = (B_str (*) (linesQ_L_31proc))B_valueG_methods.__repr__;
        linesQ_L_31procG_methods.__init__ = NULL;
        linesQ_L_31procG_methods.__call__ = ($R (*) (linesQ_L_31proc, $Cont))linesQ_L_31procD___call__;
        linesQ_L_31procG_methods.__exec__ = ($R (*) (linesQ_L_31proc, $Cont))linesQ_L_31procD___call__;
        linesQ_L_31procG_methods.__serialize__ = linesQ_L_31procD___serialize__;
        linesQ_L_31procG_methods.__deserialize__ = linesQ_L_31procD___deserialize__;
        $register(&linesQ_L_31procG_methods);
    }
    {
        linesQ_ApaG_methods.$GCINFO = "linesQ_Apa";
        linesQ_ApaG_methods.$superclass = ($SuperG_class)&$ActorG_methods;
        linesQ_ApaG_methods.__bool__ = (bool (*) (linesQ_Apa))$ActorG_methods.__bool__;
        linesQ_ApaG_methods.__str__ = (B_str (*) (linesQ_Apa))$ActorG_methods.__str__;
        linesQ_ApaG_methods.__repr__ = (B_str (*) (linesQ_Apa))$ActorG_methods.__repr__;
        linesQ_ApaG_methods.__resume__ = (B_NoneType (*) (linesQ_Apa))$ActorG_methods.__resume__;
        linesQ_ApaG_methods.__cleanup__ = (B_NoneType (*) (linesQ_Apa))$ActorG_methods.__cleanup__;
        linesQ_ApaG_methods.__init__ = ($R (*) (linesQ_Apa, $Cont))linesQ_ApaD___init__;
        linesQ_ApaG_methods.setupG_local = ($R (*) (linesQ_Apa, $Cont, $action))linesQ_ApaD_setupG_local;
        linesQ_ApaG_methods.computeG_local = ($R (*) (linesQ_Apa, $Cont, $action))linesQ_ApaD_computeG_local;
        linesQ_ApaG_methods.noticeG_local = ($R (*) (linesQ_Apa, $Cont, int64_t))linesQ_ApaD_noticeG_local;
        linesQ_ApaG_methods.setup = (B_Msg (*) (linesQ_Apa, $action))linesQ_ApaD_setup;
        linesQ_ApaG_methods.compute = (B_Msg (*) (linesQ_Apa, $action))linesQ_ApaD_compute;
        linesQ_ApaG_methods.notice = (B_Msg (*) (linesQ_Apa, int64_t))linesQ_ApaD_notice;
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
        linesQ_BepaG_methods.__init__ = ($R (*) (linesQ_Bepa, $Cont))linesQ_BepaD___init__;
        linesQ_BepaG_methods.callbackG_local = ($R (*) (linesQ_Bepa, $Cont, int64_t))linesQ_BepaD_callbackG_local;
        linesQ_BepaG_methods.callback = (B_Msg (*) (linesQ_Bepa, int64_t))linesQ_BepaD_callback;
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
        linesQ_mainG_methods.__init__ = ($R (*) (linesQ_main, $Cont, B_Env))linesQ_mainD___init__;
        linesQ_mainG_methods.myprocG_local = ($R (*) (linesQ_main, $Cont, int64_t))linesQ_mainD_myprocG_local;
        linesQ_mainG_methods.nopG_local = ($R (*) (linesQ_main, $Cont))linesQ_mainD_nopG_local;
        linesQ_mainG_methods.myproc = (B_Msg (*) (linesQ_main, int64_t))linesQ_mainD_myproc;
        linesQ_mainG_methods.nop = (B_Msg (*) (linesQ_main))linesQ_mainD_nop;
        linesQ_mainG_methods.__serialize__ = linesQ_mainD___serialize__;
        linesQ_mainG_methods.__deserialize__ = linesQ_mainD___deserialize__;
        $register(&linesQ_mainG_methods);
    }
    linesQ___init__$g0();
}