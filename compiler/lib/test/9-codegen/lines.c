/* Acton source hash: test-hash */
#include "rts/common.h"
#include "out/types/lines.h"
B_Number linesQ_W_19;
B_Times linesQ_W_238;
B_Plus linesQ_W_609;
B_Eq linesQ_W_784;
B_Ord linesQ_W_1216;
$R linesQ_L_1C_1cont (linesQ_Apa self, $Cont C_cont, B_NoneType C_2res) {
    #line 18 "test/src/lines.act"
    self->z = ((B_bigint)toB_bigint(1UL));
    #line 19 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("Apa")), B_None, B_None, B_None, B_None);
    return $R_CONT(C_cont, B_None);
}
B_NoneType linesQ_L_2ContD___init__ (linesQ_L_2Cont L_self, linesQ_Apa self, $Cont C_cont) {
    L_self->self = self;
    L_self->C_cont = C_cont;
    return B_None;
}
$R linesQ_L_2ContD___call__ (linesQ_L_2Cont L_self, B_NoneType G_1) {
    linesQ_Apa self = L_self->self;
    $Cont C_cont = L_self->C_cont;
    return linesQ_L_1C_1cont(self, C_cont, G_1);
}
void linesQ_L_2ContD___serialize__ (linesQ_L_2Cont self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->C_cont, state);
}
linesQ_L_2Cont linesQ_L_2ContD___deserialize__ (linesQ_L_2Cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_2Cont));
            self->$class = &linesQ_L_2ContG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_2Cont, state);
    }
    self->self = $step_deserialize(state);
    self->C_cont = $step_deserialize(state);
    return self;
}
linesQ_L_2Cont linesQ_L_2ContG_new(linesQ_Apa G_1, $Cont G_2) {
    linesQ_L_2Cont $tmp = acton_malloc(sizeof(struct linesQ_L_2Cont));
    $tmp->$class = &linesQ_L_2ContG_methods;
    linesQ_L_2ContG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct linesQ_L_2ContG_class linesQ_L_2ContG_methods;
B_NoneType linesQ_L_4actionD___init__ (linesQ_L_4action L_self, linesQ_Apa L_3obj) {
    L_self->L_3obj = L_3obj;
    return B_None;
}
$R linesQ_L_4actionD___call__ (linesQ_L_4action L_self, $Cont L_cont, B_bigint G_1) {
    return $AWAIT(L_cont, ((B_Msg)((B_Msg (*) (linesQ_L_4action, B_bigint))L_self->$class->__asyn__)(L_self, G_1)));
}
$R linesQ_L_4actionD___exec__ (linesQ_L_4action L_self, $Cont L_cont, B_bigint G_1) {
    return $R_CONT(L_cont, ((B_value)((B_Msg (*) (linesQ_L_4action, B_bigint))L_self->$class->__asyn__)(L_self, G_1)));
}
B_Msg linesQ_L_4actionD___asyn__ (linesQ_L_4action L_self, B_bigint G_1) {
    linesQ_Apa L_3obj = L_self->L_3obj;
    return ((B_Msg)((B_Msg (*) (linesQ_Apa, B_bigint))L_3obj->$class->notice)(L_3obj, G_1));
}
void linesQ_L_4actionD___serialize__ (linesQ_L_4action self, $Serial$state state) {
    $step_serialize(self->L_3obj, state);
}
linesQ_L_4action linesQ_L_4actionD___deserialize__ (linesQ_L_4action self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_4action));
            self->$class = &linesQ_L_4actionG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_4action, state);
    }
    self->L_3obj = $step_deserialize(state);
    return self;
}
linesQ_L_4action linesQ_L_4actionG_new(linesQ_Apa G_1) {
    linesQ_L_4action $tmp = acton_malloc(sizeof(struct linesQ_L_4action));
    $tmp->$class = &linesQ_L_4actionG_methods;
    linesQ_L_4actionG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct linesQ_L_4actionG_class linesQ_L_4actionG_methods;
$R linesQ_L_5C_3cont ($action cb, $Cont C_cont, B_bigint C_4res) {
    #line 9 "test/src/lines.act"
    B_bigint v = C_4res;
    #line 10 "test/src/lines.act"
    B_Msg m = ((B_Msg)((B_Msg (*) ($action, B_bigint))cb->$class->__asyn__)(cb, ((B_bigint)toB_bigint(2UL))));
    B_bigint N_tmp = ((B_bigint (*) (B_Times, B_bigint, B_bigint))linesQ_W_238->$class->__mul__)(linesQ_W_238, v, ((B_bigint)toB_bigint(10UL)));
    return $R_CONT(C_cont, N_tmp);
}
B_NoneType linesQ_L_6ContD___init__ (linesQ_L_6Cont L_self, $action cb, $Cont C_cont) {
    L_self->cb = cb;
    L_self->C_cont = C_cont;
    return B_None;
}
$R linesQ_L_6ContD___call__ (linesQ_L_6Cont L_self, B_bigint G_1) {
    $action cb = L_self->cb;
    $Cont C_cont = L_self->C_cont;
    return linesQ_L_5C_3cont(cb, C_cont, G_1);
}
void linesQ_L_6ContD___serialize__ (linesQ_L_6Cont self, $Serial$state state) {
    $step_serialize(self->cb, state);
    $step_serialize(self->C_cont, state);
}
linesQ_L_6Cont linesQ_L_6ContD___deserialize__ (linesQ_L_6Cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_6Cont));
            self->$class = &linesQ_L_6ContG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_6Cont, state);
    }
    self->cb = $step_deserialize(state);
    self->C_cont = $step_deserialize(state);
    return self;
}
linesQ_L_6Cont linesQ_L_6ContG_new($action G_1, $Cont G_2) {
    linesQ_L_6Cont $tmp = acton_malloc(sizeof(struct linesQ_L_6Cont));
    $tmp->$class = &linesQ_L_6ContG_methods;
    linesQ_L_6ContG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct linesQ_L_6ContG_class linesQ_L_6ContG_methods;
B_NoneType linesQ_L_7procD___init__ (linesQ_L_7proc L_self, linesQ_Apa self, $action cb) {
    L_self->self = self;
    L_self->cb = cb;
    return B_None;
}
$R linesQ_L_7procD___call__ (linesQ_L_7proc L_self, $Cont C_cont) {
    linesQ_Apa self = L_self->self;
    $action cb = L_self->cb;
    return (($R (*) (linesQ_Apa, $Cont, $action))self->$class->setupG_local)(self, C_cont, cb);
}
$R linesQ_L_7procD___exec__ (linesQ_L_7proc L_self, $Cont C_cont) {
    return (($R (*) (linesQ_L_7proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void linesQ_L_7procD___serialize__ (linesQ_L_7proc self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->cb, state);
}
linesQ_L_7proc linesQ_L_7procD___deserialize__ (linesQ_L_7proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_7proc));
            self->$class = &linesQ_L_7procG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_7proc, state);
    }
    self->self = $step_deserialize(state);
    self->cb = $step_deserialize(state);
    return self;
}
linesQ_L_7proc linesQ_L_7procG_new(linesQ_Apa G_1, $action G_2) {
    linesQ_L_7proc $tmp = acton_malloc(sizeof(struct linesQ_L_7proc));
    $tmp->$class = &linesQ_L_7procG_methods;
    linesQ_L_7procG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct linesQ_L_7procG_class linesQ_L_7procG_methods;
B_NoneType linesQ_L_8procD___init__ (linesQ_L_8proc L_self, linesQ_Apa self, $action cb) {
    L_self->self = self;
    L_self->cb = cb;
    return B_None;
}
$R linesQ_L_8procD___call__ (linesQ_L_8proc L_self, $Cont C_cont) {
    linesQ_Apa self = L_self->self;
    $action cb = L_self->cb;
    return (($R (*) (linesQ_Apa, $Cont, $action))self->$class->computeG_local)(self, C_cont, cb);
}
$R linesQ_L_8procD___exec__ (linesQ_L_8proc L_self, $Cont C_cont) {
    return (($R (*) (linesQ_L_8proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void linesQ_L_8procD___serialize__ (linesQ_L_8proc self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->cb, state);
}
linesQ_L_8proc linesQ_L_8procD___deserialize__ (linesQ_L_8proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_8proc));
            self->$class = &linesQ_L_8procG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_8proc, state);
    }
    self->self = $step_deserialize(state);
    self->cb = $step_deserialize(state);
    return self;
}
linesQ_L_8proc linesQ_L_8procG_new(linesQ_Apa G_1, $action G_2) {
    linesQ_L_8proc $tmp = acton_malloc(sizeof(struct linesQ_L_8proc));
    $tmp->$class = &linesQ_L_8procG_methods;
    linesQ_L_8procG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct linesQ_L_8procG_class linesQ_L_8procG_methods;
B_NoneType linesQ_L_9procD___init__ (linesQ_L_9proc L_self, linesQ_Apa self, B_bigint i) {
    L_self->self = self;
    L_self->i = i;
    return B_None;
}
$R linesQ_L_9procD___call__ (linesQ_L_9proc L_self, $Cont C_cont) {
    linesQ_Apa self = L_self->self;
    B_bigint i = L_self->i;
    return (($R (*) (linesQ_Apa, $Cont, B_bigint))self->$class->noticeG_local)(self, C_cont, i);
}
$R linesQ_L_9procD___exec__ (linesQ_L_9proc L_self, $Cont C_cont) {
    return (($R (*) (linesQ_L_9proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void linesQ_L_9procD___serialize__ (linesQ_L_9proc self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->i, state);
}
linesQ_L_9proc linesQ_L_9procD___deserialize__ (linesQ_L_9proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_9proc));
            self->$class = &linesQ_L_9procG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_9proc, state);
    }
    self->self = $step_deserialize(state);
    self->i = $step_deserialize(state);
    return self;
}
linesQ_L_9proc linesQ_L_9procG_new(linesQ_Apa G_1, B_bigint G_2) {
    linesQ_L_9proc $tmp = acton_malloc(sizeof(struct linesQ_L_9proc));
    $tmp->$class = &linesQ_L_9procG_methods;
    linesQ_L_9procG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct linesQ_L_9procG_class linesQ_L_9procG_methods;
B_NoneType linesQ_L_10procD___init__ (linesQ_L_10proc L_self, linesQ_Bepa self, B_bigint i) {
    L_self->self = self;
    L_self->i = i;
    return B_None;
}
$R linesQ_L_10procD___call__ (linesQ_L_10proc L_self, $Cont C_cont) {
    linesQ_Bepa self = L_self->self;
    B_bigint i = L_self->i;
    return (($R (*) (linesQ_Bepa, $Cont, B_bigint))self->$class->callbackG_local)(self, C_cont, i);
}
$R linesQ_L_10procD___exec__ (linesQ_L_10proc L_self, $Cont C_cont) {
    return (($R (*) (linesQ_L_10proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void linesQ_L_10procD___serialize__ (linesQ_L_10proc self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->i, state);
}
linesQ_L_10proc linesQ_L_10procD___deserialize__ (linesQ_L_10proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_10proc));
            self->$class = &linesQ_L_10procG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_10proc, state);
    }
    self->self = $step_deserialize(state);
    self->i = $step_deserialize(state);
    return self;
}
linesQ_L_10proc linesQ_L_10procG_new(linesQ_Bepa G_1, B_bigint G_2) {
    linesQ_L_10proc $tmp = acton_malloc(sizeof(struct linesQ_L_10proc));
    $tmp->$class = &linesQ_L_10procG_methods;
    linesQ_L_10procG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct linesQ_L_10procG_class linesQ_L_10procG_methods;
B_NoneType linesQ_L_14actionD___init__ (linesQ_L_14action L_self, linesQ_Apa L_13obj) {
    L_self->L_13obj = L_13obj;
    return B_None;
}
$R linesQ_L_14actionD___call__ (linesQ_L_14action L_self, $Cont L_cont, B_bigint G_1) {
    return $AWAIT(L_cont, ((B_Msg)((B_Msg (*) (linesQ_L_14action, B_bigint))L_self->$class->__asyn__)(L_self, G_1)));
}
$R linesQ_L_14actionD___exec__ (linesQ_L_14action L_self, $Cont L_cont, B_bigint G_1) {
    return $R_CONT(L_cont, ((B_value)((B_Msg (*) (linesQ_L_14action, B_bigint))L_self->$class->__asyn__)(L_self, G_1)));
}
B_Msg linesQ_L_14actionD___asyn__ (linesQ_L_14action L_self, B_bigint G_1) {
    linesQ_Apa L_13obj = L_self->L_13obj;
    return ((B_Msg)((B_Msg (*) (linesQ_Apa, B_bigint))L_13obj->$class->notice)(L_13obj, G_1));
}
void linesQ_L_14actionD___serialize__ (linesQ_L_14action self, $Serial$state state) {
    $step_serialize(self->L_13obj, state);
}
linesQ_L_14action linesQ_L_14actionD___deserialize__ (linesQ_L_14action self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_14action));
            self->$class = &linesQ_L_14actionG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_14action, state);
    }
    self->L_13obj = $step_deserialize(state);
    return self;
}
linesQ_L_14action linesQ_L_14actionG_new(linesQ_Apa G_1) {
    linesQ_L_14action $tmp = acton_malloc(sizeof(struct linesQ_L_14action));
    $tmp->$class = &linesQ_L_14actionG_methods;
    linesQ_L_14actionG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct linesQ_L_14actionG_class linesQ_L_14actionG_methods;
B_NoneType linesQ_L_16actionD___init__ (linesQ_L_16action L_self, linesQ_Bepa L_15obj) {
    L_self->L_15obj = L_15obj;
    return B_None;
}
$R linesQ_L_16actionD___call__ (linesQ_L_16action L_self, $Cont L_cont, B_bigint G_1) {
    return $AWAIT(L_cont, ((B_Msg)((B_Msg (*) (linesQ_L_16action, B_bigint))L_self->$class->__asyn__)(L_self, G_1)));
}
$R linesQ_L_16actionD___exec__ (linesQ_L_16action L_self, $Cont L_cont, B_bigint G_1) {
    return $R_CONT(L_cont, ((B_value)((B_Msg (*) (linesQ_L_16action, B_bigint))L_self->$class->__asyn__)(L_self, G_1)));
}
B_Msg linesQ_L_16actionD___asyn__ (linesQ_L_16action L_self, B_bigint G_1) {
    linesQ_Bepa L_15obj = L_self->L_15obj;
    return ((B_Msg)((B_Msg (*) (linesQ_Bepa, B_bigint))L_15obj->$class->callback)(L_15obj, G_1));
}
void linesQ_L_16actionD___serialize__ (linesQ_L_16action self, $Serial$state state) {
    $step_serialize(self->L_15obj, state);
}
linesQ_L_16action linesQ_L_16actionD___deserialize__ (linesQ_L_16action self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_16action));
            self->$class = &linesQ_L_16actionG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_16action, state);
    }
    self->L_15obj = $step_deserialize(state);
    return self;
}
linesQ_L_16action linesQ_L_16actionG_new(linesQ_Bepa G_1) {
    linesQ_L_16action $tmp = acton_malloc(sizeof(struct linesQ_L_16action));
    $tmp->$class = &linesQ_L_16actionG_methods;
    linesQ_L_16actionG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct linesQ_L_16actionG_class linesQ_L_16actionG_methods;
B_NoneType linesQ_L_19actionD___init__ (linesQ_L_19action L_self, linesQ_main L_18obj) {
    L_self->L_18obj = L_18obj;
    return B_None;
}
$R linesQ_L_19actionD___call__ (linesQ_L_19action L_self, $Cont L_cont, B_bigint G_1) {
    return $AWAIT(L_cont, ((B_Msg)((B_Msg (*) (linesQ_L_19action, B_bigint))L_self->$class->__asyn__)(L_self, G_1)));
}
$R linesQ_L_19actionD___exec__ (linesQ_L_19action L_self, $Cont L_cont, B_bigint G_1) {
    return $R_CONT(L_cont, ((B_value)((B_Msg (*) (linesQ_L_19action, B_bigint))L_self->$class->__asyn__)(L_self, G_1)));
}
B_Msg linesQ_L_19actionD___asyn__ (linesQ_L_19action L_self, B_bigint G_1) {
    linesQ_main L_18obj = L_self->L_18obj;
    return ((B_Msg)((B_Msg (*) (linesQ_main, B_bigint))L_18obj->$class->myproc)(L_18obj, G_1));
}
void linesQ_L_19actionD___serialize__ (linesQ_L_19action self, $Serial$state state) {
    $step_serialize(self->L_18obj, state);
}
linesQ_L_19action linesQ_L_19actionD___deserialize__ (linesQ_L_19action self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_19action));
            self->$class = &linesQ_L_19actionG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_19action, state);
    }
    self->L_18obj = $step_deserialize(state);
    return self;
}
linesQ_L_19action linesQ_L_19actionG_new(linesQ_main G_1) {
    linesQ_L_19action $tmp = acton_malloc(sizeof(struct linesQ_L_19action));
    $tmp->$class = &linesQ_L_19actionG_methods;
    linesQ_L_19actionG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct linesQ_L_19actionG_class linesQ_L_19actionG_methods;
B_NoneType linesQ_L_20procD___init__ (linesQ_L_20proc L_self, linesQ_main self) {
    L_self->self = self;
    return B_None;
}
$R linesQ_L_20procD___call__ (linesQ_L_20proc L_self, $Cont C_cont) {
    linesQ_main self = L_self->self;
    return (($R (*) (linesQ_main, $Cont, B_bigint))self->$class->myprocG_local)(self, C_cont, ((B_bigint)toB_bigint(0UL)));
}
$R linesQ_L_20procD___exec__ (linesQ_L_20proc L_self, $Cont C_cont) {
    return (($R (*) (linesQ_L_20proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void linesQ_L_20procD___serialize__ (linesQ_L_20proc self, $Serial$state state) {
    $step_serialize(self->self, state);
}
linesQ_L_20proc linesQ_L_20procD___deserialize__ (linesQ_L_20proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_20proc));
            self->$class = &linesQ_L_20procG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_20proc, state);
    }
    self->self = $step_deserialize(state);
    return self;
}
linesQ_L_20proc linesQ_L_20procG_new(linesQ_main G_1) {
    linesQ_L_20proc $tmp = acton_malloc(sizeof(struct linesQ_L_20proc));
    $tmp->$class = &linesQ_L_20procG_methods;
    linesQ_L_20procG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct linesQ_L_20procG_class linesQ_L_20procG_methods;
$R linesQ_L_17C_9cont (linesQ_main self, B_Iterable W_1909, B_Number W_2124, $Cont C_cont, B_bigint C_10res) {
    #line 38 "test/src/lines.act"
    self->r = C_10res;
    #line 39 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(2, to$str("r ="), self->r), B_None, B_None, B_None, B_None);
    #line 40 "test/src/lines.act"
    ((B_Msg (*) (linesQ_Apa, $action))self->a->$class->compute)(self->a, (($action)linesQ_L_19actionG_new(self)));
    #line 41 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("main")), B_None, B_None, B_None, B_None);
    #line 44 "test/src/lines.act"
    self->v = ((B_bigint)toB_bigint(0UL));
    #line 45 "test/src/lines.act"
    if (ORD_B_bigint__eq__(((B_bigint)self->v), ((B_bigint)((B_bigint)toB_bigint(0UL))))) {
        #line 46 "test/src/lines.act"
        ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("if branch")), B_None, B_None, B_None, B_None);
        #line 47 "test/src/lines.act"
        if (ORD_B_bigint__lt__(((B_bigint)self->v), ((B_bigint)((B_bigint)toB_bigint(1UL))))) {
            #line 48 "test/src/lines.act"
            ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("nested if")), B_None, B_None, B_None, B_None);
        }
        else if (ORD_B_bigint__eq__(((B_bigint)self->v), ((B_bigint)((B_bigint (*) (B_Number, B_bigint))linesQ_W_19->$class->__neg__)(linesQ_W_19, ((B_bigint)toB_bigint(1UL)))))) {
            #line 50 "test/src/lines.act"
            ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("nested elif")), B_None, B_None, B_None, B_None);
        }
        else {
            #line 52 "test/src/lines.act"
            ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("nested else")), B_None, B_None, B_None, B_None);
        }
    }
    else if (ORD_B_bigint__eq__(((B_bigint)self->v), ((B_bigint)((B_bigint)toB_bigint(1UL))))) {
        #line 54 "test/src/lines.act"
        ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("outer elif")), B_None, B_None, B_None, B_None);
    }
    else {
        #line 56 "test/src/lines.act"
        ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("outer else")), B_None, B_None, B_None, B_None);
    }
    #line 59 "test/src/lines.act"
    self->i = ((B_bigint)toB_bigint(0UL));
    #line 60 "test/src/lines.act"
    while (true) {
        if (ORD_B_bigint__lt__(((B_bigint)self->i), ((B_bigint)((B_bigint)toB_bigint(3UL))))) {
        }
        else {
            #line 70 "test/src/lines.act"
            ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("while else")), B_None, B_None, B_None, B_None);
            break;
        }
        #line 61 "test/src/lines.act"
        self->i = ((B_bigint (*) (B_Plus, B_bigint, B_bigint))linesQ_W_609->$class->__add__)(linesQ_W_609, self->i, ((B_bigint)toB_bigint(1UL)));
        #line 62 "test/src/lines.act"
        if (ORD_B_bigint__eq__(((B_bigint)self->i), ((B_bigint)((B_bigint)toB_bigint(1UL))))) {
            #line 63 "test/src/lines.act"
            ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("continue path")), B_None, B_None, B_None, B_None);
            #line 64 "test/src/lines.act"
            continue;
        }
        #line 65 "test/src/lines.act"
        if (ORD_B_bigint__eq__(((B_bigint)self->i), ((B_bigint)((B_bigint)toB_bigint(2UL))))) {
            #line 66 "test/src/lines.act"
            ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("break path")), B_None, B_None, B_None, B_None);
            #line 67 "test/src/lines.act"
            break;
        }
        #line 68 "test/src/lines.act"
        ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(2, to$str("loop body"), self->i), B_None, B_None, B_None, B_None);
    }
    B_Iterator N_3iter = ((B_Iterator (*) (B_Iterable, B_list))W_1909->$class->__iter__)(W_1909, B_mk_list(3, toB_bigint(1UL) , toB_bigint(2UL) , toB_bigint(3UL)));
    if ($PUSH()) {
        #line 73 "test/src/lines.act"
        while (true) {
            B_bigint j = ((B_bigint (*) (B_Iterator))N_3iter->$class->__next__)(N_3iter);
            #line 74 "test/src/lines.act"
            if (ORD_B_bigint__eq__(((B_bigint)j), ((B_bigint)((B_bigint)toB_bigint(2UL))))) {
                #line 75 "test/src/lines.act"
                continue;
            }
            #line 76 "test/src/lines.act"
            ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(2, to$str("for j"), j), B_None, B_None, B_None, B_None);
        }
        $DROP();
    }
    else {
        B_BaseException N_5x = $POP();
        if ($ISINSTANCE0(N_5x, B_StopIteration)) {
        }
        else {
            $RAISE(N_5x);
        }
    }
    B_BaseException N_7xx;
    if ($PUSHF()) {
        if ($PUSH()) {
            #line 80 "test/src/lines.act"
            if (ORD_B_bigint__eq__(((B_bigint)self->v), ((B_bigint)((B_bigint)toB_bigint(0UL))))) {
                #line 81 "test/src/lines.act"
                $RAISE(((B_BaseException)B_ValueErrorG_new(to$str("boom"))));
            }
            #line 82 "test/src/lines.act"
            ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("unreached")), B_None, B_None, B_None, B_None);
            $DROP();
        }
        else {
            B_BaseException N_6x = $POP();
            B_ValueError e;
            if ($ISINSTANCE0(N_6x, B_ValueError)) {
                e = ((B_ValueError)N_6x);
                #line 84 "test/src/lines.act"
                ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(2, to$str("caught"), to$str("ValueError")), B_None, B_None, B_None, B_None);
            }
            else {
                $RAISE(N_6x);
            }
        }
        $RAISE(((B_BaseException)$SEQG_new()));
    }
    else {
        N_7xx = $POP();
        #line 86 "test/src/lines.act"
        ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("finally")), B_None, B_None, B_None, B_None);
        if ($ISINSTANCE0(N_7xx, $SEQ)) {
        }
        else {
            $RAISE(N_7xx);
        }
    }
    #line 89 "test/src/lines.act"
    $AFTER(toB_float(1), (($Cont)linesQ_L_20procG_new(self)));
    return (($R (*) (linesQ_main, $Cont))self->$class->nopG_local)(self, C_cont);
}
B_NoneType linesQ_L_21ContD___init__ (linesQ_L_21Cont L_self, linesQ_main self, B_Iterable W_1909, B_Number W_2124, $Cont C_cont) {
    L_self->self = self;
    L_self->W_1909 = W_1909;
    L_self->W_2124 = W_2124;
    L_self->C_cont = C_cont;
    return B_None;
}
$R linesQ_L_21ContD___call__ (linesQ_L_21Cont L_self, B_bigint G_1) {
    linesQ_main self = L_self->self;
    B_Iterable W_1909 = L_self->W_1909;
    B_Number W_2124 = L_self->W_2124;
    $Cont C_cont = L_self->C_cont;
    return linesQ_L_17C_9cont(self, W_1909, W_2124, C_cont, G_1);
}
void linesQ_L_21ContD___serialize__ (linesQ_L_21Cont self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->W_1909, state);
    $step_serialize(self->W_2124, state);
    $step_serialize(self->C_cont, state);
}
linesQ_L_21Cont linesQ_L_21ContD___deserialize__ (linesQ_L_21Cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_21Cont));
            self->$class = &linesQ_L_21ContG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_21Cont, state);
    }
    self->self = $step_deserialize(state);
    self->W_1909 = $step_deserialize(state);
    self->W_2124 = $step_deserialize(state);
    self->C_cont = $step_deserialize(state);
    return self;
}
linesQ_L_21Cont linesQ_L_21ContG_new(linesQ_main G_1, B_Iterable G_2, B_Number G_3, $Cont G_4) {
    linesQ_L_21Cont $tmp = acton_malloc(sizeof(struct linesQ_L_21Cont));
    $tmp->$class = &linesQ_L_21ContG_methods;
    linesQ_L_21ContG_methods.__init__($tmp, G_1, G_2, G_3, G_4);
    return $tmp;
}
struct linesQ_L_21ContG_class linesQ_L_21ContG_methods;
$R linesQ_L_12C_7cont (linesQ_main self, B_Iterable W_1909, B_Number W_2124, $Cont C_cont, linesQ_Bepa C_8res) {
    #line 34 "test/src/lines.act"
    self->b = C_8res;
    #line 35 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("-----")), B_None, B_None, B_None, B_None);
    #line 36 "test/src/lines.act"
    ((B_Msg (*) (linesQ_Apa, $action))self->a->$class->setup)(self->a, (($action)linesQ_L_14actionG_new(self->a)));
    #line 37 "test/src/lines.act"
    self->x = ((B_Msg (*) (linesQ_Apa, $action))self->a->$class->compute)(self->a, (($action)linesQ_L_16actionG_new(self->b)));
    return $AWAIT((($Cont)linesQ_L_21ContG_new(self, W_1909, W_2124, C_cont)), self->x);
}
B_NoneType linesQ_L_22ContD___init__ (linesQ_L_22Cont L_self, linesQ_main self, B_Iterable W_1909, B_Number W_2124, $Cont C_cont) {
    L_self->self = self;
    L_self->W_1909 = W_1909;
    L_self->W_2124 = W_2124;
    L_self->C_cont = C_cont;
    return B_None;
}
$R linesQ_L_22ContD___call__ (linesQ_L_22Cont L_self, linesQ_Bepa G_1) {
    linesQ_main self = L_self->self;
    B_Iterable W_1909 = L_self->W_1909;
    B_Number W_2124 = L_self->W_2124;
    $Cont C_cont = L_self->C_cont;
    return linesQ_L_12C_7cont(self, W_1909, W_2124, C_cont, G_1);
}
void linesQ_L_22ContD___serialize__ (linesQ_L_22Cont self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->W_1909, state);
    $step_serialize(self->W_2124, state);
    $step_serialize(self->C_cont, state);
}
linesQ_L_22Cont linesQ_L_22ContD___deserialize__ (linesQ_L_22Cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_22Cont));
            self->$class = &linesQ_L_22ContG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_22Cont, state);
    }
    self->self = $step_deserialize(state);
    self->W_1909 = $step_deserialize(state);
    self->W_2124 = $step_deserialize(state);
    self->C_cont = $step_deserialize(state);
    return self;
}
linesQ_L_22Cont linesQ_L_22ContG_new(linesQ_main G_1, B_Iterable G_2, B_Number G_3, $Cont G_4) {
    linesQ_L_22Cont $tmp = acton_malloc(sizeof(struct linesQ_L_22Cont));
    $tmp->$class = &linesQ_L_22ContG_methods;
    linesQ_L_22ContG_methods.__init__($tmp, G_1, G_2, G_3, G_4);
    return $tmp;
}
struct linesQ_L_22ContG_class linesQ_L_22ContG_methods;
$R linesQ_L_11C_5cont (linesQ_main self, B_Iterable W_1909, B_Number W_2124, $Cont C_cont, linesQ_Apa C_6res) {
    #line 33 "test/src/lines.act"
    self->a = C_6res;
    return linesQ_BepaG_newact((($Cont)linesQ_L_22ContG_new(self, W_1909, W_2124, C_cont)));
}
B_NoneType linesQ_L_23ContD___init__ (linesQ_L_23Cont L_self, linesQ_main self, B_Iterable W_1909, B_Number W_2124, $Cont C_cont) {
    L_self->self = self;
    L_self->W_1909 = W_1909;
    L_self->W_2124 = W_2124;
    L_self->C_cont = C_cont;
    return B_None;
}
$R linesQ_L_23ContD___call__ (linesQ_L_23Cont L_self, linesQ_Apa G_1) {
    linesQ_main self = L_self->self;
    B_Iterable W_1909 = L_self->W_1909;
    B_Number W_2124 = L_self->W_2124;
    $Cont C_cont = L_self->C_cont;
    return linesQ_L_11C_5cont(self, W_1909, W_2124, C_cont, G_1);
}
void linesQ_L_23ContD___serialize__ (linesQ_L_23Cont self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->W_1909, state);
    $step_serialize(self->W_2124, state);
    $step_serialize(self->C_cont, state);
}
linesQ_L_23Cont linesQ_L_23ContD___deserialize__ (linesQ_L_23Cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_23Cont));
            self->$class = &linesQ_L_23ContG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_23Cont, state);
    }
    self->self = $step_deserialize(state);
    self->W_1909 = $step_deserialize(state);
    self->W_2124 = $step_deserialize(state);
    self->C_cont = $step_deserialize(state);
    return self;
}
linesQ_L_23Cont linesQ_L_23ContG_new(linesQ_main G_1, B_Iterable G_2, B_Number G_3, $Cont G_4) {
    linesQ_L_23Cont $tmp = acton_malloc(sizeof(struct linesQ_L_23Cont));
    $tmp->$class = &linesQ_L_23ContG_methods;
    linesQ_L_23ContG_methods.__init__($tmp, G_1, G_2, G_3, G_4);
    return $tmp;
}
struct linesQ_L_23ContG_class linesQ_L_23ContG_methods;
B_NoneType linesQ_L_24procD___init__ (linesQ_L_24proc L_self, linesQ_main self, B_bigint i) {
    L_self->self = self;
    L_self->i = i;
    return B_None;
}
$R linesQ_L_24procD___call__ (linesQ_L_24proc L_self, $Cont C_cont) {
    linesQ_main self = L_self->self;
    B_bigint i = L_self->i;
    return (($R (*) (linesQ_main, $Cont, B_bigint))self->$class->myprocG_local)(self, C_cont, i);
}
$R linesQ_L_24procD___exec__ (linesQ_L_24proc L_self, $Cont C_cont) {
    return (($R (*) (linesQ_L_24proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void linesQ_L_24procD___serialize__ (linesQ_L_24proc self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->i, state);
}
linesQ_L_24proc linesQ_L_24procD___deserialize__ (linesQ_L_24proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_24proc));
            self->$class = &linesQ_L_24procG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_24proc, state);
    }
    self->self = $step_deserialize(state);
    self->i = $step_deserialize(state);
    return self;
}
linesQ_L_24proc linesQ_L_24procG_new(linesQ_main G_1, B_bigint G_2) {
    linesQ_L_24proc $tmp = acton_malloc(sizeof(struct linesQ_L_24proc));
    $tmp->$class = &linesQ_L_24procG_methods;
    linesQ_L_24procG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct linesQ_L_24procG_class linesQ_L_24procG_methods;
B_NoneType linesQ_L_25procD___init__ (linesQ_L_25proc L_self, linesQ_main self) {
    L_self->self = self;
    return B_None;
}
$R linesQ_L_25procD___call__ (linesQ_L_25proc L_self, $Cont C_cont) {
    linesQ_main self = L_self->self;
    return (($R (*) (linesQ_main, $Cont))self->$class->nopG_local)(self, C_cont);
}
$R linesQ_L_25procD___exec__ (linesQ_L_25proc L_self, $Cont C_cont) {
    return (($R (*) (linesQ_L_25proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void linesQ_L_25procD___serialize__ (linesQ_L_25proc self, $Serial$state state) {
    $step_serialize(self->self, state);
}
linesQ_L_25proc linesQ_L_25procD___deserialize__ (linesQ_L_25proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_25proc));
            self->$class = &linesQ_L_25procG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_25proc, state);
    }
    self->self = $step_deserialize(state);
    return self;
}
linesQ_L_25proc linesQ_L_25procG_new(linesQ_main G_1) {
    linesQ_L_25proc $tmp = acton_malloc(sizeof(struct linesQ_L_25proc));
    $tmp->$class = &linesQ_L_25procG_methods;
    linesQ_L_25procG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct linesQ_L_25procG_class linesQ_L_25procG_methods;
$R linesQ_L_26C_11cont ($Cont C_cont, linesQ_Apa G_act, B_NoneType C_12res) {
    return $R_CONT(C_cont, G_act);
}
B_NoneType linesQ_L_27ContD___init__ (linesQ_L_27Cont L_self, $Cont C_cont, linesQ_Apa G_act) {
    L_self->C_cont = C_cont;
    L_self->G_act = G_act;
    return B_None;
}
$R linesQ_L_27ContD___call__ (linesQ_L_27Cont L_self, B_NoneType G_1) {
    $Cont C_cont = L_self->C_cont;
    linesQ_Apa G_act = L_self->G_act;
    return linesQ_L_26C_11cont(C_cont, G_act, G_1);
}
void linesQ_L_27ContD___serialize__ (linesQ_L_27Cont self, $Serial$state state) {
    $step_serialize(self->C_cont, state);
    $step_serialize(self->G_act, state);
}
linesQ_L_27Cont linesQ_L_27ContD___deserialize__ (linesQ_L_27Cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_27Cont));
            self->$class = &linesQ_L_27ContG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_27Cont, state);
    }
    self->C_cont = $step_deserialize(state);
    self->G_act = $step_deserialize(state);
    return self;
}
linesQ_L_27Cont linesQ_L_27ContG_new($Cont G_1, linesQ_Apa G_2) {
    linesQ_L_27Cont $tmp = acton_malloc(sizeof(struct linesQ_L_27Cont));
    $tmp->$class = &linesQ_L_27ContG_methods;
    linesQ_L_27ContG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct linesQ_L_27ContG_class linesQ_L_27ContG_methods;
B_NoneType linesQ_L_28procD___init__ (linesQ_L_28proc L_self, linesQ_Apa G_act) {
    L_self->G_act = G_act;
    return B_None;
}
$R linesQ_L_28procD___call__ (linesQ_L_28proc L_self, $Cont C_cont) {
    linesQ_Apa G_act = L_self->G_act;
    return (($R (*) (linesQ_Apa, $Cont))G_act->$class->__init__)(G_act, C_cont);
}
$R linesQ_L_28procD___exec__ (linesQ_L_28proc L_self, $Cont C_cont) {
    return (($R (*) (linesQ_L_28proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void linesQ_L_28procD___serialize__ (linesQ_L_28proc self, $Serial$state state) {
    $step_serialize(self->G_act, state);
}
linesQ_L_28proc linesQ_L_28procD___deserialize__ (linesQ_L_28proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_28proc));
            self->$class = &linesQ_L_28procG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_28proc, state);
    }
    self->G_act = $step_deserialize(state);
    return self;
}
linesQ_L_28proc linesQ_L_28procG_new(linesQ_Apa G_1) {
    linesQ_L_28proc $tmp = acton_malloc(sizeof(struct linesQ_L_28proc));
    $tmp->$class = &linesQ_L_28procG_methods;
    linesQ_L_28procG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct linesQ_L_28procG_class linesQ_L_28procG_methods;
$R linesQ_L_29C_13cont ($Cont C_cont, linesQ_Bepa G_act, B_NoneType C_14res) {
    return $R_CONT(C_cont, G_act);
}
B_NoneType linesQ_L_30ContD___init__ (linesQ_L_30Cont L_self, $Cont C_cont, linesQ_Bepa G_act) {
    L_self->C_cont = C_cont;
    L_self->G_act = G_act;
    return B_None;
}
$R linesQ_L_30ContD___call__ (linesQ_L_30Cont L_self, B_NoneType G_1) {
    $Cont C_cont = L_self->C_cont;
    linesQ_Bepa G_act = L_self->G_act;
    return linesQ_L_29C_13cont(C_cont, G_act, G_1);
}
void linesQ_L_30ContD___serialize__ (linesQ_L_30Cont self, $Serial$state state) {
    $step_serialize(self->C_cont, state);
    $step_serialize(self->G_act, state);
}
linesQ_L_30Cont linesQ_L_30ContD___deserialize__ (linesQ_L_30Cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_30Cont));
            self->$class = &linesQ_L_30ContG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_30Cont, state);
    }
    self->C_cont = $step_deserialize(state);
    self->G_act = $step_deserialize(state);
    return self;
}
linesQ_L_30Cont linesQ_L_30ContG_new($Cont G_1, linesQ_Bepa G_2) {
    linesQ_L_30Cont $tmp = acton_malloc(sizeof(struct linesQ_L_30Cont));
    $tmp->$class = &linesQ_L_30ContG_methods;
    linesQ_L_30ContG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct linesQ_L_30ContG_class linesQ_L_30ContG_methods;
B_NoneType linesQ_L_31procD___init__ (linesQ_L_31proc L_self, linesQ_Bepa G_act) {
    L_self->G_act = G_act;
    return B_None;
}
$R linesQ_L_31procD___call__ (linesQ_L_31proc L_self, $Cont C_cont) {
    linesQ_Bepa G_act = L_self->G_act;
    return (($R (*) (linesQ_Bepa, $Cont))G_act->$class->__init__)(G_act, C_cont);
}
$R linesQ_L_31procD___exec__ (linesQ_L_31proc L_self, $Cont C_cont) {
    return (($R (*) (linesQ_L_31proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void linesQ_L_31procD___serialize__ (linesQ_L_31proc self, $Serial$state state) {
    $step_serialize(self->G_act, state);
}
linesQ_L_31proc linesQ_L_31procD___deserialize__ (linesQ_L_31proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_31proc));
            self->$class = &linesQ_L_31procG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_31proc, state);
    }
    self->G_act = $step_deserialize(state);
    return self;
}
linesQ_L_31proc linesQ_L_31procG_new(linesQ_Bepa G_1) {
    linesQ_L_31proc $tmp = acton_malloc(sizeof(struct linesQ_L_31proc));
    $tmp->$class = &linesQ_L_31procG_methods;
    linesQ_L_31procG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct linesQ_L_31procG_class linesQ_L_31procG_methods;
$R linesQ_L_32C_15cont ($Cont C_cont, linesQ_main G_act, B_NoneType C_16res) {
    return $R_CONT(C_cont, G_act);
}
B_NoneType linesQ_L_33ContD___init__ (linesQ_L_33Cont L_self, $Cont C_cont, linesQ_main G_act) {
    L_self->C_cont = C_cont;
    L_self->G_act = G_act;
    return B_None;
}
$R linesQ_L_33ContD___call__ (linesQ_L_33Cont L_self, B_NoneType G_1) {
    $Cont C_cont = L_self->C_cont;
    linesQ_main G_act = L_self->G_act;
    return linesQ_L_32C_15cont(C_cont, G_act, G_1);
}
void linesQ_L_33ContD___serialize__ (linesQ_L_33Cont self, $Serial$state state) {
    $step_serialize(self->C_cont, state);
    $step_serialize(self->G_act, state);
}
linesQ_L_33Cont linesQ_L_33ContD___deserialize__ (linesQ_L_33Cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_33Cont));
            self->$class = &linesQ_L_33ContG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_33Cont, state);
    }
    self->C_cont = $step_deserialize(state);
    self->G_act = $step_deserialize(state);
    return self;
}
linesQ_L_33Cont linesQ_L_33ContG_new($Cont G_1, linesQ_main G_2) {
    linesQ_L_33Cont $tmp = acton_malloc(sizeof(struct linesQ_L_33Cont));
    $tmp->$class = &linesQ_L_33ContG_methods;
    linesQ_L_33ContG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct linesQ_L_33ContG_class linesQ_L_33ContG_methods;
B_NoneType linesQ_L_34procD___init__ (linesQ_L_34proc L_self, linesQ_main G_act, B_Env env) {
    L_self->G_act = G_act;
    L_self->env = env;
    return B_None;
}
$R linesQ_L_34procD___call__ (linesQ_L_34proc L_self, $Cont C_cont) {
    linesQ_main G_act = L_self->G_act;
    B_Env env = L_self->env;
    return (($R (*) (linesQ_main, $Cont, B_Env))G_act->$class->__init__)(G_act, C_cont, env);
}
$R linesQ_L_34procD___exec__ (linesQ_L_34proc L_self, $Cont C_cont) {
    return (($R (*) (linesQ_L_34proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void linesQ_L_34procD___serialize__ (linesQ_L_34proc self, $Serial$state state) {
    $step_serialize(self->G_act, state);
    $step_serialize(self->env, state);
}
linesQ_L_34proc linesQ_L_34procD___deserialize__ (linesQ_L_34proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_34proc));
            self->$class = &linesQ_L_34procG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_34proc, state);
    }
    self->G_act = $step_deserialize(state);
    self->env = $step_deserialize(state);
    return self;
}
linesQ_L_34proc linesQ_L_34procG_new(linesQ_main G_1, B_Env G_2) {
    linesQ_L_34proc $tmp = acton_malloc(sizeof(struct linesQ_L_34proc));
    $tmp->$class = &linesQ_L_34procG_methods;
    linesQ_L_34procG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct linesQ_L_34procG_class linesQ_L_34procG_methods;
$R linesQ_ApaD___init__ (linesQ_Apa self, $Cont C_cont) {
    #line 2 "test/src/lines.act"
    self->apa = ((B_bigint)toB_bigint(2001UL));
    #line 6 "test/src/lines.act"
    self->apb = ((B_bigint)toB_bigint(2002UL));
    #line 16 "test/src/lines.act"
    self->y = ((B_bigint)toB_bigint(123UL));
    return (($R (*) (linesQ_Apa, $Cont, $action))self->$class->setupG_local)(self, (($Cont)linesQ_L_2ContG_new(self, C_cont)), (($action)linesQ_L_4actionG_new(self)));
}
#line 3 "test/src/lines.act"
$R linesQ_ApaD_setupG_local (linesQ_Apa self, $Cont C_cont, $action cb) {
    #line 4 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("setup")), B_None, B_None, B_None, B_None);
    #line 5 "test/src/lines.act"
    ((B_Msg (*) ($action, B_bigint))cb->$class->__asyn__)(cb, ((B_bigint)toB_bigint(0UL)));
    return $R_CONT(C_cont, B_None);
}
#line 7 "test/src/lines.act"
$R linesQ_ApaD_computeG_local (linesQ_Apa self, $Cont C_cont, $action cb) {
    #line 8 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("compute")), B_None, B_None, B_None, B_None);
    return $AWAIT((($Cont)linesQ_L_6ContG_new(cb, C_cont)), ((B_Msg)((B_Msg (*) ($action, B_bigint))cb->$class->__asyn__)(cb, ((B_bigint)toB_bigint(1UL)))));
}
#line 12 "test/src/lines.act"
$R linesQ_ApaD_noticeG_local (linesQ_Apa self, $Cont C_cont, B_bigint i) {
    #line 13 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("notice")), B_None, B_None, B_None, B_None);
    B_bigint N_1tmp = ((B_bigint (*) (B_Plus, B_bigint, B_bigint))linesQ_W_609->$class->__add__)(linesQ_W_609, i, ((B_bigint)toB_bigint(1UL)));
    return $R_CONT(C_cont, N_1tmp);
}
B_Msg linesQ_ApaD_setup (linesQ_Apa self, $action cb) {
    return $ASYNC((($Actor)self), (($Cont)linesQ_L_7procG_new(self, cb)));
}
B_Msg linesQ_ApaD_compute (linesQ_Apa self, $action cb) {
    return ((B_Msg)$ASYNC((($Actor)self), (($Cont)linesQ_L_8procG_new(self, cb))));
}
B_Msg linesQ_ApaD_notice (linesQ_Apa self, B_bigint i) {
    return ((B_Msg)$ASYNC((($Actor)self), (($Cont)linesQ_L_9procG_new(self, i))));
}
void linesQ_ApaD___serialize__ (linesQ_Apa self, $Serial$state state) {
    $ActorG_methods.__serialize__(($Actor)self, state);
    $step_serialize(self->apa, state);
    $step_serialize(self->apb, state);
    $step_serialize(self->y, state);
    $step_serialize(self->z, state);
}
linesQ_Apa linesQ_ApaD___deserialize__ (linesQ_Apa self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_Apa));
            self->$class = &linesQ_ApaG_methods;
            return self;
        }
        self = $DNEW(linesQ_Apa, state);
    }
    $ActorG_methods.__deserialize__(($Actor)self, state);
    self->apa = $step_deserialize(state);
    self->apb = $step_deserialize(state);
    self->y = $step_deserialize(state);
    self->z = $step_deserialize(state);
    return self;
}
void linesQ_ApaD__GC_finalizer (void *obj, void *cdata) {
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
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("Bepa")), B_None, B_None, B_None, B_None);
    return $R_CONT(C_cont, B_None);
}
#line 22 "test/src/lines.act"
$R linesQ_BepaD_callbackG_local (linesQ_Bepa self, $Cont C_cont, B_bigint i) {
    #line 23 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(2, to$str("callback"), i), B_None, B_None, B_None, B_None);
    B_bigint N_2tmp = ((B_bigint (*) (B_Plus, B_bigint, B_bigint))linesQ_W_609->$class->__add__)(linesQ_W_609, i, ((B_bigint)toB_bigint(1UL)));
    return $R_CONT(C_cont, N_2tmp);
}
B_Msg linesQ_BepaD_callback (linesQ_Bepa self, B_bigint i) {
    return ((B_Msg)$ASYNC((($Actor)self), (($Cont)linesQ_L_10procG_new(self, i))));
}
void linesQ_BepaD___serialize__ (linesQ_Bepa self, $Serial$state state) {
    $ActorG_methods.__serialize__(($Actor)self, state);
}
linesQ_Bepa linesQ_BepaD___deserialize__ (linesQ_Bepa self, $Serial$state state) {
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
void linesQ_BepaD__GC_finalizer (void *obj, void *cdata) {
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
    self->env = env;
    B_Iterable W_1909 = (B_Iterable)B_SequenceD_listG_witness->W_Collection;
    B_Number W_2124 = (B_Number)B_RealFloatD_floatG_witness;
    return linesQ_ApaG_newact((($Cont)linesQ_L_23ContG_new(self, W_1909, W_2124, C_cont)));
}
#line 28 "test/src/lines.act"
$R linesQ_mainD_myprocG_local (linesQ_main self, $Cont C_cont, B_bigint i) {
    #line 29 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(2, to$str("myproc"), i), B_None, B_None, B_None, B_None);
    #line 30 "test/src/lines.act"
    if (ORD_B_bigint__eq__(((B_bigint)i), ((B_bigint)((B_bigint)toB_bigint(2UL))))) {
        #line 31 "test/src/lines.act"
        ((B_Msg (*) (B_Env, B_int))self->env->$class->exit)(self->env, toB_int(0LL));
    }
    return $R_CONT(C_cont, i);
}
#line 92 "test/src/lines.act"
$R linesQ_mainD_nopG_local (linesQ_main self, $Cont C_cont) {
    #line 93 "test/src/lines.act"
    return $R_CONT(C_cont, B_None);
}
B_Msg linesQ_mainD_myproc (linesQ_main self, B_bigint i) {
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
    $step_serialize(self->r, state);
    $step_serialize(self->v, state);
    $step_serialize(self->i, state);
}
linesQ_main linesQ_mainD___deserialize__ (linesQ_main self, $Serial$state state) {
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
    self->r = $step_deserialize(state);
    self->v = $step_deserialize(state);
    self->i = $step_deserialize(state);
    return self;
}
void linesQ_mainD__GC_finalizer (void *obj, void *cdata) {
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
    if ((void*)G_act->$class->__cleanup__ != (void*)$ActorD___cleanup__) $GCfinalizer(G_act, linesQ_ApaD__GC_finalizer);
    return $AWAIT((($Cont)linesQ_L_27ContG_new(C_cont, G_act)), $ASYNC((($Actor)G_act), (($Cont)linesQ_L_28procG_new(G_act))));
}
$R linesQ_BepaG_newact ($Cont C_cont) {
    linesQ_Bepa G_act = $NEWACTOR(linesQ_Bepa);
    if ((void*)G_act->$class->__cleanup__ != (void*)$ActorD___cleanup__) $GCfinalizer(G_act, linesQ_BepaD__GC_finalizer);
    return $AWAIT((($Cont)linesQ_L_30ContG_new(C_cont, G_act)), $ASYNC((($Actor)G_act), (($Cont)linesQ_L_31procG_new(G_act))));
}
$R linesQ_mainG_newact ($Cont C_cont, B_Env env) {
    linesQ_main G_act = $NEWACTOR(linesQ_main);
    if ((void*)G_act->$class->__cleanup__ != (void*)$ActorD___cleanup__) $GCfinalizer(G_act, linesQ_mainD__GC_finalizer);
    return $AWAIT((($Cont)linesQ_L_33ContG_new(C_cont, G_act)), $ASYNC((($Actor)G_act), (($Cont)linesQ_L_34procG_new(G_act, env))));
}
int linesQ_done$ = 0;
void linesQ___init__ () {
    if (linesQ_done$) return;
    linesQ_done$ = 1;
    {
        linesQ_L_2ContG_methods.$GCINFO = "linesQ_L_2Cont";
        linesQ_L_2ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        linesQ_L_2ContG_methods.__bool__ = (B_bool (*) (linesQ_L_2Cont))B_valueG_methods.__bool__;
        linesQ_L_2ContG_methods.__str__ = (B_str (*) (linesQ_L_2Cont))B_valueG_methods.__str__;
        linesQ_L_2ContG_methods.__repr__ = (B_str (*) (linesQ_L_2Cont))B_valueG_methods.__repr__;
        linesQ_L_2ContG_methods.__init__ = linesQ_L_2ContD___init__;
        linesQ_L_2ContG_methods.__call__ = linesQ_L_2ContD___call__;
        linesQ_L_2ContG_methods.__serialize__ = linesQ_L_2ContD___serialize__;
        linesQ_L_2ContG_methods.__deserialize__ = linesQ_L_2ContD___deserialize__;
        $register(&linesQ_L_2ContG_methods);
    }
    {
        linesQ_L_4actionG_methods.$GCINFO = "linesQ_L_4action";
        linesQ_L_4actionG_methods.$superclass = ($SuperG_class)&$actionG_methods;
        linesQ_L_4actionG_methods.__bool__ = (B_bool (*) (linesQ_L_4action))B_valueG_methods.__bool__;
        linesQ_L_4actionG_methods.__str__ = (B_str (*) (linesQ_L_4action))B_valueG_methods.__str__;
        linesQ_L_4actionG_methods.__repr__ = (B_str (*) (linesQ_L_4action))B_valueG_methods.__repr__;
        linesQ_L_4actionG_methods.__init__ = linesQ_L_4actionD___init__;
        linesQ_L_4actionG_methods.__call__ = linesQ_L_4actionD___call__;
        linesQ_L_4actionG_methods.__exec__ = linesQ_L_4actionD___exec__;
        linesQ_L_4actionG_methods.__asyn__ = linesQ_L_4actionD___asyn__;
        linesQ_L_4actionG_methods.__serialize__ = linesQ_L_4actionD___serialize__;
        linesQ_L_4actionG_methods.__deserialize__ = linesQ_L_4actionD___deserialize__;
        $register(&linesQ_L_4actionG_methods);
    }
    {
        linesQ_L_6ContG_methods.$GCINFO = "linesQ_L_6Cont";
        linesQ_L_6ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        linesQ_L_6ContG_methods.__bool__ = (B_bool (*) (linesQ_L_6Cont))B_valueG_methods.__bool__;
        linesQ_L_6ContG_methods.__str__ = (B_str (*) (linesQ_L_6Cont))B_valueG_methods.__str__;
        linesQ_L_6ContG_methods.__repr__ = (B_str (*) (linesQ_L_6Cont))B_valueG_methods.__repr__;
        linesQ_L_6ContG_methods.__init__ = linesQ_L_6ContD___init__;
        linesQ_L_6ContG_methods.__call__ = linesQ_L_6ContD___call__;
        linesQ_L_6ContG_methods.__serialize__ = linesQ_L_6ContD___serialize__;
        linesQ_L_6ContG_methods.__deserialize__ = linesQ_L_6ContD___deserialize__;
        $register(&linesQ_L_6ContG_methods);
    }
    {
        linesQ_L_7procG_methods.$GCINFO = "linesQ_L_7proc";
        linesQ_L_7procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        linesQ_L_7procG_methods.__bool__ = (B_bool (*) (linesQ_L_7proc))B_valueG_methods.__bool__;
        linesQ_L_7procG_methods.__str__ = (B_str (*) (linesQ_L_7proc))B_valueG_methods.__str__;
        linesQ_L_7procG_methods.__repr__ = (B_str (*) (linesQ_L_7proc))B_valueG_methods.__repr__;
        linesQ_L_7procG_methods.__init__ = linesQ_L_7procD___init__;
        linesQ_L_7procG_methods.__call__ = linesQ_L_7procD___call__;
        linesQ_L_7procG_methods.__exec__ = linesQ_L_7procD___exec__;
        linesQ_L_7procG_methods.__serialize__ = linesQ_L_7procD___serialize__;
        linesQ_L_7procG_methods.__deserialize__ = linesQ_L_7procD___deserialize__;
        $register(&linesQ_L_7procG_methods);
    }
    {
        linesQ_L_8procG_methods.$GCINFO = "linesQ_L_8proc";
        linesQ_L_8procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        linesQ_L_8procG_methods.__bool__ = (B_bool (*) (linesQ_L_8proc))B_valueG_methods.__bool__;
        linesQ_L_8procG_methods.__str__ = (B_str (*) (linesQ_L_8proc))B_valueG_methods.__str__;
        linesQ_L_8procG_methods.__repr__ = (B_str (*) (linesQ_L_8proc))B_valueG_methods.__repr__;
        linesQ_L_8procG_methods.__init__ = linesQ_L_8procD___init__;
        linesQ_L_8procG_methods.__call__ = linesQ_L_8procD___call__;
        linesQ_L_8procG_methods.__exec__ = linesQ_L_8procD___exec__;
        linesQ_L_8procG_methods.__serialize__ = linesQ_L_8procD___serialize__;
        linesQ_L_8procG_methods.__deserialize__ = linesQ_L_8procD___deserialize__;
        $register(&linesQ_L_8procG_methods);
    }
    {
        linesQ_L_9procG_methods.$GCINFO = "linesQ_L_9proc";
        linesQ_L_9procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        linesQ_L_9procG_methods.__bool__ = (B_bool (*) (linesQ_L_9proc))B_valueG_methods.__bool__;
        linesQ_L_9procG_methods.__str__ = (B_str (*) (linesQ_L_9proc))B_valueG_methods.__str__;
        linesQ_L_9procG_methods.__repr__ = (B_str (*) (linesQ_L_9proc))B_valueG_methods.__repr__;
        linesQ_L_9procG_methods.__init__ = linesQ_L_9procD___init__;
        linesQ_L_9procG_methods.__call__ = linesQ_L_9procD___call__;
        linesQ_L_9procG_methods.__exec__ = linesQ_L_9procD___exec__;
        linesQ_L_9procG_methods.__serialize__ = linesQ_L_9procD___serialize__;
        linesQ_L_9procG_methods.__deserialize__ = linesQ_L_9procD___deserialize__;
        $register(&linesQ_L_9procG_methods);
    }
    {
        linesQ_L_10procG_methods.$GCINFO = "linesQ_L_10proc";
        linesQ_L_10procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        linesQ_L_10procG_methods.__bool__ = (B_bool (*) (linesQ_L_10proc))B_valueG_methods.__bool__;
        linesQ_L_10procG_methods.__str__ = (B_str (*) (linesQ_L_10proc))B_valueG_methods.__str__;
        linesQ_L_10procG_methods.__repr__ = (B_str (*) (linesQ_L_10proc))B_valueG_methods.__repr__;
        linesQ_L_10procG_methods.__init__ = linesQ_L_10procD___init__;
        linesQ_L_10procG_methods.__call__ = linesQ_L_10procD___call__;
        linesQ_L_10procG_methods.__exec__ = linesQ_L_10procD___exec__;
        linesQ_L_10procG_methods.__serialize__ = linesQ_L_10procD___serialize__;
        linesQ_L_10procG_methods.__deserialize__ = linesQ_L_10procD___deserialize__;
        $register(&linesQ_L_10procG_methods);
    }
    {
        linesQ_L_14actionG_methods.$GCINFO = "linesQ_L_14action";
        linesQ_L_14actionG_methods.$superclass = ($SuperG_class)&$actionG_methods;
        linesQ_L_14actionG_methods.__bool__ = (B_bool (*) (linesQ_L_14action))B_valueG_methods.__bool__;
        linesQ_L_14actionG_methods.__str__ = (B_str (*) (linesQ_L_14action))B_valueG_methods.__str__;
        linesQ_L_14actionG_methods.__repr__ = (B_str (*) (linesQ_L_14action))B_valueG_methods.__repr__;
        linesQ_L_14actionG_methods.__init__ = linesQ_L_14actionD___init__;
        linesQ_L_14actionG_methods.__call__ = linesQ_L_14actionD___call__;
        linesQ_L_14actionG_methods.__exec__ = linesQ_L_14actionD___exec__;
        linesQ_L_14actionG_methods.__asyn__ = linesQ_L_14actionD___asyn__;
        linesQ_L_14actionG_methods.__serialize__ = linesQ_L_14actionD___serialize__;
        linesQ_L_14actionG_methods.__deserialize__ = linesQ_L_14actionD___deserialize__;
        $register(&linesQ_L_14actionG_methods);
    }
    {
        linesQ_L_16actionG_methods.$GCINFO = "linesQ_L_16action";
        linesQ_L_16actionG_methods.$superclass = ($SuperG_class)&$actionG_methods;
        linesQ_L_16actionG_methods.__bool__ = (B_bool (*) (linesQ_L_16action))B_valueG_methods.__bool__;
        linesQ_L_16actionG_methods.__str__ = (B_str (*) (linesQ_L_16action))B_valueG_methods.__str__;
        linesQ_L_16actionG_methods.__repr__ = (B_str (*) (linesQ_L_16action))B_valueG_methods.__repr__;
        linesQ_L_16actionG_methods.__init__ = linesQ_L_16actionD___init__;
        linesQ_L_16actionG_methods.__call__ = linesQ_L_16actionD___call__;
        linesQ_L_16actionG_methods.__exec__ = linesQ_L_16actionD___exec__;
        linesQ_L_16actionG_methods.__asyn__ = linesQ_L_16actionD___asyn__;
        linesQ_L_16actionG_methods.__serialize__ = linesQ_L_16actionD___serialize__;
        linesQ_L_16actionG_methods.__deserialize__ = linesQ_L_16actionD___deserialize__;
        $register(&linesQ_L_16actionG_methods);
    }
    {
        linesQ_L_19actionG_methods.$GCINFO = "linesQ_L_19action";
        linesQ_L_19actionG_methods.$superclass = ($SuperG_class)&$actionG_methods;
        linesQ_L_19actionG_methods.__bool__ = (B_bool (*) (linesQ_L_19action))B_valueG_methods.__bool__;
        linesQ_L_19actionG_methods.__str__ = (B_str (*) (linesQ_L_19action))B_valueG_methods.__str__;
        linesQ_L_19actionG_methods.__repr__ = (B_str (*) (linesQ_L_19action))B_valueG_methods.__repr__;
        linesQ_L_19actionG_methods.__init__ = linesQ_L_19actionD___init__;
        linesQ_L_19actionG_methods.__call__ = linesQ_L_19actionD___call__;
        linesQ_L_19actionG_methods.__exec__ = linesQ_L_19actionD___exec__;
        linesQ_L_19actionG_methods.__asyn__ = linesQ_L_19actionD___asyn__;
        linesQ_L_19actionG_methods.__serialize__ = linesQ_L_19actionD___serialize__;
        linesQ_L_19actionG_methods.__deserialize__ = linesQ_L_19actionD___deserialize__;
        $register(&linesQ_L_19actionG_methods);
    }
    {
        linesQ_L_20procG_methods.$GCINFO = "linesQ_L_20proc";
        linesQ_L_20procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        linesQ_L_20procG_methods.__bool__ = (B_bool (*) (linesQ_L_20proc))B_valueG_methods.__bool__;
        linesQ_L_20procG_methods.__str__ = (B_str (*) (linesQ_L_20proc))B_valueG_methods.__str__;
        linesQ_L_20procG_methods.__repr__ = (B_str (*) (linesQ_L_20proc))B_valueG_methods.__repr__;
        linesQ_L_20procG_methods.__init__ = linesQ_L_20procD___init__;
        linesQ_L_20procG_methods.__call__ = linesQ_L_20procD___call__;
        linesQ_L_20procG_methods.__exec__ = linesQ_L_20procD___exec__;
        linesQ_L_20procG_methods.__serialize__ = linesQ_L_20procD___serialize__;
        linesQ_L_20procG_methods.__deserialize__ = linesQ_L_20procD___deserialize__;
        $register(&linesQ_L_20procG_methods);
    }
    {
        linesQ_L_21ContG_methods.$GCINFO = "linesQ_L_21Cont";
        linesQ_L_21ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        linesQ_L_21ContG_methods.__bool__ = (B_bool (*) (linesQ_L_21Cont))B_valueG_methods.__bool__;
        linesQ_L_21ContG_methods.__str__ = (B_str (*) (linesQ_L_21Cont))B_valueG_methods.__str__;
        linesQ_L_21ContG_methods.__repr__ = (B_str (*) (linesQ_L_21Cont))B_valueG_methods.__repr__;
        linesQ_L_21ContG_methods.__init__ = linesQ_L_21ContD___init__;
        linesQ_L_21ContG_methods.__call__ = linesQ_L_21ContD___call__;
        linesQ_L_21ContG_methods.__serialize__ = linesQ_L_21ContD___serialize__;
        linesQ_L_21ContG_methods.__deserialize__ = linesQ_L_21ContD___deserialize__;
        $register(&linesQ_L_21ContG_methods);
    }
    {
        linesQ_L_22ContG_methods.$GCINFO = "linesQ_L_22Cont";
        linesQ_L_22ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        linesQ_L_22ContG_methods.__bool__ = (B_bool (*) (linesQ_L_22Cont))B_valueG_methods.__bool__;
        linesQ_L_22ContG_methods.__str__ = (B_str (*) (linesQ_L_22Cont))B_valueG_methods.__str__;
        linesQ_L_22ContG_methods.__repr__ = (B_str (*) (linesQ_L_22Cont))B_valueG_methods.__repr__;
        linesQ_L_22ContG_methods.__init__ = linesQ_L_22ContD___init__;
        linesQ_L_22ContG_methods.__call__ = linesQ_L_22ContD___call__;
        linesQ_L_22ContG_methods.__serialize__ = linesQ_L_22ContD___serialize__;
        linesQ_L_22ContG_methods.__deserialize__ = linesQ_L_22ContD___deserialize__;
        $register(&linesQ_L_22ContG_methods);
    }
    {
        linesQ_L_23ContG_methods.$GCINFO = "linesQ_L_23Cont";
        linesQ_L_23ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        linesQ_L_23ContG_methods.__bool__ = (B_bool (*) (linesQ_L_23Cont))B_valueG_methods.__bool__;
        linesQ_L_23ContG_methods.__str__ = (B_str (*) (linesQ_L_23Cont))B_valueG_methods.__str__;
        linesQ_L_23ContG_methods.__repr__ = (B_str (*) (linesQ_L_23Cont))B_valueG_methods.__repr__;
        linesQ_L_23ContG_methods.__init__ = linesQ_L_23ContD___init__;
        linesQ_L_23ContG_methods.__call__ = linesQ_L_23ContD___call__;
        linesQ_L_23ContG_methods.__serialize__ = linesQ_L_23ContD___serialize__;
        linesQ_L_23ContG_methods.__deserialize__ = linesQ_L_23ContD___deserialize__;
        $register(&linesQ_L_23ContG_methods);
    }
    {
        linesQ_L_24procG_methods.$GCINFO = "linesQ_L_24proc";
        linesQ_L_24procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        linesQ_L_24procG_methods.__bool__ = (B_bool (*) (linesQ_L_24proc))B_valueG_methods.__bool__;
        linesQ_L_24procG_methods.__str__ = (B_str (*) (linesQ_L_24proc))B_valueG_methods.__str__;
        linesQ_L_24procG_methods.__repr__ = (B_str (*) (linesQ_L_24proc))B_valueG_methods.__repr__;
        linesQ_L_24procG_methods.__init__ = linesQ_L_24procD___init__;
        linesQ_L_24procG_methods.__call__ = linesQ_L_24procD___call__;
        linesQ_L_24procG_methods.__exec__ = linesQ_L_24procD___exec__;
        linesQ_L_24procG_methods.__serialize__ = linesQ_L_24procD___serialize__;
        linesQ_L_24procG_methods.__deserialize__ = linesQ_L_24procD___deserialize__;
        $register(&linesQ_L_24procG_methods);
    }
    {
        linesQ_L_25procG_methods.$GCINFO = "linesQ_L_25proc";
        linesQ_L_25procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        linesQ_L_25procG_methods.__bool__ = (B_bool (*) (linesQ_L_25proc))B_valueG_methods.__bool__;
        linesQ_L_25procG_methods.__str__ = (B_str (*) (linesQ_L_25proc))B_valueG_methods.__str__;
        linesQ_L_25procG_methods.__repr__ = (B_str (*) (linesQ_L_25proc))B_valueG_methods.__repr__;
        linesQ_L_25procG_methods.__init__ = linesQ_L_25procD___init__;
        linesQ_L_25procG_methods.__call__ = linesQ_L_25procD___call__;
        linesQ_L_25procG_methods.__exec__ = linesQ_L_25procD___exec__;
        linesQ_L_25procG_methods.__serialize__ = linesQ_L_25procD___serialize__;
        linesQ_L_25procG_methods.__deserialize__ = linesQ_L_25procD___deserialize__;
        $register(&linesQ_L_25procG_methods);
    }
    {
        linesQ_L_27ContG_methods.$GCINFO = "linesQ_L_27Cont";
        linesQ_L_27ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        linesQ_L_27ContG_methods.__bool__ = (B_bool (*) (linesQ_L_27Cont))B_valueG_methods.__bool__;
        linesQ_L_27ContG_methods.__str__ = (B_str (*) (linesQ_L_27Cont))B_valueG_methods.__str__;
        linesQ_L_27ContG_methods.__repr__ = (B_str (*) (linesQ_L_27Cont))B_valueG_methods.__repr__;
        linesQ_L_27ContG_methods.__init__ = linesQ_L_27ContD___init__;
        linesQ_L_27ContG_methods.__call__ = linesQ_L_27ContD___call__;
        linesQ_L_27ContG_methods.__serialize__ = linesQ_L_27ContD___serialize__;
        linesQ_L_27ContG_methods.__deserialize__ = linesQ_L_27ContD___deserialize__;
        $register(&linesQ_L_27ContG_methods);
    }
    {
        linesQ_L_28procG_methods.$GCINFO = "linesQ_L_28proc";
        linesQ_L_28procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        linesQ_L_28procG_methods.__bool__ = (B_bool (*) (linesQ_L_28proc))B_valueG_methods.__bool__;
        linesQ_L_28procG_methods.__str__ = (B_str (*) (linesQ_L_28proc))B_valueG_methods.__str__;
        linesQ_L_28procG_methods.__repr__ = (B_str (*) (linesQ_L_28proc))B_valueG_methods.__repr__;
        linesQ_L_28procG_methods.__init__ = linesQ_L_28procD___init__;
        linesQ_L_28procG_methods.__call__ = linesQ_L_28procD___call__;
        linesQ_L_28procG_methods.__exec__ = linesQ_L_28procD___exec__;
        linesQ_L_28procG_methods.__serialize__ = linesQ_L_28procD___serialize__;
        linesQ_L_28procG_methods.__deserialize__ = linesQ_L_28procD___deserialize__;
        $register(&linesQ_L_28procG_methods);
    }
    {
        linesQ_L_30ContG_methods.$GCINFO = "linesQ_L_30Cont";
        linesQ_L_30ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        linesQ_L_30ContG_methods.__bool__ = (B_bool (*) (linesQ_L_30Cont))B_valueG_methods.__bool__;
        linesQ_L_30ContG_methods.__str__ = (B_str (*) (linesQ_L_30Cont))B_valueG_methods.__str__;
        linesQ_L_30ContG_methods.__repr__ = (B_str (*) (linesQ_L_30Cont))B_valueG_methods.__repr__;
        linesQ_L_30ContG_methods.__init__ = linesQ_L_30ContD___init__;
        linesQ_L_30ContG_methods.__call__ = linesQ_L_30ContD___call__;
        linesQ_L_30ContG_methods.__serialize__ = linesQ_L_30ContD___serialize__;
        linesQ_L_30ContG_methods.__deserialize__ = linesQ_L_30ContD___deserialize__;
        $register(&linesQ_L_30ContG_methods);
    }
    {
        linesQ_L_31procG_methods.$GCINFO = "linesQ_L_31proc";
        linesQ_L_31procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        linesQ_L_31procG_methods.__bool__ = (B_bool (*) (linesQ_L_31proc))B_valueG_methods.__bool__;
        linesQ_L_31procG_methods.__str__ = (B_str (*) (linesQ_L_31proc))B_valueG_methods.__str__;
        linesQ_L_31procG_methods.__repr__ = (B_str (*) (linesQ_L_31proc))B_valueG_methods.__repr__;
        linesQ_L_31procG_methods.__init__ = linesQ_L_31procD___init__;
        linesQ_L_31procG_methods.__call__ = linesQ_L_31procD___call__;
        linesQ_L_31procG_methods.__exec__ = linesQ_L_31procD___exec__;
        linesQ_L_31procG_methods.__serialize__ = linesQ_L_31procD___serialize__;
        linesQ_L_31procG_methods.__deserialize__ = linesQ_L_31procD___deserialize__;
        $register(&linesQ_L_31procG_methods);
    }
    {
        linesQ_L_33ContG_methods.$GCINFO = "linesQ_L_33Cont";
        linesQ_L_33ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        linesQ_L_33ContG_methods.__bool__ = (B_bool (*) (linesQ_L_33Cont))B_valueG_methods.__bool__;
        linesQ_L_33ContG_methods.__str__ = (B_str (*) (linesQ_L_33Cont))B_valueG_methods.__str__;
        linesQ_L_33ContG_methods.__repr__ = (B_str (*) (linesQ_L_33Cont))B_valueG_methods.__repr__;
        linesQ_L_33ContG_methods.__init__ = linesQ_L_33ContD___init__;
        linesQ_L_33ContG_methods.__call__ = linesQ_L_33ContD___call__;
        linesQ_L_33ContG_methods.__serialize__ = linesQ_L_33ContD___serialize__;
        linesQ_L_33ContG_methods.__deserialize__ = linesQ_L_33ContD___deserialize__;
        $register(&linesQ_L_33ContG_methods);
    }
    {
        linesQ_L_34procG_methods.$GCINFO = "linesQ_L_34proc";
        linesQ_L_34procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        linesQ_L_34procG_methods.__bool__ = (B_bool (*) (linesQ_L_34proc))B_valueG_methods.__bool__;
        linesQ_L_34procG_methods.__str__ = (B_str (*) (linesQ_L_34proc))B_valueG_methods.__str__;
        linesQ_L_34procG_methods.__repr__ = (B_str (*) (linesQ_L_34proc))B_valueG_methods.__repr__;
        linesQ_L_34procG_methods.__init__ = linesQ_L_34procD___init__;
        linesQ_L_34procG_methods.__call__ = linesQ_L_34procD___call__;
        linesQ_L_34procG_methods.__exec__ = linesQ_L_34procD___exec__;
        linesQ_L_34procG_methods.__serialize__ = linesQ_L_34procD___serialize__;
        linesQ_L_34procG_methods.__deserialize__ = linesQ_L_34procD___deserialize__;
        $register(&linesQ_L_34procG_methods);
    }
    {
        linesQ_ApaG_methods.$GCINFO = "linesQ_Apa";
        linesQ_ApaG_methods.$superclass = ($SuperG_class)&$ActorG_methods;
        linesQ_ApaG_methods.__bool__ = (B_bool (*) (linesQ_Apa))$ActorG_methods.__bool__;
        linesQ_ApaG_methods.__str__ = (B_str (*) (linesQ_Apa))$ActorG_methods.__str__;
        linesQ_ApaG_methods.__repr__ = (B_str (*) (linesQ_Apa))$ActorG_methods.__repr__;
        linesQ_ApaG_methods.__resume__ = (B_NoneType (*) (linesQ_Apa))$ActorG_methods.__resume__;
        linesQ_ApaG_methods.__cleanup__ = (B_NoneType (*) (linesQ_Apa))$ActorG_methods.__cleanup__;
        linesQ_ApaG_methods.__init__ = linesQ_ApaD___init__;
        linesQ_ApaG_methods.setupG_local = linesQ_ApaD_setupG_local;
        linesQ_ApaG_methods.computeG_local = linesQ_ApaD_computeG_local;
        linesQ_ApaG_methods.noticeG_local = linesQ_ApaD_noticeG_local;
        linesQ_ApaG_methods.setup = linesQ_ApaD_setup;
        linesQ_ApaG_methods.compute = linesQ_ApaD_compute;
        linesQ_ApaG_methods.notice = linesQ_ApaD_notice;
        linesQ_ApaG_methods.__serialize__ = linesQ_ApaD___serialize__;
        linesQ_ApaG_methods.__deserialize__ = linesQ_ApaD___deserialize__;
        $register(&linesQ_ApaG_methods);
    }
    {
        linesQ_BepaG_methods.$GCINFO = "linesQ_Bepa";
        linesQ_BepaG_methods.$superclass = ($SuperG_class)&$ActorG_methods;
        linesQ_BepaG_methods.__bool__ = (B_bool (*) (linesQ_Bepa))$ActorG_methods.__bool__;
        linesQ_BepaG_methods.__str__ = (B_str (*) (linesQ_Bepa))$ActorG_methods.__str__;
        linesQ_BepaG_methods.__repr__ = (B_str (*) (linesQ_Bepa))$ActorG_methods.__repr__;
        linesQ_BepaG_methods.__resume__ = (B_NoneType (*) (linesQ_Bepa))$ActorG_methods.__resume__;
        linesQ_BepaG_methods.__cleanup__ = (B_NoneType (*) (linesQ_Bepa))$ActorG_methods.__cleanup__;
        linesQ_BepaG_methods.__init__ = linesQ_BepaD___init__;
        linesQ_BepaG_methods.callbackG_local = linesQ_BepaD_callbackG_local;
        linesQ_BepaG_methods.callback = linesQ_BepaD_callback;
        linesQ_BepaG_methods.__serialize__ = linesQ_BepaD___serialize__;
        linesQ_BepaG_methods.__deserialize__ = linesQ_BepaD___deserialize__;
        $register(&linesQ_BepaG_methods);
    }
    {
        linesQ_mainG_methods.$GCINFO = "linesQ_main";
        linesQ_mainG_methods.$superclass = ($SuperG_class)&$ActorG_methods;
        linesQ_mainG_methods.__bool__ = (B_bool (*) (linesQ_main))$ActorG_methods.__bool__;
        linesQ_mainG_methods.__str__ = (B_str (*) (linesQ_main))$ActorG_methods.__str__;
        linesQ_mainG_methods.__repr__ = (B_str (*) (linesQ_main))$ActorG_methods.__repr__;
        linesQ_mainG_methods.__resume__ = (B_NoneType (*) (linesQ_main))$ActorG_methods.__resume__;
        linesQ_mainG_methods.__cleanup__ = (B_NoneType (*) (linesQ_main))$ActorG_methods.__cleanup__;
        linesQ_mainG_methods.__init__ = linesQ_mainD___init__;
        linesQ_mainG_methods.myprocG_local = linesQ_mainD_myprocG_local;
        linesQ_mainG_methods.nopG_local = linesQ_mainD_nopG_local;
        linesQ_mainG_methods.myproc = linesQ_mainD_myproc;
        linesQ_mainG_methods.nop = linesQ_mainD_nop;
        linesQ_mainG_methods.__serialize__ = linesQ_mainD___serialize__;
        linesQ_mainG_methods.__deserialize__ = linesQ_mainD___deserialize__;
        $register(&linesQ_mainG_methods);
    }
    B_Number W_19 = (B_Number)B_IntegralD_bigintG_witness;
    linesQ_W_19 = W_19;
    B_Times W_238 = (B_Times)B_IntegralD_bigintG_witness;
    linesQ_W_238 = W_238;
    B_Plus W_609 = (B_Plus)B_IntegralD_bigintG_witness;
    linesQ_W_609 = W_609;
    B_Eq W_784 = (B_Eq)B_OrdD_bigintG_witness;
    linesQ_W_784 = W_784;
    B_Ord W_1216 = (B_Ord)B_OrdD_bigintG_witness;
    linesQ_W_1216 = W_1216;
}