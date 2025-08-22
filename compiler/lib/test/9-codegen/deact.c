/* Acton source hash: test-hash */
#include "rts/common.h"
#include "out/types/deact.h"
B_Times deactQ_W_223;
B_Plus deactQ_W_586;
B_Eq deactQ_W_761;
$R deactQ_L_1C_1cont ($Cont C_cont, B_NoneType C_2res) {
    #line 15 "test/src/deact.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("Apa")), B_None, B_None, B_None, B_None);
    return $R_CONT(C_cont, B_None);
}
B_NoneType deactQ_L_2ContD___init__ (deactQ_L_2Cont L_self, $Cont C_cont) {
    L_self->C_cont = C_cont;
    return B_None;
}
$R deactQ_L_2ContD___call__ (deactQ_L_2Cont L_self, B_NoneType G_1) {
    $Cont C_cont = L_self->C_cont;
    return deactQ_L_1C_1cont(C_cont, G_1);
}
void deactQ_L_2ContD___serialize__ (deactQ_L_2Cont self, $Serial$state state) {
    $step_serialize(self->C_cont, state);
}
deactQ_L_2Cont deactQ_L_2ContD___deserialize__ (deactQ_L_2Cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_L_2Cont));
            self->$class = &deactQ_L_2ContG_methods;
            return self;
        }
        self = $DNEW(deactQ_L_2Cont, state);
    }
    self->C_cont = $step_deserialize(state);
    return self;
}
deactQ_L_2Cont deactQ_L_2ContG_new($Cont G_1) {
    deactQ_L_2Cont $tmp = acton_malloc(sizeof(struct deactQ_L_2Cont));
    $tmp->$class = &deactQ_L_2ContG_methods;
    deactQ_L_2ContG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct deactQ_L_2ContG_class deactQ_L_2ContG_methods;
B_NoneType deactQ_L_4actionD___init__ (deactQ_L_4action L_self, deactQ_Apa L_3obj) {
    L_self->L_3obj = L_3obj;
    return B_None;
}
$R deactQ_L_4actionD___call__ (deactQ_L_4action L_self, $Cont L_cont, B_int G_1) {
    return $AWAIT(L_cont, ((B_Msg)((B_Msg (*) (deactQ_L_4action, B_int))L_self->$class->__asyn__)(L_self, G_1)));
}
$R deactQ_L_4actionD___exec__ (deactQ_L_4action L_self, $Cont L_cont, B_int G_1) {
    return $R_CONT(L_cont, ((B_value)((B_Msg (*) (deactQ_L_4action, B_int))L_self->$class->__asyn__)(L_self, G_1)));
}
B_Msg deactQ_L_4actionD___asyn__ (deactQ_L_4action L_self, B_int G_1) {
    deactQ_Apa L_3obj = L_self->L_3obj;
    return ((B_Msg)((B_Msg (*) (deactQ_Apa, B_int))L_3obj->$class->notice)(L_3obj, G_1));
}
void deactQ_L_4actionD___serialize__ (deactQ_L_4action self, $Serial$state state) {
    $step_serialize(self->L_3obj, state);
}
deactQ_L_4action deactQ_L_4actionD___deserialize__ (deactQ_L_4action self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_L_4action));
            self->$class = &deactQ_L_4actionG_methods;
            return self;
        }
        self = $DNEW(deactQ_L_4action, state);
    }
    self->L_3obj = $step_deserialize(state);
    return self;
}
deactQ_L_4action deactQ_L_4actionG_new(deactQ_Apa G_1) {
    deactQ_L_4action $tmp = acton_malloc(sizeof(struct deactQ_L_4action));
    $tmp->$class = &deactQ_L_4actionG_methods;
    deactQ_L_4actionG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct deactQ_L_4actionG_class deactQ_L_4actionG_methods;
$R deactQ_L_5C_3cont ($action cb, $Cont C_cont, B_int C_4res) {
    #line 7 "test/src/deact.act"
    B_int v = C_4res;
    #line 8 "test/src/deact.act"
    B_Msg m = ((B_Msg)((B_Msg (*) ($action, B_int))cb->$class->__asyn__)(cb, to$int(2)));
    B_int N_tmp = ((B_int (*) (B_Times, B_int, B_int))deactQ_W_223->$class->__mul__)(deactQ_W_223, v, to$int(10));
    return $R_CONT(C_cont, N_tmp);
}
B_NoneType deactQ_L_6ContD___init__ (deactQ_L_6Cont L_self, $action cb, $Cont C_cont) {
    L_self->cb = cb;
    L_self->C_cont = C_cont;
    return B_None;
}
$R deactQ_L_6ContD___call__ (deactQ_L_6Cont L_self, B_int G_1) {
    $action cb = L_self->cb;
    $Cont C_cont = L_self->C_cont;
    return deactQ_L_5C_3cont(cb, C_cont, G_1);
}
void deactQ_L_6ContD___serialize__ (deactQ_L_6Cont self, $Serial$state state) {
    $step_serialize(self->cb, state);
    $step_serialize(self->C_cont, state);
}
deactQ_L_6Cont deactQ_L_6ContD___deserialize__ (deactQ_L_6Cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_L_6Cont));
            self->$class = &deactQ_L_6ContG_methods;
            return self;
        }
        self = $DNEW(deactQ_L_6Cont, state);
    }
    self->cb = $step_deserialize(state);
    self->C_cont = $step_deserialize(state);
    return self;
}
deactQ_L_6Cont deactQ_L_6ContG_new($action G_1, $Cont G_2) {
    deactQ_L_6Cont $tmp = acton_malloc(sizeof(struct deactQ_L_6Cont));
    $tmp->$class = &deactQ_L_6ContG_methods;
    deactQ_L_6ContG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct deactQ_L_6ContG_class deactQ_L_6ContG_methods;
B_NoneType deactQ_L_7procD___init__ (deactQ_L_7proc L_self, deactQ_Apa self, $action cb) {
    L_self->self = self;
    L_self->cb = cb;
    return B_None;
}
$R deactQ_L_7procD___call__ (deactQ_L_7proc L_self, $Cont C_cont) {
    deactQ_Apa self = L_self->self;
    $action cb = L_self->cb;
    return (($R (*) (deactQ_Apa, $Cont, $action))self->$class->setupG_local)(self, C_cont, cb);
}
$R deactQ_L_7procD___exec__ (deactQ_L_7proc L_self, $Cont C_cont) {
    return (($R (*) (deactQ_L_7proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void deactQ_L_7procD___serialize__ (deactQ_L_7proc self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->cb, state);
}
deactQ_L_7proc deactQ_L_7procD___deserialize__ (deactQ_L_7proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_L_7proc));
            self->$class = &deactQ_L_7procG_methods;
            return self;
        }
        self = $DNEW(deactQ_L_7proc, state);
    }
    self->self = $step_deserialize(state);
    self->cb = $step_deserialize(state);
    return self;
}
deactQ_L_7proc deactQ_L_7procG_new(deactQ_Apa G_1, $action G_2) {
    deactQ_L_7proc $tmp = acton_malloc(sizeof(struct deactQ_L_7proc));
    $tmp->$class = &deactQ_L_7procG_methods;
    deactQ_L_7procG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct deactQ_L_7procG_class deactQ_L_7procG_methods;
B_NoneType deactQ_L_8procD___init__ (deactQ_L_8proc L_self, deactQ_Apa self, $action cb) {
    L_self->self = self;
    L_self->cb = cb;
    return B_None;
}
$R deactQ_L_8procD___call__ (deactQ_L_8proc L_self, $Cont C_cont) {
    deactQ_Apa self = L_self->self;
    $action cb = L_self->cb;
    return (($R (*) (deactQ_Apa, $Cont, $action))self->$class->computeG_local)(self, C_cont, cb);
}
$R deactQ_L_8procD___exec__ (deactQ_L_8proc L_self, $Cont C_cont) {
    return (($R (*) (deactQ_L_8proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void deactQ_L_8procD___serialize__ (deactQ_L_8proc self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->cb, state);
}
deactQ_L_8proc deactQ_L_8procD___deserialize__ (deactQ_L_8proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_L_8proc));
            self->$class = &deactQ_L_8procG_methods;
            return self;
        }
        self = $DNEW(deactQ_L_8proc, state);
    }
    self->self = $step_deserialize(state);
    self->cb = $step_deserialize(state);
    return self;
}
deactQ_L_8proc deactQ_L_8procG_new(deactQ_Apa G_1, $action G_2) {
    deactQ_L_8proc $tmp = acton_malloc(sizeof(struct deactQ_L_8proc));
    $tmp->$class = &deactQ_L_8procG_methods;
    deactQ_L_8procG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct deactQ_L_8procG_class deactQ_L_8procG_methods;
B_NoneType deactQ_L_9procD___init__ (deactQ_L_9proc L_self, deactQ_Apa self, B_int i) {
    L_self->self = self;
    L_self->i = i;
    return B_None;
}
$R deactQ_L_9procD___call__ (deactQ_L_9proc L_self, $Cont C_cont) {
    deactQ_Apa self = L_self->self;
    B_int i = L_self->i;
    return (($R (*) (deactQ_Apa, $Cont, B_int))self->$class->noticeG_local)(self, C_cont, i);
}
$R deactQ_L_9procD___exec__ (deactQ_L_9proc L_self, $Cont C_cont) {
    return (($R (*) (deactQ_L_9proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void deactQ_L_9procD___serialize__ (deactQ_L_9proc self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->i, state);
}
deactQ_L_9proc deactQ_L_9procD___deserialize__ (deactQ_L_9proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_L_9proc));
            self->$class = &deactQ_L_9procG_methods;
            return self;
        }
        self = $DNEW(deactQ_L_9proc, state);
    }
    self->self = $step_deserialize(state);
    self->i = $step_deserialize(state);
    return self;
}
deactQ_L_9proc deactQ_L_9procG_new(deactQ_Apa G_1, B_int G_2) {
    deactQ_L_9proc $tmp = acton_malloc(sizeof(struct deactQ_L_9proc));
    $tmp->$class = &deactQ_L_9procG_methods;
    deactQ_L_9procG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct deactQ_L_9procG_class deactQ_L_9procG_methods;
B_NoneType deactQ_L_10procD___init__ (deactQ_L_10proc L_self, deactQ_Bepa self, B_int i) {
    L_self->self = self;
    L_self->i = i;
    return B_None;
}
$R deactQ_L_10procD___call__ (deactQ_L_10proc L_self, $Cont C_cont) {
    deactQ_Bepa self = L_self->self;
    B_int i = L_self->i;
    return (($R (*) (deactQ_Bepa, $Cont, B_int))self->$class->callbackG_local)(self, C_cont, i);
}
$R deactQ_L_10procD___exec__ (deactQ_L_10proc L_self, $Cont C_cont) {
    return (($R (*) (deactQ_L_10proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void deactQ_L_10procD___serialize__ (deactQ_L_10proc self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->i, state);
}
deactQ_L_10proc deactQ_L_10procD___deserialize__ (deactQ_L_10proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_L_10proc));
            self->$class = &deactQ_L_10procG_methods;
            return self;
        }
        self = $DNEW(deactQ_L_10proc, state);
    }
    self->self = $step_deserialize(state);
    self->i = $step_deserialize(state);
    return self;
}
deactQ_L_10proc deactQ_L_10procG_new(deactQ_Bepa G_1, B_int G_2) {
    deactQ_L_10proc $tmp = acton_malloc(sizeof(struct deactQ_L_10proc));
    $tmp->$class = &deactQ_L_10procG_methods;
    deactQ_L_10procG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct deactQ_L_10procG_class deactQ_L_10procG_methods;
B_NoneType deactQ_L_14actionD___init__ (deactQ_L_14action L_self, deactQ_Apa L_13obj) {
    L_self->L_13obj = L_13obj;
    return B_None;
}
$R deactQ_L_14actionD___call__ (deactQ_L_14action L_self, $Cont L_cont, B_int G_1) {
    return $AWAIT(L_cont, ((B_Msg)((B_Msg (*) (deactQ_L_14action, B_int))L_self->$class->__asyn__)(L_self, G_1)));
}
$R deactQ_L_14actionD___exec__ (deactQ_L_14action L_self, $Cont L_cont, B_int G_1) {
    return $R_CONT(L_cont, ((B_value)((B_Msg (*) (deactQ_L_14action, B_int))L_self->$class->__asyn__)(L_self, G_1)));
}
B_Msg deactQ_L_14actionD___asyn__ (deactQ_L_14action L_self, B_int G_1) {
    deactQ_Apa L_13obj = L_self->L_13obj;
    return ((B_Msg)((B_Msg (*) (deactQ_Apa, B_int))L_13obj->$class->notice)(L_13obj, G_1));
}
void deactQ_L_14actionD___serialize__ (deactQ_L_14action self, $Serial$state state) {
    $step_serialize(self->L_13obj, state);
}
deactQ_L_14action deactQ_L_14actionD___deserialize__ (deactQ_L_14action self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_L_14action));
            self->$class = &deactQ_L_14actionG_methods;
            return self;
        }
        self = $DNEW(deactQ_L_14action, state);
    }
    self->L_13obj = $step_deserialize(state);
    return self;
}
deactQ_L_14action deactQ_L_14actionG_new(deactQ_Apa G_1) {
    deactQ_L_14action $tmp = acton_malloc(sizeof(struct deactQ_L_14action));
    $tmp->$class = &deactQ_L_14actionG_methods;
    deactQ_L_14actionG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct deactQ_L_14actionG_class deactQ_L_14actionG_methods;
B_NoneType deactQ_L_16actionD___init__ (deactQ_L_16action L_self, deactQ_Bepa L_15obj) {
    L_self->L_15obj = L_15obj;
    return B_None;
}
$R deactQ_L_16actionD___call__ (deactQ_L_16action L_self, $Cont L_cont, B_int G_1) {
    return $AWAIT(L_cont, ((B_Msg)((B_Msg (*) (deactQ_L_16action, B_int))L_self->$class->__asyn__)(L_self, G_1)));
}
$R deactQ_L_16actionD___exec__ (deactQ_L_16action L_self, $Cont L_cont, B_int G_1) {
    return $R_CONT(L_cont, ((B_value)((B_Msg (*) (deactQ_L_16action, B_int))L_self->$class->__asyn__)(L_self, G_1)));
}
B_Msg deactQ_L_16actionD___asyn__ (deactQ_L_16action L_self, B_int G_1) {
    deactQ_Bepa L_15obj = L_self->L_15obj;
    return ((B_Msg)((B_Msg (*) (deactQ_Bepa, B_int))L_15obj->$class->callback)(L_15obj, G_1));
}
void deactQ_L_16actionD___serialize__ (deactQ_L_16action self, $Serial$state state) {
    $step_serialize(self->L_15obj, state);
}
deactQ_L_16action deactQ_L_16actionD___deserialize__ (deactQ_L_16action self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_L_16action));
            self->$class = &deactQ_L_16actionG_methods;
            return self;
        }
        self = $DNEW(deactQ_L_16action, state);
    }
    self->L_15obj = $step_deserialize(state);
    return self;
}
deactQ_L_16action deactQ_L_16actionG_new(deactQ_Bepa G_1) {
    deactQ_L_16action $tmp = acton_malloc(sizeof(struct deactQ_L_16action));
    $tmp->$class = &deactQ_L_16actionG_methods;
    deactQ_L_16actionG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct deactQ_L_16actionG_class deactQ_L_16actionG_methods;
B_NoneType deactQ_L_19actionD___init__ (deactQ_L_19action L_self, deactQ_main L_18obj) {
    L_self->L_18obj = L_18obj;
    return B_None;
}
$R deactQ_L_19actionD___call__ (deactQ_L_19action L_self, $Cont L_cont, B_int G_1) {
    return $AWAIT(L_cont, ((B_Msg)((B_Msg (*) (deactQ_L_19action, B_int))L_self->$class->__asyn__)(L_self, G_1)));
}
$R deactQ_L_19actionD___exec__ (deactQ_L_19action L_self, $Cont L_cont, B_int G_1) {
    return $R_CONT(L_cont, ((B_value)((B_Msg (*) (deactQ_L_19action, B_int))L_self->$class->__asyn__)(L_self, G_1)));
}
B_Msg deactQ_L_19actionD___asyn__ (deactQ_L_19action L_self, B_int G_1) {
    deactQ_main L_18obj = L_self->L_18obj;
    return ((B_Msg)((B_Msg (*) (deactQ_main, B_int))L_18obj->$class->myproc)(L_18obj, G_1));
}
void deactQ_L_19actionD___serialize__ (deactQ_L_19action self, $Serial$state state) {
    $step_serialize(self->L_18obj, state);
}
deactQ_L_19action deactQ_L_19actionD___deserialize__ (deactQ_L_19action self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_L_19action));
            self->$class = &deactQ_L_19actionG_methods;
            return self;
        }
        self = $DNEW(deactQ_L_19action, state);
    }
    self->L_18obj = $step_deserialize(state);
    return self;
}
deactQ_L_19action deactQ_L_19actionG_new(deactQ_main G_1) {
    deactQ_L_19action $tmp = acton_malloc(sizeof(struct deactQ_L_19action));
    $tmp->$class = &deactQ_L_19actionG_methods;
    deactQ_L_19actionG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct deactQ_L_19actionG_class deactQ_L_19actionG_methods;
$R deactQ_L_17C_9cont (deactQ_main self, $Cont C_cont, B_int C_10res) {
    #line 34 "test/src/deact.act"
    self->r = C_10res;
    #line 35 "test/src/deact.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(2, to$str("r ="), self->r), B_None, B_None, B_None, B_None);
    #line 36 "test/src/deact.act"
    ((B_Msg (*) (deactQ_Apa, $action))self->a->$class->compute)(self->a, (($action)deactQ_L_19actionG_new(self)));
    #line 37 "test/src/deact.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("main")), B_None, B_None, B_None, B_None);
    return $R_CONT(C_cont, B_None);
}
B_NoneType deactQ_L_20ContD___init__ (deactQ_L_20Cont L_self, deactQ_main self, $Cont C_cont) {
    L_self->self = self;
    L_self->C_cont = C_cont;
    return B_None;
}
$R deactQ_L_20ContD___call__ (deactQ_L_20Cont L_self, B_int G_1) {
    deactQ_main self = L_self->self;
    $Cont C_cont = L_self->C_cont;
    return deactQ_L_17C_9cont(self, C_cont, G_1);
}
void deactQ_L_20ContD___serialize__ (deactQ_L_20Cont self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->C_cont, state);
}
deactQ_L_20Cont deactQ_L_20ContD___deserialize__ (deactQ_L_20Cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_L_20Cont));
            self->$class = &deactQ_L_20ContG_methods;
            return self;
        }
        self = $DNEW(deactQ_L_20Cont, state);
    }
    self->self = $step_deserialize(state);
    self->C_cont = $step_deserialize(state);
    return self;
}
deactQ_L_20Cont deactQ_L_20ContG_new(deactQ_main G_1, $Cont G_2) {
    deactQ_L_20Cont $tmp = acton_malloc(sizeof(struct deactQ_L_20Cont));
    $tmp->$class = &deactQ_L_20ContG_methods;
    deactQ_L_20ContG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct deactQ_L_20ContG_class deactQ_L_20ContG_methods;
$R deactQ_L_12C_7cont (deactQ_main self, $Cont C_cont, deactQ_Bepa C_8res) {
    #line 30 "test/src/deact.act"
    self->b = C_8res;
    #line 31 "test/src/deact.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("-----")), B_None, B_None, B_None, B_None);
    #line 32 "test/src/deact.act"
    ((B_Msg (*) (deactQ_Apa, $action))self->a->$class->setup)(self->a, (($action)deactQ_L_14actionG_new(self->a)));
    #line 33 "test/src/deact.act"
    self->x = ((B_Msg (*) (deactQ_Apa, $action))self->a->$class->compute)(self->a, (($action)deactQ_L_16actionG_new(self->b)));
    return $AWAIT((($Cont)deactQ_L_20ContG_new(self, C_cont)), self->x);
}
B_NoneType deactQ_L_21ContD___init__ (deactQ_L_21Cont L_self, deactQ_main self, $Cont C_cont) {
    L_self->self = self;
    L_self->C_cont = C_cont;
    return B_None;
}
$R deactQ_L_21ContD___call__ (deactQ_L_21Cont L_self, deactQ_Bepa G_1) {
    deactQ_main self = L_self->self;
    $Cont C_cont = L_self->C_cont;
    return deactQ_L_12C_7cont(self, C_cont, G_1);
}
void deactQ_L_21ContD___serialize__ (deactQ_L_21Cont self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->C_cont, state);
}
deactQ_L_21Cont deactQ_L_21ContD___deserialize__ (deactQ_L_21Cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_L_21Cont));
            self->$class = &deactQ_L_21ContG_methods;
            return self;
        }
        self = $DNEW(deactQ_L_21Cont, state);
    }
    self->self = $step_deserialize(state);
    self->C_cont = $step_deserialize(state);
    return self;
}
deactQ_L_21Cont deactQ_L_21ContG_new(deactQ_main G_1, $Cont G_2) {
    deactQ_L_21Cont $tmp = acton_malloc(sizeof(struct deactQ_L_21Cont));
    $tmp->$class = &deactQ_L_21ContG_methods;
    deactQ_L_21ContG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct deactQ_L_21ContG_class deactQ_L_21ContG_methods;
$R deactQ_L_11C_5cont (deactQ_main self, $Cont C_cont, deactQ_Apa C_6res) {
    #line 29 "test/src/deact.act"
    self->a = C_6res;
    return deactQ_BepaG_newact((($Cont)deactQ_L_21ContG_new(self, C_cont)));
}
B_NoneType deactQ_L_22ContD___init__ (deactQ_L_22Cont L_self, deactQ_main self, $Cont C_cont) {
    L_self->self = self;
    L_self->C_cont = C_cont;
    return B_None;
}
$R deactQ_L_22ContD___call__ (deactQ_L_22Cont L_self, deactQ_Apa G_1) {
    deactQ_main self = L_self->self;
    $Cont C_cont = L_self->C_cont;
    return deactQ_L_11C_5cont(self, C_cont, G_1);
}
void deactQ_L_22ContD___serialize__ (deactQ_L_22Cont self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->C_cont, state);
}
deactQ_L_22Cont deactQ_L_22ContD___deserialize__ (deactQ_L_22Cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_L_22Cont));
            self->$class = &deactQ_L_22ContG_methods;
            return self;
        }
        self = $DNEW(deactQ_L_22Cont, state);
    }
    self->self = $step_deserialize(state);
    self->C_cont = $step_deserialize(state);
    return self;
}
deactQ_L_22Cont deactQ_L_22ContG_new(deactQ_main G_1, $Cont G_2) {
    deactQ_L_22Cont $tmp = acton_malloc(sizeof(struct deactQ_L_22Cont));
    $tmp->$class = &deactQ_L_22ContG_methods;
    deactQ_L_22ContG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct deactQ_L_22ContG_class deactQ_L_22ContG_methods;
B_NoneType deactQ_L_23procD___init__ (deactQ_L_23proc L_self, deactQ_main self, B_int i) {
    L_self->self = self;
    L_self->i = i;
    return B_None;
}
$R deactQ_L_23procD___call__ (deactQ_L_23proc L_self, $Cont C_cont) {
    deactQ_main self = L_self->self;
    B_int i = L_self->i;
    return (($R (*) (deactQ_main, $Cont, B_int))self->$class->myprocG_local)(self, C_cont, i);
}
$R deactQ_L_23procD___exec__ (deactQ_L_23proc L_self, $Cont C_cont) {
    return (($R (*) (deactQ_L_23proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void deactQ_L_23procD___serialize__ (deactQ_L_23proc self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->i, state);
}
deactQ_L_23proc deactQ_L_23procD___deserialize__ (deactQ_L_23proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_L_23proc));
            self->$class = &deactQ_L_23procG_methods;
            return self;
        }
        self = $DNEW(deactQ_L_23proc, state);
    }
    self->self = $step_deserialize(state);
    self->i = $step_deserialize(state);
    return self;
}
deactQ_L_23proc deactQ_L_23procG_new(deactQ_main G_1, B_int G_2) {
    deactQ_L_23proc $tmp = acton_malloc(sizeof(struct deactQ_L_23proc));
    $tmp->$class = &deactQ_L_23procG_methods;
    deactQ_L_23procG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct deactQ_L_23procG_class deactQ_L_23procG_methods;
$R deactQ_L_24C_11cont ($Cont C_cont, deactQ_Apa G_act, B_NoneType C_12res) {
    return $R_CONT(C_cont, G_act);
}
B_NoneType deactQ_L_25ContD___init__ (deactQ_L_25Cont L_self, $Cont C_cont, deactQ_Apa G_act) {
    L_self->C_cont = C_cont;
    L_self->G_act = G_act;
    return B_None;
}
$R deactQ_L_25ContD___call__ (deactQ_L_25Cont L_self, B_NoneType G_1) {
    $Cont C_cont = L_self->C_cont;
    deactQ_Apa G_act = L_self->G_act;
    return deactQ_L_24C_11cont(C_cont, G_act, G_1);
}
void deactQ_L_25ContD___serialize__ (deactQ_L_25Cont self, $Serial$state state) {
    $step_serialize(self->C_cont, state);
    $step_serialize(self->G_act, state);
}
deactQ_L_25Cont deactQ_L_25ContD___deserialize__ (deactQ_L_25Cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_L_25Cont));
            self->$class = &deactQ_L_25ContG_methods;
            return self;
        }
        self = $DNEW(deactQ_L_25Cont, state);
    }
    self->C_cont = $step_deserialize(state);
    self->G_act = $step_deserialize(state);
    return self;
}
deactQ_L_25Cont deactQ_L_25ContG_new($Cont G_1, deactQ_Apa G_2) {
    deactQ_L_25Cont $tmp = acton_malloc(sizeof(struct deactQ_L_25Cont));
    $tmp->$class = &deactQ_L_25ContG_methods;
    deactQ_L_25ContG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct deactQ_L_25ContG_class deactQ_L_25ContG_methods;
B_NoneType deactQ_L_26procD___init__ (deactQ_L_26proc L_self, deactQ_Apa G_act) {
    L_self->G_act = G_act;
    return B_None;
}
$R deactQ_L_26procD___call__ (deactQ_L_26proc L_self, $Cont C_cont) {
    deactQ_Apa G_act = L_self->G_act;
    return (($R (*) (deactQ_Apa, $Cont))G_act->$class->__init__)(G_act, C_cont);
}
$R deactQ_L_26procD___exec__ (deactQ_L_26proc L_self, $Cont C_cont) {
    return (($R (*) (deactQ_L_26proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void deactQ_L_26procD___serialize__ (deactQ_L_26proc self, $Serial$state state) {
    $step_serialize(self->G_act, state);
}
deactQ_L_26proc deactQ_L_26procD___deserialize__ (deactQ_L_26proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_L_26proc));
            self->$class = &deactQ_L_26procG_methods;
            return self;
        }
        self = $DNEW(deactQ_L_26proc, state);
    }
    self->G_act = $step_deserialize(state);
    return self;
}
deactQ_L_26proc deactQ_L_26procG_new(deactQ_Apa G_1) {
    deactQ_L_26proc $tmp = acton_malloc(sizeof(struct deactQ_L_26proc));
    $tmp->$class = &deactQ_L_26procG_methods;
    deactQ_L_26procG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct deactQ_L_26procG_class deactQ_L_26procG_methods;
$R deactQ_L_27C_13cont ($Cont C_cont, deactQ_Bepa G_act, B_NoneType C_14res) {
    return $R_CONT(C_cont, G_act);
}
B_NoneType deactQ_L_28ContD___init__ (deactQ_L_28Cont L_self, $Cont C_cont, deactQ_Bepa G_act) {
    L_self->C_cont = C_cont;
    L_self->G_act = G_act;
    return B_None;
}
$R deactQ_L_28ContD___call__ (deactQ_L_28Cont L_self, B_NoneType G_1) {
    $Cont C_cont = L_self->C_cont;
    deactQ_Bepa G_act = L_self->G_act;
    return deactQ_L_27C_13cont(C_cont, G_act, G_1);
}
void deactQ_L_28ContD___serialize__ (deactQ_L_28Cont self, $Serial$state state) {
    $step_serialize(self->C_cont, state);
    $step_serialize(self->G_act, state);
}
deactQ_L_28Cont deactQ_L_28ContD___deserialize__ (deactQ_L_28Cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_L_28Cont));
            self->$class = &deactQ_L_28ContG_methods;
            return self;
        }
        self = $DNEW(deactQ_L_28Cont, state);
    }
    self->C_cont = $step_deserialize(state);
    self->G_act = $step_deserialize(state);
    return self;
}
deactQ_L_28Cont deactQ_L_28ContG_new($Cont G_1, deactQ_Bepa G_2) {
    deactQ_L_28Cont $tmp = acton_malloc(sizeof(struct deactQ_L_28Cont));
    $tmp->$class = &deactQ_L_28ContG_methods;
    deactQ_L_28ContG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct deactQ_L_28ContG_class deactQ_L_28ContG_methods;
B_NoneType deactQ_L_29procD___init__ (deactQ_L_29proc L_self, deactQ_Bepa G_act) {
    L_self->G_act = G_act;
    return B_None;
}
$R deactQ_L_29procD___call__ (deactQ_L_29proc L_self, $Cont C_cont) {
    deactQ_Bepa G_act = L_self->G_act;
    return (($R (*) (deactQ_Bepa, $Cont))G_act->$class->__init__)(G_act, C_cont);
}
$R deactQ_L_29procD___exec__ (deactQ_L_29proc L_self, $Cont C_cont) {
    return (($R (*) (deactQ_L_29proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void deactQ_L_29procD___serialize__ (deactQ_L_29proc self, $Serial$state state) {
    $step_serialize(self->G_act, state);
}
deactQ_L_29proc deactQ_L_29procD___deserialize__ (deactQ_L_29proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_L_29proc));
            self->$class = &deactQ_L_29procG_methods;
            return self;
        }
        self = $DNEW(deactQ_L_29proc, state);
    }
    self->G_act = $step_deserialize(state);
    return self;
}
deactQ_L_29proc deactQ_L_29procG_new(deactQ_Bepa G_1) {
    deactQ_L_29proc $tmp = acton_malloc(sizeof(struct deactQ_L_29proc));
    $tmp->$class = &deactQ_L_29procG_methods;
    deactQ_L_29procG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct deactQ_L_29procG_class deactQ_L_29procG_methods;
$R deactQ_L_30C_15cont ($Cont C_cont, deactQ_main G_act, B_NoneType C_16res) {
    return $R_CONT(C_cont, G_act);
}
B_NoneType deactQ_L_31ContD___init__ (deactQ_L_31Cont L_self, $Cont C_cont, deactQ_main G_act) {
    L_self->C_cont = C_cont;
    L_self->G_act = G_act;
    return B_None;
}
$R deactQ_L_31ContD___call__ (deactQ_L_31Cont L_self, B_NoneType G_1) {
    $Cont C_cont = L_self->C_cont;
    deactQ_main G_act = L_self->G_act;
    return deactQ_L_30C_15cont(C_cont, G_act, G_1);
}
void deactQ_L_31ContD___serialize__ (deactQ_L_31Cont self, $Serial$state state) {
    $step_serialize(self->C_cont, state);
    $step_serialize(self->G_act, state);
}
deactQ_L_31Cont deactQ_L_31ContD___deserialize__ (deactQ_L_31Cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_L_31Cont));
            self->$class = &deactQ_L_31ContG_methods;
            return self;
        }
        self = $DNEW(deactQ_L_31Cont, state);
    }
    self->C_cont = $step_deserialize(state);
    self->G_act = $step_deserialize(state);
    return self;
}
deactQ_L_31Cont deactQ_L_31ContG_new($Cont G_1, deactQ_main G_2) {
    deactQ_L_31Cont $tmp = acton_malloc(sizeof(struct deactQ_L_31Cont));
    $tmp->$class = &deactQ_L_31ContG_methods;
    deactQ_L_31ContG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct deactQ_L_31ContG_class deactQ_L_31ContG_methods;
B_NoneType deactQ_L_32procD___init__ (deactQ_L_32proc L_self, deactQ_main G_act, B_Env env) {
    L_self->G_act = G_act;
    L_self->env = env;
    return B_None;
}
$R deactQ_L_32procD___call__ (deactQ_L_32proc L_self, $Cont C_cont) {
    deactQ_main G_act = L_self->G_act;
    B_Env env = L_self->env;
    return (($R (*) (deactQ_main, $Cont, B_Env))G_act->$class->__init__)(G_act, C_cont, env);
}
$R deactQ_L_32procD___exec__ (deactQ_L_32proc L_self, $Cont C_cont) {
    return (($R (*) (deactQ_L_32proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void deactQ_L_32procD___serialize__ (deactQ_L_32proc self, $Serial$state state) {
    $step_serialize(self->G_act, state);
    $step_serialize(self->env, state);
}
deactQ_L_32proc deactQ_L_32procD___deserialize__ (deactQ_L_32proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_L_32proc));
            self->$class = &deactQ_L_32procG_methods;
            return self;
        }
        self = $DNEW(deactQ_L_32proc, state);
    }
    self->G_act = $step_deserialize(state);
    self->env = $step_deserialize(state);
    return self;
}
deactQ_L_32proc deactQ_L_32procG_new(deactQ_main G_1, B_Env G_2) {
    deactQ_L_32proc $tmp = acton_malloc(sizeof(struct deactQ_L_32proc));
    $tmp->$class = &deactQ_L_32procG_methods;
    deactQ_L_32procG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct deactQ_L_32procG_class deactQ_L_32procG_methods;
$R deactQ_ApaD___init__ (deactQ_Apa self, $Cont C_cont) {
    return (($R (*) (deactQ_Apa, $Cont, $action))self->$class->setupG_local)(self, (($Cont)deactQ_L_2ContG_new(C_cont)), (($action)deactQ_L_4actionG_new(self)));
}
#line 2 "test/src/deact.act"
$R deactQ_ApaD_setupG_local (deactQ_Apa self, $Cont C_cont, $action cb) {
    #line 3 "test/src/deact.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("setup")), B_None, B_None, B_None, B_None);
    #line 4 "test/src/deact.act"
    ((B_Msg (*) ($action, B_int))cb->$class->__asyn__)(cb, to$int(0));
    return $R_CONT(C_cont, B_None);
}
#line 5 "test/src/deact.act"
$R deactQ_ApaD_computeG_local (deactQ_Apa self, $Cont C_cont, $action cb) {
    #line 6 "test/src/deact.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("compute")), B_None, B_None, B_None, B_None);
    return $AWAIT((($Cont)deactQ_L_6ContG_new(cb, C_cont)), ((B_Msg)((B_Msg (*) ($action, B_int))cb->$class->__asyn__)(cb, to$int(1))));
}
#line 10 "test/src/deact.act"
$R deactQ_ApaD_noticeG_local (deactQ_Apa self, $Cont C_cont, B_int i) {
    #line 11 "test/src/deact.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("notice")), B_None, B_None, B_None, B_None);
    B_int N_1tmp = ((B_int (*) (B_Plus, B_int, B_int))deactQ_W_586->$class->__add__)(deactQ_W_586, i, to$int(1));
    return $R_CONT(C_cont, N_1tmp);
}
B_Msg deactQ_ApaD_setup (deactQ_Apa self, $action cb) {
    return $ASYNC((($Actor)self), (($Cont)deactQ_L_7procG_new(self, cb)));
}
B_Msg deactQ_ApaD_compute (deactQ_Apa self, $action cb) {
    return ((B_Msg)$ASYNC((($Actor)self), (($Cont)deactQ_L_8procG_new(self, cb))));
}
B_Msg deactQ_ApaD_notice (deactQ_Apa self, B_int i) {
    return ((B_Msg)$ASYNC((($Actor)self), (($Cont)deactQ_L_9procG_new(self, i))));
}
void deactQ_ApaD___serialize__ (deactQ_Apa self, $Serial$state state) {
    $ActorG_methods.__serialize__(($Actor)self, state);
}
deactQ_Apa deactQ_ApaD___deserialize__ (deactQ_Apa self, $Serial$state state) {
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
void deactQ_ApaD__GC_finalizer (void *obj, void *cdata) {
    deactQ_Apa self = (deactQ_Apa)obj;
    self->$class->__cleanup__(self);
}
$R deactQ_ApaG_new($Cont G_1) {
    deactQ_Apa $tmp = acton_malloc(sizeof(struct deactQ_Apa));
    $tmp->$class = &deactQ_ApaG_methods;
    return deactQ_ApaG_methods.__init__($tmp, $CONSTCONT($tmp, G_1));
}
struct deactQ_ApaG_class deactQ_ApaG_methods;
$R deactQ_BepaD___init__ (deactQ_Bepa self, $Cont C_cont) {
    #line 21 "test/src/deact.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("Bepa")), B_None, B_None, B_None, B_None);
    return $R_CONT(C_cont, B_None);
}
#line 18 "test/src/deact.act"
$R deactQ_BepaD_callbackG_local (deactQ_Bepa self, $Cont C_cont, B_int i) {
    #line 19 "test/src/deact.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(2, to$str("callback"), i), B_None, B_None, B_None, B_None);
    B_int N_2tmp = ((B_int (*) (B_Plus, B_int, B_int))deactQ_W_586->$class->__add__)(deactQ_W_586, i, to$int(1));
    return $R_CONT(C_cont, N_2tmp);
}
B_Msg deactQ_BepaD_callback (deactQ_Bepa self, B_int i) {
    return ((B_Msg)$ASYNC((($Actor)self), (($Cont)deactQ_L_10procG_new(self, i))));
}
void deactQ_BepaD___serialize__ (deactQ_Bepa self, $Serial$state state) {
    $ActorG_methods.__serialize__(($Actor)self, state);
}
deactQ_Bepa deactQ_BepaD___deserialize__ (deactQ_Bepa self, $Serial$state state) {
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
void deactQ_BepaD__GC_finalizer (void *obj, void *cdata) {
    deactQ_Bepa self = (deactQ_Bepa)obj;
    self->$class->__cleanup__(self);
}
$R deactQ_BepaG_new($Cont G_1) {
    deactQ_Bepa $tmp = acton_malloc(sizeof(struct deactQ_Bepa));
    $tmp->$class = &deactQ_BepaG_methods;
    return deactQ_BepaG_methods.__init__($tmp, $CONSTCONT($tmp, G_1));
}
struct deactQ_BepaG_class deactQ_BepaG_methods;
$R deactQ_mainD___init__ (deactQ_main self, $Cont C_cont, B_Env env) {
    self->env = env;
    return deactQ_ApaG_newact((($Cont)deactQ_L_22ContG_new(self, C_cont)));
}
#line 24 "test/src/deact.act"
$R deactQ_mainD_myprocG_local (deactQ_main self, $Cont C_cont, B_int i) {
    #line 25 "test/src/deact.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(2, to$str("myproc"), i), B_None, B_None, B_None, B_None);
<<<<<<< HEAD
    #line 26 "test/src/deact.act"
    if (ORD_B_int__eq__(((B_int)i), ((B_int)to$int(2)))) {
        ((B_Msg (*) (B_Env, B_int))self->env->$class->exit)(self->env, to$int(0));
    }
    return $R_CONT(C_cont, i);
}
B_Msg deactQ_mainD_myproc (deactQ_main self, B_int i) {
    return ((B_Msg)$ASYNC((($Actor)self), (($Cont)deactQ_L_23procG_new(self, i))));
}
void deactQ_mainD___serialize__ (deactQ_main self, $Serial$state state) {
    $ActorG_methods.__serialize__(($Actor)self, state);
    $step_serialize(self->env, state);
    $step_serialize(self->a, state);
    $step_serialize(self->b, state);
    $step_serialize(self->x, state);
    $step_serialize(self->r, state);
}
deactQ_main deactQ_mainD___deserialize__ (deactQ_main self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_main));
            self->$class = &deactQ_mainG_methods;
            return self;
        }
        self = $DNEW(deactQ_main, state);
    }
    $ActorG_methods.__deserialize__(($Actor)self, state);
    self->env = $step_deserialize(state);
    self->a = $step_deserialize(state);
    self->b = $step_deserialize(state);
    self->x = $step_deserialize(state);
    self->r = $step_deserialize(state);
    return self;
}
void deactQ_mainD__GC_finalizer (void *obj, void *cdata) {
    deactQ_main self = (deactQ_main)obj;
    self->$class->__cleanup__(self);
}
$R deactQ_mainG_new($Cont G_1, B_Env G_2) {
    deactQ_main $tmp = acton_malloc(sizeof(struct deactQ_main));
    $tmp->$class = &deactQ_mainG_methods;
    return deactQ_mainG_methods.__init__($tmp, $CONSTCONT($tmp, G_1), G_2);
}
struct deactQ_mainG_class deactQ_mainG_methods;
$R deactQ_ApaG_newact ($Cont C_cont) {
    deactQ_Apa G_act = $NEWACTOR(deactQ_Apa);
    if ((void*)G_act->$class->__cleanup__ != (void*)$ActorD___cleanup__) $GCfinalizer(G_act, deactQ_ApaD__GC_finalizer);
    return $AWAIT((($Cont)deactQ_L_25ContG_new(C_cont, G_act)), $ASYNC((($Actor)G_act), (($Cont)deactQ_L_26procG_new(G_act))));
}
$R deactQ_BepaG_newact ($Cont C_cont) {
    deactQ_Bepa G_act = $NEWACTOR(deactQ_Bepa);
    if ((void*)G_act->$class->__cleanup__ != (void*)$ActorD___cleanup__) $GCfinalizer(G_act, deactQ_BepaD__GC_finalizer);
    return $AWAIT((($Cont)deactQ_L_28ContG_new(C_cont, G_act)), $ASYNC((($Actor)G_act), (($Cont)deactQ_L_29procG_new(G_act))));
}
$R deactQ_mainG_newact ($Cont C_cont, B_Env env) {
    deactQ_main G_act = $NEWACTOR(deactQ_main);
    if ((void*)G_act->$class->__cleanup__ != (void*)$ActorD___cleanup__) $GCfinalizer(G_act, deactQ_mainD__GC_finalizer);
    return $AWAIT((($Cont)deactQ_L_31ContG_new(C_cont, G_act)), $ASYNC((($Actor)G_act), (($Cont)deactQ_L_32procG_new(G_act, env))));
}
int deactQ_done$ = 0;
void deactQ___init__ () {
    if (deactQ_done$) return;
    deactQ_done$ = 1;
    {
        deactQ_L_2ContG_methods.$GCINFO = "deactQ_L_2Cont";
        deactQ_L_2ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        deactQ_L_2ContG_methods.__bool__ = (B_bool (*) (deactQ_L_2Cont))B_valueG_methods.__bool__;
        deactQ_L_2ContG_methods.__str__ = (B_str (*) (deactQ_L_2Cont))B_valueG_methods.__str__;
        deactQ_L_2ContG_methods.__repr__ = (B_str (*) (deactQ_L_2Cont))B_valueG_methods.__repr__;
        deactQ_L_2ContG_methods.__init__ = deactQ_L_2ContD___init__;
        deactQ_L_2ContG_methods.__call__ = deactQ_L_2ContD___call__;
        deactQ_L_2ContG_methods.__serialize__ = deactQ_L_2ContD___serialize__;
        deactQ_L_2ContG_methods.__deserialize__ = deactQ_L_2ContD___deserialize__;
        $register(&deactQ_L_2ContG_methods);
    }
    {
        deactQ_L_4actionG_methods.$GCINFO = "deactQ_L_4action";
        deactQ_L_4actionG_methods.$superclass = ($SuperG_class)&$actionG_methods;
        deactQ_L_4actionG_methods.__bool__ = (B_bool (*) (deactQ_L_4action))B_valueG_methods.__bool__;
        deactQ_L_4actionG_methods.__str__ = (B_str (*) (deactQ_L_4action))B_valueG_methods.__str__;
        deactQ_L_4actionG_methods.__repr__ = (B_str (*) (deactQ_L_4action))B_valueG_methods.__repr__;
        deactQ_L_4actionG_methods.__init__ = deactQ_L_4actionD___init__;
        deactQ_L_4actionG_methods.__call__ = deactQ_L_4actionD___call__;
        deactQ_L_4actionG_methods.__exec__ = deactQ_L_4actionD___exec__;
        deactQ_L_4actionG_methods.__asyn__ = deactQ_L_4actionD___asyn__;
        deactQ_L_4actionG_methods.__serialize__ = deactQ_L_4actionD___serialize__;
        deactQ_L_4actionG_methods.__deserialize__ = deactQ_L_4actionD___deserialize__;
        $register(&deactQ_L_4actionG_methods);
    }
    {
        deactQ_L_6ContG_methods.$GCINFO = "deactQ_L_6Cont";
        deactQ_L_6ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        deactQ_L_6ContG_methods.__bool__ = (B_bool (*) (deactQ_L_6Cont))B_valueG_methods.__bool__;
        deactQ_L_6ContG_methods.__str__ = (B_str (*) (deactQ_L_6Cont))B_valueG_methods.__str__;
        deactQ_L_6ContG_methods.__repr__ = (B_str (*) (deactQ_L_6Cont))B_valueG_methods.__repr__;
        deactQ_L_6ContG_methods.__init__ = deactQ_L_6ContD___init__;
        deactQ_L_6ContG_methods.__call__ = deactQ_L_6ContD___call__;
        deactQ_L_6ContG_methods.__serialize__ = deactQ_L_6ContD___serialize__;
        deactQ_L_6ContG_methods.__deserialize__ = deactQ_L_6ContD___deserialize__;
        $register(&deactQ_L_6ContG_methods);
    }
    {
        deactQ_L_7procG_methods.$GCINFO = "deactQ_L_7proc";
        deactQ_L_7procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        deactQ_L_7procG_methods.__bool__ = (B_bool (*) (deactQ_L_7proc))B_valueG_methods.__bool__;
        deactQ_L_7procG_methods.__str__ = (B_str (*) (deactQ_L_7proc))B_valueG_methods.__str__;
        deactQ_L_7procG_methods.__repr__ = (B_str (*) (deactQ_L_7proc))B_valueG_methods.__repr__;
        deactQ_L_7procG_methods.__init__ = deactQ_L_7procD___init__;
        deactQ_L_7procG_methods.__call__ = deactQ_L_7procD___call__;
        deactQ_L_7procG_methods.__exec__ = deactQ_L_7procD___exec__;
        deactQ_L_7procG_methods.__serialize__ = deactQ_L_7procD___serialize__;
        deactQ_L_7procG_methods.__deserialize__ = deactQ_L_7procD___deserialize__;
        $register(&deactQ_L_7procG_methods);
    }
    {
        deactQ_L_8procG_methods.$GCINFO = "deactQ_L_8proc";
        deactQ_L_8procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        deactQ_L_8procG_methods.__bool__ = (B_bool (*) (deactQ_L_8proc))B_valueG_methods.__bool__;
        deactQ_L_8procG_methods.__str__ = (B_str (*) (deactQ_L_8proc))B_valueG_methods.__str__;
        deactQ_L_8procG_methods.__repr__ = (B_str (*) (deactQ_L_8proc))B_valueG_methods.__repr__;
        deactQ_L_8procG_methods.__init__ = deactQ_L_8procD___init__;
        deactQ_L_8procG_methods.__call__ = deactQ_L_8procD___call__;
        deactQ_L_8procG_methods.__exec__ = deactQ_L_8procD___exec__;
        deactQ_L_8procG_methods.__serialize__ = deactQ_L_8procD___serialize__;
        deactQ_L_8procG_methods.__deserialize__ = deactQ_L_8procD___deserialize__;
        $register(&deactQ_L_8procG_methods);
    }
    {
        deactQ_L_9procG_methods.$GCINFO = "deactQ_L_9proc";
        deactQ_L_9procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        deactQ_L_9procG_methods.__bool__ = (B_bool (*) (deactQ_L_9proc))B_valueG_methods.__bool__;
        deactQ_L_9procG_methods.__str__ = (B_str (*) (deactQ_L_9proc))B_valueG_methods.__str__;
        deactQ_L_9procG_methods.__repr__ = (B_str (*) (deactQ_L_9proc))B_valueG_methods.__repr__;
        deactQ_L_9procG_methods.__init__ = deactQ_L_9procD___init__;
        deactQ_L_9procG_methods.__call__ = deactQ_L_9procD___call__;
        deactQ_L_9procG_methods.__exec__ = deactQ_L_9procD___exec__;
        deactQ_L_9procG_methods.__serialize__ = deactQ_L_9procD___serialize__;
        deactQ_L_9procG_methods.__deserialize__ = deactQ_L_9procD___deserialize__;
        $register(&deactQ_L_9procG_methods);
    }
    {
        deactQ_L_10procG_methods.$GCINFO = "deactQ_L_10proc";
        deactQ_L_10procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        deactQ_L_10procG_methods.__bool__ = (B_bool (*) (deactQ_L_10proc))B_valueG_methods.__bool__;
        deactQ_L_10procG_methods.__str__ = (B_str (*) (deactQ_L_10proc))B_valueG_methods.__str__;
        deactQ_L_10procG_methods.__repr__ = (B_str (*) (deactQ_L_10proc))B_valueG_methods.__repr__;
        deactQ_L_10procG_methods.__init__ = deactQ_L_10procD___init__;
        deactQ_L_10procG_methods.__call__ = deactQ_L_10procD___call__;
        deactQ_L_10procG_methods.__exec__ = deactQ_L_10procD___exec__;
        deactQ_L_10procG_methods.__serialize__ = deactQ_L_10procD___serialize__;
        deactQ_L_10procG_methods.__deserialize__ = deactQ_L_10procD___deserialize__;
        $register(&deactQ_L_10procG_methods);
    }
    {
        deactQ_L_14actionG_methods.$GCINFO = "deactQ_L_14action";
        deactQ_L_14actionG_methods.$superclass = ($SuperG_class)&$actionG_methods;
        deactQ_L_14actionG_methods.__bool__ = (B_bool (*) (deactQ_L_14action))B_valueG_methods.__bool__;
        deactQ_L_14actionG_methods.__str__ = (B_str (*) (deactQ_L_14action))B_valueG_methods.__str__;
        deactQ_L_14actionG_methods.__repr__ = (B_str (*) (deactQ_L_14action))B_valueG_methods.__repr__;
        deactQ_L_14actionG_methods.__init__ = deactQ_L_14actionD___init__;
        deactQ_L_14actionG_methods.__call__ = deactQ_L_14actionD___call__;
        deactQ_L_14actionG_methods.__exec__ = deactQ_L_14actionD___exec__;
        deactQ_L_14actionG_methods.__asyn__ = deactQ_L_14actionD___asyn__;
        deactQ_L_14actionG_methods.__serialize__ = deactQ_L_14actionD___serialize__;
        deactQ_L_14actionG_methods.__deserialize__ = deactQ_L_14actionD___deserialize__;
        $register(&deactQ_L_14actionG_methods);
    }
    {
        deactQ_L_16actionG_methods.$GCINFO = "deactQ_L_16action";
        deactQ_L_16actionG_methods.$superclass = ($SuperG_class)&$actionG_methods;
        deactQ_L_16actionG_methods.__bool__ = (B_bool (*) (deactQ_L_16action))B_valueG_methods.__bool__;
        deactQ_L_16actionG_methods.__str__ = (B_str (*) (deactQ_L_16action))B_valueG_methods.__str__;
        deactQ_L_16actionG_methods.__repr__ = (B_str (*) (deactQ_L_16action))B_valueG_methods.__repr__;
        deactQ_L_16actionG_methods.__init__ = deactQ_L_16actionD___init__;
        deactQ_L_16actionG_methods.__call__ = deactQ_L_16actionD___call__;
        deactQ_L_16actionG_methods.__exec__ = deactQ_L_16actionD___exec__;
        deactQ_L_16actionG_methods.__asyn__ = deactQ_L_16actionD___asyn__;
        deactQ_L_16actionG_methods.__serialize__ = deactQ_L_16actionD___serialize__;
        deactQ_L_16actionG_methods.__deserialize__ = deactQ_L_16actionD___deserialize__;
        $register(&deactQ_L_16actionG_methods);
    }
    {
        deactQ_L_19actionG_methods.$GCINFO = "deactQ_L_19action";
        deactQ_L_19actionG_methods.$superclass = ($SuperG_class)&$actionG_methods;
        deactQ_L_19actionG_methods.__bool__ = (B_bool (*) (deactQ_L_19action))B_valueG_methods.__bool__;
        deactQ_L_19actionG_methods.__str__ = (B_str (*) (deactQ_L_19action))B_valueG_methods.__str__;
        deactQ_L_19actionG_methods.__repr__ = (B_str (*) (deactQ_L_19action))B_valueG_methods.__repr__;
        deactQ_L_19actionG_methods.__init__ = deactQ_L_19actionD___init__;
        deactQ_L_19actionG_methods.__call__ = deactQ_L_19actionD___call__;
        deactQ_L_19actionG_methods.__exec__ = deactQ_L_19actionD___exec__;
        deactQ_L_19actionG_methods.__asyn__ = deactQ_L_19actionD___asyn__;
        deactQ_L_19actionG_methods.__serialize__ = deactQ_L_19actionD___serialize__;
        deactQ_L_19actionG_methods.__deserialize__ = deactQ_L_19actionD___deserialize__;
        $register(&deactQ_L_19actionG_methods);
    }
    {
        deactQ_L_20ContG_methods.$GCINFO = "deactQ_L_20Cont";
        deactQ_L_20ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        deactQ_L_20ContG_methods.__bool__ = (B_bool (*) (deactQ_L_20Cont))B_valueG_methods.__bool__;
        deactQ_L_20ContG_methods.__str__ = (B_str (*) (deactQ_L_20Cont))B_valueG_methods.__str__;
        deactQ_L_20ContG_methods.__repr__ = (B_str (*) (deactQ_L_20Cont))B_valueG_methods.__repr__;
        deactQ_L_20ContG_methods.__init__ = deactQ_L_20ContD___init__;
        deactQ_L_20ContG_methods.__call__ = deactQ_L_20ContD___call__;
        deactQ_L_20ContG_methods.__serialize__ = deactQ_L_20ContD___serialize__;
        deactQ_L_20ContG_methods.__deserialize__ = deactQ_L_20ContD___deserialize__;
        $register(&deactQ_L_20ContG_methods);
    }
    {
        deactQ_L_21ContG_methods.$GCINFO = "deactQ_L_21Cont";
        deactQ_L_21ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        deactQ_L_21ContG_methods.__bool__ = (B_bool (*) (deactQ_L_21Cont))B_valueG_methods.__bool__;
        deactQ_L_21ContG_methods.__str__ = (B_str (*) (deactQ_L_21Cont))B_valueG_methods.__str__;
        deactQ_L_21ContG_methods.__repr__ = (B_str (*) (deactQ_L_21Cont))B_valueG_methods.__repr__;
        deactQ_L_21ContG_methods.__init__ = deactQ_L_21ContD___init__;
        deactQ_L_21ContG_methods.__call__ = deactQ_L_21ContD___call__;
        deactQ_L_21ContG_methods.__serialize__ = deactQ_L_21ContD___serialize__;
        deactQ_L_21ContG_methods.__deserialize__ = deactQ_L_21ContD___deserialize__;
        $register(&deactQ_L_21ContG_methods);
    }
    {
        deactQ_L_22ContG_methods.$GCINFO = "deactQ_L_22Cont";
        deactQ_L_22ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        deactQ_L_22ContG_methods.__bool__ = (B_bool (*) (deactQ_L_22Cont))B_valueG_methods.__bool__;
        deactQ_L_22ContG_methods.__str__ = (B_str (*) (deactQ_L_22Cont))B_valueG_methods.__str__;
        deactQ_L_22ContG_methods.__repr__ = (B_str (*) (deactQ_L_22Cont))B_valueG_methods.__repr__;
        deactQ_L_22ContG_methods.__init__ = deactQ_L_22ContD___init__;
        deactQ_L_22ContG_methods.__call__ = deactQ_L_22ContD___call__;
        deactQ_L_22ContG_methods.__serialize__ = deactQ_L_22ContD___serialize__;
        deactQ_L_22ContG_methods.__deserialize__ = deactQ_L_22ContD___deserialize__;
        $register(&deactQ_L_22ContG_methods);
    }
    {
        deactQ_L_23procG_methods.$GCINFO = "deactQ_L_23proc";
        deactQ_L_23procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        deactQ_L_23procG_methods.__bool__ = (B_bool (*) (deactQ_L_23proc))B_valueG_methods.__bool__;
        deactQ_L_23procG_methods.__str__ = (B_str (*) (deactQ_L_23proc))B_valueG_methods.__str__;
        deactQ_L_23procG_methods.__repr__ = (B_str (*) (deactQ_L_23proc))B_valueG_methods.__repr__;
        deactQ_L_23procG_methods.__init__ = deactQ_L_23procD___init__;
        deactQ_L_23procG_methods.__call__ = deactQ_L_23procD___call__;
        deactQ_L_23procG_methods.__exec__ = deactQ_L_23procD___exec__;
        deactQ_L_23procG_methods.__serialize__ = deactQ_L_23procD___serialize__;
        deactQ_L_23procG_methods.__deserialize__ = deactQ_L_23procD___deserialize__;
        $register(&deactQ_L_23procG_methods);
    }
    {
        deactQ_L_25ContG_methods.$GCINFO = "deactQ_L_25Cont";
        deactQ_L_25ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        deactQ_L_25ContG_methods.__bool__ = (B_bool (*) (deactQ_L_25Cont))B_valueG_methods.__bool__;
        deactQ_L_25ContG_methods.__str__ = (B_str (*) (deactQ_L_25Cont))B_valueG_methods.__str__;
        deactQ_L_25ContG_methods.__repr__ = (B_str (*) (deactQ_L_25Cont))B_valueG_methods.__repr__;
        deactQ_L_25ContG_methods.__init__ = deactQ_L_25ContD___init__;
        deactQ_L_25ContG_methods.__call__ = deactQ_L_25ContD___call__;
        deactQ_L_25ContG_methods.__serialize__ = deactQ_L_25ContD___serialize__;
        deactQ_L_25ContG_methods.__deserialize__ = deactQ_L_25ContD___deserialize__;
        $register(&deactQ_L_25ContG_methods);
    }
    {
        deactQ_L_26procG_methods.$GCINFO = "deactQ_L_26proc";
        deactQ_L_26procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        deactQ_L_26procG_methods.__bool__ = (B_bool (*) (deactQ_L_26proc))B_valueG_methods.__bool__;
        deactQ_L_26procG_methods.__str__ = (B_str (*) (deactQ_L_26proc))B_valueG_methods.__str__;
        deactQ_L_26procG_methods.__repr__ = (B_str (*) (deactQ_L_26proc))B_valueG_methods.__repr__;
        deactQ_L_26procG_methods.__init__ = deactQ_L_26procD___init__;
        deactQ_L_26procG_methods.__call__ = deactQ_L_26procD___call__;
        deactQ_L_26procG_methods.__exec__ = deactQ_L_26procD___exec__;
        deactQ_L_26procG_methods.__serialize__ = deactQ_L_26procD___serialize__;
        deactQ_L_26procG_methods.__deserialize__ = deactQ_L_26procD___deserialize__;
        $register(&deactQ_L_26procG_methods);
    }
    {
        deactQ_L_28ContG_methods.$GCINFO = "deactQ_L_28Cont";
        deactQ_L_28ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        deactQ_L_28ContG_methods.__bool__ = (B_bool (*) (deactQ_L_28Cont))B_valueG_methods.__bool__;
        deactQ_L_28ContG_methods.__str__ = (B_str (*) (deactQ_L_28Cont))B_valueG_methods.__str__;
        deactQ_L_28ContG_methods.__repr__ = (B_str (*) (deactQ_L_28Cont))B_valueG_methods.__repr__;
        deactQ_L_28ContG_methods.__init__ = deactQ_L_28ContD___init__;
        deactQ_L_28ContG_methods.__call__ = deactQ_L_28ContD___call__;
        deactQ_L_28ContG_methods.__serialize__ = deactQ_L_28ContD___serialize__;
        deactQ_L_28ContG_methods.__deserialize__ = deactQ_L_28ContD___deserialize__;
        $register(&deactQ_L_28ContG_methods);
    }
    {
        deactQ_L_29procG_methods.$GCINFO = "deactQ_L_29proc";
        deactQ_L_29procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        deactQ_L_29procG_methods.__bool__ = (B_bool (*) (deactQ_L_29proc))B_valueG_methods.__bool__;
        deactQ_L_29procG_methods.__str__ = (B_str (*) (deactQ_L_29proc))B_valueG_methods.__str__;
        deactQ_L_29procG_methods.__repr__ = (B_str (*) (deactQ_L_29proc))B_valueG_methods.__repr__;
        deactQ_L_29procG_methods.__init__ = deactQ_L_29procD___init__;
        deactQ_L_29procG_methods.__call__ = deactQ_L_29procD___call__;
        deactQ_L_29procG_methods.__exec__ = deactQ_L_29procD___exec__;
        deactQ_L_29procG_methods.__serialize__ = deactQ_L_29procD___serialize__;
        deactQ_L_29procG_methods.__deserialize__ = deactQ_L_29procD___deserialize__;
        $register(&deactQ_L_29procG_methods);
    }
    {
        deactQ_L_31ContG_methods.$GCINFO = "deactQ_L_31Cont";
        deactQ_L_31ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        deactQ_L_31ContG_methods.__bool__ = (B_bool (*) (deactQ_L_31Cont))B_valueG_methods.__bool__;
        deactQ_L_31ContG_methods.__str__ = (B_str (*) (deactQ_L_31Cont))B_valueG_methods.__str__;
        deactQ_L_31ContG_methods.__repr__ = (B_str (*) (deactQ_L_31Cont))B_valueG_methods.__repr__;
        deactQ_L_31ContG_methods.__init__ = deactQ_L_31ContD___init__;
        deactQ_L_31ContG_methods.__call__ = deactQ_L_31ContD___call__;
        deactQ_L_31ContG_methods.__serialize__ = deactQ_L_31ContD___serialize__;
        deactQ_L_31ContG_methods.__deserialize__ = deactQ_L_31ContD___deserialize__;
        $register(&deactQ_L_31ContG_methods);
    }
    {
        deactQ_L_32procG_methods.$GCINFO = "deactQ_L_32proc";
        deactQ_L_32procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        deactQ_L_32procG_methods.__bool__ = (B_bool (*) (deactQ_L_32proc))B_valueG_methods.__bool__;
        deactQ_L_32procG_methods.__str__ = (B_str (*) (deactQ_L_32proc))B_valueG_methods.__str__;
        deactQ_L_32procG_methods.__repr__ = (B_str (*) (deactQ_L_32proc))B_valueG_methods.__repr__;
        deactQ_L_32procG_methods.__init__ = deactQ_L_32procD___init__;
        deactQ_L_32procG_methods.__call__ = deactQ_L_32procD___call__;
        deactQ_L_32procG_methods.__exec__ = deactQ_L_32procD___exec__;
        deactQ_L_32procG_methods.__serialize__ = deactQ_L_32procD___serialize__;
        deactQ_L_32procG_methods.__deserialize__ = deactQ_L_32procD___deserialize__;
        $register(&deactQ_L_32procG_methods);
    }
    {
        deactQ_ApaG_methods.$GCINFO = "deactQ_Apa";
        deactQ_ApaG_methods.$superclass = ($SuperG_class)&$ActorG_methods;
        deactQ_ApaG_methods.__bool__ = (B_bool (*) (deactQ_Apa))$ActorG_methods.__bool__;
        deactQ_ApaG_methods.__str__ = (B_str (*) (deactQ_Apa))$ActorG_methods.__str__;
        deactQ_ApaG_methods.__repr__ = (B_str (*) (deactQ_Apa))$ActorG_methods.__repr__;
        deactQ_ApaG_methods.__resume__ = (B_NoneType (*) (deactQ_Apa))$ActorG_methods.__resume__;
        deactQ_ApaG_methods.__cleanup__ = (B_NoneType (*) (deactQ_Apa))$ActorG_methods.__cleanup__;
        deactQ_ApaG_methods.__init__ = deactQ_ApaD___init__;
        deactQ_ApaG_methods.setupG_local = deactQ_ApaD_setupG_local;
        deactQ_ApaG_methods.computeG_local = deactQ_ApaD_computeG_local;
        deactQ_ApaG_methods.noticeG_local = deactQ_ApaD_noticeG_local;
        deactQ_ApaG_methods.setup = deactQ_ApaD_setup;
        deactQ_ApaG_methods.compute = deactQ_ApaD_compute;
        deactQ_ApaG_methods.notice = deactQ_ApaD_notice;
        deactQ_ApaG_methods.__serialize__ = deactQ_ApaD___serialize__;
        deactQ_ApaG_methods.__deserialize__ = deactQ_ApaD___deserialize__;
        $register(&deactQ_ApaG_methods);
    }
    {
        deactQ_BepaG_methods.$GCINFO = "deactQ_Bepa";
        deactQ_BepaG_methods.$superclass = ($SuperG_class)&$ActorG_methods;
        deactQ_BepaG_methods.__bool__ = (B_bool (*) (deactQ_Bepa))$ActorG_methods.__bool__;
        deactQ_BepaG_methods.__str__ = (B_str (*) (deactQ_Bepa))$ActorG_methods.__str__;
        deactQ_BepaG_methods.__repr__ = (B_str (*) (deactQ_Bepa))$ActorG_methods.__repr__;
        deactQ_BepaG_methods.__resume__ = (B_NoneType (*) (deactQ_Bepa))$ActorG_methods.__resume__;
        deactQ_BepaG_methods.__cleanup__ = (B_NoneType (*) (deactQ_Bepa))$ActorG_methods.__cleanup__;
        deactQ_BepaG_methods.__init__ = deactQ_BepaD___init__;
        deactQ_BepaG_methods.callbackG_local = deactQ_BepaD_callbackG_local;
        deactQ_BepaG_methods.callback = deactQ_BepaD_callback;
        deactQ_BepaG_methods.__serialize__ = deactQ_BepaD___serialize__;
        deactQ_BepaG_methods.__deserialize__ = deactQ_BepaD___deserialize__;
        $register(&deactQ_BepaG_methods);
    }
    {
        deactQ_mainG_methods.$GCINFO = "deactQ_main";
        deactQ_mainG_methods.$superclass = ($SuperG_class)&$ActorG_methods;
        deactQ_mainG_methods.__bool__ = (B_bool (*) (deactQ_main))$ActorG_methods.__bool__;
        deactQ_mainG_methods.__str__ = (B_str (*) (deactQ_main))$ActorG_methods.__str__;
        deactQ_mainG_methods.__repr__ = (B_str (*) (deactQ_main))$ActorG_methods.__repr__;
        deactQ_mainG_methods.__resume__ = (B_NoneType (*) (deactQ_main))$ActorG_methods.__resume__;
        deactQ_mainG_methods.__cleanup__ = (B_NoneType (*) (deactQ_main))$ActorG_methods.__cleanup__;
        deactQ_mainG_methods.__init__ = deactQ_mainD___init__;
        deactQ_mainG_methods.myprocG_local = deactQ_mainD_myprocG_local;
        deactQ_mainG_methods.myproc = deactQ_mainD_myproc;
        deactQ_mainG_methods.__serialize__ = deactQ_mainD___serialize__;
        deactQ_mainG_methods.__deserialize__ = deactQ_mainD___deserialize__;
        $register(&deactQ_mainG_methods);
    }
    B_Times W_223 = (B_Times)B_IntegralD_intG_witness;
    deactQ_W_223 = W_223;
    B_Plus W_586 = (B_Plus)B_IntegralD_intG_witness;
    deactQ_W_586 = W_586;
    B_Eq W_761 = (B_Eq)B_OrdD_intG_witness;
    deactQ_W_761 = W_761;
}
