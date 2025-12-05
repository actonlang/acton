/* Acton source hash: test-hash */
#include "rts/common.h"
#include "out/types/lines.h"
$R linesQ_L_1C_1cont (linesQ_Apa self, $Cont C_cont, B_NoneType C_2res) {
    #line 18 "test/src/lines.act"
    self->z = toB_int(1LL);
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
B_NoneType linesQ_L_4procD___init__ (linesQ_L_4proc L_self, linesQ_Apa L_3obj) {
    L_self->L_3obj = L_3obj;
    return B_None;
}
$R linesQ_L_4procD___call__ (linesQ_L_4proc L_self, $Cont G_1, B_int G_2) {
    linesQ_Apa L_3obj = L_self->L_3obj;
    return (($R (*) (linesQ_Apa, $Cont, B_int))L_3obj->$class->noticeG_local)(L_3obj, G_1, G_2);
}
$R linesQ_L_4procD___exec__ (linesQ_L_4proc L_self, $Cont G_1, B_int G_2) {
    return (($R (*) (linesQ_L_4proc, $Cont, B_int))L_self->$class->__call__)(L_self, G_1, G_2);
}
void linesQ_L_4procD___serialize__ (linesQ_L_4proc self, $Serial$state state) {
    $step_serialize(self->L_3obj, state);
}
linesQ_L_4proc linesQ_L_4procD___deserialize__ (linesQ_L_4proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_4proc));
            self->$class = &linesQ_L_4procG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_4proc, state);
    }
    self->L_3obj = $step_deserialize(state);
    return self;
}
linesQ_L_4proc linesQ_L_4procG_new(linesQ_Apa G_1) {
    linesQ_L_4proc $tmp = acton_malloc(sizeof(struct linesQ_L_4proc));
    $tmp->$class = &linesQ_L_4procG_methods;
    linesQ_L_4procG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct linesQ_L_4procG_class linesQ_L_4procG_methods;
$R linesQ_U_L_5C_3cont ($Cont C_cont, int64_t U_3C_4res) {
    return $R_CONT(C_cont, B_None);
}
$R linesQ_L_5C_3cont ($Cont C_cont, B_int C_4res) {
    return linesQ_U_L_5C_3cont(C_cont, ((B_int)C_4res)->val);
}
B_NoneType linesQ_L_6ContD___init__ (linesQ_L_6Cont L_self, $Cont C_cont) {
    L_self->C_cont = C_cont;
    return B_None;
}
$R linesQ_L_6ContD___call__ (linesQ_L_6Cont L_self, B_int G_1) {
    $Cont C_cont = L_self->C_cont;
    return linesQ_L_5C_3cont(C_cont, G_1);
}
void linesQ_L_6ContD___serialize__ (linesQ_L_6Cont self, $Serial$state state) {
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
    self->C_cont = $step_deserialize(state);
    return self;
}
linesQ_L_6Cont linesQ_L_6ContG_new($Cont G_1) {
    linesQ_L_6Cont $tmp = acton_malloc(sizeof(struct linesQ_L_6Cont));
    $tmp->$class = &linesQ_L_6ContG_methods;
    linesQ_L_6ContG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct linesQ_L_6ContG_class linesQ_L_6ContG_methods;
$R linesQ_U_1L_7C_5cont ($action cb, $Cont C_cont, int64_t U_4C_6res) {
    #line 9 "test/src/lines.act"
    int64_t U_5v = U_4C_6res;
    #line 10 "test/src/lines.act"
    B_Msg m = ((B_Msg)((B_Msg (*) ($action, B_int))cb->$class->__asyn__)(cb, toB_int(2LL)));
    int64_t U_6N_tmp = (U_5v * 10LL);
    return $R_CONT(C_cont, toB_int(U_6N_tmp));
}
$R linesQ_L_7C_5cont ($action cb, $Cont C_cont, B_int C_6res) {
    return linesQ_U_1L_7C_5cont(cb, C_cont, ((B_int)C_6res)->val);
}
B_NoneType linesQ_L_8ContD___init__ (linesQ_L_8Cont L_self, $action cb, $Cont C_cont) {
    L_self->cb = cb;
    L_self->C_cont = C_cont;
    return B_None;
}
$R linesQ_L_8ContD___call__ (linesQ_L_8Cont L_self, B_int G_1) {
    $action cb = L_self->cb;
    $Cont C_cont = L_self->C_cont;
    return linesQ_L_7C_5cont(cb, C_cont, G_1);
}
void linesQ_L_8ContD___serialize__ (linesQ_L_8Cont self, $Serial$state state) {
    $step_serialize(self->cb, state);
    $step_serialize(self->C_cont, state);
}
linesQ_L_8Cont linesQ_L_8ContD___deserialize__ (linesQ_L_8Cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_8Cont));
            self->$class = &linesQ_L_8ContG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_8Cont, state);
    }
    self->cb = $step_deserialize(state);
    self->C_cont = $step_deserialize(state);
    return self;
}
linesQ_L_8Cont linesQ_L_8ContG_new($action G_1, $Cont G_2) {
    linesQ_L_8Cont $tmp = acton_malloc(sizeof(struct linesQ_L_8Cont));
    $tmp->$class = &linesQ_L_8ContG_methods;
    linesQ_L_8ContG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct linesQ_L_8ContG_class linesQ_L_8ContG_methods;
B_NoneType linesQ_L_9procD___init__ (linesQ_L_9proc L_self, linesQ_Apa self, $proc cb) {
    L_self->self = self;
    L_self->cb = cb;
    return B_None;
}
$R linesQ_L_9procD___call__ (linesQ_L_9proc L_self, $Cont C_cont) {
    linesQ_Apa self = L_self->self;
    $proc cb = L_self->cb;
    return (($R (*) (linesQ_Apa, $Cont, $proc))self->$class->setupG_local)(self, C_cont, cb);
}
$R linesQ_L_9procD___exec__ (linesQ_L_9proc L_self, $Cont C_cont) {
    return (($R (*) (linesQ_L_9proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void linesQ_L_9procD___serialize__ (linesQ_L_9proc self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->cb, state);
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
    self->cb = $step_deserialize(state);
    return self;
}
linesQ_L_9proc linesQ_L_9procG_new(linesQ_Apa G_1, $proc G_2) {
    linesQ_L_9proc $tmp = acton_malloc(sizeof(struct linesQ_L_9proc));
    $tmp->$class = &linesQ_L_9procG_methods;
    linesQ_L_9procG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct linesQ_L_9procG_class linesQ_L_9procG_methods;
B_NoneType linesQ_L_10procD___init__ (linesQ_L_10proc L_self, linesQ_Apa self, $action cb) {
    L_self->self = self;
    L_self->cb = cb;
    return B_None;
}
$R linesQ_L_10procD___call__ (linesQ_L_10proc L_self, $Cont C_cont) {
    linesQ_Apa self = L_self->self;
    $action cb = L_self->cb;
    return (($R (*) (linesQ_Apa, $Cont, $action))self->$class->computeG_local)(self, C_cont, cb);
}
$R linesQ_L_10procD___exec__ (linesQ_L_10proc L_self, $Cont C_cont) {
    return (($R (*) (linesQ_L_10proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void linesQ_L_10procD___serialize__ (linesQ_L_10proc self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->cb, state);
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
    self->cb = $step_deserialize(state);
    return self;
}
linesQ_L_10proc linesQ_L_10procG_new(linesQ_Apa G_1, $action G_2) {
    linesQ_L_10proc $tmp = acton_malloc(sizeof(struct linesQ_L_10proc));
    $tmp->$class = &linesQ_L_10procG_methods;
    linesQ_L_10procG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct linesQ_L_10procG_class linesQ_L_10procG_methods;
B_NoneType linesQ_L_11procD___init__ (linesQ_L_11proc L_self, linesQ_Apa self, B_int i) {
    L_self->self = self;
    L_self->i = i;
    return B_None;
}
$R linesQ_L_11procD___call__ (linesQ_L_11proc L_self, $Cont C_cont) {
    linesQ_Apa self = L_self->self;
    int64_t U_7i = ((B_int)L_self->i)->val;
    return (($R (*) (linesQ_Apa, $Cont, B_int))self->$class->noticeG_local)(self, C_cont, toB_int(U_7i));
}
$R linesQ_L_11procD___exec__ (linesQ_L_11proc L_self, $Cont C_cont) {
    return (($R (*) (linesQ_L_11proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void linesQ_L_11procD___serialize__ (linesQ_L_11proc self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->i, state);
}
linesQ_L_11proc linesQ_L_11procD___deserialize__ (linesQ_L_11proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_11proc));
            self->$class = &linesQ_L_11procG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_11proc, state);
    }
    self->self = $step_deserialize(state);
    self->i = $step_deserialize(state);
    return self;
}
linesQ_L_11proc linesQ_L_11procG_new(linesQ_Apa G_1, B_int G_2) {
    linesQ_L_11proc $tmp = acton_malloc(sizeof(struct linesQ_L_11proc));
    $tmp->$class = &linesQ_L_11procG_methods;
    linesQ_L_11procG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct linesQ_L_11procG_class linesQ_L_11procG_methods;
B_NoneType linesQ_L_12procD___init__ (linesQ_L_12proc L_self, linesQ_Bepa self, B_int i) {
    L_self->self = self;
    L_self->i = i;
    return B_None;
}
$R linesQ_L_12procD___call__ (linesQ_L_12proc L_self, $Cont C_cont) {
    linesQ_Bepa self = L_self->self;
    int64_t U_8i = ((B_int)L_self->i)->val;
    return (($R (*) (linesQ_Bepa, $Cont, B_int))self->$class->callbackG_local)(self, C_cont, toB_int(U_8i));
}
$R linesQ_L_12procD___exec__ (linesQ_L_12proc L_self, $Cont C_cont) {
    return (($R (*) (linesQ_L_12proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void linesQ_L_12procD___serialize__ (linesQ_L_12proc self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->i, state);
}
linesQ_L_12proc linesQ_L_12procD___deserialize__ (linesQ_L_12proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_12proc));
            self->$class = &linesQ_L_12procG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_12proc, state);
    }
    self->self = $step_deserialize(state);
    self->i = $step_deserialize(state);
    return self;
}
linesQ_L_12proc linesQ_L_12procG_new(linesQ_Bepa G_1, B_int G_2) {
    linesQ_L_12proc $tmp = acton_malloc(sizeof(struct linesQ_L_12proc));
    $tmp->$class = &linesQ_L_12procG_methods;
    linesQ_L_12procG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct linesQ_L_12procG_class linesQ_L_12procG_methods;
B_NoneType linesQ_L_16actionD___init__ (linesQ_L_16action L_self, linesQ_Apa L_15obj) {
    L_self->L_15obj = L_15obj;
    return B_None;
}
$R linesQ_L_16actionD___call__ (linesQ_L_16action L_self, $Cont L_cont, B_int G_1) {
    return $AWAIT(L_cont, ((B_Msg)((B_Msg (*) (linesQ_L_16action, B_int))L_self->$class->__asyn__)(L_self, G_1)));
}
$R linesQ_L_16actionD___exec__ (linesQ_L_16action L_self, $Cont L_cont, B_int G_1) {
    return $R_CONT(L_cont, ((B_value)((B_Msg (*) (linesQ_L_16action, B_int))L_self->$class->__asyn__)(L_self, G_1)));
}
B_Msg linesQ_L_16actionD___asyn__ (linesQ_L_16action L_self, B_int G_1) {
    linesQ_Apa L_15obj = L_self->L_15obj;
    return ((B_Msg)((B_Msg (*) (linesQ_Apa, B_int))L_15obj->$class->notice)(L_15obj, G_1));
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
linesQ_L_16action linesQ_L_16actionG_new(linesQ_Apa G_1) {
    linesQ_L_16action $tmp = acton_malloc(sizeof(struct linesQ_L_16action));
    $tmp->$class = &linesQ_L_16actionG_methods;
    linesQ_L_16actionG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct linesQ_L_16actionG_class linesQ_L_16actionG_methods;
B_NoneType linesQ_L_18actionD___init__ (linesQ_L_18action L_self, linesQ_Bepa L_17obj) {
    L_self->L_17obj = L_17obj;
    return B_None;
}
$R linesQ_L_18actionD___call__ (linesQ_L_18action L_self, $Cont L_cont, B_int G_1) {
    return $AWAIT(L_cont, ((B_Msg)((B_Msg (*) (linesQ_L_18action, B_int))L_self->$class->__asyn__)(L_self, G_1)));
}
$R linesQ_L_18actionD___exec__ (linesQ_L_18action L_self, $Cont L_cont, B_int G_1) {
    return $R_CONT(L_cont, ((B_value)((B_Msg (*) (linesQ_L_18action, B_int))L_self->$class->__asyn__)(L_self, G_1)));
}
B_Msg linesQ_L_18actionD___asyn__ (linesQ_L_18action L_self, B_int G_1) {
    linesQ_Bepa L_17obj = L_self->L_17obj;
    return ((B_Msg)((B_Msg (*) (linesQ_Bepa, B_int))L_17obj->$class->callback)(L_17obj, G_1));
}
void linesQ_L_18actionD___serialize__ (linesQ_L_18action self, $Serial$state state) {
    $step_serialize(self->L_17obj, state);
}
linesQ_L_18action linesQ_L_18actionD___deserialize__ (linesQ_L_18action self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_18action));
            self->$class = &linesQ_L_18actionG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_18action, state);
    }
    self->L_17obj = $step_deserialize(state);
    return self;
}
linesQ_L_18action linesQ_L_18actionG_new(linesQ_Bepa G_1) {
    linesQ_L_18action $tmp = acton_malloc(sizeof(struct linesQ_L_18action));
    $tmp->$class = &linesQ_L_18actionG_methods;
    linesQ_L_18actionG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct linesQ_L_18actionG_class linesQ_L_18actionG_methods;
B_NoneType linesQ_L_21actionD___init__ (linesQ_L_21action L_self, linesQ_main L_20obj) {
    L_self->L_20obj = L_20obj;
    return B_None;
}
$R linesQ_L_21actionD___call__ (linesQ_L_21action L_self, $Cont L_cont, B_int G_1) {
    return $AWAIT(L_cont, ((B_Msg)((B_Msg (*) (linesQ_L_21action, B_int))L_self->$class->__asyn__)(L_self, G_1)));
}
$R linesQ_L_21actionD___exec__ (linesQ_L_21action L_self, $Cont L_cont, B_int G_1) {
    return $R_CONT(L_cont, ((B_value)((B_Msg (*) (linesQ_L_21action, B_int))L_self->$class->__asyn__)(L_self, G_1)));
}
B_Msg linesQ_L_21actionD___asyn__ (linesQ_L_21action L_self, B_int G_1) {
    linesQ_main L_20obj = L_self->L_20obj;
    return ((B_Msg)((B_Msg (*) (linesQ_main, B_int))L_20obj->$class->myproc)(L_20obj, G_1));
}
void linesQ_L_21actionD___serialize__ (linesQ_L_21action self, $Serial$state state) {
    $step_serialize(self->L_20obj, state);
}
linesQ_L_21action linesQ_L_21actionD___deserialize__ (linesQ_L_21action self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_21action));
            self->$class = &linesQ_L_21actionG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_21action, state);
    }
    self->L_20obj = $step_deserialize(state);
    return self;
}
linesQ_L_21action linesQ_L_21actionG_new(linesQ_main G_1) {
    linesQ_L_21action $tmp = acton_malloc(sizeof(struct linesQ_L_21action));
    $tmp->$class = &linesQ_L_21actionG_methods;
    linesQ_L_21actionG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct linesQ_L_21actionG_class linesQ_L_21actionG_methods;
B_NoneType linesQ_L_22procD___init__ (linesQ_L_22proc L_self, linesQ_main self) {
    L_self->self = self;
    return B_None;
}
$R linesQ_L_22procD___call__ (linesQ_L_22proc L_self, $Cont C_cont) {
    linesQ_main self = L_self->self;
    return (($R (*) (linesQ_main, $Cont, B_int))self->$class->myprocG_local)(self, C_cont, toB_int(0LL));
}
$R linesQ_L_22procD___exec__ (linesQ_L_22proc L_self, $Cont C_cont) {
    return (($R (*) (linesQ_L_22proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void linesQ_L_22procD___serialize__ (linesQ_L_22proc self, $Serial$state state) {
    $step_serialize(self->self, state);
}
linesQ_L_22proc linesQ_L_22procD___deserialize__ (linesQ_L_22proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_22proc));
            self->$class = &linesQ_L_22procG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_22proc, state);
    }
    self->self = $step_deserialize(state);
    return self;
}
linesQ_L_22proc linesQ_L_22procG_new(linesQ_main G_1) {
    linesQ_L_22proc $tmp = acton_malloc(sizeof(struct linesQ_L_22proc));
    $tmp->$class = &linesQ_L_22procG_methods;
    linesQ_L_22procG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct linesQ_L_22procG_class linesQ_L_22procG_methods;
$R linesQ_U_2L_19C_11cont (linesQ_main self, B_Iterable W_1908, B_Number W_2123, $Cont C_cont, int64_t U_9C_12res) {
    #line 38 "test/src/lines.act"
    self->r = toB_int(U_9C_12res);
    #line 39 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(2, to$str("r ="), self->r), B_None, B_None, B_None, B_None);
    #line 40 "test/src/lines.act"
    ((B_Msg (*) (linesQ_Apa, $action))self->a->$class->compute)(self->a, (($action)linesQ_L_21actionG_new(self)));
    #line 41 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("main")), B_None, B_None, B_None, B_None);
    #line 44 "test/src/lines.act"
    self->v = toB_int(0LL);
    #line 45 "test/src/lines.act"
    if ((((B_int)self->v)->val == 0LL)) {
        #line 46 "test/src/lines.act"
        ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("if branch")), B_None, B_None, B_None, B_None);
        #line 47 "test/src/lines.act"
        if ((((B_int)self->v)->val < 1LL)) {
            #line 48 "test/src/lines.act"
            ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("nested if")), B_None, B_None, B_None, B_None);
        }
        else if ((((B_int)self->v)->val == -1LL)) {
            #line 50 "test/src/lines.act"
            ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("nested elif")), B_None, B_None, B_None, B_None);
        }
        else {
            #line 52 "test/src/lines.act"
            ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("nested else")), B_None, B_None, B_None, B_None);
        }
    }
    else if ((((B_int)self->v)->val == 1LL)) {
        #line 54 "test/src/lines.act"
        ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("outer elif")), B_None, B_None, B_None, B_None);
    }
    else {
        #line 56 "test/src/lines.act"
        ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("outer else")), B_None, B_None, B_None, B_None);
    }
    #line 59 "test/src/lines.act"
    self->i = toB_int(0LL);
    #line 60 "test/src/lines.act"
    while (true) {
        if ((((B_int)self->i)->val < 3LL)) {
        }
        else {
            #line 70 "test/src/lines.act"
            ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("while else")), B_None, B_None, B_None, B_None);
            break;
        }
        #line 61 "test/src/lines.act"
        self->i = toB_int((((B_int)self->i)->val + 1LL));
        #line 62 "test/src/lines.act"
        if ((((B_int)self->i)->val == 1LL)) {
            #line 63 "test/src/lines.act"
            ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("continue path")), B_None, B_None, B_None, B_None);
            #line 64 "test/src/lines.act"
            continue;
        }
        #line 65 "test/src/lines.act"
        if ((((B_int)self->i)->val == 2LL)) {
            #line 66 "test/src/lines.act"
            ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("break path")), B_None, B_None, B_None, B_None);
            #line 67 "test/src/lines.act"
            break;
        }
        #line 68 "test/src/lines.act"
        ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(2, to$str("loop body"), self->i), B_None, B_None, B_None, B_None);
    }
    B_Iterator N_3iter = ((B_Iterator (*) (B_Iterable, B_list))W_1908->$class->__iter__)(W_1908, B_mk_list(3, toB_int(1LL) , toB_int(2LL) , toB_int(3LL)));
    if ($PUSH()) {
        #line 73 "test/src/lines.act"
        while (true) {
            int64_t U_10j = ((B_int)((B_int (*) (B_Iterator))N_3iter->$class->__next__)(N_3iter))->val;
            #line 74 "test/src/lines.act"
            if ((U_10j == 2LL)) {
                #line 75 "test/src/lines.act"
                continue;
            }
            #line 76 "test/src/lines.act"
            ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(2, to$str("for j"), toB_int(U_10j)), B_None, B_None, B_None, B_None);
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
            if ((((B_int)self->v)->val == 0LL)) {
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
    $AFTER(toB_float(1), (($Cont)linesQ_L_22procG_new(self)));
    return (($R (*) (linesQ_main, $Cont))self->$class->nopG_local)(self, C_cont);
}
$R linesQ_L_19C_11cont (linesQ_main self, B_Iterable W_1908, B_Number W_2123, $Cont C_cont, B_int C_12res) {
    return linesQ_U_2L_19C_11cont(self, W_1908, W_2123, C_cont, ((B_int)C_12res)->val);
}
B_NoneType linesQ_L_23ContD___init__ (linesQ_L_23Cont L_self, linesQ_main self, B_Iterable W_1908, B_Number W_2123, $Cont C_cont) {
    L_self->self = self;
    L_self->W_1908 = W_1908;
    L_self->W_2123 = W_2123;
    L_self->C_cont = C_cont;
    return B_None;
}
$R linesQ_L_23ContD___call__ (linesQ_L_23Cont L_self, B_int G_1) {
    linesQ_main self = L_self->self;
    B_Iterable W_1908 = L_self->W_1908;
    B_Number W_2123 = L_self->W_2123;
    $Cont C_cont = L_self->C_cont;
    return linesQ_L_19C_11cont(self, W_1908, W_2123, C_cont, G_1);
}
void linesQ_L_23ContD___serialize__ (linesQ_L_23Cont self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->W_1908, state);
    $step_serialize(self->W_2123, state);
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
    self->W_1908 = $step_deserialize(state);
    self->W_2123 = $step_deserialize(state);
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
$R linesQ_L_14C_9cont (linesQ_main self, B_Iterable W_1908, B_Number W_2123, $Cont C_cont, linesQ_Bepa C_10res) {
    #line 34 "test/src/lines.act"
    self->b = C_10res;
    #line 35 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("-----")), B_None, B_None, B_None, B_None);
    #line 36 "test/src/lines.act"
    ((B_Msg (*) (linesQ_Apa, $proc))self->a->$class->setup)(self->a, (($proc)linesQ_L_16actionG_new(self->a)));
    #line 37 "test/src/lines.act"
    self->x = ((B_Msg (*) (linesQ_Apa, $action))self->a->$class->compute)(self->a, (($action)linesQ_L_18actionG_new(self->b)));
    return $AWAIT((($Cont)linesQ_L_23ContG_new(self, W_1908, W_2123, C_cont)), self->x);
}
B_NoneType linesQ_L_24ContD___init__ (linesQ_L_24Cont L_self, linesQ_main self, B_Iterable W_1908, B_Number W_2123, $Cont C_cont) {
    L_self->self = self;
    L_self->W_1908 = W_1908;
    L_self->W_2123 = W_2123;
    L_self->C_cont = C_cont;
    return B_None;
}
$R linesQ_L_24ContD___call__ (linesQ_L_24Cont L_self, linesQ_Bepa G_1) {
    linesQ_main self = L_self->self;
    B_Iterable W_1908 = L_self->W_1908;
    B_Number W_2123 = L_self->W_2123;
    $Cont C_cont = L_self->C_cont;
    return linesQ_L_14C_9cont(self, W_1908, W_2123, C_cont, G_1);
}
void linesQ_L_24ContD___serialize__ (linesQ_L_24Cont self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->W_1908, state);
    $step_serialize(self->W_2123, state);
    $step_serialize(self->C_cont, state);
}
linesQ_L_24Cont linesQ_L_24ContD___deserialize__ (linesQ_L_24Cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_24Cont));
            self->$class = &linesQ_L_24ContG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_24Cont, state);
    }
    self->self = $step_deserialize(state);
    self->W_1908 = $step_deserialize(state);
    self->W_2123 = $step_deserialize(state);
    self->C_cont = $step_deserialize(state);
    return self;
}
linesQ_L_24Cont linesQ_L_24ContG_new(linesQ_main G_1, B_Iterable G_2, B_Number G_3, $Cont G_4) {
    linesQ_L_24Cont $tmp = acton_malloc(sizeof(struct linesQ_L_24Cont));
    $tmp->$class = &linesQ_L_24ContG_methods;
    linesQ_L_24ContG_methods.__init__($tmp, G_1, G_2, G_3, G_4);
    return $tmp;
}
struct linesQ_L_24ContG_class linesQ_L_24ContG_methods;
$R linesQ_L_13C_7cont (linesQ_main self, B_Iterable W_1908, B_Number W_2123, $Cont C_cont, linesQ_Apa C_8res) {
    #line 33 "test/src/lines.act"
    self->a = C_8res;
    return linesQ_BepaG_newact((($Cont)linesQ_L_24ContG_new(self, W_1908, W_2123, C_cont)));
}
B_NoneType linesQ_L_25ContD___init__ (linesQ_L_25Cont L_self, linesQ_main self, B_Iterable W_1908, B_Number W_2123, $Cont C_cont) {
    L_self->self = self;
    L_self->W_1908 = W_1908;
    L_self->W_2123 = W_2123;
    L_self->C_cont = C_cont;
    return B_None;
}
$R linesQ_L_25ContD___call__ (linesQ_L_25Cont L_self, linesQ_Apa G_1) {
    linesQ_main self = L_self->self;
    B_Iterable W_1908 = L_self->W_1908;
    B_Number W_2123 = L_self->W_2123;
    $Cont C_cont = L_self->C_cont;
    return linesQ_L_13C_7cont(self, W_1908, W_2123, C_cont, G_1);
}
void linesQ_L_25ContD___serialize__ (linesQ_L_25Cont self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->W_1908, state);
    $step_serialize(self->W_2123, state);
    $step_serialize(self->C_cont, state);
}
linesQ_L_25Cont linesQ_L_25ContD___deserialize__ (linesQ_L_25Cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_25Cont));
            self->$class = &linesQ_L_25ContG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_25Cont, state);
    }
    self->self = $step_deserialize(state);
    self->W_1908 = $step_deserialize(state);
    self->W_2123 = $step_deserialize(state);
    self->C_cont = $step_deserialize(state);
    return self;
}
linesQ_L_25Cont linesQ_L_25ContG_new(linesQ_main G_1, B_Iterable G_2, B_Number G_3, $Cont G_4) {
    linesQ_L_25Cont $tmp = acton_malloc(sizeof(struct linesQ_L_25Cont));
    $tmp->$class = &linesQ_L_25ContG_methods;
    linesQ_L_25ContG_methods.__init__($tmp, G_1, G_2, G_3, G_4);
    return $tmp;
}
struct linesQ_L_25ContG_class linesQ_L_25ContG_methods;
B_NoneType linesQ_L_26procD___init__ (linesQ_L_26proc L_self, linesQ_main self, B_int i) {
    L_self->self = self;
    L_self->i = i;
    return B_None;
}
$R linesQ_L_26procD___call__ (linesQ_L_26proc L_self, $Cont C_cont) {
    linesQ_main self = L_self->self;
    int64_t U_11i = ((B_int)L_self->i)->val;
    return (($R (*) (linesQ_main, $Cont, B_int))self->$class->myprocG_local)(self, C_cont, toB_int(U_11i));
}
$R linesQ_L_26procD___exec__ (linesQ_L_26proc L_self, $Cont C_cont) {
    return (($R (*) (linesQ_L_26proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void linesQ_L_26procD___serialize__ (linesQ_L_26proc self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->i, state);
}
linesQ_L_26proc linesQ_L_26procD___deserialize__ (linesQ_L_26proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_26proc));
            self->$class = &linesQ_L_26procG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_26proc, state);
    }
    self->self = $step_deserialize(state);
    self->i = $step_deserialize(state);
    return self;
}
linesQ_L_26proc linesQ_L_26procG_new(linesQ_main G_1, B_int G_2) {
    linesQ_L_26proc $tmp = acton_malloc(sizeof(struct linesQ_L_26proc));
    $tmp->$class = &linesQ_L_26procG_methods;
    linesQ_L_26procG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct linesQ_L_26procG_class linesQ_L_26procG_methods;
B_NoneType linesQ_L_27procD___init__ (linesQ_L_27proc L_self, linesQ_main self) {
    L_self->self = self;
    return B_None;
}
$R linesQ_L_27procD___call__ (linesQ_L_27proc L_self, $Cont C_cont) {
    linesQ_main self = L_self->self;
    return (($R (*) (linesQ_main, $Cont))self->$class->nopG_local)(self, C_cont);
}
$R linesQ_L_27procD___exec__ (linesQ_L_27proc L_self, $Cont C_cont) {
    return (($R (*) (linesQ_L_27proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void linesQ_L_27procD___serialize__ (linesQ_L_27proc self, $Serial$state state) {
    $step_serialize(self->self, state);
}
linesQ_L_27proc linesQ_L_27procD___deserialize__ (linesQ_L_27proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_27proc));
            self->$class = &linesQ_L_27procG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_27proc, state);
    }
    self->self = $step_deserialize(state);
    return self;
}
linesQ_L_27proc linesQ_L_27procG_new(linesQ_main G_1) {
    linesQ_L_27proc $tmp = acton_malloc(sizeof(struct linesQ_L_27proc));
    $tmp->$class = &linesQ_L_27procG_methods;
    linesQ_L_27procG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct linesQ_L_27procG_class linesQ_L_27procG_methods;
$R linesQ_L_28C_13cont ($Cont C_cont, linesQ_Apa G_act, B_NoneType C_14res) {
    return $R_CONT(C_cont, G_act);
}
B_NoneType linesQ_L_29ContD___init__ (linesQ_L_29Cont L_self, $Cont C_cont, linesQ_Apa G_act) {
    L_self->C_cont = C_cont;
    L_self->G_act = G_act;
    return B_None;
}
$R linesQ_L_29ContD___call__ (linesQ_L_29Cont L_self, B_NoneType G_1) {
    $Cont C_cont = L_self->C_cont;
    linesQ_Apa G_act = L_self->G_act;
    return linesQ_L_28C_13cont(C_cont, G_act, G_1);
}
void linesQ_L_29ContD___serialize__ (linesQ_L_29Cont self, $Serial$state state) {
    $step_serialize(self->C_cont, state);
    $step_serialize(self->G_act, state);
}
linesQ_L_29Cont linesQ_L_29ContD___deserialize__ (linesQ_L_29Cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_29Cont));
            self->$class = &linesQ_L_29ContG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_29Cont, state);
    }
    self->C_cont = $step_deserialize(state);
    self->G_act = $step_deserialize(state);
    return self;
}
linesQ_L_29Cont linesQ_L_29ContG_new($Cont G_1, linesQ_Apa G_2) {
    linesQ_L_29Cont $tmp = acton_malloc(sizeof(struct linesQ_L_29Cont));
    $tmp->$class = &linesQ_L_29ContG_methods;
    linesQ_L_29ContG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct linesQ_L_29ContG_class linesQ_L_29ContG_methods;
B_NoneType linesQ_L_30procD___init__ (linesQ_L_30proc L_self, linesQ_Apa G_act) {
    L_self->G_act = G_act;
    return B_None;
}
$R linesQ_L_30procD___call__ (linesQ_L_30proc L_self, $Cont C_cont) {
    linesQ_Apa G_act = L_self->G_act;
    return (($R (*) (linesQ_Apa, $Cont))G_act->$class->__init__)(G_act, C_cont);
}
$R linesQ_L_30procD___exec__ (linesQ_L_30proc L_self, $Cont C_cont) {
    return (($R (*) (linesQ_L_30proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void linesQ_L_30procD___serialize__ (linesQ_L_30proc self, $Serial$state state) {
    $step_serialize(self->G_act, state);
}
linesQ_L_30proc linesQ_L_30procD___deserialize__ (linesQ_L_30proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_30proc));
            self->$class = &linesQ_L_30procG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_30proc, state);
    }
    self->G_act = $step_deserialize(state);
    return self;
}
linesQ_L_30proc linesQ_L_30procG_new(linesQ_Apa G_1) {
    linesQ_L_30proc $tmp = acton_malloc(sizeof(struct linesQ_L_30proc));
    $tmp->$class = &linesQ_L_30procG_methods;
    linesQ_L_30procG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct linesQ_L_30procG_class linesQ_L_30procG_methods;
$R linesQ_L_31C_15cont ($Cont C_cont, linesQ_Bepa G_act, B_NoneType C_16res) {
    return $R_CONT(C_cont, G_act);
}
B_NoneType linesQ_L_32ContD___init__ (linesQ_L_32Cont L_self, $Cont C_cont, linesQ_Bepa G_act) {
    L_self->C_cont = C_cont;
    L_self->G_act = G_act;
    return B_None;
}
$R linesQ_L_32ContD___call__ (linesQ_L_32Cont L_self, B_NoneType G_1) {
    $Cont C_cont = L_self->C_cont;
    linesQ_Bepa G_act = L_self->G_act;
    return linesQ_L_31C_15cont(C_cont, G_act, G_1);
}
void linesQ_L_32ContD___serialize__ (linesQ_L_32Cont self, $Serial$state state) {
    $step_serialize(self->C_cont, state);
    $step_serialize(self->G_act, state);
}
linesQ_L_32Cont linesQ_L_32ContD___deserialize__ (linesQ_L_32Cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_32Cont));
            self->$class = &linesQ_L_32ContG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_32Cont, state);
    }
    self->C_cont = $step_deserialize(state);
    self->G_act = $step_deserialize(state);
    return self;
}
linesQ_L_32Cont linesQ_L_32ContG_new($Cont G_1, linesQ_Bepa G_2) {
    linesQ_L_32Cont $tmp = acton_malloc(sizeof(struct linesQ_L_32Cont));
    $tmp->$class = &linesQ_L_32ContG_methods;
    linesQ_L_32ContG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct linesQ_L_32ContG_class linesQ_L_32ContG_methods;
B_NoneType linesQ_L_33procD___init__ (linesQ_L_33proc L_self, linesQ_Bepa G_act) {
    L_self->G_act = G_act;
    return B_None;
}
$R linesQ_L_33procD___call__ (linesQ_L_33proc L_self, $Cont C_cont) {
    linesQ_Bepa G_act = L_self->G_act;
    return (($R (*) (linesQ_Bepa, $Cont))G_act->$class->__init__)(G_act, C_cont);
}
$R linesQ_L_33procD___exec__ (linesQ_L_33proc L_self, $Cont C_cont) {
    return (($R (*) (linesQ_L_33proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void linesQ_L_33procD___serialize__ (linesQ_L_33proc self, $Serial$state state) {
    $step_serialize(self->G_act, state);
}
linesQ_L_33proc linesQ_L_33procD___deserialize__ (linesQ_L_33proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_33proc));
            self->$class = &linesQ_L_33procG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_33proc, state);
    }
    self->G_act = $step_deserialize(state);
    return self;
}
linesQ_L_33proc linesQ_L_33procG_new(linesQ_Bepa G_1) {
    linesQ_L_33proc $tmp = acton_malloc(sizeof(struct linesQ_L_33proc));
    $tmp->$class = &linesQ_L_33procG_methods;
    linesQ_L_33procG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct linesQ_L_33procG_class linesQ_L_33procG_methods;
$R linesQ_L_34C_17cont ($Cont C_cont, linesQ_main G_act, B_NoneType C_18res) {
    return $R_CONT(C_cont, G_act);
}
B_NoneType linesQ_L_35ContD___init__ (linesQ_L_35Cont L_self, $Cont C_cont, linesQ_main G_act) {
    L_self->C_cont = C_cont;
    L_self->G_act = G_act;
    return B_None;
}
$R linesQ_L_35ContD___call__ (linesQ_L_35Cont L_self, B_NoneType G_1) {
    $Cont C_cont = L_self->C_cont;
    linesQ_main G_act = L_self->G_act;
    return linesQ_L_34C_17cont(C_cont, G_act, G_1);
}
void linesQ_L_35ContD___serialize__ (linesQ_L_35Cont self, $Serial$state state) {
    $step_serialize(self->C_cont, state);
    $step_serialize(self->G_act, state);
}
linesQ_L_35Cont linesQ_L_35ContD___deserialize__ (linesQ_L_35Cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_35Cont));
            self->$class = &linesQ_L_35ContG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_35Cont, state);
    }
    self->C_cont = $step_deserialize(state);
    self->G_act = $step_deserialize(state);
    return self;
}
linesQ_L_35Cont linesQ_L_35ContG_new($Cont G_1, linesQ_main G_2) {
    linesQ_L_35Cont $tmp = acton_malloc(sizeof(struct linesQ_L_35Cont));
    $tmp->$class = &linesQ_L_35ContG_methods;
    linesQ_L_35ContG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct linesQ_L_35ContG_class linesQ_L_35ContG_methods;
B_NoneType linesQ_L_36procD___init__ (linesQ_L_36proc L_self, linesQ_main G_act, B_Env env) {
    L_self->G_act = G_act;
    L_self->env = env;
    return B_None;
}
$R linesQ_L_36procD___call__ (linesQ_L_36proc L_self, $Cont C_cont) {
    linesQ_main G_act = L_self->G_act;
    B_Env env = L_self->env;
    return (($R (*) (linesQ_main, $Cont, B_Env))G_act->$class->__init__)(G_act, C_cont, env);
}
$R linesQ_L_36procD___exec__ (linesQ_L_36proc L_self, $Cont C_cont) {
    return (($R (*) (linesQ_L_36proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void linesQ_L_36procD___serialize__ (linesQ_L_36proc self, $Serial$state state) {
    $step_serialize(self->G_act, state);
    $step_serialize(self->env, state);
}
linesQ_L_36proc linesQ_L_36procD___deserialize__ (linesQ_L_36proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct linesQ_L_36proc));
            self->$class = &linesQ_L_36procG_methods;
            return self;
        }
        self = $DNEW(linesQ_L_36proc, state);
    }
    self->G_act = $step_deserialize(state);
    self->env = $step_deserialize(state);
    return self;
}
linesQ_L_36proc linesQ_L_36procG_new(linesQ_main G_1, B_Env G_2) {
    linesQ_L_36proc $tmp = acton_malloc(sizeof(struct linesQ_L_36proc));
    $tmp->$class = &linesQ_L_36procG_methods;
    linesQ_L_36procG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct linesQ_L_36procG_class linesQ_L_36procG_methods;
$R linesQ_ApaD___init__ (linesQ_Apa self, $Cont C_cont) {
    #line 2 "test/src/lines.act"
    self->apa = toB_int(2001LL);
    #line 6 "test/src/lines.act"
    self->apb = toB_int(2002LL);
    #line 16 "test/src/lines.act"
    self->y = toB_int(123LL);
    return (($R (*) (linesQ_Apa, $Cont, $proc))self->$class->setupG_local)(self, (($Cont)linesQ_L_2ContG_new(self, C_cont)), (($proc)linesQ_L_4procG_new(self)));
}
#line 3 "test/src/lines.act"
$R linesQ_ApaD_setupG_local (linesQ_Apa self, $Cont C_cont, $proc cb) {
    #line 4 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("setup")), B_None, B_None, B_None, B_None);
    return (($R (*) ($proc, B_value, B_int))cb->$class->__exec__)(cb, ((B_value)linesQ_L_6ContG_new(C_cont)), toB_int(0LL));
}
#line 7 "test/src/lines.act"
$R linesQ_ApaD_computeG_local (linesQ_Apa self, $Cont C_cont, $action cb) {
    #line 8 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("compute")), B_None, B_None, B_None, B_None);
    return $AWAIT((($Cont)linesQ_L_8ContG_new(cb, C_cont)), ((B_Msg)((B_Msg (*) ($action, B_int))cb->$class->__asyn__)(cb, toB_int(1LL))));
}
#line 12 "test/src/lines.act"
$R linesQ_ApaD_noticeG_local (linesQ_Apa self, $Cont C_cont, B_int i) {
    #line 13 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("notice")), B_None, B_None, B_None, B_None);
    int64_t U_12N_1tmp = (((B_int)i)->val + 1LL);
    return $R_CONT(C_cont, toB_int(U_12N_1tmp));
}
B_Msg linesQ_ApaD_setup (linesQ_Apa self, $proc cb) {
    return $ASYNC((($Actor)self), (($Cont)linesQ_L_9procG_new(self, cb)));
}
B_Msg linesQ_ApaD_compute (linesQ_Apa self, $action cb) {
    return ((B_Msg)$ASYNC((($Actor)self), (($Cont)linesQ_L_10procG_new(self, cb))));
}
B_Msg linesQ_ApaD_notice (linesQ_Apa self, B_int i) {
    return ((B_Msg)$ASYNC((($Actor)self), (($Cont)linesQ_L_11procG_new(self, i))));
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
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("Bepa")), B_None, B_None, B_None, B_None);
    return $R_CONT(C_cont, B_None);
}
#line 22 "test/src/lines.act"
$R linesQ_BepaD_callbackG_local (linesQ_Bepa self, $Cont C_cont, B_int i) {
    #line 23 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(2, to$str("callback"), i), B_None, B_None, B_None, B_None);
    int64_t U_13N_2tmp = (((B_int)i)->val + 1LL);
    return $R_CONT(C_cont, toB_int(U_13N_2tmp));
}
B_Msg linesQ_BepaD_callback (linesQ_Bepa self, B_int i) {
    return ((B_Msg)$ASYNC((($Actor)self), (($Cont)linesQ_L_12procG_new(self, i))));
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
    self->env = env;
    B_Iterable W_1908 = (B_Iterable)B_SequenceD_listG_witness->W_Collection;
    B_Number W_2123 = (B_Number)B_RealFloatD_floatG_witness;
    return linesQ_ApaG_newact((($Cont)linesQ_L_25ContG_new(self, W_1908, W_2123, C_cont)));
}
#line 28 "test/src/lines.act"
$R linesQ_mainD_myprocG_local (linesQ_main self, $Cont C_cont, B_int i) {
    #line 29 "test/src/lines.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(2, to$str("myproc"), i), B_None, B_None, B_None, B_None);
    #line 30 "test/src/lines.act"
    if ((((B_int)i)->val == 2LL)) {
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
B_Msg linesQ_mainD_myproc (linesQ_main self, B_int i) {
    return ((B_Msg)$ASYNC((($Actor)self), (($Cont)linesQ_L_26procG_new(self, i))));
}
B_Msg linesQ_mainD_nop (linesQ_main self) {
    return $ASYNC((($Actor)self), (($Cont)linesQ_L_27procG_new(self)));
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
    return $AWAIT((($Cont)linesQ_L_29ContG_new(C_cont, G_act)), $ASYNC((($Actor)G_act), (($Cont)linesQ_L_30procG_new(G_act))));
}
$R linesQ_BepaG_newact ($Cont C_cont) {
    linesQ_Bepa G_act = $NEWACTOR(linesQ_Bepa);
    if ((void*)G_act->$class->__cleanup__ != (void*)$ActorD___cleanup__) $InstallFinalizer(G_act, linesQ_BepaD_GCfinalizer);
    return $AWAIT((($Cont)linesQ_L_32ContG_new(C_cont, G_act)), $ASYNC((($Actor)G_act), (($Cont)linesQ_L_33procG_new(G_act))));
}
$R linesQ_mainG_newact ($Cont C_cont, B_Env env) {
    linesQ_main G_act = $NEWACTOR(linesQ_main);
    if ((void*)G_act->$class->__cleanup__ != (void*)$ActorD___cleanup__) $InstallFinalizer(G_act, linesQ_mainD_GCfinalizer);
    return $AWAIT((($Cont)linesQ_L_35ContG_new(C_cont, G_act)), $ASYNC((($Actor)G_act), (($Cont)linesQ_L_36procG_new(G_act, env))));
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
        linesQ_L_4procG_methods.$GCINFO = "linesQ_L_4proc";
        linesQ_L_4procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        linesQ_L_4procG_methods.__bool__ = (B_bool (*) (linesQ_L_4proc))B_valueG_methods.__bool__;
        linesQ_L_4procG_methods.__str__ = (B_str (*) (linesQ_L_4proc))B_valueG_methods.__str__;
        linesQ_L_4procG_methods.__repr__ = (B_str (*) (linesQ_L_4proc))B_valueG_methods.__repr__;
        linesQ_L_4procG_methods.__init__ = linesQ_L_4procD___init__;
        linesQ_L_4procG_methods.__call__ = linesQ_L_4procD___call__;
        linesQ_L_4procG_methods.__exec__ = linesQ_L_4procD___exec__;
        linesQ_L_4procG_methods.__serialize__ = linesQ_L_4procD___serialize__;
        linesQ_L_4procG_methods.__deserialize__ = linesQ_L_4procD___deserialize__;
        $register(&linesQ_L_4procG_methods);
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
        linesQ_L_8ContG_methods.$GCINFO = "linesQ_L_8Cont";
        linesQ_L_8ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        linesQ_L_8ContG_methods.__bool__ = (B_bool (*) (linesQ_L_8Cont))B_valueG_methods.__bool__;
        linesQ_L_8ContG_methods.__str__ = (B_str (*) (linesQ_L_8Cont))B_valueG_methods.__str__;
        linesQ_L_8ContG_methods.__repr__ = (B_str (*) (linesQ_L_8Cont))B_valueG_methods.__repr__;
        linesQ_L_8ContG_methods.__init__ = linesQ_L_8ContD___init__;
        linesQ_L_8ContG_methods.__call__ = linesQ_L_8ContD___call__;
        linesQ_L_8ContG_methods.__serialize__ = linesQ_L_8ContD___serialize__;
        linesQ_L_8ContG_methods.__deserialize__ = linesQ_L_8ContD___deserialize__;
        $register(&linesQ_L_8ContG_methods);
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
        linesQ_L_11procG_methods.$GCINFO = "linesQ_L_11proc";
        linesQ_L_11procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        linesQ_L_11procG_methods.__bool__ = (B_bool (*) (linesQ_L_11proc))B_valueG_methods.__bool__;
        linesQ_L_11procG_methods.__str__ = (B_str (*) (linesQ_L_11proc))B_valueG_methods.__str__;
        linesQ_L_11procG_methods.__repr__ = (B_str (*) (linesQ_L_11proc))B_valueG_methods.__repr__;
        linesQ_L_11procG_methods.__init__ = linesQ_L_11procD___init__;
        linesQ_L_11procG_methods.__call__ = linesQ_L_11procD___call__;
        linesQ_L_11procG_methods.__exec__ = linesQ_L_11procD___exec__;
        linesQ_L_11procG_methods.__serialize__ = linesQ_L_11procD___serialize__;
        linesQ_L_11procG_methods.__deserialize__ = linesQ_L_11procD___deserialize__;
        $register(&linesQ_L_11procG_methods);
    }
    {
        linesQ_L_12procG_methods.$GCINFO = "linesQ_L_12proc";
        linesQ_L_12procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        linesQ_L_12procG_methods.__bool__ = (B_bool (*) (linesQ_L_12proc))B_valueG_methods.__bool__;
        linesQ_L_12procG_methods.__str__ = (B_str (*) (linesQ_L_12proc))B_valueG_methods.__str__;
        linesQ_L_12procG_methods.__repr__ = (B_str (*) (linesQ_L_12proc))B_valueG_methods.__repr__;
        linesQ_L_12procG_methods.__init__ = linesQ_L_12procD___init__;
        linesQ_L_12procG_methods.__call__ = linesQ_L_12procD___call__;
        linesQ_L_12procG_methods.__exec__ = linesQ_L_12procD___exec__;
        linesQ_L_12procG_methods.__serialize__ = linesQ_L_12procD___serialize__;
        linesQ_L_12procG_methods.__deserialize__ = linesQ_L_12procD___deserialize__;
        $register(&linesQ_L_12procG_methods);
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
        linesQ_L_18actionG_methods.$GCINFO = "linesQ_L_18action";
        linesQ_L_18actionG_methods.$superclass = ($SuperG_class)&$actionG_methods;
        linesQ_L_18actionG_methods.__bool__ = (B_bool (*) (linesQ_L_18action))B_valueG_methods.__bool__;
        linesQ_L_18actionG_methods.__str__ = (B_str (*) (linesQ_L_18action))B_valueG_methods.__str__;
        linesQ_L_18actionG_methods.__repr__ = (B_str (*) (linesQ_L_18action))B_valueG_methods.__repr__;
        linesQ_L_18actionG_methods.__init__ = linesQ_L_18actionD___init__;
        linesQ_L_18actionG_methods.__call__ = linesQ_L_18actionD___call__;
        linesQ_L_18actionG_methods.__exec__ = linesQ_L_18actionD___exec__;
        linesQ_L_18actionG_methods.__asyn__ = linesQ_L_18actionD___asyn__;
        linesQ_L_18actionG_methods.__serialize__ = linesQ_L_18actionD___serialize__;
        linesQ_L_18actionG_methods.__deserialize__ = linesQ_L_18actionD___deserialize__;
        $register(&linesQ_L_18actionG_methods);
    }
    {
        linesQ_L_21actionG_methods.$GCINFO = "linesQ_L_21action";
        linesQ_L_21actionG_methods.$superclass = ($SuperG_class)&$actionG_methods;
        linesQ_L_21actionG_methods.__bool__ = (B_bool (*) (linesQ_L_21action))B_valueG_methods.__bool__;
        linesQ_L_21actionG_methods.__str__ = (B_str (*) (linesQ_L_21action))B_valueG_methods.__str__;
        linesQ_L_21actionG_methods.__repr__ = (B_str (*) (linesQ_L_21action))B_valueG_methods.__repr__;
        linesQ_L_21actionG_methods.__init__ = linesQ_L_21actionD___init__;
        linesQ_L_21actionG_methods.__call__ = linesQ_L_21actionD___call__;
        linesQ_L_21actionG_methods.__exec__ = linesQ_L_21actionD___exec__;
        linesQ_L_21actionG_methods.__asyn__ = linesQ_L_21actionD___asyn__;
        linesQ_L_21actionG_methods.__serialize__ = linesQ_L_21actionD___serialize__;
        linesQ_L_21actionG_methods.__deserialize__ = linesQ_L_21actionD___deserialize__;
        $register(&linesQ_L_21actionG_methods);
    }
    {
        linesQ_L_22procG_methods.$GCINFO = "linesQ_L_22proc";
        linesQ_L_22procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        linesQ_L_22procG_methods.__bool__ = (B_bool (*) (linesQ_L_22proc))B_valueG_methods.__bool__;
        linesQ_L_22procG_methods.__str__ = (B_str (*) (linesQ_L_22proc))B_valueG_methods.__str__;
        linesQ_L_22procG_methods.__repr__ = (B_str (*) (linesQ_L_22proc))B_valueG_methods.__repr__;
        linesQ_L_22procG_methods.__init__ = linesQ_L_22procD___init__;
        linesQ_L_22procG_methods.__call__ = linesQ_L_22procD___call__;
        linesQ_L_22procG_methods.__exec__ = linesQ_L_22procD___exec__;
        linesQ_L_22procG_methods.__serialize__ = linesQ_L_22procD___serialize__;
        linesQ_L_22procG_methods.__deserialize__ = linesQ_L_22procD___deserialize__;
        $register(&linesQ_L_22procG_methods);
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
        linesQ_L_24ContG_methods.$GCINFO = "linesQ_L_24Cont";
        linesQ_L_24ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        linesQ_L_24ContG_methods.__bool__ = (B_bool (*) (linesQ_L_24Cont))B_valueG_methods.__bool__;
        linesQ_L_24ContG_methods.__str__ = (B_str (*) (linesQ_L_24Cont))B_valueG_methods.__str__;
        linesQ_L_24ContG_methods.__repr__ = (B_str (*) (linesQ_L_24Cont))B_valueG_methods.__repr__;
        linesQ_L_24ContG_methods.__init__ = linesQ_L_24ContD___init__;
        linesQ_L_24ContG_methods.__call__ = linesQ_L_24ContD___call__;
        linesQ_L_24ContG_methods.__serialize__ = linesQ_L_24ContD___serialize__;
        linesQ_L_24ContG_methods.__deserialize__ = linesQ_L_24ContD___deserialize__;
        $register(&linesQ_L_24ContG_methods);
    }
    {
        linesQ_L_25ContG_methods.$GCINFO = "linesQ_L_25Cont";
        linesQ_L_25ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        linesQ_L_25ContG_methods.__bool__ = (B_bool (*) (linesQ_L_25Cont))B_valueG_methods.__bool__;
        linesQ_L_25ContG_methods.__str__ = (B_str (*) (linesQ_L_25Cont))B_valueG_methods.__str__;
        linesQ_L_25ContG_methods.__repr__ = (B_str (*) (linesQ_L_25Cont))B_valueG_methods.__repr__;
        linesQ_L_25ContG_methods.__init__ = linesQ_L_25ContD___init__;
        linesQ_L_25ContG_methods.__call__ = linesQ_L_25ContD___call__;
        linesQ_L_25ContG_methods.__serialize__ = linesQ_L_25ContD___serialize__;
        linesQ_L_25ContG_methods.__deserialize__ = linesQ_L_25ContD___deserialize__;
        $register(&linesQ_L_25ContG_methods);
    }
    {
        linesQ_L_26procG_methods.$GCINFO = "linesQ_L_26proc";
        linesQ_L_26procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        linesQ_L_26procG_methods.__bool__ = (B_bool (*) (linesQ_L_26proc))B_valueG_methods.__bool__;
        linesQ_L_26procG_methods.__str__ = (B_str (*) (linesQ_L_26proc))B_valueG_methods.__str__;
        linesQ_L_26procG_methods.__repr__ = (B_str (*) (linesQ_L_26proc))B_valueG_methods.__repr__;
        linesQ_L_26procG_methods.__init__ = linesQ_L_26procD___init__;
        linesQ_L_26procG_methods.__call__ = linesQ_L_26procD___call__;
        linesQ_L_26procG_methods.__exec__ = linesQ_L_26procD___exec__;
        linesQ_L_26procG_methods.__serialize__ = linesQ_L_26procD___serialize__;
        linesQ_L_26procG_methods.__deserialize__ = linesQ_L_26procD___deserialize__;
        $register(&linesQ_L_26procG_methods);
    }
    {
        linesQ_L_27procG_methods.$GCINFO = "linesQ_L_27proc";
        linesQ_L_27procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        linesQ_L_27procG_methods.__bool__ = (B_bool (*) (linesQ_L_27proc))B_valueG_methods.__bool__;
        linesQ_L_27procG_methods.__str__ = (B_str (*) (linesQ_L_27proc))B_valueG_methods.__str__;
        linesQ_L_27procG_methods.__repr__ = (B_str (*) (linesQ_L_27proc))B_valueG_methods.__repr__;
        linesQ_L_27procG_methods.__init__ = linesQ_L_27procD___init__;
        linesQ_L_27procG_methods.__call__ = linesQ_L_27procD___call__;
        linesQ_L_27procG_methods.__exec__ = linesQ_L_27procD___exec__;
        linesQ_L_27procG_methods.__serialize__ = linesQ_L_27procD___serialize__;
        linesQ_L_27procG_methods.__deserialize__ = linesQ_L_27procD___deserialize__;
        $register(&linesQ_L_27procG_methods);
    }
    {
        linesQ_L_29ContG_methods.$GCINFO = "linesQ_L_29Cont";
        linesQ_L_29ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        linesQ_L_29ContG_methods.__bool__ = (B_bool (*) (linesQ_L_29Cont))B_valueG_methods.__bool__;
        linesQ_L_29ContG_methods.__str__ = (B_str (*) (linesQ_L_29Cont))B_valueG_methods.__str__;
        linesQ_L_29ContG_methods.__repr__ = (B_str (*) (linesQ_L_29Cont))B_valueG_methods.__repr__;
        linesQ_L_29ContG_methods.__init__ = linesQ_L_29ContD___init__;
        linesQ_L_29ContG_methods.__call__ = linesQ_L_29ContD___call__;
        linesQ_L_29ContG_methods.__serialize__ = linesQ_L_29ContD___serialize__;
        linesQ_L_29ContG_methods.__deserialize__ = linesQ_L_29ContD___deserialize__;
        $register(&linesQ_L_29ContG_methods);
    }
    {
        linesQ_L_30procG_methods.$GCINFO = "linesQ_L_30proc";
        linesQ_L_30procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        linesQ_L_30procG_methods.__bool__ = (B_bool (*) (linesQ_L_30proc))B_valueG_methods.__bool__;
        linesQ_L_30procG_methods.__str__ = (B_str (*) (linesQ_L_30proc))B_valueG_methods.__str__;
        linesQ_L_30procG_methods.__repr__ = (B_str (*) (linesQ_L_30proc))B_valueG_methods.__repr__;
        linesQ_L_30procG_methods.__init__ = linesQ_L_30procD___init__;
        linesQ_L_30procG_methods.__call__ = linesQ_L_30procD___call__;
        linesQ_L_30procG_methods.__exec__ = linesQ_L_30procD___exec__;
        linesQ_L_30procG_methods.__serialize__ = linesQ_L_30procD___serialize__;
        linesQ_L_30procG_methods.__deserialize__ = linesQ_L_30procD___deserialize__;
        $register(&linesQ_L_30procG_methods);
    }
    {
        linesQ_L_32ContG_methods.$GCINFO = "linesQ_L_32Cont";
        linesQ_L_32ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        linesQ_L_32ContG_methods.__bool__ = (B_bool (*) (linesQ_L_32Cont))B_valueG_methods.__bool__;
        linesQ_L_32ContG_methods.__str__ = (B_str (*) (linesQ_L_32Cont))B_valueG_methods.__str__;
        linesQ_L_32ContG_methods.__repr__ = (B_str (*) (linesQ_L_32Cont))B_valueG_methods.__repr__;
        linesQ_L_32ContG_methods.__init__ = linesQ_L_32ContD___init__;
        linesQ_L_32ContG_methods.__call__ = linesQ_L_32ContD___call__;
        linesQ_L_32ContG_methods.__serialize__ = linesQ_L_32ContD___serialize__;
        linesQ_L_32ContG_methods.__deserialize__ = linesQ_L_32ContD___deserialize__;
        $register(&linesQ_L_32ContG_methods);
    }
    {
        linesQ_L_33procG_methods.$GCINFO = "linesQ_L_33proc";
        linesQ_L_33procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        linesQ_L_33procG_methods.__bool__ = (B_bool (*) (linesQ_L_33proc))B_valueG_methods.__bool__;
        linesQ_L_33procG_methods.__str__ = (B_str (*) (linesQ_L_33proc))B_valueG_methods.__str__;
        linesQ_L_33procG_methods.__repr__ = (B_str (*) (linesQ_L_33proc))B_valueG_methods.__repr__;
        linesQ_L_33procG_methods.__init__ = linesQ_L_33procD___init__;
        linesQ_L_33procG_methods.__call__ = linesQ_L_33procD___call__;
        linesQ_L_33procG_methods.__exec__ = linesQ_L_33procD___exec__;
        linesQ_L_33procG_methods.__serialize__ = linesQ_L_33procD___serialize__;
        linesQ_L_33procG_methods.__deserialize__ = linesQ_L_33procD___deserialize__;
        $register(&linesQ_L_33procG_methods);
    }
    {
        linesQ_L_35ContG_methods.$GCINFO = "linesQ_L_35Cont";
        linesQ_L_35ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        linesQ_L_35ContG_methods.__bool__ = (B_bool (*) (linesQ_L_35Cont))B_valueG_methods.__bool__;
        linesQ_L_35ContG_methods.__str__ = (B_str (*) (linesQ_L_35Cont))B_valueG_methods.__str__;
        linesQ_L_35ContG_methods.__repr__ = (B_str (*) (linesQ_L_35Cont))B_valueG_methods.__repr__;
        linesQ_L_35ContG_methods.__init__ = linesQ_L_35ContD___init__;
        linesQ_L_35ContG_methods.__call__ = linesQ_L_35ContD___call__;
        linesQ_L_35ContG_methods.__serialize__ = linesQ_L_35ContD___serialize__;
        linesQ_L_35ContG_methods.__deserialize__ = linesQ_L_35ContD___deserialize__;
        $register(&linesQ_L_35ContG_methods);
    }
    {
        linesQ_L_36procG_methods.$GCINFO = "linesQ_L_36proc";
        linesQ_L_36procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        linesQ_L_36procG_methods.__bool__ = (B_bool (*) (linesQ_L_36proc))B_valueG_methods.__bool__;
        linesQ_L_36procG_methods.__str__ = (B_str (*) (linesQ_L_36proc))B_valueG_methods.__str__;
        linesQ_L_36procG_methods.__repr__ = (B_str (*) (linesQ_L_36proc))B_valueG_methods.__repr__;
        linesQ_L_36procG_methods.__init__ = linesQ_L_36procD___init__;
        linesQ_L_36procG_methods.__call__ = linesQ_L_36procD___call__;
        linesQ_L_36procG_methods.__exec__ = linesQ_L_36procD___exec__;
        linesQ_L_36procG_methods.__serialize__ = linesQ_L_36procD___serialize__;
        linesQ_L_36procG_methods.__deserialize__ = linesQ_L_36procD___deserialize__;
        $register(&linesQ_L_36procG_methods);
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
}