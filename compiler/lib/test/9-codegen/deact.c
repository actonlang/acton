/* Acton source hash: test-hash */
#include "rts/common.h"
#include "out/types/deact.h"
<<<<<<< HEAD
=======
B_Plus deactQ_W_313;
B_Times deactQ_W_223;
B_Eq deactQ_W_761;
>>>>>>> 31837f3b (Update golden files)
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
B_NoneType deactQ_L_4procD___init__ (deactQ_L_4proc L_self, deactQ_Apa L_3obj) {
    L_self->L_3obj = L_3obj;
    return B_None;
}
$R deactQ_L_4procD___call__ (deactQ_L_4proc L_self, $Cont G_1, B_int G_2) {
    deactQ_Apa L_3obj = L_self->L_3obj;
    return (($R (*) (deactQ_Apa, $Cont, B_int))L_3obj->$class->noticeG_local)(L_3obj, G_1, G_2);
}
$R deactQ_L_4procD___exec__ (deactQ_L_4proc L_self, $Cont G_1, B_int G_2) {
    return (($R (*) (deactQ_L_4proc, $Cont, B_int))L_self->$class->__call__)(L_self, G_1, G_2);
}
void deactQ_L_4procD___serialize__ (deactQ_L_4proc self, $Serial$state state) {
    $step_serialize(self->L_3obj, state);
}
deactQ_L_4proc deactQ_L_4procD___deserialize__ (deactQ_L_4proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_L_4proc));
            self->$class = &deactQ_L_4procG_methods;
            return self;
        }
        self = $DNEW(deactQ_L_4proc, state);
    }
    self->L_3obj = $step_deserialize(state);
    return self;
}
deactQ_L_4proc deactQ_L_4procG_new(deactQ_Apa G_1) {
    deactQ_L_4proc $tmp = acton_malloc(sizeof(struct deactQ_L_4proc));
    $tmp->$class = &deactQ_L_4procG_methods;
    deactQ_L_4procG_methods.__init__($tmp, G_1);
    return $tmp;
}
<<<<<<< HEAD
struct deactQ_L_4actionG_class deactQ_L_4actionG_methods;
$R deactQ_U_L_5C_3cont ($action cb, $Cont C_cont, int64_t U_2C_4res) {
    #line 7 "test/src/deact.act"
    int64_t U_3v = U_2C_4res;
    #line 8 "test/src/deact.act"
    B_Msg m = ((B_Msg)((B_Msg (*) ($action, B_int))cb->$class->__asyn__)(cb, toB_int(2LL)));
    int64_t U_4N_tmp = (U_3v * 10LL);
    return $R_CONT(C_cont, toB_int(U_4N_tmp));
}
$R deactQ_L_5C_3cont ($action cb, $Cont C_cont, B_int C_4res) {
    return deactQ_U_L_5C_3cont(cb, C_cont, ((B_int)C_4res)->val);
=======
struct deactQ_L_4procG_class deactQ_L_4procG_methods;
$R deactQ_L_5C_3cont ($Cont C_cont, B_int C_4res) {
    return $R_CONT(C_cont, B_None);
>>>>>>> ebe2c02f (Golden updates, mostly just new numbers but also some *internal* effects that become proc instead of action)
}
B_NoneType deactQ_L_6ContD___init__ (deactQ_L_6Cont L_self, $Cont C_cont) {
    L_self->C_cont = C_cont;
    return B_None;
}
$R deactQ_L_6ContD___call__ (deactQ_L_6Cont L_self, B_int G_1) {
    $Cont C_cont = L_self->C_cont;
    return deactQ_L_5C_3cont(C_cont, G_1);
}
void deactQ_L_6ContD___serialize__ (deactQ_L_6Cont self, $Serial$state state) {
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
    self->C_cont = $step_deserialize(state);
    return self;
}
deactQ_L_6Cont deactQ_L_6ContG_new($Cont G_1) {
    deactQ_L_6Cont $tmp = acton_malloc(sizeof(struct deactQ_L_6Cont));
    $tmp->$class = &deactQ_L_6ContG_methods;
    deactQ_L_6ContG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct deactQ_L_6ContG_class deactQ_L_6ContG_methods;
$R deactQ_L_7C_5cont ($action cb, $Cont C_cont, B_int C_6res) {
    #line 7 "test/src/deact.act"
    B_int v = C_6res;
    #line 8 "test/src/deact.act"
    B_Msg m = ((B_Msg)((B_Msg (*) ($action, B_int))cb->$class->__asyn__)(cb, to$int(2)));
    B_int N_tmp = ((B_int (*) (B_Times, B_int, B_int))deactQ_W_223->$class->__mul__)(deactQ_W_223, v, to$int(10));
    return $R_CONT(C_cont, N_tmp);
}
B_NoneType deactQ_L_8ContD___init__ (deactQ_L_8Cont L_self, $action cb, $Cont C_cont) {
    L_self->cb = cb;
    L_self->C_cont = C_cont;
    return B_None;
}
$R deactQ_L_8ContD___call__ (deactQ_L_8Cont L_self, B_int G_1) {
    $action cb = L_self->cb;
    $Cont C_cont = L_self->C_cont;
    return deactQ_L_7C_5cont(cb, C_cont, G_1);
}
void deactQ_L_8ContD___serialize__ (deactQ_L_8Cont self, $Serial$state state) {
    $step_serialize(self->cb, state);
    $step_serialize(self->C_cont, state);
}
deactQ_L_8Cont deactQ_L_8ContD___deserialize__ (deactQ_L_8Cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_L_8Cont));
            self->$class = &deactQ_L_8ContG_methods;
            return self;
        }
        self = $DNEW(deactQ_L_8Cont, state);
    }
    self->cb = $step_deserialize(state);
    self->C_cont = $step_deserialize(state);
    return self;
}
deactQ_L_8Cont deactQ_L_8ContG_new($action G_1, $Cont G_2) {
    deactQ_L_8Cont $tmp = acton_malloc(sizeof(struct deactQ_L_8Cont));
    $tmp->$class = &deactQ_L_8ContG_methods;
    deactQ_L_8ContG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct deactQ_L_8ContG_class deactQ_L_8ContG_methods;
B_NoneType deactQ_L_9procD___init__ (deactQ_L_9proc L_self, deactQ_Apa self, $proc cb) {
    L_self->self = self;
    L_self->cb = cb;
    return B_None;
}
$R deactQ_L_9procD___call__ (deactQ_L_9proc L_self, $Cont C_cont) {
    deactQ_Apa self = L_self->self;
<<<<<<< HEAD
    int64_t U_5i = ((B_int)L_self->i)->val;
    return (($R (*) (deactQ_Apa, $Cont, B_int))self->$class->noticeG_local)(self, C_cont, toB_int(U_5i));
=======
    $proc cb = L_self->cb;
    return (($R (*) (deactQ_Apa, $Cont, $proc))self->$class->setupG_local)(self, C_cont, cb);
>>>>>>> ebe2c02f (Golden updates, mostly just new numbers but also some *internal* effects that become proc instead of action)
}
$R deactQ_L_9procD___exec__ (deactQ_L_9proc L_self, $Cont C_cont) {
    return (($R (*) (deactQ_L_9proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void deactQ_L_9procD___serialize__ (deactQ_L_9proc self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->cb, state);
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
    self->cb = $step_deserialize(state);
    return self;
}
deactQ_L_9proc deactQ_L_9procG_new(deactQ_Apa G_1, $proc G_2) {
    deactQ_L_9proc $tmp = acton_malloc(sizeof(struct deactQ_L_9proc));
    $tmp->$class = &deactQ_L_9procG_methods;
    deactQ_L_9procG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct deactQ_L_9procG_class deactQ_L_9procG_methods;
B_NoneType deactQ_L_10procD___init__ (deactQ_L_10proc L_self, deactQ_Apa self, $action cb) {
    L_self->self = self;
    L_self->cb = cb;
    return B_None;
}
$R deactQ_L_10procD___call__ (deactQ_L_10proc L_self, $Cont C_cont) {
<<<<<<< HEAD
    deactQ_Bepa self = L_self->self;
    int64_t U_6i = ((B_int)L_self->i)->val;
    return (($R (*) (deactQ_Bepa, $Cont, B_int))self->$class->callbackG_local)(self, C_cont, toB_int(U_6i));
=======
    deactQ_Apa self = L_self->self;
    $action cb = L_self->cb;
    return (($R (*) (deactQ_Apa, $Cont, $action))self->$class->computeG_local)(self, C_cont, cb);
>>>>>>> ebe2c02f (Golden updates, mostly just new numbers but also some *internal* effects that become proc instead of action)
}
$R deactQ_L_10procD___exec__ (deactQ_L_10proc L_self, $Cont C_cont) {
    return (($R (*) (deactQ_L_10proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void deactQ_L_10procD___serialize__ (deactQ_L_10proc self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->cb, state);
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
    self->cb = $step_deserialize(state);
    return self;
}
deactQ_L_10proc deactQ_L_10procG_new(deactQ_Apa G_1, $action G_2) {
    deactQ_L_10proc $tmp = acton_malloc(sizeof(struct deactQ_L_10proc));
    $tmp->$class = &deactQ_L_10procG_methods;
    deactQ_L_10procG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct deactQ_L_10procG_class deactQ_L_10procG_methods;
B_NoneType deactQ_L_11procD___init__ (deactQ_L_11proc L_self, deactQ_Apa self, B_int i) {
    L_self->self = self;
    L_self->i = i;
    return B_None;
}
$R deactQ_L_11procD___call__ (deactQ_L_11proc L_self, $Cont C_cont) {
    deactQ_Apa self = L_self->self;
    B_int i = L_self->i;
    return (($R (*) (deactQ_Apa, $Cont, B_int))self->$class->noticeG_local)(self, C_cont, i);
}
$R deactQ_L_11procD___exec__ (deactQ_L_11proc L_self, $Cont C_cont) {
    return (($R (*) (deactQ_L_11proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void deactQ_L_11procD___serialize__ (deactQ_L_11proc self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->i, state);
}
deactQ_L_11proc deactQ_L_11procD___deserialize__ (deactQ_L_11proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_L_11proc));
            self->$class = &deactQ_L_11procG_methods;
            return self;
        }
        self = $DNEW(deactQ_L_11proc, state);
    }
    self->self = $step_deserialize(state);
    self->i = $step_deserialize(state);
    return self;
}
deactQ_L_11proc deactQ_L_11procG_new(deactQ_Apa G_1, B_int G_2) {
    deactQ_L_11proc $tmp = acton_malloc(sizeof(struct deactQ_L_11proc));
    $tmp->$class = &deactQ_L_11procG_methods;
    deactQ_L_11procG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct deactQ_L_11procG_class deactQ_L_11procG_methods;
B_NoneType deactQ_L_12procD___init__ (deactQ_L_12proc L_self, deactQ_Bepa self, B_int i) {
    L_self->self = self;
    L_self->i = i;
    return B_None;
}
$R deactQ_L_12procD___call__ (deactQ_L_12proc L_self, $Cont C_cont) {
    deactQ_Bepa self = L_self->self;
    B_int i = L_self->i;
    return (($R (*) (deactQ_Bepa, $Cont, B_int))self->$class->callbackG_local)(self, C_cont, i);
}
$R deactQ_L_12procD___exec__ (deactQ_L_12proc L_self, $Cont C_cont) {
    return (($R (*) (deactQ_L_12proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void deactQ_L_12procD___serialize__ (deactQ_L_12proc self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->i, state);
}
deactQ_L_12proc deactQ_L_12procD___deserialize__ (deactQ_L_12proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_L_12proc));
            self->$class = &deactQ_L_12procG_methods;
            return self;
        }
        self = $DNEW(deactQ_L_12proc, state);
    }
    self->self = $step_deserialize(state);
    self->i = $step_deserialize(state);
    return self;
}
deactQ_L_12proc deactQ_L_12procG_new(deactQ_Bepa G_1, B_int G_2) {
    deactQ_L_12proc $tmp = acton_malloc(sizeof(struct deactQ_L_12proc));
    $tmp->$class = &deactQ_L_12procG_methods;
    deactQ_L_12procG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct deactQ_L_12procG_class deactQ_L_12procG_methods;
B_NoneType deactQ_L_16actionD___init__ (deactQ_L_16action L_self, deactQ_Apa L_15obj) {
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
    deactQ_Apa L_15obj = L_self->L_15obj;
    return ((B_Msg)((B_Msg (*) (deactQ_Apa, B_int))L_15obj->$class->notice)(L_15obj, G_1));
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
deactQ_L_16action deactQ_L_16actionG_new(deactQ_Apa G_1) {
    deactQ_L_16action $tmp = acton_malloc(sizeof(struct deactQ_L_16action));
    $tmp->$class = &deactQ_L_16actionG_methods;
    deactQ_L_16actionG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct deactQ_L_16actionG_class deactQ_L_16actionG_methods;
B_NoneType deactQ_L_18actionD___init__ (deactQ_L_18action L_self, deactQ_Bepa L_17obj) {
    L_self->L_17obj = L_17obj;
    return B_None;
}
$R deactQ_L_18actionD___call__ (deactQ_L_18action L_self, $Cont L_cont, B_int G_1) {
    return $AWAIT(L_cont, ((B_Msg)((B_Msg (*) (deactQ_L_18action, B_int))L_self->$class->__asyn__)(L_self, G_1)));
}
$R deactQ_L_18actionD___exec__ (deactQ_L_18action L_self, $Cont L_cont, B_int G_1) {
    return $R_CONT(L_cont, ((B_value)((B_Msg (*) (deactQ_L_18action, B_int))L_self->$class->__asyn__)(L_self, G_1)));
}
B_Msg deactQ_L_18actionD___asyn__ (deactQ_L_18action L_self, B_int G_1) {
    deactQ_Bepa L_17obj = L_self->L_17obj;
    return ((B_Msg)((B_Msg (*) (deactQ_Bepa, B_int))L_17obj->$class->callback)(L_17obj, G_1));
}
void deactQ_L_18actionD___serialize__ (deactQ_L_18action self, $Serial$state state) {
    $step_serialize(self->L_17obj, state);
}
deactQ_L_18action deactQ_L_18actionD___deserialize__ (deactQ_L_18action self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_L_18action));
            self->$class = &deactQ_L_18actionG_methods;
            return self;
        }
        self = $DNEW(deactQ_L_18action, state);
    }
    self->L_17obj = $step_deserialize(state);
    return self;
}
deactQ_L_18action deactQ_L_18actionG_new(deactQ_Bepa G_1) {
    deactQ_L_18action $tmp = acton_malloc(sizeof(struct deactQ_L_18action));
    $tmp->$class = &deactQ_L_18actionG_methods;
    deactQ_L_18actionG_methods.__init__($tmp, G_1);
    return $tmp;
}
<<<<<<< HEAD
struct deactQ_L_19actionG_class deactQ_L_19actionG_methods;
$R deactQ_U_1L_17C_9cont (deactQ_main self, $Cont C_cont, int64_t U_7C_10res) {
    #line 34 "test/src/deact.act"
    self->r = toB_int(U_7C_10res);
=======
struct deactQ_L_18actionG_class deactQ_L_18actionG_methods;
B_NoneType deactQ_L_21actionD___init__ (deactQ_L_21action L_self, deactQ_main L_20obj) {
    L_self->L_20obj = L_20obj;
    return B_None;
}
$R deactQ_L_21actionD___call__ (deactQ_L_21action L_self, $Cont L_cont, B_int G_1) {
    return $AWAIT(L_cont, ((B_Msg)((B_Msg (*) (deactQ_L_21action, B_int))L_self->$class->__asyn__)(L_self, G_1)));
}
$R deactQ_L_21actionD___exec__ (deactQ_L_21action L_self, $Cont L_cont, B_int G_1) {
    return $R_CONT(L_cont, ((B_value)((B_Msg (*) (deactQ_L_21action, B_int))L_self->$class->__asyn__)(L_self, G_1)));
}
B_Msg deactQ_L_21actionD___asyn__ (deactQ_L_21action L_self, B_int G_1) {
    deactQ_main L_20obj = L_self->L_20obj;
    return ((B_Msg)((B_Msg (*) (deactQ_main, B_int))L_20obj->$class->myproc)(L_20obj, G_1));
}
void deactQ_L_21actionD___serialize__ (deactQ_L_21action self, $Serial$state state) {
    $step_serialize(self->L_20obj, state);
}
deactQ_L_21action deactQ_L_21actionD___deserialize__ (deactQ_L_21action self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_L_21action));
            self->$class = &deactQ_L_21actionG_methods;
            return self;
        }
        self = $DNEW(deactQ_L_21action, state);
    }
    self->L_20obj = $step_deserialize(state);
    return self;
}
deactQ_L_21action deactQ_L_21actionG_new(deactQ_main G_1) {
    deactQ_L_21action $tmp = acton_malloc(sizeof(struct deactQ_L_21action));
    $tmp->$class = &deactQ_L_21actionG_methods;
    deactQ_L_21actionG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct deactQ_L_21actionG_class deactQ_L_21actionG_methods;
$R deactQ_L_19C_11cont (deactQ_main self, $Cont C_cont, B_int C_12res) {
    #line 34 "test/src/deact.act"
    self->r = C_12res;
>>>>>>> ebe2c02f (Golden updates, mostly just new numbers but also some *internal* effects that become proc instead of action)
    #line 35 "test/src/deact.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(2, to$str("r ="), self->r), B_None, B_None, B_None, B_None);
    #line 36 "test/src/deact.act"
    ((B_Msg (*) (deactQ_Apa, $action))self->a->$class->compute)(self->a, (($action)deactQ_L_21actionG_new(self)));
    #line 37 "test/src/deact.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("main")), B_None, B_None, B_None, B_None);
    return $R_CONT(C_cont, B_None);
}
<<<<<<< HEAD
$R deactQ_L_17C_9cont (deactQ_main self, $Cont C_cont, B_int C_10res) {
    return deactQ_U_1L_17C_9cont(self, C_cont, ((B_int)C_10res)->val);
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
=======
>>>>>>> ebe2c02f (Golden updates, mostly just new numbers but also some *internal* effects that become proc instead of action)
B_NoneType deactQ_L_22ContD___init__ (deactQ_L_22Cont L_self, deactQ_main self, $Cont C_cont) {
    L_self->self = self;
    L_self->C_cont = C_cont;
    return B_None;
}
$R deactQ_L_22ContD___call__ (deactQ_L_22Cont L_self, B_int G_1) {
    deactQ_main self = L_self->self;
    $Cont C_cont = L_self->C_cont;
    return deactQ_L_19C_11cont(self, C_cont, G_1);
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
$R deactQ_L_14C_9cont (deactQ_main self, $Cont C_cont, deactQ_Bepa C_10res) {
    #line 30 "test/src/deact.act"
    self->b = C_10res;
    #line 31 "test/src/deact.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("-----")), B_None, B_None, B_None, B_None);
    #line 32 "test/src/deact.act"
    ((B_Msg (*) (deactQ_Apa, $proc))self->a->$class->setup)(self->a, (($proc)deactQ_L_16actionG_new(self->a)));
    #line 33 "test/src/deact.act"
    self->x = ((B_Msg (*) (deactQ_Apa, $action))self->a->$class->compute)(self->a, (($action)deactQ_L_18actionG_new(self->b)));
    return $AWAIT((($Cont)deactQ_L_22ContG_new(self, C_cont)), self->x);
}
B_NoneType deactQ_L_23ContD___init__ (deactQ_L_23Cont L_self, deactQ_main self, $Cont C_cont) {
    L_self->self = self;
    L_self->C_cont = C_cont;
    return B_None;
}
$R deactQ_L_23ContD___call__ (deactQ_L_23Cont L_self, deactQ_Bepa G_1) {
    deactQ_main self = L_self->self;
    $Cont C_cont = L_self->C_cont;
    return deactQ_L_14C_9cont(self, C_cont, G_1);
}
void deactQ_L_23ContD___serialize__ (deactQ_L_23Cont self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->C_cont, state);
}
deactQ_L_23Cont deactQ_L_23ContD___deserialize__ (deactQ_L_23Cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_L_23Cont));
            self->$class = &deactQ_L_23ContG_methods;
            return self;
        }
        self = $DNEW(deactQ_L_23Cont, state);
    }
    self->self = $step_deserialize(state);
    self->C_cont = $step_deserialize(state);
    return self;
}
deactQ_L_23Cont deactQ_L_23ContG_new(deactQ_main G_1, $Cont G_2) {
    deactQ_L_23Cont $tmp = acton_malloc(sizeof(struct deactQ_L_23Cont));
    $tmp->$class = &deactQ_L_23ContG_methods;
    deactQ_L_23ContG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct deactQ_L_23ContG_class deactQ_L_23ContG_methods;
$R deactQ_L_13C_7cont (deactQ_main self, $Cont C_cont, deactQ_Apa C_8res) {
    #line 29 "test/src/deact.act"
    self->a = C_8res;
    return deactQ_BepaG_newact((($Cont)deactQ_L_23ContG_new(self, C_cont)));
}
B_NoneType deactQ_L_24ContD___init__ (deactQ_L_24Cont L_self, deactQ_main self, $Cont C_cont) {
    L_self->self = self;
    L_self->C_cont = C_cont;
    return B_None;
}
$R deactQ_L_24ContD___call__ (deactQ_L_24Cont L_self, deactQ_Apa G_1) {
    deactQ_main self = L_self->self;
    $Cont C_cont = L_self->C_cont;
    return deactQ_L_13C_7cont(self, C_cont, G_1);
}
void deactQ_L_24ContD___serialize__ (deactQ_L_24Cont self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->C_cont, state);
}
deactQ_L_24Cont deactQ_L_24ContD___deserialize__ (deactQ_L_24Cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_L_24Cont));
            self->$class = &deactQ_L_24ContG_methods;
            return self;
        }
        self = $DNEW(deactQ_L_24Cont, state);
    }
    self->self = $step_deserialize(state);
    self->C_cont = $step_deserialize(state);
    return self;
}
deactQ_L_24Cont deactQ_L_24ContG_new(deactQ_main G_1, $Cont G_2) {
    deactQ_L_24Cont $tmp = acton_malloc(sizeof(struct deactQ_L_24Cont));
    $tmp->$class = &deactQ_L_24ContG_methods;
    deactQ_L_24ContG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct deactQ_L_24ContG_class deactQ_L_24ContG_methods;
B_NoneType deactQ_L_25procD___init__ (deactQ_L_25proc L_self, deactQ_main self, B_int i) {
    L_self->self = self;
    L_self->i = i;
    return B_None;
}
$R deactQ_L_25procD___call__ (deactQ_L_25proc L_self, $Cont C_cont) {
    deactQ_main self = L_self->self;
    int64_t U_8i = ((B_int)L_self->i)->val;
    return (($R (*) (deactQ_main, $Cont, B_int))self->$class->myprocG_local)(self, C_cont, toB_int(U_8i));
}
$R deactQ_L_25procD___exec__ (deactQ_L_25proc L_self, $Cont C_cont) {
    return (($R (*) (deactQ_L_25proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void deactQ_L_25procD___serialize__ (deactQ_L_25proc self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->i, state);
}
deactQ_L_25proc deactQ_L_25procD___deserialize__ (deactQ_L_25proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_L_25proc));
            self->$class = &deactQ_L_25procG_methods;
            return self;
        }
        self = $DNEW(deactQ_L_25proc, state);
    }
    self->self = $step_deserialize(state);
    self->i = $step_deserialize(state);
    return self;
}
deactQ_L_25proc deactQ_L_25procG_new(deactQ_main G_1, B_int G_2) {
    deactQ_L_25proc $tmp = acton_malloc(sizeof(struct deactQ_L_25proc));
    $tmp->$class = &deactQ_L_25procG_methods;
    deactQ_L_25procG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct deactQ_L_25procG_class deactQ_L_25procG_methods;
$R deactQ_L_26C_13cont ($Cont C_cont, deactQ_Apa G_act, B_NoneType C_14res) {
    return $R_CONT(C_cont, G_act);
}
B_NoneType deactQ_L_27ContD___init__ (deactQ_L_27Cont L_self, $Cont C_cont, deactQ_Apa G_act) {
    L_self->C_cont = C_cont;
    L_self->G_act = G_act;
    return B_None;
}
$R deactQ_L_27ContD___call__ (deactQ_L_27Cont L_self, B_NoneType G_1) {
    $Cont C_cont = L_self->C_cont;
    deactQ_Apa G_act = L_self->G_act;
    return deactQ_L_26C_13cont(C_cont, G_act, G_1);
}
void deactQ_L_27ContD___serialize__ (deactQ_L_27Cont self, $Serial$state state) {
    $step_serialize(self->C_cont, state);
    $step_serialize(self->G_act, state);
}
deactQ_L_27Cont deactQ_L_27ContD___deserialize__ (deactQ_L_27Cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_L_27Cont));
            self->$class = &deactQ_L_27ContG_methods;
            return self;
        }
        self = $DNEW(deactQ_L_27Cont, state);
    }
    self->C_cont = $step_deserialize(state);
    self->G_act = $step_deserialize(state);
    return self;
}
deactQ_L_27Cont deactQ_L_27ContG_new($Cont G_1, deactQ_Apa G_2) {
    deactQ_L_27Cont $tmp = acton_malloc(sizeof(struct deactQ_L_27Cont));
    $tmp->$class = &deactQ_L_27ContG_methods;
    deactQ_L_27ContG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct deactQ_L_27ContG_class deactQ_L_27ContG_methods;
B_NoneType deactQ_L_28procD___init__ (deactQ_L_28proc L_self, deactQ_Apa G_act) {
    L_self->G_act = G_act;
    return B_None;
}
$R deactQ_L_28procD___call__ (deactQ_L_28proc L_self, $Cont C_cont) {
    deactQ_Apa G_act = L_self->G_act;
    return (($R (*) (deactQ_Apa, $Cont))G_act->$class->__init__)(G_act, C_cont);
}
$R deactQ_L_28procD___exec__ (deactQ_L_28proc L_self, $Cont C_cont) {
    return (($R (*) (deactQ_L_28proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void deactQ_L_28procD___serialize__ (deactQ_L_28proc self, $Serial$state state) {
    $step_serialize(self->G_act, state);
}
deactQ_L_28proc deactQ_L_28procD___deserialize__ (deactQ_L_28proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_L_28proc));
            self->$class = &deactQ_L_28procG_methods;
            return self;
        }
        self = $DNEW(deactQ_L_28proc, state);
    }
    self->G_act = $step_deserialize(state);
    return self;
}
deactQ_L_28proc deactQ_L_28procG_new(deactQ_Apa G_1) {
    deactQ_L_28proc $tmp = acton_malloc(sizeof(struct deactQ_L_28proc));
    $tmp->$class = &deactQ_L_28procG_methods;
    deactQ_L_28procG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct deactQ_L_28procG_class deactQ_L_28procG_methods;
$R deactQ_L_29C_15cont ($Cont C_cont, deactQ_Bepa G_act, B_NoneType C_16res) {
    return $R_CONT(C_cont, G_act);
}
B_NoneType deactQ_L_30ContD___init__ (deactQ_L_30Cont L_self, $Cont C_cont, deactQ_Bepa G_act) {
    L_self->C_cont = C_cont;
    L_self->G_act = G_act;
    return B_None;
}
$R deactQ_L_30ContD___call__ (deactQ_L_30Cont L_self, B_NoneType G_1) {
    $Cont C_cont = L_self->C_cont;
    deactQ_Bepa G_act = L_self->G_act;
    return deactQ_L_29C_15cont(C_cont, G_act, G_1);
}
void deactQ_L_30ContD___serialize__ (deactQ_L_30Cont self, $Serial$state state) {
    $step_serialize(self->C_cont, state);
    $step_serialize(self->G_act, state);
}
deactQ_L_30Cont deactQ_L_30ContD___deserialize__ (deactQ_L_30Cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_L_30Cont));
            self->$class = &deactQ_L_30ContG_methods;
            return self;
        }
        self = $DNEW(deactQ_L_30Cont, state);
    }
    self->C_cont = $step_deserialize(state);
    self->G_act = $step_deserialize(state);
    return self;
}
deactQ_L_30Cont deactQ_L_30ContG_new($Cont G_1, deactQ_Bepa G_2) {
    deactQ_L_30Cont $tmp = acton_malloc(sizeof(struct deactQ_L_30Cont));
    $tmp->$class = &deactQ_L_30ContG_methods;
    deactQ_L_30ContG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct deactQ_L_30ContG_class deactQ_L_30ContG_methods;
B_NoneType deactQ_L_31procD___init__ (deactQ_L_31proc L_self, deactQ_Bepa G_act) {
    L_self->G_act = G_act;
    return B_None;
}
$R deactQ_L_31procD___call__ (deactQ_L_31proc L_self, $Cont C_cont) {
    deactQ_Bepa G_act = L_self->G_act;
    return (($R (*) (deactQ_Bepa, $Cont))G_act->$class->__init__)(G_act, C_cont);
}
$R deactQ_L_31procD___exec__ (deactQ_L_31proc L_self, $Cont C_cont) {
    return (($R (*) (deactQ_L_31proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void deactQ_L_31procD___serialize__ (deactQ_L_31proc self, $Serial$state state) {
    $step_serialize(self->G_act, state);
}
deactQ_L_31proc deactQ_L_31procD___deserialize__ (deactQ_L_31proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_L_31proc));
            self->$class = &deactQ_L_31procG_methods;
            return self;
        }
        self = $DNEW(deactQ_L_31proc, state);
    }
    self->G_act = $step_deserialize(state);
    return self;
}
deactQ_L_31proc deactQ_L_31procG_new(deactQ_Bepa G_1) {
    deactQ_L_31proc $tmp = acton_malloc(sizeof(struct deactQ_L_31proc));
    $tmp->$class = &deactQ_L_31procG_methods;
    deactQ_L_31procG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct deactQ_L_31procG_class deactQ_L_31procG_methods;
$R deactQ_L_32C_17cont ($Cont C_cont, deactQ_main G_act, B_NoneType C_18res) {
    return $R_CONT(C_cont, G_act);
}
B_NoneType deactQ_L_33ContD___init__ (deactQ_L_33Cont L_self, $Cont C_cont, deactQ_main G_act) {
    L_self->C_cont = C_cont;
    L_self->G_act = G_act;
    return B_None;
}
$R deactQ_L_33ContD___call__ (deactQ_L_33Cont L_self, B_NoneType G_1) {
    $Cont C_cont = L_self->C_cont;
    deactQ_main G_act = L_self->G_act;
    return deactQ_L_32C_17cont(C_cont, G_act, G_1);
}
void deactQ_L_33ContD___serialize__ (deactQ_L_33Cont self, $Serial$state state) {
    $step_serialize(self->C_cont, state);
    $step_serialize(self->G_act, state);
}
deactQ_L_33Cont deactQ_L_33ContD___deserialize__ (deactQ_L_33Cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_L_33Cont));
            self->$class = &deactQ_L_33ContG_methods;
            return self;
        }
        self = $DNEW(deactQ_L_33Cont, state);
    }
    self->C_cont = $step_deserialize(state);
    self->G_act = $step_deserialize(state);
    return self;
}
deactQ_L_33Cont deactQ_L_33ContG_new($Cont G_1, deactQ_main G_2) {
    deactQ_L_33Cont $tmp = acton_malloc(sizeof(struct deactQ_L_33Cont));
    $tmp->$class = &deactQ_L_33ContG_methods;
    deactQ_L_33ContG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct deactQ_L_33ContG_class deactQ_L_33ContG_methods;
B_NoneType deactQ_L_34procD___init__ (deactQ_L_34proc L_self, deactQ_main G_act, B_Env env) {
    L_self->G_act = G_act;
    L_self->env = env;
    return B_None;
}
$R deactQ_L_34procD___call__ (deactQ_L_34proc L_self, $Cont C_cont) {
    deactQ_main G_act = L_self->G_act;
    B_Env env = L_self->env;
    return (($R (*) (deactQ_main, $Cont, B_Env))G_act->$class->__init__)(G_act, C_cont, env);
}
$R deactQ_L_34procD___exec__ (deactQ_L_34proc L_self, $Cont C_cont) {
    return (($R (*) (deactQ_L_34proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void deactQ_L_34procD___serialize__ (deactQ_L_34proc self, $Serial$state state) {
    $step_serialize(self->G_act, state);
    $step_serialize(self->env, state);
}
deactQ_L_34proc deactQ_L_34procD___deserialize__ (deactQ_L_34proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct deactQ_L_34proc));
            self->$class = &deactQ_L_34procG_methods;
            return self;
        }
        self = $DNEW(deactQ_L_34proc, state);
    }
    self->G_act = $step_deserialize(state);
    self->env = $step_deserialize(state);
    return self;
}
deactQ_L_34proc deactQ_L_34procG_new(deactQ_main G_1, B_Env G_2) {
    deactQ_L_34proc $tmp = acton_malloc(sizeof(struct deactQ_L_34proc));
    $tmp->$class = &deactQ_L_34procG_methods;
    deactQ_L_34procG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct deactQ_L_34procG_class deactQ_L_34procG_methods;
$R deactQ_ApaD___init__ (deactQ_Apa self, $Cont C_cont) {
    return (($R (*) (deactQ_Apa, $Cont, $proc))self->$class->setupG_local)(self, (($Cont)deactQ_L_2ContG_new(C_cont)), (($proc)deactQ_L_4procG_new(self)));
}
#line 2 "test/src/deact.act"
$R deactQ_ApaD_setupG_local (deactQ_Apa self, $Cont C_cont, $proc cb) {
    #line 3 "test/src/deact.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("setup")), B_None, B_None, B_None, B_None);
<<<<<<< HEAD
    #line 4 "test/src/deact.act"
    ((B_Msg (*) ($action, B_int))cb->$class->__asyn__)(cb, toB_int(0LL));
    return $R_CONT(C_cont, B_None);
=======
    return (($R (*) ($proc, B_value, B_int))cb->$class->__exec__)(cb, ((B_value)deactQ_L_6ContG_new(C_cont)), to$int(0));
>>>>>>> ebe2c02f (Golden updates, mostly just new numbers but also some *internal* effects that become proc instead of action)
}
#line 5 "test/src/deact.act"
$R deactQ_ApaD_computeG_local (deactQ_Apa self, $Cont C_cont, $action cb) {
    #line 6 "test/src/deact.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("compute")), B_None, B_None, B_None, B_None);
<<<<<<< HEAD
    return $AWAIT((($Cont)deactQ_L_6ContG_new(cb, C_cont)), ((B_Msg)((B_Msg (*) ($action, B_int))cb->$class->__asyn__)(cb, toB_int(1LL))));
=======
    return $AWAIT((($Cont)deactQ_L_8ContG_new(cb, C_cont)), ((B_Msg)((B_Msg (*) ($action, B_int))cb->$class->__asyn__)(cb, to$int(1))));
>>>>>>> ebe2c02f (Golden updates, mostly just new numbers but also some *internal* effects that become proc instead of action)
}
#line 10 "test/src/deact.act"
$R deactQ_ApaD_noticeG_local (deactQ_Apa self, $Cont C_cont, B_int i) {
    #line 11 "test/src/deact.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("notice")), B_None, B_None, B_None, B_None);
<<<<<<< HEAD
    int64_t U_9N_1tmp = (((B_int)i)->val + 1LL);
    return $R_CONT(C_cont, toB_int(U_9N_1tmp));
=======
    B_int N_1tmp = ((B_int (*) (B_Plus, B_int, B_int))deactQ_W_313->$class->__add__)(deactQ_W_313, i, to$int(1));
    return $R_CONT(C_cont, N_1tmp);
>>>>>>> 31837f3b (Update golden files)
}
B_Msg deactQ_ApaD_setup (deactQ_Apa self, $proc cb) {
    return $ASYNC((($Actor)self), (($Cont)deactQ_L_9procG_new(self, cb)));
}
B_Msg deactQ_ApaD_compute (deactQ_Apa self, $action cb) {
    return ((B_Msg)$ASYNC((($Actor)self), (($Cont)deactQ_L_10procG_new(self, cb))));
}
B_Msg deactQ_ApaD_notice (deactQ_Apa self, B_int i) {
    return ((B_Msg)$ASYNC((($Actor)self), (($Cont)deactQ_L_11procG_new(self, i))));
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
void deactQ_ApaD_GCfinalizer (void *obj, void *cdata) {
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
<<<<<<< HEAD
    int64_t U_10N_2tmp = (((B_int)i)->val + 1LL);
    return $R_CONT(C_cont, toB_int(U_10N_2tmp));
=======
    B_int N_2tmp = ((B_int (*) (B_Plus, B_int, B_int))deactQ_W_313->$class->__add__)(deactQ_W_313, i, to$int(1));
    return $R_CONT(C_cont, N_2tmp);
>>>>>>> 31837f3b (Update golden files)
}
B_Msg deactQ_BepaD_callback (deactQ_Bepa self, B_int i) {
    return ((B_Msg)$ASYNC((($Actor)self), (($Cont)deactQ_L_12procG_new(self, i))));
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
void deactQ_BepaD_GCfinalizer (void *obj, void *cdata) {
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
    return deactQ_ApaG_newact((($Cont)deactQ_L_24ContG_new(self, C_cont)));
}
#line 24 "test/src/deact.act"
$R deactQ_mainD_myprocG_local (deactQ_main self, $Cont C_cont, B_int i) {
    #line 25 "test/src/deact.act"
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(2, to$str("myproc"), i), B_None, B_None, B_None, B_None);
    #line 26 "test/src/deact.act"
    if ((((B_int)i)->val == 2LL)) {
        #line 27 "test/src/deact.act"
        ((B_Msg (*) (B_Env, B_int))self->env->$class->exit)(self->env, toB_int(0LL));
    }
    return $R_CONT(C_cont, i);
}
B_Msg deactQ_mainD_myproc (deactQ_main self, B_int i) {
    return ((B_Msg)$ASYNC((($Actor)self), (($Cont)deactQ_L_25procG_new(self, i))));
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
void deactQ_mainD_GCfinalizer (void *obj, void *cdata) {
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
    if ((void*)G_act->$class->__cleanup__ != (void*)$ActorD___cleanup__) $InstallFinalizer(G_act, deactQ_ApaD_GCfinalizer);
    return $AWAIT((($Cont)deactQ_L_27ContG_new(C_cont, G_act)), $ASYNC((($Actor)G_act), (($Cont)deactQ_L_28procG_new(G_act))));
}
$R deactQ_BepaG_newact ($Cont C_cont) {
    deactQ_Bepa G_act = $NEWACTOR(deactQ_Bepa);
    if ((void*)G_act->$class->__cleanup__ != (void*)$ActorD___cleanup__) $InstallFinalizer(G_act, deactQ_BepaD_GCfinalizer);
    return $AWAIT((($Cont)deactQ_L_30ContG_new(C_cont, G_act)), $ASYNC((($Actor)G_act), (($Cont)deactQ_L_31procG_new(G_act))));
}
$R deactQ_mainG_newact ($Cont C_cont, B_Env env) {
    deactQ_main G_act = $NEWACTOR(deactQ_main);
    if ((void*)G_act->$class->__cleanup__ != (void*)$ActorD___cleanup__) $InstallFinalizer(G_act, deactQ_mainD_GCfinalizer);
    return $AWAIT((($Cont)deactQ_L_33ContG_new(C_cont, G_act)), $ASYNC((($Actor)G_act), (($Cont)deactQ_L_34procG_new(G_act, env))));
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
        deactQ_L_4procG_methods.$GCINFO = "deactQ_L_4proc";
        deactQ_L_4procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        deactQ_L_4procG_methods.__bool__ = (B_bool (*) (deactQ_L_4proc))B_valueG_methods.__bool__;
        deactQ_L_4procG_methods.__str__ = (B_str (*) (deactQ_L_4proc))B_valueG_methods.__str__;
        deactQ_L_4procG_methods.__repr__ = (B_str (*) (deactQ_L_4proc))B_valueG_methods.__repr__;
        deactQ_L_4procG_methods.__init__ = deactQ_L_4procD___init__;
        deactQ_L_4procG_methods.__call__ = deactQ_L_4procD___call__;
        deactQ_L_4procG_methods.__exec__ = deactQ_L_4procD___exec__;
        deactQ_L_4procG_methods.__serialize__ = deactQ_L_4procD___serialize__;
        deactQ_L_4procG_methods.__deserialize__ = deactQ_L_4procD___deserialize__;
        $register(&deactQ_L_4procG_methods);
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
        deactQ_L_8ContG_methods.$GCINFO = "deactQ_L_8Cont";
        deactQ_L_8ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        deactQ_L_8ContG_methods.__bool__ = (B_bool (*) (deactQ_L_8Cont))B_valueG_methods.__bool__;
        deactQ_L_8ContG_methods.__str__ = (B_str (*) (deactQ_L_8Cont))B_valueG_methods.__str__;
        deactQ_L_8ContG_methods.__repr__ = (B_str (*) (deactQ_L_8Cont))B_valueG_methods.__repr__;
        deactQ_L_8ContG_methods.__init__ = deactQ_L_8ContD___init__;
        deactQ_L_8ContG_methods.__call__ = deactQ_L_8ContD___call__;
        deactQ_L_8ContG_methods.__serialize__ = deactQ_L_8ContD___serialize__;
        deactQ_L_8ContG_methods.__deserialize__ = deactQ_L_8ContD___deserialize__;
        $register(&deactQ_L_8ContG_methods);
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
        deactQ_L_11procG_methods.$GCINFO = "deactQ_L_11proc";
        deactQ_L_11procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        deactQ_L_11procG_methods.__bool__ = (B_bool (*) (deactQ_L_11proc))B_valueG_methods.__bool__;
        deactQ_L_11procG_methods.__str__ = (B_str (*) (deactQ_L_11proc))B_valueG_methods.__str__;
        deactQ_L_11procG_methods.__repr__ = (B_str (*) (deactQ_L_11proc))B_valueG_methods.__repr__;
        deactQ_L_11procG_methods.__init__ = deactQ_L_11procD___init__;
        deactQ_L_11procG_methods.__call__ = deactQ_L_11procD___call__;
        deactQ_L_11procG_methods.__exec__ = deactQ_L_11procD___exec__;
        deactQ_L_11procG_methods.__serialize__ = deactQ_L_11procD___serialize__;
        deactQ_L_11procG_methods.__deserialize__ = deactQ_L_11procD___deserialize__;
        $register(&deactQ_L_11procG_methods);
    }
    {
        deactQ_L_12procG_methods.$GCINFO = "deactQ_L_12proc";
        deactQ_L_12procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        deactQ_L_12procG_methods.__bool__ = (B_bool (*) (deactQ_L_12proc))B_valueG_methods.__bool__;
        deactQ_L_12procG_methods.__str__ = (B_str (*) (deactQ_L_12proc))B_valueG_methods.__str__;
        deactQ_L_12procG_methods.__repr__ = (B_str (*) (deactQ_L_12proc))B_valueG_methods.__repr__;
        deactQ_L_12procG_methods.__init__ = deactQ_L_12procD___init__;
        deactQ_L_12procG_methods.__call__ = deactQ_L_12procD___call__;
        deactQ_L_12procG_methods.__exec__ = deactQ_L_12procD___exec__;
        deactQ_L_12procG_methods.__serialize__ = deactQ_L_12procD___serialize__;
        deactQ_L_12procG_methods.__deserialize__ = deactQ_L_12procD___deserialize__;
        $register(&deactQ_L_12procG_methods);
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
        deactQ_L_18actionG_methods.$GCINFO = "deactQ_L_18action";
        deactQ_L_18actionG_methods.$superclass = ($SuperG_class)&$actionG_methods;
        deactQ_L_18actionG_methods.__bool__ = (B_bool (*) (deactQ_L_18action))B_valueG_methods.__bool__;
        deactQ_L_18actionG_methods.__str__ = (B_str (*) (deactQ_L_18action))B_valueG_methods.__str__;
        deactQ_L_18actionG_methods.__repr__ = (B_str (*) (deactQ_L_18action))B_valueG_methods.__repr__;
        deactQ_L_18actionG_methods.__init__ = deactQ_L_18actionD___init__;
        deactQ_L_18actionG_methods.__call__ = deactQ_L_18actionD___call__;
        deactQ_L_18actionG_methods.__exec__ = deactQ_L_18actionD___exec__;
        deactQ_L_18actionG_methods.__asyn__ = deactQ_L_18actionD___asyn__;
        deactQ_L_18actionG_methods.__serialize__ = deactQ_L_18actionD___serialize__;
        deactQ_L_18actionG_methods.__deserialize__ = deactQ_L_18actionD___deserialize__;
        $register(&deactQ_L_18actionG_methods);
    }
    {
        deactQ_L_21actionG_methods.$GCINFO = "deactQ_L_21action";
        deactQ_L_21actionG_methods.$superclass = ($SuperG_class)&$actionG_methods;
        deactQ_L_21actionG_methods.__bool__ = (B_bool (*) (deactQ_L_21action))B_valueG_methods.__bool__;
        deactQ_L_21actionG_methods.__str__ = (B_str (*) (deactQ_L_21action))B_valueG_methods.__str__;
        deactQ_L_21actionG_methods.__repr__ = (B_str (*) (deactQ_L_21action))B_valueG_methods.__repr__;
        deactQ_L_21actionG_methods.__init__ = deactQ_L_21actionD___init__;
        deactQ_L_21actionG_methods.__call__ = deactQ_L_21actionD___call__;
        deactQ_L_21actionG_methods.__exec__ = deactQ_L_21actionD___exec__;
        deactQ_L_21actionG_methods.__asyn__ = deactQ_L_21actionD___asyn__;
        deactQ_L_21actionG_methods.__serialize__ = deactQ_L_21actionD___serialize__;
        deactQ_L_21actionG_methods.__deserialize__ = deactQ_L_21actionD___deserialize__;
        $register(&deactQ_L_21actionG_methods);
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
        deactQ_L_23ContG_methods.$GCINFO = "deactQ_L_23Cont";
        deactQ_L_23ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        deactQ_L_23ContG_methods.__bool__ = (B_bool (*) (deactQ_L_23Cont))B_valueG_methods.__bool__;
        deactQ_L_23ContG_methods.__str__ = (B_str (*) (deactQ_L_23Cont))B_valueG_methods.__str__;
        deactQ_L_23ContG_methods.__repr__ = (B_str (*) (deactQ_L_23Cont))B_valueG_methods.__repr__;
        deactQ_L_23ContG_methods.__init__ = deactQ_L_23ContD___init__;
        deactQ_L_23ContG_methods.__call__ = deactQ_L_23ContD___call__;
        deactQ_L_23ContG_methods.__serialize__ = deactQ_L_23ContD___serialize__;
        deactQ_L_23ContG_methods.__deserialize__ = deactQ_L_23ContD___deserialize__;
        $register(&deactQ_L_23ContG_methods);
    }
    {
        deactQ_L_24ContG_methods.$GCINFO = "deactQ_L_24Cont";
        deactQ_L_24ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        deactQ_L_24ContG_methods.__bool__ = (B_bool (*) (deactQ_L_24Cont))B_valueG_methods.__bool__;
        deactQ_L_24ContG_methods.__str__ = (B_str (*) (deactQ_L_24Cont))B_valueG_methods.__str__;
        deactQ_L_24ContG_methods.__repr__ = (B_str (*) (deactQ_L_24Cont))B_valueG_methods.__repr__;
        deactQ_L_24ContG_methods.__init__ = deactQ_L_24ContD___init__;
        deactQ_L_24ContG_methods.__call__ = deactQ_L_24ContD___call__;
        deactQ_L_24ContG_methods.__serialize__ = deactQ_L_24ContD___serialize__;
        deactQ_L_24ContG_methods.__deserialize__ = deactQ_L_24ContD___deserialize__;
        $register(&deactQ_L_24ContG_methods);
    }
    {
        deactQ_L_25procG_methods.$GCINFO = "deactQ_L_25proc";
        deactQ_L_25procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        deactQ_L_25procG_methods.__bool__ = (B_bool (*) (deactQ_L_25proc))B_valueG_methods.__bool__;
        deactQ_L_25procG_methods.__str__ = (B_str (*) (deactQ_L_25proc))B_valueG_methods.__str__;
        deactQ_L_25procG_methods.__repr__ = (B_str (*) (deactQ_L_25proc))B_valueG_methods.__repr__;
        deactQ_L_25procG_methods.__init__ = deactQ_L_25procD___init__;
        deactQ_L_25procG_methods.__call__ = deactQ_L_25procD___call__;
        deactQ_L_25procG_methods.__exec__ = deactQ_L_25procD___exec__;
        deactQ_L_25procG_methods.__serialize__ = deactQ_L_25procD___serialize__;
        deactQ_L_25procG_methods.__deserialize__ = deactQ_L_25procD___deserialize__;
        $register(&deactQ_L_25procG_methods);
    }
    {
        deactQ_L_27ContG_methods.$GCINFO = "deactQ_L_27Cont";
        deactQ_L_27ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        deactQ_L_27ContG_methods.__bool__ = (B_bool (*) (deactQ_L_27Cont))B_valueG_methods.__bool__;
        deactQ_L_27ContG_methods.__str__ = (B_str (*) (deactQ_L_27Cont))B_valueG_methods.__str__;
        deactQ_L_27ContG_methods.__repr__ = (B_str (*) (deactQ_L_27Cont))B_valueG_methods.__repr__;
        deactQ_L_27ContG_methods.__init__ = deactQ_L_27ContD___init__;
        deactQ_L_27ContG_methods.__call__ = deactQ_L_27ContD___call__;
        deactQ_L_27ContG_methods.__serialize__ = deactQ_L_27ContD___serialize__;
        deactQ_L_27ContG_methods.__deserialize__ = deactQ_L_27ContD___deserialize__;
        $register(&deactQ_L_27ContG_methods);
    }
    {
        deactQ_L_28procG_methods.$GCINFO = "deactQ_L_28proc";
        deactQ_L_28procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        deactQ_L_28procG_methods.__bool__ = (B_bool (*) (deactQ_L_28proc))B_valueG_methods.__bool__;
        deactQ_L_28procG_methods.__str__ = (B_str (*) (deactQ_L_28proc))B_valueG_methods.__str__;
        deactQ_L_28procG_methods.__repr__ = (B_str (*) (deactQ_L_28proc))B_valueG_methods.__repr__;
        deactQ_L_28procG_methods.__init__ = deactQ_L_28procD___init__;
        deactQ_L_28procG_methods.__call__ = deactQ_L_28procD___call__;
        deactQ_L_28procG_methods.__exec__ = deactQ_L_28procD___exec__;
        deactQ_L_28procG_methods.__serialize__ = deactQ_L_28procD___serialize__;
        deactQ_L_28procG_methods.__deserialize__ = deactQ_L_28procD___deserialize__;
        $register(&deactQ_L_28procG_methods);
    }
    {
        deactQ_L_30ContG_methods.$GCINFO = "deactQ_L_30Cont";
        deactQ_L_30ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        deactQ_L_30ContG_methods.__bool__ = (B_bool (*) (deactQ_L_30Cont))B_valueG_methods.__bool__;
        deactQ_L_30ContG_methods.__str__ = (B_str (*) (deactQ_L_30Cont))B_valueG_methods.__str__;
        deactQ_L_30ContG_methods.__repr__ = (B_str (*) (deactQ_L_30Cont))B_valueG_methods.__repr__;
        deactQ_L_30ContG_methods.__init__ = deactQ_L_30ContD___init__;
        deactQ_L_30ContG_methods.__call__ = deactQ_L_30ContD___call__;
        deactQ_L_30ContG_methods.__serialize__ = deactQ_L_30ContD___serialize__;
        deactQ_L_30ContG_methods.__deserialize__ = deactQ_L_30ContD___deserialize__;
        $register(&deactQ_L_30ContG_methods);
    }
    {
        deactQ_L_31procG_methods.$GCINFO = "deactQ_L_31proc";
        deactQ_L_31procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        deactQ_L_31procG_methods.__bool__ = (B_bool (*) (deactQ_L_31proc))B_valueG_methods.__bool__;
        deactQ_L_31procG_methods.__str__ = (B_str (*) (deactQ_L_31proc))B_valueG_methods.__str__;
        deactQ_L_31procG_methods.__repr__ = (B_str (*) (deactQ_L_31proc))B_valueG_methods.__repr__;
        deactQ_L_31procG_methods.__init__ = deactQ_L_31procD___init__;
        deactQ_L_31procG_methods.__call__ = deactQ_L_31procD___call__;
        deactQ_L_31procG_methods.__exec__ = deactQ_L_31procD___exec__;
        deactQ_L_31procG_methods.__serialize__ = deactQ_L_31procD___serialize__;
        deactQ_L_31procG_methods.__deserialize__ = deactQ_L_31procD___deserialize__;
        $register(&deactQ_L_31procG_methods);
    }
    {
        deactQ_L_33ContG_methods.$GCINFO = "deactQ_L_33Cont";
        deactQ_L_33ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        deactQ_L_33ContG_methods.__bool__ = (B_bool (*) (deactQ_L_33Cont))B_valueG_methods.__bool__;
        deactQ_L_33ContG_methods.__str__ = (B_str (*) (deactQ_L_33Cont))B_valueG_methods.__str__;
        deactQ_L_33ContG_methods.__repr__ = (B_str (*) (deactQ_L_33Cont))B_valueG_methods.__repr__;
        deactQ_L_33ContG_methods.__init__ = deactQ_L_33ContD___init__;
        deactQ_L_33ContG_methods.__call__ = deactQ_L_33ContD___call__;
        deactQ_L_33ContG_methods.__serialize__ = deactQ_L_33ContD___serialize__;
        deactQ_L_33ContG_methods.__deserialize__ = deactQ_L_33ContD___deserialize__;
        $register(&deactQ_L_33ContG_methods);
    }
    {
        deactQ_L_34procG_methods.$GCINFO = "deactQ_L_34proc";
        deactQ_L_34procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        deactQ_L_34procG_methods.__bool__ = (B_bool (*) (deactQ_L_34proc))B_valueG_methods.__bool__;
        deactQ_L_34procG_methods.__str__ = (B_str (*) (deactQ_L_34proc))B_valueG_methods.__str__;
        deactQ_L_34procG_methods.__repr__ = (B_str (*) (deactQ_L_34proc))B_valueG_methods.__repr__;
        deactQ_L_34procG_methods.__init__ = deactQ_L_34procD___init__;
        deactQ_L_34procG_methods.__call__ = deactQ_L_34procD___call__;
        deactQ_L_34procG_methods.__exec__ = deactQ_L_34procD___exec__;
        deactQ_L_34procG_methods.__serialize__ = deactQ_L_34procD___serialize__;
        deactQ_L_34procG_methods.__deserialize__ = deactQ_L_34procD___deserialize__;
        $register(&deactQ_L_34procG_methods);
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
<<<<<<< HEAD
=======
    B_Plus W_313 = (B_Plus)B_IntegralD_intG_witness;
    deactQ_W_313 = W_313;
    B_Times W_223 = (B_Times)B_IntegralD_intG_witness;
    deactQ_W_223 = W_223;
    B_Eq W_761 = (B_Eq)B_OrdD_intG_witness;
    deactQ_W_761 = W_761;
>>>>>>> 31837f3b (Update golden files)
}