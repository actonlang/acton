/* Acton impl hash: test-hash */
#include "rts/common.h"
#include "out/types/init_call_barrier.h"
#line 8 "test/src/init_call_barrier.act"
$R init_call_barrierQ_step ($Cont C_cont) {
    #line 9 "test/src/init_call_barrier.act"
    return $R_CONT(C_cont, B_None);
}
$R init_call_barrierQ_L_1C_1cont (init_call_barrierQ_Later self, $Cont C_cont, B_NoneType C_2res) {
    #line 22 "test/src/init_call_barrier.act"
    ((B_NoneType (*) (init_call_barrierQ_Base))init_call_barrierQ_BaseG_methods.__init__)(((init_call_barrierQ_Base)self));
    return $R_CONT(C_cont, B_None);
}
$R init_call_barrierQ_L_2ContD___call__($Cont $closure, $WORD $result) {
    init_call_barrierQ_L_2Cont L_self = (init_call_barrierQ_L_2Cont)$closure;
    B_NoneType G_1 = (B_NoneType)$result;
    init_call_barrierQ_Later self = (init_call_barrierQ_Later)(((init_call_barrierQ_L_2Cont)(L_self))->ref0);
    $Cont C_cont = ($Cont)(((init_call_barrierQ_L_2Cont)(L_self))->ref1);
    return init_call_barrierQ_L_1C_1cont(self, C_cont, G_1);
}
$Cont init_call_barrierQ_L_2ContD___deserialize__($Cont self, $Serial$state state) {
    return $cont_restore(&init_call_barrierQ_L_2ContG_methods, self, state);
}
struct $ContClass init_call_barrierQ_L_2ContG_methods;
B_value init_call_barrierQ_BaseD___get_attr__ (init_call_barrierQ_Base self, B_str name) {
    if (name->nbytes == sizeof("items") - 1 && memcmp(name->str, "items", sizeof("items") - 1) == 0) {
        return (B_value)(self->items);
    }
    return B_None;
}
B_NoneType init_call_barrierQ_BaseG_init (init_call_barrierQ_Base self) {
    return B_None;
}
#line 12 "test/src/init_call_barrier.act"
B_NoneType init_call_barrierQ_BaseD___init__ (init_call_barrierQ_Base self) {
    #line 13 "test/src/init_call_barrier.act"
    ((init_call_barrierQ_Base)(self))->items = B_mk_list(1, toB_int(1LL));
    return B_None;
}
void init_call_barrierQ_BaseD___serialize__ (init_call_barrierQ_Base self, $Serial$state state) {
    $step_serialize(self->items, state);
}
init_call_barrierQ_Base init_call_barrierQ_BaseD___deserialize__ (init_call_barrierQ_Base self, $Serial$state state) {
    $WORD $tmp;
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct init_call_barrierQ_Base));
            self->$class = &init_call_barrierQ_BaseG_methods;
            return self;
        }
        self = $DNEW(init_call_barrierQ_Base, state);
    }
    ACTON_STORE_ATTR(self, items, $step_deserialize(state));
    return self;
}
init_call_barrierQ_Base init_call_barrierQ_BaseG_new() {
    init_call_barrierQ_Base $tmp = acton_malloc(sizeof(struct init_call_barrierQ_Base));
    $tmp->$class = &init_call_barrierQ_BaseG_methods;
    init_call_barrierQ_BaseG_methods.G_init($tmp);
    init_call_barrierQ_BaseG_methods.__init__($tmp);
    return $tmp;
}
struct init_call_barrierQ_BaseG_class init_call_barrierQ_BaseG_methods;
B_value init_call_barrierQ_DerivedD___get_attr__ (init_call_barrierQ_Derived self, B_str name) {
    if (name->nbytes == sizeof("items") - 1 && memcmp(name->str, "items", sizeof("items") - 1) == 0) {
        return (B_value)(self->items);
    }
    return B_None;
}
B_NoneType init_call_barrierQ_DerivedG_init (init_call_barrierQ_Derived self) {
    ((B_NoneType (*) (init_call_barrierQ_Base))init_call_barrierQ_BaseG_methods.G_init)(((init_call_barrierQ_Base)self));
    return B_None;
}
#line 16 "test/src/init_call_barrier.act"
B_NoneType init_call_barrierQ_DerivedD___init__ (init_call_barrierQ_Derived self) {
    #line 17 "test/src/init_call_barrier.act"
    ((B_NoneType (*) (init_call_barrierQ_Base))init_call_barrierQ_BaseG_methods.__init__)(((init_call_barrierQ_Base)self));
    return B_None;
}
void init_call_barrierQ_DerivedD___serialize__ (init_call_barrierQ_Derived self, $Serial$state state) {
    $step_serialize(self->items, state);
}
init_call_barrierQ_Derived init_call_barrierQ_DerivedD___deserialize__ (init_call_barrierQ_Derived self, $Serial$state state) {
    $WORD $tmp;
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct init_call_barrierQ_Derived));
            self->$class = &init_call_barrierQ_DerivedG_methods;
            return self;
        }
        self = $DNEW(init_call_barrierQ_Derived, state);
    }
    ACTON_STORE_ATTR(self, items, $step_deserialize(state));
    return self;
}
init_call_barrierQ_Derived init_call_barrierQ_DerivedG_new() {
    init_call_barrierQ_Derived $tmp = acton_malloc(sizeof(struct init_call_barrierQ_Derived));
    $tmp->$class = &init_call_barrierQ_DerivedG_methods;
    init_call_barrierQ_DerivedG_methods.G_init($tmp);
    init_call_barrierQ_DerivedG_methods.__init__($tmp);
    return $tmp;
}
struct init_call_barrierQ_DerivedG_class init_call_barrierQ_DerivedG_methods;
B_value init_call_barrierQ_LaterD___get_attr__ (init_call_barrierQ_Later self, B_str name) {
    if (name->nbytes == sizeof("items") - 1 && memcmp(name->str, "items", sizeof("items") - 1) == 0) {
        return (B_value)(self->items);
    }
    return B_None;
}
B_NoneType init_call_barrierQ_LaterG_init (init_call_barrierQ_Later self) {
    ((B_NoneType (*) (init_call_barrierQ_Base))init_call_barrierQ_BaseG_methods.G_init)(((init_call_barrierQ_Base)self));
    return B_None;
}
#line 20 "test/src/init_call_barrier.act"
$R init_call_barrierQ_LaterD___init__ (init_call_barrierQ_Later self, $Cont C_cont) {
    return init_call_barrierQ_step($SKIPRES((($Cont)((init_call_barrierQ_L_2Cont)$Cont_R2_new(&init_call_barrierQ_L_2ContG_methods, self, C_cont)))));
}
void init_call_barrierQ_LaterD___serialize__ (init_call_barrierQ_Later self, $Serial$state state) {
    $step_serialize(self->items, state);
}
init_call_barrierQ_Later init_call_barrierQ_LaterD___deserialize__ (init_call_barrierQ_Later self, $Serial$state state) {
    $WORD $tmp;
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct init_call_barrierQ_Later));
            self->$class = &init_call_barrierQ_LaterG_methods;
            return self;
        }
        self = $DNEW(init_call_barrierQ_Later, state);
    }
    ACTON_STORE_ATTR(self, items, $step_deserialize(state));
    return self;
}
$R init_call_barrierQ_LaterG_new($Cont G_1) {
    init_call_barrierQ_Later $tmp = acton_malloc(sizeof(struct init_call_barrierQ_Later));
    $tmp->$class = &init_call_barrierQ_LaterG_methods;
    init_call_barrierQ_LaterG_methods.G_init($tmp);
    return init_call_barrierQ_LaterG_methods.__init__($tmp, $CONSTCONT($tmp, G_1));
}
struct init_call_barrierQ_LaterG_class init_call_barrierQ_LaterG_methods;
B_value init_call_barrierQ_RenewD___get_attr__ (init_call_barrierQ_Renew self, B_str name) {
    if (name->nbytes == sizeof("items") - 1 && memcmp(name->str, "items", sizeof("items") - 1) == 0) {
        return (B_value)(self->items);
    }
    return B_None;
}
B_NoneType init_call_barrierQ_RenewG_init (init_call_barrierQ_Renew self) {
    return B_None;
}
#line 27 "test/src/init_call_barrier.act"
B_NoneType init_call_barrierQ_RenewD___init__ (init_call_barrierQ_Base b) {
    #line 29 "test/src/init_call_barrier.act"
    ((init_call_barrierQ_Base)(b))->items = B_mk_list(1, toB_int(3LL));
    return B_None;
}
void init_call_barrierQ_RenewD___serialize__ (init_call_barrierQ_Renew self, $Serial$state state) {
    $step_serialize(self->items, state);
}
init_call_barrierQ_Renew init_call_barrierQ_RenewD___deserialize__ (init_call_barrierQ_Renew self, $Serial$state state) {
    $WORD $tmp;
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct init_call_barrierQ_Renew));
            self->$class = &init_call_barrierQ_RenewG_methods;
            return self;
        }
        self = $DNEW(init_call_barrierQ_Renew, state);
    }
    ACTON_STORE_ATTR(self, items, $step_deserialize(state));
    return self;
}
struct init_call_barrierQ_RenewG_class init_call_barrierQ_RenewG_methods;
#line 31 "test/src/init_call_barrier.act"
B_NoneType init_call_barrierQ_reinit (init_call_barrierQ_Base b) {
    #line 32 "test/src/init_call_barrier.act"
    ((B_NoneType (*) (init_call_barrierQ_Base))init_call_barrierQ_BaseG_methods.__init__)(b);
    return B_None;
}
#line 34 "test/src/init_call_barrier.act"
B_NoneType init_call_barrierQ_renew (init_call_barrierQ_Base b) {
    #line 35 "test/src/init_call_barrier.act"
    ((B_NoneType (*) (init_call_barrierQ_Base))init_call_barrierQ_RenewG_methods.__init__)(b);
    return B_None;
}
int init_call_barrierQ_done$ = 0;
void init_call_barrierQ___init__ () {
    if (init_call_barrierQ_done$) return;
    init_call_barrierQ_done$ = 1;
    $cont_class_init(&init_call_barrierQ_L_2ContG_methods, "init_call_barrierQ_L_2Cont", &$Cont_R2_shape, init_call_barrierQ_L_2ContD___call__, init_call_barrierQ_L_2ContD___deserialize__);
    {
        init_call_barrierQ_BaseG_methods.$GCINFO = "init_call_barrierQ_Base";
        init_call_barrierQ_BaseG_methods.$superclass = ($SuperG_class)&B_objectG_methods;
        init_call_barrierQ_BaseG_methods.__bool__ = (bool (*) (init_call_barrierQ_Base))B_valueG_methods.__bool__;
        init_call_barrierQ_BaseG_methods.__str__ = (B_str (*) (init_call_barrierQ_Base))B_valueG_methods.__str__;
        init_call_barrierQ_BaseG_methods.__repr__ = (B_str (*) (init_call_barrierQ_Base))B_valueG_methods.__repr__;
        init_call_barrierQ_BaseG_methods.__get_attr__ = (B_value (*) (init_call_barrierQ_Base, B_str))init_call_barrierQ_BaseD___get_attr__;
        init_call_barrierQ_BaseG_methods.G_init = (B_NoneType (*) (init_call_barrierQ_Base))init_call_barrierQ_BaseG_init;
        init_call_barrierQ_BaseG_methods.__init__ = (B_NoneType (*) (init_call_barrierQ_Base))init_call_barrierQ_BaseD___init__;
        init_call_barrierQ_BaseG_methods.__serialize__ = init_call_barrierQ_BaseD___serialize__;
        init_call_barrierQ_BaseG_methods.__deserialize__ = init_call_barrierQ_BaseD___deserialize__;
        $register(&init_call_barrierQ_BaseG_methods);
    }
    {
        init_call_barrierQ_DerivedG_methods.$GCINFO = "init_call_barrierQ_Derived";
        init_call_barrierQ_DerivedG_methods.$superclass = ($SuperG_class)&init_call_barrierQ_BaseG_methods;
        init_call_barrierQ_DerivedG_methods.__bool__ = (bool (*) (init_call_barrierQ_Derived))B_valueG_methods.__bool__;
        init_call_barrierQ_DerivedG_methods.__str__ = (B_str (*) (init_call_barrierQ_Derived))B_valueG_methods.__str__;
        init_call_barrierQ_DerivedG_methods.__repr__ = (B_str (*) (init_call_barrierQ_Derived))B_valueG_methods.__repr__;
        init_call_barrierQ_DerivedG_methods.__get_attr__ = (B_value (*) (init_call_barrierQ_Derived, B_str))init_call_barrierQ_DerivedD___get_attr__;
        init_call_barrierQ_DerivedG_methods.G_init = (B_NoneType (*) (init_call_barrierQ_Derived))init_call_barrierQ_DerivedG_init;
        init_call_barrierQ_DerivedG_methods.__init__ = (B_NoneType (*) (init_call_barrierQ_Derived))init_call_barrierQ_DerivedD___init__;
        init_call_barrierQ_DerivedG_methods.__serialize__ = init_call_barrierQ_DerivedD___serialize__;
        init_call_barrierQ_DerivedG_methods.__deserialize__ = init_call_barrierQ_DerivedD___deserialize__;
        $register(&init_call_barrierQ_DerivedG_methods);
    }
    {
        init_call_barrierQ_LaterG_methods.$GCINFO = "init_call_barrierQ_Later";
        init_call_barrierQ_LaterG_methods.$superclass = ($SuperG_class)&init_call_barrierQ_BaseG_methods;
        init_call_barrierQ_LaterG_methods.__bool__ = (bool (*) (init_call_barrierQ_Later))B_valueG_methods.__bool__;
        init_call_barrierQ_LaterG_methods.__str__ = (B_str (*) (init_call_barrierQ_Later))B_valueG_methods.__str__;
        init_call_barrierQ_LaterG_methods.__repr__ = (B_str (*) (init_call_barrierQ_Later))B_valueG_methods.__repr__;
        init_call_barrierQ_LaterG_methods.__get_attr__ = (B_value (*) (init_call_barrierQ_Later, B_str))init_call_barrierQ_LaterD___get_attr__;
        init_call_barrierQ_LaterG_methods.G_init = (B_NoneType (*) (init_call_barrierQ_Later))init_call_barrierQ_LaterG_init;
        init_call_barrierQ_LaterG_methods.__init__ = ($R (*) (init_call_barrierQ_Later, $Cont))init_call_barrierQ_LaterD___init__;
        init_call_barrierQ_LaterG_methods.__serialize__ = init_call_barrierQ_LaterD___serialize__;
        init_call_barrierQ_LaterG_methods.__deserialize__ = init_call_barrierQ_LaterD___deserialize__;
        $register(&init_call_barrierQ_LaterG_methods);
    }
    {
        init_call_barrierQ_RenewG_methods.$GCINFO = "init_call_barrierQ_Renew";
        init_call_barrierQ_RenewG_methods.$superclass = ($SuperG_class)&B_objectG_methods;
        init_call_barrierQ_RenewG_methods.__bool__ = (bool (*) (init_call_barrierQ_Renew))B_valueG_methods.__bool__;
        init_call_barrierQ_RenewG_methods.__str__ = (B_str (*) (init_call_barrierQ_Renew))B_valueG_methods.__str__;
        init_call_barrierQ_RenewG_methods.__repr__ = (B_str (*) (init_call_barrierQ_Renew))B_valueG_methods.__repr__;
        init_call_barrierQ_RenewG_methods.__get_attr__ = (B_value (*) (init_call_barrierQ_Renew, B_str))init_call_barrierQ_RenewD___get_attr__;
        init_call_barrierQ_RenewG_methods.G_init = (B_NoneType (*) (init_call_barrierQ_Renew))init_call_barrierQ_RenewG_init;
        init_call_barrierQ_RenewG_methods.__init__ = (B_NoneType (*) (init_call_barrierQ_Base))init_call_barrierQ_RenewD___init__;
        init_call_barrierQ_RenewG_methods.__serialize__ = init_call_barrierQ_RenewD___serialize__;
        init_call_barrierQ_RenewG_methods.__deserialize__ = init_call_barrierQ_RenewD___deserialize__;
        $register(&init_call_barrierQ_RenewG_methods);
    }
}