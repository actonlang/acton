#include "rts/common.h"
#include "out/types/protofoo.h"
B_Eq protofooQ_W_14;
B_Hashable protofooQ_W_194;
$R protofooQ_L_1C_1cont ($Cont C_cont, protofooQ_main G_act, B_NoneType C_2res) {
    return $R_CONT(C_cont, G_act);
}
B_NoneType protofooQ_L_2ContD___init__ (protofooQ_L_2Cont L_self, $Cont C_cont, protofooQ_main G_act) {
    L_self->C_cont = C_cont;
    L_self->G_act = G_act;
    return B_None;
}
$R protofooQ_L_2ContD___call__ (protofooQ_L_2Cont L_self, B_NoneType G_1) {
    $Cont C_cont = L_self->C_cont;
    protofooQ_main G_act = L_self->G_act;
    return protofooQ_L_1C_1cont(C_cont, G_act, G_1);
}
void protofooQ_L_2ContD___serialize__ (protofooQ_L_2Cont self, $Serial$state state) {
    $step_serialize(self->C_cont, state);
    $step_serialize(self->G_act, state);
}
protofooQ_L_2Cont protofooQ_L_2ContD___deserialize__ (protofooQ_L_2Cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct protofooQ_L_2Cont));
            self->$class = &protofooQ_L_2ContG_methods;
            return self;
        }
        self = $DNEW(protofooQ_L_2Cont, state);
    }
    self->C_cont = $step_deserialize(state);
    self->G_act = $step_deserialize(state);
    return self;
}
protofooQ_L_2Cont protofooQ_L_2ContG_new($Cont G_1, protofooQ_main G_2) {
    protofooQ_L_2Cont $tmp = acton_malloc(sizeof(struct protofooQ_L_2Cont));
    $tmp->$class = &protofooQ_L_2ContG_methods;
    protofooQ_L_2ContG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct protofooQ_L_2ContG_class protofooQ_L_2ContG_methods;
B_NoneType protofooQ_L_3procD___init__ (protofooQ_L_3proc L_self, protofooQ_main G_act, B_Env env) {
    L_self->G_act = G_act;
    L_self->env = env;
    return B_None;
}
$R protofooQ_L_3procD___call__ (protofooQ_L_3proc L_self, $Cont C_cont) {
    protofooQ_main G_act = L_self->G_act;
    B_Env env = L_self->env;
    return (($R (*) (protofooQ_main, $Cont, B_Env))G_act->$class->__init__)(G_act, C_cont, env);
}
$R protofooQ_L_3procD___exec__ (protofooQ_L_3proc L_self, $Cont C_cont) {
    return (($R (*) (protofooQ_L_3proc, $Cont))L_self->$class->__call__)(L_self, C_cont);
}
void protofooQ_L_3procD___serialize__ (protofooQ_L_3proc self, $Serial$state state) {
    $step_serialize(self->G_act, state);
    $step_serialize(self->env, state);
}
protofooQ_L_3proc protofooQ_L_3procD___deserialize__ (protofooQ_L_3proc self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct protofooQ_L_3proc));
            self->$class = &protofooQ_L_3procG_methods;
            return self;
        }
        self = $DNEW(protofooQ_L_3proc, state);
    }
    self->G_act = $step_deserialize(state);
    self->env = $step_deserialize(state);
    return self;
}
protofooQ_L_3proc protofooQ_L_3procG_new(protofooQ_main G_1, B_Env G_2) {
    protofooQ_L_3proc $tmp = acton_malloc(sizeof(struct protofooQ_L_3proc));
    $tmp->$class = &protofooQ_L_3procG_methods;
    protofooQ_L_3procG_methods.__init__($tmp, G_1, G_2);
    return $tmp;
}
struct protofooQ_L_3procG_class protofooQ_L_3procG_methods;
B_NoneType protofooQ_KeyD___init__ (protofooQ_Key self, B_int x) {
    self->x = x;
    return B_None;
}
void protofooQ_KeyD___serialize__ (protofooQ_Key self, $Serial$state state) {
    $step_serialize(self->x, state);
}
protofooQ_Key protofooQ_KeyD___deserialize__ (protofooQ_Key self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct protofooQ_Key));
            self->$class = &protofooQ_KeyG_methods;
            return self;
        }
        self = $DNEW(protofooQ_Key, state);
    }
    self->x = $step_deserialize(state);
    return self;
}
protofooQ_Key protofooQ_KeyG_new(B_int G_1) {
    protofooQ_Key $tmp = acton_malloc(sizeof(struct protofooQ_Key));
    $tmp->$class = &protofooQ_KeyG_methods;
    protofooQ_KeyG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct protofooQ_KeyG_class protofooQ_KeyG_methods;
B_NoneType protofooQ_EqD_KeyD___init__ (protofooQ_EqD_Key W_self) {
    ((B_NoneType (*) (B_Eq))B_EqG_methods.__init__)(((B_Eq)W_self));
    return B_None;
}
B_bool protofooQ_EqD_KeyD___eq__ (protofooQ_EqD_Key W_self, protofooQ_Key a, protofooQ_Key b) {
    B_bool N_tmp = ((B_bool (*) (B_Eq, B_int, B_int))protofooQ_W_14->$class->__eq__)(protofooQ_W_14, a->x, b->x);
    return N_tmp;
}
void protofooQ_EqD_KeyD___serialize__ (protofooQ_EqD_Key self, $Serial$state state) {
}
protofooQ_EqD_Key protofooQ_EqD_KeyD___deserialize__ (protofooQ_EqD_Key self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct protofooQ_EqD_Key));
            self->$class = &protofooQ_EqD_KeyG_methods;
            return self;
        }
        self = $DNEW(protofooQ_EqD_Key, state);
    }
    return self;
}
protofooQ_EqD_Key protofooQ_EqD_KeyG_new() {
    protofooQ_EqD_Key $tmp = acton_malloc(sizeof(struct protofooQ_EqD_Key));
    $tmp->$class = &protofooQ_EqD_KeyG_methods;
    protofooQ_EqD_KeyG_methods.__init__($tmp);
    return $tmp;
}
struct protofooQ_EqD_KeyG_class protofooQ_EqD_KeyG_methods;
B_NoneType protofooQ_HashableD_KeyD___init__ (protofooQ_HashableD_Key W_self) {
    ((B_NoneType (*) (B_Hashable))B_HashableG_methods.__init__)(((B_Hashable)W_self));
    return B_None;
}
B_NoneType protofooQ_HashableD_KeyD_hash (protofooQ_HashableD_Key W_self, protofooQ_Key self, B_hasher hasher) {
    ((B_NoneType (*) (B_Hashable, B_int, B_hasher))protofooQ_W_194->$class->hash)(protofooQ_W_194, self->x, hasher);
    return B_None;
}
void protofooQ_HashableD_KeyD___serialize__ (protofooQ_HashableD_Key self, $Serial$state state) {
}
protofooQ_HashableD_Key protofooQ_HashableD_KeyD___deserialize__ (protofooQ_HashableD_Key self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct protofooQ_HashableD_Key));
            self->$class = &protofooQ_HashableD_KeyG_methods;
            return self;
        }
        self = $DNEW(protofooQ_HashableD_Key, state);
    }
    return self;
}
protofooQ_HashableD_Key protofooQ_HashableD_KeyG_new() {
    protofooQ_HashableD_Key $tmp = acton_malloc(sizeof(struct protofooQ_HashableD_Key));
    $tmp->$class = &protofooQ_HashableD_KeyG_methods;
    protofooQ_HashableD_KeyG_methods.__init__($tmp);
    return $tmp;
}
struct protofooQ_HashableD_KeyG_class protofooQ_HashableD_KeyG_methods;
$R protofooQ_mainD___init__ (protofooQ_main self, $Cont C_cont, B_Env env) {
    B_Hashable W_117 = ((B_Hashable)protofooQ_HashableD_KeyG_new());
    self->k = ((protofooQ_Key)protofooQ_KeyG_new(to$int(42)));
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, ((B_u64 (*) (B_Hashable, protofooQ_Key))B_hash)(W_117, self->k)), B_None, B_None, B_None, B_None);
    ((B_Msg (*) (B_Env, B_int))env->$class->exit)(env, to$int(0));
    return $R_CONT(C_cont, B_None);
}
void protofooQ_mainD___serialize__ (protofooQ_main self, $Serial$state state) {
    $ActorG_methods.__serialize__(($Actor)self, state);
    $step_serialize(self->k, state);
}
protofooQ_main protofooQ_mainD___deserialize__ (protofooQ_main self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct protofooQ_main));
            self->$class = &protofooQ_mainG_methods;
            return self;
        }
        self = $DNEW(protofooQ_main, state);
    }
    $ActorG_methods.__deserialize__(($Actor)self, state);
    self->k = $step_deserialize(state);
    return self;
}
void protofooQ_mainD__GC_finalizer (void *obj, void *cdata) {
    protofooQ_main self = (protofooQ_main)obj;
    self->$class->__cleanup__(self);
}
$R protofooQ_mainG_new($Cont G_1, B_Env G_2) {
    protofooQ_main $tmp = acton_malloc(sizeof(struct protofooQ_main));
    $tmp->$class = &protofooQ_mainG_methods;
    return protofooQ_mainG_methods.__init__($tmp, $CONSTCONT($tmp, G_1), G_2);
}
struct protofooQ_mainG_class protofooQ_mainG_methods;
$R protofooQ_mainG_newact ($Cont C_cont, B_Env env) {
    protofooQ_main G_act = $NEWACTOR(protofooQ_main);
    if ((void*)G_act->$class->__cleanup__ != (void*)$ActorD___cleanup__) $GCfinalizer(G_act, protofooQ_mainD__GC_finalizer);
    return $AWAIT((($Cont)protofooQ_L_2ContG_new(C_cont, G_act)), $ASYNC((($Actor)G_act), (($Cont)protofooQ_L_3procG_new(G_act, env))));
}
int protofooQ_done$ = 0;
void protofooQ___init__ () {
    if (protofooQ_done$) return;
    protofooQ_done$ = 1;
    B_Eq W_14 = (B_Eq)B_OrdD_intG_witness;
    protofooQ_W_14 = W_14;
    B_Hashable W_194 = (B_Hashable)B_HashableD_intG_witness;
    protofooQ_W_194 = W_194;
    {
        protofooQ_L_2ContG_methods.$GCINFO = "protofooQ_L_2Cont";
        protofooQ_L_2ContG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        protofooQ_L_2ContG_methods.__bool__ = (B_bool (*) (protofooQ_L_2Cont))B_valueG_methods.__bool__;
        protofooQ_L_2ContG_methods.__str__ = (B_str (*) (protofooQ_L_2Cont))B_valueG_methods.__str__;
        protofooQ_L_2ContG_methods.__repr__ = (B_str (*) (protofooQ_L_2Cont))B_valueG_methods.__repr__;
        protofooQ_L_2ContG_methods.__init__ = protofooQ_L_2ContD___init__;
        protofooQ_L_2ContG_methods.__call__ = protofooQ_L_2ContD___call__;
        protofooQ_L_2ContG_methods.__serialize__ = protofooQ_L_2ContD___serialize__;
        protofooQ_L_2ContG_methods.__deserialize__ = protofooQ_L_2ContD___deserialize__;
        $register(&protofooQ_L_2ContG_methods);
    }
    {
        protofooQ_L_3procG_methods.$GCINFO = "protofooQ_L_3proc";
        protofooQ_L_3procG_methods.$superclass = ($SuperG_class)&$procG_methods;
        protofooQ_L_3procG_methods.__bool__ = (B_bool (*) (protofooQ_L_3proc))B_valueG_methods.__bool__;
        protofooQ_L_3procG_methods.__str__ = (B_str (*) (protofooQ_L_3proc))B_valueG_methods.__str__;
        protofooQ_L_3procG_methods.__repr__ = (B_str (*) (protofooQ_L_3proc))B_valueG_methods.__repr__;
        protofooQ_L_3procG_methods.__init__ = protofooQ_L_3procD___init__;
        protofooQ_L_3procG_methods.__call__ = protofooQ_L_3procD___call__;
        protofooQ_L_3procG_methods.__exec__ = protofooQ_L_3procD___exec__;
        protofooQ_L_3procG_methods.__serialize__ = protofooQ_L_3procD___serialize__;
        protofooQ_L_3procG_methods.__deserialize__ = protofooQ_L_3procD___deserialize__;
        $register(&protofooQ_L_3procG_methods);
    }
    {
        protofooQ_KeyG_methods.$GCINFO = "protofooQ_Key";
        protofooQ_KeyG_methods.$superclass = ($SuperG_class)&B_valueG_methods;
        protofooQ_KeyG_methods.__bool__ = (B_bool (*) (protofooQ_Key))B_valueG_methods.__bool__;
        protofooQ_KeyG_methods.__str__ = (B_str (*) (protofooQ_Key))B_valueG_methods.__str__;
        protofooQ_KeyG_methods.__repr__ = (B_str (*) (protofooQ_Key))B_valueG_methods.__repr__;
        protofooQ_KeyG_methods.__init__ = protofooQ_KeyD___init__;
        protofooQ_KeyG_methods.__serialize__ = protofooQ_KeyD___serialize__;
        protofooQ_KeyG_methods.__deserialize__ = protofooQ_KeyD___deserialize__;
        $register(&protofooQ_KeyG_methods);
    }
    {
        protofooQ_EqD_KeyG_methods.$GCINFO = "protofooQ_EqD_Key";
        protofooQ_EqD_KeyG_methods.$superclass = ($SuperG_class)&B_EqG_methods;
        protofooQ_EqD_KeyG_methods.__bool__ = (B_bool (*) (protofooQ_EqD_Key))B_valueG_methods.__bool__;
        protofooQ_EqD_KeyG_methods.__str__ = (B_str (*) (protofooQ_EqD_Key))B_valueG_methods.__str__;
        protofooQ_EqD_KeyG_methods.__repr__ = (B_str (*) (protofooQ_EqD_Key))B_valueG_methods.__repr__;
        protofooQ_EqD_KeyG_methods.__ne__ = (B_bool (*) (protofooQ_EqD_Key, protofooQ_Key, protofooQ_Key))B_EqG_methods.__ne__;
        protofooQ_EqD_KeyG_methods.__init__ = protofooQ_EqD_KeyD___init__;
        protofooQ_EqD_KeyG_methods.__eq__ = protofooQ_EqD_KeyD___eq__;
        protofooQ_EqD_KeyG_methods.__serialize__ = protofooQ_EqD_KeyD___serialize__;
        protofooQ_EqD_KeyG_methods.__deserialize__ = protofooQ_EqD_KeyD___deserialize__;
        $register(&protofooQ_EqD_KeyG_methods);
    }
    {
        protofooQ_HashableD_KeyG_methods.$GCINFO = "protofooQ_HashableD_Key";
        protofooQ_HashableD_KeyG_methods.$superclass = ($SuperG_class)&B_HashableG_methods;
        protofooQ_HashableD_KeyG_methods.__bool__ = (B_bool (*) (protofooQ_HashableD_Key))B_valueG_methods.__bool__;
        protofooQ_HashableD_KeyG_methods.__str__ = (B_str (*) (protofooQ_HashableD_Key))B_valueG_methods.__str__;
        protofooQ_HashableD_KeyG_methods.__repr__ = (B_str (*) (protofooQ_HashableD_Key))B_valueG_methods.__repr__;
        protofooQ_HashableD_KeyG_methods.__ne__ = (B_bool (*) (protofooQ_HashableD_Key, protofooQ_Key, protofooQ_Key))B_EqG_methods.__ne__;
        protofooQ_HashableD_KeyG_methods.__init__ = protofooQ_HashableD_KeyD___init__;
        protofooQ_HashableD_KeyG_methods.hash = protofooQ_HashableD_KeyD_hash;
        protofooQ_HashableD_KeyG_methods.__serialize__ = protofooQ_HashableD_KeyD___serialize__;
        protofooQ_HashableD_KeyG_methods.__deserialize__ = protofooQ_HashableD_KeyD___deserialize__;
        $register(&protofooQ_HashableD_KeyG_methods);
    }
    {
        protofooQ_mainG_methods.$GCINFO = "protofooQ_main";
        protofooQ_mainG_methods.$superclass = ($SuperG_class)&$ActorG_methods;
        protofooQ_mainG_methods.__bool__ = (B_bool (*) (protofooQ_main))$ActorG_methods.__bool__;
        protofooQ_mainG_methods.__str__ = (B_str (*) (protofooQ_main))$ActorG_methods.__str__;
        protofooQ_mainG_methods.__repr__ = (B_str (*) (protofooQ_main))$ActorG_methods.__repr__;
        protofooQ_mainG_methods.__resume__ = (B_NoneType (*) (protofooQ_main))$ActorG_methods.__resume__;
        protofooQ_mainG_methods.__cleanup__ = (B_NoneType (*) (protofooQ_main))$ActorG_methods.__cleanup__;
        protofooQ_mainG_methods.__init__ = protofooQ_mainD___init__;
        protofooQ_mainG_methods.__serialize__ = protofooQ_mainD___serialize__;
        protofooQ_mainG_methods.__deserialize__ = protofooQ_mainD___deserialize__;
        $register(&protofooQ_mainG_methods);
    }
}