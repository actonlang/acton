/* Acton impl hash: test-hash */
#include "rts/common.h"
#include "out/types/witness_forward.h"
B_NoneType witness_forwardQ_PAD___init__ (witness_forwardQ_PA W_self) {
    return B_None;
}
void witness_forwardQ_PAD___serialize__ (witness_forwardQ_PA self, $Serial$state state) {
}
witness_forwardQ_PA witness_forwardQ_PAD___deserialize__ (witness_forwardQ_PA self, $Serial$state state) {
    $WORD $tmp;
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct witness_forwardQ_PA));
            self->$class = &witness_forwardQ_PAG_methods;
            return self;
        }
        self = $DNEW(witness_forwardQ_PA, state);
    }
    return self;
}
struct witness_forwardQ_PAG_class witness_forwardQ_PAG_methods;
B_NoneType witness_forwardQ_PBD___init__ (witness_forwardQ_PB W_self) {
    ((B_NoneType (*) (witness_forwardQ_PA))witness_forwardQ_PAG_methods.__init__)(((witness_forwardQ_PA)W_self));
    return B_None;
}
void witness_forwardQ_PBD___serialize__ (witness_forwardQ_PB self, $Serial$state state) {
}
witness_forwardQ_PB witness_forwardQ_PBD___deserialize__ (witness_forwardQ_PB self, $Serial$state state) {
    $WORD $tmp;
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct witness_forwardQ_PB));
            self->$class = &witness_forwardQ_PBG_methods;
            return self;
        }
        self = $DNEW(witness_forwardQ_PB, state);
    }
    return self;
}
struct witness_forwardQ_PBG_class witness_forwardQ_PBG_methods;
B_NoneType witness_forwardQ_PCD___init__ (witness_forwardQ_PC W_self) {
    ((B_NoneType (*) (witness_forwardQ_PA))witness_forwardQ_PAG_methods.__init__)(((witness_forwardQ_PA)W_self));
    return B_None;
}
void witness_forwardQ_PCD___serialize__ (witness_forwardQ_PC self, $Serial$state state) {
}
witness_forwardQ_PC witness_forwardQ_PCD___deserialize__ (witness_forwardQ_PC self, $Serial$state state) {
    $WORD $tmp;
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct witness_forwardQ_PC));
            self->$class = &witness_forwardQ_PCG_methods;
            return self;
        }
        self = $DNEW(witness_forwardQ_PC, state);
    }
    return self;
}
struct witness_forwardQ_PCG_class witness_forwardQ_PCG_methods;
B_value witness_forwardQ_ThingD___get_attr__ (witness_forwardQ_Thing self, B_str name) {
    if (B_OrdD_strD___eq__(B_OrdD_strG_witness, name, to$str("n"))) {
        return (B_value)toB_int(self->n);
    }
    return B_None;
}
B_NoneType witness_forwardQ_ThingG_init (witness_forwardQ_Thing self) {
    return B_None;
}
#line 19 "test/src/witness_forward.act"
B_NoneType witness_forwardQ_ThingD___init__ (witness_forwardQ_Thing self, int64_t n) {
    #line 20 "test/src/witness_forward.act"
    ((witness_forwardQ_Thing)(self))->n = n;
    return B_None;
}
void witness_forwardQ_ThingD___serialize__ (witness_forwardQ_Thing self, $Serial$state state) {
    $val_serialize(I64_ID, &self->n, state);
}
witness_forwardQ_Thing witness_forwardQ_ThingD___deserialize__ (witness_forwardQ_Thing self, $Serial$state state) {
    $WORD $tmp;
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct witness_forwardQ_Thing));
            self->$class = &witness_forwardQ_ThingG_methods;
            return self;
        }
        self = $DNEW(witness_forwardQ_Thing, state);
    }
    $tmp = $val_deserialize(state);
    memcpy(&self->n, &$tmp, sizeof(self->n));
    return self;
}
witness_forwardQ_Thing witness_forwardQ_ThingG_new(int64_t G_1) {
    witness_forwardQ_Thing $tmp = acton_malloc(sizeof(struct witness_forwardQ_Thing));
    $tmp->$class = &witness_forwardQ_ThingG_methods;
    witness_forwardQ_ThingG_methods.G_init($tmp);
    witness_forwardQ_ThingG_methods.__init__($tmp, G_1);
    return $tmp;
}
struct witness_forwardQ_ThingG_class witness_forwardQ_ThingG_methods;
B_NoneType witness_forwardQ_PBD_ThingD___init__ (witness_forwardQ_PBD_Thing W_self) {
    ((B_NoneType (*) (witness_forwardQ_PB))witness_forwardQ_PBG_methods.__init__)(((witness_forwardQ_PB)W_self));
    return B_None;
}
#line 23 "test/src/witness_forward.act"
bool witness_forwardQ_PBD_ThingD_same (witness_forwardQ_PBD_Thing W_self, witness_forwardQ_Thing a, witness_forwardQ_Thing b) {
    bool N_tmp = (((int64_t)((witness_forwardQ_Thing)(a))->n) == ((int64_t)((witness_forwardQ_Thing)(b))->n));
    #line 25 "test/src/witness_forward.act"
    return N_tmp;
}
#line 26 "test/src/witness_forward.act"
int64_t witness_forwardQ_PBD_ThingD_total (witness_forwardQ_PBD_Thing W_self, witness_forwardQ_Thing self) {
    #line 27 "test/src/witness_forward.act"
    return ((int64_t)((witness_forwardQ_Thing)(self))->n);
}
#line 28 "test/src/witness_forward.act"
int64_t witness_forwardQ_PBD_ThingD_beta (witness_forwardQ_PBD_Thing W_self, witness_forwardQ_Thing self) {
    int64_t N_1tmp = 1LL;
    #line 29 "test/src/witness_forward.act"
    return N_1tmp;
}
void witness_forwardQ_PBD_ThingD___serialize__ (witness_forwardQ_PBD_Thing self, $Serial$state state) {
}
witness_forwardQ_PBD_Thing witness_forwardQ_PBD_ThingD___deserialize__ (witness_forwardQ_PBD_Thing self, $Serial$state state) {
    $WORD $tmp;
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct witness_forwardQ_PBD_Thing));
            self->$class = &witness_forwardQ_PBD_ThingG_methods;
            return self;
        }
        self = $DNEW(witness_forwardQ_PBD_Thing, state);
    }
    return self;
}
witness_forwardQ_PBD_Thing witness_forwardQ_PBD_ThingG_new() {
    witness_forwardQ_PBD_Thing $tmp = acton_malloc(sizeof(struct witness_forwardQ_PBD_Thing));
    $tmp->$class = &witness_forwardQ_PBD_ThingG_methods;
    witness_forwardQ_PBD_ThingG_methods.__init__($tmp);
    return $tmp;
}
struct witness_forwardQ_PBD_ThingG_class witness_forwardQ_PBD_ThingG_methods;
B_NoneType witness_forwardQ_PCD_ThingD___init__ (witness_forwardQ_PCD_Thing W_self) {
    ((witness_forwardQ_PCD_Thing)(W_self))->W_PA_34 = ((witness_forwardQ_PA)witness_forwardQ_PBD_ThingG_new());
    ((B_NoneType (*) (witness_forwardQ_PC))witness_forwardQ_PCG_methods.__init__)(((witness_forwardQ_PC)W_self));
    return B_None;
}
#line 32 "test/src/witness_forward.act"
int64_t witness_forwardQ_PCD_ThingD_gamma (witness_forwardQ_PCD_Thing W_self, witness_forwardQ_Thing self) {
    int64_t N_2tmp = 2LL;
    #line 33 "test/src/witness_forward.act"
    return N_2tmp;
}
int64_t witness_forwardQ_PCD_ThingD_total (witness_forwardQ_PCD_Thing W_self, witness_forwardQ_Thing G_1p) {
    int64_t N_3tmp = ((int64_t (*) ($WORD, witness_forwardQ_Thing))((witness_forwardQ_PA)(((witness_forwardQ_PCD_Thing)(W_self))->W_PA_34))->$class->total)(((witness_forwardQ_PCD_Thing)(W_self))->W_PA_34, G_1p);
    return N_3tmp;
}
bool witness_forwardQ_PCD_ThingD_same (witness_forwardQ_PCD_Thing W_self, witness_forwardQ_Thing G_1p, witness_forwardQ_Thing G_2p) {
    bool N_4tmp = ((bool (*) ($WORD, witness_forwardQ_Thing, witness_forwardQ_Thing))((witness_forwardQ_PA)(((witness_forwardQ_PCD_Thing)(W_self))->W_PA_34))->$class->same)(((witness_forwardQ_PCD_Thing)(W_self))->W_PA_34, G_1p, G_2p);
    return N_4tmp;
}
void witness_forwardQ_PCD_ThingD___serialize__ (witness_forwardQ_PCD_Thing self, $Serial$state state) {
    $step_serialize(self->W_PA_34, state);
}
witness_forwardQ_PCD_Thing witness_forwardQ_PCD_ThingD___deserialize__ (witness_forwardQ_PCD_Thing self, $Serial$state state) {
    $WORD $tmp;
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct witness_forwardQ_PCD_Thing));
            self->$class = &witness_forwardQ_PCD_ThingG_methods;
            return self;
        }
        self = $DNEW(witness_forwardQ_PCD_Thing, state);
    }
    self->W_PA_34 = $step_deserialize(state);
    return self;
}
witness_forwardQ_PCD_Thing witness_forwardQ_PCD_ThingG_new() {
    witness_forwardQ_PCD_Thing $tmp = acton_malloc(sizeof(struct witness_forwardQ_PCD_Thing));
    $tmp->$class = &witness_forwardQ_PCD_ThingG_methods;
    witness_forwardQ_PCD_ThingG_methods.__init__($tmp);
    return $tmp;
}
struct witness_forwardQ_PCD_ThingG_class witness_forwardQ_PCD_ThingG_methods;
int witness_forwardQ_done$ = 0;
void witness_forwardQ___init__ () {
    if (witness_forwardQ_done$) return;
    witness_forwardQ_done$ = 1;
    {
        witness_forwardQ_PAG_methods.$GCINFO = "witness_forwardQ_PA";
        witness_forwardQ_PAG_methods.$superclass = ($SuperG_class)&B_valueG_methods;
        witness_forwardQ_PAG_methods.__bool__ = (bool (*) (witness_forwardQ_PA))B_valueG_methods.__bool__;
        witness_forwardQ_PAG_methods.__str__ = (B_str (*) (witness_forwardQ_PA))B_valueG_methods.__str__;
        witness_forwardQ_PAG_methods.__repr__ = (B_str (*) (witness_forwardQ_PA))B_valueG_methods.__repr__;
        witness_forwardQ_PAG_methods.__init__ = (B_NoneType (*) (witness_forwardQ_PA))witness_forwardQ_PAD___init__;
        witness_forwardQ_PAG_methods.__serialize__ = witness_forwardQ_PAD___serialize__;
        witness_forwardQ_PAG_methods.__deserialize__ = witness_forwardQ_PAD___deserialize__;
        $register(&witness_forwardQ_PAG_methods);
    }
    {
        witness_forwardQ_PBG_methods.$GCINFO = "witness_forwardQ_PB";
        witness_forwardQ_PBG_methods.$superclass = ($SuperG_class)&witness_forwardQ_PAG_methods;
        witness_forwardQ_PBG_methods.__bool__ = (bool (*) (witness_forwardQ_PB))B_valueG_methods.__bool__;
        witness_forwardQ_PBG_methods.__str__ = (B_str (*) (witness_forwardQ_PB))B_valueG_methods.__str__;
        witness_forwardQ_PBG_methods.__repr__ = (B_str (*) (witness_forwardQ_PB))B_valueG_methods.__repr__;
        witness_forwardQ_PBG_methods.__init__ = (B_NoneType (*) (witness_forwardQ_PB))witness_forwardQ_PBD___init__;
        witness_forwardQ_PBG_methods.__serialize__ = witness_forwardQ_PBD___serialize__;
        witness_forwardQ_PBG_methods.__deserialize__ = witness_forwardQ_PBD___deserialize__;
        $register(&witness_forwardQ_PBG_methods);
    }
    {
        witness_forwardQ_PCG_methods.$GCINFO = "witness_forwardQ_PC";
        witness_forwardQ_PCG_methods.$superclass = ($SuperG_class)&witness_forwardQ_PAG_methods;
        witness_forwardQ_PCG_methods.__bool__ = (bool (*) (witness_forwardQ_PC))B_valueG_methods.__bool__;
        witness_forwardQ_PCG_methods.__str__ = (B_str (*) (witness_forwardQ_PC))B_valueG_methods.__str__;
        witness_forwardQ_PCG_methods.__repr__ = (B_str (*) (witness_forwardQ_PC))B_valueG_methods.__repr__;
        witness_forwardQ_PCG_methods.__init__ = (B_NoneType (*) (witness_forwardQ_PC))witness_forwardQ_PCD___init__;
        witness_forwardQ_PCG_methods.__serialize__ = witness_forwardQ_PCD___serialize__;
        witness_forwardQ_PCG_methods.__deserialize__ = witness_forwardQ_PCD___deserialize__;
        $register(&witness_forwardQ_PCG_methods);
    }
    {
        witness_forwardQ_ThingG_methods.$GCINFO = "witness_forwardQ_Thing";
        witness_forwardQ_ThingG_methods.$superclass = ($SuperG_class)&B_objectG_methods;
        witness_forwardQ_ThingG_methods.__bool__ = (bool (*) (witness_forwardQ_Thing))B_valueG_methods.__bool__;
        witness_forwardQ_ThingG_methods.__str__ = (B_str (*) (witness_forwardQ_Thing))B_valueG_methods.__str__;
        witness_forwardQ_ThingG_methods.__repr__ = (B_str (*) (witness_forwardQ_Thing))B_valueG_methods.__repr__;
        witness_forwardQ_ThingG_methods.__get_attr__ = (B_value (*) (witness_forwardQ_Thing, B_str))witness_forwardQ_ThingD___get_attr__;
        witness_forwardQ_ThingG_methods.G_init = (B_NoneType (*) (witness_forwardQ_Thing))witness_forwardQ_ThingG_init;
        witness_forwardQ_ThingG_methods.__init__ = (B_NoneType (*) (witness_forwardQ_Thing, int64_t))witness_forwardQ_ThingD___init__;
        witness_forwardQ_ThingG_methods.__serialize__ = witness_forwardQ_ThingD___serialize__;
        witness_forwardQ_ThingG_methods.__deserialize__ = witness_forwardQ_ThingD___deserialize__;
        $register(&witness_forwardQ_ThingG_methods);
    }
    {
        witness_forwardQ_PBD_ThingG_methods.$GCINFO = "witness_forwardQ_PBD_Thing";
        witness_forwardQ_PBD_ThingG_methods.$superclass = ($SuperG_class)&witness_forwardQ_PBG_methods;
        witness_forwardQ_PBD_ThingG_methods.__bool__ = (bool (*) (witness_forwardQ_PBD_Thing))B_valueG_methods.__bool__;
        witness_forwardQ_PBD_ThingG_methods.__str__ = (B_str (*) (witness_forwardQ_PBD_Thing))B_valueG_methods.__str__;
        witness_forwardQ_PBD_ThingG_methods.__repr__ = (B_str (*) (witness_forwardQ_PBD_Thing))B_valueG_methods.__repr__;
        witness_forwardQ_PBD_ThingG_methods.__init__ = (B_NoneType (*) (witness_forwardQ_PBD_Thing))witness_forwardQ_PBD_ThingD___init__;
        witness_forwardQ_PBD_ThingG_methods.same = (bool (*) (witness_forwardQ_PBD_Thing, witness_forwardQ_Thing, witness_forwardQ_Thing))witness_forwardQ_PBD_ThingD_same;
        witness_forwardQ_PBD_ThingG_methods.total = (int64_t (*) (witness_forwardQ_PBD_Thing, witness_forwardQ_Thing))witness_forwardQ_PBD_ThingD_total;
        witness_forwardQ_PBD_ThingG_methods.beta = (int64_t (*) (witness_forwardQ_PBD_Thing, witness_forwardQ_Thing))witness_forwardQ_PBD_ThingD_beta;
        witness_forwardQ_PBD_ThingG_methods.__serialize__ = witness_forwardQ_PBD_ThingD___serialize__;
        witness_forwardQ_PBD_ThingG_methods.__deserialize__ = witness_forwardQ_PBD_ThingD___deserialize__;
        $register(&witness_forwardQ_PBD_ThingG_methods);
    }
    {
        witness_forwardQ_PCD_ThingG_methods.$GCINFO = "witness_forwardQ_PCD_Thing";
        witness_forwardQ_PCD_ThingG_methods.$superclass = ($SuperG_class)&witness_forwardQ_PCG_methods;
        witness_forwardQ_PCD_ThingG_methods.__bool__ = (bool (*) (witness_forwardQ_PCD_Thing))B_valueG_methods.__bool__;
        witness_forwardQ_PCD_ThingG_methods.__str__ = (B_str (*) (witness_forwardQ_PCD_Thing))B_valueG_methods.__str__;
        witness_forwardQ_PCD_ThingG_methods.__repr__ = (B_str (*) (witness_forwardQ_PCD_Thing))B_valueG_methods.__repr__;
        witness_forwardQ_PCD_ThingG_methods.__init__ = (B_NoneType (*) (witness_forwardQ_PCD_Thing))witness_forwardQ_PCD_ThingD___init__;
        witness_forwardQ_PCD_ThingG_methods.gamma = (int64_t (*) (witness_forwardQ_PCD_Thing, witness_forwardQ_Thing))witness_forwardQ_PCD_ThingD_gamma;
        witness_forwardQ_PCD_ThingG_methods.total = (int64_t (*) (witness_forwardQ_PCD_Thing, witness_forwardQ_Thing))witness_forwardQ_PCD_ThingD_total;
        witness_forwardQ_PCD_ThingG_methods.same = (bool (*) (witness_forwardQ_PCD_Thing, witness_forwardQ_Thing, witness_forwardQ_Thing))witness_forwardQ_PCD_ThingD_same;
        witness_forwardQ_PCD_ThingG_methods.__serialize__ = witness_forwardQ_PCD_ThingD___serialize__;
        witness_forwardQ_PCD_ThingG_methods.__deserialize__ = witness_forwardQ_PCD_ThingD___deserialize__;
        $register(&witness_forwardQ_PCD_ThingG_methods);
    }
}