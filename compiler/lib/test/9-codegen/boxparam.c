/* Acton impl hash: test-hash */
#include "rts/common.h"
#include "out/types/boxparam.h"
B_value boxparamQ_BaseD___get_attr__ (boxparamQ_Base self, B_str name) {
    return B_None;
}
B_NoneType boxparamQ_BaseG_init (boxparamQ_Base self) {
    return B_None;
}
#line 5 "test/src/boxparam.act"
B_NoneType boxparamQ_BaseD___init__ (boxparamQ_Base self) {
    #line 6 "test/src/boxparam.act"
    return B_None;
}
#line 7 "test/src/boxparam.act"
bool boxparamQ_BaseD_cmp (boxparamQ_Base self, $WORD x) {
    #line 8 "test/src/boxparam.act"
    return true;
}
#line 9 "test/src/boxparam.act"
$WORD boxparamQ_BaseD_bump (boxparamQ_Base self, $WORD x) {
    #line 10 "test/src/boxparam.act"
    return x;
}
#line 11 "test/src/boxparam.act"
$WORD boxparamQ_BaseD_fwd (boxparamQ_Base self, $WORD x) {
    #line 12 "test/src/boxparam.act"
    return x;
}
void boxparamQ_BaseD___serialize__ (boxparamQ_Base self, $Serial$state state) {
}
boxparamQ_Base boxparamQ_BaseD___deserialize__ (boxparamQ_Base self, $Serial$state state) {
    $WORD $tmp;
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct boxparamQ_Base));
            self->$class = &boxparamQ_BaseG_methods;
            return self;
        }
        self = $DNEW(boxparamQ_Base, state);
    }
    return self;
}
boxparamQ_Base boxparamQ_BaseG_new() {
    boxparamQ_Base $tmp = acton_malloc(sizeof(struct boxparamQ_Base));
    $tmp->$class = &boxparamQ_BaseG_methods;
    boxparamQ_BaseG_methods.G_init($tmp);
    boxparamQ_BaseG_methods.__init__($tmp);
    return $tmp;
}
struct boxparamQ_BaseG_class boxparamQ_BaseG_methods;
B_NoneType boxparamQ_DerivD___init__ (boxparamQ_Deriv G_1p) {
    ((B_NoneType (*) (boxparamQ_Base))boxparamQ_BaseG_methods.__init__)(((boxparamQ_Base)G_1p));
    return B_None;
}
B_value boxparamQ_DerivD___get_attr__ (boxparamQ_Deriv self, B_str name) {
    return B_None;
}
B_NoneType boxparamQ_DerivG_init (boxparamQ_Deriv self) {
    ((B_NoneType (*) (boxparamQ_Base))boxparamQ_BaseG_methods.G_init)(((boxparamQ_Base)self));
    return B_None;
}
#line 15 "test/src/boxparam.act"
bool boxparamQ_DerivD_cmp (boxparamQ_Deriv self, B_int x) {
    bool N_tmp = (((B_int)x)->val > 3LL);
    #line 16 "test/src/boxparam.act"
    return N_tmp;
}
#line 17 "test/src/boxparam.act"
B_int boxparamQ_DerivD_bump (boxparamQ_Deriv self, B_int xD_boxed) {
    int64_t x = ((B_int)xD_boxed)->val;
    x += 1LL;
    #line 19 "test/src/boxparam.act"
    return toB_int(x);
}
#line 20 "test/src/boxparam.act"
B_int boxparamQ_DerivD_fwd (boxparamQ_Deriv self, B_int x) {
    int64_t N_1tmp = ((B_int)((B_int (*) ($WORD, B_int))((boxparamQ_Deriv)(self))->$class->bump)(self, x))->val;
    #line 21 "test/src/boxparam.act"
    return toB_int(N_1tmp);
}
void boxparamQ_DerivD___serialize__ (boxparamQ_Deriv self, $Serial$state state) {
}
boxparamQ_Deriv boxparamQ_DerivD___deserialize__ (boxparamQ_Deriv self, $Serial$state state) {
    $WORD $tmp;
    if (!self) {
        if (!state) {
            self = acton_malloc(sizeof(struct boxparamQ_Deriv));
            self->$class = &boxparamQ_DerivG_methods;
            return self;
        }
        self = $DNEW(boxparamQ_Deriv, state);
    }
    return self;
}
boxparamQ_Deriv boxparamQ_DerivG_new() {
    boxparamQ_Deriv $tmp = acton_malloc(sizeof(struct boxparamQ_Deriv));
    $tmp->$class = &boxparamQ_DerivG_methods;
    boxparamQ_DerivG_methods.G_init($tmp);
    boxparamQ_DerivG_methods.__init__($tmp);
    return $tmp;
}
struct boxparamQ_DerivG_class boxparamQ_DerivG_methods;
int boxparamQ_done$ = 0;
void boxparamQ___init__ () {
    if (boxparamQ_done$) return;
    boxparamQ_done$ = 1;
    {
        boxparamQ_BaseG_methods.$GCINFO = "boxparamQ_Base";
        boxparamQ_BaseG_methods.$superclass = ($SuperG_class)&B_objectG_methods;
        boxparamQ_BaseG_methods.__bool__ = (bool (*) (boxparamQ_Base))B_valueG_methods.__bool__;
        boxparamQ_BaseG_methods.__str__ = (B_str (*) (boxparamQ_Base))B_valueG_methods.__str__;
        boxparamQ_BaseG_methods.__repr__ = (B_str (*) (boxparamQ_Base))B_valueG_methods.__repr__;
        boxparamQ_BaseG_methods.__get_attr__ = (B_value (*) (boxparamQ_Base, B_str))boxparamQ_BaseD___get_attr__;
        boxparamQ_BaseG_methods.G_init = (B_NoneType (*) (boxparamQ_Base))boxparamQ_BaseG_init;
        boxparamQ_BaseG_methods.__init__ = (B_NoneType (*) (boxparamQ_Base))boxparamQ_BaseD___init__;
        boxparamQ_BaseG_methods.cmp = (bool (*) (boxparamQ_Base, $WORD))boxparamQ_BaseD_cmp;
        boxparamQ_BaseG_methods.bump = ($WORD (*) (boxparamQ_Base, $WORD))boxparamQ_BaseD_bump;
        boxparamQ_BaseG_methods.fwd = ($WORD (*) (boxparamQ_Base, $WORD))boxparamQ_BaseD_fwd;
        boxparamQ_BaseG_methods.__serialize__ = boxparamQ_BaseD___serialize__;
        boxparamQ_BaseG_methods.__deserialize__ = boxparamQ_BaseD___deserialize__;
        $register(&boxparamQ_BaseG_methods);
    }
    {
        boxparamQ_DerivG_methods.$GCINFO = "boxparamQ_Deriv";
        boxparamQ_DerivG_methods.$superclass = ($SuperG_class)&boxparamQ_BaseG_methods;
        boxparamQ_DerivG_methods.__bool__ = (bool (*) (boxparamQ_Deriv))B_valueG_methods.__bool__;
        boxparamQ_DerivG_methods.__str__ = (B_str (*) (boxparamQ_Deriv))B_valueG_methods.__str__;
        boxparamQ_DerivG_methods.__repr__ = (B_str (*) (boxparamQ_Deriv))B_valueG_methods.__repr__;
        boxparamQ_DerivG_methods.__init__ = (B_NoneType (*) (boxparamQ_Deriv))boxparamQ_DerivD___init__;
        boxparamQ_DerivG_methods.__get_attr__ = (B_value (*) (boxparamQ_Deriv, B_str))boxparamQ_DerivD___get_attr__;
        boxparamQ_DerivG_methods.G_init = (B_NoneType (*) (boxparamQ_Deriv))boxparamQ_DerivG_init;
        boxparamQ_DerivG_methods.cmp = (bool (*) (boxparamQ_Deriv, B_int))boxparamQ_DerivD_cmp;
        boxparamQ_DerivG_methods.bump = (B_int (*) (boxparamQ_Deriv, B_int))boxparamQ_DerivD_bump;
        boxparamQ_DerivG_methods.fwd = (B_int (*) (boxparamQ_Deriv, B_int))boxparamQ_DerivD_fwd;
        boxparamQ_DerivG_methods.__serialize__ = boxparamQ_DerivD___serialize__;
        boxparamQ_DerivG_methods.__deserialize__ = boxparamQ_DerivD___deserialize__;
        $register(&boxparamQ_DerivG_methods);
    }
}