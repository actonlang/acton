
// EqOpt //////////////////////////////////////////////////////

extern struct $EqOptG_class $EqOptG_methods;

void $EqOptD___init__($EqOpt wit, B_Eq W_Eq$A) {
    wit->W_Eq$A = W_Eq$A;
}

void $EqOptD_serialize($EqOpt self,$Serial$state state) {
    $step_serialize(self->W_Eq$A,state);
}

$EqOpt $EqOptD_deserialize($EqOpt res, $Serial$state state) {
    if (!res)
        res = $DNEW($EqOpt,state);
    res->W_Eq$A = $step_deserialize(state);
    return res;
}

B_bool $EqOptD___eq__($EqOpt wit, $WORD a, $WORD b) {
    if (a && b) {
        return wit->W_Eq$A->$class->__eq__(wit->W_Eq$A, a, b);
    }
    return (!a && !b) ? B_True : B_False;
}

B_bool $EqOptD___ne__($EqOpt wit, $WORD a, $WORD b) {
    if (a && b)
        return wit->W_Eq$A->$class->__ne__(wit->W_Eq$A, a, b);
    return (!a && !b) ? B_False : B_True;
}

struct $EqOptG_class $EqOptG_methods = {"$EqOpt", UNASSIGNED, NULL, $EqOptD___init__, $EqOptD_serialize, $EqOptD_deserialize, $EqOptD___eq__, $EqOptD___ne__};


$EqOpt $EqOptG_new(B_Eq W_Eq$A) {
    return $NEW($EqOpt, W_Eq$A);
}


// ShowOpt ////////////////////////////////////////

extern struct $ShowOptG_class $ShowOptG_methods;

void $ShowOptD___init__($ShowOpt wit, B_Show W_Show$A) {
    wit->W_Show$A = W_Show$A;
}

void $ShowOptD_serialize($ShowOpt self, $Serial$state state) {
    $step_serialize(self->W_Show$A,state);
}

$ShowOpt $ShowOptD_deserialize($ShowOpt res, $Serial$state state) {
    if (!res)
        res = $DNEW($ShowOpt,state);
    res->W_Show$A = $step_deserialize(state);
    return res;
}

B_str $ShowOptD___str__($ShowOpt wit, $WORD a) {
    if (a) {
        return wit->W_Show$A->$class->__str__(wit->W_Show$A, a);
    }
    return to$str("None");
}

B_str $ShowOptD___repr__($ShowOpt wit, $WORD a) {
    if (a) {
        return wit->W_Show$A->$class->__repr__(wit->W_Show$A, a);
    }
    return to$str("None");
}

struct $ShowOptG_class $ShowOptG_methods = {"$ShowOpt", UNASSIGNED, NULL, $ShowOptD___init__, $ShowOptD_serialize, $ShowOptD_deserialize, $ShowOptD___str__, $ShowOptD___repr__};


$ShowOpt $ShowOptG_new(B_Show W_Show$A) {
    return $NEW($ShowOpt, W_Show$A);
}

 

