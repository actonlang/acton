// Integral$ndarray /////////////////////////////////////////////////////////////////////////////////////////////


numpy$$ndarray numpy$$Integral$ndarray$__add__(numpy$$Integral$ndarray wit, numpy$$ndarray a, numpy$$ndarray b){
  return numpy$$ndarray_oper(wit->w$Primitive$A$numpy->$class->$add,a,b);
}

numpy$$ndarray numpy$$Integral$ndarray$__fromatom__(numpy$$Integral$ndarray wit,$WORD atom) {
  return numpy$$ndarray_fromatom(($Super)atom);
}

numpy$$ndarray numpy$$Integral$ndarray$__mul__(numpy$$Integral$ndarray wit, numpy$$ndarray a, numpy$$ndarray b) {
  return numpy$$ndarray_oper(wit->w$Primitive$A$numpy->$class->$mul,a,b);
}

numpy$$ndarray numpy$$Integral$ndarray$__truediv__(numpy$$Integral$ndarray wit, numpy$$ndarray a, numpy$$ndarray b) {
    return numpy$$ndarray_oper(wit->w$Primitive$A$numpy->$class->$div,a,b);
}


numpy$$ndarray numpy$$Integral$ndarray$__pow__(numpy$$Integral$ndarray wit, numpy$$ndarray a, numpy$$ndarray b) {
    return numpy$$ndarray_oper(wit->w$Primitive$A$numpy->$class->$pow,a,b);
}

numpy$$ndarray numpy$$Integral$ndarray$__neg__(numpy$$Integral$ndarray wit, numpy$$ndarray a) {
  return numpy$$ndarray_func(wit->w$Primitive$A$numpy->$class->$neg,a);
}

numpy$$ndarray numpy$$Integral$ndarray$__pos__(numpy$$Integral$ndarray wit, numpy$$ndarray a) {
  return a;
}

$WORD numpy$$Integral$ndarray$real(numpy$$Integral$ndarray wit, $Real wit2, numpy$$ndarray a);
$WORD numpy$$Integral$ndarray$imag(numpy$$Integral$ndarray wit, $Real wit2, numpy$$ndarray a);
$WORD numpy$$Integral$ndarray$__abs__(numpy$$Integral$ndarray wit, $Real wit2, numpy$$ndarray a) {
    return numpy$$ndarray_func(wit->w$Primitive$A$numpy->$class->$abs,a);
}
numpy$$ndarray numpy$$Integral$ndarray$conjugate(numpy$$Integral$ndarray wit, numpy$$ndarray a);
$float numpy$$Integral$ndarray$__float__ (numpy$$Integral$ndarray wit, numpy$$ndarray a);
$WORD numpy$$Integral$ndarray$__trunc__ (numpy$$Integral$ndarray wit, $Integral wit2, numpy$$ndarray a);
$WORD numpy$$Integral$ndarray$__floor__ (numpy$$Integral$ndarray wit, $Integral wit2, numpy$$ndarray a);
$WORD numpy$$Integral$ndarray$__ceil__ (numpy$$Integral$ndarray wit, $Integral wit2, numpy$$ndarray a);
numpy$$ndarray numpy$$Integral$ndarray$__round__ (numpy$$Integral$ndarray wit, numpy$$ndarray a, numpy$$ndarray b);
$WORD numpy$$Integral$ndarray$numerator (numpy$$Integral$ndarray wit, $Integral wit2, numpy$$ndarray a);
$WORD numpy$$Integral$ndarray$denominator (numpy$$Integral$ndarray wit, $Integral wit2, numpy$$ndarray a);
numpy$$ndarray numpy$$Integral$ndarray$__int__ (numpy$$Integral$ndarray wit, numpy$$ndarray a);
numpy$$ndarray numpy$$Integral$ndarray$__index__ (numpy$$Integral$ndarray wit, numpy$$ndarray a);
$tuple numpy$$Integral$ndarray$__divmod__ (numpy$$Integral$ndarray wit, numpy$$ndarray a, numpy$$ndarray b);
numpy$$ndarray numpy$$Integral$ndarray$__floordiv__ (numpy$$Integral$ndarray wit, numpy$$ndarray a, numpy$$ndarray b) {
    return numpy$$ndarray_oper(wit->w$Primitive$A$numpy->$class->$div,a,b);
}  
numpy$$ndarray numpy$$Integral$ndarray$__mod__ (numpy$$Integral$ndarray wit, numpy$$ndarray a, numpy$$ndarray b);
numpy$$ndarray numpy$$Integral$ndarray$__lshift__ (numpy$$Integral$ndarray wit, numpy$$ndarray a, numpy$$ndarray b);
numpy$$ndarray numpy$$Integral$ndarray$__rshift__ (numpy$$Integral$ndarray wit, numpy$$ndarray a, numpy$$ndarray b);
numpy$$ndarray numpy$$Integral$ndarray$__invert__ (numpy$$Integral$ndarray wit, numpy$$ndarray a);

// Logical$ndarray //////////////////////////////////////////////////////////////////////////////

void numpy$$Logical$ndarray$__init__ (numpy$$Logical$ndarray wit, $Integral a);
numpy$$ndarray numpy$$Logical$ndarray$__and__ (numpy$$Logical$ndarray wit, numpy$$ndarray a, numpy$$ndarray b);
numpy$$ndarray numpy$$Logical$ndarray$__or__ (numpy$$Logical$ndarray wit, numpy$$ndarray a, numpy$$ndarray b);
numpy$$ndarray numpy$$Logical$ndarray$__xor__ (numpy$$Logical$ndarray wit, numpy$$ndarray a, numpy$$ndarray b);

// Minus$ndarray /////////////////////////////////////////////////////////////////////////////////


numpy$$ndarray numpy$$Minus$ndarray$__sub__ (numpy$$Minus$ndarray wit, numpy$$ndarray a, numpy$$ndarray b) {
  return numpy$$ndarray_oper(((numpy$$Integral$ndarray)wit->w$Integral)->w$Primitive$A$numpy->$class->$sub,a,b);
}

// Initialization //////////////////////////////////////////////////////////////////////////////////

void numpy$$Integral$ndarray_init(numpy$$Integral$ndarray wit, numpy$$Primitive w$Primitive$A$numpy) {
  wit->w$Logical = ($Logical)$NEW(numpy$$Logical$ndarray,($Integral)wit);
  wit->w$Minus = ($Minus)$NEW(numpy$$Minus$ndarray,($Integral)wit);
  wit->w$Primitive$A$numpy = w$Primitive$A$numpy;
}; 

void numpy$$Logical$ndarray_init(numpy$$Logical$ndarray wit, $Integral w$Integral) {
  wit->w$Integral =  w$Integral;
};

void numpy$$Minus$ndarray_init(numpy$$Minus$ndarray wit, $Integral w$Integral) {
  wit->w$Integral =  w$Integral;
};

struct numpy$$Integral$ndarray numpy$$Integral$ndarray_instance;
struct numpy$$Logical$ndarray numpy$$Logical$ndarray_instance;
struct numpy$$Minus$ndarray numpy$$Minus$ndarray_instance;

struct numpy$$Integral$ndarray$class numpy$$Integral$ndarray$methods = {"numpy$Integral$ndarray",UNASSIGNED,NULL,numpy$$Integral$ndarray_init,
                                                                        NULL,NULL,NULL,NULL,
                                                                        numpy$$Integral$ndarray$__add__,numpy$$Integral$ndarray$__fromatom__,NULL,
                                                    numpy$$Integral$ndarray$__mul__,numpy$$Integral$ndarray$__truediv__,numpy$$Integral$ndarray$__pow__,numpy$$Integral$ndarray$__neg__,
                                                    numpy$$Integral$ndarray$__pos__,NULL,NULL,numpy$$Integral$ndarray$__abs__,NULL,
                                                    NULL,NULL,NULL,
                                                    NULL,NULL,NULL,NULL,
                                                    NULL,NULL,NULL, numpy$$Integral$ndarray$__floordiv__ ,
                                                    NULL,NULL,NULL,NULL};

struct numpy$$Logical$ndarray$class numpy$$Logical$ndarray$methods =  {"numpy$Logical$ndarray", UNASSIGNED,NULL,numpy$$Logical$ndarray_init,
                                                                       NULL,NULL,NULL,NULL,
                                                                       NULL,NULL,NULL};
struct numpy$$Logical$ndarray numpy$$Logical$ndarray_instance = {&numpy$$Logical$ndarray$methods, ($Integral)&numpy$$Integral$ndarray_instance};
numpy$$Logical$ndarray numpy$$Logical$ndarray$witness = &numpy$$Logical$ndarray_instance;

struct numpy$$Minus$ndarray$class numpy$$Minus$ndarray$methods = {"numpy$Minus$ndarray",UNASSIGNED, NULL,numpy$$Minus$ndarray_init,
                                                                  NULL,NULL,NULL,NULL,
                                                                  numpy$$Minus$ndarray$__sub__};
struct numpy$$Minus$ndarray numpy$$Minus$ndarray_instance = {&numpy$$Minus$ndarray$methods,  ($Integral)&numpy$$Integral$ndarray_instance};
numpy$$Minus$ndarray numpy$$Minus$ndarray$witness = &numpy$$Minus$ndarray_instance;
 
// numpy$$Iterable$ndarray ////////////////////////////////////////////////////////

/*
void numpy$$Iterable$ndarray$__init__(numpy$$Iterable$ndarray self, numpy$$Primitive pwit) {
  self->pwit = pwit;
}
  
$Iterator numpy$$Iterable$ndarray$__iter__(numpy$$Iterable$ndarray self, numpy$$ndarray a) {
  return ($Iterator)$NEW($Iterator$ndarray,self->pwit,a);
}

struct numpy$$Iterable$ndarray$class  numpy$$Iterable$ndarray$methods =  {"", UNASSIGNED,NULL,numpy$$Iterable$ndarray$__init__,numpy$$Iterable$ndarray$__iter__};
*/
