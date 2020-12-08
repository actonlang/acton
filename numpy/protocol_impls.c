// Integral$ndarray /////////////////////////////////////////////////////////////////////////////////////////////

void numpy$$Integral$ndarray_init(numpy$$Integral$ndarray wit, numpy$$Primitive w$Primitive$A$numpy) {
  wit->w$Logical = ($Logical)$NEW(numpy$$Logical$ndarray,($Integral)wit);
  wit->w$Minus = ($Minus)$NEW(numpy$$Minus$ndarray,($Integral)wit);
  wit->w$Primitive$A$Integral$ndarray = w$Primitive$A$numpy;
}; 


void numpy$$Integral$ndarray$__serialize__(numpy$$Integral$ndarray wit, $Serial$state state) {
    $step_serialize(wit->w$Logical, state);
    $step_serialize(wit->w$Minus, state);
    $step_serialize(wit->w$Primitive$A$Integral$ndarray, state);
}

numpy$$Integral$ndarray numpy$$Integral$ndarray$__deserialize__($Serial$state state) {
    numpy$$Integral$ndarray res = $DNEW(numpy$$Integral$ndarray,state);
    res->w$Logical = ($Logical)$step_deserialize(state);
    res->w$Minus = ($Minus)$step_deserialize(state);
    res->w$Primitive$A$Integral$ndarray = (numpy$$Primitive)$step_deserialize(state);
    return res;
}

numpy$$ndarray numpy$$Integral$ndarray$__add__(numpy$$Integral$ndarray wit, numpy$$ndarray a, numpy$$ndarray b){
  return numpy$$ndarray_oper(wit->w$Primitive$A$Integral$ndarray->$class->$add,a,b);
}

numpy$$ndarray numpy$$Integral$ndarray$__fromatom__(numpy$$Integral$ndarray wit,$WORD atom) {
  return numpy$$ndarray_fromatom(($Super)atom);
}

$complex numpy$$Integral$ndarray$__complx__(numpy$$Integral$ndarray wit, numpy$$ndarray a) {
    RAISE(($BaseException)$NEW($NotImplementedError,to$str("complex not implemented for ndarray")));
    return NULL;
}

numpy$$ndarray numpy$$Integral$ndarray$__mul__(numpy$$Integral$ndarray wit, numpy$$ndarray a, numpy$$ndarray b) {
  return numpy$$ndarray_oper(wit->w$Primitive$A$Integral$ndarray->$class->$mul,a,b);
}

numpy$$ndarray numpy$$Integral$ndarray$__truediv__(numpy$$Integral$ndarray wit, numpy$$ndarray a, numpy$$ndarray b) {
    return numpy$$ndarray_oper(wit->w$Primitive$A$Integral$ndarray->$class->$div,a,b);
}


numpy$$ndarray numpy$$Integral$ndarray$__pow__(numpy$$Integral$ndarray wit, numpy$$ndarray a, numpy$$ndarray b) {
    return numpy$$ndarray_oper(wit->w$Primitive$A$Integral$ndarray->$class->$pow,a,b);
}

numpy$$ndarray numpy$$Integral$ndarray$__neg__(numpy$$Integral$ndarray wit, numpy$$ndarray a) {
  return numpy$$ndarray_func(wit->w$Primitive$A$Integral$ndarray->$class->$neg,a);
}

numpy$$ndarray numpy$$Integral$ndarray$__pos__(numpy$$Integral$ndarray wit, numpy$$ndarray a) {
  return a;
}

$WORD numpy$$Integral$ndarray$real(numpy$$Integral$ndarray wit, $Real wit2, numpy$$ndarray a) {
  return a;
}
$WORD numpy$$Integral$ndarray$imag(numpy$$Integral$ndarray wit, $Real wit2, numpy$$ndarray a);
$WORD numpy$$Integral$ndarray$__abs__(numpy$$Integral$ndarray wit, $Real wit2, numpy$$ndarray a) {
    return numpy$$ndarray_func(wit->w$Primitive$A$Integral$ndarray->$class->$abs,a);
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
    return numpy$$ndarray_oper(wit->w$Primitive$A$Integral$ndarray->$class->$div,a,b);
}  
numpy$$ndarray numpy$$Integral$ndarray$__mod__ (numpy$$Integral$ndarray wit, numpy$$ndarray a, numpy$$ndarray b);
numpy$$ndarray numpy$$Integral$ndarray$__lshift__ (numpy$$Integral$ndarray wit, numpy$$ndarray a, numpy$$ndarray b);
numpy$$ndarray numpy$$Integral$ndarray$__rshift__ (numpy$$Integral$ndarray wit, numpy$$ndarray a, numpy$$ndarray b);
numpy$$ndarray numpy$$Integral$ndarray$__invert__ (numpy$$Integral$ndarray wit, numpy$$ndarray a);

// Logical$ndarray //////////////////////////////////////////////////////////////////////////////

void numpy$$Logical$ndarray_init(numpy$$Logical$ndarray wit, $Integral w$Integral) {
  wit->w$Integral =  w$Integral;
};

void numpy$$Logical$ndarray$__serialize__(numpy$$Logical$ndarray wit, $Serial$state state) {
    $step_serialize(wit->w$Integral, state);
}

numpy$$Logical$ndarray numpy$$Logical$ndarray$__deserialize__($Serial$state state) {
    numpy$$Logical$ndarray res = $DNEW(numpy$$Logical$ndarray,state);
    res->w$Integral = ($Integral)$step_deserialize(state);
    return res;
}

numpy$$ndarray numpy$$Logical$ndarray$__and__ (numpy$$Logical$ndarray wit, numpy$$ndarray a, numpy$$ndarray b) {
  return numpy$$ndarray_oper(((numpy$$Integral$ndarray)wit->w$Integral)->w$Primitive$A$Integral$ndarray->$class->$band,a,b);
}
numpy$$ndarray numpy$$Logical$ndarray$__or__ (numpy$$Logical$ndarray wit, numpy$$ndarray a, numpy$$ndarray b) {
    return numpy$$ndarray_oper(((numpy$$Integral$ndarray)wit->w$Integral)->w$Primitive$A$Integral$ndarray->$class->$bor,a,b);
}
numpy$$ndarray numpy$$Logical$ndarray$__xor__ (numpy$$Logical$ndarray wit, numpy$$ndarray a, numpy$$ndarray b) {
    return numpy$$ndarray_oper(((numpy$$Integral$ndarray)wit->w$Integral)->w$Primitive$A$Integral$ndarray->$class->$bxor,a,b);
}

// Minus$ndarray /////////////////////////////////////////////////////////////////////////////////

void numpy$$Minus$ndarray_init(numpy$$Minus$ndarray wit, $Integral w$Integral) {
  wit->w$Integral =  w$Integral;
};

void numpy$$Minus$ndarray$__serialize__(numpy$$Minus$ndarray wit, $Serial$state state) {
    $step_serialize(wit->w$Integral, state);
}

numpy$$Minus$ndarray numpy$$Minus$ndarray$__deserialize__($Serial$state state) {
    numpy$$Minus$ndarray res = $DNEW(numpy$$Minus$ndarray,state);
    res->w$Integral = ($Integral)$step_deserialize(state);
    return res;
}

numpy$$ndarray numpy$$Minus$ndarray$__sub__ (numpy$$Minus$ndarray wit, numpy$$ndarray a, numpy$$ndarray b) {
  return numpy$$ndarray_oper(((numpy$$Integral$ndarray)wit->w$Integral)->w$Primitive$A$Integral$ndarray->$class->$sub,a,b);
}

// Sliceable$ndarray ///////////////////////////////////////////////////////////////////////////////

numpy$$Sliceable$ndarray numpy$$Sliceable$ndarray$new() {
    numpy$$Sliceable$ndarray $tmp = malloc(sizeof(struct numpy$$Sliceable$ndarray));
    $tmp->$class = &numpy$$Sliceable$ndarray$methods;
    //numpy$$Sliceable$ndarray$methods.__init__($tmp);
    return $tmp;
}

void numpy$$Sliceable$ndarray$__init__ (numpy$$Sliceable$ndarray self) {
}

void numpy$$Sliceable$ndarray$__serialize__(numpy$$Sliceable$ndarray wit, $Serial$state state) {
}

numpy$$Sliceable$ndarray numpy$$Sliceable$ndarray$__deserialize__($Serial$state state) {
    numpy$$Sliceable$ndarray res = $DNEW(numpy$$Sliceable$ndarray,state);
    return res;
}

numpy$$ndarray numpy$$Sliceable$ndarray$__getitem__ (numpy$$Sliceable$ndarray wit, numpy$$ndarray a, $int i) {
  $list lst = $list$new(NULL, NULL);
  $list_append(lst, numpy$$ndindex$new(i));
  return a->$class->__ndgetslice__(a, lst);
}

void numpy$$Sliceable$ndarray$__setitem__ (numpy$$Sliceable$ndarray wit, numpy$$ndarray a, $int i, $WORD val) {
  fprintf(stderr,"Internal error: call to mutating method setitem on ndarray");
  exit(-1);
}

void numpy$$Sliceable$ndarray$__delitem__ (numpy$$Sliceable$ndarray wit, numpy$$ndarray a, $int i) {
  fprintf(stderr,"Internal error: call to mutating method delitem on ndarray");
  exit(-1);
}

numpy$$ndarray numpy$$Sliceable$ndarray$__getslice__ (numpy$$Sliceable$ndarray wit, numpy$$ndarray a, $Slice slc) {
  $list lst = $list$new(NULL, NULL);
  $list_append(lst, numpy$$ndslice$new(slc));
  return a->$class->__ndgetslice__(a, lst);
}

void numpy$$Sliceable$ndarray$__setslice__ (numpy$$Sliceable$ndarray wit, $Iterable wit2, numpy$$ndarray a, $Slice slc, $WORD iter) {
  fprintf(stderr,"Internal error: call to mutating method setslice on ndarray");
  exit(-1);
}

void numpy$$Sliceable$ndarray$__delslice__ (numpy$$Sliceable$ndarray wit, numpy$$ndarray a, $Slice slc) {
  fprintf(stderr,"Internal error: call to mutating method delslice on ndarray");
  exit(-1);
}

struct numpy$$Integral$ndarray numpy$$Integral$ndarray_instance;
struct numpy$$Logical$ndarray numpy$$Logical$ndarray_instance;
struct numpy$$Minus$ndarray numpy$$Minus$ndarray_instance;
struct numpy$$Sliceable$ndarray numpy$$Sliceable$ndarray_instance;

struct numpy$$Integral$ndarray$class numpy$$Integral$ndarray$methods = {
    "numpy$$Integral$ndarray",
    UNASSIGNED,
    ($Super$class)&$Integral$methods,
    numpy$$Integral$ndarray_init,
    numpy$$Integral$ndarray$__serialize__,
    numpy$$Integral$ndarray$__deserialize__,
    ($bool (*)(numpy$$Integral$ndarray))$default__bool__,
    ($str (*)(numpy$$Integral$ndarray))$default__str__,
    numpy$$Integral$ndarray$__add__,
    numpy$$Integral$ndarray$__fromatom__,
    NULL,
    numpy$$Integral$ndarray$__mul__,
    numpy$$Integral$ndarray$__truediv__,
    numpy$$Integral$ndarray$__pow__,
    numpy$$Integral$ndarray$__neg__,
    numpy$$Integral$ndarray$__pos__,
    NULL,
    NULL,
    numpy$$Integral$ndarray$__abs__,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    numpy$$Integral$ndarray$__floordiv__ ,
    NULL,
    NULL,
    NULL,
    NULL
};

struct numpy$$Logical$ndarray$class numpy$$Logical$ndarray$methods =  {
    "numpy$$Logical$ndarray",
    UNASSIGNED,
    ($Super$class)&$Logical$methods,
    numpy$$Logical$ndarray_init,
    numpy$$Logical$ndarray$__serialize__,
    numpy$$Logical$ndarray$__deserialize__,
    ($bool (*)(numpy$$Logical$ndarray))$default__bool__,
    ($str (*)(numpy$$Logical$ndarray))$default__str__,
    numpy$$Logical$ndarray$__and__,
    numpy$$Logical$ndarray$__or__,
    numpy$$Logical$ndarray$__xor__
};

struct numpy$$Logical$ndarray numpy$$Logical$ndarray_instance = {&numpy$$Logical$ndarray$methods, ($Integral)&numpy$$Integral$ndarray_instance};
numpy$$Logical$ndarray numpy$$Logical$ndarray$witness = &numpy$$Logical$ndarray_instance;

struct numpy$$Minus$ndarray$class numpy$$Minus$ndarray$methods = {
    "numpy$$Minus$ndarray",
    UNASSIGNED,
    ($Super$class)&$Minus$methods,
    numpy$$Minus$ndarray_init,
    numpy$$Minus$ndarray$__serialize__,
    numpy$$Minus$ndarray$__deserialize__,
    ($bool (*)(numpy$$Minus$ndarray))$default__bool__,
    ($str (*)(numpy$$Minus$ndarray))$default__str__,
    numpy$$Minus$ndarray$__sub__
};
struct numpy$$Minus$ndarray numpy$$Minus$ndarray_instance = {&numpy$$Minus$ndarray$methods,  ($Integral)&numpy$$Integral$ndarray_instance};
numpy$$Minus$ndarray numpy$$Minus$ndarray$witness = &numpy$$Minus$ndarray_instance;


struct numpy$$Sliceable$ndarray$class numpy$$Sliceable$ndarray$methods = {
    "numpy$$Sliceable$ndarray",
    UNASSIGNED,
    ($Super$class)&$Sliceable$methods,
    numpy$$Sliceable$ndarray$__init__,
    numpy$$Sliceable$ndarray$__serialize__,
    numpy$$Sliceable$ndarray$__deserialize__,
    ($bool (*)(numpy$$Sliceable$ndarray))$default__bool__,
    ($str (*)(numpy$$Sliceable$ndarray))$default__str__,
    numpy$$Sliceable$ndarray$__getitem__,
    numpy$$Sliceable$ndarray$__setitem__,
    numpy$$Sliceable$ndarray$__delitem__,
    numpy$$Sliceable$ndarray$__getslice__,
    numpy$$Sliceable$ndarray$__setslice__,
    numpy$$Sliceable$ndarray$__delslice__,
};
struct numpy$$Sliceable$ndarray numpy$$Sliceable$ndarray_instance = {&numpy$$Sliceable$ndarray$methods};
numpy$$Sliceable$ndarray numpy$$Sliceable$ndarray$witness = &numpy$$Sliceable$ndarray_instance;

// numpy$$Iterable$ndarray ////////////////////////////////////////////////////////


void numpy$$Iterable$ndarray$__init__(numpy$$Iterable$ndarray self, numpy$$Primitive pwit) {
  self->pwit = pwit;
}
  

void numpy$$Iterable$ndarray$__serialize__(numpy$$Iterable$ndarray wit, $Serial$state state) {
}

numpy$$Iterable$ndarray numpy$$Iterable$ndarray$__deserialize__($Serial$state state) {
    numpy$$Iterable$ndarray res = $DNEW(numpy$$Iterable$ndarray,state);
    return res;
}


$Iterator numpy$$Iterable$ndarray$__iter__(numpy$$Iterable$ndarray self, numpy$$ndarray a) {
  return ($Iterator)numpy$$Iterator$ndarray$new(self->pwit,a);
}

struct numpy$$Iterable$ndarray$class numpy$$Iterable$ndarray$methods = {
    "numpy$$Iterable$ndarray",
    UNASSIGNED,
    ($Super$class)&$Iterable$methods,
    numpy$$Iterable$ndarray$__init__,
    numpy$$Iterable$ndarray$__serialize__,
    numpy$$Iterable$ndarray$__deserialize__,
    ($bool (*)(numpy$$Iterable$ndarray))$default__bool__,
    ($str (*)(numpy$$Iterable$ndarray))$default__str__,
    numpy$$Iterable$ndarray$__iter__,
};


