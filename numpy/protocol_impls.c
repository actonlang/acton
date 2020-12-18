// Integral$ndarray /////////////////////////////////////////////////////////////////////////////////////////////

void numpy$$Integral$ndarray$__init__(numpy$$Integral$ndarray wit, numpy$$Primitive w$Primitive$A$numpy) {
  wit->w$Logical = ($Logical)$NEW(numpy$$Logical$ndarray,($Integral)wit);
  wit->w$Minus = ($Minus)$NEW(numpy$$Minus$ndarray,($Integral)wit);
  wit->w$Primitive$A$Integral$ndarray = w$Primitive$A$numpy;
}; 

numpy$$Integral$ndarray numpy$$Integral$ndarray$new(numpy$$Primitive pwit) {
  numpy$$Integral$ndarray res = malloc(sizeof (struct numpy$$Integral$ndarray));
  res->$class = &numpy$$Integral$ndarray$methods;
  numpy$$Integral$ndarray$__init__(res, pwit);
  return res;
}


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
  return numpy$$oper(wit->w$Primitive$A$Integral$ndarray->$class->$add,a,b);
}

numpy$$ndarray numpy$$Integral$ndarray$__fromatom__(numpy$$Integral$ndarray wit,$atom a) {
  return numpy$$fromatom(a);
}

$complex numpy$$Integral$ndarray$__complx__(numpy$$Integral$ndarray wit, numpy$$ndarray a) {
    RAISE(($BaseException)$NEW($NotImplementedError,to$str("complex not implemented for ndarray")));
    return NULL;
}

numpy$$ndarray numpy$$Integral$ndarray$__mul__(numpy$$Integral$ndarray wit, numpy$$ndarray a, numpy$$ndarray b) {
  return numpy$$oper(wit->w$Primitive$A$Integral$ndarray->$class->$mul,a,b);
}

numpy$$ndarray numpy$$Integral$ndarray$__truediv__(numpy$$Integral$ndarray wit, numpy$$ndarray a, numpy$$ndarray b) {
    return numpy$$oper(wit->w$Primitive$A$Integral$ndarray->$class->$div,a,b);
}


numpy$$ndarray numpy$$Integral$ndarray$__pow__(numpy$$Integral$ndarray wit, numpy$$ndarray a, numpy$$ndarray b) {
    return numpy$$oper(wit->w$Primitive$A$Integral$ndarray->$class->$pow,a,b);
}

numpy$$ndarray numpy$$Integral$ndarray$__neg__(numpy$$Integral$ndarray wit, numpy$$ndarray a) {
  return numpy$$func(wit->w$Primitive$A$Integral$ndarray->$class->$neg,a);
}

numpy$$ndarray numpy$$Integral$ndarray$__pos__(numpy$$Integral$ndarray wit, numpy$$ndarray a) {
  return a;
}

$WORD numpy$$Integral$ndarray$real(numpy$$Integral$ndarray wit, $Real wit2, numpy$$ndarray a) {
  return a;
}
$WORD numpy$$Integral$ndarray$imag(numpy$$Integral$ndarray wit, $Real wit2, numpy$$ndarray a);
$WORD numpy$$Integral$ndarray$__abs__(numpy$$Integral$ndarray wit, $Real wit2, numpy$$ndarray a) {
    return numpy$$func(wit->w$Primitive$A$Integral$ndarray->$class->$abs,a);
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
    return numpy$$oper(wit->w$Primitive$A$Integral$ndarray->$class->$div,a,b);
}  
numpy$$ndarray numpy$$Integral$ndarray$__mod__ (numpy$$Integral$ndarray wit, numpy$$ndarray a, numpy$$ndarray b);
numpy$$ndarray numpy$$Integral$ndarray$__lshift__ (numpy$$Integral$ndarray wit, numpy$$ndarray a, numpy$$ndarray b);
numpy$$ndarray numpy$$Integral$ndarray$__rshift__ (numpy$$Integral$ndarray wit, numpy$$ndarray a, numpy$$ndarray b);
numpy$$ndarray numpy$$Integral$ndarray$__invert__ (numpy$$Integral$ndarray wit, numpy$$ndarray a);

// Logical$ndarray //////////////////////////////////////////////////////////////////////////////

void numpy$$Logical$ndarray$__init__(numpy$$Logical$ndarray wit, $Integral w$Integral) {
  wit->w$Integral =  w$Integral;
};

numpy$$Logical$ndarray numpy$$Logical$ndarray$new($Integral w$Integral) {
  numpy$$Logical$ndarray res = malloc(sizeof (struct numpy$$Logical$ndarray));
  res->$class = &numpy$$Logical$ndarray$methods;
  numpy$$Logical$ndarray$__init__(res, w$Integral);
  return res;
}
void numpy$$Logical$ndarray$__serialize__(numpy$$Logical$ndarray wit, $Serial$state state) {
    $step_serialize(wit->w$Integral, state);
}

numpy$$Logical$ndarray numpy$$Logical$ndarray$__deserialize__($Serial$state state) {
    numpy$$Logical$ndarray res = $DNEW(numpy$$Logical$ndarray,state);
    res->w$Integral = ($Integral)$step_deserialize(state);
    return res;
}

numpy$$ndarray numpy$$Logical$ndarray$__and__ (numpy$$Logical$ndarray wit, numpy$$ndarray a, numpy$$ndarray b) {
  return numpy$$oper(((numpy$$Integral$ndarray)wit->w$Integral)->w$Primitive$A$Integral$ndarray->$class->$band,a,b);
}
numpy$$ndarray numpy$$Logical$ndarray$__or__ (numpy$$Logical$ndarray wit, numpy$$ndarray a, numpy$$ndarray b) {
    return numpy$$oper(((numpy$$Integral$ndarray)wit->w$Integral)->w$Primitive$A$Integral$ndarray->$class->$bor,a,b);
}
numpy$$ndarray numpy$$Logical$ndarray$__xor__ (numpy$$Logical$ndarray wit, numpy$$ndarray a, numpy$$ndarray b) {
    return numpy$$oper(((numpy$$Integral$ndarray)wit->w$Integral)->w$Primitive$A$Integral$ndarray->$class->$bxor,a,b);
}

// Minus$ndarray /////////////////////////////////////////////////////////////////////////////////

void numpy$$Minus$ndarray$__init__(numpy$$Minus$ndarray wit, $Integral w$Integral) {
  wit->w$Integral =  w$Integral;
};

numpy$$Minus$ndarray numpy$$Minus$ndarray$new($Integral w$Integral) {
  numpy$$Minus$ndarray res = malloc(sizeof (struct numpy$$Minus$ndarray));
  res->$class = &numpy$$Minus$ndarray$methods;
  numpy$$Minus$ndarray$__init__(res, w$Integral);
  return res;
}

void numpy$$Minus$ndarray$__serialize__(numpy$$Minus$ndarray wit, $Serial$state state) {
    $step_serialize(wit->w$Integral, state);
}

numpy$$Minus$ndarray numpy$$Minus$ndarray$__deserialize__($Serial$state state) {
    numpy$$Minus$ndarray res = $DNEW(numpy$$Minus$ndarray,state);
    res->w$Integral = ($Integral)$step_deserialize(state);
    return res;
}

numpy$$ndarray numpy$$Minus$ndarray$__sub__ (numpy$$Minus$ndarray wit, numpy$$ndarray a, numpy$$ndarray b) {
  return numpy$$oper(((numpy$$Integral$ndarray)wit->w$Integral)->w$Primitive$A$Integral$ndarray->$class->$sub,a,b);
}

// Sliceable$ndarray ///////////////////////////////////////////////////////////////////////////////

void numpy$$Sliceable$ndarray$__init__ (numpy$$Sliceable$ndarray self) {
}

void numpy$$Sliceable$ndarray$__serialize__(numpy$$Sliceable$ndarray wit, $Serial$state state) {
}

numpy$$Sliceable$ndarray numpy$$Sliceable$ndarray$new() {
    numpy$$Sliceable$ndarray res = malloc(sizeof(struct numpy$$Sliceable$ndarray));
    res->$class = &numpy$$Sliceable$ndarray$methods;
    return res;
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

numpy$$ndarray numpy$$Sliceable$ndarray$__getslice__ (numpy$$Sliceable$ndarray wit, numpy$$ndarray a, $slice slc) {
  $list lst = $list$new(NULL, NULL);
  $list_append(lst, numpy$$ndslice$new(slc));
  return a->$class->__ndgetslice__(a, lst);
}

void numpy$$Sliceable$ndarray$__setslice__ (numpy$$Sliceable$ndarray wit, numpy$$ndarray a, $Iterable wit2, $slice slc, $WORD iter) {
  fprintf(stderr,"Internal error: call to mutating method setslice on ndarray");
  exit(-1);
}

void numpy$$Sliceable$ndarray$__delslice__ (numpy$$Sliceable$ndarray wit, numpy$$ndarray a, $slice slc) {
  fprintf(stderr,"Internal error: call to mutating method delslice on ndarray");
  exit(-1);
}

struct numpy$$Integral$ndarray numpy$$Integral$instance;
struct numpy$$Logical$ndarray numpy$$Logical$instance;
struct numpy$$Minus$ndarray numpy$$Minus$instance;
struct numpy$$Sliceable$ndarray numpy$$Sliceable$instance;

struct numpy$$Integral$ndarray$class numpy$$Integral$ndarray$methods = {
    "numpy$$Integral$ndarray",
    UNASSIGNED,
    ($Super$class)&$Integral$methods,
    numpy$$Integral$ndarray$__init__,
    numpy$$Integral$ndarray$__serialize__,
    numpy$$Integral$ndarray$__deserialize__,
    ($bool (*)(numpy$$Integral$ndarray))$default__bool__,
    ($str (*)(numpy$$Integral$ndarray))$default__str__,
    numpy$$Integral$ndarray$__add__,
    (numpy$$ndarray (*)(numpy$$Integral$ndarray, numpy$$ndarray, numpy$$ndarray))$Plus$__iadd__,    
    numpy$$Integral$ndarray$__fromatom__,
    NULL,
    numpy$$Integral$ndarray$__mul__,
    numpy$$Integral$ndarray$__truediv__,
    numpy$$Integral$ndarray$__pow__,
    (numpy$$ndarray (*)(numpy$$Integral$ndarray, numpy$$ndarray, numpy$$ndarray))$Number$__imul__ ,
    (numpy$$ndarray (*)(numpy$$Integral$ndarray, numpy$$ndarray, numpy$$ndarray))$Number$__itruediv__ ,
    (numpy$$ndarray (*)(numpy$$Integral$ndarray, numpy$$ndarray, numpy$$ndarray))$Number$__ipow__ ,
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
    (numpy$$ndarray (*)(numpy$$Integral$ndarray, numpy$$ndarray, numpy$$ndarray))$Integral$__ifloordiv__,
    NULL,
    NULL,
    NULL,
    NULL
};

struct numpy$$Logical$ndarray$class numpy$$Logical$ndarray$methods =  {
    "numpy$$Logical$ndarray",
    UNASSIGNED,
    ($Super$class)&$Logical$methods,
    numpy$$Logical$ndarray$__init__,
    numpy$$Logical$ndarray$__serialize__,
    numpy$$Logical$ndarray$__deserialize__,
    ($bool (*)(numpy$$Logical$ndarray))$default__bool__,
    ($str (*)(numpy$$Logical$ndarray))$default__str__,
    numpy$$Logical$ndarray$__and__,
    numpy$$Logical$ndarray$__or__,
    numpy$$Logical$ndarray$__xor__,
    (numpy$$ndarray (*)(numpy$$Logical$ndarray, numpy$$ndarray, numpy$$ndarray))$Logical$__iand__,
    (numpy$$ndarray (*)(numpy$$Logical$ndarray, numpy$$ndarray, numpy$$ndarray))$Logical$__ior__,
    (numpy$$ndarray (*)(numpy$$Logical$ndarray, numpy$$ndarray, numpy$$ndarray))$Logical$__ixor__
};

struct numpy$$Logical$ndarray numpy$$Logical$instance = {&numpy$$Logical$ndarray$methods, ($Integral)&numpy$$Integral$instance};
numpy$$Logical$ndarray numpy$$Logical$ndarray$witness = &numpy$$Logical$instance;

struct numpy$$Minus$ndarray$class numpy$$Minus$ndarray$methods = {
    "numpy$$Minus$ndarray",
    UNASSIGNED,
    ($Super$class)&$Minus$methods,
    numpy$$Minus$ndarray$__init__,
    numpy$$Minus$ndarray$__serialize__,
    numpy$$Minus$ndarray$__deserialize__,
    ($bool (*)(numpy$$Minus$ndarray))$default__bool__,
    ($str (*)(numpy$$Minus$ndarray))$default__str__,
    numpy$$Minus$ndarray$__sub__,
    (numpy$$ndarray (*)(numpy$$Minus$ndarray, numpy$$ndarray, numpy$$ndarray))$Minus$__isub__
};
struct numpy$$Minus$ndarray numpy$$Minus$instance = {&numpy$$Minus$ndarray$methods,  ($Integral)&numpy$$Integral$instance};
numpy$$Minus$ndarray numpy$$Minus$ndarray$witness = &numpy$$Minus$instance;


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
struct numpy$$Sliceable$ndarray numpy$$Sliceable$instance = {&numpy$$Sliceable$ndarray$methods};
numpy$$Sliceable$ndarray numpy$$Sliceable$ndarray$witness = &numpy$$Sliceable$instance;

// numpy$$Iterable$ndarray ////////////////////////////////////////////////////////


void numpy$$Iterable$ndarray$__init__(numpy$$Iterable$ndarray self, numpy$$Primitive pwit) {
  self->pwit = pwit;
}
  
numpy$$Iterable$ndarray numpy$$Iterable$ndarray$new(numpy$$Primitive pwit) {
  numpy$$Iterable$ndarray res = malloc(sizeof (struct numpy$$Iterable$ndarray));
  numpy$$Iterable$ndarray$__init__(res, pwit);
  return res;
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


// numpy$$RealFuns$math$ndarray ///////////////////////////////////////////////////////////

$NoneType numpy$$RealFuns$math$ndarray$__init__ (numpy$$RealFuns$math$ndarray w$self, numpy$$Primitive w$Primitive, math$$RealFuns w$RealFuns) {
    w$self->w$Primitive$A$RealFuns$math$ndarray = w$Primitive;
    w$self-> w$RealFuns$math$A$RealFuns$math$ndarray = w$RealFuns;
    return $None;
}

$NoneType numpy$$RealFuns$math$ndarray$__serialize__(numpy$$RealFuns$math$ndarray wit, $Serial$state state) {
    $step_serialize(wit->w$Primitive$A$RealFuns$math$ndarray, state);
    $step_serialize(wit->w$RealFuns$math$A$RealFuns$math$ndarray, state);
    return $None;
}

numpy$$RealFuns$math$ndarray numpy$$RealFuns$math$ndarray$__deserialize__($Serial$state state) {
    numpy$$RealFuns$math$ndarray res = $DNEW(numpy$$RealFuns$math$ndarray,state);
    res->w$Primitive$A$RealFuns$math$ndarray = (numpy$$Primitive)$step_deserialize(state);
    res->w$RealFuns$math$A$RealFuns$math$ndarray = (math$$RealFuns)$step_deserialize(state);
    return res;
}

#define B8Fun(f,fB)  static union $Bytes8 fB(union $Bytes8 a) {union $Bytes8 res; res.d = f(a.d); return res;}

B8Fun(sqrt,sqrtB)
B8Fun(exp,expB)
B8Fun(log,logB)
B8Fun(sin,sinB)
B8Fun(cos,cosB)
B8Fun(tan,tanB)
B8Fun(asin,asinB)
B8Fun(acos,acosB)
B8Fun(atan,atanB)
B8Fun(sinh,sinhB)
B8Fun(cosh,coshB)
B8Fun(tanh,tanhB)
B8Fun(asinh,asinhB)
B8Fun(acosh,acoshB)
B8Fun(atanh,atanhB)
  
numpy$$ndarray numpy$$RealFuns$math$ndarray$sqrt(numpy$$RealFuns$math$ndarray wit, numpy$$ndarray a) {
  return numpy$$func(sqrtB,a);
}
numpy$$ndarray numpy$$RealFuns$math$ndarray$exp(numpy$$RealFuns$math$ndarray wit, numpy$$ndarray a) {
  return numpy$$func(expB,a);
}
numpy$$ndarray numpy$$RealFuns$math$ndarray$log(numpy$$RealFuns$math$ndarray wit, numpy$$ndarray a) {
  return numpy$$func(logB,a);
}
numpy$$ndarray numpy$$RealFuns$math$ndarray$sin(numpy$$RealFuns$math$ndarray wit, numpy$$ndarray a) {
  return numpy$$func(sinB,a);
}
numpy$$ndarray numpy$$RealFuns$math$ndarray$cos(numpy$$RealFuns$math$ndarray wit, numpy$$ndarray a) {
  return numpy$$func(cosB,a);
}
numpy$$ndarray numpy$$RealFuns$math$ndarray$tan(numpy$$RealFuns$math$ndarray wit, numpy$$ndarray a) {
  return numpy$$func(tanB,a);
}
numpy$$ndarray numpy$$RealFuns$math$ndarray$asin(numpy$$RealFuns$math$ndarray wit, numpy$$ndarray a) {
  return numpy$$func(asinB,a);
}
numpy$$ndarray numpy$$RealFuns$math$ndarray$acos(numpy$$RealFuns$math$ndarray wit, numpy$$ndarray a) {
  return numpy$$func(acosB,a);
}
numpy$$ndarray numpy$$RealFuns$math$ndarray$atan(numpy$$RealFuns$math$ndarray wit, numpy$$ndarray a) {
  return numpy$$func(atanB,a);
}
numpy$$ndarray numpy$$RealFuns$math$ndarray$sinh(numpy$$RealFuns$math$ndarray wit, numpy$$ndarray a) {
  return numpy$$func(sinhB,a);
}
numpy$$ndarray numpy$$RealFuns$math$ndarray$cosh(numpy$$RealFuns$math$ndarray wit, numpy$$ndarray a) {
  return numpy$$func(coshB,a);
}
numpy$$ndarray numpy$$RealFuns$math$ndarray$tanh(numpy$$RealFuns$math$ndarray wit, numpy$$ndarray a) {
  return numpy$$func(tanhB,a);
}
numpy$$ndarray numpy$$RealFuns$math$ndarray$asinh(numpy$$RealFuns$math$ndarray wit, numpy$$ndarray a) {
  return numpy$$func(asinhB,a);
}
numpy$$ndarray numpy$$RealFuns$math$ndarray$acosh(numpy$$RealFuns$math$ndarray wit, numpy$$ndarray a) {
  return numpy$$func(acoshB,a);
}
numpy$$ndarray numpy$$RealFuns$math$ndarray$atanh(numpy$$RealFuns$math$ndarray wit, numpy$$ndarray a) {
  return numpy$$func(atanhB,a);
}

                      
numpy$$RealFuns$math$ndarray numpy$$RealFuns$math$ndarray$new(numpy$$Primitive w$Primitive, math$$RealFuns w$RealFuns) {
    numpy$$RealFuns$math$ndarray $tmp = malloc(sizeof(struct numpy$$RealFuns$math$ndarray));
    $tmp->$class = &numpy$$RealFuns$math$ndarray$methods;
    numpy$$RealFuns$math$ndarray$methods.__init__($tmp, w$Primitive, w$RealFuns);
    return $tmp;
}
struct numpy$$RealFuns$math$ndarray$class numpy$$RealFuns$math$ndarray$methods = {
    "numpy$$RealFuns$math$ndarray",
    UNASSIGNED,
    ($Super$class)&math$$RealFuns$methods,
    numpy$$RealFuns$math$ndarray$__init__,
    numpy$$RealFuns$math$ndarray$__serialize__,
    numpy$$RealFuns$math$ndarray$__deserialize__,
    ($bool (*)(numpy$$RealFuns$math$ndarray))$default__bool__,
    ($str (*)(numpy$$RealFuns$math$ndarray))$default__str__,
    numpy$$RealFuns$math$ndarray$sqrt,        
    numpy$$RealFuns$math$ndarray$exp,        
    numpy$$RealFuns$math$ndarray$log,        
    numpy$$RealFuns$math$ndarray$sin,        
    numpy$$RealFuns$math$ndarray$cos,        
    numpy$$RealFuns$math$ndarray$tan,        
    numpy$$RealFuns$math$ndarray$asin,        
    numpy$$RealFuns$math$ndarray$acos,        
    numpy$$RealFuns$math$ndarray$atan,        
    numpy$$RealFuns$math$ndarray$sinh,        
    numpy$$RealFuns$math$ndarray$cosh,        
    numpy$$RealFuns$math$ndarray$tanh,        
    numpy$$RealFuns$math$ndarray$asinh,        
    numpy$$RealFuns$math$ndarray$acosh,        
    numpy$$RealFuns$math$ndarray$atanh     
};
 
