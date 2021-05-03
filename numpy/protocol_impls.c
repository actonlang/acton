// numpy$$Integral$ndarray$int /////////////////////////////////////////////////////////////////////////////////////////////

void numpy$$Integral$ndarray$int$__init__(numpy$$Integral$ndarray$int wit) {
  wit->w$Logical = ($Logical)$NEW(numpy$$Logical$ndarray$int,($Integral)wit);
  wit->w$Minus = ($Minus)$NEW(numpy$$Minus$ndarray$int,($Integral)wit);
}; 

numpy$$Integral$ndarray$int numpy$$Integral$ndarray$int$new() {
  numpy$$Integral$ndarray$int res = malloc(sizeof (struct numpy$$Integral$ndarray$int));
  res->$class = &numpy$$Integral$ndarray$int$methods;
  numpy$$Integral$ndarray$int$__init__(res);
  return res;
}


void numpy$$Integral$ndarray$int$__serialize__(numpy$$Integral$ndarray$int wit, $Serial$state state) {
    $step_serialize(wit->w$Logical, state);
    $step_serialize(wit->w$Minus, state);
}

numpy$$Integral$ndarray$int numpy$$Integral$ndarray$int$__deserialize__(numpy$$Integral$ndarray$int wit, $Serial$state state) {
    numpy$$Integral$ndarray$int res = $DNEW(numpy$$Integral$ndarray$int,state);
    res->w$Logical = ($Logical)$step_deserialize(state);
    res->w$Minus = ($Minus)$step_deserialize(state);
    return res;
}

numpy$$ndarray numpy$$Integral$ndarray$int$__add__(numpy$$Integral$ndarray$int wit, numpy$$ndarray a, numpy$$ndarray b){
  return numpy$$oper(numpy$$Primitive$int$witness->$class->$add,a,b);
}

numpy$$ndarray numpy$$Integral$ndarray$int$__fromatom__(numpy$$Integral$ndarray$int wit,$atom a) {
  return numpy$$fromatom(a);
}

$complex numpy$$Integral$ndarray$int$__complx__(numpy$$Integral$ndarray$int wit, numpy$$ndarray a) {
    RAISE(($BaseException)$NEW($NotImplementedError,to$str("complex not implemented for ndarray")));
    return NULL;
}

numpy$$ndarray numpy$$Integral$ndarray$int$__mul__(numpy$$Integral$ndarray$int wit, numpy$$ndarray a, numpy$$ndarray b) {
  return numpy$$oper(numpy$$Primitive$int$witness->$class->$mul,a,b);
}

numpy$$ndarray numpy$$Integral$ndarray$int$__pow__(numpy$$Integral$ndarray$int wit, numpy$$ndarray a, numpy$$ndarray b) {
    return numpy$$oper(numpy$$Primitive$int$witness->$class->$pow,a,b);
}

numpy$$ndarray numpy$$Integral$ndarray$int$__neg__(numpy$$Integral$ndarray$int wit, numpy$$ndarray a) {
  return numpy$$func(numpy$$Primitive$int$witness->$class->$neg,a);
}

numpy$$ndarray numpy$$Integral$ndarray$int$__pos__(numpy$$Integral$ndarray$int wit, numpy$$ndarray a) {
  return a;
}

$WORD numpy$$Integral$ndarray$int$real(numpy$$Integral$ndarray$int wit, numpy$$ndarray a, $Real wit2) {
  return a;
}
$WORD numpy$$Integral$ndarray$int$imag(numpy$$Integral$ndarray$int wit, numpy$$ndarray a, $Real wit2);
$WORD numpy$$Integral$ndarray$int$__abs__(numpy$$Integral$ndarray$int wit, numpy$$ndarray a, $Real wit2) {
    return numpy$$func(numpy$$Primitive$int$witness->$class->$abs,a);
}
numpy$$ndarray numpy$$Integral$ndarray$int$conjugate(numpy$$Integral$ndarray$int wit, numpy$$ndarray a);
$float numpy$$Integral$ndarray$int$__float__ (numpy$$Integral$ndarray$int wit, numpy$$ndarray a);
$WORD numpy$$Integral$ndarray$int$__trunc__ (numpy$$Integral$ndarray$int wit, numpy$$ndarray a, $Integral wit2);
$WORD numpy$$Integral$ndarray$int$__floor__ (numpy$$Integral$ndarray$int wit, numpy$$ndarray a, $Integral wit2);
$WORD numpy$$Integral$ndarray$int$__ceil__ (numpy$$Integral$ndarray$int wit, numpy$$ndarray a, $Integral wit2);
numpy$$ndarray numpy$$Integral$ndarray$int$__round__ (numpy$$Integral$ndarray$int wit, numpy$$ndarray a, numpy$$ndarray b);
$WORD numpy$$Integral$ndarray$int$numerator (numpy$$Integral$ndarray$int wit, numpy$$ndarray a, $Integral wit2);
$WORD numpy$$Integral$ndarray$int$denominator (numpy$$Integral$ndarray$int wit, numpy$$ndarray a, $Integral wit2);
numpy$$ndarray numpy$$Integral$ndarray$int$__int__ (numpy$$Integral$ndarray$int wit, numpy$$ndarray a);
numpy$$ndarray numpy$$Integral$ndarray$int$__index__ (numpy$$Integral$ndarray$int wit, numpy$$ndarray a);
$tuple numpy$$Integral$ndarray$int$__divmod__ (numpy$$Integral$ndarray$int wit, numpy$$ndarray a, numpy$$ndarray b);
numpy$$ndarray numpy$$Integral$ndarray$int$__floordiv__ (numpy$$Integral$ndarray$int wit, numpy$$ndarray a, numpy$$ndarray b) {
  return numpy$$oper(l$floordiv,a,b);
}  
numpy$$ndarray numpy$$Integral$ndarray$int$__mod__ (numpy$$Integral$ndarray$int wit, numpy$$ndarray a, numpy$$ndarray b);
numpy$$ndarray numpy$$Integral$ndarray$int$__lshift__ (numpy$$Integral$ndarray$int wit, numpy$$ndarray a, numpy$$ndarray b);
numpy$$ndarray numpy$$Integral$ndarray$int$__rshift__ (numpy$$Integral$ndarray$int wit, numpy$$ndarray a, numpy$$ndarray b);
numpy$$ndarray numpy$$Integral$ndarray$int$__invert__ (numpy$$Integral$ndarray$int wit, numpy$$ndarray a);

// numpy$$Logical$ndarray$int //////////////////////////////////////////////////////////////////////////////

void numpy$$Logical$ndarray$int$__init__(numpy$$Logical$ndarray$int wit, $Integral w$Integral) {
  wit->w$Integral =  w$Integral;
};

numpy$$Logical$ndarray$int numpy$$Logical$ndarray$int$new($Integral w$Integral) {
  numpy$$Logical$ndarray$int res = malloc(sizeof (struct numpy$$Logical$ndarray$int));
  res->$class = &numpy$$Logical$ndarray$int$methods;
  numpy$$Logical$ndarray$int$__init__(res, w$Integral);
  return res;
}
void numpy$$Logical$ndarray$int$__serialize__(numpy$$Logical$ndarray$int wit, $Serial$state state) {
    $step_serialize(wit->w$Integral, state);
}

numpy$$Logical$ndarray$int numpy$$Logical$ndarray$int$__deserialize__(numpy$$Logical$ndarray$int wit, $Serial$state state) {
    numpy$$Logical$ndarray$int res = $DNEW(numpy$$Logical$ndarray$int,state);
    res->w$Integral = ($Integral)$step_deserialize(state);
    return res;
}

numpy$$ndarray numpy$$Logical$ndarray$int$__and__ (numpy$$Logical$ndarray$int wit, numpy$$ndarray a, numpy$$ndarray b) {
  return numpy$$oper(numpy$$Primitive$int$witness->$class->$band,a,b);
}
numpy$$ndarray numpy$$Logical$ndarray$int$__or__ (numpy$$Logical$ndarray$int wit, numpy$$ndarray a, numpy$$ndarray b) {
    return numpy$$oper(numpy$$Primitive$int$witness->$class->$bor,a,b);
}
numpy$$ndarray numpy$$Logical$ndarray$int$__xor__ (numpy$$Logical$ndarray$int wit, numpy$$ndarray a, numpy$$ndarray b) {
    return numpy$$oper(numpy$$Primitive$int$witness->$class->$bxor,a,b);
}

// numpy$$Minus$ndarray$int /////////////////////////////////////////////////////////////////////////////////

void numpy$$Minus$ndarray$int$__init__(numpy$$Minus$ndarray$int wit, $Integral w$Integral) {
  wit->w$Integral =  w$Integral;
};

numpy$$Minus$ndarray$int numpy$$Minus$ndarray$int$new($Integral w$Integral) {
  numpy$$Minus$ndarray$int res = malloc(sizeof (struct numpy$$Minus$ndarray$int));
  res->$class = &numpy$$Minus$ndarray$int$methods;
  numpy$$Minus$ndarray$int$__init__(res, w$Integral);
  return res;
}

void numpy$$Minus$ndarray$int$__serialize__(numpy$$Minus$ndarray$int wit, $Serial$state state) {
    $step_serialize(wit->w$Integral, state);
}

numpy$$Minus$ndarray$int numpy$$Minus$ndarray$int$__deserialize__(numpy$$Minus$ndarray$int wit, $Serial$state state) {
    numpy$$Minus$ndarray$int res = $DNEW(numpy$$Minus$ndarray$int,state);
    res->w$Integral = ($Integral)$step_deserialize(state);
    return res;
}

numpy$$ndarray numpy$$Minus$ndarray$int$__sub__ (numpy$$Minus$ndarray$int wit, numpy$$ndarray a, numpy$$ndarray b) {
  return numpy$$oper(numpy$$Primitive$int$witness->$class->$sub,a,b);
}

// numpy$$Real$ndarray /////////////////////////////////////////////////////////////////////////////////////////////

void numpy$$Real$ndarray$__init__(numpy$$Real$ndarray wit, numpy$$Primitive w$Primitive$A$numpy) {
  wit->w$Minus = ($Minus)$NEW(numpy$$Minus$ndarray,($Real)wit);
  wit->w$Primitive$A$Real$ndarray =  w$Primitive$A$numpy;
}; 

numpy$$Real$ndarray numpy$$Real$ndarray$new(numpy$$Primitive w$Primitive$A$numpy) {
  numpy$$Real$ndarray res = malloc(sizeof (struct numpy$$Real$ndarray));
  res->$class = &numpy$$Real$ndarray$methods;
  numpy$$Real$ndarray$__init__(res, w$Primitive$A$numpy);
  return res;
}


void numpy$$Real$ndarray$__serialize__(numpy$$Real$ndarray wit, $Serial$state state) {
    $step_serialize(wit->w$Minus, state);
}

numpy$$Real$ndarray numpy$$Real$ndarray$__deserialize__(numpy$$Real$ndarray wit, $Serial$state state) {
    numpy$$Real$ndarray res = $DNEW(numpy$$Real$ndarray,state);
    res->w$Minus = ($Minus)$step_deserialize(state);
    return res;
}

numpy$$ndarray numpy$$Real$ndarray$__add__(numpy$$Real$ndarray wit, numpy$$ndarray a, numpy$$ndarray b){
  return numpy$$oper(wit->w$Primitive$A$Real$ndarray->$class->$add,a,b);
}

numpy$$ndarray numpy$$Real$ndarray$__fromatom__(numpy$$Real$ndarray wit,$atom a) {
  return numpy$$fromatom(a);
}

$complex numpy$$Real$ndarray$__complx__(numpy$$Real$ndarray wit, numpy$$ndarray a) {
    RAISE(($BaseException)$NEW($NotImplementedError,to$str("complex not implemented for ndarray")));
    return NULL;
}

numpy$$ndarray numpy$$Real$ndarray$__mul__(numpy$$Real$ndarray wit, numpy$$ndarray a, numpy$$ndarray b) {
  return numpy$$oper(wit->w$Primitive$A$Real$ndarray->$class->$mul,a,b);
}

numpy$$ndarray numpy$$Real$ndarray$__pow__(numpy$$Real$ndarray wit, numpy$$ndarray a, numpy$$ndarray b) {
    return numpy$$oper(wit->w$Primitive$A$Real$ndarray->$class->$pow,a,b);
}

numpy$$ndarray numpy$$Real$ndarray$__neg__(numpy$$Real$ndarray wit, numpy$$ndarray a) {
  return numpy$$func(wit->w$Primitive$A$Real$ndarray->$class->$neg,a);
}

numpy$$ndarray numpy$$Real$ndarray$__pos__(numpy$$Real$ndarray wit, numpy$$ndarray a) {
  return a;
}

$WORD numpy$$Real$ndarray$real(numpy$$Real$ndarray wit, numpy$$ndarray a, $Real wit2) {
  return a;
}
$WORD numpy$$Real$ndarray$imag(numpy$$Real$ndarray wit, numpy$$ndarray a, $Real wit2);
$WORD numpy$$Real$ndarray$__abs__(numpy$$Real$ndarray wit, numpy$$ndarray a, $Real wit2) {
    return numpy$$func(wit->w$Primitive$A$Real$ndarray->$class->$abs,a);
}
numpy$$ndarray numpy$$Real$ndarray$conjugate(numpy$$Real$ndarray wit, numpy$$ndarray a);
$float numpy$$Real$ndarray$__float__ (numpy$$Real$ndarray wit, numpy$$ndarray a);
$WORD numpy$$Real$ndarray$__trunc__ (numpy$$Real$ndarray wit, numpy$$ndarray a, $Integral wit2);
$WORD numpy$$Real$ndarray$__floor__ (numpy$$Real$ndarray wit, numpy$$ndarray a, $Integral wit2);
$WORD numpy$$Real$ndarray$__ceil__ (numpy$$Real$ndarray wit, numpy$$ndarray a, $Integral wit2);
numpy$$ndarray numpy$$Real$ndarray$__round__ (numpy$$Real$ndarray wit, numpy$$ndarray a, numpy$$ndarray b);

/*
$WORD numpy$$Real$ndarray$numerator (numpy$$Real$ndarray wit, numpy$$ndarray a, $Integral wit2);
$WORD numpy$$Real$ndarray$denominator (numpy$$Real$ndarray wit, numpy$$ndarray a, $Integral wit2);
numpy$$ndarray numpy$$Real$ndarray$__int__ (numpy$$Real$ndarray wit, numpy$$ndarray a);
numpy$$ndarray numpy$$Real$ndarray$__index__ (numpy$$Real$ndarray wit, numpy$$ndarray a);
$tuple numpy$$Real$ndarray$__divmod__ (numpy$$Real$ndarray wit, numpy$$ndarray a, numpy$$ndarray b);
numpy$$ndarray numpy$$Real$ndarray$__floordiv__ (numpy$$Real$ndarray wit, numpy$$ndarray a, numpy$$ndarray b) {
  return numpy$$oper(wit->w$Primitive$A$Real$ndarray->$class->$floodiv,a,b);
}  
numpy$$ndarray numpy$$Real$ndarray$__mod__ (numpy$$Real$ndarray wit, numpy$$ndarray a, numpy$$ndarray b);
numpy$$ndarray numpy$$Real$ndarray$__lshift__ (numpy$$Real$ndarray wit, numpy$$ndarray a, numpy$$ndarray b);
numpy$$ndarray numpy$$Real$ndarray$__rshift__ (numpy$$Real$ndarray wit, numpy$$ndarray a, numpy$$ndarray b);
numpy$$ndarray numpy$$Real$ndarray$__invert__ (numpy$$Real$ndarray wit, numpy$$ndarray a);
*/
 
// numpy$$Minus$ndarray /////////////////////////////////////////////////////////////////////////////////

void numpy$$Minus$ndarray$__init__(numpy$$Minus$ndarray wit, $Real w$Real) {
  wit->w$Real =  w$Real;
};

numpy$$Minus$ndarray numpy$$Minus$ndarray$new($Real w$Real) {
  numpy$$Minus$ndarray res = malloc(sizeof (struct numpy$$Minus$ndarray));
  res->$class = &numpy$$Minus$ndarray$methods;
  numpy$$Minus$ndarray$__init__(res, w$Real);
  return res;
}

void numpy$$Minus$ndarray$__serialize__(numpy$$Minus$ndarray wit, $Serial$state state) {
    $step_serialize(wit->w$Real, state);
}

numpy$$Minus$ndarray numpy$$Minus$ndarray$__deserialize__(numpy$$Minus$ndarray wit, $Serial$state state) {
    numpy$$Minus$ndarray res = $DNEW(numpy$$Minus$ndarray,state);
    res->w$Real = ($Real)$step_deserialize(state);
    return res;
}

numpy$$ndarray numpy$$Minus$ndarray$__sub__ (numpy$$Minus$ndarray wit, numpy$$ndarray a, numpy$$ndarray b) {
  return numpy$$oper(((numpy$$Real$ndarray)wit->w$Real)-> w$Primitive$A$Real$ndarray->$class->$sub,a,b);
}

// numpy$$Div$ndarray$int /////////////////////////////////////////////////////////////////////////////////

void numpy$$Div$ndarray$int$__init__(numpy$$Div$ndarray$int wit) {
};

numpy$$Div$ndarray$int numpy$$Div$ndarray$int$new() {
  numpy$$Div$ndarray$int res = malloc(sizeof (struct numpy$$Div$ndarray$int));
  res->$class = &numpy$$Div$ndarray$int$methods;
  return res;
}

void numpy$$Div$ndarray$int$__serialize__(numpy$$Div$ndarray$int wit, $Serial$state state) {
}

numpy$$Div$ndarray$int numpy$$Div$ndarray$int$__deserialize__(numpy$$Div$ndarray$int wit, $Serial$state state) {
    numpy$$Div$ndarray$int res = $DNEW(numpy$$Div$ndarray$int,state);
    return res;
}

numpy$$ndarray numpy$$Div$ndarray$int$__truediv__ (numpy$$Div$ndarray$int wit, numpy$$ndarray a, numpy$$ndarray b) {
  numpy$$ndarray res = numpy$$oper(l$truediv,a,b);
  res->elem_type = DblType;
  return res;
}

// numpy$$Div$ndarray$float /////////////////////////////////////////////////////////////////////////////////

void numpy$$Div$ndarray$float$__init__(numpy$$Div$ndarray$float wit) {
};

numpy$$Div$ndarray$float numpy$$Div$ndarray$float$new() {
  numpy$$Div$ndarray$float res = malloc(sizeof (struct numpy$$Div$ndarray$float));
  res->$class = &numpy$$Div$ndarray$float$methods;
  return res;
}

void numpy$$Div$ndarray$float$__serialize__(numpy$$Div$ndarray$float wit, $Serial$state state) {
}

numpy$$Div$ndarray$float numpy$$Div$ndarray$float$__deserialize__(numpy$$Div$ndarray$float wit, $Serial$state state) {
    numpy$$Div$ndarray$float res = $DNEW(numpy$$Div$ndarray$float,state);
    return res;
}

numpy$$ndarray numpy$$Div$ndarray$float$__truediv__ (numpy$$Div$ndarray$float wit, numpy$$ndarray a, numpy$$ndarray b) {
  return numpy$$oper(d$truediv,a,b);
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

numpy$$Sliceable$ndarray numpy$$Sliceable$ndarray$__deserialize__(numpy$$Sliceable$ndarray wit, $Serial$state state) {
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

struct numpy$$Integral$ndarray$int numpy$$Integral$ndarray$int$instance;
struct numpy$$Logical$ndarray$int numpy$$Logical$ndarray$int$instance;
struct numpy$$Minus$ndarray$int numpy$$Minus$ndarray$int$instance;
struct numpy$$Real$ndarray numpy$$Real$ndarray$instance;
struct numpy$$Minus$ndarray numpy$$Minus$ndarray$instance;
struct numpy$$Sliceable$ndarray numpy$$Sliceable$ndarray$instance;

struct numpy$$Integral$ndarray$int$class numpy$$Integral$ndarray$int$methods = {
    "numpy$$Integral$ndarray$int",
    UNASSIGNED,
    ($Super$class)&$Integral$methods,
    numpy$$Integral$ndarray$int$__init__,
    numpy$$Integral$ndarray$int$__serialize__,
    numpy$$Integral$ndarray$int$__deserialize__,
    ($bool (*)(numpy$$Integral$ndarray$int))$default__bool__,
    ($str (*)(numpy$$Integral$ndarray$int))$default__str__,
    numpy$$Integral$ndarray$int$__add__,
    (numpy$$ndarray (*)(numpy$$Integral$ndarray$int, numpy$$ndarray, numpy$$ndarray))$Plus$__iadd__,    
    numpy$$Integral$ndarray$int$__mul__,
    (numpy$$ndarray (*)(numpy$$Integral$ndarray$int, numpy$$ndarray, numpy$$ndarray))$Times$__imul__ ,
    numpy$$Integral$ndarray$int$__fromatom__,
    NULL,
    numpy$$Integral$ndarray$int$__pow__,
    (numpy$$ndarray (*)(numpy$$Integral$ndarray$int, numpy$$ndarray, numpy$$ndarray))$Number$__ipow__ ,
    numpy$$Integral$ndarray$int$__neg__,
    numpy$$Integral$ndarray$int$__pos__,
    NULL,
    NULL,
    numpy$$Integral$ndarray$int$__abs__,
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
    numpy$$Integral$ndarray$int$__floordiv__ ,
    NULL,
    NULL,
    NULL,
    (numpy$$ndarray (*)(numpy$$Integral$ndarray$int, numpy$$ndarray, numpy$$ndarray))$Integral$__ifloordiv__,
    NULL,
    NULL,
    NULL,
    NULL
};

 struct numpy$$Integral$ndarray$int numpy$$Integral$ndarray$int$instance = {&numpy$$Integral$ndarray$int$methods,
                                                                            ($Logical)&numpy$$Logical$ndarray$int$instance,  ($Minus)&numpy$$Minus$ndarray$int$instance};
numpy$$Integral$ndarray$int numpy$$Integral$ndarray$int$witness = &numpy$$Integral$ndarray$int$instance;

struct numpy$$Logical$ndarray$int$class numpy$$Logical$ndarray$int$methods =  {
    "numpy$$Logical$ndarray$int",
    UNASSIGNED,
    ($Super$class)&$Logical$methods,
    numpy$$Logical$ndarray$int$__init__,
    numpy$$Logical$ndarray$int$__serialize__,
    numpy$$Logical$ndarray$int$__deserialize__,
    ($bool (*)(numpy$$Logical$ndarray$int))$default__bool__,
    ($str (*)(numpy$$Logical$ndarray$int))$default__str__,
    numpy$$Logical$ndarray$int$__and__,
    numpy$$Logical$ndarray$int$__or__,
    numpy$$Logical$ndarray$int$__xor__,
    (numpy$$ndarray (*)(numpy$$Logical$ndarray$int, numpy$$ndarray, numpy$$ndarray))$Logical$__iand__,
    (numpy$$ndarray (*)(numpy$$Logical$ndarray$int, numpy$$ndarray, numpy$$ndarray))$Logical$__ior__,
    (numpy$$ndarray (*)(numpy$$Logical$ndarray$int, numpy$$ndarray, numpy$$ndarray))$Logical$__ixor__
};

struct numpy$$Logical$ndarray$int numpy$$Logical$ndarray$int$instance = {&numpy$$Logical$ndarray$int$methods, ($Integral)&numpy$$Integral$ndarray$int$instance};
numpy$$Logical$ndarray$int numpy$$Logical$ndarray$int$witness = &numpy$$Logical$ndarray$int$instance;

struct numpy$$Minus$ndarray$int$class numpy$$Minus$ndarray$int$methods = {
    "numpy$$Minus$ndarray$int",
    UNASSIGNED,
    ($Super$class)&$Minus$methods,
    numpy$$Minus$ndarray$int$__init__,
    numpy$$Minus$ndarray$int$__serialize__,
    numpy$$Minus$ndarray$int$__deserialize__,
    ($bool (*)(numpy$$Minus$ndarray$int))$default__bool__,
    ($str (*)(numpy$$Minus$ndarray$int))$default__str__,
    numpy$$Minus$ndarray$int$__sub__,
    (numpy$$ndarray (*)(numpy$$Minus$ndarray$int, numpy$$ndarray, numpy$$ndarray))$Minus$__isub__
};
struct numpy$$Minus$ndarray$int numpy$$Minus$ndarray$int$instance = {&numpy$$Minus$ndarray$int$methods,  ($Integral)&numpy$$Integral$ndarray$int$instance};
numpy$$Minus$ndarray$int numpy$$Minus$ndarray$int$witness = &numpy$$Minus$ndarray$int$instance;

struct numpy$$Real$ndarray$class numpy$$Real$ndarray$methods = {
    "numpy$$Real$ndarray",
    UNASSIGNED,
    ($Super$class)&$Integral$methods,
    numpy$$Real$ndarray$__init__,
    numpy$$Real$ndarray$__serialize__,
    numpy$$Real$ndarray$__deserialize__,
    ($bool (*)(numpy$$Real$ndarray))$default__bool__,
    ($str (*)(numpy$$Real$ndarray))$default__str__,
    numpy$$Real$ndarray$__add__,
    (numpy$$ndarray (*)(numpy$$Real$ndarray, numpy$$ndarray, numpy$$ndarray))$Plus$__iadd__,    
    numpy$$Real$ndarray$__mul__,
    (numpy$$ndarray (*)(numpy$$Real$ndarray, numpy$$ndarray, numpy$$ndarray))$Times$__imul__ ,
    numpy$$Real$ndarray$__fromatom__,
    NULL,
    numpy$$Real$ndarray$__pow__,
    (numpy$$ndarray (*)(numpy$$Real$ndarray, numpy$$ndarray, numpy$$ndarray))$Number$__ipow__ ,
    numpy$$Real$ndarray$__neg__,
    numpy$$Real$ndarray$__pos__,
    NULL,
    NULL,
    numpy$$Real$ndarray$__abs__,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
};
 
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

struct numpy$$Minus$ndarray numpy$$Minus$ndarray$instance = {&numpy$$Minus$ndarray$methods,  ($Real)&numpy$$Real$ndarray$instance};
numpy$$Minus$ndarray numpy$$Minus$ndarray$witness = &numpy$$Minus$ndarray$instance;

struct numpy$$Div$ndarray$int$class numpy$$Div$ndarray$int$methods = {
    "numpy$$Div$ndarray$int",
    UNASSIGNED,
    ($Super$class)&$Minus$methods,
    numpy$$Div$ndarray$int$__init__,
    numpy$$Div$ndarray$int$__serialize__,
    numpy$$Div$ndarray$int$__deserialize__,
    ($bool (*)(numpy$$Div$ndarray$int))$default__bool__,
    ($str (*)(numpy$$Div$ndarray$int))$default__str__,
    numpy$$Div$ndarray$int$__truediv__,
    (numpy$$ndarray (*)(numpy$$Div$ndarray$int, numpy$$ndarray, numpy$$ndarray))$Div$__itruediv__
};

struct numpy$$Div$ndarray$int numpy$$Div$ndarray$int$instance = {&numpy$$Div$ndarray$int$methods};
numpy$$Div$ndarray$int numpy$$Div$ndarray$int$witness = &numpy$$Div$ndarray$int$instance;

struct numpy$$Div$ndarray$float$class numpy$$Div$ndarray$float$methods = {
    "numpy$$Div$ndarray$float",
    UNASSIGNED,
    ($Super$class)&$Minus$methods,
    numpy$$Div$ndarray$float$__init__,
    numpy$$Div$ndarray$float$__serialize__,
    numpy$$Div$ndarray$float$__deserialize__,
    ($bool (*)(numpy$$Div$ndarray$float))$default__bool__,
    ($str (*)(numpy$$Div$ndarray$float))$default__str__,
    numpy$$Div$ndarray$float$__truediv__,
    (numpy$$ndarray (*)(numpy$$Div$ndarray$float, numpy$$ndarray, numpy$$ndarray))$Div$__itruediv__
};

struct numpy$$Div$ndarray$float numpy$$Div$ndarray$float$instance = {&numpy$$Div$ndarray$float$methods};
numpy$$Div$ndarray$float numpy$$Div$ndarray$float$witness = &numpy$$Div$ndarray$float$instance;

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

// numpy$$Collection$ndarray ////////////////////////////////////////////////////////


void numpy$$Collection$ndarray$__init__(numpy$$Collection$ndarray self, numpy$$Primitive pwit) {
  self->pwit = pwit;
}
  
numpy$$Collection$ndarray numpy$$Collection$ndarray$new(numpy$$Primitive pwit) {
  numpy$$Collection$ndarray res = malloc(sizeof (struct numpy$$Collection$ndarray));
  res->$class = &numpy$$Collection$ndarray$methods;
  numpy$$Collection$ndarray$__init__(res, pwit);
  return res;
}

void numpy$$Collection$ndarray$__serialize__(numpy$$Collection$ndarray wit, $Serial$state state) {
}

numpy$$Collection$ndarray numpy$$Collection$ndarray$__deserialize__(numpy$$Collection$ndarray wit, $Serial$state state) {
    numpy$$Collection$ndarray res = $DNEW(numpy$$Collection$ndarray,state);
    return res;
}


$Iterator numpy$$Collection$ndarray$__iter__(numpy$$Collection$ndarray self, numpy$$ndarray a) {
  return ($Iterator)numpy$$Iterator$ndarray$new(self->pwit,a);
}

numpy$$ndarray numpy$$Collection$ndarray$__fromiter__(numpy$$Collection$ndarray wit, $Iterable iter) {
  return NULL;
}
$int numpy$$Collection$ndarray$__len__(numpy$$Collection$ndarray wit, numpy$$ndarray a) {
  return $list_getitem(a->shape,-1);
}

struct numpy$$Collection$ndarray$class numpy$$Collection$ndarray$methods = {
    "numpy$$Collection$ndarray",
    UNASSIGNED,
    ($Super$class)&$Collection$methods,
    numpy$$Collection$ndarray$__init__,
    numpy$$Collection$ndarray$__serialize__,
    numpy$$Collection$ndarray$__deserialize__,
    ($bool (*)(numpy$$Collection$ndarray))$default__bool__,
    ($str (*)(numpy$$Collection$ndarray))$default__str__,
    numpy$$Collection$ndarray$__iter__,
    numpy$$Collection$ndarray$__fromiter__,
    numpy$$Collection$ndarray$__len__
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

numpy$$RealFuns$math$ndarray numpy$$RealFuns$math$ndarray$__deserialize__(numpy$$RealFuns$math$ndarray wit, $Serial$state state) {
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
 
