// Integral$ndarray /////////////////////////////////////////////////////////////////////////////////////////////

void $Integral$ndarray$__init__ ($Integral$ndarray);

$ndarray $Integral$ndarray$__add__($Integral$ndarray wit, $ndarray a, $ndarray b){
  return $ndarray_oper(wit->w$Primitive$A->$class->$add,a,b);
}

$ndarray $Integral$ndarray$__fromatom__($Integral$ndarray wit,$WORD atom) {
  return $ndarray_fromatom(($Super)atom);
}

$ndarray $Integral$ndarray$__mul__($Integral$ndarray wit, $ndarray a, $ndarray b) {
  return $ndarray_oper(wit->w$Primitive$A->$class->$mul,a,b);
}

$ndarray $Integral$ndarray$__truediv__($Integral$ndarray wit, $ndarray a, $ndarray b) {
    return $ndarray_oper(wit->w$Primitive$A->$class->$div,a,b);
}


$ndarray $Integral$ndarray$__pow__($Integral$ndarray wit, $ndarray a, $ndarray b) {
    return $ndarray_oper(wit->w$Primitive$A->$class->$pow,a,b);
}

$ndarray $Integral$ndarray$__neg__($Integral$ndarray wit, $ndarray a) {
  return $ndarray_func(wit->w$Primitive$A->$class->$neg,a);
}

$ndarray $Integral$ndarray$__pos__($Integral$ndarray wit, $ndarray a) {
  return a;
}

$WORD $Integral$ndarray$real($Integral$ndarray wit, $Real wit2, $ndarray a);
$WORD $Integral$ndarray$imag($Integral$ndarray wit, $Real wit2, $ndarray a);
$WORD $Integral$ndarray$__abs__($Integral$ndarray wit, $Real wit2, $ndarray a) {
    return $ndarray_func(wit->w$Primitive$A->$class->$abs,a);
}
$ndarray $Integral$ndarray$conjugate($Integral$ndarray wit, $ndarray a);
$float $Integral$ndarray$__float__ ($Integral$ndarray wit, $ndarray a);
$WORD $Integral$ndarray$__trunc__ ($Integral$ndarray wit, $Integral wit2, $ndarray a);
$WORD $Integral$ndarray$__floor__ ($Integral$ndarray wit, $Integral wit2, $ndarray a);
$WORD $Integral$ndarray$__ceil__ ($Integral$ndarray wit, $Integral wit2, $ndarray a);
$ndarray $Integral$ndarray$__round__ ($Integral$ndarray wit, $ndarray a, $ndarray b);
$WORD $Integral$ndarray$numerator ($Integral$ndarray wit, $Integral wit2, $ndarray a);
$WORD $Integral$ndarray$denominator ($Integral$ndarray wit, $Integral wit2, $ndarray a);
$ndarray $Integral$ndarray$__int__ ($Integral$ndarray wit, $ndarray a);
$ndarray $Integral$ndarray$__index__ ($Integral$ndarray wit, $ndarray a);
$tuple $Integral$ndarray$__divmod__ ($Integral$ndarray wit, $ndarray a, $ndarray b);
$ndarray $Integral$ndarray$__floordiv__ ($Integral$ndarray wit, $ndarray a, $ndarray b) {
    return $ndarray_oper(wit->w$Primitive$A->$class->$div,a,b);
}  
$ndarray $Integral$ndarray$__mod__ ($Integral$ndarray wit, $ndarray a, $ndarray b);
$ndarray $Integral$ndarray$__lshift__ ($Integral$ndarray wit, $ndarray a, $ndarray b);
$ndarray $Integral$ndarray$__rshift__ ($Integral$ndarray wit, $ndarray a, $ndarray b);
$ndarray $Integral$ndarray$__invert__ ($Integral$ndarray wit, $ndarray a);

// Logical$ndarray //////////////////////////////////////////////////////////////////////////////

void $Logical$ndarray$__init__ ($Logical$ndarray wit, $Integral$ndarray a);
$ndarray $Logical$ndarray$__and__ ($Logical$ndarray wit, $ndarray a, $ndarray b);
$ndarray $Logical$ndarray$__or__ ($Logical$ndarray wit, $ndarray a, $ndarray b);
$ndarray $Logical$ndarray$__xor__ ($Logical$ndarray wit, $ndarray a, $ndarray b);

// Minus$ndarray /////////////////////////////////////////////////////////////////////////////////


$ndarray $Minus$ndarray$__sub__ ($Minus$ndarray wit, $ndarray a, $ndarray b) {
  return $ndarray_oper(wit->w$Integral$ndarray->w$Primitive$A->$class->$sub,a,b);
}

// Initialization //////////////////////////////////////////////////////////////////////////////////

void $Integral$ndarray_init($Integral$ndarray wit, $Primitive w$Primitive$A) {
  wit->w$Logical$Integral = $NEW($Logical$ndarray,wit);
  wit->w$Minus$Integral = $NEW($Minus$ndarray,wit);
  wit->w$Primitive$A = w$Primitive$A;
};

void $Logical$ndarray_init($Logical$ndarray wit, $Integral$ndarray w$Integral$ndarray) {
  wit->w$Integral$ndarray =  w$Integral$ndarray;
};

void $Minus$ndarray_init($Minus$ndarray wit, $Integral$ndarray w$Integral$ndarray) {
  wit->w$Integral$ndarray =  w$Integral$ndarray;
};

struct $Integral$ndarray $Integral$ndarray_instance;
struct $Logical$ndarray $Logical$ndarray_instance;
struct $Minus$ndarray $Minus$ndarray_instance;

struct $Integral$ndarray$class $Integral$ndarray$methods = {"",UNASSIGNED,NULL,$Integral$ndarray_init,$Integral$ndarray$__add__,$Integral$ndarray$__fromatom__,NULL,
                                                    $Integral$ndarray$__mul__,$Integral$ndarray$__truediv__,$Integral$ndarray$__pow__,$Integral$ndarray$__neg__,
                                                    $Integral$ndarray$__pos__,NULL,NULL,$Integral$ndarray$__abs__,NULL,
                                                    NULL,NULL,NULL,
                                                    NULL,NULL,NULL,NULL,
                                                    NULL,NULL,NULL, $Integral$ndarray$__floordiv__ ,
                                                    NULL,NULL,NULL,NULL};

struct $Logical$ndarray$class $Logical$ndarray$methods =  {"", UNASSIGNED,NULL,$Logical$ndarray_init,NULL,NULL,NULL};
struct $Logical$ndarray $Logical$ndarray_instance = {&$Logical$ndarray$methods, &$Integral$ndarray_instance};
$Logical$ndarray $Logical$ndarray$witness = &$Logical$ndarray_instance;

struct $Minus$ndarray$class $Minus$ndarray$methods = {"",UNASSIGNED, NULL,$Minus$ndarray_init, $Minus$ndarray$__sub__};
struct $Minus$ndarray $Minus$ndarray_instance = {&$Minus$ndarray$methods, &$Integral$ndarray_instance};
$Minus$ndarray $Minus$ndarray$witness = &$Minus$ndarray_instance;
 
// $Iterable$ndarray ////////////////////////////////////////////////////////


void $Iterable$ndarray$__init__($Iterable$ndarray self, $Primitive pwit) {
  self->pwit = pwit;
}
  
$Iterator $Iterable$ndarray$__iter__($Iterable$ndarray self, $ndarray a) {
  return ($Iterator)$NEW($Iterator$ndarray,self->pwit,a);
}

struct $Iterable$ndarray$class  $Iterable$ndarray$methods =  {"", UNASSIGNED,NULL,$Iterable$ndarray$__init__,$Iterable$ndarray$__iter__};
