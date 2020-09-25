
struct $Integral$ndarray;
typedef struct $Integral$ndarray *$Integral$ndarray;

struct $Integral$ndarray$class;
typedef struct $Integral$ndarray$class *$Integral$ndarray$class;

struct $Logical$ndarray;
typedef struct $Logical$ndarray *$Logical$ndarray;

struct $Logical$ndarray$class;
typedef struct $Logical$ndarray$class *$Logical$ndarray$class;

struct $Minus$ndarray;
typedef struct $Minus$ndarray *$Minus$ndarray;

struct $Minus$ndarray$class;
typedef struct $Minus$ndarray$class *$Minus$ndarray$class;

// $Integral$ndarray ////////////////////////////////////////////////////////////

struct $Integral$ndarray {
    $Integral$ndarray$class $class;
    $Logical$ndarray w$Logical$Integral;
    $Minus$ndarray w$Minus$Integral;
    $Primitive w$Primitive$A;
};

struct $Integral$ndarray$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Integral$ndarray,$Primitive);
    $ndarray (*__add__)($Integral$ndarray, $ndarray, $ndarray);
    $ndarray (*__fromatom__)($Integral$ndarray,$WORD);
    $complex (*__complx__)($Integral$ndarray, $ndarray);
    $ndarray (*__mul__)($Integral$ndarray, $ndarray, $ndarray);
    $ndarray (*__truediv__)($Integral$ndarray, $ndarray, $ndarray);
    $ndarray (*__pow__)($Integral$ndarray, $ndarray, $ndarray);
    $ndarray (*__neg__)($Integral$ndarray, $ndarray);
    $ndarray (*__pos__)($Integral$ndarray, $ndarray);
    $WORD (*real)($Integral$ndarray, $Real, $ndarray);
    $WORD (*imag)($Integral$ndarray, $Real, $ndarray);
    $WORD (*__abs__)($Integral$ndarray, $Real, $ndarray);
    $ndarray (*conjugate)($Integral$ndarray, $ndarray);
    $float (*__float__)($Integral$ndarray, $ndarray);
    $WORD (*__trunc__)($Integral$ndarray, $Integral, $ndarray);
    $WORD (*__floor__)($Integral$ndarray, $Integral, $ndarray);
    $WORD (*__ceil__)($Integral$ndarray, $Integral, $ndarray);
    $ndarray (*__round__)($Integral$ndarray, $ndarray, $ndarray);
    $WORD (*numerator)($Integral$ndarray, $Integral, $ndarray);
    $WORD (*denominator)($Integral$ndarray, $Integral, $ndarray);
    $ndarray (*__int__)($Integral$ndarray, $ndarray);
    $ndarray (*__index__)($Integral$ndarray, $ndarray);
    $tuple (*__divmod__)($Integral$ndarray, $ndarray, $ndarray);
    $ndarray (*__floordiv__)($Integral$ndarray, $ndarray, $ndarray);
    $ndarray (*__mod__)($Integral$ndarray, $ndarray, $ndarray);
    $ndarray (*__lshift__)($Integral$ndarray, $ndarray, $ndarray);
    $ndarray (*__rshift__)($Integral$ndarray, $ndarray, $ndarray);
    $ndarray (*__invert__)($Integral$ndarray, $ndarray);
};

void $Integral$ndarray$__init__ ($Integral$ndarray);
$ndarray $Integral$ndarray$__add__($Integral$ndarray, $ndarray, $ndarray);
$ndarray $Integral$ndarray$__fromatom__($Integral$ndarray,$WORD);
$complex $Integral$ndarray$__complx__($Integral$ndarray, $ndarray);
$ndarray $Integral$ndarray$__mul__($Integral$ndarray, $ndarray, $ndarray);
$ndarray $Integral$ndarray$__truediv__($Integral$ndarray, $ndarray, $ndarray);
$ndarray $Integral$ndarray$__pow__($Integral$ndarray, $ndarray, $ndarray);
$ndarray $Integral$ndarray$__neg__($Integral$ndarray, $ndarray);
$ndarray $Integral$ndarray$__pos__($Integral$ndarray, $ndarray);
$WORD $Integral$ndarray$real($Integral$ndarray, $Real, $ndarray);
$WORD $Integral$ndarray$imag($Integral$ndarray, $Real, $ndarray);
$WORD $Integral$ndarray$__abs__($Integral$ndarray, $Real, $ndarray);
$ndarray $Integral$ndarray$conjugate($Integral$ndarray, $ndarray);
$float $Integral$ndarray$__float__ ($Integral$ndarray, $ndarray);
$WORD $Integral$ndarray$__trunc__ ($Integral$ndarray, $Integral, $ndarray);
$WORD $Integral$ndarray$__floor__ ($Integral$ndarray, $Integral, $ndarray);
$WORD $Integral$ndarray$__ceil__ ($Integral$ndarray, $Integral, $ndarray);
$ndarray $Integral$ndarray$__round__ ($Integral$ndarray, $ndarray, $ndarray);
$WORD $Integral$ndarray$numerator ($Integral$ndarray, $Integral, $ndarray);
$WORD $Integral$ndarray$denominator ($Integral$ndarray, $Integral, $ndarray);
$ndarray $Integral$ndarray$__int__ ($Integral$ndarray, $ndarray);
$ndarray $Integral$ndarray$__index__ ($Integral$ndarray, $ndarray);
$tuple $Integral$ndarray$__divmod__ ($Integral$ndarray, $ndarray, $ndarray);
$ndarray $Integral$ndarray$__floordiv__ ($Integral$ndarray, $ndarray, $ndarray);
$ndarray $Integral$ndarray$__mod__ ($Integral$ndarray, $ndarray, $ndarray);
$ndarray $Integral$ndarray$__lshift__ ($Integral$ndarray, $ndarray, $ndarray);
$ndarray $Integral$ndarray$__rshift__ ($Integral$ndarray, $ndarray, $ndarray);
$ndarray $Integral$ndarray$__invert__ ($Integral$ndarray, $ndarray);

// $Logical$ndarray ////////////////////////////////////////////////////////////

struct $Logical$ndarray {
    $Logical$ndarray$class $class;
    $Integral$ndarray w$Integral$ndarray;
};

struct $Logical$ndarray$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Logical$ndarray, $Integral$ndarray);
    $ndarray (*__and__)($Logical$ndarray, $ndarray, $ndarray);
    $ndarray (*__or__)($Logical$ndarray, $ndarray, $ndarray);
    $ndarray (*__xor__)($Logical$ndarray, $ndarray, $ndarray);
};

void $Logical$ndarray$__init__ ($Logical$ndarray, $Integral$ndarray);
$ndarray $Logical$ndarray$__and__ ($Logical$ndarray, $ndarray, $ndarray);
$ndarray $Logical$ndarray$__or__ ($Logical$ndarray, $ndarray, $ndarray);
$ndarray $Logical$ndarray$__xor__ ($Logical$ndarray, $ndarray, $ndarray);

// $Minus$ndarray ////////////////////////////////////////////////////////////

struct $Minus$ndarray {
    $Minus$ndarray$class $class;
    $Integral$ndarray w$Integral$ndarray;
};

struct $Minus$ndarray$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
  void (*__init__)($Minus$ndarray, $Integral$ndarray);
    $ndarray (*__sub__)($Minus$ndarray, $ndarray, $ndarray);
};

void $Minus$ndarray$__init__ ($Minus$ndarray, $Integral$ndarray);
$ndarray $Minus$ndarray$__sub__ ($Minus$ndarray, $ndarray, $ndarray);

// Witnesses /////////////////////////////////////////////////////////////////

extern struct $Integral$ndarray$class $Integral$ndarray$methods;
extern struct $Logical$ndarray$class $Logical$ndarray$methods;
extern struct $Minus$ndarray$class $Minus$ndarray$methods;

extern struct $Logical$ndarray *$Logical$ndarray$witness;
extern struct $Minus$ndarray *$Minus$ndarray$witness;
