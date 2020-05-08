void $int_init($int, long);
void $int_serialize($int, $Mapping$dict, long*, $dict, struct $ROWLISTHEADER*);
$int $int_deserialize($Mapping$dict, $ROW*, $dict);

struct $int$class $int$methods = {"",NULL,$int_init,$int_serialize, $int_deserialize};

// Initialization and Serialization ///////////////////////////////////////////////////////////////////////

void $int_init($int self, long val){
  self->val = val;
}

void $int_serialize($int n, $Mapping$dict notused, long *start_no, $dict done, struct $ROWLISTHEADER *accum) {
  $val_serialize(INT_ID,&n->val,start_no,accum);
}

$int $int_deserialize($Mapping$dict notused, $ROW *row, $dict done) {
  return to$int((long)$val_deserialize(row));
}

$int to$int(long i) {
  $int res = malloc(sizeof(struct $int));
  res->$class = &$int$methods;
  res->val = i;
  return res;
}

long from$int($int w) {
  return w->val;
}


// $Integral$int /////////////////////////////////////////////////////////////////////////

$bool $Integral$int$__eq__ ($Integral$int wit, $int a, $int b) {
  return to$bool(a->val == b->val);
}

$bool $Integral$int$__ne__ ($Integral$int wit, $int a, $int b) {
  return to$bool(a->val != b->val);
}

$bool $Integral$int$__lt__ ($Integral$int wit, $int a, $int b) {
  return to$bool(a->val < b->val);
}

$bool $Integral$int$__le__ ($Integral$int wit, $int a, $int b) {
  return to$bool(a->val <= b->val);
}

$bool $Integral$int$__gt__ ($Integral$int wit, $int a, $int b) {
  return to$bool(a->val > b->val);
}

$bool $Integral$int$__ge__ ($Integral$int wit, $int a, $int b) {
  return to$bool(a->val == b->val);
}

$float $Integral$int$__float__ ($Integral$int wit, $int n) {
  return to$float((double)n->val);
}

$Integral$opaque $Integral$int$__trunc__ ($Integral$int wit, $int n) {
  return $Integral$pack(($Integral)wit,n);
}
  
$Integral$opaque $Integral$int$__floor__ ($Integral$int wit, $int n) {
  return $Integral$pack(($Integral)wit,n);
}
  
$Integral$opaque $Integral$int$__ceil__ ($Integral$int wit, $int n) {
  return $Integral$pack(($Integral)wit,n);
}
  
$Integral$opaque $Integral$int$__round__ ($Integral$int wit, $int n) {
  return $Integral$pack(($Integral)wit,n);
}
  
$Integral$opaque $Integral$int$numerator ($Integral$int wit, $int n) {
  return $Integral$pack(($Integral)wit,n);
}
  
$Integral$opaque $Integral$int$denominator ($Integral$int wit, $int n) {
  return $Integral$pack(($Integral)wit,to$int(1L));
}
  
$int $Integral$int$__int__ ($Integral$int wit, $int n) {
  return n;
}

$int $Integral$int$__index__($Integral$int wit, $int n) {
  return n;
}

$tup2_t $Integral$int$__divmod__($Integral$int wit, $int a, $int b) {
  int n = from$int(a);
  int d = from$int(b);
  $tup2_t res = malloc(sizeof(struct $tup2_t));
  res->$class = &$tup2_t$methods;
  res->a = to$int(n/d);
  res->b = to$int(n%d);
  return res;
}

$int $Integral$int$__floordiv__($Integral$int wit, $int a, $int b) {
  return to$int(from$int(a) / from$int(b));
}

$int $Integral$int$__mod__($Integral$int wit, $int a, $int b) {
  return to$int(from$int(a) % from$int(b));
}

$int $Integral$int$__lshift__($Integral$int wit,  $int a, $int b) {
  return to$int(from$int(a) << from$int(b));
}

$int $Integral$int$__rshift__($Integral$int wit,  $int a, $int b) {
  return to$int(from$int(a) >> from$int(b));
}
 
$int $Integral$int$__invert__($Integral$int wit,  $int a) {
  return to$int(~from$int(a));
}


// Logical$int  ////////////////////////////////////////////////////////////////////////////////////////

$int $Logical$int$__and__($Logical$int wit,  $int a, $int b) {
  return to$int(from$int(a) & from$int(b));
}
                                                 
$int $Logical$int$__or__($Logical$int wit,  $int a, $int b) {
  return to$int(from$int(a) | from$int(b));
}
                                                 
$int $Logical$int$__xor__($Logical$int wit,  $int a, $int b) {
  return to$int(from$int(a) ^ from$int(b));
}  

// $Complex$int //////////////////////////////////////////////////////////////////////////////////////

$bool $Complex$int$__eq__ ($Complex$int wit, $int a, $int b) {
  return to$bool(a->val == b->val);
}

$bool $Complex$int$__ne__ ($Complex$int wit, $int a, $int b) {
  return to$bool(a->val != b->val);
}

$complex $Complex$int$__complx__($Complex$int wit, $int a) {
  return to$complex(to$float((double)from$int(a)),to$float(0.0));
}

$bool $Complex$int$__bool__($Complex$int wit, $int a) {
  return from$int(a)==0L ? $true : $false;
}

$int $Complex$int$__mul__($Complex$int wit,  $int a, $int b) {
  return to$int(from$int(a) * from$int(b));
}  

// The typechecker will reject true division between two integers.
$int $Complex$int$__truediv__($Complex$int wit,  $int a, $int b) {
  // raise NOTIMPLEMENTED
  return NULL;
}  

// only called with e>=0.
static int intpow(int a, int e) {
  if (e == 0) return 1;
  if (e == 1) return a;
  if (e % 2 == 0) return intpow(a*a,e/2);
  return a * intpow(a*a,e/2);
}
  
$int $Complex$int$__pow__($Complex$int wit,  $int a, $int b) {
  if ( from$int(b) < 0) {
    // raise VALUEERROR;
    return NULL;
  }
  return to$int(intpow(from$int(a),from$int(b)));
}

$int $Complex$int$__neg__($Complex$int wit,  $int a) {
  return to$int(-from$int(a));
}

$int $Complex$int$__pos__($Complex$int wit,  $int a) {
  return a;
}

$Real$opaque $Complex$int$real($Complex$int wit,  $int a) {
  return $Real$pack(($Real)wit,a);
}

$Real$opaque $Complex$int$imag($Complex$int wit,  $int a) {
  return  $Real$pack(($Real)wit,to$int(0L));
}

$Real$opaque $Complex$int$__abs__($Complex$int wit,  $int a) {
  return  $Real$pack(($Real)wit,to$int(labs(from$int(a))));
}

$int $Complex$int$__conjugate__($Complex$int wit,  $int a) {
  return a;
}

// $Plus$int  ////////////////////////////////////////////////////////////////////////////////////////

$int $Plus$int$__add__($Plus$int wit,  $int a, $int b) {
  return to$int(from$int(a) + from$int(b));
}  
 
// $Minus$int  ////////////////////////////////////////////////////////////////////////////////////////

$int $Minus$int$__sub__($Minus$int wit,  $int a, $int b) {
  return to$int(from$int(a) - from$int(b));
}  

// $Hashable$int ///////////////////////////////////////////////////////////////////////////////////////////////////////

$bool $Hashable$int$__eq__($Hashable$int wit, $int a, $int b) {
  return to$bool(a->val == b->val);
}

$bool $Hashable$int$__neq__($Hashable$int wit, $int a, $int b) {
  return to$bool(a->val != b->val);
}

$int $Hashable$int$__hash__($Hashable$int wit, $int a) {
  return to$int($int_hash(a));
}

void $Integral$int_init($Integral$int wit) {
  wit-> w$Complex$Integral = $NEW($Complex$int,wit);
  wit-> w$Logical$Integral = $NEW($Logical$int,wit);
};

void $Complex$int_init($Complex$int wit, $Integral$int w$Integral$int) {
  wit->w$Integral$int = w$Integral$int;
  wit-> w$Plus$Complex = $NEW($Plus$int,wit);
  wit-> w$Minus$Complex = $NEW($Minus$int,wit);
}

void $Logical$int_init($Logical$int wit, $Integral$int w$Integral$int) {
  wit->w$Integral$int =  w$Integral$int;
}

void $Plus$int_init($Plus$int wit, $Complex$int w$Complex$int) {
  wit->w$Complex$int =  w$Complex$int;
}

void $Minus$int_init($Minus$int wit, $Complex$int w$Complex$int) {
  wit->w$Complex$int =  w$Complex$int;
}

struct $Integral$int $Integral$int_instance;
struct $Logical$int $Logical$int_instance;
struct $Complex$int $Complex$int_instance;
struct $Plus$int $Plus$int_instance;
struct $Minus$int $Minus$int_instance;
struct $Hashable$int $Hashable$int_instance;

struct $Integral$int$class $Integral$int$methods = {"",NULL,$Integral$int_init, $Integral$int$__eq__ , $Integral$int$__ne__ , $Integral$int$__lt__ , $Integral$int$__le__ ,
                                                     $Integral$int$__gt__ , $Integral$int$__ge__ , $Integral$int$__float__ , $Integral$int$__trunc__ , $Integral$int$__floor__ ,
                                                     $Integral$int$__ceil__ , $Integral$int$__round__ , $Integral$int$numerator , $Integral$int$denominator ,
                                                     $Integral$int$__int__ , $Integral$int$__index__ , $Integral$int$__divmod__ , $Integral$int$__floordiv__ ,
                                                     $Integral$int$__mod__ , $Integral$int$__lshift__ , $Integral$int$__rshift__ , $Integral$int$__invert__};
struct $Integral$int $Integral$int_instance = {&$Integral$int$methods, &$Logical$int_instance};
$Integral$int $Integral$int$witness = &$Integral$int_instance;


struct $Logical$int$class $Logical$int$methods =  {"", NULL,$Logical$int_init, $Logical$int$__and__ , $Logical$int$__or__ , $Logical$int$__xor__};
struct $Logical$int $Logical$int_instance = {&$Logical$int$methods, &$Integral$int_instance};
$Logical$int $Logical$int$witness = &$Logical$int_instance;


struct $Complex$int$class $Complex$int$methods = {"", NULL,$Complex$int_init, $Complex$int$__eq__,$Complex$int$__ne__,$Complex$int$__complx__,
                                               $Complex$int$__bool__,$Complex$int$__mul__,$Complex$int$__truediv__,$Complex$int$__pow__,$Complex$int$__neg__,
                                               $Complex$int$__pos__,$Complex$int$real,$Complex$int$imag,$Complex$int$__abs__,$Complex$int$__conjugate__};
struct $Complex$int $Complex$int_instance = {&$Complex$int$methods, &$Integral$int_instance, &$Plus$int_instance, &$Minus$int_instance};
$Complex$int $Complex$int$witness = &$Complex$int_instance;

struct $Plus$int$class $Plus$int$methods = {"",NULL,$Plus$int_init, $Plus$int$__add__};
struct $Plus$int $Plus$int_instance = {&$Plus$int$methods, &$Complex$int_instance};
$Plus$int $Plus$int$witness = &$Plus$int_instance;

struct $Minus$int$class $Minus$int$methods = {"", NULL,$Minus$int_init, $Minus$int$__sub__};
struct $Minus$int $Minus$int_instance = {&$Minus$int$methods, &$Complex$int_instance};
$Minus$int $Minus$int$witness = &$Minus$int_instance;

struct $Hashable$int$class $Hashable$int$methods = {"", NULL,(void (*)($Hashable$int))$default__init__, $Hashable$int$__eq__,$Hashable$int$__neq__,$Hashable$int$__hash__};
struct $Hashable$int $Hashable$int_instance = {&$Hashable$int$methods};
$Hashable$int $Hashable$int$witness = &$Hashable$int_instance;

// range //////////////////////////////////////////////

/*
class range_iterator ():
    start : int
    stop : int
    step : int
    next : int
    def __init__(self, fst, snd, step):
        if snd is not None:
            self.start = fst
            self.stop = snd
        else:
            self.start = 0
            self.stop = fst
        if step is not None:
            self.step = step
        else:
            self.step = 1
        self.next = self.start
    def __next__(self):
        next = self.next
        if next < self.stop:
            self.next += self.step
            return next
        else:
            return None
*/

struct range_iterator;
typedef struct range_iterator *range_iterator;

struct range_iterator$class {
    char *GCINFO;
    $Super$class $superclass;
    void (*__init__)(range_iterator, $int, $int, $int);
    void (*__serialize__)(range_iterator, $Mapping$dict, $WORD*, $dict, struct $ROWLISTHEADER);
    range_iterator (*__deserialize__)($Mapping$dict, $ROW*, $dict);
    $int (*__next__)(range_iterator);
};
struct range_iterator {
    struct range_iterator$class *$class;
    int start;
    int stop;
    int step;
    int next;
};

void range_iterator$__init__(range_iterator self, $int fst, $int snd, $int step) {
    if (snd != $None) {
        self->start = fst->val;
        self->stop = snd->val;
    } else {
        self->start = 0;
        self->stop = fst->val;
    }
    if (step != $None) {
        self->step = step->val;
    } else {
        self->step = 1;
    }
    self->next = self->start;
}

void range_iterator$__serialize__(range_iterator self, $Mapping$dict wit, $WORD* prefix, $dict done, struct $ROWLISTHEADER accum) {
    // TBD
}

range_iterator range_iterator$__deserialize__($Mapping$dict wit, $ROW* row, $dict done) {
    // TBD
    return NULL;
}

$int range_iterator$__next__(range_iterator self) {
    int next = self->next;
    if (next < self->stop) {
        self->next += self->step;
        return to$int(next);
    } else {
        return $None;
    }
}

struct range_iterator$class range_iterator$methods = {
    "range_iterator",
    NULL,
    range_iterator$__init__,
    range_iterator$__serialize__,
    range_iterator$__deserialize__,
    range_iterator$__next__
};

$Iterator $range($int fst, $int snd, $int step) {
    return ($Iterator)$NEW(range_iterator, fst, snd, step);
}