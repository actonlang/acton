/*
 * Copyright (C) 2019-2021 Data Ductus AB
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

static struct B_ZeroDivisionError B_int_zero_division_error =
    STATIC_EXCEPTION(B_ZeroDivisionError, "division by zero");
struct B_ZeroDivisionError B_int_truediv_zero_error =
    STATIC_EXCEPTION(B_ZeroDivisionError, "int truediv: division by zero");
struct B_ZeroDivisionError B_int_floordiv_zero_error =
    STATIC_EXCEPTION(B_ZeroDivisionError, "int floordiv: division by zero");
struct B_ZeroDivisionError B_int_mod_zero_error =
    STATIC_EXCEPTION(B_ZeroDivisionError, "int mod: division by zero");

// Auxiliary //////////////////////////////////////////////////////////////////////////////

// only called with e>=0.
int64_t int_pow(int64_t a, int64_t e) {
    if (e == 0) return 1;
    if (e == 1) return a;
    if (e%2 == 0) return int_pow(a*a,e/2);
    return a * int_pow(a*a,e/2);
}

// General methods ///////////////////////////////////////////////////////////////////////

int64_t B_intG_new(B_atom a, B_int base) {  // base is optional
    B_bigint b = B_bigintG_new(a, base);
    int64_t sz = b->val.size;
    uint64_t n = sz == 0 ? 0 : b->val.n[0];
    if (llabs(sz) > 1 || (sz==1 && n > INT64_MAX) || (sz == -1 && n > (UINT64_C(1) << 63))) {
        char errmsg[1024];
        snprintf(errmsg, sizeof(errmsg), "int(): value %s out of range for type int",get_str(&b->val));
        RAISE(B_ValueError, actStrFromCStringCopy(errmsg));
    }
    // Subtract before negating so INT64_MIN stays representable.
    return sz < 0 ? -(int64_t)(n - 1) - 1 : (int64_t)n;
}
 
B_NoneType B_intD___init__(B_int self, B_atom a, B_int base){
    // TODO: Move construction out of __init__ in the constructor redesign.
    // Shared integer boxes rely on that change to remain immutable.
    self->val = B_intG_new(a,base);
    return B_None;
}

void B_intD___serialize__(B_int n, $Serial$state state) {
    $val_serialize(INT_ID,&n->val,state);
}

B_int B_intD___deserialize__(B_int n, $Serial$state state) {
    return toB_int((int64_t)(intptr_t)$val_deserialize(state));
}

bool B_int_bool(B_int n) {
    return n->val != 0;
}

B_str B_int_str(B_int n) {
    return $FORMAT("%lld", n->val);
}

B_str B_int_repr(B_int n) {
    return $FORMAT("%lld", n->val);
}

// Small ints share static boxes and are treated as immutable. See the
// constructor TODO above. They contain no pointers to GC memory and need
// neither collection nor write barrier work.
#define B_INT_SMALL_MIN (-5)
static const struct B_int B_int_small[] = {
    {&B_intG_methods, -5},
    {&B_intG_methods, -4},
    {&B_intG_methods, -3},
    {&B_intG_methods, -2},
    {&B_intG_methods, -1},
    {&B_intG_methods, 0},
    {&B_intG_methods, 1},
    {&B_intG_methods, 2},
    {&B_intG_methods, 3},
    {&B_intG_methods, 4},
    {&B_intG_methods, 5},
    {&B_intG_methods, 6},
    {&B_intG_methods, 7},
    {&B_intG_methods, 8},
    {&B_intG_methods, 9},
    {&B_intG_methods, 10},
    {&B_intG_methods, 11},
    {&B_intG_methods, 12},
    {&B_intG_methods, 13},
    {&B_intG_methods, 14},
    {&B_intG_methods, 15},
    {&B_intG_methods, 16},
    {&B_intG_methods, 17},
    {&B_intG_methods, 18},
    {&B_intG_methods, 19},
    {&B_intG_methods, 20},
    {&B_intG_methods, 21},
    {&B_intG_methods, 22},
    {&B_intG_methods, 23},
    {&B_intG_methods, 24},
    {&B_intG_methods, 25},
    {&B_intG_methods, 26},
    {&B_intG_methods, 27},
    {&B_intG_methods, 28},
    {&B_intG_methods, 29},
    {&B_intG_methods, 30},
    {&B_intG_methods, 31},
    {&B_intG_methods, 32},
    {&B_intG_methods, 33},
    {&B_intG_methods, 34},
    {&B_intG_methods, 35},
    {&B_intG_methods, 36},
    {&B_intG_methods, 37},
    {&B_intG_methods, 38},
    {&B_intG_methods, 39},
    {&B_intG_methods, 40},
    {&B_intG_methods, 41},
    {&B_intG_methods, 42},
    {&B_intG_methods, 43},
    {&B_intG_methods, 44},
    {&B_intG_methods, 45},
    {&B_intG_methods, 46},
    {&B_intG_methods, 47},
    {&B_intG_methods, 48},
    {&B_intG_methods, 49},
    {&B_intG_methods, 50},
    {&B_intG_methods, 51},
    {&B_intG_methods, 52},
    {&B_intG_methods, 53},
    {&B_intG_methods, 54},
    {&B_intG_methods, 55},
    {&B_intG_methods, 56},
    {&B_intG_methods, 57},
    {&B_intG_methods, 58},
    {&B_intG_methods, 59},
    {&B_intG_methods, 60},
    {&B_intG_methods, 61},
    {&B_intG_methods, 62},
    {&B_intG_methods, 63},
    {&B_intG_methods, 64},
    {&B_intG_methods, 65},
    {&B_intG_methods, 66},
    {&B_intG_methods, 67},
    {&B_intG_methods, 68},
    {&B_intG_methods, 69},
    {&B_intG_methods, 70},
    {&B_intG_methods, 71},
    {&B_intG_methods, 72},
    {&B_intG_methods, 73},
    {&B_intG_methods, 74},
    {&B_intG_methods, 75},
    {&B_intG_methods, 76},
    {&B_intG_methods, 77},
    {&B_intG_methods, 78},
    {&B_intG_methods, 79},
    {&B_intG_methods, 80},
    {&B_intG_methods, 81},
    {&B_intG_methods, 82},
    {&B_intG_methods, 83},
    {&B_intG_methods, 84},
    {&B_intG_methods, 85},
    {&B_intG_methods, 86},
    {&B_intG_methods, 87},
    {&B_intG_methods, 88},
    {&B_intG_methods, 89},
    {&B_intG_methods, 90},
    {&B_intG_methods, 91},
    {&B_intG_methods, 92},
    {&B_intG_methods, 93},
    {&B_intG_methods, 94},
    {&B_intG_methods, 95},
    {&B_intG_methods, 96},
    {&B_intG_methods, 97},
    {&B_intG_methods, 98},
    {&B_intG_methods, 99},
    {&B_intG_methods, 100},
    {&B_intG_methods, 101},
    {&B_intG_methods, 102},
    {&B_intG_methods, 103},
    {&B_intG_methods, 104},
    {&B_intG_methods, 105},
    {&B_intG_methods, 106},
    {&B_intG_methods, 107},
    {&B_intG_methods, 108},
    {&B_intG_methods, 109},
    {&B_intG_methods, 110},
    {&B_intG_methods, 111},
    {&B_intG_methods, 112},
    {&B_intG_methods, 113},
    {&B_intG_methods, 114},
    {&B_intG_methods, 115},
    {&B_intG_methods, 116},
    {&B_intG_methods, 117},
    {&B_intG_methods, 118},
    {&B_intG_methods, 119},
    {&B_intG_methods, 120},
    {&B_intG_methods, 121},
    {&B_intG_methods, 122},
    {&B_intG_methods, 123},
    {&B_intG_methods, 124},
    {&B_intG_methods, 125},
    {&B_intG_methods, 126},
    {&B_intG_methods, 127},
    {&B_intG_methods, 128},
    {&B_intG_methods, 129},
    {&B_intG_methods, 130},
    {&B_intG_methods, 131},
    {&B_intG_methods, 132},
    {&B_intG_methods, 133},
    {&B_intG_methods, 134},
    {&B_intG_methods, 135},
    {&B_intG_methods, 136},
    {&B_intG_methods, 137},
    {&B_intG_methods, 138},
    {&B_intG_methods, 139},
    {&B_intG_methods, 140},
    {&B_intG_methods, 141},
    {&B_intG_methods, 142},
    {&B_intG_methods, 143},
    {&B_intG_methods, 144},
    {&B_intG_methods, 145},
    {&B_intG_methods, 146},
    {&B_intG_methods, 147},
    {&B_intG_methods, 148},
    {&B_intG_methods, 149},
    {&B_intG_methods, 150},
    {&B_intG_methods, 151},
    {&B_intG_methods, 152},
    {&B_intG_methods, 153},
    {&B_intG_methods, 154},
    {&B_intG_methods, 155},
    {&B_intG_methods, 156},
    {&B_intG_methods, 157},
    {&B_intG_methods, 158},
    {&B_intG_methods, 159},
    {&B_intG_methods, 160},
    {&B_intG_methods, 161},
    {&B_intG_methods, 162},
    {&B_intG_methods, 163},
    {&B_intG_methods, 164},
    {&B_intG_methods, 165},
    {&B_intG_methods, 166},
    {&B_intG_methods, 167},
    {&B_intG_methods, 168},
    {&B_intG_methods, 169},
    {&B_intG_methods, 170},
    {&B_intG_methods, 171},
    {&B_intG_methods, 172},
    {&B_intG_methods, 173},
    {&B_intG_methods, 174},
    {&B_intG_methods, 175},
    {&B_intG_methods, 176},
    {&B_intG_methods, 177},
    {&B_intG_methods, 178},
    {&B_intG_methods, 179},
    {&B_intG_methods, 180},
    {&B_intG_methods, 181},
    {&B_intG_methods, 182},
    {&B_intG_methods, 183},
    {&B_intG_methods, 184},
    {&B_intG_methods, 185},
    {&B_intG_methods, 186},
    {&B_intG_methods, 187},
    {&B_intG_methods, 188},
    {&B_intG_methods, 189},
    {&B_intG_methods, 190},
    {&B_intG_methods, 191},
    {&B_intG_methods, 192},
    {&B_intG_methods, 193},
    {&B_intG_methods, 194},
    {&B_intG_methods, 195},
    {&B_intG_methods, 196},
    {&B_intG_methods, 197},
    {&B_intG_methods, 198},
    {&B_intG_methods, 199},
    {&B_intG_methods, 200},
    {&B_intG_methods, 201},
    {&B_intG_methods, 202},
    {&B_intG_methods, 203},
    {&B_intG_methods, 204},
    {&B_intG_methods, 205},
    {&B_intG_methods, 206},
    {&B_intG_methods, 207},
    {&B_intG_methods, 208},
    {&B_intG_methods, 209},
    {&B_intG_methods, 210},
    {&B_intG_methods, 211},
    {&B_intG_methods, 212},
    {&B_intG_methods, 213},
    {&B_intG_methods, 214},
    {&B_intG_methods, 215},
    {&B_intG_methods, 216},
    {&B_intG_methods, 217},
    {&B_intG_methods, 218},
    {&B_intG_methods, 219},
    {&B_intG_methods, 220},
    {&B_intG_methods, 221},
    {&B_intG_methods, 222},
    {&B_intG_methods, 223},
    {&B_intG_methods, 224},
    {&B_intG_methods, 225},
    {&B_intG_methods, 226},
    {&B_intG_methods, 227},
    {&B_intG_methods, 228},
    {&B_intG_methods, 229},
    {&B_intG_methods, 230},
    {&B_intG_methods, 231},
    {&B_intG_methods, 232},
    {&B_intG_methods, 233},
    {&B_intG_methods, 234},
    {&B_intG_methods, 235},
    {&B_intG_methods, 236},
    {&B_intG_methods, 237},
    {&B_intG_methods, 238},
    {&B_intG_methods, 239},
    {&B_intG_methods, 240},
    {&B_intG_methods, 241},
    {&B_intG_methods, 242},
    {&B_intG_methods, 243},
    {&B_intG_methods, 244},
    {&B_intG_methods, 245},
    {&B_intG_methods, 246},
    {&B_intG_methods, 247},
    {&B_intG_methods, 248},
    {&B_intG_methods, 249},
    {&B_intG_methods, 250},
    {&B_intG_methods, 251},
    {&B_intG_methods, 252},
    {&B_intG_methods, 253},
    {&B_intG_methods, 254},
    {&B_intG_methods, 255},
    {&B_intG_methods, 256},
};

// Other boxed scalars are allocated pointer-free: the only pointer in them is
// $class, which points to a static method table. Atomic memory is not zeroed,
// so constructors must write every field and nothing may read the padding.
B_int toB_int(int64_t i) {
    uint64_t index = (uint64_t)i - (uint64_t)B_INT_SMALL_MIN;
    if (index < sizeof B_int_small / sizeof B_int_small[0])
        return (B_int)&B_int_small[index];
    B_int res = acton_malloc_atomic(sizeof(struct B_int));
    res->$class = &B_intG_methods;
    res->val = i;
    return res;
}

B_int to$int(int64_t n) {
    return toB_int(n);
}

int64_t fromB_int(B_int w) {
    return w->val;
}



// B_IntegralD_int /////////////////////////////////////////////////////////////////////////


B_int B_IntegralD_intD___add__(B_IntegralD_int wit,  B_int a, B_int b) {
    return toB_int(a->val + b->val); 
}

B_int B_IntegralD_intD___zero__(B_IntegralD_int wit) {
    return toB_int(0);
}

B_complex B_IntegralD_intD___complex__(B_IntegralD_int wit, B_int a) {
    return toB_complex((double)(a->val));
}

B_int B_IntegralD_intD___fromatom__(B_IntegralD_int wit, B_atom a) {
    return toB_int(B_intG_new(a,NULL));
}

B_int B_IntegralD_intD___mul__(B_IntegralD_int wit,  B_int a, B_int b) {
    return toB_int(a->val * b->val);
}  
  
B_int B_IntegralD_intD___pow__(B_IntegralD_int wit,  B_int a, B_int b) {
    int64_t aval = a->val;
    int64_t bval = b->val;
    if ( bval < 0) {
        char errmsg[1024];
        snprintf(errmsg, sizeof(errmsg), "int.__pow__: negative exponent %" PRId64 " ",bval);
        RAISE(B_ValueError, actStrFromCStringCopy(errmsg));
    }
    return toB_int(int_pow(aval,bval));
}

B_int B_IntegralD_intD___neg__(B_IntegralD_int wit,  B_int a) {
    return toB_int(-a->val);
}

B_int B_IntegralD_intD___pos__(B_IntegralD_int wit,  B_int a) {
    return a;
}

$WORD B_IntegralD_intD_real(B_IntegralD_int wit, B_int a, B_Real wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)(a));
}

$WORD B_IntegralD_intD_imag(B_IntegralD_int wit, B_int a, B_Real wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)toB_int(0LL));
}

$WORD B_IntegralD_intD___abs__(B_IntegralD_int wit, B_int a, B_Real wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)toB_int(llabs(a->val)));
}

B_int B_IntegralD_intD_conjugate(B_IntegralD_int wit,  B_int a) {
    return a;
}

double B_IntegralD_intD___float__ (B_IntegralD_int wit, B_int n) {
    return (double)n->val;
}

$WORD B_IntegralD_intD___trunc__ (B_IntegralD_int wit, B_int n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)n);
}
  
$WORD B_IntegralD_intD___floor__ (B_IntegralD_int wit, B_int n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)n);
}
  
$WORD B_IntegralD_intD___ceil__ (B_IntegralD_int wit, B_int n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)n);
}
  
B_int B_IntegralD_intD___round__ (B_IntegralD_int wit, B_int n, B_int p) {
    int64_t nval = n->val;
    if (nval<0)
        return  toB_int(-B_IntegralD_intD___round__(wit,toB_int(-nval),p)->val);
    int64_t pval = p==NULL ? 0 : fromB_int(p);
    if (pval>=0)
        return n;
    int64_t p10 = int_pow(10,-pval);
    int64_t res = nval/p10;
    if (nval%p10 * 2 > p10)
        res++; 
    return toB_int(res * p10);
}
  
$WORD B_IntegralD_intD_numerator (B_IntegralD_int wit, B_int n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)n);
}
  
$WORD B_IntegralD_intD_denominator (B_IntegralD_int wit, B_int n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)toB_int(1LL));
}
  
int64_t B_IntegralD_intD___int__ (B_IntegralD_int wit, B_int n) {
    return n->val;
}

int64_t B_IntegralD_intD___index__(B_IntegralD_int wit, B_int n) {
    return n->val;
}

B_tuple B_IntegralD_intD___divmod__(B_IntegralD_int wit, B_int a, B_int b) {
    int64_t n = a->val;
    int64_t d = b->val;
    return $NEWTUPLE(2, toB_int(n/d), toB_int(n%d));
}

B_int B_IntegralD_intD___floordiv__(B_IntegralD_int wit, B_int a, B_int b) {
    if (b->val == 0)
        RAISE_EXC(&B_int_zero_division_error);
    return toB_int(a->val / b->val);
}

B_int B_IntegralD_intD___mod__(B_IntegralD_int wit, B_int a, B_int b) {
    return toB_int(a->val % b->val);
}

B_int B_IntegralD_intD___lshift__(B_IntegralD_int wit,  B_int a, int64_t b) {
    return toB_int(a->val << b);
}

B_int B_IntegralD_intD___rshift__(B_IntegralD_int wit,  B_int a, int64_t b) {
    return toB_int(a->val >> b);
}
 
B_int B_IntegralD_intD___invert__(B_IntegralD_int wit,  B_int a) {
    return toB_int(~a->val);
}


// B_LogicalD_IntegralD_int  ////////////////////////////////////////////////////////////////////////////////////////

B_int B_LogicalD_IntegralD_intD___and__(B_LogicalD_IntegralD_int wit,  B_int a, B_int b) {
    return toB_int(a->val & b->val);
}
                                                 
B_int B_LogicalD_IntegralD_intD___or__(B_LogicalD_IntegralD_int wit,  B_int a, B_int b) {
    return toB_int(a->val | b->val);
}
                                                 
B_int B_LogicalD_IntegralD_intD___xor__(B_LogicalD_IntegralD_int wit,  B_int a, B_int b) {
    return toB_int(a->val ^ b->val);
}  
 
// B_MinusD_IntegralD_int  ////////////////////////////////////////////////////////////////////////////////////////

 
B_int B_MinusD_IntegralD_intD___sub__(B_MinusD_IntegralD_int wit,  B_int a, B_int b) {
    return toB_int(a->val - b->val);
}  

// B_DivD_int  ////////////////////////////////////////////////////////////////////////////////////////

 
B_float B_DivD_intD___truediv__ (B_DivD_int wit, B_int a, B_int b) {
    if (b->val == 0)
        RAISE_EXC(&B_int_zero_division_error);
    return toB_float((double)a->val/(double)b->val);
}

// B_OrdD_int  ////////////////////////////////////////////////////////////////////////////////////////

bool B_OrdD_intD___eq__ (B_OrdD_int wit, B_int a, B_int b) {
    return a->val == b->val;
}

bool B_OrdD_intD___ne__ (B_OrdD_int wit, B_int a, B_int b) {
    return a->val != b->val;
}

bool B_OrdD_intD___lt__ (B_OrdD_int wit, B_int a, B_int b) {
    return a->val < b->val;
}

bool B_OrdD_intD___le__ (B_OrdD_int wit, B_int a, B_int b) {
    return a->val <= b->val;
}

bool B_OrdD_intD___gt__ (B_OrdD_int wit, B_int a, B_int b) {
    return a->val > b->val;
}

bool B_OrdD_intD___ge__ (B_OrdD_int wit, B_int a, B_int b) {
    return a->val >= b->val;
}

// B_HashableD_int ///////////////////////////////////////////////////////////////////////////////////////////////////////

B_NoneType B_HashableD_intD_hash(B_HashableD_int wit, B_int a, B_hasher h) {
    zig_hash_wyhash_update(h->_hasher, (const uint8_t *)&(a->val), 8);
    return B_None;
}

// Show/Bool protocol entry points and legacy class-method shims.
B_DEFINE_SHOW(int)
B_DEFINE_BOOL(int)
