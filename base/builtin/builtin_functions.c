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

#include <stdarg.h>

// print //////////////////////////////////////////////////////////////////////////////

B_NoneType B_print(B_Show show_args, B_tuple t, B_str sep_arg, B_str end_arg, bool stderr_arg, bool flush_arg) {
    // The row bound in print's type guarantees structural tuple evidence.
    // Its component policies are also preserved by generic *args forwarding.
    B_tuple witnesses = (($ShowTuple)show_args)->W_Show;
    FILE *outfd = stdout;
    if (stderr_arg) {
        outfd = stderr;
    }
    B_str sep = actStrFromCString(" ");
    if (sep_arg)
        sep = sep_arg;
    B_str end = actStrFromCString("\n");
    if (end_arg)
        end = end_arg;

    // Write to temporary buffer first, making us much less prone to interleaved
    // output from multiple threads. It costs a malloc and some copies but print
    // should not be used in performance critical code. Each element's str is
    // taken once, since taking it again can give a different str, and the
    // buffer is written by length, since a str can contain NUL.
    B_str *strs = acton_malloc(t->size * sizeof(B_str));
    int64_t tlen = end->nbytes;
    for (int i=0; i<t->size; i++) {
        B_Show w = (B_Show)witnesses->components[i];
        strs[i] = w->$class->__str__(w, t->components[i]);
        tlen += strs[i]->nbytes + (i > 0 ? sep->nbytes : 0);
    }
    char *s = acton_malloc_atomic(tlen);
    int64_t pos = 0;
    for (int i=0; i<t->size; i++) {
        if (i > 0) {
            memcpy(s+pos, sep->str, sep->nbytes);
            pos += sep->nbytes;
        }
        memcpy(s+pos, strs[i]->str, strs[i]->nbytes);
        pos += strs[i]->nbytes;
    }
    memcpy(s+pos, end->str, end->nbytes);
    pos += end->nbytes;
    fwrite(s, 1, pos, outfd);

    if (flush_arg)
        fflush(outfd);
    return B_None;
}

// enumerate //////////////////////////////////////////////////////////////////////////
/*
void B_IteratorD_enumerate_init(B_IteratorD_enumerate self, B_Iterator it, B_int n) {
    self->it = it;
    self->nxt = fromB_int(n);
}

bool B_IteratorD_enumerate_bool(B_IteratorD_enumerate self) {
    return true;
}

B_str B_IteratorD_enumerate_str(B_IteratorD_enumerate self) {
    return $FORMAT("<enumerate iterator object at %p>", self);
}

void B_IteratorD_enumerate_serialize(B_IteratorD_enumerate self,$Serial$state state) {
    $step_serialize(self->it,state);
    $step_serialize(toB_int(self->nxt),state);
}

B_IteratorD_enumerate B_IteratorD_enumerate$_deserialize(B_IteratorD_enumerate res,$Serial$state state) {
    if (!res)
        res = $DNEW(B_IteratorD_enumerate,state);
    ACTON_STORE(res, res->it, $step_deserialize(state));
    res->nxt = fromB_int((B_int)$step_deserialize(state));
    return res;
}

$WORD B_IteratorD_enumerate_next(B_IteratorD_enumerate it) {
    $WORD w = it->it->$class->__next__(it->it);
    return $NEWTUPLE(2,toB_int(it->nxt++),w);
}

struct B_IteratorD_enumerateG_class B_IteratorD_enumerateG_methods = {"B_IteratorD_enumerate",UNASSIGNED,($SuperG_class)&B_IteratorG_methods,B_IteratorD_enumerate_init,
                                                                B_IteratorD_enumerate_serialize, B_IteratorD_enumerate$_deserialize, 
                                                                B_IteratorD_enumerate_bool,B_IteratorD_enumerate_str,B_IteratorD_enumerate_str, B_IteratorD_enumerate_next
};


B_IteratorD_enumerate B_IteratorD_enumerateG_new(B_Iterator it, B_int n) {
    return $NEW(B_IteratorD_enumerate, it, n);
}

B_Iterator B_enumerate(B_Iterable wit, $WORD iter, B_int start) {
    B_Iterator it = wit->$class->__iter__(wit,iter);
    if (!start)
        start = toB_int(0);
    return (B_Iterator)B_IteratorD_enumerateG_new(it,start); 
}
*/

B_list B_sorted(B_Ord wit, B_Iterable wit2, $WORD iter) {
    B_CollectionD_SequenceD_list w = B_CollectionD_SequenceD_listG_witness;
    B_list res = w->$class->__fromiter__(w, wit2, iter);
    B_tim_sort(wit, res->data, res->length);
    return res;
}

// EqOpt //////////////////////////////////////////////////////

extern struct $EqOptG_class $EqOptG_methods;

void $EqOptD___init__($EqOpt wit, B_Eq W_Eq$A) {
    wit->W_Eq$A = W_Eq$A;
}

bool $EqOptD_bool($EqOpt self) {
    return true;
}

B_str $EqOptD_str($EqOpt self) {
    return $FORMAT("<EqOpt witness at %p>", self);
}

void $EqOptD_serialize($EqOpt self,$Serial$state state) {
    $step_serialize(self->W_Eq$A,state);
}

$EqOpt $EqOptD_deserialize($EqOpt res, $Serial$state state) {
    if (!res)
        res = $DNEW($EqOpt,state);
    ACTON_STORE(res, res->W_Eq$A, $step_deserialize(state));
    return res;
}

bool $EqOptD___eq__($EqOpt wit, $WORD a, $WORD b) {
    if (a && b) {
        return wit->W_Eq$A->$class->__eq__(wit->W_Eq$A, a, b);
    }
    return (!a && !b) ? true : false;
}

bool $EqOptD___ne__($EqOpt wit, $WORD a, $WORD b) {
    if (a && b)
        return wit->W_Eq$A->$class->__ne__(wit->W_Eq$A, a, b);
    return (!a && !b) ? false : true;
}

struct $EqOptG_class $EqOptG_methods = {"$EqOpt", UNASSIGNED, NULL, $EqOptD___init__, $EqOptD_serialize, $EqOptD_deserialize, $EqOptD___eq__, $EqOptD___ne__};


$EqOpt $EqOptG_new(B_Eq W_Eq$A) {
    return $NEW($EqOpt, W_Eq$A);
}


// EqTuple //////////////////////////////////////////////////////

// The tuple witnesses hold one witness per tuple component, in component
// order. Component-wise equality is shared by all three: the Eq slots sit at
// the same offsets in the Eq, Ord and Hashable witness class structs.

static bool $tupleWitD___eq__(B_tuple wits, $WORD a, $WORD b) {
    if (a == b)
        return true;
    B_tuple ta = (B_tuple)a;
    B_tuple tb = (B_tuple)b;
    for (int i = 0; i < wits->size; i++) {
        B_Eq w = (B_Eq)wits->components[i];
        if (!w->$class->__eq__(w, ta->components[i], tb->components[i]))
            return false;
    }
    return true;
}

void $EqTupleD___init__($EqTuple wit, B_tuple W_Eq) {
    wit->W_Eq = W_Eq;
}

bool $EqTupleD_bool($EqTuple self) {
    return true;
}

B_str $EqTupleD_str($EqTuple self) {
    return $FORMAT("<EqTuple witness at %p>", self);
}

void $EqTupleD_serialize($EqTuple self,$Serial$state state) {
    $step_serialize(self->W_Eq,state);
}

$EqTuple $EqTupleD_deserialize($EqTuple res, $Serial$state state) {
    if (!res)
        res = $DNEW($EqTuple,state);
    ACTON_STORE(res, res->W_Eq, $step_deserialize(state));
    return res;
}

bool $EqTupleD___eq__($EqTuple wit, $WORD a, $WORD b) {
    return $tupleWitD___eq__(wit->W_Eq, a, b);
}

bool $EqTupleD___ne__($EqTuple wit, $WORD a, $WORD b) {
    return !$tupleWitD___eq__(wit->W_Eq, a, b);
}

struct $EqTupleG_class $EqTupleG_methods = {"$EqTuple", UNASSIGNED, NULL, $EqTupleD___init__, $EqTupleD_serialize, $EqTupleD_deserialize, $EqTupleD___eq__, $EqTupleD___ne__};


$EqTuple $EqTupleG_new(B_tuple W_Eq) {
    return $NEW($EqTuple, W_Eq);
}


// OrdTuple //////////////////////////////////////////////////////

extern struct $OrdTupleG_class $OrdTupleG_methods;

// W_Ord holds one Ord witness per tuple component, in component order.
// Comparison is lexicographic; operands always share one type, so sizes agree.

void $OrdTupleD___init__($OrdTuple wit, B_tuple W_Ord) {
    wit->W_Ord = W_Ord;
}

bool $OrdTupleD_bool($OrdTuple self) {
    return true;
}

B_str $OrdTupleD_str($OrdTuple self) {
    return $FORMAT("<OrdTuple witness at %p>", self);
}

void $OrdTupleD_serialize($OrdTuple self,$Serial$state state) {
    $step_serialize(self->W_Ord,state);
}

$OrdTuple $OrdTupleD_deserialize($OrdTuple res, $Serial$state state) {
    if (!res)
        res = $DNEW($OrdTuple,state);
    ACTON_STORE(res, res->W_Ord, $step_deserialize(state));
    return res;
}

bool $OrdTupleD___eq__($OrdTuple wit, $WORD a, $WORD b) {
    return $tupleWitD___eq__(wit->W_Ord, a, b);
}

bool $OrdTupleD___ne__($OrdTuple wit, $WORD a, $WORD b) {
    return !$tupleWitD___eq__(wit->W_Ord, a, b);
}

// Lexicographic comparison: the first component that is not equal decides;
// all-equal tuples satisfy only the non-strict comparisons.
static bool $tupleWitD_lex(B_tuple wits, $WORD a, $WORD b, bool strict) {
    B_tuple ta = (B_tuple)a;
    B_tuple tb = (B_tuple)b;
    for (int i = 0; i < wits->size; i++) {
        B_Ord w = (B_Ord)wits->components[i];
        if (w->$class->__lt__(w, ta->components[i], tb->components[i]))
            return true;
        if (w->$class->__ne__(w, ta->components[i], tb->components[i]))
            return false;
    }
    return !strict;
}

bool $OrdTupleD___lt__($OrdTuple wit, $WORD a, $WORD b) {
    return $tupleWitD_lex(wit->W_Ord, a, b, true);
}

bool $OrdTupleD___le__($OrdTuple wit, $WORD a, $WORD b) {
    return $tupleWitD_lex(wit->W_Ord, a, b, false);
}

bool $OrdTupleD___gt__($OrdTuple wit, $WORD a, $WORD b) {
    return $OrdTupleD___lt__(wit, b, a);
}

bool $OrdTupleD___ge__($OrdTuple wit, $WORD a, $WORD b) {
    return $OrdTupleD___le__(wit, b, a);
}

struct $OrdTupleG_class $OrdTupleG_methods = {"$OrdTuple", UNASSIGNED, NULL, $OrdTupleD___init__, $OrdTupleD_serialize, $OrdTupleD_deserialize, $OrdTupleD___eq__, $OrdTupleD___ne__,
                                               $OrdTupleD___lt__, $OrdTupleD___le__, $OrdTupleD___gt__, $OrdTupleD___ge__
};


$OrdTuple $OrdTupleG_new(B_tuple W_Ord) {
    return $NEW($OrdTuple, W_Ord);
}


// HashableTuple //////////////////////////////////////////////////////

extern struct $HashableTupleG_class $HashableTupleG_methods;

// W_Hashable holds one Hashable witness per tuple component, in component order.

void $HashableTupleD___init__($HashableTuple wit, B_tuple W_Hashable) {
    wit->W_Hashable = W_Hashable;
}

bool $HashableTupleD_bool($HashableTuple self) {
    return true;
}

B_str $HashableTupleD_str($HashableTuple self) {
    return $FORMAT("<HashableTuple witness at %p>", self);
}

void $HashableTupleD_serialize($HashableTuple self,$Serial$state state) {
    $step_serialize(self->W_Hashable,state);
}

$HashableTuple $HashableTupleD_deserialize($HashableTuple res, $Serial$state state) {
    if (!res)
        res = $DNEW($HashableTuple,state);
    ACTON_STORE(res, res->W_Hashable, $step_deserialize(state));
    return res;
}

bool $HashableTupleD___eq__($HashableTuple wit, $WORD a, $WORD b) {
    return $tupleWitD___eq__(wit->W_Hashable, a, b);
}

bool $HashableTupleD___ne__($HashableTuple wit, $WORD a, $WORD b) {
    return !$tupleWitD___eq__(wit->W_Hashable, a, b);
}

B_NoneType $HashableTupleD_hash($HashableTuple wit, $WORD a, B_hasher h) {
    B_tuple wits = wit->W_Hashable;
    B_tuple ta = (B_tuple)a;
    for (int i = 0; i < wits->size; i++) {
        // We hash each component, followed by the number of bytes it
        // contributed, so component boundaries cannot shift: without this,
        // ("ab", "c") and ("a", "bc") would hash equal.
        B_Hashable w = (B_Hashable)wits->components[i];
        uint64_t before = zig_hash_wyhash_total_len(h->_hasher);
        w->$class->hash(w, ta->components[i], h);
        uint64_t n = zig_hash_wyhash_total_len(h->_hasher) - before;
        zig_hash_wyhash_update(h->_hasher, (const uint8_t *)&n, sizeof(n));
    }
    return B_None;
}

struct $HashableTupleG_class $HashableTupleG_methods = {"$HashableTuple", UNASSIGNED, NULL, $HashableTupleD___init__, $HashableTupleD_serialize,
                                                         $HashableTupleD_deserialize,
                                                         $HashableTupleD___eq__, $HashableTupleD___ne__, $HashableTupleD_hash
};


$HashableTuple $HashableTupleG_new(B_tuple W_Hashable) {
    return $NEW($HashableTuple, W_Hashable);
}


// wEqNone //////////////////////////////////////////////////////

// The Eq witness for None: None is the only value of its type.

static bool $wEqNoneD___eq__(B_Eq wit, $WORD a, $WORD b) {
    return true;
}

static bool $wEqNoneD___ne__(B_Eq wit, $WORD a, $WORD b) {
    return false;
}

static void $wEqNoneD_serialize(B_Eq self, $Serial$state state) {
}

static struct B_Eq $wEqNone_instance;

static B_Eq $wEqNoneD_deserialize(B_Eq res, $Serial$state state) {
    if (!res) {
        res = &$wEqNone_instance;
        B_dictD_setitem(state->done, toB_int(state->row_no-1), res);
    }
    return res;
}

struct B_EqG_class $wEqNoneG_class = {"$wEqNone", UNASSIGNED, NULL,
    (B_NoneType (*)(B_Eq))$default__init__, $wEqNoneD_serialize, $wEqNoneD_deserialize,
    $wEqNoneD___eq__, $wEqNoneD___ne__
};

static struct B_Eq $wEqNone_instance = {&$wEqNoneG_class};

B_Eq $wEqNone = &$wEqNone_instance;


// Show and Bool for optional values and tuples //////////////////////////////

// Optional witnesses preserve the supplied policy for the present value.
// None is represented by NULL; it must never reach an element witness.

static void $ShowOptD___init__($ShowOpt self, B_Show witness) {
    self->W_Show = witness;
}

static void $ShowOptD_serialize($ShowOpt self, $Serial$state state) {
    $step_serialize(self->W_Show, state);
}

static $ShowOpt $ShowOptD_deserialize($ShowOpt self, $Serial$state state) {
    if (!self)
        self = $DNEW($ShowOpt, state);
    ACTON_STORE(self, self->W_Show, $step_deserialize(state));
    return self;
}

static B_str $ShowOptD___str__($ShowOpt self, $WORD value) {
    B_Show w = self->W_Show;
    return value ? w->$class->__str__(w, value) : B_None_str;
}

static B_str $ShowOptD___repr__($ShowOpt self, $WORD value) {
    B_Show w = self->W_Show;
    return value ? w->$class->__repr__(w, value) : B_None_str;
}

struct $ShowOptG_class $ShowOptG_methods = {
    .$GCINFO = "$ShowOpt",
    .$class_id = UNASSIGNED,
    .__init__ = $ShowOptD___init__,
    .__serialize__ = $ShowOptD_serialize,
    .__deserialize__ = $ShowOptD_deserialize,
    .__str__ = $ShowOptD___str__,
    .__repr__ = $ShowOptD___repr__
};

$ShowOpt $ShowOptG_new(B_Show witness) {
    return $NEW($ShowOpt, witness);
}

static void $BoolOptD___init__($BoolOpt self, B_Bool witness) {
    self->W_Bool = witness;
}

static void $BoolOptD_serialize($BoolOpt self, $Serial$state state) {
    $step_serialize(self->W_Bool, state);
}

static $BoolOpt $BoolOptD_deserialize($BoolOpt self, $Serial$state state) {
    if (!self)
        self = $DNEW($BoolOpt, state);
    ACTON_STORE(self, self->W_Bool, $step_deserialize(state));
    return self;
}

static bool $BoolOptD___bool__($BoolOpt self, $WORD value) {
    B_Bool w = self->W_Bool;
    return value && w->$class->__bool__(w, value);
}

struct $BoolOptG_class $BoolOptG_methods = {
    .$GCINFO = "$BoolOpt",
    .$class_id = UNASSIGNED,
    .__init__ = $BoolOptD___init__,
    .__serialize__ = $BoolOptD_serialize,
    .__deserialize__ = $BoolOptD_deserialize,
    .__bool__ = $BoolOptD___bool__
};

$BoolOpt $BoolOptG_new(B_Bool witness) {
    return $NEW($BoolOpt, witness);
}

static void $ShowTupleD___init__($ShowTuple self, B_tuple witness) {
    self->W_Show = witness;
}

static void $ShowTupleD_serialize($ShowTuple self, $Serial$state state) {
    $step_serialize(self->W_Show, state);
}

static $ShowTuple $ShowTupleD_deserialize($ShowTuple self, $Serial$state state) {
    if (!self)
        self = $DNEW($ShowTuple, state);
    ACTON_STORE(self, self->W_Show, $step_deserialize(state));
    return self;
}

static B_str $ShowTupleD___str__($ShowTuple self, $WORD value) {
    return B_tuple_str((B_tuple)value, self->W_Show);
}

struct $ShowTupleG_class $ShowTupleG_methods = {
    .$GCINFO = "$ShowTuple",
    .$class_id = UNASSIGNED,
    .__init__ = $ShowTupleD___init__,
    .__serialize__ = $ShowTupleD_serialize,
    .__deserialize__ = $ShowTupleD_deserialize,
    .__str__ = $ShowTupleD___str__,
    .__repr__ = $ShowTupleD___str__
};

$ShowTuple $ShowTupleG_new(B_tuple witness) {
    return $NEW($ShowTuple, witness);
}

// Compose structural evidence for an open tuple from its known prefix and
// supplied tail. Both inputs are compiler-generated $ShowTuple witnesses;
// their captured component witnesses retain any generic Show policies.
B_Show $ShowTupleJoin(B_Show prefix, B_Show tail) {
    B_tuple a = (($ShowTuple)prefix)->W_Show;
    B_tuple b = (($ShowTuple)tail)->W_Show;
    if (a->size == 0)
        return tail;
    if (b->size == 0)
        return prefix;
    int n = a->size + b->size;
    B_tuple witnesses = acton_malloc(sizeof(struct B_tuple) + n * sizeof($WORD));
    witnesses->$class = &B_tupleG_methods;
    witnesses->size = n;
    for (int i = 0; i < a->size; i++)
        ACTON_STORE(witnesses, witnesses->components[i], a->components[i]);
    for (int i = 0; i < b->size; i++)
        ACTON_STORE(witnesses, witnesses->components[a->size + i], b->components[i]);
    return (B_Show)$ShowTupleG_new(witnesses);
}

// Stateless witnesses deserialize to their singleton, recording the row so
// subsequent references in the same serialized graph retain sharing.
static void $statelessWitness_serialize($WORD self, $Serial$state state) {
}

static $WORD $statelessWitness_deserialize($WORD self, $Serial$state state, $WORD singleton) {
    if (!self) {
        self = singleton;
        B_dictD_setitem(state->done, toB_int(state->row_no-1), self);
    }
    return self;
}

static struct $BoolTuple $BoolTuple_instance;

static void $BoolTupleD___init__($BoolTuple self) {
}

static bool $BoolTupleD___bool__($BoolTuple self, $WORD value) {
    return B_tuple_bool((B_tuple)value);
}

static $BoolTuple $BoolTupleD_deserialize($BoolTuple self, $Serial$state state) {
    return $statelessWitness_deserialize(self, state, &$BoolTuple_instance);
}

struct $BoolTupleG_class $BoolTupleG_methods = {
    .$GCINFO = "$BoolTuple",
    .$class_id = UNASSIGNED,
    .__init__ = $BoolTupleD___init__,
    .__serialize__ = (void (*)($BoolTuple, $Serial$state))$statelessWitness_serialize,
    .__deserialize__ = $BoolTupleD_deserialize,
    .__bool__ = $BoolTupleD___bool__
};

static struct $BoolTuple $BoolTuple_instance = {&$BoolTupleG_methods};

$BoolTuple $BoolTupleG_new(void) {
    return &$BoolTuple_instance;
}

static B_str $wShowNoneD___str__(B_Show self, $WORD value) {
    return B_None_str;
}

static bool $wBoolNoneD___bool__(B_Bool self, $WORD value) {
    return false;
}

static struct B_Show $wShowNone_instance;

static B_Show $wShowNoneD_deserialize(B_Show self, $Serial$state state) {
    return $statelessWitness_deserialize(self, state, &$wShowNone_instance);
}

struct B_ShowG_class $wShowNoneG_class = {
    .$GCINFO = "$wShowNone",
    .$class_id = UNASSIGNED,
    .__init__ = (B_NoneType (*)(B_Show))$default__init__,
    .__serialize__ = (void (*)(B_Show, $Serial$state))$statelessWitness_serialize,
    .__deserialize__ = $wShowNoneD_deserialize,
    .__str__ = $wShowNoneD___str__,
    .__repr__ = $wShowNoneD___str__
};

static struct B_Show $wShowNone_instance = {&$wShowNoneG_class};
B_Show $wShowNone = &$wShowNone_instance;

static struct B_Bool $wBoolNone_instance;

static B_Bool $wBoolNoneD_deserialize(B_Bool self, $Serial$state state) {
    return $statelessWitness_deserialize(self, state, &$wBoolNone_instance);
}

struct B_BoolG_class $wBoolNoneG_class = {
    .$GCINFO = "$wBoolNone",
    .$class_id = UNASSIGNED,
    .__init__ = (B_NoneType (*)(B_Bool))$default__init__,
    .__serialize__ = (void (*)(B_Bool, $Serial$state))$statelessWitness_serialize,
    .__deserialize__ = $wBoolNoneD_deserialize,
    .__bool__ = $wBoolNoneD___bool__
};

static struct B_Bool $wBoolNone_instance = {&$wBoolNoneG_class};
B_Bool $wBoolNone = &$wBoolNone_instance;


// IdentityActor //////////////////////////////////////////////////////

extern struct $IdentityActorG_class $IdentityActorG_methods;

void $IdentityActorD___init__($IdentityActor wit) { }

bool $IdentityActorD_bool($IdentityActor self) {
    return true;
}

B_str $IdentityActorD_str($IdentityActor self) {
    return $FORMAT("<IdentityActor witness at %p>", self);
}

void $IdentityActorD_serialize($IdentityActor self,$Serial$state state) { }

$IdentityActor $IdentityActorD_deserialize($IdentityActor res, $Serial$state state) {
    if (!res)
        res = $DNEW($IdentityActor,state);
    return res;
}

bool $IdentityActorD___is__($IdentityActor wit, $WORD a, $WORD b) {
    return (a == b) ? true : false;
}

bool $IdentityActorD___isnot__($IdentityActor wit, $WORD a, $WORD b) {
    return (a == b) ? false : true;
}

struct $IdentityActorG_class $IdentityActorG_methods = {"$IdentityActor", UNASSIGNED, NULL, $IdentityActorD___init__, $IdentityActorD_serialize, $IdentityActorD_deserialize, $IdentityActorD___is__, $IdentityActorD___isnot__};


$IdentityActor $IdentityActorG_new() {
    return $NEW($IdentityActor);
}


// Various small functions //////////////////////////////////////////////////////////////


static struct B_AssertionError B_assert_error =
    STATIC_EXCEPTION(B_AssertionError, "");

$WORD $ASSERT(bool test, B_str msg) {
    if (!test) {
        if (msg)
            RAISE(B_AssertionError, msg);
        RAISE_EXC(&B_assert_error);
        return NULL; // to avoid compiler warning
    }
    return B_None;
}
