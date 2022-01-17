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

void $tuple_init($tuple self, int size, ...) {
    va_list args;
    va_start(args, size);
    self->size = size;
    self->components = malloc(size * sizeof($WORD));
    for (int i = 0; i < size; i++)
        self->components[i] = va_arg(args, $WORD);
    va_end(args);
}

$bool $tuple_bool($tuple self) {
    return to$bool(self->size > 0);
}

$str $tuple_str($tuple self) {
    $list s2 = $list_new(self->size);
    for (int i = 0; i < self->size; i++) {
        $value elem = ($value)self->components[i];
        $list_append(s2, elem->$class->__str__(elem));
    }
    return $str_join_par('(', s2, ')');
}

void $tuple_serialize($tuple self, $Serial$state state) {
    $int prevkey = ($int)$dict_get(state->done, ($Hashable)$Hashable$WORD$witness, self, NULL);
    if (prevkey) {
        $val_serialize(-TUPLE_ID, &prevkey->val, state);
        return;
    }
    $dict_setitem(state->done, ($Hashable)$Hashable$WORD$witness, self, to$int(state->row_no));
    long len = (long)self->size;
    $val_serialize(TUPLE_ID, &len, state);
    for (int i = 0; i < self->size; i++) {
        $step_serialize(self->components[i], state);
    }
}

$tuple $tuple_deserialize($tuple self, $Serial$state state) {
    $ROW this = state->row;
    state->row = this->next;
    state->row_no++;
    if (this->class_id < 0) {
        return ($tuple)$dict_get(state->done, ($Hashable)$Hashable$int$witness, to$int((long)this->blob[0]), NULL);
    } else {
        int len = (int)(long)this->blob[0];
        $tuple res = malloc(sizeof(struct $tuple));
        $dict_setitem(state->done, ($Hashable)$Hashable$int$witness, to$int(state->row_no - 1), res);
        res->components = malloc(len * sizeof($WORD));
        res->$class = &$tuple$methods;
        res->size = len;
        for (int i = 0; i < len; i++)
            res->components[i] = $step_deserialize(state);
        return res;
    }
}

struct $tuple$class $tuple$methods = {
    "tuple",
    UNASSIGNED,
    ($Super$class)&$value$methods,
    $tuple_init,
    $tuple_serialize,
    $tuple_deserialize,
    $tuple_bool,
    $tuple_str};

// Iterators over tuples ///////////////////////////////////////////////////////

static $WORD $Iterator$tuple_next($Iterator$tuple self) {
    return self->nxt >= self->src->size ? NULL : self->src->components[self->nxt++];
}

void $Iterator$tuple_init($Iterator$tuple self, $tuple lst) {
    self->src = lst;
    self->nxt = 0;
}

$bool $Iterator$tuple_bool($Iterator$tuple self) {
    return $True;
}

$str $Iterator$tuple_str($Iterator$tuple self) {
    char *s;
    asprintf(&s, "<tuple iterator object at %p>", self);
    return to$str(s);
}
void $Iterator$tuple_serialize($Iterator$tuple self, $Serial$state state) {
    $step_serialize(self->src, state);
    $step_serialize(to$int(self->nxt), state);
}

$Iterator$tuple $Iterator$tuple$_deserialize($Iterator$tuple res, $Serial$state state) {
    if (!res)
        res = $DNEW($Iterator$tuple, state);
    res->src = $step_deserialize(state);
    res->nxt = from$int(($int)$step_deserialize(state));
    return res;
}

struct $Iterator$tuple$class $Iterator$tuple$methods = {"$Iterator$tuple", UNASSIGNED, ($Super$class)&$Iterator$methods, $Iterator$tuple_init,
                                                        $Iterator$tuple_serialize, $Iterator$tuple$_deserialize, $Iterator$tuple_bool, $Iterator$tuple_str, $Iterator$tuple_next};

// Iterable ///////////////////////////////////////////////////////////////

$Iterator $Iterable$tuple$__iter__($Iterable$tuple wit, $tuple self) {
    return ($Iterator)$NEW($Iterator$tuple, self);
}

void $Iterable$tuple$__init__($Iterable$tuple self) {
    return;
}

void $Iterable$tuple$__serialize__($Iterable$tuple self, $Serial$state state) {
}

$Iterable$tuple $Iterable$tuple$__deserialize__($Iterable$tuple self, $Serial$state state) {
    $Iterable$tuple res = $DNEW($Iterable$tuple, state);
    return res;
}
struct $Iterable$tuple$class $Iterable$tuple$methods = {
    "$Iterable$tuple",
    UNASSIGNED,
    ($Super$class)&$Iterable$methods,
    $Iterable$tuple$__init__,
    $Iterable$tuple$__serialize__,
    $Iterable$tuple$__deserialize__,
    ($bool(*)($Iterable$tuple))$default__bool__,
    ($str(*)($Iterable$tuple))$default__str__,
    $Iterable$tuple$__iter__};
struct $Iterable$tuple $Iterable$tuple$instance = {&$Iterable$tuple$methods};
struct $Iterable$tuple *$Iterable$tuple$witness = &$Iterable$tuple$instance;

// Sliceable ///////////////////////////////////////////////////////////////

void $Sliceable$tuple$__serialize__($Sliceable$tuple self, $Serial$state state) {
}

$Sliceable$tuple $Sliceable$tuple$__deserialize__($Sliceable$tuple self, $Serial$state state) {
    $Sliceable$tuple res = $DNEW($Sliceable$tuple, state);
    return res;
}

void $Sliceable$tuple$__init__($Sliceable$tuple wit) {
    return;
}

$WORD $Sliceable$tuple$__getitem__($Sliceable$tuple wit, $tuple self, $int n) {
    int size = self->size;
    int ix = (int)from$int(n);
    int ix0 = ix < 0 ? size + ix : ix;
    if (ix0 < 0 || ix0 >= size) {
        $RAISE(($BaseException)$NEW($IndexError, to$str("getitem: indexing outside tuple")));
    }
    return self->components[ix0];
}

void $Sliceable$tuple$__setitem__($Sliceable$tuple wit, $tuple self, $int ix, $WORD elem) {
    fprintf(stderr, "%s\n", "internal error: setitem on immutable tuple");
    exit(-1);
}

void $Sliceable$tuple$__delitem__($Sliceable$tuple wit, $tuple self, $int ix) {
    fprintf(stderr, "%s\n", "internal error: delitem on immutable tuple");
    exit(-1);
}

$tuple $Sliceable$tuple$__getslice__($Sliceable$tuple wit, $tuple self, $slice slc) {
    int size = self->size;
    int start, stop, step, slen;
    normalize_slice(slc, size, &slen, &start, &stop, &step);
    //slice notation have been eliminated and default values applied.
    // slen now is the length of the slice
    $tuple res = malloc(sizeof(struct $tuple));
    res->$class = self->$class;
    res->size = slen;
    res->components = malloc(slen * sizeof($WORD));
    int t = start;
    for (int i = 0; i < slen; i++) {
        res->components[i] = self->components[t];
        t += step;
    }
    return res;
}

void $Sliceable$tuple$__setslice__($Sliceable$tuple wit, $tuple self, $Iterable wit2, $slice slc, $WORD iter) {
    fprintf(stderr, "%s\n", "internal error: setslice on immutable tuple");
    exit(-1);
}

void $Sliceable$tuple$__delslice__($Sliceable$tuple wit, $tuple self, $slice slc) {
    fprintf(stderr, "%s\n", "internal error: delslice on immutable tuple");
    exit(-1);
}

struct $Sliceable$tuple$class $Sliceable$tuple$methods = {
    "$Sliceable$tuple",
    UNASSIGNED,
    ($Super$class)&$Sliceable$methods,
    $Sliceable$tuple$__init__,
    $Sliceable$tuple$__serialize__,
    $Sliceable$tuple$__deserialize__,
    ($bool(*)($Sliceable$tuple))$default__bool__,
    ($str(*)($Sliceable$tuple))$default__str__,
    $Sliceable$tuple$__getitem__,
    $Sliceable$tuple$__setitem__,
    $Sliceable$tuple$__delitem__,
    $Sliceable$tuple$__getslice__,
    $Sliceable$tuple$__setslice__,
    $Sliceable$tuple$__delslice__};
struct $Sliceable$tuple $Sliceable$tuple$instance = {&$Sliceable$tuple$methods};
struct $Sliceable$tuple *$Sliceable$tuple$witness = &$Sliceable$tuple$instance;

// Hashable ///////////////////////////////////////////////////////////////

void $Hashable$tuple$__init__($Hashable$tuple wit, int n, $Hashable *comps) {
    wit->w$Hashable$tuple$size = n;
    wit->w$Hashable = comps;
}

void $Hashable$tuple$__serialize__($Hashable$tuple self, $Serial$state state) {
    $step_serialize(to$int(self->w$Hashable$tuple$size), state);
    // we need to serialize the array of Hashables!!
}

$Hashable$tuple $Hashable$tuple$__deserialize__($Hashable$tuple self, $Serial$state state) {
    $Hashable$tuple res = $DNEW($Hashable$tuple, state);
    res->w$Hashable$tuple$size = (int)from$int($step_deserialize(state));
    res->w$Hashable = NULL; // We do not get hash functions for the tuple!
    return res;
}

$bool $Hashable$tuple$__eq__($Hashable$tuple wit, $tuple tup1, $tuple tup2) {
    //type-checking guarantees that sizes are equal
    for (int i = 0; i < tup1->size; i++)
        if (!wit->w$Hashable[i]->$class->__eq__(wit->w$Hashable[i], tup1->components[i], tup2->components[i]))
            return $False;
    return $True;
}

$bool $Hashable$tuple$__ne__($Hashable$tuple wit, $tuple tup1, $tuple tup2) {
    return to$bool(!from$bool($Hashable$tuple$__eq__(wit, tup1, tup2)));
}

$int $Hashable$tuple$__hash__($Hashable$tuple wit, $tuple tup) {
    return to$int($tuple_hash(wit, tup));
}

struct $Hashable$tuple$class $Hashable$tuple$methods = {
    "$Hashable$tuple",
    UNASSIGNED,
    ($Super$class)&$Hashable$methods,
    $Hashable$tuple$__init__,
    $Hashable$tuple$__serialize__,
    $Hashable$tuple$__deserialize__,
    ($bool(*)($Hashable$tuple))$default__bool__,
    ($str(*)($Hashable$tuple))$default__str__,
    $Hashable$tuple$__eq__,
    $Hashable$tuple$__ne__,
    $Hashable$tuple$__hash__};
