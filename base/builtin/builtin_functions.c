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

static $WORD mkstr($WORD w) {
    B_value w1 = (B_value)w;
    return w1->$class->__str__(w);
}
/*
B_NoneType B_print(int size, ...) {
    va_list args;
    va_start(args,size);
    if (size > 0) {
        B_value elem = va_arg(args,B_value);
        fputs((const char*)elem->$class->__str__(elem)->str,stdout);
     }
    for (int i=1; i<size; i++) {
         putchar(' ');
         B_value elem = va_arg(args,B_value);
         fputs((const char*)elem->$class->__str__(elem)->str,stdout);
     }
     putchar('\n');
     va_end(args);
     return B_None;
}
*/

B_str __str__(B_value x) {
    if (x == B_None)
        return to$str("None");
    else
        return x->$class->__str__(x);
}

B_NoneType B_print(B_tuple t, B_str sep_arg, B_str end_arg, B_bool stderr_arg, B_bool flush_arg) {
    FILE *outfd = stdout;
    if (stderr_arg && stderr_arg->val) {
        outfd = stderr;
    }
    B_str sep = to$str(" ");
    if (sep_arg)
        sep = sep_arg;
    B_str end = to$str("\n");
    if (end_arg)
        end = end_arg;

    // Write to temporary buffer first, making us much less prone to interleaved
    // output from multiple threads. It costs a malloc and some copies but print
    // should not be used in performance critical code.
    int tlen = 0;
    for (int i=0; i<t->size; i++) {
        B_value elem = (B_value)t->components[i];
        tlen += __str__(elem)->nbytes + sep->nbytes;
    }
    tlen += end->nbytes;
    char *s = acton_malloc(tlen+1);
    int pos = 0;
    for (int i=0; i<t->size; i++) {
        if (i > 0) {
            memcpy(s+pos, sep->str, sep->nbytes);
            pos += sep->nbytes;
        }
        B_value elem = (B_value)t->components[i];
        memcpy(s+pos, __str__(elem)->str, __str__(elem)->nbytes);
        pos += __str__(elem)->nbytes;
    }
    memcpy(s+pos, end->str, end->nbytes);
    pos += end->nbytes;
    s[pos] = '\0';
    fputs(s, outfd);

    if (flush_arg && flush_arg->val)
        fflush(outfd);
    return B_None;
}

// enumerate //////////////////////////////////////////////////////////////////////////

void B_IteratorD_enumerate_init(B_IteratorD_enumerate self, B_Iterator it, B_int n) {
    self->it = it;
    self->nxt = from$int(n);
}

B_bool B_IteratorD_enumerate_bool(B_IteratorD_enumerate self) {
    return B_True;
}

B_str B_IteratorD_enumerate_str(B_IteratorD_enumerate self) {
    return $FORMAT("<enumerate iterator object at %p>", self);
}

void B_IteratorD_enumerate_serialize(B_IteratorD_enumerate self,$Serial$state state) {
    $step_serialize(self->it,state);
    $step_serialize(to$int(self->nxt),state);
}

B_IteratorD_enumerate B_IteratorD_enumerate$_deserialize(B_IteratorD_enumerate res,$Serial$state state) {
    if (!res)
        res = $DNEW(B_IteratorD_enumerate,state);
    res->it = $step_deserialize(state);
    res->nxt = from$int((B_int)$step_deserialize(state));
    return res;
}

$WORD B_IteratorD_enumerate_next(B_IteratorD_enumerate it) {
    $WORD w = it->it->$class->__next__(it->it);
    return $NEWTUPLE(2,to$int(it->nxt++),w);
}

struct B_IteratorD_enumerateG_class B_IteratorD_enumerateG_methods = {"B_IteratorD_enumerate",UNASSIGNED,($SuperG_class)&B_IteratorG_methods,B_IteratorD_enumerate_init,
                                                                B_IteratorD_enumerate_serialize, B_IteratorD_enumerate$_deserialize, 
                                                                B_IteratorD_enumerate_bool,B_IteratorD_enumerate_str,B_IteratorD_enumerate_str, B_IteratorD_enumerate_next};


B_IteratorD_enumerate B_IteratorD_enumerateG_new(B_Iterator it, B_int n) {
    return $NEW(B_IteratorD_enumerate, it, n);
}

B_Iterator B_enumerate(B_Iterable wit, $WORD iter, B_int start) {
    B_Iterator it = wit->$class->__iter__(wit,iter);
    if (!start)
        start = to$int(0);
    return (B_Iterator)B_IteratorD_enumerateG_new(it,start); 
}

// filter ////////////////////////////////////////////////////////////////////////////////

void B_IteratorD_filter_init(B_IteratorD_filter self, B_Iterator it,  $pure f) {
    self->it = it;
    self->f = f;
}

B_bool B_IteratorD_filter_bool(B_IteratorD_filter self) {
    return B_True;
}

B_str B_IteratorD_filter_str(B_IteratorD_filter self) {
    return $FORMAT("<filter iterator object at %p>", self);
}

void B_IteratorD_filter_serialize(B_IteratorD_filter self,$Serial$state state) {
    $step_serialize(self->it,state);
}

B_IteratorD_filter B_IteratorD_filter$_deserialize(B_IteratorD_filter res, $Serial$state state) {
    if (!res)
        res = $DNEW(B_IteratorD_filter,state);
    res->it = $step_deserialize(state);
    return res;
}

$WORD B_IteratorD_filter_next(B_IteratorD_filter it) {
    $WORD w;
    do
        w = it->it->$class->__next__(it->it);
    while (!fromB_bool(it->f->$class->__eval__(it->f, w)));
    return w;
}

struct B_IteratorD_filterG_class B_IteratorD_filterG_methods = {"B_IteratorD_filter",UNASSIGNED,($SuperG_class)&B_IteratorG_methods,B_IteratorD_filter_init,
                                                          B_IteratorD_filter_serialize, B_IteratorD_filter$_deserialize, 
                                                          B_IteratorD_filter_bool,B_IteratorD_filter_str,B_IteratorD_filter_str, B_IteratorD_filter_next};

B_IteratorD_filter B_IteratorD_filterG_new(B_Iterator it, $pure f) {
    return $NEW(B_IteratorD_filter, it, f);
}

B_Iterator B_filter(B_Iterable wit, $pure f, $WORD iter) {
    B_Iterator it = wit->$class->__iter__(wit,iter);
    return (B_Iterator)B_IteratorD_filterG_new(it,f);
}

// map ////////////////////////////////////////////////////////////////////////////////

void B_IteratorD_map_init(B_IteratorD_map self, B_Iterator it, $pure f) {
    self->it = it;
    self->f = f;
}

B_bool B_IteratorD_map_bool(B_IteratorD_map self) {
    return B_True;
}

B_str B_IteratorD_map_str(B_IteratorD_map self) {
    return $FORMAT("<map iterator object at %p>", self);
}

void B_IteratorD_map_serialize(B_IteratorD_map self,$Serial$state state) {
    $step_serialize(self->it,state);
}

B_IteratorD_map B_IteratorD_map$_deserialize(B_IteratorD_map res, $Serial$state state) {
    if (!res)
        res = $DNEW(B_IteratorD_map,state);
    res->it = $step_deserialize(state);
    return res;
}

$WORD B_IteratorD_map_next(B_IteratorD_map it) {
    $WORD w = it->it->$class->__next__(it->it);
    return it->f->$class->__eval__(it->f, w);
}

struct B_IteratorD_mapG_class B_IteratorD_mapG_methods = {"B_IteratorD_map",UNASSIGNED,($SuperG_class)&B_IteratorG_methods,B_IteratorD_map_init,
                                                    B_IteratorD_map_serialize, B_IteratorD_map$_deserialize,  
                                                    B_IteratorD_map_bool,B_IteratorD_map_str,B_IteratorD_map_str, B_IteratorD_map_next};

B_IteratorD_map B_IteratorD_mapG_new(B_Iterator it, $pure f) {
    return $NEW(B_IteratorD_map, it, f);
}

B_Iterator B_map(B_Iterable wit, $pure f, $WORD iter) {
    B_Iterator it = wit->$class->__iter__(wit,iter);
    return (B_Iterator)B_IteratorD_mapG_new(it,f);
}


// max, min ///////////////////////////////////////////////////////////////////////////////////

$WORD B_max(B_Ord wit, B_Iterable wit2, $WORD iter, $WORD dflt) {
    $WORD res = dflt;
    B_Iterator it = wit2->$class->__iter__(wit2,iter);  
    while(1) {
        if ($PUSH()) {
            $WORD nxt = it->$class->__next__(it);
            if (!res || fromB_bool(wit->$class->__lt__(wit,res,nxt)))
                res = nxt;
            $DROP();
        } else {
            B_BaseException ex = $POP();
            if ($ISINSTANCE0(ex, B_StopIteration))
                break;
           else
               $RAISE(ex);
        }
    }

    // If no value was found in the iterable
    if (!res) {
        if (dflt) {
            return dflt; // Return the provided default value
        } else {
            $RAISE(((B_BaseException)B_ValueErrorG_new($FORMAT("max() arg is an empty sequence"))));
        }
    }

    return res;
}

$WORD B_max_def(B_Ord wit, B_Iterable wit2, $WORD iter, $WORD dflt) {
    $WORD res = dflt;
    B_Iterator it = wit2->$class->__iter__(wit2,iter);
    while(1) {
        if ($PUSH()) {
            $WORD nxt = it->$class->__next__(it);
            if (fromB_bool(wit->$class->__lt__(wit,res,nxt)))
                res = nxt;
            $DROP();
        } else {
            B_BaseException ex = $POP();
            if ($ISINSTANCE0(ex, B_StopIteration))
                break;
           else
               $RAISE(ex);
        }
    }
    return res;
}

$WORD B_min(B_Ord wit, B_Iterable wit2, $WORD iter, $WORD dflt) {
    $WORD res = NULL;
    B_Iterator it = wit2->$class->__iter__(wit2,iter);  
    while(1) {
        if ($PUSH()) {
            $WORD nxt = it->$class->__next__(it);
            if (!res || fromB_bool(wit->$class->__gt__(wit,res,nxt)))
                res = nxt;
            $DROP();
        } else {
            B_BaseException ex = $POP();
            if ($ISINSTANCE0(ex, B_StopIteration))
                break;
            else
                $RAISE(ex);
        }
    }

    // If no value was found in the iterable
    if (!res) {
        if (dflt) {
            return dflt; // Return the provided default value
        } else {
            $RAISE((B_BaseException)B_ValueErrorG_new($FORMAT("min() arg is an empty sequence")));
        }
    }

    return res;
}

$WORD B_min_def(B_Ord wit, B_Iterable wit2, $WORD iter, $WORD dflt) {
    $WORD res = dflt;
    B_Iterator it = wit2->$class->__iter__(wit2,iter);  
    while(1) {
        if ($PUSH()) {
            $WORD nxt = it->$class->__next__(it);
            if (fromB_bool(wit->$class->__gt__(wit,res,nxt)))
                res = nxt;
            $DROP();
        } else {
            B_BaseException ex = $POP();
            if ($ISINSTANCE0(ex, B_StopIteration))
                break;
            else
                $RAISE(ex);
        }
    }
    return res;
}

B_list B_sorted(B_Ord wit, B_Iterable wit2, $WORD iter) {
    B_CollectionD_SequenceD_list w = B_CollectionD_SequenceD_listG_witness;
    B_list res = w->$class->__fromiter__(w, wit2, iter);
    B_tim_sort(wit, res->data, res->length);
    return res;
}

// sum /////////////////////////////////////////////////////////////////////////////////

$WORD B_sum(B_Plus wit, B_Iterable wit2, $WORD iter, $WORD start) {
    B_Iterator it = wit2->$class->__iter__(wit2,iter);  
    $WORD res = start;
    $WORD nxt;
    while(1) {
        if ($PUSH()) {
            nxt = it->$class->__next__(it);
            res = wit->$class->__add__(wit,res,nxt);
            $DROP();
        } else {
            B_BaseException ex = $POP();
            if ($ISINSTANCE0(ex, B_StopIteration))
                break;
           else
               $RAISE(ex);
        }
    }
    return res;
}

// zip ////////////////////////////////////////////////////////////////////////////////

void B_IteratorD_zip_init(B_IteratorD_zip self, B_Iterator it1, B_Iterator it2) {
    self->it1 = it1;
    self->it2 = it2;
}

B_bool B_IteratorD_zip_bool(B_IteratorD_zip self) {
    return B_True;
}

B_str B_IteratorD_zip_str(B_IteratorD_zip self) {
    return $FORMAT("<zip iterator object at %p>", self);
}

void B_IteratorD_zip_serialize(B_IteratorD_zip self,$Serial$state state) {
    $step_serialize(self->it1,state);
    $step_serialize(self->it2,state);
}

B_IteratorD_zip B_IteratorD_zip$_deserialize(B_IteratorD_zip res, $Serial$state state) {
    if (!res)
        res = $DNEW(B_IteratorD_zip,state);
    res->it1 = $step_deserialize(state);
    res->it2 = $step_deserialize(state);
    return res;
}

$WORD B_IteratorD_zip_next(B_IteratorD_zip it) {
    $WORD w1 = it->it1->$class->__next__(it->it1);
    $WORD w2 = it->it2->$class->__next__(it->it2);
    if (w1 && w2)
        return $NEWTUPLE(2,w1,w2);
    else
        return NULL;
}

struct B_IteratorD_zipG_class B_IteratorD_zipG_methods = {" B_IteratorD_zip",UNASSIGNED,($SuperG_class)&B_IteratorG_methods,B_IteratorD_zip_init,
                                                    B_IteratorD_zip_serialize, B_IteratorD_zip$_deserialize, 
                                                    B_IteratorD_zip_bool,B_IteratorD_zip_str,B_IteratorD_zip_str, B_IteratorD_zip_next};

B_IteratorD_zip B_IteratorD_zipG_new(B_Iterator iter1, B_Iterator iter2) {
    return $NEW(B_IteratorD_zip, iter1, iter2);
}

B_Iterator B_zip (B_Iterable wit1, B_Iterable wit2, $WORD iter1, $WORD iter2) {
    B_Iterator it1 = wit1->$class->__iter__(wit1,iter1);
    B_Iterator it2 = wit2->$class->__iter__(wit2,iter2);
    return (B_Iterator)B_IteratorD_zipG_new(it1,it2);
}

// EqOpt //////////////////////////////////////////////////////

extern struct $EqOptG_class $EqOptG_methods;

void $EqOptD___init__($EqOpt wit, B_Eq W_Eq$A) {
    wit->W_Eq$A = W_Eq$A;
}

B_bool $EqOptD_bool($EqOpt self) {
    return B_True;
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
    res->W_Eq$A = $step_deserialize(state);
    return res;
}

B_bool $EqOptD___eq__($EqOpt wit, $WORD a, $WORD b) {
    if (a && b) {
        return wit->W_Eq$A->$class->__eq__(wit->W_Eq$A, a, b);
    }
    return (!a && !b) ? B_True : B_False;
}

B_bool $EqOptD___ne__($EqOpt wit, $WORD a, $WORD b) {
    if (a && b)
        return wit->W_Eq$A->$class->__ne__(wit->W_Eq$A, a, b);
    return (!a && !b) ? B_False : B_True;
}

struct $EqOptG_class $EqOptG_methods = {"$EqOpt", UNASSIGNED, NULL, $EqOptD___init__, $EqOptD_serialize, $EqOptD_deserialize, 
                                         $EqOptD_bool, $EqOptD_str, $EqOptD_str, $EqOptD___eq__, $EqOptD___ne__};


$EqOpt $EqOptG_new(B_Eq W_Eq$A) {
    return $NEW($EqOpt, W_Eq$A);
}


// IdentityActor //////////////////////////////////////////////////////

extern struct $IdentityActorG_class $IdentityActorG_methods;

void $IdentityActorD___init__($IdentityActor wit) { }

B_bool $IdentityActorD_bool($IdentityActor self) {
    return B_True;
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

B_bool $IdentityActorD___is__($IdentityActor wit, $WORD a, $WORD b) {
    return (a == b) ? B_True : B_False;
}

B_bool $IdentityActorD___isnot__($IdentityActor wit, $WORD a, $WORD b) {
    return (a == b) ? B_False : B_True;
}

struct $IdentityActorG_class $IdentityActorG_methods = {"$IdentityActor", UNASSIGNED, NULL, $IdentityActorD___init__, $IdentityActorD_serialize, $IdentityActorD_deserialize, 
                                         $IdentityActorD_bool, $IdentityActorD_str, $IdentityActorD_str, $IdentityActorD___is__, $IdentityActorD___isnot__};


$IdentityActor $IdentityActorG_new() {
    return $NEW($IdentityActor);
}


// Various small functions //////////////////////////////////////////////////////////////

// Code generated by actonc
/*
$WORD B_abs (B_Number W_149, B_Real W_148, $WORD x) {
    return W_149->$class->__abs__(W_149, x, W_148);
}

B_bool B_all (B_Iterable W_164, $WORD it) {
    B_Iterator nB_iter = W_164->$class->__iter__(W_164, it);
    $WORD n$1val = nB_iter->$class->__next__(nB_iter);
    while ($ISNOTNONE(n$1val)) {
        B_value x = (B_value)n$1val;
        if (!x->$class->__bool__(x)->val) {
            return (B_bool)B_False;
        }
        n$1val = nB_iter->$class->__next__(nB_iter);
    }
    return (B_bool)B_True;
}
B_bool B_any (B_Iterable W_179, $WORD it) {
    B_Iterator n$2iter = W_179->$class->__iter__(W_179, it);
    $WORD n$3val = n$2iter->$class->__next__(n$2iter);
    while ($ISNOTNONE(n$3val)) {
        B_value x = (B_value)n$3val;
        if (x->$class->__bool__(x)->val) {
            return (B_bool)B_True;
        }
        n$3val = n$2iter->$class->__next__(n$2iter);
    }
    return (B_bool)B_False;
}

B_tuple B_divmod (B_Integral W_225, $WORD a, $WORD b) {
    return W_225->$class->__divmod__(W_225, a, b);
}
B_int B_hash (B_Hashable W_255, $WORD x) {
    return W_255->$class->__hash__(W_255, x);
}
B_Iterator B_iter (B_Iterable W_278, $WORD x) {
    return W_278->$class->__iter__(W_278, x);
}
B_int B_len (B_Collection W_301, $WORD x) {
    return W_301->$class->__len__(W_301, x);
}

$WORD B_pow (B_Number W_344, $WORD a, $WORD b) {
    return W_344->$class->__pow__(W_344, a, b);
}

B_str B_repr(B_value x) {
    return x->$class->__repr__(x);
}

B_Iterator B_reversed (B_Sequence W_369, $WORD seq) {
    return W_369->$class->__reversed__(W_369, seq);
}
$WORD B_round (B_Real W_395, $WORD x, B_int n) {
    return W_395->$class->__round__(W_395, x, n);
}
*/
