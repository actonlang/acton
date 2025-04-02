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

// Auxiliary functions /////////////////////////////////////////////////////////////////////////////////////////////////////
 
// For now, expansion doubles capacity. 
static void expand(B_list lst, int n) {
    if (lst->capacity >= lst->length + n)
        return;
    int newcapacity = lst->capacity==0 ? 1 : lst->capacity;
    while (newcapacity < lst->length+n)
        newcapacity <<= 1;
    $WORD* newptr = lst->data==NULL
        ? acton_malloc(newcapacity*sizeof($WORD))
        : acton_realloc(lst->data,newcapacity*sizeof($WORD));
    if (newptr == NULL) {
        $RAISE((B_BaseException)$NEW(B_MemoryError,to$str("memory allocation failed")));
    }
    lst->data = newptr;
    lst->capacity = newcapacity;
}

static void shrink(B_list lst) {
    if (lst->capacity > 20 && 2*lst->length < lst->capacity) {
        int newcapacity = lst->length;
        $WORD old = lst->data;
        lst->data = acton_malloc(newcapacity * sizeof($WORD));
        lst->capacity = newcapacity;
        assert(old != NULL);
        memcpy(lst->data, old, newcapacity * sizeof($WORD));
    }
}
            

B_list B_listD_new(int capacity) {
    if (capacity < 0) {
        fprintf(stderr,"Internal error list_new: negative capacity");
        exit(-1);
    } 
    B_list lst = acton_malloc(sizeof(struct B_list));
    if (lst == NULL) {
        $RAISE((B_BaseException)$NEW(B_MemoryError,to$str("memory allocation failed")));
    }
    if (capacity>0) {
        lst->data = acton_malloc(capacity*sizeof($WORD));
        if (lst->data == NULL) {
            $RAISE((B_BaseException)$NEW(B_MemoryError,to$str("memory allocation failed")));
        }
    } else {
        lst->data = NULL;
    }
    lst->length = 0;
    lst->capacity = capacity;
    lst->$class = &B_listG_methods; 
    return lst;
}

// General methods ///////////////////////////////////////

B_list B_listG_new(B_Iterable wit, $WORD iterable) {
    return $NEW(B_list, wit, iterable);
}

B_NoneType B_listD___init__(B_list lst, B_Iterable wit, $WORD iterable) {
    lst->length = 0;
    lst->capacity = 0;
    lst->data = NULL;
    if (!iterable || !wit) {
        return B_None;
    }
    $WORD w;
    B_Iterator it = wit->$class->__iter__(wit,iterable);
    B_SequenceD_list wit2 = B_SequenceD_listG_new();
    while(1) {
        if ($PUSH()) {
            $WORD e = it->$class->__next__(it);
            wit2->$class->append(wit2, lst, e);
            $DROP();
        } else {
            B_BaseException ex = $POP();
            if ($ISINSTANCE0(ex, B_StopIteration))
                break;
           else
               $RAISE(ex);
        }
    }
    return B_None;
}
  
B_bool B_BoolD_listD___bool__( B_BoolD_list wit, B_list self) {
    return toB_bool(self->length>0);
}

B_str B_ShowD_listD___str__( B_ShowD_list wit, B_list self) {
    B_list s2 = B_listD_new(self->length);
    B_SequenceD_list wit2 = B_SequenceD_listG_new();
    B_Show wit3 = wit->W_ShowD_AD_ShowD_list;
    for (int i=0; i< self->length; i++) {
        $WORD elem = self->data[i];
        wit2->$class->append(wit2, s2, wit3->$class->__repr__(wit3, elem));
    }
    return B_strD_join_par('[',s2,']');
}

B_str B_ShowD_listD___repr__( B_ShowD_list wit, B_list self) {
    return B_ShowD_listD___str__(wit, self);
}

void B_listD___serialize__(B_list self,$Serial$state state) {
    B_int prevkey = (B_int)B_dictD_get(state->done,(B_Hashable)B_HashableD_WORDG_witness,self,NULL);
    if (prevkey) {
        long pk = from$int(prevkey);
        $val_serialize(-LIST_ID,&pk,state);
        return;
    }
    B_dictD_setitem(state->done,(B_Hashable)B_HashableD_WORDG_witness,self,to$int(state->row_no));
    long len = (long)self->length;
    $val_serialize(LIST_ID,&len,state);
    for (int i=0; i<self->length; i++) {
        $step_serialize(self->data[i],state);
    }
}
 
B_list B_listD___deserialize__(B_list res, $Serial$state state) {
    $ROW this = state->row;
    state->row = this->next;
    state->row_no++;
    if (this->class_id < 0) {
        return (B_list)B_dictD_get(state->done,(B_Hashable)B_HashableD_intG_witness,to$int((long)this->blob[0]),NULL);
    } else {
        if (!res)
            res = B_listD_new((int)(long)this->blob[0]);
        B_dictD_setitem(state->done,(B_Hashable)B_HashableD_intG_witness,to$int(state->row_no-1),res);
        res->length = res->capacity;
        for (int i = 0; i < res->length; i++) 
            res->data[i] = $step_deserialize(state);
        return res;
    }
}

B_list B_listD_copy(B_list lst) {
    int len = lst->length;
    B_list res = B_listD_new(len);
    res->length = len;
    if (len > 0)
        memcpy(res->data, lst->data, len*sizeof($WORD));
    return res;
}

B_NoneType B_listD_clear(B_list lst) {
    lst->data = NULL;
    lst->capacity = 0;
    lst->length = 0;
    return B_None;
}

B_NoneType B_listD_extend(B_list lst, B_list other) {
    if (other->length == 0)
        return B_None;
    expand(lst, other->length);
    memcpy(lst->data + lst->length, other->data, other->length * sizeof($WORD));
    lst->length += other->length;
    return B_None;
}

$WORD B_listD_pop(B_list lst, B_int i) {
    long ix;
    int len =lst->length;
    if (!i)
        ix = len-1;
    else
        ix = from$int(i);
    long ix0 = ix < 0 ? len + ix : ix;
    if (ix0 < 0 || ix0 >= len) {
        $RAISE((B_BaseException)$NEW(B_IndexError, to$int(ix0), to$str("pop: index outside list")));
    }
    $WORD res = lst->data[ix0];
    memmove(lst->data + ix0,
            lst->data + (ix0 + 1),
            (len-(ix0+1))*sizeof($WORD));
    lst->data[len-1] = NULL;
    lst->length--;
    shrink(lst);
    return res;
}

B_int B_listD_index(B_list self, B_Eq W_EqD_B, $WORD val, B_int start, B_int stop) {
    int strt = 0;
    if (start)
        strt = from$int(start);
    if (strt < 0)
        $RAISE((B_BaseException)$NEW(B_ValueError, to$str("start position must be >= 0")));
    if (strt > self->length)
        $RAISE((B_BaseException)$NEW(B_ValueError, to$str("start position must not exceed list length")));
    int stp = self->length;
    if (stop)
        stp = from$int(stop);
    if (stp <= strt)
        $RAISE((B_BaseException)$NEW(B_ValueError, to$str("stop position must be higher than start position")));
    if (stp > self->length)
        stp = self->length;
    for (int i=strt; i < stp; i++) {
        B_value elem = (B_value)self->data[i];
        B_bool eq = W_EqD_B->$class->__eq__(W_EqD_B, val, elem);
        if (eq->val)
            return to$int(i);
    }
    $RAISE((B_BaseException)$NEW(B_KeyError, val, to$str("element is not in list")));
    return NULL; //to avoid compiler warning 
}

    

// B_OrdD_list ////////////////////////////////////////////////////////

B_bool B_OrdD_listD___eq__ (B_OrdD_list w, B_list a, B_list b) {
    if (a->length != b->length) return B_False;                                
    B_Ord w2 = w->W_OrdD_AD_OrdD_list;
    for (int i = 0; i<a->length; i++)
        if ((w2->$class->__ne__(w2,a->data[i],b->data[i]))->val) return B_False;
    return B_True;
}

B_bool B_OrdD_listD___ne__ (B_OrdD_list w, B_list a, B_list b) {
    return toB_bool(!(w->$class->__eq__(w,a,b)->val));
}

B_bool B_OrdD_listD___lt__ (B_OrdD_list w, B_list a, B_list b) {
    int minl = a->length<b->length ? a->length : b->length;
    B_Ord wA = w->W_OrdD_AD_OrdD_list;
    int i=0;
    while (i<minl && wA->$class->__eq__(wA,a->data[i],b->data[i])) i++;
    if (i==a->length)
        return toB_bool(i<b->length);
    if (i==b->length)
        return B_False;
    return  wA->$class->__lt__(wA,a->data[i],b->data[i]);
}

B_bool B_OrdD_listD___le__ (B_OrdD_list w, B_list a, B_list b) {
    int minl = a->length<b->length ? a->length : b->length;
    B_Ord wA = w->W_OrdD_AD_OrdD_list;
    int i=0;
    while (i<minl && wA->$class->__eq__(wA,a->data[i],b->data[i])) i++;
    if (i==a->length)
        return toB_bool(i<=b->length);
    if (i==b->length)
        return B_False;
    return  wA->$class->__lt__(wA,a->data[i],b->data[i]);
}

B_bool B_OrdD_listD___gt__ (B_OrdD_list w, B_list a, B_list b) {
    return  B_OrdD_listD___lt__ (w,b,a);
}

B_bool B_OrdD_listD___ge__ (B_OrdD_list w, B_list a, B_list b) {
    return  B_OrdD_listD___le__ (w,b,a);
}


//  B_TimesD_SequenceD_list /////////////////////////////////////////////////////////

 
B_list B_TimesD_SequenceD_listD___add__ (B_TimesD_SequenceD_list wit, B_list lst, B_list other) {
    int lstlen = lst->length;
    int otherlen = other->length;
    int reslen = lstlen + otherlen;
    B_list res = B_listD_new(reslen);
    if (lstlen > 0)
        memcpy(res->data,lst->data,lstlen*sizeof($WORD));
    if (otherlen > 0)
        memcpy(res->data+lstlen,other->data,otherlen*sizeof($WORD));
    res->length = reslen;
    return res;
}

B_list B_TimesD_SequenceD_listD___zero__ (B_TimesD_SequenceD_list wit) {
    return B_listD_new(0);
}

B_list B_TimesD_SequenceD_listD___mul__ (B_TimesD_SequenceD_list wit, B_list lst, B_int n) {
    int lstlen = lst->length;
    if (n->val.size <= 0)
        return B_listD_new(0);
    else {
        long n64 =  from$int(n);
        B_list res = B_listD_new(lstlen * n64);
        for (int i=0; i<n64; i++)
            memcpy(res->data + i*lstlen, lst->data, lstlen * sizeof($WORD));
        res->length = lstlen * n64;
        return res;
    }
}

//  B_CollectionD_SequenceD_list ///////////////////////////////////////////////////////

// first define the Iterator instance ///


static $WORD B_IteratorD_listD_next(B_IteratorD_list self) {
    if (self->nxt >= self->src->length)
        $RAISE ((B_BaseException)$NEW(B_StopIteration, to$str("list iterator terminated")));
    return self->src->data[self->nxt++];
}

B_IteratorD_list B_IteratorD_listG_new(B_list lst) {
    return $NEW(B_IteratorD_list, lst);
}

void B_IteratorD_listD_init(B_IteratorD_list self, B_list lst) {
    self->src = lst;
    self->nxt = 0;
}

B_bool B_IteratorD_listD_bool(B_IteratorD_list self) {
    return B_True;
}

B_str B_IteratorD_listD_str(B_IteratorD_list self) {
    return $FORMAT("<list iterator object at %p>", self);
}

void B_IteratorD_listD_serialize(B_IteratorD_list self,$Serial$state state) {
    $step_serialize(self->src,state);
    $step_serialize(to$int(self->nxt),state);
}

B_IteratorD_list B_IteratorD_list$_deserialize(B_IteratorD_list res, $Serial$state state) {
    if(!res)
        res = $DNEW(B_IteratorD_list,state);
    res->src = (B_list)$step_deserialize(state);
    res->nxt = from$int((B_int)$step_deserialize(state));
    return res;
}

struct B_IteratorD_listG_class B_IteratorD_listG_methods = {"B_IteratorD_list",UNASSIGNED,($SuperG_class)&B_IteratorG_methods, B_IteratorD_listD_init,
                                                      B_IteratorD_listD_serialize, B_IteratorD_list$_deserialize,B_IteratorD_listD_next};

// Now, we can define the protocol methods

B_Iterator B_CollectionD_SequenceD_listD___iter__(B_CollectionD_SequenceD_list wit, B_list lst) {
    return (B_Iterator)$NEW(B_IteratorD_list,lst);
}

B_list B_CollectionD_SequenceD_listD___fromiter__ (B_CollectionD_SequenceD_list wit, B_Iterable wit2, $WORD iter) {
    return B_listG_new(wit2, iter);
    /*
    B_list res = B_listD_new(4);
    B_SequenceD_list wit3 = B_SequenceD_listG_new();
    B_Iterator it = wit2->$class->__iter__(wit2,iter);
    $WORD nxt;
    while ((nxt = it->$class->__next__(it))) {
        wit3->$class->append(wit3, res, nxt);
    }
    return res;
    */
}

B_int B_CollectionD_SequenceD_listD___len__(B_CollectionD_SequenceD_list wit, B_list self) {
    return to$int(self->length);
}

//  B_SequenceD_list //////////////////////////////////////////////////////////////////
 
$WORD B_SequenceD_listD___getitem__(B_SequenceD_list wit, B_list lst, B_int n) {
    int len = lst->length;
    long ix = from$int(n);
    long ix0 = ix < 0 ? len + ix : ix;
    if (ix0 < 0 || ix0 >= len) {
        $RAISE((B_BaseException)$NEW(B_IndexError, to$int(ix0), to$str("getitem: index outside list")));
    }
    return lst->data[ix0];
}

B_NoneType B_SequenceD_listD___setitem__(B_SequenceD_list wit, B_list lst, B_int n, $WORD val) {
    int len = lst->length;
    long ix = from$int(n);
    long ix0 = ix < 0 ? len + ix : ix;
    if (ix0 < 0 || ix0 >= len) {
        $RAISE((B_BaseException)$NEW(B_IndexError, to$int(ix0), to$str("setitem: index outside list")));
    }
    lst->data[ix0] = val;
    return B_None;
}

B_NoneType B_SequenceD_listD___delitem__(B_SequenceD_list wit, B_list lst, B_int n) {
    int len = lst->length;
    long ix = from$int(n);
    long ix0 = ix < 0 ? len + ix : ix;
    if(ix0 < 0 || ix0 >= len) {
        return B_None;
    }
    memmove(lst->data + ix0,
            lst->data + (ix0 + 1),
            (len-(ix0+1))*sizeof($WORD));
    lst->data[lst->length-1] = NULL;
    lst->length--;
    shrink(lst);
    return B_None;
}

B_list B_SequenceD_listD___getslice__(B_SequenceD_list wit, B_list lst, B_slice slc) {
    int len = lst->length;
    long start, stop, step, slen;
    normalize_slice(slc, len, &slen, &start, &stop, &step);
    // slice notation has been eliminated and default values applied.
    // slen is now the length of the slice
    B_list rlst = B_listD_new(slen);
    long t = start;
    B_SequenceD_list wit2 = B_SequenceD_listG_new();
    for (int i=0; i<slen; i++) {
        $WORD w;
        w = B_SequenceD_listD___getitem__(wit, lst, to$int(t));
        wit2->$class->append(wit2, rlst, w);
        t += step;
    }
    return rlst;
}
 
B_NoneType B_SequenceD_listD___setslice__(B_SequenceD_list wit, B_list lst, B_Iterable wit2, B_slice slc, $WORD iter) {
    int len = lst->length;
    B_list other = B_listD_new(0);    
    B_SequenceD_list wit3 = B_SequenceD_listG_new();
    B_Iterator it = wit2->$class->__iter__(wit2,iter);
    while(1) {
        if ($PUSH()) {
            $WORD w = it->$class->__next__(it);
            wit3->$class->append(wit3, other, w);
            $DROP();
        } else {
            B_BaseException ex = $POP();
            if ($ISINSTANCE0(ex, B_StopIteration))
                break;
           else
               $RAISE(ex);
        }
    }
    int olen = other->length; 
    long start, stop, step, slen;
    normalize_slice(slc, len, &slen, &start, &stop, &step);
    if (step != 1 && olen != slen) {
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("setslice: illegal slice")));
    }
    int copy = olen <= slen ? olen : slen;
    int t = start;
    for (int i= 0; i<copy; i++) {
        lst->data[t] = other->data[i];
        t += step;
    }
    if (olen == slen)
        return B_None;
    // now we know that step=1
    if (olen < slen) {
        memmove(lst->data + start + copy,
                lst->data + start + slen,
                (len-(start+slen))*sizeof($WORD));
        lst->length-=slen-olen;
        return B_None;
    } else {
        expand(lst,olen-slen);
        int rest = len - (start+copy);
        int incr = olen - slen;
        memmove(lst->data + start + copy + incr,
                lst->data + start + copy,
                rest*sizeof($WORD));
        for (int i = copy; i < olen; i++)
            lst->data[start+i] = other->data[i];
        lst->length += incr;
    }
    return B_None;
}

B_NoneType B_SequenceD_listD___delslice__(B_SequenceD_list wit, B_list lst, B_slice slc) {
    int len = lst->length;
    long start, stop, step, slen;
    normalize_slice(slc, len, &slen, &start, &stop, &step);
    if (slen==0) return B_None;
    $WORD *p = lst->data + start;
    for (int i=0; i<slen-1; i++) {
        memmove(p,p+i+1,(step-1)*sizeof($WORD));
        p+=step-1;
    }
    memmove(p,p+slen,(len-1-(start+step*(slen-1)))*sizeof($WORD));
    lst->length-=slen;
    return B_None;
}  

B_NoneType B_SequenceD_listD_reverse(B_SequenceD_list wit, B_list lst) {
    int len = lst->length;
    for (int i = 0; i < len/2; i++) {
        $WORD tmp = lst->data[i];
        lst->data[i] = lst->data[len-1-i];
        lst->data[len-1-i] = tmp;
    }
    return B_None;
}

B_Iterator B_SequenceD_listD___reversed__(B_SequenceD_list wit, B_list lst) {
    B_list copy = B_listD_copy(lst);
    B_SequenceD_listD_reverse(wit, copy);
    return B_CollectionD_SequenceD_listD___iter__((B_CollectionD_SequenceD_list)wit->W_Collection, copy);
}

B_NoneType B_SequenceD_listD_insert(B_SequenceD_list wit, B_list lst, B_int n, $WORD elem) {
    int len = lst->length;
    long ix = from$int(n);
    expand(lst,1);
    long ix0 = ix < 0 ? (len+ix < 0 ? 0 : len+ix) : (ix < len ? ix : len);
    memmove(lst->data + (ix0 + 1),
            lst->data + ix0 ,
            (len - ix0) * sizeof($WORD));
    lst->data[ix0] = elem;
    lst->length++;
    return B_None;
}

B_NoneType B_SequenceD_listD_append(B_SequenceD_list wit, B_list lst, $WORD elem) {
    expand(lst,1);
    lst->data[lst->length++] = elem;
    return B_None;
}


// B_ContainerD_list ///////////////////////////////////////////////////////////////////

B_bool B_ContainerD_listD___contains__(B_ContainerD_list wit, B_list lst, $WORD elem) {
    long res = 0;
    B_Eq w = wit->W_EqD_AD_ContainerD_list;
    for (int i=0; i < lst->length; i++) {
        if (fromB_bool(w->$class->__eq__(w,elem,lst->data[i]))) {
            res = 1;
            break;
        }
    }

    return toB_bool(res);
}
                 
B_bool B_ContainerD_listD___containsnot__(B_ContainerD_list wit, B_list lst, $WORD elem) {
    return toB_bool(!B_ContainerD_listD___contains__(wit,lst,elem)->val);
}

// Witnesses used in code generation
// The initialization code that registers method tables in a list indexed by classid uses list append;
// so we need to initialize the below method table here, even if it is done in B___init__.

struct B_SequenceD_listG_class B_SequenceD_listG_methods = {
    "B_SequenceD_list", 
    UNASSIGNED,
    ($SuperG_class)&B_SequenceG_methods,
    NULL, //B_SequenceD_listD___init__,
    NULL, //B_SequenceD_listD___serialize__,
    NULL, //B_SequenceD_listD___deserialize__,
    B_SequenceD_listD___getitem__,
    B_SequenceD_listD___setitem__,
    B_SequenceD_listD___delitem__,
    B_SequenceD_listD___getslice__,
    B_SequenceD_listD___setslice__,
    B_SequenceD_listD___delslice__,
    B_SequenceD_listD___reversed__,
    B_SequenceD_listD_insert,
    B_SequenceD_listD_append,
    B_SequenceD_listD_reverse
};
