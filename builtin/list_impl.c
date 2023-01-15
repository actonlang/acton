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

// General methods //////////////////////////////////////////////////////////////////////////

B_list B_listG_new(B_Iterable wit, $WORD iterable) {
    return $NEW(B_list, wit, iterable);
}

void B_listD_init(B_list lst, B_Iterable wit, $WORD iterable) {
    lst->length = 0;
    lst->capacity = 0;
    lst->data = NULL;
    if (!iterable || !wit) {
        return;
    }
    $WORD w;
    B_Iterator it = wit->$class->__iter__(wit,iterable);
    while((w = it->$class->__next__(it)))
        B_listD_append(lst,w);
}
  
B_bool B_listD_bool(B_list self) {
    return toB_bool(self->length>0);
}

B_str B_listD_str(B_list self) {
    B_list s2 = B_listD_new(self->length);
    for (int i=0; i< self->length; i++) {
        B_value elem = (B_value)self->data[i];
        B_listD_append(s2,elem->$class->__repr__(elem));
    }
    return B_strD_join_par('[',s2,']');
}

void B_listD_serialize(B_list self,$Serial$state state) {
    B_int prevkey = (B_int)B_dictD_get(state->done,(B_Hashable)B_HashableD_WORDG_witness,self,NULL);
    if (prevkey) {
        long pk = fromB_int(prevkey);
        $val_serialize(-LIST_ID,&pk,state);
        return;
    }
    B_dictD_setitem(state->done,(B_Hashable)B_HashableD_WORDG_witness,self,toB_int(state->row_no));
    long len = (long)self->length;
    $val_serialize(LIST_ID,&len,state);
    for (int i=0; i<self->length; i++) {
        $step_serialize(self->data[i],state);
    }
}
 
B_list B_listD_deserialize(B_list res, $Serial$state state) {
    $ROW this = state->row;
    state->row = this->next;
    state->row_no++;
    if (this->class_id < 0) {
        return (B_list)B_dictD_get(state->done,(B_Hashable)B_HashableD_intG_witness,toB_int((int)this->blob[0]),NULL);
    } else {
        if (!res)
            res = B_listD_new((int)(long)this->blob[0]);
        B_dictD_setitem(state->done,(B_Hashable)B_HashableD_intG_witness,toB_int(state->row_no-1),res);
        res->length = res->capacity;
        for (int i = 0; i < res->length; i++) 
            res->data[i] = $step_deserialize(state);
        return res;
    }
}

struct B_listG_class B_listG_methods = {"B_list",UNASSIGNED,($SuperG_class)&B_objectG_methods, B_listD_init, B_listD_serialize,B_listD_deserialize, B_listD_bool, B_listD_str, B_listD_str,B_listD_copy};

// Auxiliary functions /////////////////////////////////////////////////////////////////////////////////////////////////////
 
// For now, expansion doubles capacity. 
static void expand(B_list lst,int n) {
    if (lst->capacity >= lst->length + n)
        return;
    int newcapacity = lst->capacity==0 ? 1 : lst->capacity;
    while (newcapacity < lst->length+n)
        newcapacity <<= 1;
    $WORD* newptr = lst->data==NULL
        ? malloc(newcapacity*sizeof($WORD))
        : realloc(lst->data,newcapacity*sizeof($WORD));
    if (newptr == NULL) {
        $RAISE((B_BaseException)$NEW(B_MemoryError,to$str("memory allocation failed")));
    }
    lst->data = newptr;
    lst->capacity = newcapacity;
}  

B_list B_listD_new(int capacity) {
    if (capacity < 0) {
        fprintf(stderr,"Internal error list_new: negative capacity");
        exit(-1);
    } 
    B_list lst = malloc(sizeof(struct B_list));
    if (lst == NULL) {
        $RAISE((B_BaseException)$NEW(B_MemoryError,to$str("memory allocation failed")));
    }
    if (capacity>0) {
        lst->data = malloc(capacity*sizeof($WORD));
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

// Times /////////////////////////////////////////////////////////////////////////////////////////////

B_list B_listD_add(B_list lst, B_list other) {
    int lstlen = lst->length;
    int otherlen = other->length;
    int reslen = lstlen + otherlen;
    B_list res = B_listD_new(reslen);
    memcpy(res->data,lst->data,lstlen*sizeof($WORD));
    memcpy(res->data+lstlen,other->data,otherlen*sizeof($WORD));
    res->length = reslen;
    return res;
}

B_list B_listD_mul(B_list lst, B_int n) {
    int lstlen = lst->length;
    if (n->val.size <= 0)
        return B_listD_new(0);
    else {
        long n64 =  fromB_int(n);
        B_list res = B_listD_new(lstlen * n64);
        for (int i=0; i<n64; i++)
            memcpy(res->data + i*lstlen, lst->data, lstlen * sizeof($WORD));
        res->length = lstlen * n64;
        return res;
    }
}
      
    
// Collection ///////////////////////////////////////////////////////////////////////////////////////

B_list B_listD_fromiter(B_Iterator it) {
    B_list res = B_listD_new(4);
    $WORD nxt;
    while ((nxt = it->$class->__next__(it))) {
        B_listD_append(res,nxt);
    }
    return res;
}

long B_listD_len(B_list lst) {
    return (long)lst->length;
}

// Container ///////////////////////////////////////////////////////////////////////////

int B_listD_contains(B_Eq w, B_list lst, $WORD elem) {
    for (int i=0; i < lst->length; i++) {
        if (fromB_bool(w->$class->__eq__(w,elem,lst->data[i])))
            return 1;
    }
    return 0;
}

int B_listD_containsnot(B_Eq w, B_list lst, $WORD elem) {
    return !B_listD_contains(w,lst,elem);
}

// Iterable ///////////////////////////////////////////////////////////////////////////


static $WORD B_IteratorD_listD_next(B_IteratorD_list self) {
    return self->nxt >= self->src->length ? NULL : self->src->data[self->nxt++];
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
    char *s;
    asprintf(&s,"<list iterator object at %p>",self);
    return to$str(s);
}

void B_IteratorD_listD_serialize(B_IteratorD_list self,$Serial$state state) {
    $step_serialize(self->src,state);
    $step_serialize(toB_int(self->nxt),state);
}

B_IteratorD_list B_IteratorD_list$_deserialize(B_IteratorD_list res, $Serial$state state) {
    if(!res)
        res = $DNEW(B_IteratorD_list,state);
    res->src = (B_list)$step_deserialize(state);
    res->nxt = fromB_int((B_int)$step_deserialize(state));
    return res;
}

struct B_IteratorD_listG_class B_IteratorD_listG_methods = {"B_IteratorD_list",UNASSIGNED,($SuperG_class)&B_IteratorG_methods, B_IteratorD_listD_init,
                                                      B_IteratorD_listD_serialize, B_IteratorD_list$_deserialize,B_IteratorD_listD_bool,B_IteratorD_listD_str,B_IteratorD_listD_str,B_IteratorD_listD_next};

B_Iterator B_listD_iter(B_list lst) {
    return (B_Iterator)$NEW(B_IteratorD_list,lst);
}

// Indexed ///////////////////////////////////////////////////////////////////////////

$WORD B_listD_getitem(B_list lst, int ix) {
    int len = lst->length;
    int ix0 = ix < 0 ? len + ix : ix;
    if (ix0 < 0 || ix0 >= len) {
        $RAISE((B_BaseException)$NEW(B_IndexError,to$str("getitem: indexing outside list")));
    }
    return lst->data[ix0];
}

void B_listD_setitem(B_list lst, int ix, $WORD val) {
    int len = lst->length;
    int ix0 = ix < 0 ? len + ix : ix;
    if (ix0 < 0 || ix0 >= len) {
        $RAISE((B_BaseException)$NEW(B_IndexError,to$str("setitem: indexing outside list")));
    }
    lst->data[ix0] = val;
}

void B_listD_delitem(B_list lst,int ix) {
    int len = lst->length;
    int ix0 = ix < 0 ? len + ix : ix;
    if(ix0 < 0 || ix0 >= len) {
        $RAISE((B_BaseException)$NEW(B_IndexError,to$str("delitem: indexing outside list")));
    }
    memmove(lst->data + ix0,
            lst->data + (ix0 + 1),
            (len-(ix0+1))*sizeof($WORD));
    lst->length--;
}
 

// Sliceable //////////////////////////////////////////////////////////////////////////////////////

B_list B_listD_getslice(B_list lst, B_slice slc) {
    int len = lst->length;
    int start, stop, step, slen;
    normalize_slice(slc, len, &slen, &start, &stop, &step);
    // slice notation has been eliminated and default values applied.
    // slen is now the length of the slice
    B_list rlst = B_listD_new(slen);
    int t = start;
    for (int i=0; i<slen; i++) {
        $WORD w;
        w = B_listD_getitem(lst,t);
        B_listD_append(rlst,w);
        t += step;
    }
    return rlst;
}

void B_listD_setslice(B_list lst, B_slice slc, B_Iterator it) {
    int len = lst->length;
    B_list other = B_listD_new(0);
    $WORD w;
    while((w=it->$class->__next__(it)))
        B_listD_append(other,w);
    int olen = other->length; 
    int start, stop, step, slen;
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
        return;
    // now we know that step=1
    if (olen < slen) {
        memmove(lst->data + start + copy,
                lst->data + start + slen,
                (len-(start+slen))*sizeof($WORD));
        lst->length-=slen-olen;
        return;
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
}

void B_listD_delslice(B_list lst, B_slice slc) {
    int len = lst->length;
    int start, stop, step, slen;
    normalize_slice(slc, len, &slen, &start, &stop, &step);
    if (slen==0) return;
    $WORD *p = lst->data + start;
    for (int i=0; i<slen-1; i++) {
        memmove(p,p+i+1,(step-1)*sizeof($WORD));
        p+=step-1;
    }
    memmove(p,p+slen,(len-1-(start+step*(slen-1)))*sizeof($WORD));
    lst->length-=slen;
}

// Sequence /////////////////////////////////////////////////////////////////////////////

void B_listD_append(B_list lst, $WORD val) {
    expand(lst,1);
    lst->data[lst->length++] = val;
}

static $WORD B_IteratorD_listD_reversed_next(B_IteratorD_list self) {
    return self->nxt < 0 ? NULL : self->src->data[self->nxt--];
}

B_Iterator B_listD_reversed(B_list lst){
    B_list copy = B_listD_copy(lst);
    B_listD_reverse(copy);
    return B_listD_iter(copy);
}

void B_listD_insert(B_list lst, int ix, $WORD val) {
    int len = lst->length;
    expand(lst,1);
    int ix0 = ix < 0 ? (len+ix < 0 ? 0 : len+ix) : (ix < len ? ix : len);
    memmove(lst->data + (ix0 + 1),
            lst->data + ix0 ,
            (len - ix0) * sizeof($WORD));
    lst->data[ix0] = val;
    lst->length++;
}

// In place reversal
void B_listD_reverse(B_list lst) {
    int len = lst->length;
    for (int i = 0; i < len/2; i++) {
        $WORD tmp = lst->data[i];
        lst->data[i] = lst->data[len-1-i];
        lst->data[len-1-i] = tmp;
    }
}
 
// List-specific methods /////////////////////////////////////////////////////////////////////

B_list B_listD_copy(B_list lst) {
    int len = lst->length;
    B_list res = B_listD_new(len);
    res->length = len;
    memcpy(res->data,lst->data,len*sizeof($WORD));
    return res;
}
/*                   
                     int list_sort(list_t lst, int (*cmp)(WORD,WORD)) {
                     return heapsort(lst->data, lst->length, sizeof(WORD), cmp);
                     }
*/
 
