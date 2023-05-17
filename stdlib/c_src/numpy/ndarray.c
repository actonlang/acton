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

#ifdef __linux__
#include <bsd/stdlib.h>
#endif
#include <limits.h>
#include "primitive.c"

//Select element #n in lst which is a list[int].
#define $LONGELEM(lst,n)   (from$int((B_int)lst->data[n]))

numpyQ_array_iterator_state $mk_iterator(numpyQ_ndarray a);
union $Bytes8 *iter_next(numpyQ_array_iterator_state it);


B_int numpyQ_newaxis;

// Auxiliary functions ///////////////////////////////////////////////////////////////////////////////

// method for creating ndarray structs.
// res->offset gets default value 0 (may be unsuitable when allocate_data = false)

static numpyQ_ndarray G_newarray(enum ElemType typ, long ndim, B_int size, B_list shape, B_list strides, bool allocate_data) {
    numpyQ_ndarray res = malloc(sizeof(numpyQ_ndarray));
    res->$class = &numpyQ_ndarrayG_methods;
    res->elem_type = typ;
    res->ndim = ndim;
    res->size = size;
    res->offset = 0;
    res->shape = shape;
    res->strides = strides;
    if (allocate_data) res->data = malloc(from$int(size) * $elem_size(typ) * sizeof(union $Bytes8));
    return res;
}

B_int $prod(B_list lst) {
    long res = 1;
    for (int i = 0; i<lst->length; i++)
        res *= $LONGELEM(lst,i);
    return to$int(res);
}

bool $is_contiguous(numpyQ_ndarray a) {
    if (a->offset != 0) return false;
    long size = $LONGELEM(a->strides,a->ndim-1);
    if (size != 1) return false;
    for (int i = a->ndim-2; i>=0; i--) {
        size *= $LONGELEM(a->shape,i+1);
        if (size != $LONGELEM(a->strides,i)) {
            return false;
        }
    }
    return true;
}

B_list $mk_strides(B_list shape) {
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    long size = 1;
    B_list res = B_listD_new(shape->length);
    res->length = shape->length;
    for (int i = shape->length-1; i>=0; i--) {
        int64_t s =  from$int(wit->$class->__getitem__(wit,shape, to$int(i)));
        wit->$class->__setitem__(wit,res,to$int(i),to$int(s > 1 ? size : 0));
        size *= s;
    }
    return res;
}

// Superclass methods /////////////////////////////////////////////////////////////////////////

B_NoneType numpyQ_ndarrayD___init__(numpyQ_ndarray self, numpyQ_Primitive wit, B_atom a) {
    numpyQ_ndarray r = numpyQ_fromatom(wit,a);
    memcpy(self,r,sizeof(struct numpyQ_ndarray));
    return B_None;
}

void  numpyQ_ndarrayD___serialize__(numpyQ_ndarray self, $Serial$state state) {
    B_int prevkey = (B_int)B_dictD_get(state->done,(B_Hashable)B_HashableD_WORDG_witness,self,NULL);
    if (prevkey) {
        long prevkeyval = from$int(prevkey);
        $val_serialize(-self->$class->$class_id,&prevkeyval,state);
        return;
    }
    B_dictD_setitem(state->done,(B_Hashable)B_HashableD_WORDG_witness,self,to$int(state->row_no));
    int64_t sizeval = from$int(self->size);
    int blobsize = 5 +sizeval ;
    $ROW row = $add_header(self->$class->$class_id,blobsize,state);
    row->blob[0] = ($WORD)self->elem_type;
    row->blob[1] = ($WORD)self->ndim;
    row->blob[2] = ($WORD)sizeval;
    row->blob[3] = ($WORD)self->offset;
    row->blob[4] = ($WORD)self->elem_size;
    memcpy(&row->blob[5],self->data,sizeval*sizeof($WORD));
    $step_serialize(self->shape, state);
    $step_serialize(self->strides, state);
}

numpyQ_ndarray numpyQ_ndarrayD___deserialize__(numpyQ_ndarray res, $Serial$state state) {
    int64_t resval = from$int(res->size);
    $ROW this = state->row;
    state->row = this->next;
    state->row_no++;
    if (this->class_id < 0) {
        return B_dictD_get(state->done,(B_Hashable)B_HashableD_intG_witness,to$int((long)this->blob[0]),NULL);
    } else {
        if (!res)
            res = malloc(sizeof(numpyQ_ndarray));
        B_dictD_setitem(state->done,(B_Hashable)B_HashableD_intG_witness,to$int(state->row_no-1),res);
        res->$class = &numpyQ_ndarrayG_methods;
        res->elem_type = (enum ElemType)(long)this->blob[0];
        res->ndim = (long)this->blob[1];
        res->size = to$int((long)this->blob[2]);
        res->offset = (long)this->blob[3];
        res->elem_size = (long)this->blob[4];
        res->data = malloc(resval*sizeof($WORD));
        memcpy(res->data, &this->blob[5],resval*sizeof($WORD));
        res->shape = $step_deserialize(state);
        res->strides= $step_deserialize(state);
        return res;
    }
}

B_bool  numpyQ_ndarrayD___bool__(numpyQ_ndarray a) {
    if (from$int(a->size) > 1) 
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("__bool__ undefined for ndarrays with more than one element")));
    switch (a->elem_type) {
    case LongType:
        return toB_bool(a->data[a->offset].l != 0);
    case DblType:
        return toB_bool(a->data[a->offset].d != 0.0);
    }
}

numpyQ_ndarray numpyQ_ndarrayD___ndgetslice__(numpyQ_ndarray a, B_list ix);

B_str numpyQ_ndarrayD___str__(numpyQ_ndarray a) {
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    if (a->ndim==0) {
        switch (a->elem_type) {
        case LongType:
            return B_l_prim_str(a->data[a->offset]);
        case DblType:
            return B_d_prim_str(a->data[a->offset]);
        }
    } else {
        B_list strs = $NEW(B_list,NULL,NULL);
        B_list ix = B_listD_new(1);
        ix->length = 1;
        for (long i = 0; i< $LONGELEM(a->shape,0); i++) {
            ix->data[0] = numpyQ_ndindexG_new(to$int(i));
            numpyQ_ndarray b = numpyQ_ndarrayD___ndgetslice__(a,ix);
            wit->$class->append(wit,strs,numpyQ_ndarrayD___str__(b));
        }
        B_str s = B_strD_join_par('[',strs,']');
        B_Plus wit2 = (B_Plus)B_TimesD_strG_new();
        return wit2->$class->__add__(wit2,s,to$str("\n"));
    }
}

numpyQ_ndarray numpyQ_ndarrayG_new(numpyQ_Primitive wit, B_atom a) {
    return numpyQ_fromatom(wit, a);
}
//ndarray methods /////////////////////////////////////////////////////////////////////////////////

// reshape attempts to present a new view, but may have to copy data.

numpyQ_ndarray numpyQ_ndarray$reshape(numpyQ_ndarray a, B_list newshape) {
    long newsize = from$int($prod(newshape));
    if (from$int(a->size) != newsize)
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("wrong number of array elements for reshape")));
    if (a->shape->length == newshape->length) {
        // Check if newshape is actually equal to a->shape.
        int sameshape = 1;
        int i = 0;
        while (sameshape && i < newshape->length) {
            if ($LONGELEM(a->shape,i) != $LONGELEM(newshape,i))
                sameshape = 0;
            i++;
        }
        if (sameshape)
            return a;
    }
    // Check if we can reuse a->data or we need to build a fresh data array.
    // We only reuse a->data if the indices in a->data of the elements of a form an arithmetic sequence.
    int needcopy = 0;
    long currstride = $LONGELEM(a->strides,a->strides->length-1);
    int i = a->strides->length-2;
    while (!needcopy && i >= 0) {
        long nextstride = $LONGELEM(a->strides,i);
        if (nextstride != currstride *  $LONGELEM(a->shape,i+1))
            needcopy = 1;
        else {
            i--;
            currstride = nextstride;
        }
    }
    // Compute strides list of result. Computation starts from last element, which is computed
    // differently if we reuse a->data than if we make a fresh array.
    B_list newstrides = B_listD_new(newshape->length);
    newstrides->length = newshape->length;
    newstrides->data[newstrides->length-1] = needcopy ? to$int(1) : a->strides->data[a->strides->length-1];
    for (int i = newstrides->length-2; i>=0; i--)
        newstrides->data[i] = to$int($LONGELEM(newstrides,i+1) * $LONGELEM(newshape,i+1));
    // Build result
    numpyQ_ndarray res = G_newarray(a->elem_type,newshape->length,a->size,newshape,newstrides,false);
    if (!needcopy) {
        res->offset = a->offset;
        res->data = a->data;
        return res;
    } else {
        res->data = malloc(newsize * sizeof(union $Bytes8));
        union $Bytes8 *ixres = res->data;
        union $Bytes8 *ixa;
        numpyQ_array_iterator_state ita = $mk_iterator(a);
        while ((ixa = iter_next(ita))) {
            *ixres = *ixa;
            ixres++;
        }
        return res;
    }
}  

// permutes axes in a to the order given by axes.
// If second argument is NULL, reverse order of axes.
// Does not copy data; returns a new view.
numpyQ_ndarray numpyQ_ndarray$transpose(numpyQ_ndarray a, B_list axes) {
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    B_list newshape, newstrides;
    if (!axes) {
        newshape = B_listD_copy(a->shape);
        wit->$class->reverse(wit,newshape);
        newstrides = B_listD_copy(a->strides);
        wit->$class->reverse(wit,newstrides);
    } else {
        bool is_perm = true;
        if (axes->length != a->shape->length) is_perm = false;
        long i = axes->length-1;
        B_ContainerD_list wit = B_ContainerD_listG_new((B_Eq)B_OrdD_intG_new());
        while (is_perm && i >= 0) {
            if (!wit->$class->__contains__(wit,axes,to$int(i))) is_perm = false;
            i--;
        }
        if (!is_perm)
            $RAISE((B_BaseException)$NEW(B_ValueError,to$str("transpose: axes argument must be permutation of [0..a.ndim-1]")));
        newshape = B_listD_new(axes->length);
        newstrides = B_listD_new(axes->length);
        newshape->length = axes->length;
        newstrides->length = axes->length;
        for (long i = 0; i<axes->length; i++) {
            long n = from$int((B_int)axes->data[i]);
            newshape->data[i] = a->shape->data[n];
            newstrides->data[i] = a->strides->data[n];
        }
    }
    numpyQ_ndarray res = G_newarray(a->elem_type,a->ndim,a->size,newshape,newstrides,false);
    res->data = a->data;
    return res;
}

numpyQ_ndarray numpyQ_ndarray$flatten(numpyQ_ndarray a) {
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    B_list newshape = $NEW(B_list,NULL,NULL);
    wit->$class->append(wit,newshape,a->size);
    return numpyQ_ndarray$reshape(a,newshape);
}

// Makes a contiguous deep copy of its argument with stride of last dimension == 1.

numpyQ_ndarray numpyQ_ndarray$copy(numpyQ_ndarray a) {
    numpyQ_ndarray res = G_newarray(a->elem_type,a->ndim,a->size,a->shape,$mk_strides(a->shape),true);
    if ($is_contiguous(a))
        memcpy(res->data,a->data,from$int(a->size)*sizeof($WORD)); // Hackish; what is the proper size to use?
    else {
        numpyQ_array_iterator_state it = $mk_iterator(a);
        union $Bytes8 *ixres, *ixa;
        ixres = res->data;
        while ((ixa = iter_next(it))) {
            *ixres = *ixa;
            ixres++;
        }
    }
    return res;
}

// basic slicing 

numpyQ_ndarray numpyQ_ndarrayD___ndgetslice__(numpyQ_ndarray a, B_list ix) {
    // assert length of ix > 0
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    int nulls = 0;
    int ints = 0;
    int slices = 0;
    // first analyze index list contents
    for (int i=0; i < ix->length; i++) {
        numpyQ_ndselect ixi = (numpyQ_ndselect)ix->data[i];
        if ($ISINSTANCE(ixi,numpyQ_ndindex)->val) {
            if (((numpyQ_ndindex)ixi)->index==numpyQ_newaxis)
                nulls++;
            else
                ints++;
        } else if ($ISINSTANCE(ixi,numpyQ_ndslice)->val)
            slices++;
        else {
            fprintf(stderr,"internal error: unexpected type of ndarray index element\n");
            exit(-1);
        }
    }
    if (ints+slices > a->ndim)
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("indexing too many dimensions")));
    int ndim = a->ndim + nulls - ints;
    numpyQ_ndarray res = G_newarray(a->elem_type,ndim,0,B_listD_new(ndim),B_listD_new(ndim),false); // size 0 is temporary
    res->shape->length = ndim;
    res->strides->length = ndim;
    res->data = a->data;
    int offset = a->offset;
    B_slice allslice = $NEW(B_slice,NULL,NULL,NULL);
    int ixpos = ix->length-1;
    int apos = a->ndim-1;
    int respos = ndim-1;
    int untouched = a->ndim - (ints + slices);
    numpyQ_ndselect currindex;
    long currstride = a->elem_size;
    while (ixpos>=0) {
        if (untouched-- > 0)
            currindex = (numpyQ_ndselect)numpyQ_ndsliceG_new(allslice);
        else
            currindex = (numpyQ_ndselect)ix->data[ixpos--];
        if ($ISINSTANCE(currindex,numpyQ_ndindex)->val) {
            if (((numpyQ_ndindex)currindex)->index == numpyQ_newaxis) {
                wit->$class->__setitem__(wit,res->strides,to$int(respos),to$int(0));
                wit->$class->__setitem__(wit,res->shape,to$int(respos--),to$int(1));
            } else {
                currstride = $LONGELEM(a->strides,apos--);
                offset += currstride * from$int(((numpyQ_ndindex)currindex)->index);
            }
        } else if ($ISINSTANCE(currindex,numpyQ_ndslice)->val) {
            long slen,start,stop,step;
            normalize_slice(((numpyQ_ndslice)currindex)->slc,$LONGELEM(a->shape,apos),&slen,&start,&stop,&step);
            currstride = $LONGELEM(a->strides,apos);
            offset += start * currstride;
            wit->$class->__setitem__(wit,res->strides,to$int(respos),to$int($LONGELEM(a->strides,apos--) * step));
            wit->$class->__setitem__(wit,res->shape,to$int(respos--),to$int(slen));
        } else {
            fprintf(stderr,"internal error: unexpected type of ndarray index element\n");
            exit(-1);
        }
    }
    res->offset = offset;
    res->size = $prod(res->shape);
    return res;
}

struct numpyQ_ndarrayG_class numpyQ_ndarrayG_methods = {
    0,
    "numpyQ_ndarray",
    UNASSIGNED,
    ($SuperG_class)&B_valueG_methods,
    numpyQ_ndarrayD___init__,
    numpyQ_ndarrayD___serialize__,
    numpyQ_ndarrayD___deserialize__,
    numpyQ_ndarrayD___bool__,
    numpyQ_ndarrayD___str__,
    numpyQ_ndarrayD___str__,
    numpyQ_ndarray$reshape,
    numpyQ_ndarray$transpose,
    numpyQ_ndarray$flatten,
    numpyQ_ndarray$copy,
    numpyQ_ndarrayD___ndgetslice__
};

// Iterating over an ndarray //////////////////////////////////////////////////////////////////////////////

// Aims to be as fast as possible; used in many methods below and in protocol implementations 
numpyQ_array_iterator_state $mk_iterator(numpyQ_ndarray a) {
    numpyQ_array_iterator_state res = malloc(sizeof(numpyQ_array_iterator_state));
    if (a->ndim==0) {
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("cannot make iterator for 0-dim array")));
    }
    res->ndim1 = a->ndim-1;
    for (long i=res->ndim1; i>=0; i--) {
        res->shape[i] = from$int(a->shape->data[i]);
        res->strides[i] = from$int(a->strides->data[i]);
        res->index[i] = 0;
    }
    for (long i=res->ndim1; i>=1; i--) {
        res->jumps[i] = res->strides[i-1] - res->shape[i]*res->strides[i];
    }
    res->lastshapepos = -1;//index[res->ndim1]--; //place at "position -1"
    res->lastshapelength = res->shape[res->ndim1];
    res->currentstride = res->strides[res->ndim1];
    res->current = &a->data[a->offset]-res->currentstride;
    return res;
}

union $Bytes8 *iter_next(numpyQ_array_iterator_state it) {
    it->current += it->currentstride;
    if (++it->lastshapepos == it->lastshapelength) { 
        it->lastshapepos = 0;
        if (it->ndim1==0)
            it->current = NULL;
        else {
            long i = it->ndim1;
            it->current += it->jumps[i]; 
            i--;
            it->index[i]++;
            while (it->index[i]==it->shape[i]) {
                if (i==0) {
                    it->current = NULL;
                    break;
                } else {
                    it->index[i] = 0;
                    it->current += it->jumps[i];
                    it->index[i-1]++;
                    i--;
                }
            }
        }
    }
    return it->current;      
}
 
// Auxiliary function for mapping functions and operators over an ndarray ///////////////////////////////////////////

numpyQ_ndarray numpyQ_func(union $Bytes8(*f)(union $Bytes8),numpyQ_ndarray a) {
    B_list resstrides = $mk_strides(a->shape);
    numpyQ_ndarray res = G_newarray(a->elem_type,a->ndim,a->size,a->shape,resstrides,true);
    if (res->ndim == 0) 
        res->data[0] = f(a->data[a->offset]);
    else {
        union $Bytes8 *ixres = res->data;
        union $Bytes8 *ixa;
        numpyQ_array_iterator_state it = $mk_iterator(a);
        while ((ixa = iter_next(it))) {
            *ixres = f(*ixa);
            ixres++;
        }
    }
    return res;
}

// broadcasting to adapt two arrays to common shape before mapping anoperator.

// returns the common extended shape and, as outparams, iterators for the two extended operands

static numpyQ_ndarray numpyQ_broadcast(numpyQ_ndarray a1, numpyQ_ndarray a2, numpyQ_array_iterator_state *it1, numpyQ_array_iterator_state *it2) {
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    int len;
    B_list resshape, shape1, shape2, strides1, strides2;
    shape1 = B_listD_copy(a1->shape);
    shape2 = B_listD_copy(a2->shape);
    strides1 = B_listD_copy(a1->strides);
    strides2 = B_listD_copy(a2->strides);
    if (a1->ndim < a2->ndim) {
        for (int i=0; i< a2->ndim - a1->ndim; i++) {
            wit->$class->insert(wit,shape1,to$int(0),to$int(1));
            wit->$class->insert(wit,strides1,to$int(0),to$int(0));
        }
        len = a2->ndim;
    } else if (a2->ndim < a1->ndim) {
        for (int i=0; i< a1->ndim-a2->ndim; i++) {
            wit->$class->insert(wit,shape2,to$int(0),to$int(1));
            wit->$class->insert(wit,strides2,to$int(0),to$int(0));
        }
        len = a1->ndim;
    } else
        len = a1->ndim;
    resshape = B_listD_new(len);
    resshape->length = len;
    for (int i=0; i<len; i++) {
        if ($LONGELEM(shape1,i) == $LONGELEM(shape2,i))
            resshape->data[i] = shape1->data[i];
        else if ($LONGELEM(shape1,i) == 1)
            resshape->data[i] = shape2->data[i];
        else if ($LONGELEM(shape2,i) == 1)
            resshape->data[i] = shape1->data[i];
        else
            $RAISE((B_BaseException)$NEW(B_ValueError,to$str("ndarray broadcasting: shapes do not match")));
    }
    numpyQ_ndarray res = G_newarray(a1->elem_type,len,$prod(resshape),resshape,$mk_strides(resshape),true);
    numpyQ_ndarray tmparr = G_newarray(a1->elem_type,len,res->size,res->shape,strides1,false); // only for making iterators
    tmparr->offset = a1->offset; 
    tmparr->data = a1->data;
    *it1 = $mk_iterator(tmparr);
    tmparr->strides = strides2;
    tmparr->offset = a2->offset;
    tmparr->data = a2->data;
    *it2 = $mk_iterator(tmparr);
    return res;
}
 
numpyQ_ndarray numpyQ_oper(union $Bytes8 (*f)(union $Bytes8, union $Bytes8),numpyQ_ndarray a, numpyQ_ndarray b) {
    union $Bytes8 *ix1, *ix2, *ixres;
    long stride1, stride2, len;
    numpyQ_array_iterator_state it1, it2;
    numpyQ_ndarray res;
    if (a->ndim == 0 && b->ndim == 0) {
        res = G_newarray(a->elem_type,0,to$int(1),$NEW(B_list,NULL,NULL),$NEW(B_list,NULL,NULL),true);
        res->data[0] = f(a->data[a->offset],b->data[b->offset]);
        return res;
    }
    res = numpyQ_broadcast(a,b,&it1,&it2);
    ixres = res->data;
    while ((ix1 = iter_next(it1))) {
        ix2 = iter_next(it2);
        *ixres = f(*ix1,*ix2);
        ixres++;
    }
    return res; 
}



// fromatom /////////////////////////////////////////////////////////////////////////////

// The ndarray constructor takes an atomic argument and builds a 0-dimensional array.

numpyQ_ndarray numpyQ_fromatom(numpyQ_Primitive wit, B_atom a) {
    if ($ISINSTANCE(wit,numpyQ_PrimitiveD_int)->val) {
        numpyQ_ndarray res = G_newarray(LongType,0,to$int(1),$NEW(B_list,NULL,NULL),$NEW(B_list,NULL,NULL),true);
        B_IntegralD_int iwit = B_IntegralD_intG_witness;
        res->data->l = from$int(iwit->$class->__fromatom__(iwit, a));
        return res;
    } else {
        numpyQ_ndarray res = G_newarray(DblType,0,to$int(1),$NEW(B_list,NULL,NULL),$NEW(B_list,NULL,NULL),true);
        B_RealFloatD_float rwit = B_RealFloatD_floatG_new();
        res->data->d = rwit->$class->__fromatom__(rwit, a)->val;
        return res;
    }
}

// Functions to create arrays /////////////////////////////////////////////////////////////

// n evenly spaced floats between a and b

numpyQ_ndarray numpyQ_linspace(B_float a, B_float b, B_int n) {
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    B_list shape = $NEW(B_list,NULL,NULL);
    wit->$class->append(wit,shape,n);
    B_list strides = $NEW(B_list,NULL,NULL);
    wit->$class->append(wit,strides,to$int(1));
    numpyQ_ndarray res = G_newarray(DblType,1,n,shape,strides,true);
    double step = (b->val - a->val)/from$int(n);
    for (long i = 0; i<from$int(n); i++) {
        res->data[i].d = a->val + i * step;
    }
    return res;
}

// array of ints described by a range

numpyQ_ndarray numpyQ_arange(B_int start, B_int stop, B_int step) {
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    B_range r = $NEW(B_range,start,stop,step);
    long len = (r->stop - r->start)/r->step;
    B_list shape = $NEW(B_list,NULL,NULL);
    wit->$class->append(wit,shape,to$int(len));
    B_list strides = $NEW(B_list,NULL,NULL);
    wit->$class->append(wit,strides,to$int(1));
    numpyQ_ndarray res = G_newarray(LongType,1,to$int(len),shape,strides,true);
    long elem = r->start;
    for (int i=0; i < len; i++) {
        res->data[i].l = elem;
        elem += r->step;
    }
    return res;
}  

// make an array from a list

numpyQ_ndarray numpyQ_array(numpyQ_Primitive pwit, B_list elems) {
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    B_list shape = $NEW(B_list,NULL,NULL);
    wit->$class->append(wit,shape,to$int(elems->length));
    B_list strides = $NEW(B_list,NULL,NULL);
    wit->$class->append(wit,strides,to$int(1));
    if (elems->length == 0)
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("function numpy.array cannot create empty ndarray")));
    numpyQ_ndarray res = G_newarray(pwit->$class->elem_type,1,to$int(elems->length),shape,strides,true);
    for (int i=0; i<elems->length; i++) 
        res->data[i] = pwit->$class->from$obj(elems->data[i]);
    return res;
}

// create an array with given shape and all elements of given value

numpyQ_ndarray numpyQ_full(numpyQ_Primitive wit, B_list shape, $WORD val) {
    B_list strides = $mk_strides(shape);
    B_int size = $prod(shape);
    numpyQ_ndarray res = G_newarray(wit->$class->elem_type,shape->length,size,shape,strides,true);
    for (int i=0; i<from$int(size); i++)
        res->data[i] = wit->$class->from$obj(val);
    return res;
}

numpyQ_ndarray numpyQ_unirandint(B_int a, B_int b, B_int n) {
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    if (from$int(a) >= from$int(b))
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("lower limit not smaller than upper in numpy.unirand")));
    B_list shape = $NEW(B_list,NULL,NULL);
    wit->$class->append(wit,shape,n);
    B_list strides = $NEW(B_list,NULL,NULL);
    wit->$class->append(wit,strides,to$int(1));
    long s = (from$int(b) - from$int(a));
    numpyQ_ndarray res = G_newarray(DblType,1,n,shape,strides,true);
    for (int i = 0; i<from$int(n); i++)
        res->data[i].l = from$int(a) + arc4random_uniform(s);
    return res;
}

#define NDARRAY_MAX 1000000000
numpyQ_ndarray numpyQ_unirandfloat(B_float a, B_float b, B_int n) {
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    if (a->val >= b->val)
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("lower limit not smaller than upper in numpy.unirand")));
    B_list shape = $NEW(B_list,NULL,NULL);
    wit->$class->append(wit,shape,n);
    B_list strides = $NEW(B_list,NULL,NULL);
    wit->$class->append(wit,strides,to$int(1));
    double s = (b->val - a->val);
    numpyQ_ndarray res = G_newarray(DblType,1,n,shape,strides,true);
    for (int i = 0; i<from$int(n); i++)
        res->data[i].d = a->val + s * (double)arc4random_uniform(NDARRAY_MAX)/(double)NDARRAY_MAX;
    return res;
}

// Functions over arrays /////////////////////////////////////////////////

// Most of these are yet only defined with default parameters.

numpyQ_ndarray numpyQ_partition(numpyQ_Primitive wit, numpyQ_ndarray a, B_int k) {
    numpyQ_ndarray res = numpyQ_ndarray$copy(a);
    res->ndim--; 
    numpyQ_array_iterator_state it = $mk_iterator(res); //gives an iterator that successively selects start of each last dimension column.
    res->ndim++;
    for (int i=0; i < $LONGELEM(res->shape,res->ndim-2); i++) {
        union $Bytes8 *start =iter_next(it);
        quickselect(start,0,$LONGELEM(res->shape,res->ndim-1)-1,from$int(k),wit->$class->$lt);
    }
    return res;
}

numpyQ_ndarray numpyQ_sort(numpyQ_Primitive pwit, numpyQ_ndarray a, B_int axis) {
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    numpyQ_ndarray res = numpyQ_ndarray$copy(a);
    if (!axis) {
        quicksort(res->data,0,from$int(res->size)-1,pwit->$class->$lt);
        B_list newshape = B_listD_new(1);
        wit->$class->append(wit,newshape,a->size);
        B_list newstrides = B_listD_new(1);
        wit->$class->append(wit,newstrides,to$int(1));
        res->shape = newshape;
        res->strides = newstrides;
        res->ndim = 1;
    } else if (from$int(axis) == -1 || from$int(axis) == a->ndim-1) {
        res->ndim--;
        numpyQ_array_iterator_state it = $mk_iterator(res); //gives an iterator that successively selects start of each last dimension column.
        res->ndim++;
        union $Bytes8 *start;
        while ((start = iter_next(it))) 
            quicksort(start,0,$LONGELEM(res->shape,res->ndim-1)-1,pwit->$class->$lt);
    } else
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("sorting only implemented for complete array or along last axis")));
    return res;
}


numpyQ_ndarray numpyQ_clip(numpyQ_Primitive pwit, numpyQ_ndarray a, $WORD low, $WORD high) {
    numpyQ_ndarray res = numpyQ_ndarray$copy(a);
    numpyQ_array_iterator_state it = $mk_iterator(res);
    union $Bytes8 lo, hi, x, *ix, *ixres = res->data;
    if (low) lo = pwit->$class->from$obj(low);
    if (high) hi = pwit->$class->from$obj(high);
    while((ix = iter_next(it))) {
        x = *ix;
        if (low && pwit->$class->$lt(x,lo)) 
            *ixres = lo;
        else if (high && pwit->$class->$lt(hi,x))
            *ixres = hi;
        else
            *ixres = x;
        ixres++;
    }
    return res;
}

union $Bytes8 $dot1dim(numpyQ_Primitive wit, union $Bytes8 *a, union $Bytes8 *b, long size, long stridea, long strideb) {
    // a has stride 1 since it is copied except for dot(a,b) when both a and b are 1dim
    union $Bytes8 res = (union $Bytes8)0L;
    union $Bytes8 *ixa, *ixb, *ixaend;
    ixa = a;
    ixaend = ixa + size*stridea;
    ixb = b;
    while (ixa < ixaend) {
        wit->$class->$iadd(&res,wit->$class->$mul(*ixa,*ixb));
        ixa += stridea;
        ixb += strideb;
    }
    return res;
}
 
numpyQ_ndarray numpyQ_dot(numpyQ_Primitive pwit, numpyQ_ndarray a, numpyQ_ndarray b) {
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    numpyQ_ndarray res;
    if (a->ndim==0 || b->ndim==0) {
        // following  Python's numpy, we multiply elementwise...
        numpyQ_RealD_ndarray wit2 = $NEW(numpyQ_RealD_ndarray,pwit);
        res = wit2->$class->__mul__(wit2,a,b);
    } else if (b->ndim==1) {
        long len = $LONGELEM(b->shape,0);
        if  ($LONGELEM(a->shape,a->ndim-1) != len)
            $RAISE((B_BaseException)$NEW(B_ValueError,to$str("array sizes in numpy.dot do not match")));
        long stridea = $LONGELEM(a->strides,a->ndim-1);
        long strideb = $LONGELEM(b->strides,0);
        B_list newshape = wit->$class->__getslice__(wit,a->shape,$NEW(B_slice,NULL,to$int(-1),NULL));
        res = G_newarray(a->elem_type,a->ndim-1,$prod(newshape),newshape,$mk_strides(newshape),true);
        if (a->ndim==1) {
            res->data[0] = $dot1dim(pwit, &a->data[a->offset],&b->data[b->offset],len,stridea,strideb);
        } else {
            union $Bytes8 *ixres = res->data, *ixa;
            a->ndim--;
            numpyQ_array_iterator_state ita = $mk_iterator(a);
            a->ndim++;
            while ((ixa = iter_next(ita))) {
                *ixres = $dot1dim(pwit,ixa,b->data,len,stridea,strideb);
                ixres++;
            }
        }
    } else { //b->ndim > 1. Following Python we should do a sum product over last axis of a and 2nd to last axis of b ??????
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("numpy.dot not implemented for ndim of second arg > 1")));    
    } 
    return res; 
}

union $Bytes8 B_sum1dim(numpyQ_Primitive wit, union $Bytes8 *a, long size, long stridea) { 
    union $Bytes8 res = (union $Bytes8)0L;
    union $Bytes8 *ixa, *ixaend;
    ixa = a;
    ixaend = ixa + size*stridea;
    while (ixa < ixaend) {
        wit->$class->$iadd(&res,*ixa);
        ixa += stridea;
    }
    return res;
}


numpyQ_ndarray numpyQ_sum(numpyQ_Primitive pwit, numpyQ_ndarray a, B_int axis) {
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    numpyQ_ndarray res;
    long laxis;
    if(axis) {
        laxis = from$int(axis) < 0 ? from$int(axis) + a->ndim : from$int(axis);
        if (laxis < 0 || laxis >= a->ndim)
            $RAISE((B_BaseException)$NEW(B_ValueError,to$str("illegal axis in numpy.sum")));
    }
    if(!axis || a->ndim == 1) {
        union $Bytes8 resd = (union $Bytes8) 0L;
        if ($is_contiguous(a)) {
            for (int i=0; i<from$int(a->size); i++)
                pwit->$class->$iadd(&resd,a->data[i]); 
        } else {
            numpyQ_array_iterator_state it = $mk_iterator(a);
            union $Bytes8 *ixa;
            while ((ixa = iter_next(it)))
                pwit->$class->$iadd(&resd,*ixa);
        }
        res = G_newarray(a->elem_type,0,to$int(1),$NEW(B_list,NULL,NULL),$NEW(B_list,NULL,NULL),true);
        res->data[0] = resd;
    } else {
        long len = $LONGELEM(a->shape,laxis);
        long stridea = $LONGELEM(a->strides,laxis);
        B_list newshape = B_listD_copy(a->shape);
        wit->$class->__delitem__(wit,newshape,to$int(laxis));
        B_list newstrides = B_listD_copy(a->strides);
        wit->$class->__delitem__(wit,newstrides,to$int(laxis));
        numpyQ_ndarray a1 = G_newarray(a->elem_type,a->ndim-1,$prod(newshape),newshape,newstrides,false);
        a1->offset = a->offset;
        res = G_newarray(a->elem_type,a->ndim-1,$prod(newshape),newshape,$mk_strides(newshape),true);
        a1->data = a->data;
        numpyQ_array_iterator_state ita = $mk_iterator(a1);
        union $Bytes8 *ixa;
        union $Bytes8 *ixres = res->data;
        while ((ixa = iter_next(ita))) {
            *ixres = B_sum1dim(pwit,ixa,len,stridea);
            ixres++;
        }
    }
    return res;
}         

numpyQ_ndarray numpyQ_abs(numpyQ_Primitive wit, numpyQ_ndarray a) {
    return numpyQ_func(wit->$class->B_abs,a);
}

$WORD numpyQ_scalar (numpyQ_Primitive wit, numpyQ_ndarray a) {
    if (a->ndim > 0)
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("scalar only for zero-dim arrays")));
    return wit->$class->to$obj(a->data[a->offset]);
}


// Iterator over ndarrays ////////////////////////////////////////////////////////////////////////

B_NoneType numpyQ_IteratorD_init(numpyQ_IteratorD_ndarray self, numpyQ_Primitive pwit, numpyQ_ndarray a) {
    self->pwit = pwit;
    self->it = $mk_iterator(a);
    return B_None;
}

numpyQ_IteratorD_ndarray numpyQ_IteratorD_ndarrayG_new(numpyQ_Primitive pwit, numpyQ_ndarray a) {
    numpyQ_IteratorD_ndarray res = malloc(sizeof(numpyQ_IteratorD_ndarray ));
    res->$class = &numpyQ_IteratorD_ndarrayG_methods;
    numpyQ_IteratorD_init(res,pwit,a);
    return res;
}

B_bool numpyQ_IteratorB_bool(numpyQ_IteratorD_ndarray self) {
    return B_True;
}

B_str numpyQ_IteratorB_str(numpyQ_IteratorD_ndarray self) {
    char *s;
    asprintf(&s,"<ndarray iterator object at %p>",self);
    return to$str(s);
}

$WORD numpyQ_IteratorD_ndarrayD___next__(numpyQ_IteratorD_ndarray self) {
    union $Bytes8 *n = iter_next(self->it);
    return n ? self->pwit->$class->to$obj(*n) : NULL;
}

void numpyQ_IteratorD_$serialize(numpyQ_IteratorD_ndarray self,$Serial$state state) {
    $RAISE((B_BaseException)$NEW(B_ValueError,to$str("(de)serialization not implemented for ndarray iterators")));
}

numpyQ_IteratorD_ndarray numpyQ_IteratorD_ndarray$_deserialize(numpyQ_IteratorD_ndarray self,$Serial$state state) {
    $RAISE((B_BaseException)$NEW(B_ValueError,to$str("(de)serialization not implemented for ndarray iterators")));
    return NULL;
}

struct numpyQ_IteratorD_ndarrayG_class numpyQ_IteratorD_ndarrayG_methods = {0,"",UNASSIGNED,($SuperG_class)&B_IteratorG_methods, numpyQ_IteratorD_init,                                                                        numpyQ_IteratorD_$serialize, numpyQ_IteratorD_ndarray$_deserialize,numpyQ_IteratorB_bool,
                                                                        numpyQ_IteratorB_str,numpyQ_IteratorB_str,numpyQ_IteratorD_ndarrayD___next__};

numpyQ_ndarray numpyQ_roll(numpyQ_Primitive pwit, numpyQ_ndarray a, B_int n) {
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    if (from$int(n)==0)
        return a;
    numpyQ_ndarray b = numpyQ_ndarray$flatten(a);
    B_list newshape = B_listD_new(1);
    wit->$class->append(wit,newshape,a->size);
    B_list newstrides = B_listD_new(1);
    wit->$class->append(wit,newstrides,to$int(1));
    numpyQ_ndarray res = G_newarray(b->elem_type,1,b->size,newshape,newstrides,true);
    numpyQ_CollectionD_ndarray wit2 = numpyQ_CollectionD_ndarrayG_new(pwit);
    int len = from$int(wit2->$class->__len__(wit2,a));
    int nval = from$int(n);
    int start = nval < 0 ? -nval : len-nval;
    int stride =  from$int((B_int)b->strides->data[0]);
    for (int i = 0; i < from$int(b->size); i++)
        res->data[i] = b->data[b->offset + (start+i) %from$int( b->size) * stride];
    return numpyQ_ndarray$reshape(res,a->shape);
}

union $Bytes8 $convert_to_double(union $Bytes8 a) {
    union  $Bytes8 res;
    res.d = (double)a.l;
    return res;
}
   
numpyQ_ndarray numpyQ_mean(numpyQ_Primitive pwit, numpyQ_ndarray a, B_int axis) {
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    numpyQ_ndarray sums = numpyQ_sum(pwit,a,axis);
    if (a->elem_type == LongType) {
        sums = numpyQ_func($convert_to_double,sums);
        sums->elem_type = DblType;
    }
    long x = from$int((B_int)wit->$class->__getitem__(wit,a->shape,axis));
    numpyQ_ndarray len = numpyQ_fromatom((numpyQ_Primitive)numpyQ_PrimitiveD_floatG_witness,
                                         (B_atom)to$float((double)(axis ? x : from$int(a->size))));
    return numpyQ_oper(B_d_truediv,sums,len);
}
  
   

  
numpyQ_ndarray numpyQ_tile(numpyQ_Primitive pwit, numpyQ_ndarray a, B_int n) {
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    if (from$int(n)<=0) 
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("numpy.tile: non-positive number of tiles")));
    B_int sz = to$int(from$int(a->size)*from$int(n));
    B_list newshape = B_listD_new(1);
    wit->$class->append(wit,newshape,sz);
    B_list newstrides = B_listD_new(1);
    wit->$class->append(wit,newstrides,to$int(1));
    numpyQ_ndarray res = G_newarray(a->elem_type,1,sz,newshape,newstrides,true);
    for (int i=0; i < from$int(sz); i++) 
        res->data[i] = a->data[a->offset + i % from$int(a->size)];
    return res;    
}

numpyQ_ndarray numpyQ_zeros(numpyQ_Primitive pwit, B_int n) {
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    if (from$int(n)<=0) 
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("numpy.zeros: non-positive size")));
    B_list newshape = B_listD_new(1);
    wit->$class->append(wit,newshape,n);
    B_list newstrides = B_listD_new(1);
    wit->$class->append(wit,newstrides,to$int(1));
    numpyQ_ndarray res = G_newarray(pwit->$class->elem_type,1,n,newshape,newstrides,true);
    union $Bytes8 zero;
    switch (pwit->$class->elem_type) {
    case LongType:
        zero.l = 0;
        break;
    case DblType:
        zero.d = 0.0;
        break;
    }
    for (int i=0; i<from$int(n); i++)
        res->data[i] = zero;
    return res;
}

numpyQ_ndarray numpyQ_concatenate(numpyQ_Primitive pwit, B_list as) {
    int size = 0;
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    for (int i = 0; i < as->length; i++)
        size += from$int(((numpyQ_ndarray)as->data[i])->size);
    B_int sz = to$int(size);
    B_list newshape = B_listD_new(1);
    wit->$class->append(wit,newshape,sz);
    B_list newstrides = B_listD_new(1);
    wit->$class->append(wit,newstrides,to$int(1));
    numpyQ_ndarray res = G_newarray(pwit->$class->elem_type,1,sz,newshape,newstrides,true);
    size = 0;
    for (int i=0; i < as->length; i++) {
        numpyQ_ndarray a = (numpyQ_ndarray)as->data[i];
        for (int j=0; j < from$int(a->size); j++)
            res->data[size+j] = a->data[a->offset+j*from$int((B_int)wit->$class->__getitem__(wit,a->strides,to$int(-1)))];
        size +=  from$int(a->size);
    }
    return res;
}
