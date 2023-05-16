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

#define BUF_SIZE 8192

// Queue implementation //////////////////////////////////////////////////////////////////

void $enqueue($Serial$state state, $ROW elem) {
    if (state->row)
        state->row->next = elem;
    else
        state->fst = elem;
    state->row = elem;
}
void $enqueue2(struct $ROWLISTHEADER *header, $ROW elem) {
    if (header->last)
        header->last->next = elem;
    else
        header->fst = elem;
    header->last = elem;
}
 
// Hashable$WORD methods //////////////////////////////////////////////////

void B_HashableD_WORDD___serialize__(B_HashableD_WORD self, $Serial$state state) {
}

B_HashableD_WORD B_HashableD_WORDD___deserialize__(B_HashableD_WORD self, $Serial$state state) {
    B_HashableD_WORD res = $DNEW(B_HashableD_WORD,state);
    return res;
}

B_bool B_HashableD_WORD_eq(B_HashableD_WORD wit, $WORD a, $WORD b) {
    return toB_bool(a==b);
}

B_bool B_HashableD_WORD_ne(B_HashableD_WORD wit, $WORD a, $WORD b) {
    return  toB_bool(a != b);
}

B_int B_HashableD_WORD_hash(B_HashableD_WORD wit, $WORD a) {
    return to$int($pointer_hash(a));
}

struct B_HashableD_WORDG_class B_HashableD_WORDG_methods = {
    0,
    "B_HashableD_WORD",
    UNASSIGNED,
    ($SuperG_class)&B_HashableG_methods,
    (void (*)(B_HashableD_WORD))$default__init__,
    B_HashableD_WORDD___serialize__,
    B_HashableD_WORDD___deserialize__,
    (B_bool (*)(B_HashableD_WORD))$default__bool__,
    (B_str (*)(B_HashableD_WORD))$default__str__,
    (B_str (*)(B_HashableD_WORD))$default__str__,
    B_HashableD_WORD_eq,
    B_HashableD_WORD_ne,
    B_HashableD_WORD_hash
};
struct B_HashableD_WORD B_HashableD_WORD_instance = {&B_HashableD_WORDG_methods};
struct B_HashableD_WORD *B_HashableD_WORDG_witness = &B_HashableD_WORD_instance;


// small-step functions for (de)serializing the next object /////////////////////////////////////////////////

$ROW $add_header(int class_id, int blob_size, $Serial$state state) {
    $ROW res = malloc(2 * sizeof(int) + (1+blob_size)*sizeof($WORD));
    res->class_id = class_id;
    state->row_no++;
    res->blob_size = blob_size;
    res->next = NULL;
    $enqueue(state,res);
    return res;
}

void $step_serialize($WORD self, $Serial$state state) {
    if (self) {
        int class_id = $GET_CLASSID((($Serializable)self)->$class);
        if (class_id > ITEM_ID) { // not one of the Acton builtin datatypes, which have hand-crafted serializations
            if (state->globmap) {
                long key = (long)state->globmap(self);
                if (key < 0) {
                    $val_serialize(-class_id,&key,state);
                    return;
                }
            }
            B_int prevkey = (B_int)B_dictD_get(state->done,(B_Hashable)B_HashableD_WORDG_witness,self,NULL);
            if (prevkey) {
                $val_serialize(-class_id,&prevkey->val,state);
            } else {
                B_dictD_setitem(state->done,(B_Hashable)B_HashableD_WORDG_witness,self,to$int(state->row_no));
                $add_header(class_id,0,state);
                (($Serializable)self)->$class->__serialize__(self,state);
            }
        } else 
            (($Serializable)self)->$class->__serialize__(self,state);
    } else
        $add_header(NONE_ID,0,state);
}

$WORD $step_deserialize($Serial$state state) {
    if (abs(state->row->class_id) > ITEM_ID) {
        $ROW this = state->row;
        state->row = this->next;
        state->row_no++;
        if (this->class_id < 0) {
            long key = (long)this->blob[0];
            if (key < 0)
                return state->globmap(($WORD)key);
            else
                return B_dictD_get(state->done,(B_Hashable)B_HashableD_intG_witness,to$int(key),NULL);
        } else
            return $GET_METHODS(this->class_id)->__deserialize__(NULL, state);
    } else
        return $GET_METHODS(abs(state->row->class_id))->__deserialize__(NULL, state);
}



void $val_serialize(int class_id, $WORD val,$Serial$state state) {
    $ROW row = $add_header(class_id,1,state);
    memcpy(row->blob,val,sizeof($WORD));
}

$WORD $val_deserialize($Serial$state state) {
    $WORD res;
    memcpy(&res,(state->row)->blob,sizeof($WORD));
    state->row = state->row->next;
    state->row_no++;
    return res;
}

// Serialization methods ///////////////////////////////////////////////////////////////////////////////

void $write_serialized($ROW row, char *file) {
    char buf[BUF_SIZE];
    char *p = buf;
    char *bufend = buf + BUF_SIZE;
    FILE *fileptr = fopen(file,"wb");
    int chunk_size;
    char *start;
    while(row) {
        chunk_size = 2*sizeof(int);
        start = (char*)row;
        if (p+chunk_size > bufend) {
            int fits = bufend - p;
            memcpy(p,start,fits);
            fwrite(buf,1,sizeof(buf),fileptr); // TODO:  handle file write error
            p = buf;
            chunk_size -= fits;
            start += fits;
        }
        memcpy(p,start,chunk_size);
        p+=chunk_size;
    
        chunk_size = (row->blob_size)*sizeof($WORD);
        start =  (char*)row->blob;
        while (p+chunk_size > bufend) {
            int fits = bufend - p;
            memcpy(p,start,fits);
            fwrite(buf,1,sizeof(buf),fileptr); // TODO:  handle file write error
            p = buf;
            chunk_size -= fits;
            start += fits;
        }
        memcpy(p,start,chunk_size);
        p+=chunk_size;
    
        row = row->next;
    }
    fwrite(buf,1,p-buf,fileptr); // TODO:  handle file write error
    fclose(fileptr);
}
 
$ROW $serialize($Serializable s, $WORD (*globmap)($WORD)) {
    $Serial$state state = malloc(sizeof(struct $Serial$state));
    state->done = $NEW(B_dict,(B_Hashable)B_HashableD_WORDG_witness,NULL,NULL);
    state->globmap = globmap;
    state->row_no=0;
    state->row = NULL;
    state->fst = NULL;
    $step_serialize(s,state);
    return state->fst;
}

$ROW $glob_serialize($Serializable self, $WORD (*globmap)($WORD)) {
    $Serial$state state = malloc(sizeof(struct $Serial$state));
    state->done = $NEW(B_dict,(B_Hashable)B_HashableD_WORDG_witness,NULL,NULL);
    state->globmap = globmap;
    state->row_no=0;
    state->row = NULL;
    state->fst = NULL;
    $add_header(self->$class->$class_id,0,state);
    self->$class->__serialize__(self,state);
    return state->fst;
}

void $serialize_file($Serializable s, char *file) {
    $write_serialized($serialize(s,NULL),file);
}

$Serializable $deserialize($ROW row, $WORD (*globmap)($WORD)) {
    $Serial$state state = malloc(sizeof(struct $Serial$state));
    state->done = $NEW(B_dict,(B_Hashable)B_HashableD_intG_witness,NULL,NULL);
    state->globmap = globmap;
    state->row_no=0;
    state->row = row;
    state->fst = NULL;
    return $step_deserialize(state);
}

$Serializable $glob_deserialize($Serializable self, $ROW row, $WORD (*globmap)($WORD)) {
    $Serial$state state = malloc(sizeof(struct $Serial$state));
    state->done = $NEW(B_dict,(B_Hashable)B_HashableD_intG_witness,NULL,NULL);
    state->globmap = globmap;
    state->row_no=1;
    state->row = row->next;
    state->fst = NULL;
    return self->$class->__deserialize__(self,state);
}

$ROW $read_serialized(char *file) {
    char buf[BUF_SIZE];
    char *p = buf;
    char *bufend;
    FILE *fileptr = fopen(file,"rb");
    int chunk_size;
    char *start;
    struct $ROWLISTHEADER header;
    header.fst = NULL;
    header.last = NULL;
    bufend = buf + fread(buf,1,sizeof(buf),fileptr);
    while(p < bufend || !feof(fileptr)) {
        int init[2];//4
        chunk_size = 2*sizeof(int); 
        start = (char*)init;
        if (p + chunk_size > bufend) {
            int fits = bufend - p;
            memcpy(start,p,fits);
            bufend = buf + fread(buf,1,sizeof(buf),fileptr); // TODO:  handle file write error
            p = buf;
            chunk_size -= fits;
            start += fits;
        }
        memcpy(start,p,chunk_size);
        p+=chunk_size;
        $ROW row = malloc(2*sizeof(int) + (init[1]+1) * sizeof($WORD));
        memcpy(row,init,2*sizeof(int));//4
        row->next = NULL;
        chunk_size =  (init[1]) * sizeof($WORD);
        start = (char*)row->blob;
        while (p + chunk_size > bufend) {
            int fits = bufend - p;
            memcpy(start,p,fits);
            bufend = buf + fread(buf,1,sizeof(buf),fileptr); // TODO:  handle file write error
            p = buf;
            chunk_size -= fits;
            start += fits;
        }
        memcpy(start,p,chunk_size);
        p+=chunk_size;
        $enqueue2(&header,row);
    }
    return header.fst;
}
       
$Serializable $deserialize_file(char *file) {
    return $deserialize($read_serialized(file), NULL);
}
