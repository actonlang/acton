
void B_listD_init(B_list lst, B_Iterable wit, $WORD seq);

B_list B_listD_copy(B_list lst);

B_list B_listD_add(B_list lst, B_list other);
B_list B_listD_mul(B_list lst, B_int n);

B_Iterator B_listD_iter(B_list lst);

B_list B_listD_fromiter(B_Iterator it);
long B_listD_len(B_list lst);

int B_listD_contains(B_Eq w,B_list lst, $WORD elem);
int B_listD_containsnot(B_Eq w, B_list lst, $WORD elem);

$WORD B_listD_getitem(B_list lst, int ix);
void B_listD_setitem(B_list lst, int ix, $WORD val);
void B_listD_delitem(B_list lst,int ix);

B_list B_listD_getslice(B_list lst, B_slice slc);
void B_listD_setslice(B_list lst, B_slice slc, B_Iterator it);
void B_listD_delslice(B_list lst, B_slice slc);

void B_listD_append(B_list lst, $WORD val);
B_Iterator B_listD_reversed(B_list lst);
void B_listD_insert(B_list lst, int ix, $WORD val);
void B_listD_reverse(B_list lst);

B_list B_listD_new(int capacity);
