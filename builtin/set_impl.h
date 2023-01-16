B_Iterator B_set_iter(B_set set);
//B_Iterator B_set_iter_entry(B_set set);
B_set B_set_copy(B_set set, B_Hashable hashwit);

B_set B_set_fromiter(B_Hashable hashwit, B_Iterator it);
long B_set_len(B_set set);

int B_set_contains(B_set set, B_Hashable hashwit, $WORD elem);

int B_set_isdisjoint(B_Hashable hashwit, B_set set, B_set other);
void B_set_add(B_set set, B_Hashable hashwit, $WORD elem);
void B_set_discard(B_set set, B_Hashable hashwit, $WORD elem);
$WORD B_set_pop(B_set set);


int B_set_eq(B_Hashable hashwit, B_set set, B_set other);

int B_set_lt(B_Hashable hashwit, B_set set, B_set other);
int B_set_le(B_Hashable hashwit, B_set set, B_set other);
int B_set_gt(B_Hashable hashwit, B_set set, B_set other);
int B_set_ge(B_Hashable hashwit, B_set set, B_set other);

B_set B_set_intersection(B_Hashable hashwit, B_set set, B_set other);
B_set B_set_union(B_Hashable hashwit, B_set set, B_set other);
B_set B_set_symmetric_difference(B_Hashable hashwit, B_set set, B_set other);

B_set B_set_difference(B_Hashable hashwit, B_set set, B_set other);
