int $lookdict(B_dict dict, B_Hashable hashwit, long hash, $WORD key, $WORD *res);

B_Iterator B_dictD_iter(B_dict dict);
B_str B_dictD_str(B_dict);

void B_dictD_setitem(B_dict dict, B_Hashable hashwit, $WORD key, $WORD value);
$WORD B_dictD_getitem(B_dict dict, B_Hashable hashwit, $WORD key);
void B_dictD_delitem(B_dict dict, B_Hashable hashwit, $WORD key);

B_dict B_dictD_fromiter(B_Hashable hashwit, B_Iterator it);
long B_dictD_len(B_dict dict);

int B_dictD_contains(B_dict dict, B_Hashable hashwit, $WORD key);

$WORD B_dictD_get(B_dict dict, B_Hashable hashwit, $WORD key, $WORD deflt);
B_tuple B_dictD_popitem(B_dict dict, B_Hashable hashwit);
void B_dictD_update(B_dict dict, B_Hashable hashwit, B_Iterator it);
$WORD B_dictD_setdefault(B_dict dict, B_Hashable hashwit, $WORD key, $WORD deflt);
B_Iterator B_dictD_keys(B_dict dict);
B_Iterator B_dictD_values(B_dict dict);
B_Iterator B_dictD_items(B_dict dict);

