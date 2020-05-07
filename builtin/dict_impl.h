void $dict_init($dict dict, $Hashable hashwit, $Iterable$opaque it);
$Iterator $dict_iter($dict dict);

void $dict_setitem($dict dict, $Hashable hashwit, $WORD key, $WORD value);
$WORD $dict_getitem($dict dict, $Hashable hashwit, $WORD key);
void $dict_delitem($dict dict, $Hashable hashwit, $WORD key);

long $dict_len($dict dict);
$dict $dict_fromiter($Hashable hashwit, $Iterable$opaque it);

int $dict_contains($dict dict, $Hashable hashwit, $WORD key);

$WORD $dict_get($dict dict, $Hashable hashwit, $WORD key, $WORD deflt);
$WORD $dict_popitem($dict dict, $Hashable hashwit);
void $dict_update($dict dict, $Hashable hashwit, $Iterable$opaque it);
$WORD $dict_setdefault($dict dict, $Hashable hashwit, $WORD key, $WORD deflt);
$Iterator $dict_keys($dict dict);
$Iterator $dict_values($dict dict);
$Iterator $dict_items($dict dict);
