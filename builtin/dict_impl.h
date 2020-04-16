$dict $new_dict(Hashable wit);
Hashable $dict_hashwitness($dict);
Iterator $dict_iter($dict dict);

void $dict_setitem($dict dict, $WORD key, $WORD value);
$WORD $dict_getitem($dict dict, $WORD key);
void $dict_delitem($dict dict,  $WORD key);

long $dict_len($dict dict);
$dict $dict_fromiter(Hashable wit, Iterator it);

int $dict_contains($dict dict, $WORD key);

$WORD $dict_get($dict dict, $WORD key, $WORD deflt);
$WORD $dict_popitem($dict dict);
void $dict_update($dict dict, Iterator it);
$WORD $dict_setdefault($dict dict, $WORD key, $WORD deflt);
Iterator $dict_keys($dict dict);
Iterator $dict_values($dict dict);
Iterator $dict_items($dict dict);
