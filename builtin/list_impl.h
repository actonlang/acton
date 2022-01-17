
void $list_init($list lst, $Iterable wit, $WORD seq);

$list $list_copy($list lst);

$list $list_add($list lst, $list other);
$list $list_mul($list lst, $int n);

$Iterator $list_iter($list lst);

$list $list_fromiter($Iterator it);
long $list_len($list lst);

int $list_contains($Eq w, $list lst, $WORD elem);
int $list_containsnot($Eq w, $list lst, $WORD elem);

$WORD $list_getitem($list lst, int ix);
void $list_setitem($list lst, int ix, $WORD val);
void $list_delitem($list lst, int ix);

$list $list_getslice($list lst, $slice slc);
void $list_setslice($list lst, $slice slc, $Iterator it);
void $list_delslice($list lst, $slice slc);

void $list_append($list lst, $WORD val);
$Iterator $list_reversed($list lst);
void $list_insert($list lst, int ix, $WORD val);
void $list_reverse($list lst);

$list $list_new(int capacity);
