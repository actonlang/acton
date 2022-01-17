$Iterator $set_iter($set set);
//$Iterator $set_iter_entry($set set);
$set $set_copy($set set, $Hashable hashwit);

$set $set_fromiter($Hashable hashwit, $Iterator it);
long $set_len($set set);

int $set_contains($set set, $Hashable hashwit, $WORD elem);

int $set_isdisjoint($Hashable hashwit, $set set, $set other);
void $set_add($set set, $Hashable hashwit, $WORD elem);
void $set_discard($set set, $Hashable hashwit, $WORD elem);
$WORD $set_pop($set set);

int $set_eq($Hashable hashwit, $set set, $set other);

int $set_lt($Hashable hashwit, $set set, $set other);
int $set_le($Hashable hashwit, $set set, $set other);
int $set_gt($Hashable hashwit, $set set, $set other);
int $set_ge($Hashable hashwit, $set set, $set other);

$set $set_intersection($Hashable hashwit, $set set, $set other);
$set $set_union($Hashable hashwit, $set set, $set other);
$set $set_symmetric_difference($Hashable hashwit, $set set, $set other);

$set $set_difference($Hashable hashwit, $set set, $set other);
