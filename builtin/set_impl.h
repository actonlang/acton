Iterator $set_iter($set set);
Iterator $set_iter_entry($set set);

$set $set_fromiter(Hashable wit, Iterator it);
long $set_len($set set);

int $set_contains($set set, $WORD elem);

int $set_isdisjoint($set set, $set other);
void $set_add($set set, $WORD elem);
void $set_discard($set set, $WORD elem);
$WORD $set_pop($set set);


int $set_eq($set set, $set other);

int $set_lt($set set, $set other);
int $set_le($set set, $set other);
int $set_gt($set set, $set other);
int $set_ge($set set, $set other);

$set $set_intersection($set set, $set other);
$set $set_union($set set, $set other);
$set $set_symmetric_difference($set set, $set other);

$set $set_difference($set set, $set other);
