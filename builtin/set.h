
struct $set;

typedef struct $set *$set;

$set $set_new(Hashable$__class__ h);
void $set_add($set set, $WORD elem);     // never fails
void $set_remove($set set, $WORD elem);   // raises KEYERROR if elem not in s
void $set_discard($set set, $WORD elem); // does not fail if elem not in s
$WORD $set_pop($set set);                // removes an unspecified element and returns it; returns KEYERROR if empty
$bool $set_contains($set set, $WORD elem);
$bool $set_isdisjoint($set set, $set other);
Iterator $set_iter($set set); 
$int $set_len($set set);

$bool $set_eq($set set, $set other);
$bool $set_ne($set set, $set other);
$bool $set_lt($set set, $set other);
$bool $set_le($set set, $set other);
$bool $set_gt($set set, $set other);
$bool $set_ge($set set, $set other);

$bool $set_sub($set set, $set other);

$set $set_union($set set, $set other);
$set $set_intersection($set set, $set other);
$set $set_symmetric_difference($set set, $set other);

Eq$__class__ Eq$set_instance;
Ord$__class__ Ord$set_instance;
Logical$__class__ Logical$set_instance;
Minus$__class__ Minus$set_instance;
Collection$__class__ Collection$set_instance;
Iterable$__class__ Iterable$set_instance;
Set$__class__ Set$set_instance;
Container_Eq$__class__ Container_Eq$set_instance;
