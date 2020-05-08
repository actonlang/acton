void $tuple_serialize($tuple self, $Mapping$dict wit, $WORD *prefix, int prefix_size, $dict done, struct $ROWLISTHEADER *accum) {
}

$tuple $tuple_deserialize($Mapping$dict wit, $ROW *row, $dict done) {
    return NULL;
}

struct $tuple$class $tuple$methods = {
    "tuple",
    $tuple_serialize,
    $tuple_deserialize
};
