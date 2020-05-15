void $tuple_serialize($tuple self, $Serial$state state) {
}

$tuple $tuple_deserialize($Serial$state state) {
    return NULL;
}

struct $tuple$class $tuple$methods = {
    "tuple",
    UNASSIGNED,
    $tuple_serialize,
    $tuple_deserialize
};
