void serialize_shared_refQ___ext_init__() {
    // NOP
}

B_list serialize_shared_refQ_round_trip(B_list x) {
    $ROW row = $serialize(($Serializable)x, NULL);
    return (B_list)$deserialize(row, NULL);
}
