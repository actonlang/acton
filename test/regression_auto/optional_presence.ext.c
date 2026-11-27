void optional_presenceQ___ext_init__(void) {}
$WORD optional_presenceQ_round_trip($WORD value) {
    return $deserialize($serialize(($Serializable)value, NULL), NULL);
}
bool optional_presenceQ_layout_checks(void) {
    // 65 payloads need two shared words. Allow pointer/alignment differences.
    _Static_assert(sizeof(struct optional_presenceQ_Wide) <=
        ((sizeof(void *) + 7) & ~7) + 67 * sizeof(uint64_t), "presence words are shared");
    _Static_assert(offsetof(struct optional_presenceQ_Child, f64) ==
        offsetof(struct optional_presenceQ_Wide, f64), "parent prefix is preserved");
    return true;
}
