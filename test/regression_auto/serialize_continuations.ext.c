void serialize_continuationsQ___ext_init__() {
    // NOP
}

extern struct $Cont $Done$instance;
extern struct $Cont $Fail$instance;
extern struct $Cont $InitRoot$cont;
extern struct $Cont $InitEnv$cont;
extern struct $Cont $StartRoot$cont;

B_list serialize_continuationsQ_round_trip_failures() {
    $Cont conts[] = { &$Done$instance, &$Fail$instance, &$InitRoot$cont, &$InitEnv$cont, &$StartRoot$cont };
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    B_list res = B_listG_new(NULL, NULL);
    for (size_t i = 0; i < sizeof(conts)/sizeof(conts[0]); i++) {
        $ROW row = $serialize(($Serializable)conts[i], NULL);
        $Cont c = ($Cont)$deserialize(row, NULL);
        if (c->$class != conts[i]->$class)
            wit->$class->append(wit, res, $FORMAT("%s came back as %s", conts[i]->$class->$GCINFO, c->$class->$GCINFO));
    }
    return res;
}
