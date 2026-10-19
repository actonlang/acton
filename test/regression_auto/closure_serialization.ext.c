void closure_serializationQ___ext_init__() {}

$WORD closure_serializationQ_round_trip($WORD value) {
    return $deserialize($serialize(($Serializable)value, NULL), NULL);
}

// Exercise the allocation-only phase and restoration into an existing object.
$WORD closure_serializationQ_round_trip_alloc($WORD value) {
    $ROW row = $serialize(($Serializable)value, NULL);
    $SerializableG_class methods = (($Serializable)value)->$class;
    $WORD restored = methods->__deserialize__(NULL, NULL);
    struct $Serial$state state = {
        .row = row->next, .row_no = 1, .globmap = NULL,
        .done = $NEW(B_dict, (B_Hashable)B_HashableD_intG_witness, NULL, NULL)
    };
    B_dictD_setitem(state.done, (B_Hashable)B_HashableD_intG_witness, toB_int(0), restored);
    return methods->__deserialize__(restored, &state);
}

// Synchronous round trips use external actor references as persistence does.
struct closure_serialization_actor_ref {
    $WORD actor;
    intptr_t key;
    struct closure_serialization_actor_ref *next;
};
static struct closure_serialization_actor_ref *closure_serialization_actor_refs;

static $WORD closure_serialization_globmap($WORD value) {
    intptr_t key = (intptr_t)value;
    for (struct closure_serialization_actor_ref *ref = closure_serialization_actor_refs; ref; ref = ref->next) {
        if (key < 0 && key == ref->key) return ref->actor;
        if (key >= 0 && value == ref->actor) return ($WORD)ref->key;
    }
    if (key < 0 || !issubtype($GET_CLASSID((($Serializable)value)->$class), ACTOR_ID))
        return NULL;
    struct closure_serialization_actor_ref *ref = acton_malloc(sizeof(*ref));
    ref->actor = value;
    ref->key = closure_serialization_actor_refs ? closure_serialization_actor_refs->key - 1 : -1;
    ref->next = closure_serialization_actor_refs;
    closure_serialization_actor_refs = ref;
    return ($WORD)ref->key;
}

$WORD closure_serializationQ_round_trip_actor($WORD value, closure_serializationQ_Sample actor_ref) {
    closure_serialization_actor_refs = NULL;
    (void)closure_serialization_globmap(actor_ref);
    return $deserialize($serialize(($Serializable)value, closure_serialization_globmap),
                        closure_serialization_globmap);
}

bool closure_serializationQ_has_cycle(closure_serializationQ_Node node, $pure callback) {
    return node->next == node && node->callback == callback;
}

$R closure_serializationQ_snapshot($Cont cont, int64_t value) {
    closure_serialization_actor_refs = NULL;
    $Cont restored = ($Cont)$deserialize($serialize(($Serializable)cont, closure_serialization_globmap),
                                  closure_serialization_globmap);
    return $R_CONT(restored, toB_int(value));
}

bool closure_serializationQ_shared_class($pure first, $pure second) {
    return first != second && first->$class == second->$class;
}

// Calling a genuine restored identity body must preserve the incoming box.
// Value equality alone would miss an unnecessary unbox/box allocation.
bool closure_serializationQ_identity_probe($pure callback, $WORD value) {
    return (($WORD (*)(void *, $WORD))callback->$class->__eval__)(callback, value) == value;
}

$R closure_serializationQ_exec_proc($Cont cont, $proc callback, int64_t value) {
    return (($R (*)(void *, $Cont, B_int))callback->$class->__exec__)(callback, cont, toB_int(value));
}

$R closure_serializationQ_exec_pure($Cont cont, $pure callback, int64_t value) {
    return (($R (*)(void *, $Cont, B_int))callback->$class->__exec__)(callback, cont, toB_int(value));
}

$R closure_serializationQ_exec_mut($Cont cont, $mut callback, int64_t value) {
    return (($R (*)(void *, $Cont, B_int))callback->$class->__exec__)(callback, cont, toB_int(value));
}

// Invoke genuine restored adapters with controlled virtual targets. Besides
// preserving the result pointer, each adapter must evaluate its target once,
// preserve argument order, and return the correct continuation/result tag.
struct closure_serialization_probe {
    void *methods;
    $WORD result;
    $WORD args[3];
    int calls;
    bool args_ok;
};

static $WORD closure_serialization_probe0(void *receiver) {
    struct closure_serialization_probe *probe = receiver;
    probe->calls++;
    return probe->result;
}

static $WORD closure_serialization_probe2(void *receiver, $WORD a, $WORD b) {
    struct closure_serialization_probe *probe = receiver;
    probe->calls++;
    probe->args_ok &= a == probe->args[0] && b == probe->args[1];
    return probe->result;
}

static $WORD closure_serialization_probe3(void *receiver, $WORD a, $WORD b, $WORD c) {
    struct closure_serialization_probe *probe = receiver;
    probe->calls++;
    probe->args_ok &= a == probe->args[0] && b == probe->args[1] && c == probe->args[2];
    return probe->result;
}

static bool closure_serialization_probe_result(struct closure_serialization_probe *probe,
                                               $Cont cont, $R call, $R exec, $RTAG tag) {
    return call.tag == tag && exec.tag == $RCONT && call.cont == cont && exec.cont == cont &&
           call.value == probe->result && exec.value == probe->result && probe->calls == 2 && probe->args_ok;
}

bool closure_serializationQ_pure_adapter_probe($pure callback) {
    struct $pureG_class methods = { .__eval__ = (void *)closure_serialization_probe0 };
    struct closure_serialization_probe probe = { .methods = &methods, .result = toB_int(1729), .args_ok = true };
    $Cont cont = ($Cont)callback;
    $R call = (($R (*)(void *, $Cont))callback->$class->__call__)(&probe, cont);
    $R exec = (($R (*)(void *, $Cont))callback->$class->__exec__)(&probe, cont);
    return closure_serialization_probe_result(&probe, cont, call, exec, $RCONT);
}

bool closure_serializationQ_pure_three_probe($pure callback) {
    struct $pureG_class methods = { .__eval__ = (void *)closure_serialization_probe3 };
    struct closure_serialization_probe probe = { .methods = &methods, .result = $NEWTUPLE0,
        .args = {toB_int(9), to$str("probe"), toB_float(2.5)}, .args_ok = true };
    $Cont cont = ($Cont)callback;
    $R call = (($R (*)(void *, $Cont, $WORD, $WORD, $WORD))callback->$class->__call__)
                  (&probe, cont, probe.args[0], probe.args[1], probe.args[2]);
    $R exec = (($R (*)(void *, $Cont, $WORD, $WORD, $WORD))callback->$class->__exec__)
                  (&probe, cont, probe.args[0], probe.args[1], probe.args[2]);
    return closure_serialization_probe_result(&probe, cont, call, exec, $RCONT);
}

bool closure_serializationQ_mut_two_probe($mut callback) {
    struct $mutG_class methods = { .__eval__ = (void *)closure_serialization_probe2 };
    struct closure_serialization_probe probe = { .methods = &methods, .result = toB_float(2.5),
        .args = {to$str("probe"), toB_float(1.5)}, .args_ok = true };
    $Cont cont = ($Cont)callback;
    $R call = (($R (*)(void *, $Cont, $WORD, $WORD))callback->$class->__call__)
                  (&probe, cont, probe.args[0], probe.args[1]);
    $R exec = (($R (*)(void *, $Cont, $WORD, $WORD))callback->$class->__exec__)
                  (&probe, cont, probe.args[0], probe.args[1]);
    return closure_serialization_probe_result(&probe, cont, call, exec, $RCONT);
}

bool closure_serializationQ_action_three_probe($action callback) {
    struct $actionG_class methods = { .__asyn__ = (void *)closure_serialization_probe3 };
    struct closure_serialization_probe probe = { .methods = &methods, .result = acton_calloc(1, sizeof(struct B_Msg)),
        .args = {toB_int(9), to$str("probe"), toB_float(2.5)}, .args_ok = true };
    $Cont cont = ($Cont)callback;
    $R call = (($R (*)(void *, $Cont, $WORD, $WORD, $WORD))callback->$class->__call__)
                  (&probe, cont, probe.args[0], probe.args[1], probe.args[2]);
    $R exec = (($R (*)(void *, $Cont, $WORD, $WORD, $WORD))callback->$class->__exec__)
                  (&probe, cont, probe.args[0], probe.args[1], probe.args[2]);
    return closure_serialization_probe_result(&probe, cont, call, exec, $RWAIT);
}
