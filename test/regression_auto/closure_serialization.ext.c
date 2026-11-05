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

// A ConstCont invocation must return one trampoline step without invoking the
// next continuation. Two links distinguish that from recursively forwarding.
struct closure_serialization_stop {
    struct $ContG_class *methods;
    int calls;
};

static $R closure_serialization_stop_call($Cont self, $WORD value) {
    ((struct closure_serialization_stop *)self)->calls++;
    return $R_CONT(NULL, value);
}

bool closure_serializationQ_constcont_step_probe(void) {
    struct $ContG_class methods = { .__call__ = closure_serialization_stop_call };
    struct closure_serialization_stop stop = { .methods = &methods };
    $WORD outer_value = toB_int(17);
    $Cont inner = $CONSTCONT(toB_int(23), ($Cont)&stop);
    $Cont outer = $CONSTCONT(outer_value, inner);
    $R step = outer->$class->__call__(outer, B_None);
    return step.tag == $RCONT && step.cont == inner &&
           step.value == outer_value && stop.calls == 0;
}

// Require the selected payload, then round-trip the genuine incoming Cont.
// Different resume bodies of the same shape must keep distinct class identity.
$R closure_serializationQ_layout_snapshot($Cont cont, int64_t arity, $WORD value,
                                          bool allocation_only) {
    bool shared = cont->$class->__serialize__ == $cont_serialize;
    const struct $ClosureShape *shapes[] = {
        NULL, &$Cont_R1_shape, &$Cont_R2_shape, &$Cont_R3_shape, &$Cont_R4_shape,
        &$Cont_R5_shape, &$Cont_R6_shape, &$Cont_R7_shape, &$Cont_R8_shape
    };
    if (arity >= 1 && arity <= 8) {
        if (!shared || ((struct $ContClass *)cont->$class)->shape != shapes[arity])
            $RAISE((B_BaseException)B_ValueErrorG_new($FORMAT("Expected shared R%lld, got %s", (long long)arity, cont->$class->$GCINFO)));
    } else if (shared) {
        $RAISE((B_BaseException)B_ValueErrorG_new(to$str("Expected ordinary Cont fallback")));
    }

    closure_serialization_actor_refs = NULL;
    $ROW row = $serialize(($Serializable)cont, closure_serialization_globmap);
    $Cont restored;
    if (allocation_only) {
        $SerializableG_class methods = ($SerializableG_class)cont->$class;
        restored = ($Cont)methods->__deserialize__(NULL, NULL);
        struct $Serial$state state = {
            .row = row->next, .row_no = 1, .globmap = closure_serialization_globmap,
            .done = $NEW(B_dict, (B_Hashable)B_HashableD_intG_witness, NULL, NULL)
        };
        B_dictD_setitem(state.done, (B_Hashable)B_HashableD_intG_witness, toB_int(0), restored);
        restored = ($Cont)methods->__deserialize__(($Serializable)restored, &state);
    } else {
        restored = ($Cont)$deserialize(row, closure_serialization_globmap);
    }
    if (restored->$class != cont->$class)
        $RAISE((B_BaseException)B_ValueErrorG_new(to$str("Continuation class changed on restore")));
    return $R_CONT(restored, value);
}

// User classes do not implicitly implement Identity; check graph identity in C.
bool closure_serializationQ_contbox_graph(closure_serializationQ_ContBox a,
                                          closure_serializationQ_ContBox b,
                                          closure_serializationQ_ContBox c) {
    return a == b && a->next == a && c->next == a;
}

int64_t closure_serializationQ_cont_result(int64_t x,
        closure_serializationQ_ContBox a, closure_serializationQ_ContBox b,
        closure_serializationQ_ContBox c, closure_serializationQ_ContBox d) {
    if (!a) return x + 1;
    return x + a->n + (b ? b->n : 0) + (c ? c->n : 0) + (d ? d->n : 0);
}

int64_t closure_serializationQ_cont_other(int64_t x, closure_serializationQ_ContBox a) {
    return x - a->n;
}

int64_t closure_serializationQ_cont_bool_result(bool flag, closure_serializationQ_ContBox a,
                                               closure_serializationQ_ContBox b) {
    return a->n + b->n + (flag ? 1 : 0);
}

int64_t closure_serializationQ_cont_scalar_result(int64_t x, int64_t n) {
    return x + n;
}

// Distinct weights expose capture-slot permutations, including repeated references.
int64_t closure_serializationQ_cont_many(int64_t x,
        closure_serializationQ_ContBox a, closure_serializationQ_ContBox b,
        closure_serializationQ_ContBox c, closure_serializationQ_ContBox d,
        closure_serializationQ_ContBox e, closure_serializationQ_ContBox f,
        closure_serializationQ_ContBox g, closure_serializationQ_ContBox h) {
    closure_serializationQ_ContBox boxes[] = {a, b, c, d, e, f, g, h};
    for (int i = 0; i < 8; i++)
        if (boxes[i]) x += (i + 1) * boxes[i]->n;
    return x;
}

// Require the real selected family/shape, then exercise both invocation slots.
// A back edge through a captured box must point to this restored Proc itself.
bool closure_serializationQ_proc0_shape($proc callback, int64_t arity, bool cyclic) {
    bool shared = callback->$class->__serialize__ == $proc0_serialize;
    const struct $ClosureShape *shapes[] = {
        NULL, &$Proc0_R1_shape, &$Proc0_R2_shape, &$Proc0_R3_shape, &$Proc0_R4_shape
    };
    if (arity < 1 || arity > 4) return !shared;
    if (!shared || ((struct $Proc0Class *)callback->$class)->shape != shapes[arity])
        return false;
    const struct $Proc0G_class *methods = &((struct $Proc0Class *)callback->$class)->methods;
    if (methods->__call__ != methods->__exec__) return false;
    if (arity >= 3 && ((struct $Proc0_R3 *)callback)->ref0 != ((struct $Proc0_R3 *)callback)->ref1)
        return false; // repeated references survive as one object
    closure_serializationQ_ContBox a = ((struct $Proc0_R1 *)callback)->ref0;
    return !cyclic || (a->next == a && a->callback == callback);
}

$R closure_serializationQ_exec_proc0($Cont cont, $proc callback) {
    return (($R (*)(void *, $Cont))callback->$class->__exec__)(callback, cont);
}

// Replace the continuation in a real queued message before the actor yields.
// This checks actor references, Proc0 dispatch through the RTS, and exceptions.
$R closure_serializationQ_proc0_message($Cont cont, closure_serializationQ_Sample actor_ref,
        closure_serializationQ_ContBox box, bool allocation_only, bool fail) {
    B_Msg msg = fail ? closure_serializationQ_SampleD_ref_fail(actor_ref, box)
                     : closure_serializationQ_SampleD_ref_plus(actor_ref, box);
    $proc original = ($proc)msg->$cont;
    if (!closure_serializationQ_proc0_shape(original, 2, false))
        $RAISE((B_BaseException)B_ValueErrorG_new(to$str("Expected Proc0 R2 actor message")));
    closure_serialization_actor_refs = NULL;
    $ROW row = $serialize(($Serializable)msg, closure_serialization_globmap);
    B_Msg restored;
    if (allocation_only) {
        restored = B_MsgD___deserialize__(NULL, NULL);
        struct $Serial$state state = {
            .row = row->next, .row_no = 1, .globmap = closure_serialization_globmap,
            .done = $NEW(B_dict, (B_Hashable)B_HashableD_intG_witness, NULL, NULL)
        };
        B_dictD_setitem(state.done, (B_Hashable)B_HashableD_intG_witness, toB_int(0), restored);
        restored = B_MsgD___deserialize__(restored, &state);
    } else {
        restored = (B_Msg)$deserialize(row, closure_serialization_globmap);
    }
    if (restored->$to != msg->$to || restored->$cont->$class != msg->$cont->$class)
        $RAISE((B_BaseException)B_ValueErrorG_new(to$str("Message identity changed on restore")));
    msg->$cont = restored->$cont;
    return $AWAIT(cont, msg);
}

bool closure_serializationQ_proc0_same_class($proc first, $proc second) {
    return first != second && first->$class == second->$class;
}

// The fixed one-reference Action layout is independent of invocation arity.
$WORD closure_serializationQ_action_round_trip($WORD value, closure_serializationQ_Sample actor_ref,
                                               int64_t arity, bool allocation_only) {
    $action callback = value;
    bool shared = callback->$class->__serialize__ == $action_r1_serialize;
    if (shared != (arity >= 0 && arity <= 3))
        $RAISE((B_BaseException)B_ValueErrorG_new(to$str("Unexpected Action layout selection")));
    closure_serialization_actor_refs = NULL;
    (void)closure_serialization_globmap(actor_ref);
    $ROW row = $serialize(($Serializable)value, closure_serialization_globmap);
    $SerializableG_class methods = (($Serializable)value)->$class;
    $WORD restored;
    if (allocation_only) {
        restored = methods->__deserialize__(NULL, NULL);
        struct $Serial$state state = {
            .row = row->next, .row_no = 1, .globmap = closure_serialization_globmap,
            .done = $NEW(B_dict, (B_Hashable)B_HashableD_intG_witness, NULL, NULL)
        };
        B_dictD_setitem(state.done, (B_Hashable)B_HashableD_intG_witness, toB_int(0), restored);
        restored = methods->__deserialize__(restored, &state);
    } else {
        restored = $deserialize(row, closure_serialization_globmap);
    }
    if (value == restored || (($Serializable)restored)->$class != methods)
        $RAISE((B_BaseException)B_ValueErrorG_new(to$str("Action class changed on restore")));
    return restored;
}

bool closure_serializationQ_action_graph($action callback) {
    closure_serializationQ_ActionBox box = ((struct $Action_R1 *)callback)->ref0;
    return box->next == box && box->callback == callback;
}

$R closure_serializationQ_exec_action_one($Cont cont, $action callback, int64_t x) {
    return (($R (*)(void *, $Cont, $WORD))callback->$class->__exec__)(callback, cont, toB_int(x));
}

static $WORD closure_serialization_probe1(void *receiver, $WORD a) {
    struct closure_serialization_probe *probe = receiver;
    probe->calls++;
    probe->args_ok &= a == probe->args[0];
    return probe->result;
}

bool closure_serializationQ_action_one_probe($action callback) {
    struct $actionG_class methods = { .__asyn__ = (void *)closure_serialization_probe1 };
    struct closure_serialization_probe probe = { .methods = &methods, .result = acton_calloc(1, sizeof(struct B_Msg)),
        .args = {toB_int(9), to$str("probe")}, .args_ok = true };
    $Cont cont = ($Cont)callback;
    $R call = (($R (*)(void *, $Cont, $WORD))callback->$class->__call__)(&probe, cont, probe.args[0]);
    $R exec = (($R (*)(void *, $Cont, $WORD))callback->$class->__exec__)(&probe, cont, probe.args[0]);
    return closure_serialization_probe_result(&probe, cont, call, exec, $RWAIT);
}

bool closure_serializationQ_action_two_probe($action callback) {
    struct $actionG_class methods = { .__asyn__ = (void *)closure_serialization_probe2 };
    struct closure_serialization_probe probe = { .methods = &methods, .result = acton_calloc(1, sizeof(struct B_Msg)),
        .args = {toB_int(9), to$str("probe")}, .args_ok = true };
    $Cont cont = ($Cont)callback;
    $R call = (($R (*)(void *, $Cont, $WORD, $WORD))callback->$class->__call__)(&probe, cont, probe.args[0], probe.args[1]);
    $R exec = (($R (*)(void *, $Cont, $WORD, $WORD))callback->$class->__exec__)(&probe, cont, probe.args[0], probe.args[1]);
    return closure_serialization_probe_result(&probe, cont, call, exec, $RWAIT);
}

bool closure_serializationQ_action_zero_probe($action callback) {
    struct $actionG_class methods = { .__asyn__ = (void *)closure_serialization_probe0 };
    struct closure_serialization_probe probe = { .methods = &methods, .result = acton_calloc(1, sizeof(struct B_Msg)),
        .args_ok = true };
    $Cont cont = ($Cont)callback;
    $R call = (($R (*)(void *, $Cont))callback->$class->__call__)(&probe, cont);
    $R exec = (($R (*)(void *, $Cont))callback->$class->__exec__)(&probe, cont);
    return closure_serialization_probe_result(&probe, cont, call, exec, $RWAIT);
}

B_Msg closure_serializationQ_ActionBoxD_invoke(closure_serializationQ_ActionBox self, int64_t x) {
    return self->target->$class->__asyn__(self->target, toB_int(x));
}
