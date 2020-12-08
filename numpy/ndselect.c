void numpy$$ndselect$__init__(numpy$$ndselect self) {
}

void numpy$$ndselect$__serialize__(numpy$$ndselect wit, $Serial$state state) {
}

numpy$$ndselect numpy$$ndselect$__deserialize__($Serial$state state) {
    numpy$$ndselect res = $DNEW(numpy$$ndselect,state);
    return res;
}

struct numpy$$ndselect$class numpy$$ndselect$methods = {
    "numpy$$ndselect",
    UNASSIGNED,
    ($Super$class)&$struct$methods,
    numpy$$ndselect$__init__,
    numpy$$ndselect$__serialize__,
    numpy$$ndselect$__deserialize__,
    ($bool (*)(numpy$$ndselect))$default__bool__,
    ($str (*)(numpy$$ndselect))$default__str__,
};

numpy$$ndselect numpy$$ndselect$new() {
    numpy$$ndselect $tmp = malloc(sizeof(struct numpy$$ndselect));
    $tmp->$class = &numpy$$ndselect$methods;
    numpy$$ndselect$methods.__init__($tmp);
    return $tmp;
}





void numpy$$ndindex$__init__(numpy$$ndindex self, $int index) {
    self->index = index;
}

void numpy$$ndindex$__serialize__(numpy$$ndindex self, $Serial$state state) {
    $step_serialize(self->index, state);
}

numpy$$ndindex numpy$$ndindex$__deserialize__($Serial$state state) {
    numpy$$ndindex res = $DNEW(numpy$$ndindex,state);
    res->index = ($int)$step_deserialize(state);
    return res;
}

struct numpy$$ndindex$class numpy$$ndindex$methods = {
    "numpy$$ndindex",
    UNASSIGNED,
    ($Super$class)&$struct$methods,
    numpy$$ndindex$__init__,
    numpy$$ndindex$__serialize__,
    numpy$$ndindex$__deserialize__,
    ($bool (*)(numpy$$ndindex))$default__bool__,
    ($str (*)(numpy$$ndindex))$default__str__,
};

numpy$$ndindex numpy$$ndindex$new($int p$1) {
    numpy$$ndindex $tmp = malloc(sizeof(struct numpy$$ndindex));
    $tmp->$class = &numpy$$ndindex$methods;
    numpy$$ndindex$methods.__init__($tmp, p$1);
    return $tmp;
}




void numpy$$ndslice$__init__(numpy$$ndslice self, $Slice slc) {
    self->slc = slc;
}

void numpy$$ndslice$__serialize__(numpy$$ndslice self, $Serial$state state) {
    $step_serialize(self->slc, state);
}

numpy$$ndslice numpy$$ndslice$__deserialize__($Serial$state state) {
    numpy$$ndslice res = $DNEW(numpy$$ndslice,state);
    res->slc = ($Slice)$step_deserialize(state);
    return res;
}

struct numpy$$ndslice$class numpy$$ndslice$methods = {
    "numpy$$ndslice",
    UNASSIGNED,
    ($Super$class)&$struct$methods,
    numpy$$ndslice$__init__,
    numpy$$ndslice$__serialize__,
    numpy$$ndslice$__deserialize__,
    ($bool (*)(numpy$$ndslice))$default__bool__,
    ($str (*)(numpy$$ndslice))$default__str__,
};

numpy$$ndslice numpy$$ndslice$new($Slice p$1) {
    numpy$$ndslice $tmp = malloc(sizeof(struct numpy$$ndslice));
    $tmp->$class = &numpy$$ndslice$methods;
    numpy$$ndslice$methods.__init__($tmp, p$1);
    return $tmp;
}
