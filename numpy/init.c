int numpy$$done = 0;

void numpy$$__init__() {
    if (numpy$$done) return;
    numpy$$done = 1;
    $register_force(ATOM_ID,&numpy$$ndarray$methods);
    $register(&numpy$$Iterator$ndarray$methods);
    $register(&numpy$$Primitive$int$methods);
    $register(&numpy$$Primitive$float$methods);
    $register(&numpy$$ndselect$methods);
    $register(&numpy$$ndindex$methods);
    $register(&numpy$$ndslice$methods);
    $register(&numpy$$Integral$ndarray$methods);
    $register(&numpy$$Logical$ndarray$methods);
    $register(&numpy$$Minus$ndarray$methods);
    $register(&numpy$$Sliceable$ndarray$methods);
    $register(&numpy$$Collection$ndarray$methods);
    $register(&numpy$$RealFuns$math$ndarray$methods);
    numpy$$newaxis = to$int(LONG_MIN);
}

