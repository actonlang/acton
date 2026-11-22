void show_bool_conversionsQ___ext_init__(void) {}

// A valid native str with a subclass method table exercises the constructor's
// copy-to-base guarantee without depending on Acton subclass field layout.
B_str show_bool_conversionsQ_derived_string(void) {
    static struct B_strG_class methods;
    methods = B_strG_methods;
    methods.$GCINFO = "derived_string";
    methods.$superclass = ($SuperG_class)&B_strG_methods;
    B_str result = actStrFromCString("derived");
    result->$class = &methods;
    return result;
}

bool show_bool_conversionsQ_base_string(B_str value) {
    return value->$class == &B_strG_methods;
}
