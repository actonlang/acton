void show_bool_printQ___ext_init__(void) {}

static int renderings;

B_str show_bool_printQ_next_render(void) {
    return $FORMAT("tick%d", ++renderings);
}

int64_t show_bool_printQ_render_count(void) {
    return renderings;
}
