void keyerror_showQ___ext_init__(void) {}

static int renderings;

B_str keyerror_showQ_note_render(B_str text) {
    ++renderings;
    return text;
}

int64_t keyerror_showQ_render_count(void) {
    return renderings;
}
