// base links utf8proc as a static library and installs its header
#define UTF8PROC_STATIC
#include "utf8proc.h"

void stdQ_termQ___ext_init__() {
}

// The width of each code point comes from the Unicode data of utf8proc, the
// library that the str methods of the runtime use.
int64_t stdQ_termQ_display_width(B_str s) {
    const utf8proc_uint8_t *p = (const utf8proc_uint8_t *)s->str;
    const utf8proc_uint8_t *end = p + s->nbytes;
    int64_t width = 0;
    while (p < end) {
        utf8proc_int32_t codepoint;
        utf8proc_ssize_t n = utf8proc_iterate(p, end - p, &codepoint);
        if (n <= 0)
            break;
        width += utf8proc_charwidth(codepoint);
        p += n;
    }
    return width;
}
