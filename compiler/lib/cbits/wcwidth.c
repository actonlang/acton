#if defined(_WIN32)
#include <wchar.h>

static int in_range(unsigned int c, unsigned int lo, unsigned int hi) {
    return c >= lo && c <= hi;
}

int wcwidth(wchar_t wc) {
    unsigned int c = (unsigned int)wc;

    if (c == 0) {
        return 0;
    }
    if (c < 32 || in_range(c, 0x7f, 0x9f)) {
        return -1;
    }
    if (in_range(c, 0x0300, 0x036f) ||
        in_range(c, 0x0483, 0x0489) ||
        in_range(c, 0x0591, 0x05bd) ||
        c == 0x05bf ||
        in_range(c, 0x05c1, 0x05c2) ||
        in_range(c, 0x05c4, 0x05c5) ||
        c == 0x05c7 ||
        in_range(c, 0x0610, 0x061a) ||
        in_range(c, 0x064b, 0x065f) ||
        c == 0x0670 ||
        in_range(c, 0x06d6, 0x06dc) ||
        in_range(c, 0x06df, 0x06e4) ||
        in_range(c, 0x06e7, 0x06e8) ||
        in_range(c, 0x06ea, 0x06ed) ||
        in_range(c, 0x200b, 0x200f) ||
        in_range(c, 0x202a, 0x202e) ||
        in_range(c, 0x2060, 0x206f) ||
        in_range(c, 0xfe00, 0xfe0f)) {
        return 0;
    }
    if (in_range(c, 0x1100, 0x115f) ||
        c == 0x2329 ||
        c == 0x232a ||
        in_range(c, 0x2e80, 0xa4cf) ||
        in_range(c, 0xac00, 0xd7a3) ||
        in_range(c, 0xf900, 0xfaff) ||
        in_range(c, 0xfe10, 0xfe19) ||
        in_range(c, 0xfe30, 0xfe6f) ||
        in_range(c, 0xff00, 0xff60) ||
        in_range(c, 0xffe0, 0xffe6)) {
        return 2;
    }

    return 1;
}
#endif
