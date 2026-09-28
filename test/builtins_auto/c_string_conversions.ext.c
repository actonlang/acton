#include <string.h>

void c_string_conversionsQ___ext_init__() {
}

#define CHECK(condition) do { \
    if (!(condition)) \
        $RAISE((B_BaseException)B_ValueErrorG_new(actStrFromCString(#condition))); \
} while (0)

bool c_string_conversionsQ_check_strings() {
    static const char text[] = "A\xc3\xa5\0B";
    B_str shared = actStrFromCString(text);
    CHECK(shared->str == (unsigned char *)text);
    CHECK(shared->nbytes == 3 && shared->nchars == 2);
    B_str bounded = actStrFromCStringLength(text, 5);
    CHECK(bounded->str == (unsigned char *)text);
    CHECK(bounded->nbytes == 5 && bounded->nchars == 4);

    char *managed = acton_malloc_atomic(7);
    memcpy(managed, "shared", 7);
    CHECK(actStrFromCString(managed)->str == (unsigned char *)managed);
    CHECK(actStrFromCStringLength(managed, 6)->str == (unsigned char *)managed);

    char temporary[] = "before";
    B_str copied = actStrFromCStringCopy(temporary);
    temporary[0] = 'X';
    CHECK(copied->nbytes == 6 && memcmp(copied->str, "before", 7) == 0);

    // The copying length APIs must not read a terminator from the input.
    char slice[] = {'A', '\xc3', '\xa5', 0, 'B'};
    copied = actStrFromCStringLengthCopy(slice, sizeof(slice));
    slice[0] = 'X';
    CHECK(copied->nbytes == 5 && copied->nchars == 4);
    CHECK(memcmp(copied->str, text, 6) == 0);

    B_str empty = actStrFromCString("");
    CHECK(empty->nbytes == 0 && empty->nchars == 0 && empty->str[0] == 0);
    CHECK(empty == actStrFromCStringCopy(""));
    CHECK(empty == actStrFromCStringLength("", 0));
    CHECK(empty == actStrFromCStringLengthCopy(text, 0));

    // Constructors retain the existing immutable ASCII singletons.
    for (int code = 0; code < 128; code++) {
        char *one = acton_malloc_atomic(2);
        one[0] = code;
        one[1] = 0;
        B_str character = actStrFromCStringLength(one, 1);
        CHECK(character->nbytes == 1 && character->nchars == 1);
        CHECK(character->str[0] == code && character->str[1] == 0);
        CHECK(character == actStrFromCStringLengthCopy(one, 1));
        if (code != 0) {
            CHECK(character == actStrFromCString(one));
            CHECK(character == actStrFromCStringCopy(one));
        }
    }
    return true;
}

bool c_string_conversionsQ_check_bytes() {
    char temporary[] = "before";
    B_bytes copied = actBytesFromCStringCopy(temporary);
    temporary[0] = 'X';
    // bytes data is nbytes long with no terminator after it, so only the
    // data is compared
    CHECK(copied->nbytes == 6 && memcmp(copied->str, "before", 6) == 0);

    char slice[] = {'A', 0, 'B'};
    copied = actBytesFromCStringLengthCopy(slice, sizeof(slice));
    slice[0] = 'X';
    CHECK(copied->nbytes == 3 && memcmp(copied->str, "A\0B", 3) == 0);

    static const char shared[] = "before\0after";
    B_bytes borrowed = actBytesFromCString(shared);
    CHECK(borrowed->str == (unsigned char *)shared && borrowed->nbytes == 6);
    borrowed = actBytesFromCStringLength(shared, 12);
    CHECK(borrowed->str == (unsigned char *)shared && borrowed->nbytes == 12);
    copied = actBytesFromCStringCopy("");
    CHECK(copied->nbytes == 0);
    copied = actBytesFromCStringLengthCopy("", 0);
    CHECK(copied->nbytes == 0);
    return true;
}

#undef CHECK

B_str c_string_conversionsQ_from_c(B_bytes data, int64_t mode) {
    const char *str = (const char *)data->str;
    switch (mode) {
        case 0: return actStrFromCString(str);
        case 1: return actStrFromCStringCopy(str);
        case 2: return actStrFromCStringLength(str, data->nbytes);
        default: return actStrFromCStringLengthCopy(str, data->nbytes);
    }
}
