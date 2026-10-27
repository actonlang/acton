#define PCRE2_CODE_UNIT_WIDTH 8

#include <pcre2.h>

static struct B_ValueError stdQ_reQ_negative_start_error =
    STATIC_EXCEPTION(B_ValueError, "PCRE2 matching failed: negative start_pos");
static struct B_ValueError stdQ_reQ_start_too_large_error =
    STATIC_EXCEPTION(B_ValueError, "start position is greater than string length");
static struct B_RuntimeError stdQ_reQ_small_ovector_error =
    STATIC_EXCEPTION(B_RuntimeError, "ovector was not big enough for all captured substrings");
static struct B_ValueError stdQ_reQ_split_char_error =
    STATIC_EXCEPTION(B_ValueError, "matched substring starts or ends inside a character");
static struct B_ValueError stdQ_reQ_invalid_span_error =
    STATIC_EXCEPTION(B_ValueError, "matched substring starts before the search position or ends before it starts");
static struct B_MemoryError stdQ_reQ_allocation_failed_error =
    STATIC_EXCEPTION(B_MemoryError, "memory allocation failed");
static struct B_ValueError stdQ_reQ_result_too_long_error =
    STATIC_EXCEPTION(B_ValueError, "result too long");

// The compiled code of a Pattern is pointer-free memory on the GC heap.
// _compile compiles through a general context that allocates with
// acton_malloc_atomic. The code holds no pointer to GC memory: its memory
// control fields are the two functions below, its character tables are
// pcre2's static default ones, and there is no JIT code. The Pattern keeps
// the code reachable through _code, so the GC frees it with the Pattern, and
// pcre2_code_free is never called. pcre2_compile allocates temporaries only
// for large patterns, through the same context, and frees them with
// acton_free.
//
// _match, behind the module function match, compiles its pattern for the one
// call with malloc, pcre2's default allocator, which is selected by compiling
// without a compile context. It frees the code before it returns, and an
// exception handler pushed after compiling frees it when the match raises.
//
// pcre2 does not change compiled code while it matches, so actors on several
// threads can match with one Pattern at once.
//
// The match data and the backtracking frames of pcre2_match (at least 20 KiB
// per match-data block with the default heap limit) are on malloc:
// pcre2_match_data_create without a general context uses malloc for both,
// while pcre2_match_data_create_from_pattern would take the allocator of the
// code. count and replace reuse one block for the whole scan. Each call
// frees its block before it returns, and an exception handler
// pushed before they are allocated frees them when anything raises in
// between: a pcre2 error, a group that starts or ends inside a character, or
// a failed allocation while the result is built.
//
// The match data and the frames point into the bytes of the subject str and
// into the code. The GC does not scan malloc memory, but arg_text keeps the
// subject's bytes reachable, and GC_reachable_here(self) keeps the Pattern,
// and with it the code, reachable until pcre2's memory is freed.

void stdQ_reQ___ext_init__() {
}

static void *stdQ_reQ_code_malloc(size_t size, void *data) {
    return acton_malloc_atomic(size);
}

static void stdQ_reQ_code_free(void *ptr, void *data) {
    acton_free(ptr);
}

static void stdQ_reQ_raise_compile_error(int errornumber, PCRE2_SIZE erroroffset) {
    PCRE2_UCHAR buffer[256];
    pcre2_get_error_message(errornumber, buffer, sizeof(buffer));
    char errmsg[1024] = "regex compilation failed at offset ";
    snprintf(errmsg + strlen(errmsg), sizeof(errmsg) - strlen(errmsg), "%d: %s", (int)erroroffset, buffer);
    RAISE(B_ValueError, actStrFromCStringCopy(errmsg));
}

uint64_t stdQ_reQ__compile (B_str arg_pattern) {
    pcre2_general_context *gcontext = pcre2_general_context_create(stdQ_reQ_code_malloc, stdQ_reQ_code_free, NULL);
    if (gcontext == NULL)
        RAISE_EXC(&stdQ_reQ_allocation_failed_error);
    pcre2_compile_context *ccontext = pcre2_compile_context_create(gcontext);
    if (ccontext == NULL)
        RAISE_EXC(&stdQ_reQ_allocation_failed_error);
    int errornumber;
    PCRE2_SIZE erroroffset;
    pcre2_code *re = pcre2_compile(
        (PCRE2_SPTR)fromB_str(arg_pattern),
        arg_pattern->nbytes,
        PCRE2_UTF,
        &errornumber,
        &erroroffset,
        ccontext
    );
    pcre2_compile_context_free(ccontext);
    pcre2_general_context_free(gcontext);
    if (re == NULL)
        stdQ_reQ_raise_compile_error(errornumber, erroroffset);
    return (uint64_t)(uintptr_t)re;
}

// An offset where a substring of valid UTF-8 text may start or end. Matched
// offsets are on character boundaries unless the pattern uses \C, which
// matches a single byte.
static bool stdQ_reQ_char_boundary(PCRE2_SPTR text, size_t text_length, PCRE2_SIZE offset) {
    return offset == text_length || (text[offset] & 0xC0) != 0x80;
}

// The byte offset of start_pos, a code point index, in arg_text.
// TODO: use u64 instead of int to eradicate possibility of < 0
static PCRE2_SIZE stdQ_reQ_start_offset(B_str arg_text, int64_t arg_start_pos) {
    if (arg_start_pos < 0) {
        RAISE_EXC(&stdQ_reQ_negative_start_error);
    }
    if (arg_start_pos > arg_text->nchars) {
        RAISE_EXC(&stdQ_reQ_start_too_large_error);
    }
    return (PCRE2_SIZE)$byte_no(arg_text, arg_start_pos);
}

// Acton strings are valid UTF-8 and start_offset is a character boundary.
// Checking the remaining subject on every call would make repeated searches
// quadratic. Keep checking captured boundaries: \C may split a character.
static int stdQ_reQ_exec(pcre2_code *re, B_str text, PCRE2_SIZE start_offset, pcre2_match_data *match_data) {
    int rc = pcre2_match(re, text->str, text->nbytes, start_offset,
                         PCRE2_NO_UTF_CHECK, match_data, NULL);
    if (rc == PCRE2_ERROR_NOMATCH)
        return rc;
    if (rc == PCRE2_ERROR_NOMEMORY)
        RAISE_EXC(&stdQ_reQ_allocation_failed_error);
    if (rc < 0) {
        char errmsg[256];
        snprintf(errmsg, sizeof(errmsg), "PCRE2 matching error: %d", rc);
        RAISE(B_RuntimeError, actStrFromCStringCopy(errmsg));
    }
    if (rc == 0)
        RAISE_EXC(&stdQ_reQ_small_ovector_error);

    PCRE2_SIZE *ovector = pcre2_get_ovector_pointer(match_data);
    // A subroutine using \K inside an assertion can produce a reversed or
    // backward whole-match span on older PCRE2 versions. It cannot be used
    // for forward scanning or replacement. Captures may precede group 0.
    if (ovector[0] < start_offset || ovector[1] < ovector[0])
        RAISE_EXC(&stdQ_reQ_invalid_span_error);
    // Groups at or above rc are unset. This also checks named groups and
    // captures that count and replace do not otherwise use.
    for (int i = 0; i < rc; i++) {
        PCRE2_SIZE start = ovector[2*i];
        PCRE2_SIZE end = ovector[2*i+1];
        if (start == PCRE2_UNSET || end == PCRE2_UNSET)
            continue;
        if (!stdQ_reQ_char_boundary(text->str, text->nbytes, start) ||
            !stdQ_reQ_char_boundary(text->str, text->nbytes, end))
            RAISE_EXC(&stdQ_reQ_split_char_error);
    }
    return rc;
}

// Match the compiled code re against arg_text from start_offset, a byte
// offset. arg_pattern is the pattern of the returned Match.
static stdQ_reQ_Match stdQ_reQ_match_code(pcre2_code *re, B_str arg_pattern, B_str arg_text, PCRE2_SIZE start_offset) {
    B_Hashable hwit = (B_Hashable)B_HashableD_strG_witness;
    B_SequenceD_list swit = B_SequenceD_listG_witness;
    B_list groups = B_listG_new(NULL, NULL);
    B_dict named_groups = $NEW(B_dict, hwit, NULL, NULL);

    PCRE2_SPTR text = (PCRE2_SPTR)fromB_str(arg_text);

    uint32_t capture_count;
    pcre2_pattern_info(re, PCRE2_INFO_CAPTURECOUNT, &capture_count);

    // The handler is pushed before pcre2 allocates anything, so that every
    // raise after the first allocation lands in it. Its frame is a local of
    // stdQ_reQ_match_code, which pops it with $DROP or $POP before it returns or
    // raises again. match_data is assigned after the setjmp of $PUSH and read in the
    // handler, thus volatile.
    pcre2_match_data *volatile match_data = NULL;
    stdQ_reQ_Match result = (stdQ_reQ_Match)B_None;
    struct JumpBuf frame;
    if ($PUSH(&frame)) {
        match_data = pcre2_match_data_create(capture_count + 1, NULL);
        if (match_data == NULL)
            RAISE_EXC(&stdQ_reQ_allocation_failed_error);
        int rc = stdQ_reQ_exec(re, arg_text, start_offset, match_data);

        if (rc == PCRE2_ERROR_NOMATCH) {
            // result stays None
        } else {
            PCRE2_SIZE *ovector = pcre2_get_ovector_pointer(match_data);

            // Extract all unnamed groups
            // group 0 is the entire match, group 1... are subgroups
            for (int i = 0; i < rc; i++) {
                PCRE2_SIZE ss_start = ovector[2*i];
                PCRE2_SIZE ss_end = ovector[2*i+1];
                if (ss_start == PCRE2_UNSET || ss_end == PCRE2_UNSET) {
                    swit->$class->append(swit, groups, B_None);
                } else {
                    B_str substring = actStrFromCStringLengthCopy((const char *)text + ss_start, ss_end - ss_start);
                    swit->$class->append(swit, groups, substring);
                }
            }

            // Named groups
            int namecount;
            pcre2_pattern_info(re, PCRE2_INFO_NAMECOUNT, &namecount);

            if (namecount > 0) {
                PCRE2_SPTR name_table;
                int name_entry_size;
                pcre2_pattern_info(re, PCRE2_INFO_NAMETABLE, &name_table);
                pcre2_pattern_info(re, PCRE2_INFO_NAMEENTRYSIZE, &name_entry_size);

                PCRE2_SPTR tabptr = name_table;
                for (int i = 0; i < namecount; i++) {
                    int n = (tabptr[0] << 8) | tabptr[1]; // group number for this name
                    // The name starts at tabptr+2. pcre2 pads every entry with zero
                    // bytes to the size of the longest name, so the name ends at the
                    // first zero byte.
                    size_t namelen = strnlen((const char *)tabptr + 2, name_entry_size - 2);
                    B_str group_name = actStrFromCStringLengthCopy((const char *)tabptr + 2, namelen);

                    PCRE2_SIZE ss_start = ovector[2*n];
                    PCRE2_SIZE ss_end = ovector[2*n+1];
                    if (ss_start == PCRE2_UNSET || ss_end == PCRE2_UNSET) {
                        B_dictD_setitem(named_groups, hwit, group_name, B_None);
                    } else {
                        B_str substring = actStrFromCStringLengthCopy((const char *)text + ss_start, ss_end - ss_start);
                        B_dictD_setitem(named_groups, hwit, group_name, substring);
                    }

                    tabptr += name_entry_size;
                }
            }

            // Entire match offsets, as code point indexes
            int64_t match_start = $char_no(arg_text, ovector[0]);
            int64_t match_end = $char_no(arg_text, ovector[1]);

            result = stdQ_reQ_MatchG_new(arg_pattern, arg_text, match_start, match_end, groups, named_groups);
        }
        $DROP();
    } else {
        B_BaseException ex = $POP();
        // pcre2_match_data_free does nothing given NULL.
        pcre2_match_data_free(match_data);
        $RAISE(ex);
    }

    pcre2_match_data_free(match_data);
    return result;
}

stdQ_reQ_Match stdQ_reQ_PatternD__match (stdQ_reQ_Pattern self, B_str arg_text, int64_t arg_start_pos) {
    PCRE2_SIZE start_offset = stdQ_reQ_start_offset(arg_text, arg_start_pos);
    stdQ_reQ_Match result = stdQ_reQ_match_code((pcre2_code *)(uintptr_t)self->_code, self->pattern, arg_text, start_offset);
    GC_reachable_here(self);
    return result;
}

static void stdQ_reQ_append(unsigned char **buffer, int64_t *length, int64_t *capacity,
                       const unsigned char *text, int64_t size) {
    if (size > MAX_STR_LEN - *length)
        RAISE_EXC(&stdQ_reQ_result_too_long_error);
    int64_t needed = *length + size;
    if (needed >= *capacity) {
        int64_t capacity_next = *capacity == 0 ? 256 : *capacity;
        while (capacity_next <= needed) {
            if (capacity_next > MAX_STR_LEN / 2) {
                capacity_next = MAX_STR_LEN + 1;
                break;
            }
            capacity_next *= 2;
        }
        unsigned char *next = acton_realloc_atomic(*buffer, capacity_next);
        if (next == NULL)
            RAISE_EXC(&stdQ_reQ_allocation_failed_error);
        *buffer = next;
        *capacity = capacity_next;
    }
    memcpy(*buffer + *length, text, size);
    *length = needed;
}

// Count or replace the same matches as Pattern.matches, keeping byte offsets
// throughout the scan. A NULL replacement selects counting; result is only
// used for replacement. Mutable match data belongs to this call, not Pattern.
static int64_t stdQ_reQ_scan(stdQ_reQ_Pattern self, B_str text, B_str replacement, B_str *result) {
    pcre2_code *re = (pcre2_code *)(uintptr_t)self->_code;
    uint32_t capture_count;
    pcre2_pattern_info(re, PCRE2_INFO_CAPTURECOUNT, &capture_count);
    pcre2_match_data *volatile match_data = NULL;
    int64_t count = 0;
    struct JumpBuf frame;
    if ($PUSH(&frame)) {
        match_data = pcre2_match_data_create(capture_count + 1, NULL);
        if (match_data == NULL)
            RAISE_EXC(&stdQ_reQ_allocation_failed_error);
        PCRE2_SIZE pos = 0;
        PCRE2_SIZE copied = 0;
        unsigned char *buffer = NULL;
        int64_t length = 0;
        int64_t capacity = 0;
        while (pos < text->nbytes) {
            if (stdQ_reQ_exec(re, text, pos, match_data) == PCRE2_ERROR_NOMATCH)
                break;
            PCRE2_SIZE *ovector = pcre2_get_ovector_pointer(match_data);
            PCRE2_SIZE start = ovector[0];
            PCRE2_SIZE end = ovector[1];
            count++;
            if (replacement != NULL) {
                stdQ_reQ_append(&buffer, &length, &capacity, text->str + copied, start - copied);
                stdQ_reQ_append(&buffer, &length, &capacity, replacement->str, replacement->nbytes);
                copied = end;
            }
            pos = end;
            if (start == end) {
                if (pos == text->nbytes)
                    break;
                // Step over one whole UTF-8 character after an empty match.
                do {
                    pos++;
                } while (pos < text->nbytes && (text->str[pos] & 0xC0) == 0x80);
            }
        }
        if (replacement != NULL) {
            if (count == 0) {
                *result = text;
            } else {
                stdQ_reQ_append(&buffer, &length, &capacity, text->str + copied, text->nbytes - copied);
                buffer[length] = 0;
                *result = actStrFromCStringLength((const char *)buffer, length);
            }
        }
        $DROP();
    } else {
        B_BaseException ex = $POP();
        pcre2_match_data_free(match_data);
        GC_reachable_here(self);
        GC_reachable_here(text);
        GC_reachable_here(replacement);
        $RAISE(ex);
    }
    pcre2_match_data_free(match_data);
    GC_reachable_here(self);
    GC_reachable_here(text);
    GC_reachable_here(replacement);
    return count;
}

int64_t stdQ_reQ_PatternD_count(stdQ_reQ_Pattern self, B_str text) {
    return stdQ_reQ_scan(self, text, NULL, NULL);
}

B_str stdQ_reQ_PatternD_replace(stdQ_reQ_Pattern self, B_str text, B_str replacement) {
    B_str result;
    stdQ_reQ_scan(self, text, replacement, &result);
    return result;
}

stdQ_reQ_Match stdQ_reQ__match (B_str arg_pattern, B_str arg_text, int64_t arg_start_pos) {
    PCRE2_SIZE start_offset = stdQ_reQ_start_offset(arg_text, arg_start_pos);
    int errornumber;
    PCRE2_SIZE erroroffset;
    pcre2_code *re = pcre2_compile(
        (PCRE2_SPTR)fromB_str(arg_pattern),
        arg_pattern->nbytes,
        PCRE2_UTF,
        &errornumber,
        &erroroffset,
        NULL
    );
    if (re == NULL)
        stdQ_reQ_raise_compile_error(errornumber, erroroffset);

    // The handler frees the code when the match raises. Its frame is a local
    // of _match, which pops it with $DROP or $POP before it returns or raises
    // again.
    stdQ_reQ_Match result;
    struct JumpBuf frame;
    if ($PUSH(&frame)) {
        result = stdQ_reQ_match_code(re, arg_pattern, arg_text, start_offset);
        $DROP();
    } else {
        B_BaseException ex = $POP();
        pcre2_code_free(re);
        $RAISE(ex);
    }
    pcre2_code_free(re);
    return result;
}
