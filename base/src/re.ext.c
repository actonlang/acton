#define PCRE2_CODE_UNIT_WIDTH 8

#include <pcre2.h>

static void *pcre2_malloc(size_t size, void *data) {
    (void)data;
    return acton_malloc(size);
}

static void pcre2_free(void *ptr, void *data) {}

pcre2_compile_context *compile_context;
pcre2_general_context *general_context;

void reQ___ext_init__() {
    general_context = pcre2_general_context_create(pcre2_malloc, pcre2_free, NULL);
    if (general_context == NULL) {
        // Handle error
        assert(0);
    }
    compile_context = pcre2_compile_context_create(general_context);
}


// TODO: use u64 instead of int for arg_start_pos
reQ_Match reQ_U__match (B_str arg_pattern, B_str arg_text, int64_t arg_start_pos) {
    B_Hashable hwit = (B_Hashable)B_HashableD_strG_witness;
    B_SequenceD_list swit = B_SequenceD_listG_witness;
    B_list groups = B_listG_new(NULL, NULL);
    B_dict named_groups = $NEW(B_dict, hwit, NULL, NULL);

    PCRE2_SPTR pattern = (PCRE2_SPTR)fromB_str(arg_pattern);
    PCRE2_SPTR text = (PCRE2_SPTR)fromB_str(arg_text);
    size_t text_length = strlen((char *)text);
    // TODO: use u64 instead of int to eradicate possibility of < 0

    // Validate start_pos
    long start_offset = arg_start_pos;
    if (start_offset < 0) {
        $RAISE(((B_BaseException)B_ValueErrorG_new(to_str_noc("PCRE2 matching failed: negative start_pos"))));
    }
    if ((size_t)start_offset > text_length) {
        $RAISE(((B_BaseException)B_ValueErrorG_new(to_str_noc("start position is greater than string length"))));
    }

    int errornumber;
    PCRE2_SIZE erroroffset;
    pcre2_code *re = pcre2_compile(
        pattern,
        PCRE2_ZERO_TERMINATED,
        0,
        &errornumber,
        &erroroffset,
        compile_context
    );

    if (re == NULL) {
        PCRE2_UCHAR buffer[256];
        pcre2_get_error_message(errornumber, buffer, sizeof(buffer));
        char errmsg[1024] = "regex compilation failed at offset ";
        snprintf(errmsg + strlen(errmsg), sizeof(errmsg) - strlen(errmsg), "%d: %s", (int)erroroffset, buffer);
        $RAISE(((B_BaseException)B_ValueErrorG_new(to$str(errmsg))));
    }

    pcre2_match_data *match_data = pcre2_match_data_create_from_pattern(re, NULL);
    int rc = pcre2_match(
        re,
        text,
        text_length,
        (PCRE2_SIZE)start_offset,
        0,
        match_data,
        NULL
    );

    if (rc < 0) {
        pcre2_match_data_free(match_data);
        pcre2_code_free(re);

        if (rc == PCRE2_ERROR_NOMATCH) {
            return B_None;
        } else {
            // Some other error
            char errmsg[256];
            snprintf(errmsg, sizeof(errmsg), "PCRE2 matching error: %d", rc);
            $RAISE(((B_BaseException)B_RuntimeErrorG_new(to$str(errmsg))));
        }
    }

    PCRE2_SIZE *ovector = pcre2_get_ovector_pointer(match_data);
    if (rc == 0) {
        pcre2_match_data_free(match_data);
        pcre2_code_free(re);
        $RAISE(((B_BaseException)B_RuntimeErrorG_new(to_str_noc("ovector was not big enough for all captured substrings"))));
    }

    // Extract all unnamed groups
    // group 0 is the entire match, group 1... are subgroups
    for (int i = 0; i < rc; i++) {
        PCRE2_SIZE ss_start = ovector[2*i];
        PCRE2_SIZE ss_end = ovector[2*i+1];
        if (ss_start == PCRE2_UNSET || ss_end == PCRE2_UNSET) {
            swit->$class->append(swit, groups, B_None);
        } else {
            size_t substring_length = ss_end - ss_start;
            char *substring = acton_malloc_atomic(substring_length + 1);
            memcpy(substring, text + ss_start, substring_length);
            substring[substring_length] = '\0';
            swit->$class->append(swit, groups, to_str_noc(substring));
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
            // The name itself starts at tabptr+2 and has length (name_entry_size-2)
            int namelen = name_entry_size - 2;
            char *group_name = acton_malloc_atomic(namelen + 1);
            memcpy(group_name, tabptr+2, namelen);
            group_name[namelen] = '\0';

            PCRE2_SIZE ss_start = ovector[2*n];
            PCRE2_SIZE ss_end = ovector[2*n+1];
            if (ss_start == PCRE2_UNSET || ss_end == PCRE2_UNSET) {
                B_dictD_setitem(named_groups, hwit, to_str_noc(group_name), B_None);
            } else {
                size_t substring_length = ss_end - ss_start;
                char *substring = acton_malloc_atomic(substring_length + 1);
                memcpy(substring, text + ss_start, substring_length);
                substring[substring_length] = '\0';
                B_dictD_setitem(named_groups, hwit, to_str_noc(group_name), to_str_noc(substring));
            }

            tabptr += name_entry_size;
        }
    }

    // Entire match offsets
    PCRE2_SIZE match_start = ovector[0];
    PCRE2_SIZE match_end = ovector[1];

    pcre2_match_data_free(match_data);
    pcre2_code_free(re);

    return reQ_MatchG_new(arg_pattern, arg_text, toB_int(match_start), toB_int(match_end), groups, named_groups);
}
