#define PCRE2_CODE_UNIT_WIDTH 8

#include <pcre2.h>

static void *pcre2_malloc(size_t size, void *data) {
    (void)data;
    return acton_malloc(size);
}

static void pcre2_free(void *ptr, void *data) {
    (void)data;
    acton_free(ptr);
}

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
reQ_Match reQ__match (B_str arg_pattern, B_str arg_text, B_int arg_start_pos) {
    B_Hashable hwit = B_HashableD_strG_witness;
    B_SequenceD_list swit = B_SequenceD_listG_witness;
    B_list groups = B_listG_new(NULL, NULL);
    B_dict named_groups = $NEW(B_dict, hwit, NULL, NULL);

    pcre2_code *re;
    pcre2_match_data *match_data;
    PCRE2_SIZE erroroffset;
    int errornumber;
    int i;
    int rc;
    PCRE2_SIZE *ovector = NULL;
    PCRE2_SPTR name_table = NULL;
    int name_entry_size;

    PCRE2_SPTR pattern = (PCRE2_SPTR)fromB_str(arg_pattern);
    PCRE2_SPTR text = (PCRE2_SPTR)fromB_str(arg_text);
    size_t text_length = strlen((char *)text);
    // TODO: use u64 instead of int to eradicate possibility of < 0
    if (from$int(arg_start_pos) < 0) {
        $RAISE(((B_BaseException)B_RuntimeErrorG_new(to$str("PCRE2 matching failed"))));
    }
    if (from$int(arg_start_pos) > text_length) {
        $RAISE(((B_BaseException)B_ValueErrorG_new(to$str("start position is greater than string length"))));
    }

    re = pcre2_compile(
                       pattern,               /* the pattern */
                       PCRE2_ZERO_TERMINATED, /* indicates pattern is zero-terminated */
                       0,                     /* default options */
                       &errornumber,          /* for error number */
                       &erroroffset,          /* for error offset */
                       compile_context);      /* use default compile context */


    /* Compilation failed: print the error message and exit. */
    if (re == NULL) {
        PCRE2_UCHAR buffer[256];
        pcre2_get_error_message(errornumber, buffer, sizeof(buffer));
        char errmsg[1024] = "regex compilation failed at offset ";
        snprintf(errmsg + strlen(errmsg), sizeof(errmsg) - strlen(errmsg), "%d: %s", (int)erroroffset, buffer);
        $RAISE(((B_BaseException)B_ValueErrorG_new(to$str(errmsg))));
    }

    match_data = pcre2_match_data_create_from_pattern(re, NULL);
    rc = pcre2_match(
                     re,            /* the compiled pattern */
                     text,          /* the text string */
                     text_length,   /* the length of the text */
                     from$int(arg_start_pos),             /* start at offset 0 in the text */
                     0,             /* default options */
                     match_data,    /* block for storing the result */
                     NULL);         /* use default match context */

    /* Matching failed: handle error cases */
    if (rc < 0) {
        switch(rc) {
            case PCRE2_ERROR_NOMATCH:
                break;
            /* Handle other special cases if you like */
            default:
                // TODO: what to do here? when does this happen?
                printf("Matching error %d\n", rc);
                break;
        }
        pcre2_match_data_free(match_data);   /* Release memory used for the match */
        pcre2_code_free(re);                 /* data and the compiled pattern. */
        return B_None;
    }

    /* Match succeded.
     * Get a pointer to the output vector, where string offsets are stored.
     */
    ovector = pcre2_get_ovector_pointer(match_data);

    /* The output vector wasn't big enough. This should not happen, because we used
       pcre2_match_data_create_from_pattern() above. */
    if (rc == 0)
        $RAISE(((B_BaseException)B_RuntimeErrorG_new(to$str("ovector was not big enough for all the captured substrings"))));


    int end_pos = from$int(arg_start_pos);
    // Get groupings and add to groups list
    for (i = 0; i < rc; i++) {
        size_t ss_start = ovector[2*i];
        size_t ss_end = ovector[2*i+1];
        if (ss_start == PCRE2_UNSET || ss_end == PCRE2_UNSET) {
            swit->$class->append(swit, groups, B_None);
        } else {
            size_t substring_length = ss_end - ss_start;
            // TODO: get rid of extra copy, need to$str that takes a length
            char *substring = acton_malloc(substring_length + 1);
            PCRE2_SPTR substring_start = text + ss_start;
            memcpy(substring, substring_start, substring_length);
            swit->$class->append(swit, groups, to$str(substring));
        }
        end_pos = ss_end;
    }

    /* Get named groupings */

    // Get number of named groupings
    int namecount;
    (void)pcre2_pattern_info(re, PCRE2_INFO_NAMECOUNT, &namecount);

    if (namecount > 0) {
        PCRE2_SPTR tabptr;

        /* Before we can access the substrings, we must extract the table for
           translating names to numbers, and the size of each entry in the table. */

        (void)pcre2_pattern_info(re,                       /* the compiled pattern */
                                 PCRE2_INFO_NAMETABLE,     /* address of the table */
                                 &name_table);             /* where to put the answer */

        (void)pcre2_pattern_info(re,                       /* the compiled pattern */
                                 PCRE2_INFO_NAMEENTRYSIZE, /* size of each entry in the table */
                                 &name_entry_size);        /* where to put the answer */

        /* Now we can scan the table and, for each entry, print the number, the name,
           and the substring itself. In the 8-bit library the number is held in two
           bytes, most significant first. */

        tabptr = name_table;
        for (i = 0; i < namecount; i++) {
            int n = (tabptr[0] << 8) | tabptr[1];

            // Offset of name in name table is 2 bytes after the number of the substring it refers to (n) and 2 bytes after the length of the name string (which is held in the first two bytes of the entry). The name string is not necessarily zero-terminated, so we have to use memcpy() to copy it to a buffer and add a terminating zero.
            char *group_name = acton_malloc(name_entry_size - (2 + 1));
            memcpy(group_name, tabptr + 2, name_entry_size - (2 + 1));
            group_name[name_entry_size - (2 + 1)] = '\0';

            // Substring start and length of named match
            size_t ss_start = ovector[2*i];
            size_t ss_end = ovector[2*i+1];
            if (ss_start == PCRE2_UNSET || ss_end == PCRE2_UNSET) {
                B_dictD_setitem(named_groups, hwit, to_str_noc(group_name), B_None);
            } else {
                PCRE2_SPTR substring_start = text + ss_start;
                size_t substring_length = ss_end - ss_start;

                char *substring = acton_malloc(substring_length + 1);
                memcpy(substring, substring_start, substring_length);
                substring[substring_length] = '\0';

                B_dictD_setitem(named_groups, hwit, to_str_noc(group_name), to_str_noc(substring));
            }

            tabptr += name_entry_size;
        }
    }

    pcre2_match_data_free(match_data);
    pcre2_code_free(re);

    return reQ_MatchG_new(arg_pattern, arg_text, arg_start_pos, to$int(end_pos), groups, named_groups);
}
