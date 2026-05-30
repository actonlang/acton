/* gencaps: emit terminfo capability name/index tables from ncurses' Caps file.
 *
 * The compiled terminfo binary format stores boolean/number/string capabilities
 * in a fixed predefined order. ncurses assigns those indices by reading the
 * master "Caps" table top-to-bottom (see include/MKterm.h.awk). We reproduce
 * that exact assignment here so tigetflag/tigetnum/tigetstr resolve a capability
 * name to the same index the database was compiled with.
 *
 * Usage: gencaps <path-to-Caps>   (writes caps_table.h to stdout)
 *
 * Caps columns (whitespace separated): variable_name  ti_name  type  tc_name ...
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Upper bounds on the number of capabilities of each kind. ncurses 6.5 has
 * ~44 booleans, ~39 numbers and ~414 strings; these leave generous headroom
 * but we still guard against a future Caps file outgrowing them rather than
 * silently writing past the arrays. */
#define MAX_BOOL 128
#define MAX_NUM  128
#define MAX_STR  1024
#define CAP_NAME_LEN 32

static void emit_array(const char *name, char names[][CAP_NAME_LEN], int n) {
    printf("static const char *const %s[] = {\n", name);
    for (int i = 0; i < n; i++)
        printf("    \"%s\",\n", names[i]);
    printf("};\n");
    printf("#define %s_count %d\n\n", name, n);
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "usage: %s <Caps>\n", argv[0]);
        return 2;
    }
    FILE *f = fopen(argv[1], "r");
    if (!f) {
        fprintf(stderr, "gencaps: cannot open %s\n", argv[1]);
        return 1;
    }

    static char bool_ti[MAX_BOOL][CAP_NAME_LEN], bool_tc[MAX_BOOL][CAP_NAME_LEN];
    static char num_ti[MAX_NUM][CAP_NAME_LEN], num_tc[MAX_NUM][CAP_NAME_LEN];
    static char str_ti[MAX_STR][CAP_NAME_LEN], str_tc[MAX_STR][CAP_NAME_LEN];
    int nb = 0, nn = 0, ns = 0;

    char line[1024];
    while (fgets(line, sizeof line, f)) {
        if (line[0] == '#' || line[0] == '\n' || line[0] == ' ' || line[0] == '\t')
            continue;
        char var[64], ti[64], type[64], tc[64];
        if (sscanf(line, "%63s %63s %63s %63s", var, ti, type, tc) < 4)
            continue;
        const char *tcp = (strcmp(tc, "-") == 0) ? "" : tc;
        if (strcmp(type, "bool") == 0) {
            if (nb >= MAX_BOOL) {
                fprintf(stderr, "gencaps: too many bool caps (raise MAX_BOOL)\n");
                return 1;
            }
            snprintf(bool_ti[nb], CAP_NAME_LEN, "%s", ti);
            snprintf(bool_tc[nb], CAP_NAME_LEN, "%s", tcp);
            nb++;
        } else if (strcmp(type, "num") == 0) {
            if (nn >= MAX_NUM) {
                fprintf(stderr, "gencaps: too many num caps (raise MAX_NUM)\n");
                return 1;
            }
            snprintf(num_ti[nn], CAP_NAME_LEN, "%s", ti);
            snprintf(num_tc[nn], CAP_NAME_LEN, "%s", tcp);
            nn++;
        } else if (strcmp(type, "str") == 0) {
            if (ns >= MAX_STR) {
                fprintf(stderr, "gencaps: too many str caps (raise MAX_STR)\n");
                return 1;
            }
            snprintf(str_ti[ns], CAP_NAME_LEN, "%s", ti);
            snprintf(str_tc[ns], CAP_NAME_LEN, "%s", tcp);
            ns++;
        }
    }
    fclose(f);

    printf("/* Generated from ncurses Caps - do not edit. */\n\n");
    emit_array("boolnames", bool_ti, nb);
    emit_array("boolcodes", bool_tc, nb);
    emit_array("numnames", num_ti, nn);
    emit_array("numcodes", num_tc, nn);
    emit_array("strnames", str_ti, ns);
    emit_array("strcodes", str_tc, ns);
    return 0;
}
