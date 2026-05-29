/* Minimal libtinfo: enough of the terminfo API for GHC's `terminfo` boot
 * package (which is what pulls -ltinfo into the acton compiler link).
 *
 * Implements the documented terminfo(5) entry reader, capability lookups
 * (tigetflag/tigetnum/tigetstr and the termcap-name variants), the tparm
 * parameter machine and tputs. Capability name->index tables are generated
 * from ncurses' own Caps file (see caps_table.h / gencaps.c) so the indices
 * match the predefined order used in compiled terminfo databases.
 *
 * This deliberately does not implement curses/screen handling; only the
 * terminfo subset (libtinfo) is needed at link time.
 */
#define _DEFAULT_SOURCE 1
#define _POSIX_C_SOURCE 200809L
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <unistd.h>
#include <fcntl.h>
#include <ctype.h>

#include "caps_table.h"

#define TI_OK 0
#define TI_ERR (-1)

#define TI_MAGIC  0x011A /* octal 0432  - 16-bit numbers */
#define TI_MAGIC2 0x021E /* octal 01036 - 32-bit numbers */

typedef struct TERMINAL {
    char *term_name;
    char *str_table;
    int bool_count, num_count, str_count;
    char bools[boolnames_count];
    int nums[numnames_count];
    char *strs[strnames_count];
} TERMINAL;

TERMINAL *cur_term = NULL;

/* ------------------------------------------------------------------ */
/* terminfo entry reader                                              */
/* ------------------------------------------------------------------ */

static int rd16(const unsigned char *p) {
    int v = (int) (p[0] | (p[1] << 8));
    if (v & 0x8000)
        v -= 0x10000; /* sign-extend */
    return v;
}

static long rd32(const unsigned char *p) {
    uint32_t v = (uint32_t) p[0] | ((uint32_t) p[1] << 8) |
                 ((uint32_t) p[2] << 16) | ((uint32_t) p[3] << 24);
    return (long) (int32_t) v;
}

static unsigned char *read_file(const char *path, size_t *out_len) {
    int fd = open(path, O_RDONLY);
    if (fd < 0)
        return NULL;
    size_t cap = 65536;
    unsigned char *buf = malloc(cap);
    if (!buf) {
        close(fd);
        return NULL;
    }
    size_t total = 0;
    ssize_t r;
    while (total < cap && (r = read(fd, buf + total, cap - total)) > 0)
        total += (size_t) r;
    close(fd);
    *out_len = total;
    return buf;
}

static TERMINAL *parse_terminfo(const unsigned char *d, size_t len,
                                const char *name) {
    if (len < 12)
        return NULL;
    int magic = rd16(d);
    if (magic != TI_MAGIC && magic != TI_MAGIC2)
        return NULL;
    int name_size = rd16(d + 2);
    int bool_count = rd16(d + 4);
    int num_count = rd16(d + 6);
    int str_count = rd16(d + 8);
    int str_size = rd16(d + 10);
    if (name_size < 0 || bool_count < 0 || num_count < 0 || str_count < 0 ||
        str_size < 0)
        return NULL;

    int num_sz = (magic == TI_MAGIC2) ? 4 : 2;
    size_t off = 12;
    const unsigned char *names = d + off;
    off += (size_t) name_size;
    const unsigned char *bools = d + off;
    off += (size_t) bool_count;
    if ((name_size + bool_count) & 1)
        off += 1; /* numbers align on even boundary */
    const unsigned char *nums = d + off;
    off += (size_t) num_count * (size_t) num_sz;
    const unsigned char *soff = d + off;
    off += (size_t) str_count * 2;
    const unsigned char *stbl = d + off;
    if (off + (size_t) str_size > len)
        return NULL;
    (void) names;

    TERMINAL *t = calloc(1, sizeof *t);
    if (!t)
        return NULL;
    t->str_table = malloc((size_t) str_size + 1);
    if (!t->str_table) {
        free(t);
        return NULL;
    }
    memcpy(t->str_table, stbl, (size_t) str_size);
    t->str_table[str_size] = '\0';

    t->bool_count = bool_count < boolnames_count ? bool_count : boolnames_count;
    for (int i = 0; i < t->bool_count; i++)
        t->bools[i] = (char) bools[i];

    t->num_count = num_count < numnames_count ? num_count : numnames_count;
    for (int i = 0; i < t->num_count; i++)
        t->nums[i] = (num_sz == 2) ? rd16(nums + 2 * i) : (int) rd32(nums + 4 * i);

    t->str_count = str_count < strnames_count ? str_count : strnames_count;
    for (int i = 0; i < t->str_count; i++) {
        int o = rd16(soff + 2 * i);
        t->strs[i] = (o >= 0 && o < str_size) ? t->str_table + o : NULL;
    }

    t->term_name = strdup(name);
    return t;
}

/* Try "<dir>/<c>/<name>" and "<dir>/<hex>/<name>". */
static TERMINAL *try_dir(const char *dir, const char *name) {
    if (!dir || !*dir)
        return NULL;
    char path[4096];
    snprintf(path, sizeof path, "%s/%c/%s", dir, name[0], name);
    size_t len = 0;
    unsigned char *d = read_file(path, &len);
    if (!d) {
        snprintf(path, sizeof path, "%s/%02x/%s", dir, (unsigned char) name[0],
                 name);
        d = read_file(path, &len);
    }
    if (!d)
        return NULL;
    TERMINAL *t = parse_terminfo(d, len, name);
    free(d);
    return t;
}

static TERMINAL *find_terminfo(const char *name) {
    if (!name || !*name || strchr(name, '/'))
        return NULL;
    TERMINAL *t;
    const char *env;

    if ((env = getenv("TERMINFO")) && (t = try_dir(env, name)))
        return t;

    const char *home = getenv("HOME");
    if (home) {
        char hdir[4096];
        snprintf(hdir, sizeof hdir, "%s/.terminfo", home);
        if ((t = try_dir(hdir, name)))
            return t;
    }

    if ((env = getenv("TERMINFO_DIRS")) && *env) {
        char *dirs = strdup(env);
        if (dirs) {
            for (char *p = strtok(dirs, ":"); p; p = strtok(NULL, ":")) {
                const char *d = (*p == '\0') ? "/usr/share/terminfo" : p;
                if ((t = try_dir(d, name))) {
                    free(dirs);
                    return t;
                }
            }
            free(dirs);
        }
    }

    static const char *const defaults[] = {
        "/etc/terminfo",          "/lib/terminfo",
        "/usr/share/terminfo",    "/usr/lib/terminfo",
        "/usr/local/share/terminfo", "/usr/local/lib/terminfo",
        "/usr/share/lib/terminfo",
    };
    for (size_t i = 0; i < sizeof defaults / sizeof defaults[0]; i++)
        if ((t = try_dir(defaults[i], name)))
            return t;
    return NULL;
}

/* ------------------------------------------------------------------ */
/* setup / curterm                                                    */
/* ------------------------------------------------------------------ */

int setupterm(const char *term, int filedes, int *errret) {
    (void) filedes;
    if (!term)
        term = getenv("TERM");
    if (!term || !*term) {
        if (errret)
            *errret = 0;
        return TI_ERR;
    }
    TERMINAL *t = find_terminfo(term);
    if (!t) {
        if (errret)
            *errret = 0;
        return TI_ERR;
    }
    cur_term = t;
    if (errret)
        *errret = 1;
    return TI_OK;
}

int restartterm(const char *term, int filedes, int *errret) {
    return setupterm(term, filedes, errret);
}

TERMINAL *set_curterm(TERMINAL *t) {
    TERMINAL *prev = cur_term;
    cur_term = t;
    return prev;
}

int del_curterm(TERMINAL *t) {
    if (t) {
        if (t == cur_term)
            cur_term = NULL;
        free(t->str_table);
        free(t->term_name);
        free(t);
    }
    return TI_OK;
}

/* ------------------------------------------------------------------ */
/* capability lookups                                                 */
/* ------------------------------------------------------------------ */

static int find_name(const char *const *arr, int n, const char *name) {
    for (int i = 0; i < n; i++)
        if (strcmp(arr[i], name) == 0)
            return i;
    return -1;
}

int tigetflag(const char *name) {
    int i = find_name(boolnames, boolnames_count, name);
    if (i < 0)
        return -1; /* not a boolean capability */
    if (!cur_term || i >= cur_term->bool_count)
        return 0;
    return cur_term->bools[i] > 0 ? 1 : 0;
}

int tigetnum(const char *name) {
    int i = find_name(numnames, numnames_count, name);
    if (i < 0)
        return -2; /* not a numeric capability */
    if (!cur_term || i >= cur_term->num_count)
        return -1;
    int v = cur_term->nums[i];
    return v >= 0 ? v : -1;
}

char *tigetstr(const char *name) {
    int i = find_name(strnames, strnames_count, name);
    if (i < 0)
        return (char *) -1; /* not a string capability */
    if (!cur_term || i >= cur_term->str_count)
        return NULL;
    return cur_term->strs[i];
}

int tgetflag(const char *id) {
    int i = find_name(boolcodes, boolnames_count, id);
    if (i < 0 || !cur_term || i >= cur_term->bool_count)
        return 0;
    return cur_term->bools[i] > 0 ? 1 : 0;
}

int tgetnum(const char *id) {
    int i = find_name(numcodes, numnames_count, id);
    if (i < 0 || !cur_term || i >= cur_term->num_count)
        return -1;
    int v = cur_term->nums[i];
    return v >= 0 ? v : -1;
}

char *tgetstr(const char *id, char **area) {
    (void) area;
    int i = find_name(strcodes, strnames_count, id);
    if (i < 0 || !cur_term || i >= cur_term->str_count)
        return NULL;
    return cur_term->strs[i];
}

int tgetent(char *bp, const char *name) {
    (void) bp;
    int err = 0;
    return setupterm(name, 1, &err) == TI_OK ? 1 : 0;
}

/* ------------------------------------------------------------------ */
/* tparm                                                              */
/* ------------------------------------------------------------------ */

/* tparm is non-reentrant, like ncurses' own: the formatted result lives in a
 * single static buffer (tp_out) and the %P/%g dynamic variables persist in a
 * static array (vars) across calls. Callers must consume the result before the
 * next tparm call. This matches GHC terminfo's single-threaded usage. */
#define TPARM_STACK   64   /* operand stack depth */
#define TPARM_OUTBUF  4096 /* max expanded capability length */
#define TPARM_VARS    52   /* %P/%g dynamic vars: a-z = 0..25, A-Z = 26..51 */
#define TPARM_MAXPARM 9    /* %p1..%p9 */

static char tp_out[TPARM_OUTBUF];

static const char *tparm_skip(const char *s, int stop_at_else) {
    int level = 0;
    while (*s) {
        if (*s == '%') {
            char k = s[1];
            if (k == '\0')
                break;
            if (k == '?') {
                level++;
                s += 2;
                continue;
            }
            if (k == ';') {
                if (level == 0)
                    return s + 2;
                level--;
                s += 2;
                continue;
            }
            if (k == 'e' && level == 0 && stop_at_else)
                return s + 2;
            s += 2;
            continue;
        }
        s++;
    }
    return s;
}

static char *tparm_eval(const char *s, long *param) {
    long stack[TPARM_STACK];
    int sp = 0;
    static long vars[TPARM_VARS];
    size_t outi = 0;

#define PUSH(v) do { if (sp < TPARM_STACK) stack[sp++] = (v); } while (0)
#define POP() (sp > 0 ? stack[--sp] : 0)
#define OUTC(c) do { if (outi < sizeof tp_out - 1) tp_out[outi++] = (char)(c); } while (0)

    while (*s) {
        if (*s != '%') {
            OUTC(*s);
            s++;
            continue;
        }
        s++;
        char c = *s;
        /* formatted conversion: %[:][flags][width[.prec]]<doxXsc> */
        if (c == ':' || c == '#' || c == ' ' || c == '.' || isdigit((unsigned char) c) ||
            ((c == '-' || c == '+') &&
             (isdigit((unsigned char) s[1]) || s[1] == ' ' || s[1] == '#'))) {
            char fmt[32];
            int fi = 0;
            fmt[fi++] = '%';
            if (c == ':') {
                s++;
                c = *s;
            }
            while ((c == '-' || c == '+' || c == ' ' || c == '#' || c == '0') &&
                   fi < 28) {
                fmt[fi++] = c;
                s++;
                c = *s;
            }
            while (isdigit((unsigned char) c) && fi < 28) {
                fmt[fi++] = c;
                s++;
                c = *s;
            }
            if (c == '.' && fi < 28) {
                fmt[fi++] = c;
                s++;
                c = *s;
                while (isdigit((unsigned char) c) && fi < 28) {
                    fmt[fi++] = c;
                    s++;
                    c = *s;
                }
            }
            char tmp[256];
            if (c == 's') {
                fmt[fi++] = 's';
                fmt[fi] = '\0';
                char *str = (char *) POP();
                snprintf(tmp, sizeof tmp, fmt, str ? str : "");
            } else if (c == 'd' || c == 'o' || c == 'x' || c == 'X') {
                fmt[fi++] = c;
                fmt[fi] = '\0';
                snprintf(tmp, sizeof tmp, fmt, (int) POP());
            } else if (c == 'c') {
                tmp[0] = (char) POP();
                tmp[1] = '\0';
            } else {
                tmp[0] = '\0';
            }
            for (char *q = tmp; *q; q++)
                OUTC(*q);
            s++;
            continue;
        }
        s++;
        switch (c) {
        case '%':
            OUTC('%');
            break;
        case 'c': {
            OUTC((char) POP());
            break;
        }
        case 'd':
        case 'o':
        case 'x':
        case 'X': {
            char tmp[64];
            char f[3] = {'%', c, '\0'};
            snprintf(tmp, sizeof tmp, f, (int) POP());
            for (char *q = tmp; *q; q++)
                OUTC(*q);
            break;
        }
        case 's': {
            char *str = (char *) POP();
            if (str)
                for (; *str; str++)
                    OUTC(*str);
            break;
        }
        case 'p': {
            int n = *s - '1';
            s++;
            if (n >= 0 && n < TPARM_MAXPARM)
                PUSH(param[n]);
            else
                PUSH(0);
            break;
        }
        case 'P': {
            int idx = (*s >= 'a' && *s <= 'z') ? (*s - 'a')
                      : (*s >= 'A' && *s <= 'Z') ? (26 + *s - 'A')
                                                 : -1;
            s++;
            if (idx >= 0)
                vars[idx] = POP();
            break;
        }
        case 'g': {
            int idx = (*s >= 'a' && *s <= 'z') ? (*s - 'a')
                      : (*s >= 'A' && *s <= 'Z') ? (26 + *s - 'A')
                                                 : -1;
            s++;
            PUSH(idx >= 0 ? vars[idx] : 0);
            break;
        }
        case '\'': {
            PUSH((long) (unsigned char) *s);
            if (*s)
                s++;
            if (*s == '\'')
                s++;
            break;
        }
        case '{': {
            long v = 0;
            int neg = 0;
            if (*s == '-') {
                neg = 1;
                s++;
            }
            while (isdigit((unsigned char) *s)) {
                v = v * 10 + (*s - '0');
                s++;
            }
            if (*s == '}')
                s++;
            PUSH(neg ? -v : v);
            break;
        }
        case 'l': {
            char *str = (char *) POP();
            PUSH(str ? (long) strlen(str) : 0);
            break;
        }
        case '+': { long b = POP(), a = POP(); PUSH(a + b); break; }
        case '-': { long b = POP(), a = POP(); PUSH(a - b); break; }
        case '*': { long b = POP(), a = POP(); PUSH(a * b); break; }
        case '/': { long b = POP(), a = POP(); PUSH(b ? a / b : 0); break; }
        case 'm': { long b = POP(), a = POP(); PUSH(b ? a % b : 0); break; }
        case '&': { long b = POP(), a = POP(); PUSH(a & b); break; }
        case '|': { long b = POP(), a = POP(); PUSH(a | b); break; }
        case '^': { long b = POP(), a = POP(); PUSH(a ^ b); break; }
        case '=': { long b = POP(), a = POP(); PUSH(a == b); break; }
        case '<': { long b = POP(), a = POP(); PUSH(a < b); break; }
        case '>': { long b = POP(), a = POP(); PUSH(a > b); break; }
        case 'A': { long b = POP(), a = POP(); PUSH(a && b); break; }
        case 'O': { long b = POP(), a = POP(); PUSH(a || b); break; }
        case '!': { long a = POP(); PUSH(!a); break; }
        case '~': { long a = POP(); PUSH(~a); break; }
        case 'i':
            param[0]++;
            param[1]++;
            break;
        case '?':
            break; /* start if */
        case 't': {
            long cond = POP();
            if (!cond)
                s = tparm_skip(s, 1); /* skip to %e or %; */
            break;
        }
        case 'e':
            s = tparm_skip(s, 0); /* executed then-branch; skip else to %; */
            break;
        case ';':
            break; /* end if */
        default:
            break;
        }
    }
    tp_out[outi] = '\0';
    return tp_out;
#undef PUSH
#undef POP
#undef OUTC
}

/* Read the (up to 9) parameters and run the evaluator. We pull each parameter
 * as a `long`: ncurses' classic tparm signature is varargs of long/int, and on
 * the LP64 targets we build for (x86_64 and aarch64 Linux) int args are widened
 * to the 64-bit slot, so reading them as `long` is correct. */
static char *tparm_va(const char *str, va_list ap) {
    long param[TPARM_MAXPARM];
    for (int i = 0; i < TPARM_MAXPARM; i++)
        param[i] = va_arg(ap, long);
    if (!str || str == (char *) -1) {
        tp_out[0] = '\0';
        return tp_out;
    }
    return tparm_eval(str, param);
}

char *tparm(const char *str, ...) {
    va_list ap;
    va_start(ap, str);
    char *r = tparm_va(str, ap);
    va_end(ap);
    return r;
}

char *tiparm(const char *str, ...) {
    va_list ap;
    va_start(ap, str);
    char *r = tparm_va(str, ap);
    va_end(ap);
    return r;
}

char *tgoto(const char *cap, int col, int row) {
    return tparm(cap, (long) row, (long) col);
}

/* ------------------------------------------------------------------ */
/* tputs                                                              */
/* ------------------------------------------------------------------ */

int tputs(const char *str, int affcnt, int (*outc)(int)) {
    (void) affcnt;
    if (!str || str == (char *) -1)
        return TI_ERR;
    for (const char *p = str; *p;) {
        if (p[0] == '$' && p[1] == '<') {
            const char *q = strchr(p, '>');
            if (q) { /* drop padding spec; modern terminals don't need it */
                p = q + 1;
                continue;
            }
        }
        outc((unsigned char) *p);
        p++;
    }
    return TI_OK;
}

int putp(const char *str) {
    return tputs(str, 1, putchar);
}
