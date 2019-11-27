#include "act_platform.h"

#include <stdarg.h>
#include <stdlib.h>
#include <execinfo.h>
#include <err.h>
#include <unistd.h>

#if defined(IS_GNU_LINUX) || defined(IS_FREEBSD) || defined(IS_MACOS)
    #define __ACT_TRACE_BACKTRACE_ADDRESS_SIZE 4096
    #define __ACT_TRACE_BUFFER_SIZE 65536

    void __act_trace_panic_errno(char* format, ...);

    #define PANIC_ERRNO __act_trace_panic_errno
    #define WARN_ERRNO warn
#endif
