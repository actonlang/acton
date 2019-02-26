#include "act_trace.h"

#if defined(IS_GNU_LINUX) || defined(IS_FREEBSD) || defined(IS_MACOS)

    void __act_trace_panic_errno(char* format, ...) {
        va_list args;
        va_start(args, format);

        void* backtrace_buffer[__ACT_TRACE_BACKTRACE_ADDRESS_SIZE];
        int naddresses = backtrace(backtrace_buffer, sizeof(backtrace_buffer));
        
        backtrace_symbols_fd(backtrace_buffer, naddresses, STDERR_FILENO);

        err(EXIT_FAILURE, format, args);
        va_end(args);                
    }

#endif