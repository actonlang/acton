#pragma once

#ifdef __gnu_linux__
    #define IS_GNU_LINUX
    #define USE_EPOLL
#elif __FreeBSD__
    #define IS_FREEBSD
    #define USE_KQUEUE
#elif  __APPLE__ && __MACH__
    #define IS_MACOS
    #define USE_KQUEUE
#endif
