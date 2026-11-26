#pragma once

#ifdef __linux__
#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif
#endif

#define GC_THREADS 1
#include <gc.h>

#include <uv.h>

#ifdef __gnu_linux__
    #define IS_GNU_LINUX
#elif  __APPLE__ && __MACH__
    #define IS_MACOS
#endif

extern uv_loop_t *aux_uv_loop;
uv_loop_t *get_uv_loop();

void alloc_buffer(uv_handle_t *handle, size_t size, uv_buf_t *buf);

// Longest address string: an IPv6 address with a zone (fe80::1%eth0)
#define NET_ADDRSTRLEN (INET6_ADDRSTRLEN + UV_IF_NAMESIZE)

// Formats the address of an IPv4 or IPv6 socket address as a string into
// addrbuf and sets *port, the form net's endpoints() gives. An IPv6 address
// with a scope, like a link-local address, gets its zone: fe80::1%eth0.
// Returns 0, or a libuv error code.
int sockaddr_to_addr_port(const struct sockaddr *addr, char *addrbuf, size_t addrbuf_len, int *port);
