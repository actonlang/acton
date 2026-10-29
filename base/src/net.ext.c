#define GC_THREADS 1
#ifdef _WIN32
#include <winsock2.h>
#endif
#include <gc.h>

#include <stdint.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <tlsuv/tlsuv.h>
#include <uv.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef _WIN32
#include <arpa/inet.h>
#include <net/if.h>
#include <netinet/in.h>
#include <unistd.h>
#endif
#include "../rts/io.h"
#include "../rts/log.h"

void netQ___ext_init__() {
    // NOP
}

// Longest address string: an IPv6 address with a zone (fe80::1%eth0)
#define NET_ADDRSTRLEN (INET6_ADDRSTRLEN + UV_IF_NAMESIZE)
// ...with a port: [fe80::1%eth0]:65535
#define NET_ADDRPORTSTRLEN (NET_ADDRSTRLEN + 8)
// A destination named in messages: a host name or address, and a port
#define NET_DESTSTRLEN (NET_ADDRPORTSTRLEN + 256)

// Parses an IPv6 address, optionally with a zone given by interface name or
// index: fe80::1%eth0 or fe80::1%2. Unlike uv_ip6_addr, an unknown interface
// is an error rather than silently ignored.
static int parse_ip6(const char *address, int port, struct sockaddr_in6 *addr6) {
    const char *zone = strchr(address, '%');
    if (zone == NULL) {
        return uv_ip6_addr(address, port, addr6);
    }
    char ip[INET6_ADDRSTRLEN];
    size_t ip_len = (size_t)(zone - address);
    zone++;
    if (ip_len >= sizeof(ip) || *zone == '\0') {
        return UV_EINVAL;
    }
    memcpy(ip, address, ip_len);
    ip[ip_len] = '\0';
    int r = uv_ip6_addr(ip, port, addr6);
    if (r != 0) {
        return r;
    }
    char *end = NULL;
    unsigned long index = strtoul(zone, &end, 10);
    if (*end != '\0') {
#ifdef _WIN32
        return UV_EINVAL;
#else
        index = if_nametoindex(zone);
#endif
    }
    if (index == 0 || index > UINT32_MAX) {
        return UV_EINVAL;
    }
    addr6->sin6_scope_id = (uint32_t)index;
    return 0;
}


bool netQ_is_ipv4 (B_str address) {
    struct sockaddr_in sa;
    if (inet_pton(AF_INET, (const char *)fromB_str(address), &(sa.sin_addr)) == 0) {
        return false;
    } else {
        return true;
    }
}

bool netQ_is_ipv6 (B_str address) {
    struct sockaddr_in6 sa;
    return parse_ip6((const char *)fromB_str(address), 0, &sa) == 0;
}

// Holds GC pointers (callback closures, hostname bytes) but is referenced only
// from the libc-allocated uv request while a lookup is in flight, so it must
// be GC_malloc_uncollectable: GC-scanned (keeping the closures alive) with an
// explicit lifetime ending in the resolve callback.
struct dns_cb_data {
    char* hostname;
    $action on_resolve;
    $action2 on_error;
};

static B_NoneType net_lookup_address(B_str name,
                                     $action on_resolve,
                                     $action on_error,
                                     int family,
                                     int socktype,
                                     int protocol,
                                     uv_getaddrinfo_cb cb) {
    // libuv copies node/service/hints synchronously into its own request
    // buffer, so hints can live on the stack.
    struct addrinfo hints = {0};
    hints.ai_family = family;
    hints.ai_socktype = socktype;
    hints.ai_protocol = protocol;

    struct dns_cb_data *cb_data = (struct dns_cb_data *)GC_malloc_uncollectable(sizeof(struct dns_cb_data));
    if (cb_data == NULL) {
        $action2 f = ($action2)on_error;
        f->$class->__asyn__(f, name, actStrFromCString("Unable to run DNS query: out of memory"));
        return B_None;
    }
    cb_data->hostname = (char *)fromB_str(name);
    cb_data->on_resolve = on_resolve;
    cb_data->on_error = ($action2)on_error;

    // The request enters libuv's threadpool work queue, whose intrusive links
    // pass through libc-allocated nodes (tlsuv connects); a GC-allocated
    // request would be invisible to the collector while queued behind one.
    uv_getaddrinfo_t *req = (uv_getaddrinfo_t*)malloc(sizeof(uv_getaddrinfo_t));
    if (req == NULL) {
        GC_free(cb_data);
        $action2 f = ($action2)on_error;
        f->$class->__asyn__(f, name, actStrFromCString("Unable to run DNS query: out of memory"));
        return B_None;
    }
    req->data = cb_data;

    int r = uv_getaddrinfo(get_uv_loop(), req, cb, (const char *)fromB_str(name), NULL, &hints);
    if (r != 0) {
        char errmsg[1024] = "Unable to run DNS query: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)cb_data->on_error;
        f->$class->__asyn__(f, name, actStrFromCStringCopy(errmsg));
        GC_free(cb_data);
        free(req);
        return B_None;
    }

    return B_None;
}

static void _lookup_a__on_resolve (uv_getaddrinfo_t *req, int status, struct addrinfo *dns_res) {
    struct dns_cb_data *cb_data = req->data;
    B_list $res = B_listG_new(NULL, NULL);

    if (status != 0) {
        char errmsg[1024] = "DNS lookup error: ";
        uv_strerror_r(status, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        $action2 f = ($action2)cb_data->on_error;
        f->$class->__asyn__(f, actStrFromCStringCopy(cb_data->hostname), actStrFromCStringCopy(errmsg));

        uv_freeaddrinfo(dns_res);
        GC_free(cb_data);
        free(req);
        return;
    }

    struct addrinfo *rp;
    char addr[17] = {'\0'};
    for (rp = dns_res; rp != NULL; rp = rp->ai_next) {
        uv_ip4_name((struct sockaddr_in*) rp->ai_addr, addr, 16);
        B_SequenceD_listG_witness->$class->append(B_SequenceD_listG_witness, $res, actStrFromCStringCopy(addr));
    }

    $action f = ($action)cb_data->on_resolve;
    f->$class->__asyn__(f, $res);

    uv_freeaddrinfo(dns_res);
    GC_free(cb_data);
    free(req);
}

B_NoneType netQ__lookup_a (B_str name, $action on_resolve, $action on_error) {
    return net_lookup_address(name, on_resolve, on_error, PF_INET, SOCK_STREAM, IPPROTO_TCP, _lookup_a__on_resolve);
}

static void _lookup_aaaa__on_resolve (uv_getaddrinfo_t *req, int status, struct addrinfo *dns_res) {
    struct dns_cb_data *cb_data = req->data;
    B_list $res = B_listG_new(NULL, NULL);

    if (status != 0) {
        char errmsg[1024] = "DNS lookup error: ";
        uv_strerror_r(status, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        $action2 f = ($action2)cb_data->on_error;
        f->$class->__asyn__(f, actStrFromCStringCopy(cb_data->hostname), actStrFromCStringCopy(errmsg));

        uv_freeaddrinfo(dns_res);
        GC_free(cb_data);
        free(req);
        return;
    }

    struct addrinfo *rp;
    char addr[40] = {'\0'};
    for (rp = dns_res; rp != NULL; rp = rp->ai_next) {
        uv_ip6_name((struct sockaddr_in6*)rp->ai_addr, addr, 39);
        B_SequenceD_listG_witness->$class->append(B_SequenceD_listG_witness, $res, actStrFromCStringCopy(addr));
    }

    $action f = ($action)cb_data->on_resolve;
    f->$class->__asyn__(f, $res);

    uv_freeaddrinfo(dns_res);
    GC_free(cb_data);
    free(req);
}

B_NoneType netQ__lookup_aaaa (B_str name, $action on_resolve, $action on_error) {
    return net_lookup_address(name, on_resolve, on_error, PF_INET6, SOCK_STREAM, IPPROTO_TCP, _lookup_aaaa__on_resolve);
}

B_NoneType netQ__lookup_udp_a (B_str name, $action on_resolve, $action on_error) {
    return net_lookup_address(name, on_resolve, on_error, PF_INET, SOCK_DGRAM, IPPROTO_UDP, _lookup_a__on_resolve);
}

B_NoneType netQ__lookup_udp_aaaa (B_str name, $action on_resolve, $action on_error) {
    return net_lookup_address(name, on_resolve, on_error, PF_INET6, SOCK_DGRAM, IPPROTO_UDP, _lookup_aaaa__on_resolve);
}

struct udp_send_req_state {
    char *buf;
    size_t len;
    $WORD actor;
    bool listener;
    char dest[NET_DESTSTRLEN];
};

static void udp_call_receive($WORD cb, $WORD actor, B_bytes data, B_str address, B_int port) {
    ((B_Msg (*)($WORD, $WORD, $WORD, $WORD, $WORD))(($action)cb)->$class->__asyn__)(cb, actor, data, address, port);
}

// Formats an address as a string. An IPv6 address with a scope, like a
// link-local address, gets its zone: fe80::1%eth0.
static int sockaddr_to_addr_port(const struct sockaddr *addr, char *addrbuf, size_t addrbuf_len, int *port) {
    if (addr == NULL) {
        return UV_EINVAL;
    }
    if (addr->sa_family == AF_INET) {
        const struct sockaddr_in *addr4 = (const struct sockaddr_in *)addr;
        int r = uv_ip4_name(addr4, addrbuf, addrbuf_len);
        if (r != 0) {
            return r;
        }
        *port = (int)ntohs(addr4->sin_port);
        return 0;
    } else if (addr->sa_family == AF_INET6) {
        const struct sockaddr_in6 *addr6 = (const struct sockaddr_in6 *)addr;
        int r = uv_ip6_name(addr6, addrbuf, addrbuf_len);
        if (r != 0) {
            return r;
        }
        if (addr6->sin6_scope_id != 0) {
            size_t len = strlen(addrbuf);
            char zone[UV_IF_NAMESIZE];
            size_t zone_len = sizeof(zone);
            if (uv_if_indextoname(addr6->sin6_scope_id, zone, &zone_len) != 0) {
                snprintf(zone, sizeof(zone), "%u", (unsigned)addr6->sin6_scope_id);
            }
            snprintf(addrbuf + len, addrbuf_len - len, "%%%s", zone);
        }
        *port = (int)ntohs(addr6->sin6_port);
        return 0;
    }
    return UV_EINVAL;
}

// Formats address and port for messages: 192.0.2.1:161 or [2001:db8::1]:161
static void format_addr_port(char *buf, size_t buf_len, const char *address, int64_t port) {
    if (strchr(address, ':') != NULL) {
        snprintf(buf, buf_len, "[%s]:%lld", address, (long long)port);
    } else {
        snprintf(buf, buf_len, "%s:%lld", address, (long long)port);
    }
}

// Parses an IPv4 or IPv6 address, see parse_ip6 for zones
static int make_sockaddr(B_str address, int64_t port, struct sockaddr_storage *dest) {
    // TODO: port should be uint16 at the Acton API boundary.
    const char *addr = (const char *)fromB_str(address);
    struct sockaddr_in addr4;
    struct sockaddr_in6 addr6;
    if (uv_ip4_addr(addr, (int)port, &addr4) == 0) {
        memcpy(dest, &addr4, sizeof(addr4));
        return 0;
    }
    int r = parse_ip6(addr, (int)port, &addr6);
    if (r == 0) {
        memcpy(dest, &addr6, sizeof(addr6));
    }
    return r;
}

static int make_udp_bind_addr(int family, struct sockaddr_storage *dest) {
    if (family == AF_INET) {
        struct sockaddr_in addr4;
        int r = uv_ip4_addr("0.0.0.0", 0, &addr4);
        if (r == 0) {
            memcpy(dest, &addr4, sizeof(addr4));
        }
        return r;
    } else if (family == AF_INET6) {
        struct sockaddr_in6 addr6;
        int r = uv_ip6_addr("::", 0, &addr6);
        if (r == 0) {
            memcpy(dest, &addr6, sizeof(addr6));
        }
        return r;
    }
    return UV_EINVAL;
}

static struct udp_send_req_state *udp_send_req_state_new(B_bytes data, $WORD actor, bool listener, const char *dest) {
    struct udp_send_req_state *state = (struct udp_send_req_state *)malloc(sizeof(*state));
    if (state == NULL) {
        return NULL;
    }

    state->len = data->nbytes;
    state->buf = (char *)malloc(state->len > 0 ? state->len : 1);
    if (state->buf == NULL) {
        free(state);
        return NULL;
    }
    if (state->len > 0) {
        memcpy(state->buf, data->str, state->len);
    }
    state->actor = actor;
    state->listener = listener;
    snprintf(state->dest, sizeof(state->dest), "%s", dest);
    return state;
}

static void udp_send_req_state_free(struct udp_send_req_state *state) {
    if (state != NULL) {
        free(state->buf);
        free(state);
    }
}

static void udp_send_cb(uv_udp_send_t *req, int status) {
    struct udp_send_req_state *state = (struct udp_send_req_state *)req->data;
    if (status < 0 && status != UV_ECANCELED && state != NULL && state->actor != NULL) {
        char errmsg[1024];
        snprintf(errmsg, sizeof(errmsg), "Failed to send UDP datagram to %s: ", state->dest);
        uv_strerror_r(status, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        if (state->listener) {
            netQ_UDPListener self = (netQ_UDPListener)state->actor;
            $action2 f = ($action2)self->on_error;
            f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
        } else {
            netQ_UDPConnection self = (netQ_UDPConnection)state->actor;
            $action2 f = ($action2)self->on_error;
            f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
        }
    }
    udp_send_req_state_free(state);
    free(req);
}

static void udp_connection_on_receive(uv_udp_t *handle, ssize_t nread, const uv_buf_t *buf, const struct sockaddr *addr, unsigned flags) {
    (void)flags;
    netQ_UDPConnection self = (netQ_UDPConnection)handle->data;
    if (self == NULL) {
        return;
    }

    if (nread >= 0 && addr != NULL) {
        char addrbuf[NET_ADDRSTRLEN] = { '\0' };
        int port = 0;
        int r = sockaddr_to_addr_port(addr, addrbuf, sizeof(addrbuf), &port);
        if (r != 0) {
            char errmsg[1024] = "Failed to format UDP source address: ";
            uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
            log_warn(errmsg);
            $action2 f = ($action2)self->on_error;
            f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
            return;
        }
        B_bytes data = actBytesFromCStringLengthCopy(buf->base, nread);
        udp_call_receive(($WORD)self->on_receive, ($WORD)self, data, actStrFromCStringCopy(addrbuf), toB_int(port));
        self->_bytes_in += nread;
        self->_datagrams_in += 1;
    } else if (nread < 0) {
        char errmsg[1024] = "UDP receive error: ";
        uv_strerror_r((int)nread, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)self->on_error;
        f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
    }
}

static void udp_listener_on_receive(uv_udp_t *handle, ssize_t nread, const uv_buf_t *buf, const struct sockaddr *addr, unsigned flags) {
    (void)flags;
    netQ_UDPListener self = (netQ_UDPListener)handle->data;
    if (self == NULL) {
        return;
    }

    if (nread >= 0 && addr != NULL) {
        char addrbuf[NET_ADDRSTRLEN] = { '\0' };
        int port = 0;
        int r = sockaddr_to_addr_port(addr, addrbuf, sizeof(addrbuf), &port);
        if (r != 0) {
            char errmsg[1024] = "Failed to format UDP source address: ";
            uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
            log_warn(errmsg);
            $action2 f = ($action2)self->on_error;
            f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
            return;
        }
        B_bytes data = actBytesFromCStringLengthCopy(buf->base, nread);
        udp_call_receive(($WORD)self->on_receive, ($WORD)self, data, actStrFromCStringCopy(addrbuf), toB_int(port));
        self->_bytes_in += nread;
        self->_datagrams_in += 1;
    } else if (nread < 0) {
        char errmsg[1024] = "UDP receive error: ";
        uv_strerror_r((int)nread, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)self->on_error;
        f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
    }
}

static void udp_close_handle(int64_t *sock_ptr) {
    uv_udp_t *udp = (uv_udp_t *)(intptr_t)*sock_ptr;
    if ((intptr_t)udp == -1) {
        return;
    }

    uv_udp_recv_stop(udp);
    if (uv_is_closing((uv_handle_t *)udp) == 0) {
        uv_close((uv_handle_t *)udp, NULL);
    }
    *sock_ptr = -1LL;
}

static void udp_close_initialized_handle(uv_udp_t *udp) {
    if (udp != NULL && uv_is_closing((uv_handle_t *)udp) == 0) {
        uv_close((uv_handle_t *)udp, NULL);
    }
}

static int udp_connection_start(netQ_UDPConnection self, B_str ip_address, int family) {
    uv_udp_t *udp = (uv_udp_t *)acton_malloc(sizeof(uv_udp_t));
    int r = uv_udp_init(get_uv_loop(), udp);
    if (r != 0) {
        return r;
    }

    struct sockaddr_storage bind_addr;
    r = make_udp_bind_addr(family, &bind_addr);
    if (r != 0) {
        udp_close_initialized_handle(udp);
        return r;
    }

    r = uv_udp_bind(udp, (const struct sockaddr *)&bind_addr, 0);
    if (r != 0) {
        udp_close_initialized_handle(udp);
        return r;
    }

    struct sockaddr_storage remote_addr;
    r = make_sockaddr(ip_address, self->port, &remote_addr);
    if (r != 0) {
        udp_close_initialized_handle(udp);
        return r;
    }

    r = uv_udp_connect(udp, (const struct sockaddr *)&remote_addr);
    if (r != 0) {
        udp_close_initialized_handle(udp);
        return r;
    }

    udp->data = self;
    r = uv_udp_recv_start(udp, alloc_buffer, udp_connection_on_receive);
    if (r != 0) {
        udp_close_initialized_handle(udp);
        return r;
    }

    self->_sock = (int64_t)(intptr_t)udp;
    self->_ip_version = family == AF_INET ? 4LL : 6LL;
    self->_remote_address = ip_address;
    self->$class->_on_udp_ready(self);
    return 0;
}

$R netQ_UDPConnectionD__pin_affinityG_local (netQ_UDPConnection self, $Cont c$cont) {
    pin_actor_affinity();
    return $R_CONT(c$cont, B_None);
}

$R netQ_UDPConnectionD__connect4G_local (netQ_UDPConnection self, $Cont c$cont, B_str ip_address) {
    log_debug("UDP connecting over IPv4 to %s", fromB_str(ip_address));
    int r = udp_connection_start(self, ip_address, AF_INET);
    if (r != 0) {
        char errmsg[1024] = "Error in UDP setup over IPv4: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        self->$class->_on_udp_error(self, (int64_t)r, actStrFromCStringCopy(errmsg));
    }
    return $R_CONT(c$cont, B_None);
}

$R netQ_UDPConnectionD__connect6G_local (netQ_UDPConnection self, $Cont c$cont, B_str ip_address) {
    log_debug("UDP connecting over IPv6 to %s", fromB_str(ip_address));
    int r = udp_connection_start(self, ip_address, AF_INET6);
    if (r != 0) {
        char errmsg[1024] = "Error in UDP setup over IPv6: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        self->$class->_on_udp_error(self, (int64_t)r, actStrFromCStringCopy(errmsg));
    }
    return $R_CONT(c$cont, B_None);
}

$R netQ_UDPConnectionD_writeG_local (netQ_UDPConnection self, $Cont c$cont, B_bytes data) {
    uv_udp_t *udp = (uv_udp_t *)(intptr_t)self->_sock;
    if ((intptr_t)udp == -1) {
        return $R_CONT(c$cont, B_None);
    }

    char dest[NET_ADDRPORTSTRLEN];
    format_addr_port(dest, sizeof(dest), (const char *)fromB_str(self->_remote_address), self->port);
    uv_udp_send_t *req = (uv_udp_send_t *)malloc(sizeof(uv_udp_send_t));
    struct udp_send_req_state *state = udp_send_req_state_new(data, ($WORD)self, false, dest);
    if (req == NULL || state == NULL) {
        free(req);
        udp_send_req_state_free(state);
        $action2 f = ($action2)self->on_error;
        f->$class->__asyn__(f, self, actStrFromCString("Failed to allocate UDP send request"));
        return $R_CONT(c$cont, B_None);
    }
    req->data = state;
    uv_buf_t buf = uv_buf_init(state->buf, state->len);
    int r = uv_udp_send(req, udp, &buf, 1, NULL, udp_send_cb);
    if (r != 0) {
        char errmsg[1024];
        snprintf(errmsg, sizeof(errmsg), "Failed to send UDP datagram to %s: ", dest);
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)self->on_error;
        f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
        udp_send_req_state_free(state);
        free(req);
        return $R_CONT(c$cont, B_None);
    }
    self->_bytes_out += data->nbytes;
    self->_datagrams_out += 1;
    return $R_CONT(c$cont, B_None);
}

$R netQ_UDPConnectionD_closeG_local (netQ_UDPConnection self, $Cont c$cont) {
    self->_state = self->STATE_CLOSED;
    udp_close_handle(&self->_sock);
    return $R_CONT(c$cont, B_None);
}

B_NoneType netQ_UDPConnectionD___resume__ (netQ_UDPConnection self) {
    self->_sock = -1LL;
    self->_state = self->STATE_CLOSED;
    $action2 f = ($action2)self->on_error;
    f->$class->__asyn__(f, self, actStrFromCString("resume"));
    return B_None;
}

$R netQ_UDPConnectionD_local_addressG_local (netQ_UDPConnection self, $Cont c$cont) {
    uv_udp_t *udp = (uv_udp_t *)(intptr_t)self->_sock;
    if ((intptr_t)udp == -1) {
        return $R_CONT(c$cont, actStrFromCString(""));
    }
    struct sockaddr_storage sockname;
    int namelen = sizeof(sockname);
    char addr[NET_ADDRSTRLEN] = { '\0' };
    if (uv_udp_getsockname((const uv_udp_t *)udp, (struct sockaddr *)&sockname, &namelen) == 0) {
        int port = 0;
        if (sockaddr_to_addr_port((struct sockaddr *)&sockname, addr, sizeof(addr), &port) != 0) {
            addr[0] = '\0';
        }
    }
    return $R_CONT(c$cont, actStrFromCStringCopy(addr));
}

$R netQ_UDPConnectionD_local_portG_local (netQ_UDPConnection self, $Cont c$cont) {
    uv_udp_t *udp = (uv_udp_t *)(intptr_t)self->_sock;
    if ((intptr_t)udp == -1) {
        return $R_CONT(c$cont, toB_int(0));
    }
    struct sockaddr_storage sockname;
    int namelen = sizeof(sockname);
    char addr[NET_ADDRSTRLEN] = { '\0' };
    int port = 0;
    if (uv_udp_getsockname((const uv_udp_t *)udp, (struct sockaddr *)&sockname, &namelen) == 0) {
        sockaddr_to_addr_port((struct sockaddr *)&sockname, addr, sizeof(addr), &port);
    }
    return $R_CONT(c$cont, toB_int(port));
}

static int udp_set_recv_buffer_size(uv_udp_t *udp, int64_t size) {
    if (size <= 0 || size > INT_MAX) {
        return UV_EINVAL;
    }
    int value = (int)size;
    return uv_recv_buffer_size((uv_handle_t *)udp, &value);
}

$R netQ_UDPListenerD__initG_local (netQ_UDPListener self, $Cont c$cont) {
    pin_actor_affinity();

    uv_udp_t *udp = (uv_udp_t *)acton_malloc(sizeof(uv_udp_t));
    int r = uv_udp_init(get_uv_loop(), udp);
    if (r != 0) {
        char errmsg[1024] = "Failed to initialize UDP listener: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
        return $R_CONT(c$cont, B_None);
    }

    struct sockaddr_storage bind_addr;
    r = make_sockaddr(self->address, self->port, &bind_addr);
    if (r != 0) {
        udp_close_initialized_handle(udp);
        B_str errmsg = $FORMAT("Address is not an IPv4 or IPv6 address: %s", fromB_str(self->address));
        log_warn((const char *)fromB_str(errmsg));
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, errmsg);
        return $R_CONT(c$cont, B_None);
    }

    r = uv_udp_bind(udp, (const struct sockaddr *)&bind_addr, 0);
    if (r != 0) {
        udp_close_initialized_handle(udp);
        char errmsg[1024] = "Error in UDP bind: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
        return $R_CONT(c$cont, B_None);
    }

    if (self->recv_buffer_size != 0) {
        r = udp_set_recv_buffer_size(udp, self->recv_buffer_size);
        if (r != 0) {
            udp_close_initialized_handle(udp);
            char errmsg[1024] = "Failed to set UDP receive buffer size: ";
            uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
            log_warn(errmsg);
            $action2 f = ($action2)self->on_listen;
            f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
            return $R_CONT(c$cont, B_None);
        }
    }

    udp->data = self;
    r = uv_udp_recv_start(udp, alloc_buffer, udp_listener_on_receive);
    if (r != 0) {
        udp_close_initialized_handle(udp);
        char errmsg[1024] = "Failed to start reading from UDP listener: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
        return $R_CONT(c$cont, B_None);
    }

    self->_sock = (int64_t)(intptr_t)udp;
    $action2 f = ($action2)self->on_listen;
    f->$class->__asyn__(f, self, B_None);
    return $R_CONT(c$cont, B_None);
}

// Sends a datagram to a parsed address, dest_name names it in errors
static void udp_listener_send_to(netQ_UDPListener self, uv_udp_t *udp, B_bytes data, const struct sockaddr *dest, const char *dest_name) {
    uv_udp_send_t *req = (uv_udp_send_t *)malloc(sizeof(uv_udp_send_t));
    struct udp_send_req_state *state = udp_send_req_state_new(data, ($WORD)self, true, dest_name);
    if (req == NULL || state == NULL) {
        free(req);
        udp_send_req_state_free(state);
        $action2 f = ($action2)self->on_error;
        f->$class->__asyn__(f, self, actStrFromCString("Failed to allocate UDP send request"));
        return;
    }
    req->data = state;
    uv_buf_t buf = uv_buf_init(state->buf, state->len);
    int r = uv_udp_send(req, udp, &buf, 1, dest, udp_send_cb);
    if (r != 0) {
        char errmsg[1024];
        snprintf(errmsg, sizeof(errmsg), "Failed to send UDP datagram to %s: ", dest_name);
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)self->on_error;
        f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
        udp_send_req_state_free(state);
        free(req);
        return;
    }
    self->_bytes_out += data->nbytes;
    self->_datagrams_out += 1;
}

static void udp_listener_send_error(netQ_UDPListener self, const char *dest_name, const char *reason) {
    char errmsg[1024];
    snprintf(errmsg, sizeof(errmsg), "Failed to send UDP datagram to %s: %s", dest_name, reason);
    log_warn(errmsg);
    $action2 f = ($action2)self->on_error;
    f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
}

// A datagram waiting for its destination host name to resolve. Like struct
// dns_cb_data it holds GC pointers but is referenced only from the libc
// request, so it is GC_malloc_uncollectable.
struct udp_resolve_state {
    netQ_UDPListener self;
    B_bytes data;
    int64_t port;
    char dest_name[NET_DESTSTRLEN];
};

static void udp_listener_on_resolve(uv_getaddrinfo_t *req, int status, struct addrinfo *res) {
    struct udp_resolve_state *state = (struct udp_resolve_state *)req->data;
    netQ_UDPListener self = state->self;
    uv_udp_t *udp = (uv_udp_t *)(intptr_t)self->_sock;
    if ((intptr_t)udp == -1) {
        // The listener was closed while resolving
    } else if (status != 0 || res == NULL) {
        char reason[256];
        snprintf(reason, sizeof(reason), "unable to resolve host name: %s", uv_strerror(status != 0 ? status : UV_EAI_NONAME));
        udp_listener_send_error(self, state->dest_name, reason);
    } else {
        struct sockaddr_storage dest;
        memcpy(&dest, res->ai_addr, res->ai_addrlen);
        if (dest.ss_family == AF_INET) {
            ((struct sockaddr_in *)&dest)->sin_port = htons((uint16_t)state->port);
        } else {
            ((struct sockaddr_in6 *)&dest)->sin6_port = htons((uint16_t)state->port);
        }
        udp_listener_send_to(self, udp, state->data, (const struct sockaddr *)&dest, state->dest_name);
    }
    uv_freeaddrinfo(res);
    GC_free(state);
    free(req);
}

// Resolves a host name to an address of the listener socket's family, then
// sends the datagram there
static void udp_listener_resolve_and_send(netQ_UDPListener self, uv_udp_t *udp, B_bytes data, B_str address, int64_t port, const char *dest_name) {
    struct sockaddr_storage sockname;
    int namelen = sizeof(sockname);
    int r = uv_udp_getsockname(udp, (struct sockaddr *)&sockname, &namelen);
    if (r != 0) {
        udp_listener_send_error(self, dest_name, uv_strerror(r));
        return;
    }
    // libuv copies the name and hints, see net_lookup_address
    struct addrinfo hints = {0};
    hints.ai_family = sockname.ss_family;
    hints.ai_socktype = SOCK_DGRAM;
    hints.ai_protocol = IPPROTO_UDP;
#ifdef AI_V4MAPPED
    if (sockname.ss_family == AF_INET6) {
        hints.ai_flags = AI_V4MAPPED;
    }
#endif

    struct udp_resolve_state *state = (struct udp_resolve_state *)GC_malloc_uncollectable(sizeof(*state));
    // libc memory, see net_lookup_address
    uv_getaddrinfo_t *req = (uv_getaddrinfo_t *)malloc(sizeof(uv_getaddrinfo_t));
    if (state == NULL || req == NULL) {
        GC_free(state);
        free(req);
        udp_listener_send_error(self, dest_name, "out of memory");
        return;
    }
    state->self = self;
    state->data = data;
    state->port = port;
    snprintf(state->dest_name, sizeof(state->dest_name), "%s", dest_name);
    req->data = state;
    r = uv_getaddrinfo(get_uv_loop(), req, udp_listener_on_resolve, (const char *)fromB_str(address), NULL, &hints);
    if (r != 0) {
        GC_free(state);
        free(req);
        char reason[256];
        snprintf(reason, sizeof(reason), "unable to resolve host name: %s", uv_strerror(r));
        udp_listener_send_error(self, dest_name, reason);
    }
}

$R netQ_UDPListenerD_sendG_local (netQ_UDPListener self, $Cont c$cont, B_bytes data, B_str address, int64_t port) {
    uv_udp_t *udp = (uv_udp_t *)(intptr_t)self->_sock;
    if ((intptr_t)udp == -1) {
        return $R_CONT(c$cont, B_None);
    }

    const char *addr = (const char *)fromB_str(address);
    char dest_name[NET_DESTSTRLEN];
    format_addr_port(dest_name, sizeof(dest_name), addr, port);

    struct sockaddr_storage dest;
    if (make_sockaddr(address, port, &dest) == 0) {
        udp_listener_send_to(self, udp, data, (const struct sockaddr *)&dest, dest_name);
    } else if (strchr(addr, ':') != NULL || strchr(addr, '%') != NULL) {
        // A malformed IPv6 address or zone is no host name
        udp_listener_send_error(self, dest_name, "invalid address");
    } else {
        udp_listener_resolve_and_send(self, udp, data, address, port, dest_name);
    }
    return $R_CONT(c$cont, B_None);
}

$R netQ_UDPListenerD_set_recv_buffer_sizeG_local (netQ_UDPListener self, $Cont c$cont, int64_t size) {
    uv_udp_t *udp = (uv_udp_t *)(intptr_t)self->_sock;
    if ((intptr_t)udp == -1) {
        return $R_CONT(c$cont, B_None);
    }
    int r = udp_set_recv_buffer_size(udp, size);
    if (r != 0) {
        char errmsg[1024] = "Failed to set UDP receive buffer size: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)self->on_error;
        f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
    }
    return $R_CONT(c$cont, B_None);
}

$R netQ_UDPListenerD_get_recv_buffer_sizeG_local (netQ_UDPListener self, $Cont c$cont) {
    uv_udp_t *udp = (uv_udp_t *)(intptr_t)self->_sock;
    int value = 0;
    if ((intptr_t)udp == -1 || uv_recv_buffer_size((uv_handle_t *)udp, &value) != 0) {
        value = 0;
    }
    return $R_CONT(c$cont, toB_int(value));
}

$R netQ_UDPListenerD_closeG_local (netQ_UDPListener self, $Cont c$cont) {
    udp_close_handle(&self->_sock);
    return $R_CONT(c$cont, B_None);
}

B_NoneType netQ_UDPListenerD___resume__ (netQ_UDPListener self) {
    self->_sock = -1LL;
    $action2 f = ($action2)self->on_listen;
    f->$class->__asyn__(f, self, actStrFromCString("resume"));
    return B_None;
}

$R netQ_UDPListenerD_local_addressG_local (netQ_UDPListener self, $Cont c$cont) {
    uv_udp_t *udp = (uv_udp_t *)(intptr_t)self->_sock;
    if ((intptr_t)udp == -1) {
        return $R_CONT(c$cont, actStrFromCString(""));
    }
    struct sockaddr_storage sockname;
    int namelen = sizeof(sockname);
    char addr[NET_ADDRSTRLEN] = { '\0' };
    if (uv_udp_getsockname((const uv_udp_t *)udp, (struct sockaddr *)&sockname, &namelen) == 0) {
        int port = 0;
        if (sockaddr_to_addr_port((struct sockaddr *)&sockname, addr, sizeof(addr), &port) != 0) {
            addr[0] = '\0';
        }
    }
    return $R_CONT(c$cont, actStrFromCStringCopy(addr));
}

$R netQ_UDPListenerD_local_portG_local (netQ_UDPListener self, $Cont c$cont) {
    uv_udp_t *udp = (uv_udp_t *)(intptr_t)self->_sock;
    if ((intptr_t)udp == -1) {
        return $R_CONT(c$cont, toB_int(0));
    }
    struct sockaddr_storage sockname;
    int namelen = sizeof(sockname);
    char addr[NET_ADDRSTRLEN] = { '\0' };
    int port = 0;
    if (uv_udp_getsockname((const uv_udp_t *)udp, (struct sockaddr *)&sockname, &namelen) == 0) {
        sockaddr_to_addr_port((struct sockaddr *)&sockname, addr, sizeof(addr), &port);
    }
    return $R_CONT(c$cont, toB_int(port));
}

// Closes a TCP handle. TCP handles are GC memory, so there is nothing to free
// once the close completes.
static void tcp_close_handle(uv_handle_t *handle) {
    if (uv_is_closing(handle) == 0) {
        uv_close(handle, NULL);
    }
}

// Closes a TCP handle with a reset (RST) rather than an orderly shutdown
static void tcp_close_reset(uv_tcp_t *handle) {
    if (uv_is_closing((uv_handle_t *)handle)) {
        return;
    }
    // Fails while a shutdown is in progress
    if (uv_tcp_close_reset(handle, NULL) != 0) {
        uv_close((uv_handle_t *)handle, NULL);
    }
}

// Local and remote address and port of a connected TCP handle
struct tcp_names {
    char local_address[NET_ADDRSTRLEN];
    int local_port;
    char remote_address[NET_ADDRSTRLEN];
    int remote_port;
    int family;
};

static void tcp_get_names(const uv_tcp_t *handle, struct tcp_names *names) {
    memset(names, 0, sizeof(*names));
    struct sockaddr_storage name;
    int namelen = sizeof(name);
    if (uv_tcp_getsockname(handle, (struct sockaddr *)&name, &namelen) == 0) {
        sockaddr_to_addr_port((struct sockaddr *)&name, names->local_address, sizeof(names->local_address), &names->local_port);
    }
    namelen = sizeof(name);
    if (uv_tcp_getpeername(handle, (struct sockaddr *)&name, &namelen) == 0) {
        sockaddr_to_addr_port((struct sockaddr *)&name, names->remote_address, sizeof(names->remote_address), &names->remote_port);
        names->family = name.ss_family == AF_INET6 ? 6 : 4;
    }
}

// Binds and listens on a TCP server handle, errmsg is set on failure
static int tcp_listen(uv_tcp_t *server, B_str address, int64_t port, uv_connection_cb cb, const char *proto, char *errmsg, size_t errmsg_len) {
    struct sockaddr_storage addr;
    int r = make_sockaddr(address, port, &addr);
    if (r != 0) {
        snprintf(errmsg, errmsg_len, "Address is not an IPv4 or IPv6 address: %s", (const char *)fromB_str(address));
        return r;
    }
    r = uv_tcp_bind(server, (const struct sockaddr *)&addr, 0);
    if (r != 0) {
        snprintf(errmsg, errmsg_len, "Error in %s bind: %s", proto, uv_strerror(r));
        return r;
    }
    r = uv_listen((uv_stream_t *)server, 1024, cb);
    if (r != 0) {
        snprintf(errmsg, errmsg_len, "Error in %s listen: %s", proto, uv_strerror(r));
        return r;
    }
    return 0;
}

static void tcp_listener_names(int64_t sock, char *addr, size_t addr_len, int *port) {
    uv_tcp_t *server = (uv_tcp_t *)(intptr_t)sock;
    addr[0] = '\0';
    *port = 0;
    if ((intptr_t)server == -1) {
        return;
    }
    struct sockaddr_storage name;
    int namelen = sizeof(name);
    if (uv_tcp_getsockname(server, (struct sockaddr *)&name, &namelen) == 0) {
        if (sockaddr_to_addr_port((struct sockaddr *)&name, addr, addr_len, port) != 0) {
            addr[0] = '\0';
        }
    }
}

void netQ_TCPConnection__on_receive(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buf) {
    netQ_TCPConnection self = stream->data;
    if (self == NULL) {
        return;
    }

    if (nread > 0) {
        $action2 f = ($action2)self->on_receive;
        f->$class->__asyn__(f, self, actBytesFromCStringLengthCopy(buf->base, nread));
        self->_bytes_in += nread;
    } else if (nread < 0) {
        // End of stream or a read error, like a reset: the connection is over
        stream->data = NULL;
        tcp_close_handle((uv_handle_t *)stream);
        if ((uv_stream_t *)(intptr_t)self->_sock == stream) {
            self->_sock = -1LL;
            self->_state = self->STATE_CLOSED;
        }
        if (nread == UV_EOF) {
            if (self->on_remote_close) {
                $action f = ($action)self->on_remote_close;
                f->$class->__asyn__(f, self);
            }
        } else {
            char errmsg[1024] = "TCP read error: ";
            uv_strerror_r((int)nread, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
            log_debug("%s", errmsg);
            $action2 f = ($action2)self->on_error;
            f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
        }
    }
}

$R netQ_TCPConnectionD__pin_affinityG_local (netQ_TCPConnection self, $Cont c$cont) {
    pin_actor_affinity();
    return $R_CONT(c$cont, B_None);
}

static void tcp_connection_on_connect(uv_connect_t *connect_req, int status, int64_t family) {
    netQ_TCPConnection self = (netQ_TCPConnection)connect_req->data;
    uv_stream_t *socket = connect_req->handle;

    // The attempt was abandoned and its socket closed, by close() or by
    // losing the Happy Eyeballs race
    if (status == UV_ECANCELED) {
        return;
    }
    if (status != 0) {
        char errmsg[1024];
        snprintf(errmsg, sizeof(errmsg), "Error in TCP connect over IPv%lld: ", (long long)family);
        uv_strerror_r(status, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        tcp_close_handle((uv_handle_t *)socket);
        self->$class->_on_tcp_error(self, family, (int64_t)(intptr_t)socket, (int64_t)status, actStrFromCStringCopy(errmsg));
        return;
    }
    if (family == 4) {
        self->$class->_on_connect4(self, (int64_t)(intptr_t)socket);
    } else {
        self->$class->_on_connect6(self, (int64_t)(intptr_t)socket);
    }
}

void on_connect4(uv_connect_t *connect_req, int status) {
    tcp_connection_on_connect(connect_req, status, 4);
}

void on_connect6(uv_connect_t *connect_req, int status) {
    tcp_connection_on_connect(connect_req, status, 6);
}

static void tcp_connection_connect(netQ_TCPConnection self, B_str ip_address, int64_t family) {
    log_debug("TCP connecting over IPv%lld to %s", (long long)family, fromB_str(ip_address));
    int64_t *sock = family == 4 ? &self->_sock4 : &self->_sock6;
    *sock = -1LL;

    uv_tcp_t *socket = NULL;
    struct sockaddr_storage dest;
    int r = make_sockaddr(ip_address, self->port, &dest);
    if (r == 0) {
        socket = (uv_tcp_t *)acton_malloc(sizeof(uv_tcp_t));
        r = uv_tcp_init(get_uv_loop(), socket);
        if (r != 0) {
            socket = NULL;
        }
    }
    if (r == 0) {
        *sock = (int64_t)(intptr_t)socket;
        uv_connect_t *connect_req = (uv_connect_t *)acton_malloc(sizeof(uv_connect_t));
        connect_req->data = (void *)self;
        r = uv_tcp_connect(connect_req, socket, (const struct sockaddr *)&dest, family == 4 ? on_connect4 : on_connect6);
    }
    if (r != 0) {
        char errmsg[1024];
        snprintf(errmsg, sizeof(errmsg), "Error in TCP connect over IPv%lld: ", (long long)family);
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        if (socket != NULL) {
            tcp_close_handle((uv_handle_t *)socket);
        }
        self->$class->_on_tcp_error(self, family, *sock, (int64_t)r, actStrFromCStringCopy(errmsg));
    }
}

$R netQ_TCPConnectionD__connect4G_local (netQ_TCPConnection self, $Cont c$cont, B_str ip_address) {
    tcp_connection_connect(self, ip_address, 4);
    return $R_CONT(c$cont, B_None);
}

$R netQ_TCPConnectionD__connect6G_local (netQ_TCPConnection self, $Cont c$cont, B_str ip_address) {
    tcp_connection_connect(self, ip_address, 6);
    return $R_CONT(c$cont, B_None);
}

static void tcp_close_attempt(int64_t *sock) {
    uv_handle_t *socket = (uv_handle_t *)(intptr_t)*sock;
    *sock = -1LL;
    if ((intptr_t)socket != -1) {
        // A pending connect completes with UV_ECANCELED
        tcp_close_handle(socket);
    }
}

$R netQ_TCPConnectionD__close_attemptsG_local (netQ_TCPConnection self, $Cont c$cont) {
    tcp_close_attempt(&self->_sock4);
    tcp_close_attempt(&self->_sock6);
    return $R_CONT(c$cont, B_None);
}

$R netQ_TCPConnectionD__read_startG_local (netQ_TCPConnection self, $Cont c$cont) {
    uv_tcp_t* socket = (uv_tcp_t *)(intptr_t)self->_sock;
    struct tcp_names names;
    tcp_get_names(socket, &names);
    self->_ip_version = names.family;
    self->_local_address = actStrFromCStringCopy(names.local_address);
    self->_local_port = names.local_port;
    self->_remote_address = actStrFromCStringCopy(names.remote_address);
    self->_remote_port = names.remote_port;

    socket->data = self;
    int r = uv_read_start((uv_stream_t *)socket, alloc_buffer, netQ_TCPConnection__on_receive);
    if (r < 0) {
        char errmsg[1024] = "Failed to start reading from TCP client socket: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)self->on_error;
        f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
        return $R_CONT(c$cont, B_None);
    }

    return $R_CONT(c$cont, B_None);
}

// A write failed: close the connection and report the error, unless the
// connection was already closed
static void tcp_connection_write_failed(netQ_TCPConnection self, uv_stream_t *stream, int status) {
    char errmsg[1024] = "Failed to write to TCP socket: ";
    uv_strerror_r(status, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
    log_debug("%s", errmsg);
    if ((uv_stream_t *)(intptr_t)self->_sock != stream) {
        return;
    }
    self->_sock = -1LL;
    self->_state = self->STATE_CLOSED;
    stream->data = NULL;
    tcp_close_handle((uv_handle_t *)stream);
    $action2 f = ($action2)self->on_error;
    f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
}

static void tcp_connection_write_cb(uv_write_t *req, int status) {
    // UV_ECANCELED: the connection was closed with the write still queued
    if (status < 0 && status != UV_ECANCELED) {
        tcp_connection_write_failed((netQ_TCPConnection)req->data, req->handle, status);
    }
}

$R netQ_TCPConnectionD_writeG_local (netQ_TCPConnection self, $Cont c$cont, B_bytes data) {
    uv_stream_t *stream = (uv_stream_t *)(intptr_t)self->_sock;
    // fd == -1 means invalid FD and can happen after __resume__
    if ((intptr_t)stream == -1)
        return $R_CONT(c$cont, B_None);

    uv_write_t *req = (uv_write_t *)acton_malloc(sizeof(uv_write_t));
    req->data = self;
    uv_buf_t buf = uv_buf_init((char *)data->str, data->nbytes);
    int r = uv_write(req, stream, &buf, 1, tcp_connection_write_cb);
    if (r < 0) {
        tcp_connection_write_failed(self, stream, r);
        return $R_CONT(c$cont, B_None);
    }
    self->_bytes_out += data->nbytes;
    return $R_CONT(c$cont, B_None);
}

B_NoneType netQ_TCPConnectionD___resume__ (netQ_TCPConnection self) {
    self->_sock = -1LL;
    self->_sock4 = -1LL;
    self->_sock6 = -1LL;
    self->_state = self->STATE_CLOSED;
    $action2 f = ($action2)self->on_error;
    f->$class->__asyn__(f, self, actStrFromCString("resume"));
    return B_None;
}

struct close_cb_data {
    netQ_TCPConnection self;
    $action on_close;
};

static void after_shutdown(uv_shutdown_t* req, int status) {
    log_debug("TCP Shutdown complete, closing handle");
    if (status < 0 && status != UV_ECANCELED)
        log_warn("Error in TCP shutdown: %s", uv_strerror(status));
    tcp_close_handle((uv_handle_t*)req->handle);
    struct close_cb_data *cb_data = (struct close_cb_data *)req->data;
    $action on_close = ($action)cb_data->on_close;
    on_close->$class->__asyn__(on_close, cb_data->self);
}

$R netQ_TCPConnectionD__closeG_local (netQ_TCPConnection self, $Cont c$cont, $action on_close) {
    uv_stream_t *stream = (uv_stream_t *)(intptr_t)self->_sock;
    self->_sock = -1LL;
    // fd == -1 means invalid FD and can happen after __resume__
    if ((intptr_t)stream == -1) {
        on_close->$class->__asyn__(on_close, self);
        return $R_CONT(c$cont, B_None);
    }

    log_debug("Closing TCP connection");
    // No more on_receive or on_error once the connection is being closed
    uv_read_stop(stream);
    stream->data = NULL;
    struct close_cb_data *cb_data = (struct close_cb_data *)acton_malloc(sizeof(struct close_cb_data));
    cb_data->self = self;
    cb_data->on_close = on_close;
    uv_shutdown_t *req = (uv_shutdown_t *)acton_malloc(sizeof(uv_shutdown_t));
    req->data = (void *)cb_data;
    int r = uv_shutdown(req, stream, after_shutdown);
    if (r < 0) {
        // Nothing to flush on a broken connection, for example one that
        // was reset
        log_debug("TCP shutdown failed, closing handle: %s", uv_strerror(r));
        tcp_close_handle((uv_handle_t *)stream);
        on_close->$class->__asyn__(on_close, self);
    }
    return $R_CONT(c$cont, B_None);
}

$R netQ_TCPConnectionD__abortG_local (netQ_TCPConnection self, $Cont c$cont) {
    uv_tcp_t *socket = (uv_tcp_t *)(intptr_t)self->_sock;
    self->_sock = -1LL;
    if ((intptr_t)socket != -1) {
        log_debug("Resetting TCP connection");
        socket->data = NULL;
        tcp_close_reset(socket);
    }
    return $R_CONT(c$cont, B_None);
}

void on_new_connection(uv_stream_t *server, int status) {
    netQ_TCPListener self = (netQ_TCPListener)server->data;
    if (self == NULL) {
        return;
    }

    if (status != 0) {
        char errmsg[1024] = "Error on new TCP client connection: ";
        uv_strerror_r(status, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
        // NOTE: free() here if do manual memory management in I/O one day
        return;
    }

    uv_tcp_t *client = (uv_tcp_t*)acton_malloc(sizeof(uv_tcp_t));
    uv_tcp_init(get_uv_loop(), client);
    int r = uv_accept(server, (uv_stream_t *)client);
    if (r != 0) {
        char errmsg[1024] = "Error in accepting TCP client connection: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        tcp_close_handle((uv_handle_t *)client);
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
        // NOTE: free() here if do manual memory management in I/O one day
        return;
    }

    // The handle belongs to this loop, so the connection actor runs on this
    // worker thread
    self->$class->create_tcp_listen_connection(self, B_None, (int64_t)(intptr_t)client, get_wtid());
    // NOTE: free() here if do manual memory management in I/O one day
}

$R netQ_TCPListenerD__initG_local (netQ_TCPListener self, $Cont c$cont) {
    pin_actor_affinity();

    uv_tcp_t *server = (uv_tcp_t *)acton_malloc(sizeof(uv_tcp_t));
    uv_tcp_init(get_uv_loop(), server);
    server->data = (void *)self;
    char errmsg[1024];
    int r = tcp_listen(server, self->address, self->port, on_new_connection, "TCP", errmsg, sizeof(errmsg));
    if (r != 0) {
        log_warn(errmsg);
        server->data = NULL;
        tcp_close_handle((uv_handle_t *)server);
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
        return $R_CONT(c$cont, B_None);
    }

    self->_stream = (int64_t)(intptr_t)server;
    $action2 f = ($action2)self->on_listen;
    f->$class->__asyn__(f, self, B_None);
    return $R_CONT(c$cont, B_None);
}

static void tcp_listener_close(netQ_TCPListener self) {
    uv_tcp_t *server = (uv_tcp_t *)(intptr_t)self->_stream;
    self->_stream = -1LL;
    if ((intptr_t)server != -1) {
        server->data = NULL;
        tcp_close_handle((uv_handle_t *)server);
    }
}

$R netQ_TCPListenerD_closeG_local (netQ_TCPListener self, $Cont c$cont) {
    tcp_listener_close(self);
    return $R_CONT(c$cont, B_None);
}

$R netQ_TCPListenerD_local_addressG_local (netQ_TCPListener self, $Cont c$cont) {
    char addr[NET_ADDRSTRLEN];
    int port;
    tcp_listener_names(self->_stream, addr, sizeof(addr), &port);
    return $R_CONT(c$cont, actStrFromCStringCopy(addr));
}

$R netQ_TCPListenerD_local_portG_local (netQ_TCPListener self, $Cont c$cont) {
    char addr[NET_ADDRSTRLEN];
    int port;
    tcp_listener_names(self->_stream, addr, sizeof(addr), &port);
    return $R_CONT(c$cont, toB_int(port));
}

B_NoneType netQ_TCPListenerD___resume__ (netQ_TCPListener self) {
    self->_stream = -1LL;
    $action2 f = ($action2)self->on_listen;
    f->$class->__asyn__(f, self, actStrFromCString("resume"));
    return B_None;
}

$R netQ_TCPListenConnectionD__initG_local (netQ_TCPListenConnection self, $Cont c$cont) {
    // The accepted handle belongs to the loop of the listener's worker thread.
    // This may run on another worker, so it must not use the loop; only the
    // actor's later messages run on the listener's worker.
    set_actor_affinity(self->worker_id);

    uv_tcp_t *client_handle = (uv_tcp_t *)(intptr_t)self->server_client;
    self->client = (int64_t)(intptr_t)client_handle;

    struct tcp_names names;
    tcp_get_names(client_handle, &names);
    self->_local_address = actStrFromCStringCopy(names.local_address);
    self->_local_port = names.local_port;
    self->_remote_address = actStrFromCStringCopy(names.remote_address);
    self->_remote_port = names.remote_port;

    return $R_CONT(c$cont, B_None);
}

void netQ_TCPListenConnection__on_receive(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buf) {
    netQ_TCPListenConnection self = stream->data;
    if (self == NULL) {
        return;
    }

    if (nread > 0) {
        $action2 f = ($action2)self->on_receive;
        f->$class->__asyn__(f, self, actBytesFromCStringLengthCopy(buf->base, nread));
    } else if (nread < 0) {
        // End of stream or a read error, like a reset: the connection is over
        stream->data = NULL;
        tcp_close_handle((uv_handle_t *)stream);
        if ((uv_stream_t *)(intptr_t)self->client == stream) {
            self->client = -1LL;
        }
        if (nread == UV_EOF) {
            if (self->on_remote_close) {
                $action f = ($action)self->on_remote_close;
                f->$class->__asyn__(f, self);
            }
        } else {
            char errmsg[1024] = "TCP read error: ";
            uv_strerror_r((int)nread, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
            log_debug("%s", errmsg);
            if (self->on_error) {
                $action2 f = ($action2)self->on_error;
                f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
            }
        }
    }
}

$R netQ_TCPListenConnectionD__read_startG_local (netQ_TCPListenConnection self, $Cont c$cont) {
    uv_stream_t *client = (uv_stream_t *)(intptr_t)self->client;
    if ((intptr_t)client == -1)
        return $R_CONT(c$cont, B_None);
    client->data = self;
    int r = uv_read_start(client, alloc_buffer, netQ_TCPListenConnection__on_receive);
    if (r < 0) {
        char errmsg[1024] = "Failed to start reading from TCP socket: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)self->on_error;
        f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
        return $R_CONT(c$cont, B_None);
    }

    return $R_CONT(c$cont, B_None);
}

// A write failed: close the connection and report the error, unless the
// connection was already closed
static void tcp_listen_conn_write_failed(netQ_TCPListenConnection self, uv_stream_t *stream, int status) {
    char errmsg[1024] = "Failed to write to TCP socket: ";
    uv_strerror_r(status, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
    log_debug("%s", errmsg);
    if ((uv_stream_t *)(intptr_t)self->client != stream) {
        return;
    }
    self->client = -1LL;
    stream->data = NULL;
    tcp_close_handle((uv_handle_t *)stream);
    if (self->on_error) {
        $action2 f = ($action2)self->on_error;
        f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
    }
}

static void tcp_listen_conn_write_cb(uv_write_t *req, int status) {
    // UV_ECANCELED: the connection was closed with the write still queued
    if (status < 0 && status != UV_ECANCELED) {
        tcp_listen_conn_write_failed((netQ_TCPListenConnection)req->data, req->handle, status);
    }
}

$R netQ_TCPListenConnectionD_writeG_local (netQ_TCPListenConnection self, $Cont c$cont, B_bytes data) {
    uv_stream_t *stream = (uv_stream_t *)(intptr_t)self->client;
    // fd == -1 means invalid FD and can happen after __resume__
    if ((intptr_t)stream == -1)
        return $R_CONT(c$cont, B_None);

    uv_write_t *req = (uv_write_t *)acton_malloc(sizeof(uv_write_t));
    req->data = self;
    uv_buf_t buf = uv_buf_init((char *)data->str, data->nbytes);
    int r = uv_write(req, stream, &buf, 1, tcp_listen_conn_write_cb);
    if (r < 0) {
        tcp_listen_conn_write_failed(self, stream, r);
    }
    return $R_CONT(c$cont, B_None);
}

static void listen_conn_after_shutdown(uv_shutdown_t *req, int status) {
    log_debug("TCP listen connection shutdown complete, closing handle");
    if (status < 0 && status != UV_ECANCELED)
        log_warn("Error in TCP listen connection shutdown: %s", uv_strerror(status));
    tcp_close_handle((uv_handle_t *)req->handle);
}

$R netQ_TCPListenConnectionD_closeG_local (netQ_TCPListenConnection self, $Cont c$cont) {
    log_debug("Closing TCP connection, affinity=%d", self->$affinity);
    uv_stream_t *client = (uv_stream_t *)(intptr_t)self->client;
    // fd == -1 means invalid FD and can happen after __resume__ or if the socket was closed
    if ((intptr_t)client == -1)
        return $R_CONT(c$cont, B_None);

    self->client = -1LL;
    uv_read_stop(client);
    if (uv_is_closing((uv_handle_t *)client))
        return $R_CONT(c$cont, B_None);

    uv_shutdown_t *req = (uv_shutdown_t *)acton_malloc(sizeof(uv_shutdown_t));
    int r = uv_shutdown(req, client, listen_conn_after_shutdown);
    if (r < 0) {
        log_debug("TCP listen connection shutdown failed, closing handle: %s", uv_strerror(r));
        uv_close((uv_handle_t *)client, NULL);
    }
    return $R_CONT(c$cont, B_None);
}

$R netQ_TCPListenConnectionD_abortG_local (netQ_TCPListenConnection self, $Cont c$cont) {
    uv_tcp_t *client = (uv_tcp_t *)(intptr_t)self->client;
    self->client = -1LL;
    if ((intptr_t)client != -1) {
        log_debug("Resetting TCP listen connection");
        client->data = NULL;
        tcp_close_reset(client);
    }
    return $R_CONT(c$cont, B_None);
}

B_NoneType netQ_TCPListenConnectionD___resume__ (netQ_TCPListenConnection self) {
    self->server_client = -1LL;
    self->client = -1LL;
    return B_None;
}

struct tls_listener_server_state;

struct tls_listener_conn {
    struct tls_listener_server_state *listener_state;
    tlsuv_stream_t *stream;
    struct tls_listener_stream_state *state;
};

struct tls_listener_server_state {
    _Atomic int refs;
    netQ_TLSListener actor;
};

// A TLS context with its own certificate and key, which every engine created
// from the context refers to. Shared by a listener and the connections it
// accepted, or owned by a client connection that brings its own CA or
// certificate.
struct tls_owner {
    _Atomic int refs;
    tls_context *tls;
    tlsuv_private_key_t key;
    tlsuv_certificate_t cert;
    // Listener settings for accepted streams
    int authmode;
};

struct tls_listener_stream_state {
    struct tls_owner *owner;
    netQ_TLSListenConnection actor;
};

struct tls_client_state {
    netQ_TLSConnection actor;
    bool connected;
    struct tls_owner *owner; // NULL for the default TLS context
    int alpn_count;
    char **alpn_protocols;
};

struct tls_write_req_state {
    char *buf;
};

static void tls_release_resources(tls_context *tls,
                                  tlsuv_private_key_t key,
                                  tlsuv_certificate_t cert);
static void tls_owner_release(struct tls_owner *owner);
static struct tls_listener_stream_state *tls_listener_stream_state_new(struct tls_owner *owner);
static void tls_listener_release_connection_owner(netQ_TLSListenConnection self);
static struct tls_listener_server_state *tls_listener_server_state_new(netQ_TLSListener actor);
static void tls_listener_server_state_retain(struct tls_listener_server_state *state);
static void tls_listener_server_state_release(struct tls_listener_server_state *state);
static struct tls_client_state *tls_client_state_new(netQ_TLSConnection actor);
static void tls_client_state_free(struct tls_client_state *state);
static struct tls_write_req_state *tls_write_req_state_new(B_bytes data);
static void tls_write_req_state_free(struct tls_write_req_state *state);

static void close_uv_socket(uv_os_sock_t sock) {
#ifdef _WIN32
    closesocket(sock);
#else
    close((int)sock);
#endif
}

static void uv_handle_free_on_close(uv_handle_t *handle) {
    free(handle);
}

static void tls_listener_server_on_close(uv_handle_t *server) {
    log_debug("TLS listener server handle closed: %p", server);
    struct tls_listener_server_state *state = (struct tls_listener_server_state *)server->data;
    server->data = NULL;
    tls_listener_server_state_release(state);
    free(server);
}

static void tls_listener_on_close(uv_handle_t *stream) {
    log_debug("TLS listener handle closed, stream: %p  stream->data: %p", stream, stream->data);
    struct tls_listener_stream_state *state = (struct tls_listener_stream_state *)stream->data;
    if (state != NULL) {
        netQ_TLSListenConnection self = state->actor;
        if (self != NULL) {
            self->server_stream = -1LL;
            self->_stream = -1LL;
            state->actor = NULL;
        }
        tls_owner_release(state->owner);
        free(state);
    }
    stream->data = NULL;
    free(stream);
}

static void tls_release_resources(tls_context *tls,
                                  tlsuv_private_key_t key,
                                  tlsuv_certificate_t cert) {
    if (tls != NULL && (intptr_t)tls != -1 && tls->set_own_cert != NULL) {
        tls->set_own_cert(tls, NULL, NULL);
    }
    if (cert != NULL && (intptr_t)cert != -1 && cert->free != NULL) {
        cert->free(cert);
    }
    if (key != NULL && (intptr_t)key != -1 && key->free != NULL) {
        key->free(key);
    }
    if (tls != NULL && (intptr_t)tls != -1 && tls->free_ctx != NULL) {
        tls->free_ctx(tls);
    }
}

static struct tls_owner *tls_owner_new(tls_context *tls,
                                       tlsuv_private_key_t key,
                                       tlsuv_certificate_t cert) {
    struct tls_owner *owner = GC_malloc_uncollectable(sizeof(*owner));
    if (owner == NULL) {
        return NULL;
    }
    atomic_init(&owner->refs, 1);
    owner->tls = tls;
    owner->key = key;
    owner->cert = cert;
    owner->authmode = 0;
    return owner;
}

// Creates a TLS context that trusts the CA certificates in ca_pem, or the
// system CA bundle without it, and presents cert_pem and key_pem if given
static struct tls_owner *tls_owner_from_pem(B_bytes ca_pem, B_bytes cert_pem, B_bytes key_pem, char *errmsg, size_t errmsg_len) {
    tls_context *tls = ca_pem != NULL
        ? default_tls_context((const char *)ca_pem->str, ca_pem->nbytes)
        : default_tls_context(NULL, 0);
    tlsuv_private_key_t key = NULL;
    tlsuv_certificate_t cert = NULL;
    if (tls == NULL) {
        snprintf(errmsg, errmsg_len, "Failed to initialize TLS context");
        return NULL;
    }

    if (ca_pem != NULL) {
        // The CA bundle is parsed again for every connection, check it once
        // here rather than fail every handshake
        tlsuv_certificate_t ca = NULL;
        int rc = tls->load_cert(&ca, (const char *)ca_pem->str, ca_pem->nbytes);
        if (ca != NULL) {
            ca->free(ca);
        }
        if (rc < 0) {
            snprintf(errmsg, errmsg_len, "Failed to load TLS CA certificates: %s", tls->strerror(rc));
            tls_release_resources(tls, key, cert);
            return NULL;
        }
    }

    if (cert_pem != NULL || key_pem != NULL) {
        if (cert_pem == NULL || key_pem == NULL) {
            snprintf(errmsg, errmsg_len, "A TLS certificate needs both cert_pem and key_pem");
            tls_release_resources(tls, key, cert);
            return NULL;
        }
        int rc = tls->load_key(&key, (const char *)key_pem->str, key_pem->nbytes);
        if (rc != 0) {
            snprintf(errmsg, errmsg_len, "Failed to load TLS key: %s", tls->strerror(rc));
            tls_release_resources(tls, key, cert);
            return NULL;
        }
        rc = tls->load_cert(&cert, (const char *)cert_pem->str, cert_pem->nbytes);
        if (rc != 0) {
            snprintf(errmsg, errmsg_len, "Failed to load TLS cert: %s", tls->strerror(rc));
            tls_release_resources(tls, key, cert);
            return NULL;
        }
        rc = tls->set_own_cert(tls, key, cert);
        if (rc != 0) {
            snprintf(errmsg, errmsg_len, "Failed to set TLS certificate, does it match the key?");
            tls_release_resources(tls, key, cert);
            return NULL;
        }
    }

    struct tls_owner *owner = tls_owner_new(tls, key, cert);
    if (owner == NULL) {
        snprintf(errmsg, errmsg_len, "Failed to allocate TLS context owner");
        tls_release_resources(tls, key, cert);
    }
    return owner;
}

static void tls_owner_retain(struct tls_owner *owner) {
    if (owner != NULL && (intptr_t)owner != -1) {
        atomic_fetch_add(&owner->refs, 1);
    }
}

static void tls_owner_release(struct tls_owner *owner) {
    if (owner == NULL || (intptr_t)owner == -1) {
        return;
    }
    int prev = atomic_fetch_sub(&owner->refs, 1);
    if (prev == 1) {
        tls_release_resources(owner->tls, owner->key, owner->cert);
        GC_free(owner);
    }
}

static struct tls_listener_stream_state *tls_listener_stream_state_new(struct tls_owner *owner) {
    struct tls_listener_stream_state *state = malloc(sizeof(*state));
    if (state == NULL) {
        return NULL;
    }
    state->owner = owner;
    state->actor = NULL;
    return state;
}

static struct tls_listener_server_state *tls_listener_server_state_new(netQ_TLSListener actor) {
    struct tls_listener_server_state *state = malloc(sizeof(*state));
    if (state == NULL) {
        return NULL;
    }
    atomic_init(&state->refs, 1);
    state->actor = actor;
    return state;
}

static void tls_listener_server_state_retain(struct tls_listener_server_state *state) {
    if (state != NULL) {
        atomic_fetch_add(&state->refs, 1);
    }
}

static void tls_listener_server_state_release(struct tls_listener_server_state *state) {
    if (state == NULL) {
        return;
    }
    int prev = atomic_fetch_sub(&state->refs, 1);
    if (prev == 1) {
        free(state);
    }
}

static struct tls_client_state *tls_client_state_new(netQ_TLSConnection actor) {
    struct tls_client_state *state = malloc(sizeof(*state));
    if (state == NULL) {
        return NULL;
    }
    state->actor = actor;
    state->connected = false;
    state->owner = NULL;
    state->alpn_count = 0;
    state->alpn_protocols = NULL;
    return state;
}

// Frees the client state once its stream, and so the TLS engine that refers
// to the owner's context, is gone
static void tls_client_state_free(struct tls_client_state *state) {
    if (state == NULL) {
        return;
    }

    if (state->alpn_protocols != NULL) {
        for (int i = 0; i < state->alpn_count; i++) {
            free(state->alpn_protocols[i]);
        }
        free(state->alpn_protocols);
    }
    tls_owner_release(state->owner);
    free(state);
}

static struct tls_write_req_state *tls_write_req_state_new(B_bytes data) {
    struct tls_write_req_state *state = malloc(sizeof(*state));
    if (state == NULL) {
        return NULL;
    }

    size_t len = data->nbytes;
    state->buf = malloc(len > 0 ? len : 1);
    if (state->buf == NULL) {
        free(state);
        return NULL;
    }
    if (len > 0) {
        memcpy(state->buf, data->str, len);
    }
    return state;
}

static void tls_write_req_state_free(struct tls_write_req_state *state) {
    if (state != NULL) {
        free(state->buf);
        free(state);
    }
}

static void tls_listener_release_actor_resources(netQ_TLSListener self) {
    struct tls_owner *owner = (struct tls_owner *)(intptr_t)self->_tls_owner;
    self->_tls_owner = -1LL;
    tls_owner_release(owner);
}

static void tls_listener_release_connection_owner(netQ_TLSListenConnection self) {
    struct tls_owner *owner = (struct tls_owner *)(intptr_t)self->tls_owner;
    self->tls_owner = -1LL;
    tls_owner_release(owner);
}

static bool tls_listener_stream_is_closing(tlsuv_stream_t *stream) {
    if (stream == NULL || (intptr_t)stream == -1) {
        return false;
    }
    uv_handle_t *watcher = (uv_handle_t *)&stream->watcher;
    return uv_handle_get_type(watcher) != UV_UNKNOWN_HANDLE && uv_is_closing(watcher);
}

static void tls_listener_close_server(netQ_TLSListener self) {
    uv_tcp_t *server = (uv_tcp_t *)(intptr_t)self->_stream;
    self->_stream = -1LL;

    if ((intptr_t)server != -1) {
        struct tls_listener_server_state *state = (struct tls_listener_server_state *)server->data;
        if (state != NULL) {
            state->actor = NULL;
        }
        if (uv_is_closing((uv_handle_t *)server) == 0) {
            uv_close((uv_handle_t *)server, tls_listener_server_on_close);
        }
    }

    tls_listener_release_actor_resources(self);
}

static void tls_listener_close_stream(tlsuv_stream_t *stream) {
    if (stream == NULL) {
        return;
    }
    if (tls_listener_stream_is_closing(stream)) {
        return;
    }
    tlsuv_stream_read_stop(stream);
    tlsuv_stream_close(stream, tls_listener_on_close);
}

// Local and remote names of the TCP connection under a TLS stream
static void tls_stream_names(tlsuv_stream_t *stream, struct tcp_names *names) {
    memset(names, 0, sizeof(*names));
    uv_os_fd_t fd;
    if (uv_fileno((const uv_handle_t *)&stream->watcher, &fd) != 0) {
        return;
    }
    struct sockaddr_storage name;
    socklen_t namelen = sizeof(name);
    if (getsockname((uv_os_sock_t)fd, (struct sockaddr *)&name, &namelen) == 0) {
        sockaddr_to_addr_port((struct sockaddr *)&name, names->local_address, sizeof(names->local_address), &names->local_port);
    }
    namelen = sizeof(name);
    if (getpeername((uv_os_sock_t)fd, (struct sockaddr *)&name, &namelen) == 0) {
        sockaddr_to_addr_port((struct sockaddr *)&name, names->remote_address, sizeof(names->remote_address), &names->remote_port);
        names->family = name.ss_family == AF_INET6 ? 6 : 4;
    }
}

// The peer certificate chain (DER, leaf first) of a completed handshake
static B_list tls_peer_certificates(tlsuv_stream_t *stream) {
    B_list certs = B_listG_new(NULL, NULL);
    tlsuv_engine_t engine = stream->tls_engine;
    if (engine == NULL || engine->get_peer_cert == NULL) {
        return certs;
    }
    const unsigned char *der;
    size_t der_len;
    for (int i = 0; engine->get_peer_cert(engine, i, &der, &der_len) == 0; i++) {
        B_SequenceD_listG_witness->$class->append(B_SequenceD_listG_witness, certs, actBytesFromCStringLengthCopy((const char *)der, (int64_t)der_len));
    }
    return certs;
}

static int64_t tls_verify_result(tlsuv_stream_t *stream) {
    tlsuv_engine_t engine = stream->tls_engine;
    if (engine == NULL || engine->verify_result == NULL) {
        return -1;
    }
    return engine->verify_result(engine);
}

static B_str tls_alpn(tlsuv_stream_t *stream) {
    const char *alpn = tlsuv_stream_get_protocol(stream);
    return alpn != NULL ? actStrFromCStringCopy(alpn) : NULL;
}

static B_str tls_version(tlsuv_stream_t *stream) {
    tlsuv_engine_t engine = stream->tls_engine;
    const char *version = NULL;
    if (engine != NULL && engine->get_tls_version != NULL) {
        version = engine->get_tls_version(engine);
    }
    return actStrFromCStringCopy(version != NULL ? version : "");
}

static void tls_listener_on_connect(uv_connect_t *creq, int status) {
    struct tls_listener_conn *conn = (struct tls_listener_conn *)creq->data;
    if (conn == NULL) {
        return;
    }

    struct tls_listener_server_state *listener_state = conn->listener_state;
    netQ_TLSListener listener = listener_state != NULL ? listener_state->actor : NULL;
    tlsuv_stream_t *stream = conn->stream;
    struct tls_listener_stream_state *state = conn->state;
    struct tls_owner *owner = state != NULL ? state->owner : NULL;

    if (listener == NULL) {
        tls_listener_close_stream(stream);
        tls_listener_server_state_release(listener_state);
        free(conn);
        free(creq);
        return;
    }

    if (status != 0) {
        char errmsg[1024] = "TLS handshake failed: ";
        uv_strerror_r(status, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        if (listener->on_listen != NULL) {
            $action2 f = ($action2)listener->on_listen;
            f->$class->__asyn__(f, listener, actStrFromCStringCopy(errmsg));
        }
        tls_listener_close_stream(stream);
        tls_listener_server_state_release(listener_state);
        free(conn);
        free(creq);
        return;
    }

    if (listener->on_accept == NULL) {
        tls_listener_close_stream(stream);
        tls_listener_server_state_release(listener_state);
        free(conn);
        free(creq);
        return;
    }

    tls_owner_retain(owner);
    listener->$class->create_tls_listen_connection(listener,
                                                   B_None,
                                                   (int64_t)(intptr_t)stream,
                                                   (int64_t)(intptr_t)owner,
                                                   get_wtid());
    tls_listener_server_state_release(listener_state);
    free(conn);
    free(creq);
}

static void tls_listener_on_receive(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buf) {
    struct tls_listener_stream_state *state = (struct tls_listener_stream_state *)((tlsuv_stream_t *)stream)->data;
    netQ_TLSListenConnection self = state != NULL ? state->actor : NULL;
    if (self == NULL) {
        return;
    }

    if (nread > 0) {
        if (stream->data && self->on_receive != NULL) {
            $action2 f = ($action2)self->on_receive;
            B_bytes data = actBytesFromCStringLengthCopy(buf->base, nread);
            f->$class->__asyn__(f, self, data);
        }
    } else if (nread == UV_EOF) {
        log_debug("TLS listen connection closed %p", stream);
        self->_stream = -1LL;
        tls_listener_close_stream((tlsuv_stream_t *)stream);
        if (self->on_remote_close) {
            $action f = ($action)self->on_remote_close;
            f->$class->__asyn__(f, self);
        }
    } else if (nread < 0) {
        char errmsg[1024] = "TLS listen read error: ";
        uv_strerror_r((int)nread, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_debug("%s", errmsg);
        self->_stream = -1LL;
        tls_listener_close_stream((tlsuv_stream_t *)stream);
        if (self->on_error != NULL) {
            $action2 on_error = ($action2)self->on_error;
            on_error->$class->__asyn__(on_error, self, actStrFromCStringCopy(errmsg));
        }
    }
}

static void tls_listener_write_cb(uv_write_t *wreq, int status) {
    struct tls_write_req_state *write_state = (struct tls_write_req_state *)wreq->data;
    // ECANCELED means the stream is already being torn down (tlsuv flushes
    // queued writes from its close path); reporting it or re-closing would
    // surface an error for a deliberate close.
    if (status < 0 && status != UV_ECANCELED) {
        char errmsg[1024] = "Failed to write to TLS listen socket: ";
        uv_strerror_r(status, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        struct tls_listener_stream_state *state = NULL;
        if (wreq->handle != NULL) {
            state = (struct tls_listener_stream_state *)((tlsuv_stream_t *)wreq->handle)->data;
        }
        netQ_TLSListenConnection self = state != NULL ? state->actor : NULL;
        if (self != NULL && self->on_error != NULL) {
            $action2 on_error = ($action2)self->on_error;
            on_error->$class->__asyn__(on_error, self, actStrFromCStringCopy(errmsg));
        }
        tls_listener_close_stream((tlsuv_stream_t *)wreq->handle);
    }
    tls_write_req_state_free(write_state);
    free(wreq);
}

static void on_new_tls_connection(uv_stream_t *server, int status) {
    struct tls_listener_server_state *listener_state = (struct tls_listener_server_state *)server->data;
    netQ_TLSListener self = listener_state != NULL ? listener_state->actor : NULL;
    if (self == NULL) {
        return;
    }

    if (status != 0) {
        char errmsg[1024] = "Error on new TLS client connection: ";
        uv_strerror_r(status, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        if (self->on_listen != NULL) {
            $action2 f = ($action2)self->on_listen;
            f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
        }
        // No release here: this callback borrows the server handle's own
        // reference (server->data); only the retain below, taken for an
        // in-flight handshake, is ever paired with a release.
        return;
    }

    tls_listener_server_state_retain(listener_state);

    uv_tcp_t *client = (uv_tcp_t *)malloc(sizeof(uv_tcp_t));
    if (client == NULL) {
        if (self->on_listen != NULL) {
            $action2 f = ($action2)self->on_listen;
            f->$class->__asyn__(f, self, actStrFromCString("Failed to allocate TLS client handle"));
        }
        tls_listener_server_state_release(listener_state);
        return;
    }

    int r = uv_tcp_init(get_uv_loop(), client);
    if (r != 0) {
        char errmsg[1024] = "Failed to initialize TLS client handle: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn("%s", errmsg);
        if (self->on_listen != NULL) {
            $action2 f = ($action2)self->on_listen;
            f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
        }
        free(client);
        tls_listener_server_state_release(listener_state);
        return;
    }

    r = uv_accept(server, (uv_stream_t *)client);
    if (r != 0) {
        char errmsg[1024] = "Error in accepting TLS client connection: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        if (self->on_listen != NULL) {
            $action2 f = ($action2)self->on_listen;
            f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
        }
        uv_close((uv_handle_t *)client, uv_handle_free_on_close);
        tls_listener_server_state_release(listener_state);
        return;
    }

    uv_os_fd_t fd;
    r = uv_fileno((uv_handle_t *)client, &fd);
    if (r != 0) {
        char errmsg[1024] = "Error getting TLS client socket fd: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        if (self->on_listen != NULL) {
            $action2 f = ($action2)self->on_listen;
            f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
        }
        uv_close((uv_handle_t *)client, uv_handle_free_on_close);
        tls_listener_server_state_release(listener_state);
        return;
    }

#ifdef _WIN32
    // NOTE: Windows socket duplication path is untested.
    SOCKET orig_sock = (SOCKET)fd;
    WSAPROTOCOL_INFOA dup_info;
    if (WSADuplicateSocket(orig_sock, GetCurrentProcessId(), &dup_info) != 0) {
        char errmsg[1024] = "Error duplicating TLS client socket: ";
        snprintf(errmsg + strlen(errmsg), sizeof(errmsg) - strlen(errmsg), "WSA error %d", WSAGetLastError());
        log_warn(errmsg);
        if (self->on_listen != NULL) {
            $action2 f = ($action2)self->on_listen;
            f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
        }
        uv_close((uv_handle_t *)client, uv_handle_free_on_close);
        tls_listener_server_state_release(listener_state);
        return;
    }
    SOCKET dup_sock = WSASocket(FROM_PROTOCOL_INFO, FROM_PROTOCOL_INFO, FROM_PROTOCOL_INFO,
                                &dup_info, 0, WSA_FLAG_OVERLAPPED);
    uv_close((uv_handle_t *)client, uv_handle_free_on_close);
    if (dup_sock == INVALID_SOCKET) {
        char errmsg[1024] = "Error duplicating TLS client socket: ";
        snprintf(errmsg + strlen(errmsg), sizeof(errmsg) - strlen(errmsg), "WSA error %d", WSAGetLastError());
        log_warn(errmsg);
        if (self->on_listen != NULL) {
            $action2 f = ($action2)self->on_listen;
            f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
        }
        tls_listener_server_state_release(listener_state);
        return;
    }
    uv_os_sock_t sock = (uv_os_sock_t)dup_sock;
#else
    int dup_fd = dup((int)fd);
    uv_close((uv_handle_t *)client, uv_handle_free_on_close);
    if (dup_fd < 0) {
        char errmsg[1024] = "Error duplicating TLS client socket: ";
        snprintf(errmsg + strlen(errmsg), sizeof(errmsg) - strlen(errmsg), "%s", strerror(errno));
        log_warn(errmsg);
        if (self->on_listen != NULL) {
            $action2 f = ($action2)self->on_listen;
            f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
        }
        tls_listener_server_state_release(listener_state);
        return;
    }
    uv_os_sock_t sock = (uv_os_sock_t)dup_fd;
#endif

    struct tls_owner *owner = (struct tls_owner *)(intptr_t)self->_tls_owner;
    if (owner == NULL || (intptr_t)owner == -1 || owner->tls == NULL) {
        if (self->on_listen != NULL) {
            $action2 f = ($action2)self->on_listen;
            f->$class->__asyn__(f, self, actStrFromCString("TLS listener resources unavailable"));
        }
        close_uv_socket(sock);
        tls_listener_server_state_release(listener_state);
        return;
    }
    tls_owner_retain(owner);
    tls_context *tls = owner->tls;
    tlsuv_stream_t *stream = (tlsuv_stream_t *)malloc(sizeof(tlsuv_stream_t));
    if (stream == NULL) {
        if (self->on_listen != NULL) {
            $action2 f = ($action2)self->on_listen;
            f->$class->__asyn__(f, self, actStrFromCString("Failed to allocate TLS listener stream"));
        }
        tls_owner_release(owner);
        close_uv_socket(sock);
        tls_listener_server_state_release(listener_state);
        return;
    }
    struct tls_listener_stream_state *state = tls_listener_stream_state_new(owner);
    if (state == NULL) {
        if (self->on_listen != NULL) {
            $action2 f = ($action2)self->on_listen;
            f->$class->__asyn__(f, self, actStrFromCString("Failed to allocate TLS listener stream state"));
        }
        tls_owner_release(owner);
        free(stream);
        close_uv_socket(sock);
        tls_listener_server_state_release(listener_state);
        return;
    }
    r = tlsuv_stream_init(get_uv_loop(), stream, tls);
    if (r != 0) {
        char errmsg[1024] = "Failed to initialize TLS listener stream: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn("%s", errmsg);
        if (self->on_listen != NULL) {
            $action2 f = ($action2)self->on_listen;
            f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
        }
        free(state);
        tls_owner_release(owner);
        free(stream);
        close_uv_socket(sock);
        tls_listener_server_state_release(listener_state);
        return;
    }
    tlsuv_stream_set_server(stream, 1);
    stream->authmode = owner->authmode;
    stream->data = state;

    struct tls_listener_conn *conn = (struct tls_listener_conn *)malloc(sizeof(*conn));
    if (conn == NULL) {
        if (self->on_listen != NULL) {
            $action2 f = ($action2)self->on_listen;
            f->$class->__asyn__(f, self, actStrFromCString("Failed to allocate TLS listener connect state"));
        }
        stream->data = NULL;
        free(state);
        tls_owner_release(owner);
        free(stream);
        close_uv_socket(sock);
        tls_listener_server_state_release(listener_state);
        return;
    }
    conn->listener_state = listener_state;
    conn->stream = stream;
    conn->state = state;

    uv_connect_t *connect_req = (uv_connect_t *)calloc(1, sizeof(uv_connect_t));
    if (connect_req == NULL) {
        if (self->on_listen != NULL) {
            $action2 f = ($action2)self->on_listen;
            f->$class->__asyn__(f, self, actStrFromCString("Failed to allocate TLS handshake request"));
        }
        free(conn);
        stream->data = NULL;
        free(state);
        tls_owner_release(owner);
        free(stream);
        close_uv_socket(sock);
        tls_listener_server_state_release(listener_state);
        return;
    }
    connect_req->data = conn;

    r = tlsuv_stream_open(connect_req, stream, sock, tls_listener_on_connect);
    if (r != 0) {
        char errmsg[1024] = "Failed to start TLS handshake: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        if (self->on_listen != NULL) {
            $action2 f = ($action2)self->on_listen;
            f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
        }
        // tlsuv only adopts the fd once its poll watcher is initialized; on
        // failure the dup'd socket is still ours to close.
        close_uv_socket(sock);
        tls_listener_close_stream(stream);
        tls_listener_server_state_release(listener_state);
        free(conn);
        free(connect_req);
        return;
    }
}

static void tls_listener_report_listen_error(netQ_TLSListener self, const char *errmsg) {
    log_warn("%s", errmsg);
    $action2 f = ($action2)self->on_listen;
    f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
}

$R netQ_TLSListenerD__initG_local (netQ_TLSListener self, $Cont c$cont) {
    pin_actor_affinity();

    char errmsg[1024];
    struct tls_owner *owner = tls_owner_from_pem(self->ca_pem, self->cert_pem, self->key_pem, errmsg, sizeof(errmsg));
    if (owner == NULL) {
        tls_listener_report_listen_error(self, errmsg);
        return $R_CONT(c$cont, B_None);
    }
    // 0=none, 2=require (mbedtls specific)
    owner->authmode = self->client_auth ? 2 : 0;

    struct tls_listener_server_state *server_state = tls_listener_server_state_new(self);
    if (server_state == NULL) {
        tls_owner_release(owner);
        tls_listener_report_listen_error(self, "Failed to allocate TLS listener server state");
        return $R_CONT(c$cont, B_None);
    }

    uv_tcp_t *server = (uv_tcp_t *)malloc(sizeof(uv_tcp_t));
    if (server == NULL) {
        free(server_state);
        tls_owner_release(owner);
        tls_listener_report_listen_error(self, "Failed to allocate TLS listener server handle");
        return $R_CONT(c$cont, B_None);
    }

    int r = uv_tcp_init(get_uv_loop(), server);
    if (r != 0) {
        snprintf(errmsg, sizeof(errmsg), "Failed to initialize TLS listener server handle: %s", uv_strerror(r));
        free(server_state);
        free(server);
        tls_owner_release(owner);
        tls_listener_report_listen_error(self, errmsg);
        return $R_CONT(c$cont, B_None);
    }
    server->data = (void *)server_state;

    r = tcp_listen(server, self->address, self->port, on_new_tls_connection, "TLS", errmsg, sizeof(errmsg));
    if (r != 0) {
        uv_close((uv_handle_t *)server, tls_listener_server_on_close);
        tls_owner_release(owner);
        tls_listener_report_listen_error(self, errmsg);
        return $R_CONT(c$cont, B_None);
    }

    self->_tls_owner = (int64_t)(intptr_t)owner;
    self->_stream = (int64_t)(intptr_t)server;
    $action2 f = ($action2)self->on_listen;
    f->$class->__asyn__(f, self, B_None);
    return $R_CONT(c$cont, B_None);
}

$R netQ_TLSListenerD_local_addressG_local (netQ_TLSListener self, $Cont c$cont) {
    char addr[NET_ADDRSTRLEN];
    int port;
    tcp_listener_names(self->_stream, addr, sizeof(addr), &port);
    return $R_CONT(c$cont, actStrFromCStringCopy(addr));
}

$R netQ_TLSListenerD_local_portG_local (netQ_TLSListener self, $Cont c$cont) {
    char addr[NET_ADDRSTRLEN];
    int port;
    tcp_listener_names(self->_stream, addr, sizeof(addr), &port);
    return $R_CONT(c$cont, toB_int(port));
}

$R netQ_TLSListenerD_closeG_local (netQ_TLSListener self, $Cont c$cont) {
    self->on_listen = NULL;
    self->on_accept = NULL;
    tls_listener_close_server(self);
    return $R_CONT(c$cont, B_None);
}

$R netQ_TLSListenerD___cleanup__G_local (netQ_TLSListener self, $Cont c$cont) {
    self->on_listen = NULL;
    self->on_accept = NULL;
    tls_listener_close_server(self);
    return $R_CONT(c$cont, B_None);
}

B_NoneType netQ_TLSListenerD___resume__ (netQ_TLSListener self) {
    self->_stream = -1LL;
    self->_tls_owner = -1LL;
    // on_listen is NULL once close()/__cleanup__ has run.
    if (self->on_listen != NULL) {
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, actStrFromCString("resume"));
    }
    return B_None;
}

$R netQ_TLSListenConnectionD__initG_local (netQ_TLSListenConnection self, $Cont c$cont) {
    set_actor_affinity(self->worker_id);

    tlsuv_stream_t *stream = (tlsuv_stream_t *)(intptr_t)self->server_stream;
    if ((intptr_t)stream != -1) {
        struct tls_listener_stream_state *state = (struct tls_listener_stream_state *)stream->data;
        if (state != NULL) {
            state->actor = self;
            self->_stream = (int64_t)(intptr_t)stream;
            struct tcp_names names;
            tls_stream_names(stream, &names);
            self->_local_address = actStrFromCStringCopy(names.local_address);
            self->_local_port = names.local_port;
            self->_remote_address = actStrFromCStringCopy(names.remote_address);
            self->_remote_port = names.remote_port;
            self->_peer_certificates = tls_peer_certificates(stream);
            self->_verify_result = tls_verify_result(stream);
            self->_alpn = tls_alpn(stream);
            self->_tls_version = tls_version(stream);
        } else {
            self->server_stream = -1LL;
            self->_stream = -1LL;
        }
    } else {
        self->server_stream = -1LL;
        self->_stream = -1LL;
    }

    return $R_CONT(c$cont, B_None);
}

$R netQ_TLSListenConnectionD__read_startG_local (netQ_TLSListenConnection self, $Cont c$cont) {
    tlsuv_stream_t *stream = (tlsuv_stream_t *)(intptr_t)self->_stream;
    if ((intptr_t)stream == -1)
        return $R_CONT(c$cont, B_None);

    int r = tlsuv_stream_read_start(stream, alloc_buffer, tls_listener_on_receive);
    if (r < 0) {
        char errmsg[1024] = "Failed to start reading from TLS listen socket: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        if (self->on_error != NULL) {
            $action2 f = ($action2)self->on_error;
            f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
        }
        return $R_CONT(c$cont, B_None);
    }

    return $R_CONT(c$cont, B_None);
}

$R netQ_TLSListenConnectionD_writeG_local (netQ_TLSListenConnection self, $Cont c$cont, B_bytes data) {
    tlsuv_stream_t *stream = (tlsuv_stream_t *)(intptr_t)self->_stream;
    if ((intptr_t)stream == -1)
        return $R_CONT(c$cont, B_None);

    uv_write_t *wreq = (uv_write_t *)malloc(sizeof(uv_write_t));
    if (wreq == NULL) {
        if (self->on_error != NULL) {
            $action2 f = ($action2)self->on_error;
            f->$class->__asyn__(f, self, actStrFromCString("Failed to allocate TLS listen write request"));
        }
        return $R_CONT(c$cont, B_None);
    }

    struct tls_write_req_state *write_state = tls_write_req_state_new(data);
    if (write_state == NULL) {
        if (self->on_error != NULL) {
            $action2 f = ($action2)self->on_error;
            f->$class->__asyn__(f, self, actStrFromCString("Failed to allocate TLS listen write buffer"));
        }
        free(wreq);
        return $R_CONT(c$cont, B_None);
    }

    wreq->data = write_state;
    uv_buf_t buf = uv_buf_init(write_state->buf, data->nbytes);
    int r = tlsuv_stream_write(wreq, stream, &buf, tls_listener_write_cb);
    if (r < 0) {
        char errmsg[1024] = "Failed to write to TLS listen socket: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        if (self->on_error != NULL) {
            $action2 f = ($action2)self->on_error;
            f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
        }
        tls_write_req_state_free(write_state);
        free(wreq);
    }
    return $R_CONT(c$cont, B_None);
}

// Detach the actor backref before an actor-initiated asynchronous close: the
// close and pending-write callbacks run after the current message completes,
// by which point nothing may be keeping the actor alive (the GC pointer lives
// in a libc struct the collector cannot see).
static void tls_listener_stream_detach_actor(tlsuv_stream_t *stream) {
    struct tls_listener_stream_state *state = (struct tls_listener_stream_state *)stream->data;
    if (state != NULL) {
        state->actor = NULL;
    }
}

$R netQ_TLSListenConnectionD_closeG_local (netQ_TLSListenConnection self, $Cont c$cont) {
    tlsuv_stream_t *stream = (tlsuv_stream_t *)(intptr_t)self->_stream;
    if ((intptr_t)stream == -1) {
        tls_listener_release_connection_owner(self);
        return $R_CONT(c$cont, B_None);
    }

    self->_stream = -1LL;
    self->server_stream = -1LL;
    tls_listener_stream_detach_actor(stream);
    tls_listener_close_stream(stream);
    tls_listener_release_connection_owner(self);
    return $R_CONT(c$cont, B_None);
}

$R netQ_TLSListenConnectionD___cleanup__G_local (netQ_TLSListenConnection self, $Cont c$cont) {
    tlsuv_stream_t *stream = (tlsuv_stream_t *)(intptr_t)self->_stream;
    self->_stream = -1LL;
    self->server_stream = -1LL;
    self->on_receive = NULL;
    self->on_error = NULL;
    self->on_remote_close = NULL;

    if ((intptr_t)stream != -1) {
        tls_listener_stream_detach_actor(stream);
        tls_listener_close_stream(stream);
    }
    tls_listener_release_connection_owner(self);
    return $R_CONT(c$cont, B_None);
}

B_NoneType netQ_TLSListenConnectionD___resume__ (netQ_TLSListenConnection self) {
    self->server_stream = -1LL;
    self->_stream = -1LL;
    self->tls_owner = -1LL;
    self->worker_id = -1LL;
    return B_None;
}

//netQ_DNSCap netQ_TCPConnectionD__dnscap (netQ_TCPConnection self) {
//    netQ_DNSCap c = netQ_DNSCapG_new(void);
//    return c;
//}

static void tls_on_close(uv_handle_t* stream) {
    log_debug("TLS handle closed, stream: %p  stream->data: %p", stream, stream->data);
    struct tls_client_state *state = (struct tls_client_state *)stream->data;
    if (state != NULL) {
        if (state->actor != NULL) {
            state->actor->_stream = -1LL;
            state->actor = NULL;
        }
        tls_client_state_free(state);
    }
    stream->data = NULL;
    free(stream);
}

static void tls_close(tlsuv_stream_t *stream) {
    if (stream == NULL) {
        return;
    }
    uv_handle_t *watcher = (uv_handle_t *)&stream->watcher;
    if (uv_handle_get_type(watcher) != UV_UNKNOWN_HANDLE && uv_is_closing(watcher)) {
        return;
    }

    log_debug("Closing TLS stream: %p  stream->data: %p", stream, stream->data);
    struct tls_client_state *state = (struct tls_client_state *)stream->data;
    netQ_TLSConnection self = state != NULL ? state->actor : NULL;
    if (self != NULL) {
        $action on_close = self->_on_close;
        self->_on_close = NULL;
        self->_stream = -1LL;
        state->actor = NULL;
        if (on_close) {
            on_close->$class->__asyn__(on_close, self);
        }
    }
    tlsuv_stream_read_stop(stream);
    tlsuv_stream_close(stream, tls_on_close);
}

void tls_on_receive(uv_stream_t *stream, ssize_t nread, const uv_buf_t* buf) {
    struct tls_client_state *state = (struct tls_client_state *)((tlsuv_stream_t *)stream)->data;
    netQ_TLSConnection self = state != NULL ? state->actor : NULL;
    if (self == NULL) {
        return;
    }

    if (nread > 0) {
        if (stream->data && self->on_receive != NULL) {
            $action2 f = ($action2)self->on_receive;
            B_bytes data = actBytesFromCStringLengthCopy(buf->base, nread);
            f->$class->__asyn__(f, self, data);
            self->_bytes_in += nread;
        }
    } else if (nread == UV_EOF) {
        log_debug("TLS connection closed %p", stream);
        tls_close((tlsuv_stream_t *)stream);
        if (self->on_remote_close) {
            $action f = ($action)self->on_remote_close;
            f->$class->__asyn__(f, self);
        }
    }
    else if (nread < 0) {
        char errmsg[1024] = "TLS read error: ";
        uv_strerror_r((int)nread, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_debug("%s", errmsg);
        tls_close((tlsuv_stream_t *)stream);
        if (self->on_error != NULL) {
            self->$class->_on_tls_error(self, -1LL, (int64_t)nread, actStrFromCStringCopy(errmsg));
        }
    }
}

void tls_write_cb(uv_write_t *wreq, int status) {
    struct tls_write_req_state *write_state = (struct tls_write_req_state *)wreq->data;
    // ECANCELED: queued write flushed by the stream's own close path.
    if (status < 0 && status != UV_ECANCELED) {
        log_debug("TLS write error %d: %s", status, uv_strerror(status));
        char errmsg[1024] = "Failed to write to TLS TCP socket: ";
        uv_strerror_r(status, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_debug(errmsg);
        struct tls_client_state *state = NULL;
        if (wreq->handle != NULL) {
            state = (struct tls_client_state *)((tlsuv_stream_t *)wreq->handle)->data;
        }
        netQ_TLSConnection self = state != NULL ? state->actor : NULL;
        if (self != NULL && self->on_error != NULL) {
            $action2 on_error = ($action2)self->on_error;
            on_error->$class->__asyn__(on_error, self, actStrFromCStringCopy(errmsg));
        }
        tls_close((tlsuv_stream_t *)wreq->handle);
    }
    tls_write_req_state_free(write_state);
    free(wreq);
}

$R netQ_TLSConnectionD_closeG_local (netQ_TLSConnection self, $Cont c$cont, $action on_close) {
    uv_stream_t *stream = (uv_stream_t *)(intptr_t)self->_stream;
    // fd == -1 means invalid FD and can happen after __resume__
    if ((intptr_t)stream == -1)
        return $R_CONT(c$cont, B_None);

    self->_on_close = on_close;

    log_debug("Closing TLS TCP connection");
    tls_close((tlsuv_stream_t *)stream);
    // TODO: implement on_error? but tlsuv_stream_close always returns 0 and in
    // the on_close callback the ->data is already NULL so how to find our actor
    // and real callback?
    return $R_CONT(c$cont, B_None);
}

$R netQ_TLSConnectionD_writeG_local (netQ_TLSConnection self, $Cont c$cont, B_bytes data) {
    tlsuv_stream_t *stream = (tlsuv_stream_t *)(intptr_t)self->_stream;
    // fd == -1 means invalid FD and can happen after __resume__
    if ((intptr_t)stream == -1)
        return $R_CONT(c$cont, B_None);

    // Writes before the handshake completed would reach into a TLS engine
    // that does not exist yet (tlsuv creates it during connect).
    struct tls_client_state *state = (struct tls_client_state *)stream->data;
    if (state == NULL || !state->connected) {
        if (self->on_error != NULL) {
            $action2 f = ($action2)self->on_error;
            f->$class->__asyn__(f, self, actStrFromCString("TLS connection not established"));
        }
        return $R_CONT(c$cont, B_None);
    }

    uv_write_t *wreq = (uv_write_t *)malloc(sizeof(uv_write_t));
    if (wreq == NULL) {
        if (self->on_error != NULL) {
            $action2 f = ($action2)self->on_error;
            f->$class->__asyn__(f, self, actStrFromCString("Failed to allocate TLS write request"));
        }
        return $R_CONT(c$cont, B_None);
    }

    struct tls_write_req_state *write_state = tls_write_req_state_new(data);
    if (write_state == NULL) {
        if (self->on_error != NULL) {
            $action2 f = ($action2)self->on_error;
            f->$class->__asyn__(f, self, actStrFromCString("Failed to allocate TLS write buffer"));
        }
        free(wreq);
        return $R_CONT(c$cont, B_None);
    }

    wreq->data = write_state;
    uv_buf_t buf = uv_buf_init(write_state->buf, data->nbytes);
    int r = tlsuv_stream_write(wreq, stream, &buf, tls_write_cb);
    if (r < 0) {
        char errmsg[1024] = "Failed to write to TLS TCP socket: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_debug(errmsg);
        if (self->on_error != NULL) {
            $action2 f = ($action2)self->on_error;
            f->$class->__asyn__(f, self, actStrFromCStringCopy(errmsg));
        }
        tls_write_req_state_free(write_state);
        free(wreq);
    }
    self->_bytes_out += data->nbytes;
    return $R_CONT(c$cont, B_None);
}


static void tls_on_connect(uv_connect_t *creq, int status) {
    struct tls_client_state *state = (struct tls_client_state *)creq->data;
    netQ_TLSConnection self = state != NULL ? state->actor : NULL;
    if (self == NULL) {
        tls_close((tlsuv_stream_t *)creq->handle);
        free(creq);
        return;
    }

    if (status != 0) {
        char errmsg[1024] = "Error in TLS TCP connect: ";
        uv_strerror_r(status, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_debug(errmsg);
        tls_close((tlsuv_stream_t *)creq->handle);
        if (self->on_error != NULL) {
            self->$class->_on_tls_error(self, -1LL, (int64_t)status, actStrFromCStringCopy(errmsg));
        }
        free(creq);
        return;
    }

    tlsuv_stream_t *stream = (tlsuv_stream_t *) creq->handle;
    int r = tlsuv_stream_read_start(stream, alloc_buffer, tls_on_receive);
    if (r < 0) {
        char errmsg[1024] = "Failed to start reading from TLS TCP socket: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_debug(errmsg);
        tls_close(stream);
        if (self->on_error != NULL) {
            self->$class->_on_tls_error(self, -1LL, (int64_t)r, actStrFromCStringCopy(errmsg));
        }
        free(creq);
        return;
    }

    state->connected = true;
    struct tcp_names names;
    tls_stream_names(stream, &names);
    self->_local_address = actStrFromCStringCopy(names.local_address);
    self->_local_port = names.local_port;
    self->_remote_address = actStrFromCStringCopy(names.remote_address);
    self->_remote_port = names.remote_port;
    self->_peer_certificates = tls_peer_certificates(stream);
    self->_verify_result = tls_verify_result(stream);
    self->_alpn = tls_alpn(stream);
    self->_tls_version = tls_version(stream);
    if (self->on_connect != NULL) {
        self->$class->_on_tls_connect(self);
    } else {
        tls_close(stream);
    }
    free(creq);
}

void tlsuv_logger(int level, const char *file, unsigned int line, const char *msg) {
    // Map tlsuv log levels to our RTS log levels
    int log_level;
    if (level == 0) {        // NONE, used to say "we don't want logs"
        // not actually used for any log messages, so this should be unreachable
        return;
    } else if (level == 1) { // ERR
        log_level = LOG_ERROR;
    } else if (level == 2) { // WARN
        log_level = LOG_WARN;
    } else if (level == 3) { // INFO
        log_level = LOG_INFO;
    } else if (level == 4) { // DEBG
        log_level = LOG_DEBUG;
    } else if (level == 5) { // VERB
        log_level = LOG_DEBUG;
    } else if (level == 6) { // TRACE
        log_level = LOG_TRACE;
    }

    log_log(log_level, file, line, msg);
}

$R netQ_TLSConnectionD__connect_tlsG_local (netQ_TLSConnection self, $Cont c$cont) {
    char errmsg[1024];
    // A context of our own for a private CA or a client certificate, else
    // tlsuv's default context
    struct tls_owner *owner = NULL;
    if (self->ca_pem != NULL || self->cert_pem != NULL || self->key_pem != NULL) {
        owner = tls_owner_from_pem(self->ca_pem, self->cert_pem, self->key_pem, errmsg, sizeof(errmsg));
        if (owner == NULL) {
            if (self->on_error != NULL) {
                self->$class->_on_tls_error(self, -1LL, (int64_t)UV_EINVAL, actStrFromCStringCopy(errmsg));
            }
            return $R_CONT(c$cont, B_None);
        }
    }

    uv_connect_t* connect_req = (uv_connect_t*)calloc(1, sizeof(uv_connect_t));
    if (connect_req == NULL) {
        tls_owner_release(owner);
        if (self->on_error != NULL) {
            self->$class->_on_tls_error(self, -1LL, (int64_t)UV_ENOMEM, actStrFromCString("Failed to allocate TLS connect request"));
        }
        return $R_CONT(c$cont, B_None);
    }

    //tlsuv_set_debug(5, tlsuv_logger);
    tlsuv_stream_t *stream = (tlsuv_stream_t *)malloc(sizeof(tlsuv_stream_t));
    if (stream == NULL) {
        free(connect_req);
        tls_owner_release(owner);
        if (self->on_error != NULL) {
            self->$class->_on_tls_error(self, -1LL, (int64_t)UV_ENOMEM, actStrFromCString("Failed to allocate TLS stream"));
        }
        return $R_CONT(c$cont, B_None);
    }
    struct tls_client_state *state = tls_client_state_new(self);
    if (state == NULL) {
        free(stream);
        free(connect_req);
        tls_owner_release(owner);
        if (self->on_error != NULL) {
            self->$class->_on_tls_error(self, -1LL, -1LL, actStrFromCString("Failed to allocate TLS client state"));
        }
        return $R_CONT(c$cont, B_None);
    }
    state->owner = owner;
    connect_req->data = (void *)state;
    int r = tlsuv_stream_init(get_uv_loop(), stream, owner != NULL ? owner->tls : NULL);
    if (r != 0) {
        snprintf(errmsg, sizeof(errmsg), "Failed to initialize TLS stream: ");
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn("%s", errmsg);
        tls_client_state_free(state);
        free(stream);
        free(connect_req);
        if (self->on_error != NULL) {
            self->$class->_on_tls_error(self, -1LL, (int64_t)r, actStrFromCStringCopy(errmsg));
        }
        return $R_CONT(c$cont, B_None);
    }

    // Default is to verify TLS certificate. Should we disable verification?
    if (self->verify_tls == false) {
        log_debug("TLS certificate verification disabled");
        stream->authmode = 0; // 0=none, 2=require (mbedtls specific)
    }

    // TODO: take ALPN as input to TLSConnection actor
    //const char *alpn[] = { "http/1.1" };
    int num_protocols = self->protocols->length;

    if (num_protocols > 0) {
        state->alpn_protocols = calloc((size_t)num_protocols, sizeof(char *));
        if (state->alpn_protocols == NULL) {
            tls_client_state_free(state);
            free(stream);
            free(connect_req);
            if (self->on_error != NULL) {
                self->$class->_on_tls_error(self, -1LL, (int64_t)UV_ENOMEM, actStrFromCString("Failed to allocate TLS ALPN protocol list"));
            }
            return $R_CONT(c$cont, B_None);
        }
        // Count covers the whole zero-filled array from the start so a
        // partial strdup failure still frees the strings already duplicated.
        state->alpn_count = num_protocols;

        for (int i = 0; i < num_protocols; ++i) {
            const char *proto = (const char *)fromB_str(self->protocols->data[i]);
            state->alpn_protocols[i] = strdup(proto);
            if (state->alpn_protocols[i] == NULL) {
                tls_client_state_free(state);
                free(stream);
                free(connect_req);
                if (self->on_error != NULL) {
                    self->$class->_on_tls_error(self, -1LL, (int64_t)UV_ENOMEM, actStrFromCString("Failed to allocate TLS ALPN protocol"));
                }
                return $R_CONT(c$cont, B_None);
            }
        }
        tlsuv_stream_set_protocols(stream, state->alpn_count, (const char **)state->alpn_protocols);
    }
    stream->data = (void *)state;

    // _stream must be armed BEFORE the connect call: tlsuv may fail the
    // connect through its callback path, and tls_close run from the callback
    // resets _stream to -1 - assigning afterwards would re-arm a dangling
    // pointer to an already-torn-down stream.
    self->_stream = (int64_t)(intptr_t)stream;

    r = tlsuv_stream_connect(connect_req, stream, (const char *)fromB_str(self->address), (int)self->port, tls_on_connect);
    if (r != 0) {
        snprintf(errmsg, sizeof(errmsg), "Failed to start TLS TCP connect: ");
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn("%s", errmsg);
        self->_stream = -1LL;
        stream->data = NULL;
        state->actor = NULL;
        tls_client_state_free(state);
        tlsuv_stream_close(stream, tls_on_close);
        free(connect_req);
        if (self->on_error != NULL) {
            self->$class->_on_tls_error(self, -1LL, (int64_t)r, actStrFromCStringCopy(errmsg));
        }
        return $R_CONT(c$cont, B_None);
    }

    // tlsuv_stream_connect sets the connect address as the name for SNI and
    // certificate verification, which the TLS engine takes only once TCP is
    // connected, so it can be replaced here
    if (self->server_name != NULL) {
        const char *name = (const char *)fromB_str(self->server_name);
        tlsuv_stream_set_hostname(stream, name[0] != '\0' ? name : NULL);
    }

    return $R_CONT(c$cont, B_None);
}

$R netQ_TLSConnectionD___cleanup__G_local (netQ_TLSConnection self, $Cont c$cont) {
    tlsuv_stream_t *stream = (tlsuv_stream_t *)(intptr_t)self->_stream;
    self->_stream = -1LL;
    self->_on_close = NULL;
    self->on_connect = NULL;
    self->on_receive = NULL;
    self->on_error = NULL;
    self->on_remote_close = NULL;

    if ((intptr_t)stream != -1) {
        tls_close(stream);
    }
    return $R_CONT(c$cont, B_None);
}

$R netQ_TLSConnectionD__pin_affinityG_local (netQ_TLSConnection self, $Cont c$cont) {
    pin_actor_affinity();
    return $R_CONT(c$cont, B_None);
}
