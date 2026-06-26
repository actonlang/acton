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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef _WIN32
#include <unistd.h>
#endif
#include "../rts/io.h"
#include "../rts/log.h"

void netQ___ext_init__() {
    // NOP
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
    if (inet_pton(AF_INET6, (const char *)fromB_str(address), &(sa.sin6_addr)) == 0) {
        return false;
    } else {
        return true;
    }
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

static void _lookup_a__on_resolve (uv_getaddrinfo_t *req, int status, struct addrinfo *dns_res) {
    struct dns_cb_data *cb_data = req->data;
    B_list $res = B_listG_new(NULL, NULL);

    if (status != 0) {
        char errmsg[1024] = "DNS lookup error: ";
        uv_strerror_r(status, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        $action2 f = ($action2)cb_data->on_error;
        f->$class->__asyn__(f, to$str(cb_data->hostname), to$str(errmsg));

        uv_freeaddrinfo(dns_res);
        GC_free(cb_data);
        free(req);
        return;
    }

    struct addrinfo *rp;
    char addr[17] = {'\0'};
    for (rp = dns_res; rp != NULL; rp = rp->ai_next) {
        uv_ip4_name((struct sockaddr_in*) rp->ai_addr, addr, 16);
        B_SequenceD_listG_witness->$class->append(B_SequenceD_listG_witness, $res, to$str(addr));
    }

    $action f = ($action)cb_data->on_resolve;
    f->$class->__asyn__(f, $res);

    uv_freeaddrinfo(dns_res);
    GC_free(cb_data);
    free(req);
}

B_NoneType netQ__lookup_a (B_str name, $action on_resolve, $action on_error) {
    // libuv copies node/service/hints synchronously into its own request
    // buffer, so hints can live on the stack.
    struct addrinfo hints = {0};
    hints.ai_family = PF_INET;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_protocol = IPPROTO_TCP;

    struct dns_cb_data *cb_data = (struct dns_cb_data *)GC_malloc_uncollectable(sizeof(struct dns_cb_data));
    if (cb_data == NULL) {
        $action2 f = ($action2)on_error;
        f->$class->__asyn__(f, name, to$str("Unable to run DNS query: out of memory"));
        return B_None;
    }
    cb_data->hostname = (char *)fromB_str(name);
    cb_data->on_resolve = on_resolve;
    cb_data->on_error = ($action2) on_error;

    // The request enters libuv's threadpool work queue, whose intrusive links
    // pass through libc-allocated nodes (tlsuv connects); a GC-allocated
    // request would be invisible to the collector while queued behind one.
    uv_getaddrinfo_t *req = (uv_getaddrinfo_t*)malloc(sizeof(uv_getaddrinfo_t));
    if (req == NULL) {
        GC_free(cb_data);
        $action2 f = ($action2)on_error;
        f->$class->__asyn__(f, name, to$str("Unable to run DNS query: out of memory"));
        return B_None;
    }
    req->data = cb_data;

    int r = uv_getaddrinfo(get_uv_loop(), req, _lookup_a__on_resolve, (const char *)fromB_str(name), NULL, &hints);
    if (r != 0) {
        char errmsg[1024] = "Unable to run DNS query: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)cb_data->on_error;
        f->$class->__asyn__(f, name, to$str(errmsg));
        GC_free(cb_data);
        free(req);
        return B_None;
    }

    return B_None;
}

static void _lookup_aaaa__on_resolve (uv_getaddrinfo_t *req, int status, struct addrinfo *dns_res) {
    struct dns_cb_data *cb_data = req->data;
    B_list $res = B_listG_new(NULL, NULL);

    if (status != 0) {
        char errmsg[1024] = "DNS lookup error: ";
        uv_strerror_r(status, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        $action2 f = ($action2)cb_data->on_error;
        f->$class->__asyn__(f, to$str(cb_data->hostname), to$str(errmsg));

        uv_freeaddrinfo(dns_res);
        GC_free(cb_data);
        free(req);
        return;
    }

    struct addrinfo *rp;
    char addr[40] = {'\0'};
    for (rp = dns_res; rp != NULL; rp = rp->ai_next) {
        uv_ip6_name((struct sockaddr_in6*)rp->ai_addr, addr, 39);
        B_SequenceD_listG_witness->$class->append(B_SequenceD_listG_witness, $res, to$str(addr));
    }

    $action f = ($action)cb_data->on_resolve;
    f->$class->__asyn__(f, $res);

    uv_freeaddrinfo(dns_res);
    GC_free(cb_data);
    free(req);
}

B_NoneType netQ__lookup_aaaa (B_str name, $action on_resolve, $action on_error) {
    struct addrinfo hints = {0};
    hints.ai_family = PF_INET6;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_protocol = IPPROTO_TCP;

    struct dns_cb_data *cb_data = (struct dns_cb_data *)GC_malloc_uncollectable(sizeof(struct dns_cb_data));
    if (cb_data == NULL) {
        $action2 f = ($action2)on_error;
        f->$class->__asyn__(f, name, to$str("Unable to run DNS query: out of memory"));
        return B_None;
    }
    cb_data->hostname = (char *)fromB_str(name);
    cb_data->on_resolve = on_resolve;
    cb_data->on_error = ($action2)on_error;

    uv_getaddrinfo_t *req = (uv_getaddrinfo_t*)malloc(sizeof(uv_getaddrinfo_t));
    if (req == NULL) {
        GC_free(cb_data);
        $action2 f = ($action2)on_error;
        f->$class->__asyn__(f, name, to$str("Unable to run DNS query: out of memory"));
        return B_None;
    }
    req->data = cb_data;

    int r = uv_getaddrinfo(get_uv_loop(), req, _lookup_aaaa__on_resolve, (const char *)fromB_str(name), NULL, &hints);
    if (r != 0) {
        char errmsg[1024] = "Unable to run DNS query: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)cb_data->on_error;
        f->$class->__asyn__(f, name, to$str(errmsg));
        GC_free(cb_data);
        free(req);
        return B_None;
    }

    return B_None;
}

void netQ_TCPConnection__on_receive(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buf) {
    if (nread < 0){
        if (nread == UV_EOF) {
            netQ_TCPConnection self = stream->data;
            if ((intptr_t)stream != -1)
                uv_close((uv_handle_t *)stream, NULL);
            self->_sock = -1LL;
            self->_sock4 = -1LL;
            self->_sock6 = -1LL;
            if (self->on_remote_close) {
                $action f = ($action)self->on_remote_close;
                f->$class->__asyn__(f, self);
            }
        }
    } else if (nread > 0) {
        if (stream->data) {
            netQ_TCPConnection self = stream->data;
            $action2 f = ($action2)self->on_receive;
            f->$class->__asyn__(f, self, to$bytesD_len(buf->base, nread));
            self->_bytes_in += nread;
        }
    }
}


$R netQ_TCPConnectionD__pin_affinityG_local (netQ_TCPConnection self, $Cont c$cont) {
    pin_actor_affinity();
    return $R_CONT(c$cont, B_None);
}

void on_connect4(uv_connect_t *connect_req, int status) {
    netQ_TCPConnection self = (netQ_TCPConnection)connect_req->data;

    if (status != 0) {
        char errmsg[1024] = "Error in TCP connect over IPv4: ";
        uv_strerror_r(status, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        self->$class->_on_tcp_error(self, 4LL, (int64_t)status, to$str(errmsg));
        return;
    }
    self->$class->_on_connect4(self);
}

void on_connect6(uv_connect_t *connect_req, int status) {
    netQ_TCPConnection self = (netQ_TCPConnection)connect_req->data;

    if (status != 0) {
        char errmsg[1024] = "Error in TCP connect over IPv6: ";
        uv_strerror_r(status, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        self->$class->_on_tcp_error(self, 6LL, (int64_t)status, to$str(errmsg));
        return;
    }
    self->$class->_on_connect6(self);
}

$R netQ_TCPConnectionD__connect4G_local (netQ_TCPConnection self, $Cont c$cont, B_str ip_address) {
    log_debug("TCP connecting over IPv4 to %s", fromB_str(ip_address));
    uv_tcp_t* socket = (uv_tcp_t*)acton_malloc(sizeof(uv_tcp_t));
    uv_tcp_init(get_uv_loop(), socket);
    self->_sock4 = (int64_t)(intptr_t)socket;

    uv_connect_t* connect_req = (uv_connect_t*)acton_malloc(sizeof(uv_connect_t));
    connect_req->data = (void *)self;

    struct sockaddr_in dest;
    uv_ip4_addr((const char *)fromB_str(ip_address), (int)self->port, &dest);

    uv_tcp_connect(connect_req, socket, (const struct sockaddr*)&dest, on_connect4);

    return $R_CONT(c$cont, B_None);
}

$R netQ_TCPConnectionD__connect6G_local (netQ_TCPConnection self, $Cont c$cont, B_str ip_address) {
    log_debug("TCP connecting over IPv6 to %s", fromB_str(ip_address));
    uv_tcp_t* socket = (uv_tcp_t*)acton_malloc(sizeof(uv_tcp_t));
    uv_tcp_init(get_uv_loop(), socket);
    self->_sock6 = (int64_t)(intptr_t)socket;

    uv_connect_t* connect_req = (uv_connect_t*)acton_malloc(sizeof(uv_connect_t));
    connect_req->data = (void *)self;

    struct sockaddr_in6 dest;
    uv_ip6_addr((const char *)fromB_str(ip_address), (int)self->port, &dest);

    uv_tcp_connect(connect_req, socket, (const struct sockaddr*)&dest, on_connect6);

    return $R_CONT(c$cont, B_None);
}

$R netQ_TCPConnectionD__read_startG_local (netQ_TCPConnection self, $Cont c$cont) {
    uv_tcp_t* socket = (uv_tcp_t *)(intptr_t)self->_sock;
    socket->data = self;
    int r = uv_read_start((uv_stream_t *)socket, alloc_buffer, netQ_TCPConnection__on_receive);
    if (r < 0) {
        char errmsg[1024] = "Failed to start reading from TCP client socket: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)self->on_error;
        f->$class->__asyn__(f, self, to$str(errmsg));
        return $R_CONT(c$cont, B_None);
    }

    return $R_CONT(c$cont, B_None);
}

$R netQ_TCPConnectionD_writeG_local (netQ_TCPConnection self, $Cont c$cont, B_bytes data) {
    uv_stream_t *stream = (uv_stream_t *)(intptr_t)self->_sock;
    // fd == -1 means invalid FD and can happen after __resume__
    if ((intptr_t)stream == -1)
        return $R_CONT(c$cont, B_None);

    uv_write_t *req = (uv_write_t *)acton_malloc(sizeof(uv_write_t));
    uv_buf_t buf = uv_buf_init((char *)data->str, data->nbytes);
    int r = uv_write(req, stream, &buf, 1, NULL);
    if (r < 0) {
        char errmsg[1024] = "Failed to write to TCP socket: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)self->on_error;
        f->$class->__asyn__(f, self, to$str(errmsg));
    }
    self->_bytes_out += data->nbytes;
    return $R_CONT(c$cont, B_None);
}

B_NoneType netQ_TCPConnectionD___resume__ (netQ_TCPConnection self) {
    self->_sock = -1LL;
    self->_sock4 = -1LL;
    self->_sock6 = -1LL;
    $action2 f = ($action2)self->on_error;
    f->$class->__asyn__(f, self, to$str("resume"));
    return B_None;
}

struct close_cb_data {
    netQ_TCPConnection self;
    $action on_close;
};

static void after_shutdown(uv_shutdown_t* req, int status) {
    log_debug("TCP Shutdown complete, closing handle");
    if (status < 0)
        log_warn("Error in TCP shutdown: %s", uv_strerror(status));
    uv_close((uv_handle_t*)req->handle, NULL);
    struct close_cb_data *cb_data = (struct close_cb_data *)req->data;
    $action on_close = ($action)cb_data->on_close;
    on_close->$class->__asyn__(on_close, cb_data->self);
}

$R netQ_TCPConnectionD_closeG_local (netQ_TCPConnection self, $Cont c$cont, $action on_close) {
    uv_stream_t *stream = (uv_stream_t *)(intptr_t)self->_sock;
    // fd == -1 means invalid FD and can happen after __resume__
    if ((intptr_t)stream == -1) {
        on_close->$class->__asyn__(on_close, self);
        return $R_CONT(c$cont, B_None);
    }

    log_debug("Closing TCP connection");
    struct close_cb_data *cb_data = (struct close_cb_data *)acton_malloc(sizeof(struct close_cb_data));
    cb_data->self = self;
    cb_data->on_close = on_close;
    uv_shutdown_t *req = (uv_shutdown_t *)acton_malloc(sizeof(uv_shutdown_t));
    req->data = (void *)cb_data;
    int r = uv_shutdown(req, stream, after_shutdown);
    self->_sock = -1LL;
    self->_sock4 = -1LL;
    self->_sock6 = -1LL;
    if (r < 0) {
        // TODO: we could probably ignore most or all of these errors as the
        // purpose is to shutdown our side of the connection and many of these
        // errors are about "not connected" or "already closed", turning this
        // into a NOP
        char errmsg[1024] = "Failed to shutdown TCP client socket: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        on_close->$class->__asyn__(on_close, self);
        // TODO: we should always call uv_close() here
        return $R_CONT(c$cont, B_None);
    }
    return $R_CONT(c$cont, B_None);
}

$R netQ_TCPConnectionD_ip_versionG_local (netQ_TCPConnection self, $Cont c$cont) {
    struct sockaddr_storage peername;
    int namelen = sizeof(peername);
    if (uv_tcp_getpeername((const uv_tcp_t *)(intptr_t)self->_sock, (struct sockaddr*)&peername, &namelen) == 0) {
        if (peername.ss_family == AF_INET) {
            return $R_CONT(c$cont, toB_int(4));
        } else if (peername.ss_family == AF_INET6) {
            return $R_CONT(c$cont, toB_int(6));
        } else {
            log_error("Unhandled AFI");
        }
    } else {
        log_error("Failed to get peer name from handle %lld", (long long)self->_sock);
    }
    return $R_CONT(c$cont, B_None);
}

$R netQ_TCPConnectionD_local_addressG_local (netQ_TCPConnection self, $Cont c$cont) {
    struct sockaddr_storage sockname;
    int namelen = sizeof(sockname);
    char addr[INET6_ADDRSTRLEN] = { '\0' };
    if (uv_tcp_getsockname((const uv_tcp_t *)(intptr_t)self->_sock, (struct sockaddr*)&sockname, &namelen) == 0) {
        if (sockname.ss_family == AF_INET) {
            uv_ip4_name((struct sockaddr_in*)&sockname, addr, sizeof(addr));
        } else if (sockname.ss_family == AF_INET6) {
            uv_ip6_name((struct sockaddr_in6*)&sockname, addr, sizeof(addr));
        } else {
            log_error("Unhandled AFI");
        }
    } else {
        log_error("Failed to get sock name from handle %lld", (long long)self->_sock);
    }
    return $R_CONT(c$cont, to$str(addr));
}

$R netQ_TCPConnectionD_remote_addressG_local (netQ_TCPConnection self, $Cont c$cont) {
    struct sockaddr_storage peername;
    int namelen = sizeof(peername);
    char addr[INET6_ADDRSTRLEN] = { '\0' };
    if (uv_tcp_getpeername((const uv_tcp_t *)(intptr_t)self->_sock, (struct sockaddr*)&peername, &namelen) == 0) {
        if (peername.ss_family == AF_INET) {
            uv_ip4_name((struct sockaddr_in*)&peername, addr, sizeof(addr));
        } else if (peername.ss_family == AF_INET6) {
            uv_ip6_name((struct sockaddr_in6*)&peername, addr, sizeof(addr));
        } else {
            log_error("Unhandled AFI");
        }
    } else {
        log_error("Failed to get peer name from handle %lld", (long long)self->_sock);
    }
    return $R_CONT(c$cont, to$str(addr));
}


void on_new_connection(uv_stream_t *server, int status) {
    netQ_TCPListener self = (netQ_TCPListener)server->data;

    if (status != 0) {
        char errmsg[1024] = "Error on new TCP client connection: ";
        uv_strerror_r(status, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, to$str(errmsg));
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
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, to$str(errmsg));
        // NOTE: free() here if do manual memory management in I/O one day
        return;
    }

    self->$class->create_tcp_listen_connection(self, B_None, (int64_t)(intptr_t)client);
    // NOTE: free() here if do manual memory management in I/O one day
}


$R netQ_TCPListenerD__initG_local (netQ_TCPListener self, $Cont c$cont) {
    pin_actor_affinity();

    uv_tcp_t *server = (uv_tcp_t *)acton_malloc(sizeof(uv_tcp_t));
    uv_tcp_init(get_uv_loop(), server);
    server->data = (void *)self;
    int r;
    struct sockaddr_in addr4;
    struct sockaddr_in6 addr6;
    if (inet_pton(AF_INET, (const char *)fromB_str(self->address), &(addr4.sin_addr)) == 1) {
        r = uv_ip4_addr((const char *)fromB_str(self->address), (int)self->port, &addr4);
    } else if (inet_pton(AF_INET6, (const char *)fromB_str(self->address), &(addr6.sin6_addr)) == 1) {
        r = uv_ip6_addr((const char *)fromB_str(self->address), (int)self->port, &addr6);
    } else {
        B_str errmsg = $FORMAT("Address is not an IPv4 or IPv6 address: %s", fromB_str(self->address));
        log_warn((const char *)fromB_str(errmsg));
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, errmsg);
        // NOTE: free() here if do manual memory management in I/O one day
        return $R_CONT(c$cont, B_None);
    }
    if (r != 0) {
        char errmsg[1024] = "Unable to parse address: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, to$str(errmsg));
        // NOTE: free() here if do manual memory management in I/O one day
        return $R_CONT(c$cont, B_None);
    }

    if (inet_pton(AF_INET, (const char *)fromB_str(self->address), &(addr4.sin_addr)) == 1) {
        r = uv_tcp_bind(server, (const struct sockaddr *)&addr4, 0);
    } else if (inet_pton(AF_INET6, (const char *)fromB_str(self->address), &(addr6.sin6_addr)) == 1) {
        r = uv_tcp_bind(server, (const struct sockaddr *)&addr6, 0);
    }
    if (r != 0) {
        char errmsg[1024] = "Error in TCP bind: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, to$str(errmsg));
        // NOTE: free() here if do manual memory management in I/O one day
        return $R_CONT(c$cont, B_None);
    }

    r = uv_listen((uv_stream_t *)server, 1024, on_new_connection);
    if (r != 0) {
        char errmsg[1024] = "Error in TCP listen: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, to$str(errmsg));
        // NOTE: free() here if do manual memory management in I/O one day
        return $R_CONT(c$cont, B_None);
    }

    $action2 f = ($action2)self->on_listen;
    f->$class->__asyn__(f, self, B_None);
    return $R_CONT(c$cont, B_None);
}

B_NoneType netQ_TCPListenerD___resume__ (netQ_TCPListener self) {
    self->_stream = -1LL;
    $action2 f = ($action2)self->on_listen;
    f->$class->__asyn__(f, self, to$str("resume"));
    return B_None;
}

$R netQ_TCPListenConnectionD__initG_local (netQ_TCPListenConnection self, $Cont c$cont) {
    pin_actor_affinity();

    uv_stream_t *client = (uv_stream_t *)(intptr_t)self->server_client;
    uv_os_fd_t fd;
    uv_fileno((uv_handle_t *)client, &fd);
    uv_tcp_t* client_handle = (uv_tcp_t*)acton_malloc(sizeof(uv_tcp_t));
    uv_tcp_init(get_uv_loop(), client_handle);
    uv_tcp_open(client_handle, (uv_os_sock_t)fd);
    self->client = (int64_t)(intptr_t)client_handle;

    return $R_CONT(c$cont, B_None);
}

void netQ_TCPListenConnection__on_receive(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buf) {
    if (nread < 0){
        if (nread == UV_EOF) {
            netQ_TCPListenConnection self = stream->data;
            if ((intptr_t)stream != -1)
                uv_close((uv_handle_t *)stream, NULL);
            self->client = -1LL;
            if (self->on_remote_close) {
                $action f = ($action)self->on_remote_close;
                f->$class->__asyn__(f, self);
            }
        }
    } else if (nread > 0) {
        if (stream->data) {
            netQ_TCPListenConnection self = stream->data;
            $action2 f = ($action2)self->on_receive;
            f->$class->__asyn__(f, self, to$bytesD_len(buf->base, nread));
        }
    }

    // No free with GC, but maybe one day we do this explicitly?
    //if (buf->base)
    //    free(buf->base);
}

$R netQ_TCPListenConnectionD__read_startG_local (netQ_TCPListenConnection self, $Cont c$cont) {
    uv_stream_t *client = (uv_stream_t *)(intptr_t)self->client;
    client->data = self;
    int r = uv_read_start(client, alloc_buffer, netQ_TCPListenConnection__on_receive);
    if (r < 0) {
        char errmsg[1024] = "Failed to start reading from TCP socket: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)self->on_error;
        f->$class->__asyn__(f, self, to$str(errmsg));
        return $R_CONT(c$cont, B_None);
    }

    return $R_CONT(c$cont, B_None);
}

$R netQ_TCPListenConnectionD_writeG_local (netQ_TCPListenConnection self, $Cont c$cont, B_bytes data) {
    uv_stream_t *stream = (uv_stream_t *)(intptr_t)self->client;
    // fd == -1 means invalid FD and can happen after __resume__
    if ((intptr_t)stream == -1)
        return $R_CONT(c$cont, B_None);

    uv_write_t *req = (uv_write_t *)acton_malloc(sizeof(uv_write_t));
    uv_buf_t buf = uv_buf_init((char *)data->str, data->nbytes);
    int r = uv_write(req, stream, &buf, 1, NULL);
    if (r < 0) {
        char errmsg[1024] = "Failed to write to TCP socket: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)self->on_error;
        f->$class->__asyn__(f, self, to$str(errmsg));
    }
    return $R_CONT(c$cont, B_None);
}

$R netQ_TCPListenConnectionD_closeG_local (netQ_TCPListenConnection self, $Cont c$cont) {
    log_debug("Closing TCP connection, affinity=%d", self->$affinity);
    uv_stream_t *client = (uv_stream_t *)(intptr_t)self->client;
    // fd == -1 means invalid FD and can happen after __resume__ or if the socket was closed
    if ((intptr_t)client == -1)
        return $R_CONT(c$cont, B_None);

    // TODO: shouldn't we call uv_shutdown() first? uv_read_stop() is not needed I think
    uv_read_stop(client);
    if (uv_is_closing((uv_handle_t *)client) == 0) {
        uv_close((uv_handle_t *)client, NULL);
    }
    self->client = -1LL;
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

struct tls_listener_owner {
    _Atomic int refs;
    tls_context *tls;
    tlsuv_private_key_t key;
    tlsuv_certificate_t cert;
};

struct tls_listener_stream_state {
    struct tls_listener_owner *owner;
    netQ_TLSListenConnection actor;
};

struct tls_client_state {
    netQ_TLSConnection actor;
    bool connected;
    int alpn_count;
    char **alpn_protocols;
};

struct tls_write_req_state {
    char *buf;
};

static void tls_listener_release_resources(tls_context *tls,
                                           tlsuv_private_key_t key,
                                           tlsuv_certificate_t cert);
static void tls_listener_owner_release(struct tls_listener_owner *owner);
static struct tls_listener_stream_state *tls_listener_stream_state_new(struct tls_listener_owner *owner);
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
        tls_listener_owner_release(state->owner);
        free(state);
    }
    stream->data = NULL;
    free(stream);
}

static void tls_listener_release_resources(tls_context *tls,
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

static struct tls_listener_owner *tls_listener_owner_new(tls_context *tls,
                                                         tlsuv_private_key_t key,
                                                         tlsuv_certificate_t cert) {
    struct tls_listener_owner *owner = GC_malloc_uncollectable(sizeof(*owner));
    if (owner == NULL) {
        return NULL;
    }
    atomic_init(&owner->refs, 1);
    owner->tls = tls;
    owner->key = key;
    owner->cert = cert;
    return owner;
}

static void tls_listener_owner_retain(struct tls_listener_owner *owner) {
    if (owner != NULL && (intptr_t)owner != -1) {
        atomic_fetch_add(&owner->refs, 1);
    }
}

static void tls_listener_owner_release(struct tls_listener_owner *owner) {
    if (owner == NULL || (intptr_t)owner == -1) {
        return;
    }
    int prev = atomic_fetch_sub(&owner->refs, 1);
    if (prev == 1) {
        tls_listener_release_resources(owner->tls, owner->key, owner->cert);
        GC_free(owner);
    }
}

static struct tls_listener_stream_state *tls_listener_stream_state_new(struct tls_listener_owner *owner) {
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
    state->alpn_count = 0;
    state->alpn_protocols = NULL;
    return state;
}

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
    struct tls_listener_owner *owner = (struct tls_listener_owner *)(intptr_t)self->_tls_owner;
    self->_tls_owner = -1LL;
    tls_listener_owner_release(owner);
}

static void tls_listener_release_connection_owner(netQ_TLSListenConnection self) {
    struct tls_listener_owner *owner = (struct tls_listener_owner *)(intptr_t)self->tls_owner;
    self->tls_owner = -1LL;
    tls_listener_owner_release(owner);
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

static void tls_listener_on_connect(uv_connect_t *creq, int status) {
    struct tls_listener_conn *conn = (struct tls_listener_conn *)creq->data;
    if (conn == NULL) {
        return;
    }

    struct tls_listener_server_state *listener_state = conn->listener_state;
    netQ_TLSListener listener = listener_state != NULL ? listener_state->actor : NULL;
    tlsuv_stream_t *stream = conn->stream;
    struct tls_listener_stream_state *state = conn->state;
    struct tls_listener_owner *owner = state != NULL ? state->owner : NULL;

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
            f->$class->__asyn__(f, listener, to$str(errmsg));
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

    tls_listener_owner_retain(owner);
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
            B_bytes data = to$bytesD_len(buf->base, nread);
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
            on_error->$class->__asyn__(on_error, self, to$str(errmsg));
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
            on_error->$class->__asyn__(on_error, self, to$str(errmsg));
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
            f->$class->__asyn__(f, self, to$str(errmsg));
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
            f->$class->__asyn__(f, self, to$str("Failed to allocate TLS client handle"));
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
            f->$class->__asyn__(f, self, to$str(errmsg));
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
            f->$class->__asyn__(f, self, to$str(errmsg));
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
            f->$class->__asyn__(f, self, to$str(errmsg));
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
            f->$class->__asyn__(f, self, to$str(errmsg));
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
            f->$class->__asyn__(f, self, to$str(errmsg));
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
            f->$class->__asyn__(f, self, to$str(errmsg));
        }
        tls_listener_server_state_release(listener_state);
        return;
    }
    uv_os_sock_t sock = (uv_os_sock_t)dup_fd;
#endif

    struct tls_listener_owner *owner = (struct tls_listener_owner *)(intptr_t)self->_tls_owner;
    if (owner == NULL || (intptr_t)owner == -1 || owner->tls == NULL) {
        if (self->on_listen != NULL) {
            $action2 f = ($action2)self->on_listen;
            f->$class->__asyn__(f, self, to$str("TLS listener resources unavailable"));
        }
        close_uv_socket(sock);
        tls_listener_server_state_release(listener_state);
        return;
    }
    tls_listener_owner_retain(owner);
    tls_context *tls = owner->tls;
    tlsuv_stream_t *stream = (tlsuv_stream_t *)malloc(sizeof(tlsuv_stream_t));
    if (stream == NULL) {
        if (self->on_listen != NULL) {
            $action2 f = ($action2)self->on_listen;
            f->$class->__asyn__(f, self, to$str("Failed to allocate TLS listener stream"));
        }
        tls_listener_owner_release(owner);
        close_uv_socket(sock);
        tls_listener_server_state_release(listener_state);
        return;
    }
    struct tls_listener_stream_state *state = tls_listener_stream_state_new(owner);
    if (state == NULL) {
        if (self->on_listen != NULL) {
            $action2 f = ($action2)self->on_listen;
            f->$class->__asyn__(f, self, to$str("Failed to allocate TLS listener stream state"));
        }
        tls_listener_owner_release(owner);
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
            f->$class->__asyn__(f, self, to$str(errmsg));
        }
        free(state);
        tls_listener_owner_release(owner);
        free(stream);
        close_uv_socket(sock);
        tls_listener_server_state_release(listener_state);
        return;
    }
    tlsuv_stream_set_server(stream, 1);
    stream->authmode = 0;
    stream->data = state;

    struct tls_listener_conn *conn = (struct tls_listener_conn *)malloc(sizeof(*conn));
    if (conn == NULL) {
        if (self->on_listen != NULL) {
            $action2 f = ($action2)self->on_listen;
            f->$class->__asyn__(f, self, to$str("Failed to allocate TLS listener connect state"));
        }
        stream->data = NULL;
        free(state);
        tls_listener_owner_release(owner);
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
            f->$class->__asyn__(f, self, to$str("Failed to allocate TLS handshake request"));
        }
        free(conn);
        stream->data = NULL;
        free(state);
        tls_listener_owner_release(owner);
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
            f->$class->__asyn__(f, self, to$str(errmsg));
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

$R netQ_TLSListenerD__initG_local (netQ_TLSListener self, $Cont c$cont) {
    pin_actor_affinity();

    tls_context *tls = default_tls_context(NULL, 0);
    tlsuv_private_key_t key = NULL;
    tlsuv_certificate_t cert = NULL;
    struct tls_listener_owner *owner = NULL;
    uv_tcp_t *server = NULL;
    int r;
    struct sockaddr_in addr4;
    struct sockaddr_in6 addr6;
    if (tls == NULL) {
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, to$str("Failed to initialize TLS context"));
        return $R_CONT(c$cont, B_None);
    }

    int rc = tls->load_key(&key, (const char *)self->key_pem->str, self->key_pem->nbytes);
    if (rc != 0) {
        const char *err = tls->strerror(rc);
        B_str errmsg = $FORMAT("Failed to load TLS key: %s", err);
        log_warn((const char *)fromB_str(errmsg));
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, errmsg);
        tls_listener_release_resources(tls, key, cert);
        return $R_CONT(c$cont, B_None);
    }

    rc = tls->load_cert(&cert, (const char *)self->cert_pem->str, self->cert_pem->nbytes);
    if (rc != 0) {
        const char *err = tls->strerror(rc);
        B_str errmsg = $FORMAT("Failed to load TLS cert: %s", err);
        log_warn((const char *)fromB_str(errmsg));
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, errmsg);
        tls_listener_release_resources(tls, key, cert);
        return $R_CONT(c$cont, B_None);
    }

    rc = tls->set_own_cert(tls, key, cert);
    if (rc != 0) {
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, to$str("Failed to set TLS certificate"));
        tls_listener_release_resources(tls, key, cert);
        return $R_CONT(c$cont, B_None);
    }

    owner = tls_listener_owner_new(tls, key, cert);
    if (owner == NULL) {
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, to$str("Failed to allocate TLS listener owner"));
        tls_listener_release_resources(tls, key, cert);
        return $R_CONT(c$cont, B_None);
    }

    struct tls_listener_server_state *server_state = tls_listener_server_state_new(self);
    if (server_state == NULL) {
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, to$str("Failed to allocate TLS listener server state"));
        tls_listener_owner_release(owner);
        return $R_CONT(c$cont, B_None);
    }

    server = (uv_tcp_t *)malloc(sizeof(uv_tcp_t));
    if (server == NULL) {
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, to$str("Failed to allocate TLS listener server handle"));
        free(server_state);
        tls_listener_owner_release(owner);
        return $R_CONT(c$cont, B_None);
    }

    r = uv_tcp_init(get_uv_loop(), server);
    if (r != 0) {
        char errmsg[1024] = "Failed to initialize TLS listener server handle: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn("%s", errmsg);
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, to$str(errmsg));
        free(server_state);
        free(server);
        tls_listener_owner_release(owner);
        return $R_CONT(c$cont, B_None);
    }
    server->data = (void *)server_state;
    if (inet_pton(AF_INET, (const char *)fromB_str(self->address), &(addr4.sin_addr)) == 1) {
        r = uv_ip4_addr((const char *)fromB_str(self->address), (int)self->port, &addr4);
    } else if (inet_pton(AF_INET6, (const char *)fromB_str(self->address), &(addr6.sin6_addr)) == 1) {
        r = uv_ip6_addr((const char *)fromB_str(self->address), (int)self->port, &addr6);
    } else {
        B_str errmsg = $FORMAT("Address is not an IPv4 or IPv6 address: %s", fromB_str(self->address));
        log_warn((const char *)fromB_str(errmsg));
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, errmsg);
        uv_close((uv_handle_t *)server, tls_listener_server_on_close);
        tls_listener_owner_release(owner);
        return $R_CONT(c$cont, B_None);
    }
    if (r != 0) {
        char errmsg[1024] = "Unable to parse address: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, to$str(errmsg));
        uv_close((uv_handle_t *)server, tls_listener_server_on_close);
        tls_listener_owner_release(owner);
        return $R_CONT(c$cont, B_None);
    }

    if (inet_pton(AF_INET, (const char *)fromB_str(self->address), &(addr4.sin_addr)) == 1) {
        r = uv_tcp_bind(server, (const struct sockaddr *)&addr4, 0);
    } else if (inet_pton(AF_INET6, (const char *)fromB_str(self->address), &(addr6.sin6_addr)) == 1) {
        r = uv_tcp_bind(server, (const struct sockaddr *)&addr6, 0);
    }
    if (r != 0) {
        char errmsg[1024] = "Error in TLS bind: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, to$str(errmsg));
        uv_close((uv_handle_t *)server, tls_listener_server_on_close);
        tls_listener_owner_release(owner);
        return $R_CONT(c$cont, B_None);
    }

    r = uv_listen((uv_stream_t *)server, 1024, on_new_tls_connection);
    if (r != 0) {
        char errmsg[1024] = "Error in TLS listen: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, to$str(errmsg));
        uv_close((uv_handle_t *)server, tls_listener_server_on_close);
        tls_listener_owner_release(owner);
        return $R_CONT(c$cont, B_None);
    }

    self->_tls_owner = (int64_t)(intptr_t)owner;
    self->_stream = (int64_t)(intptr_t)server;
    $action2 f = ($action2)self->on_listen;
    f->$class->__asyn__(f, self, B_None);
    return $R_CONT(c$cont, B_None);
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
        f->$class->__asyn__(f, self, to$str("resume"));
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
            f->$class->__asyn__(f, self, to$str(errmsg));
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
            f->$class->__asyn__(f, self, to$str("Failed to allocate TLS listen write request"));
        }
        return $R_CONT(c$cont, B_None);
    }

    struct tls_write_req_state *write_state = tls_write_req_state_new(data);
    if (write_state == NULL) {
        if (self->on_error != NULL) {
            $action2 f = ($action2)self->on_error;
            f->$class->__asyn__(f, self, to$str("Failed to allocate TLS listen write buffer"));
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
            f->$class->__asyn__(f, self, to$str(errmsg));
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
            B_bytes data = to$bytesD_len(buf->base, nread);
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
            self->$class->_on_tls_error(self, -1LL, (int64_t)nread, to$str(errmsg));
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
            on_error->$class->__asyn__(on_error, self, to$str(errmsg));
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
            f->$class->__asyn__(f, self, to$str("TLS connection not established"));
        }
        return $R_CONT(c$cont, B_None);
    }

    uv_write_t *wreq = (uv_write_t *)malloc(sizeof(uv_write_t));
    if (wreq == NULL) {
        if (self->on_error != NULL) {
            $action2 f = ($action2)self->on_error;
            f->$class->__asyn__(f, self, to$str("Failed to allocate TLS write request"));
        }
        return $R_CONT(c$cont, B_None);
    }

    struct tls_write_req_state *write_state = tls_write_req_state_new(data);
    if (write_state == NULL) {
        if (self->on_error != NULL) {
            $action2 f = ($action2)self->on_error;
            f->$class->__asyn__(f, self, to$str("Failed to allocate TLS write buffer"));
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
            f->$class->__asyn__(f, self, to$str(errmsg));
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
            self->$class->_on_tls_error(self, -1LL, (int64_t)status, to$str(errmsg));
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
            self->$class->_on_tls_error(self, -1LL, (int64_t)r, to$str(errmsg));
        }
        free(creq);
        return;
    }

    state->connected = true;
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
    uv_connect_t* connect_req = (uv_connect_t*)calloc(1, sizeof(uv_connect_t));
    if (connect_req == NULL) {
        if (self->on_error != NULL) {
            self->$class->_on_tls_error(self, -1LL, (int64_t)UV_ENOMEM, to$str("Failed to allocate TLS connect request"));
        }
        return $R_CONT(c$cont, B_None);
    }

    //tlsuv_set_debug(5, tlsuv_logger);
    tlsuv_stream_t *stream = (tlsuv_stream_t *)malloc(sizeof(tlsuv_stream_t));
    if (stream == NULL) {
        free(connect_req);
        if (self->on_error != NULL) {
            self->$class->_on_tls_error(self, -1LL, (int64_t)UV_ENOMEM, to$str("Failed to allocate TLS stream"));
        }
        return $R_CONT(c$cont, B_None);
    }
    struct tls_client_state *state = tls_client_state_new(self);
    if (state == NULL) {
        free(stream);
        free(connect_req);
        if (self->on_error != NULL) {
            self->$class->_on_tls_error(self, -1LL, -1LL, to$str("Failed to allocate TLS client state"));
        }
        return $R_CONT(c$cont, B_None);
    }
    connect_req->data = (void *)state;
    int r = tlsuv_stream_init(get_uv_loop(), stream, NULL);
    if (r != 0) {
        char errmsg[1024] = "Failed to initialize TLS stream: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn("%s", errmsg);
        tls_client_state_free(state);
        free(stream);
        free(connect_req);
        if (self->on_error != NULL) {
            self->$class->_on_tls_error(self, -1LL, (int64_t)r, to$str(errmsg));
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
                self->$class->_on_tls_error(self, -1LL, (int64_t)UV_ENOMEM, to$str("Failed to allocate TLS ALPN protocol list"));
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
                    self->$class->_on_tls_error(self, -1LL, (int64_t)UV_ENOMEM, to$str("Failed to allocate TLS ALPN protocol"));
                }
                return $R_CONT(c$cont, B_None);
            }
        }
        tlsuv_stream_set_protocols(stream, state->alpn_count, (const char **)state->alpn_protocols);
    }
    // TODO: take SNI as input to TLSConnection actor
    stream->data = (void *)state;

    // _stream must be armed BEFORE the connect call: tlsuv may fail the
    // connect through its callback path, and tls_close run from the callback
    // resets _stream to -1 - assigning afterwards would re-arm a dangling
    // pointer to an already-torn-down stream.
    self->_stream = (int64_t)(intptr_t)stream;

    r = tlsuv_stream_connect(connect_req, stream, (const char *)fromB_str(self->address), (int)self->port, tls_on_connect);
    if (r != 0) {
        char errmsg[1024] = "Failed to start TLS TCP connect: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn("%s", errmsg);
        self->_stream = -1LL;
        stream->data = NULL;
        state->actor = NULL;
        tls_client_state_free(state);
        tlsuv_stream_close(stream, tls_on_close);
        free(connect_req);
        if (self->on_error != NULL) {
            self->$class->_on_tls_error(self, -1LL, (int64_t)r, to$str(errmsg));
        }
        return $R_CONT(c$cont, B_None);
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
