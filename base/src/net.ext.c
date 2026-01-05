#define GC_THREADS 1
#ifdef _WIN32
#include <winsock2.h>
#endif
#include <gc.h>

#include <stdint.h>
#include <tlsuv/tlsuv.h>
#include <uv.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#ifndef _WIN32
#include <unistd.h>
#endif
#include "../rts/io.h"
#include "../rts/log.h"

void netQ___ext_init__() {
    // NOP
}


B_bool netQ_is_ipv4 (B_str address) {
    struct sockaddr_in sa;
    if (inet_pton(AF_INET, (const char *)fromB_str(address), &(sa.sin_addr)) == 0) {
        return B_False;
    } else {
        return B_True;
    }
}

B_bool netQ_is_ipv6 (B_str address) {
    struct sockaddr_in6 sa;
    if (inet_pton(AF_INET6, (const char *)fromB_str(address), &(sa.sin6_addr)) == 0) {
        return B_False;
    } else {
        return B_True;
    }
}

struct dns_cb_data {
    struct addrinfo *hints;
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

        // No free with GC, but maybe one day we do this explicitly?
        //uv_freeaddrinfo(dns_res);
        //free(cb_data->hints);
        //free(cb_data);
        //free(req);
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

    // No free with GC, but maybe one day we do this explicitly?
    //uv_freeaddrinfo(dns_res);
    //free(cb_data->hints);
    //free(cb_data);
    //free(req);
}

B_NoneType netQ__lookup_a (B_str name, $action on_resolve, $action on_error) {
    struct addrinfo *hints = (struct addrinfo *)acton_malloc(sizeof(struct addrinfo));
    hints->ai_family = PF_INET;
    hints->ai_socktype = SOCK_STREAM;
    hints->ai_protocol = IPPROTO_TCP;
    hints->ai_flags = 0;

    struct dns_cb_data *cb_data = (struct dns_cb_data *)acton_malloc(sizeof(struct dns_cb_data));
    cb_data->hints = hints;
    cb_data->hostname = (char *)fromB_str(name);
    cb_data->on_resolve = on_resolve;
    cb_data->on_error = ($action2) on_error;

    uv_getaddrinfo_t *req = (uv_getaddrinfo_t*)acton_malloc(sizeof(uv_getaddrinfo_t));
    req->data = cb_data;

    int r = uv_getaddrinfo(get_uv_loop(), req, _lookup_a__on_resolve, (const char *)fromB_str(name), NULL, hints);
    if (r != 0) {
        char errmsg[1024] = "Unable to run DNS query: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)cb_data->on_error;
        f->$class->__asyn__(f, name, to$str(errmsg));
        // NOTE: free() here if do manual memory management in I/O one day
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

        // No free with GC, but maybe one day we do this explicitly?
        //uv_freeaddrinfo(dns_res);
        //free(cb_data->hints);
        //free(cb_data);
        //free(req);
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

    // No free with GC, but maybe one day we do this explicitly?
    //uv_freeaddrinfo(dns_res);
    //free(cb_data->hints);
    //free(cb_data);
    //free(req);
}

B_NoneType netQ__lookup_aaaa (B_str name, $action on_resolve, $action on_error) {
    struct addrinfo *hints = (struct addrinfo *)acton_malloc(sizeof(struct addrinfo));
    hints->ai_family = PF_INET6;
    hints->ai_socktype = SOCK_STREAM;
    hints->ai_protocol = IPPROTO_TCP;
    hints->ai_flags = 0;

    struct dns_cb_data *cb_data = (struct dns_cb_data *)acton_malloc(sizeof(struct dns_cb_data));
    cb_data->hints = hints;
    cb_data->hostname = (char *)fromB_str(name);
    cb_data->on_resolve = on_resolve;
    cb_data->on_error = ($action2)on_error;

    uv_getaddrinfo_t *req = (uv_getaddrinfo_t*)acton_malloc(sizeof(uv_getaddrinfo_t));
    req->data = cb_data;

    int r = uv_getaddrinfo(get_uv_loop(), req, _lookup_aaaa__on_resolve, (const char *)fromB_str(name), NULL, hints);
    if (r != 0) {
        char errmsg[1024] = "Unable to run DNS query: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)cb_data->on_error;
        f->$class->__asyn__(f, name, to$str(errmsg));
        // NOTE: free() here if do manual memory management in I/O one day
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
            self->_sock = toB_int(-1);
            self->_sock4 = toB_int(-1);
            self->_sock6 = toB_int(-1);
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
            self->_bytes_in->val += nread;
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
        self->$class->_on_tcp_error(self, toB_int(4), toB_int(status), to$str(errmsg));
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
        self->$class->_on_tcp_error(self, toB_int(6), toB_int(status), to$str(errmsg));
        return;
    }
    self->$class->_on_connect6(self);
}

$R netQ_TCPConnectionD__connect4G_local (netQ_TCPConnection self, $Cont c$cont, B_str ip_address) {
    log_debug("TCP connecting over IPv4 to %s", fromB_str(ip_address));
    uv_tcp_t* socket = (uv_tcp_t*)acton_malloc(sizeof(uv_tcp_t));
    uv_tcp_init(get_uv_loop(), socket);
    self->_sock4 = toB_int((int64_t)(intptr_t)socket);

    uv_connect_t* connect_req = (uv_connect_t*)acton_malloc(sizeof(uv_connect_t));
    connect_req->data = (void *)self;

    struct sockaddr_in dest;
    uv_ip4_addr((const char *)fromB_str(ip_address), fromB_int(self->port), &dest);

    uv_tcp_connect(connect_req, socket, (const struct sockaddr*)&dest, on_connect4);

    return $R_CONT(c$cont, B_None);
}

$R netQ_TCPConnectionD__connect6G_local (netQ_TCPConnection self, $Cont c$cont, B_str ip_address) {
    log_debug("TCP connecting over IPv6 to %s", fromB_str(ip_address));
    uv_tcp_t* socket = (uv_tcp_t*)acton_malloc(sizeof(uv_tcp_t));
    uv_tcp_init(get_uv_loop(), socket);
    self->_sock6 = toB_int((int64_t)(intptr_t)socket);

    uv_connect_t* connect_req = (uv_connect_t*)acton_malloc(sizeof(uv_connect_t));
    connect_req->data = (void *)self;

    struct sockaddr_in6 dest;
    uv_ip6_addr((const char *)fromB_str(ip_address), fromB_int(self->port), &dest);

    uv_tcp_connect(connect_req, socket, (const struct sockaddr*)&dest, on_connect6);

    return $R_CONT(c$cont, B_None);
}

$R netQ_TCPConnectionD__read_startG_local (netQ_TCPConnection self, $Cont c$cont) {
    uv_tcp_t* socket = (uv_tcp_t *)fromB_int(self->_sock);
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
    uv_stream_t *stream = (uv_stream_t *)fromB_int(self->_sock);
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
    self->_bytes_out->val += data->nbytes;
    return $R_CONT(c$cont, B_None);
}

B_NoneType netQ_TCPConnectionD___resume__ (netQ_TCPConnection self) {
    self->_sock = toB_int(-1);
    self->_sock4 = toB_int(-1);
    self->_sock6 = toB_int(-1);
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
    uv_stream_t *stream = (uv_stream_t *)fromB_int(self->_sock);
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
    self->_sock = toB_int(-1);
    self->_sock4 = toB_int(-1);
    self->_sock6 = toB_int(-1);
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
    if (uv_tcp_getpeername((const uv_tcp_t *)fromB_int(self->_sock), (struct sockaddr*)&peername, &namelen) == 0) {
        if (peername.ss_family == AF_INET) {
            return $R_CONT(c$cont, toB_int(4));
        } else if (peername.ss_family == AF_INET6) {
            return $R_CONT(c$cont, toB_int(6));
        } else {
            log_error("Unhandled AFI");
        }
    } else {
        log_error("Failed to get peer name from handle %d", fromB_int(self->_sock));
    }
    return $R_CONT(c$cont, B_None);
}

$R netQ_TCPConnectionD_local_addressG_local (netQ_TCPConnection self, $Cont c$cont) {
    struct sockaddr_storage sockname;
    int namelen = sizeof(sockname);
    char addr[INET6_ADDRSTRLEN] = { '\0' };
    if (uv_tcp_getsockname((const uv_tcp_t *)fromB_int(self->_sock), (struct sockaddr*)&sockname, &namelen) == 0) {
        if (sockname.ss_family == AF_INET) {
            uv_ip4_name((struct sockaddr_in*)&sockname, addr, sizeof(addr));
        } else if (sockname.ss_family == AF_INET6) {
            uv_ip6_name((struct sockaddr_in6*)&sockname, addr, sizeof(addr));
        } else {
            log_error("Unhandled AFI");
        }
    } else {
        log_error("Failed to get sock name from handle %d", fromB_int(self->_sock));
    }
    return $R_CONT(c$cont, to$str(addr));
}

$R netQ_TCPConnectionD_remote_addressG_local (netQ_TCPConnection self, $Cont c$cont) {
    struct sockaddr_storage peername;
    int namelen = sizeof(peername);
    char addr[INET6_ADDRSTRLEN] = { '\0' };
    if (uv_tcp_getpeername((const uv_tcp_t *)fromB_int(self->_sock), (struct sockaddr*)&peername, &namelen) == 0) {
        if (peername.ss_family == AF_INET) {
            uv_ip4_name((struct sockaddr_in*)&peername, addr, sizeof(addr));
        } else if (peername.ss_family == AF_INET6) {
            uv_ip6_name((struct sockaddr_in6*)&peername, addr, sizeof(addr));
        } else {
            log_error("Unhandled AFI");
        }
    } else {
        log_error("Failed to get peer name from handle %d", fromB_int(self->_sock));
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

    self->$class->create_tcp_listen_connection(self, B_None, toB_int((int64_t)(intptr_t)client));
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
        r = uv_ip4_addr((const char *)fromB_str(self->address), fromB_int(self->port), &addr4);
    } else if (inet_pton(AF_INET6, (const char *)fromB_str(self->address), &(addr6.sin6_addr)) == 1) {
        r = uv_ip6_addr((const char *)fromB_str(self->address), fromB_int(self->port), &addr6);
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
    self->_stream = toB_int(-1);
    $action2 f = ($action2)self->on_listen;
    f->$class->__asyn__(f, self, to$str("resume"));
    return B_None;
}

$R netQ_TCPListenConnectionD__initG_local (netQ_TCPListenConnection self, $Cont c$cont) {
    pin_actor_affinity();

    uv_stream_t *client = (uv_stream_t *)fromB_int(self->server_client);
    uv_os_fd_t fd;
    uv_fileno((uv_handle_t *)client, &fd);
    uv_tcp_t* client_handle = (uv_tcp_t*)acton_malloc(sizeof(uv_tcp_t));
    uv_tcp_init(get_uv_loop(), client_handle);
    uv_tcp_open(client_handle, (uv_os_sock_t)fd);
    self->client = toB_int((int64_t)(intptr_t)client_handle);

    return $R_CONT(c$cont, B_None);
}

void netQ_TCPListenConnection__on_receive(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buf) {
    if (nread < 0){
        if (nread == UV_EOF) {
            netQ_TCPListenConnection self = stream->data;
            if ((intptr_t)stream != -1)
                uv_close((uv_handle_t *)stream, NULL);
            self->client = toB_int(-1);
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
    uv_stream_t *client = (uv_stream_t *)fromB_int(self->client);
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
    uv_stream_t *stream = (uv_stream_t *)fromB_int(self->client);
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
    uv_stream_t *client = (uv_stream_t *)fromB_int(self->client);
    // fd == -1 means invalid FD and can happen after __resume__ or if the socket was closed
    if ((intptr_t)client == -1)
        return $R_CONT(c$cont, B_None);

    // TODO: shouldn't we call uv_shutdown() first? uv_read_stop() is not needed I think
    uv_read_stop(client);
    if (uv_is_closing((uv_handle_t *)client) == 0) {
        uv_close((uv_handle_t *)client, NULL);
    }
    self->client = toB_int(-1);
    return $R_CONT(c$cont, B_None);
}

B_NoneType netQ_TCPListenConnectionD___resume__ (netQ_TCPListenConnection self) {
    self->server_client = toB_int(-1);
    self->client = toB_int(-1);
    return B_None;
}

struct tls_listener_conn {
    netQ_TLSListener listener;
    tlsuv_stream_t *stream;
};

static void tls_listener_on_close(uv_handle_t *stream) {
    log_debug("TLS listener handle closed, stream: %p  stream->data: %p", stream, stream->data);
}

static void tls_listener_close_stream(tlsuv_stream_t *stream) {
    tlsuv_stream_close(stream, tls_listener_on_close);
}

static void tls_listener_on_connect(uv_connect_t *creq, int status) {
    struct tls_listener_conn *conn = (struct tls_listener_conn *)creq->data;
    netQ_TLSListener listener = conn->listener;
    tlsuv_stream_t *stream = conn->stream;

    if (status != 0) {
        char errmsg[1024] = "TLS handshake failed: ";
        uv_strerror_r(status, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)listener->on_listen;
        f->$class->__asyn__(f, listener, to$str(errmsg));
        tls_listener_close_stream(stream);
        return;
    }

    listener->$class->create_tls_listen_connection(listener, B_None, toB_int((int64_t)(intptr_t)stream));
}

static void tls_listener_on_receive(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buf) {
    netQ_TLSListenConnection self = ((tlsuv_stream_t *)stream)->data;
    if (nread > 0) {
        if (stream->data) {
            $action2 f = ($action2)self->on_receive;
            B_bytes data = to$bytesD_len(buf->base, nread);
            f->$class->__asyn__(f, self, data);
        }
    } else if (nread == UV_EOF) {
        log_debug("TLS listen connection closed %p", stream);
        tls_listener_close_stream((tlsuv_stream_t *)stream);
        self->_stream = toB_int(-1);
        if (self->on_remote_close) {
            $action f = ($action)self->on_remote_close;
            f->$class->__asyn__(f, self);
        }
    } else if (nread < 0) {
        log_debug("TLS listen read error %ld: %s", nread, uv_strerror((int)nread));
        tls_listener_close_stream((tlsuv_stream_t *)stream);
        if (stream->data) {
            self->_stream = toB_int(-1);
        }
    }
}

static void tls_listener_write_cb(uv_write_t *wreq, int status) {
    if (status < 0) {
        char errmsg[1024] = "Failed to write to TLS listen socket: ";
        uv_strerror_r(status, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        netQ_TLSListenConnection self = (netQ_TLSListenConnection)wreq->data;
        $action2 on_error = ($action2)self->on_error;
        on_error->$class->__asyn__(on_error, self, to$str(errmsg));
        tls_listener_close_stream((tlsuv_stream_t *)wreq->handle);
    }
}

static void on_new_tls_connection(uv_stream_t *server, int status) {
    netQ_TLSListener self = (netQ_TLSListener)server->data;

    if (status != 0) {
        char errmsg[1024] = "Error on new TLS client connection: ";
        uv_strerror_r(status, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, to$str(errmsg));
        return;
    }

    uv_tcp_t *client = (uv_tcp_t *)acton_malloc(sizeof(uv_tcp_t));
    uv_tcp_init(get_uv_loop(), client);
    int r = uv_accept(server, (uv_stream_t *)client);
    if (r != 0) {
        char errmsg[1024] = "Error in accepting TLS client connection: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, to$str(errmsg));
        return;
    }

    uv_os_fd_t fd;
    r = uv_fileno((uv_handle_t *)client, &fd);
    if (r != 0) {
        char errmsg[1024] = "Error getting TLS client socket fd: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, to$str(errmsg));
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
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, to$str(errmsg));
        uv_close((uv_handle_t *)client, NULL);
        return;
    }
    SOCKET dup_sock = WSASocket(FROM_PROTOCOL_INFO, FROM_PROTOCOL_INFO, FROM_PROTOCOL_INFO,
                                &dup_info, 0, WSA_FLAG_OVERLAPPED);
    uv_close((uv_handle_t *)client, NULL);
    if (dup_sock == INVALID_SOCKET) {
        char errmsg[1024] = "Error duplicating TLS client socket: ";
        snprintf(errmsg + strlen(errmsg), sizeof(errmsg) - strlen(errmsg), "WSA error %d", WSAGetLastError());
        log_warn(errmsg);
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, to$str(errmsg));
        return;
    }
    uv_os_sock_t sock = (uv_os_sock_t)dup_sock;
#else
    int dup_fd = dup((int)fd);
    uv_close((uv_handle_t *)client, NULL);
    if (dup_fd < 0) {
        char errmsg[1024] = "Error duplicating TLS client socket: ";
        snprintf(errmsg + strlen(errmsg), sizeof(errmsg) - strlen(errmsg), "%s", strerror(errno));
        log_warn(errmsg);
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, to$str(errmsg));
        return;
    }
    uv_os_sock_t sock = (uv_os_sock_t)dup_fd;
#endif

    tls_context *tls = (tls_context *)fromB_int(self->_tls_ctx);
    tlsuv_stream_t *stream = (tlsuv_stream_t *)acton_malloc(sizeof(tlsuv_stream_t));
    tlsuv_stream_init(get_uv_loop(), stream, tls);
    tlsuv_stream_set_server(stream, 1);
    stream->authmode = 0;

    struct tls_listener_conn *conn = (struct tls_listener_conn *)acton_malloc(sizeof(*conn));
    conn->listener = self;
    conn->stream = stream;

    uv_connect_t *connect_req = (uv_connect_t *)acton_calloc(1, sizeof(uv_connect_t));
    connect_req->data = conn;

    r = tlsuv_stream_open(connect_req, stream, sock, tls_listener_on_connect);
    if (r != 0) {
        char errmsg[1024] = "Failed to start TLS handshake: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, to$str(errmsg));
        tls_listener_close_stream(stream);
        return;
    }
}

$R netQ_TLSListenerD__initG_local (netQ_TLSListener self, $Cont c$cont) {
    pin_actor_affinity();

    tls_context *tls = default_tls_context(NULL, 0);
    if (tls == NULL) {
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, to$str("Failed to initialize TLS context"));
        return $R_CONT(c$cont, B_None);
    }

    tlsuv_private_key_t key = NULL;
    tlsuv_certificate_t cert = NULL;
    int rc = tls->load_key(&key, (const char *)self->key_pem->str, self->key_pem->nbytes);
    if (rc != 0) {
        const char *err = tls->strerror(rc);
        B_str errmsg = $FORMAT("Failed to load TLS key: %s", err);
        log_warn((const char *)fromB_str(errmsg));
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, errmsg);
        return $R_CONT(c$cont, B_None);
    }

    rc = tls->load_cert(&cert, (const char *)self->cert_pem->str, self->cert_pem->nbytes);
    if (rc != 0) {
        const char *err = tls->strerror(rc);
        B_str errmsg = $FORMAT("Failed to load TLS cert: %s", err);
        log_warn((const char *)fromB_str(errmsg));
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, errmsg);
        return $R_CONT(c$cont, B_None);
    }

    rc = tls->set_own_cert(tls, key, cert);
    if (rc != 0) {
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, to$str("Failed to set TLS certificate"));
        return $R_CONT(c$cont, B_None);
    }

    self->_tls_ctx = toB_int((int64_t)(intptr_t)tls);

    uv_tcp_t *server = (uv_tcp_t *)acton_malloc(sizeof(uv_tcp_t));
    uv_tcp_init(get_uv_loop(), server);
    server->data = (void *)self;
    int r;
    struct sockaddr_in addr4;
    struct sockaddr_in6 addr6;
    if (inet_pton(AF_INET, (const char *)fromB_str(self->address), &(addr4.sin_addr)) == 1) {
        r = uv_ip4_addr((const char *)fromB_str(self->address), fromB_int(self->port), &addr4);
    } else if (inet_pton(AF_INET6, (const char *)fromB_str(self->address), &(addr6.sin6_addr)) == 1) {
        r = uv_ip6_addr((const char *)fromB_str(self->address), fromB_int(self->port), &addr6);
    } else {
        B_str errmsg = $FORMAT("Address is not an IPv4 or IPv6 address: %s", fromB_str(self->address));
        log_warn((const char *)fromB_str(errmsg));
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, errmsg);
        return $R_CONT(c$cont, B_None);
    }
    if (r != 0) {
        char errmsg[1024] = "Unable to parse address: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, to$str(errmsg));
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
        return $R_CONT(c$cont, B_None);
    }

    r = uv_listen((uv_stream_t *)server, 1024, on_new_tls_connection);
    if (r != 0) {
        char errmsg[1024] = "Error in TLS listen: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)self->on_listen;
        f->$class->__asyn__(f, self, to$str(errmsg));
        return $R_CONT(c$cont, B_None);
    }

    self->_stream = toB_int((int64_t)(intptr_t)server);
    $action2 f = ($action2)self->on_listen;
    f->$class->__asyn__(f, self, B_None);
    return $R_CONT(c$cont, B_None);
}

B_NoneType netQ_TLSListenerD___resume__ (netQ_TLSListener self) {
    self->_stream = toB_int(-1);
    self->_tls_ctx = toB_int(-1);
    $action2 f = ($action2)self->on_listen;
    f->$class->__asyn__(f, self, to$str("resume"));
    return B_None;
}

$R netQ_TLSListenConnectionD__initG_local (netQ_TLSListenConnection self, $Cont c$cont) {
    pin_actor_affinity();

    tlsuv_stream_t *stream = (tlsuv_stream_t *)fromB_int(self->server_stream);
    stream->data = self;
    self->_stream = toB_int((int64_t)(intptr_t)stream);

    return $R_CONT(c$cont, B_None);
}

$R netQ_TLSListenConnectionD__read_startG_local (netQ_TLSListenConnection self, $Cont c$cont) {
    tlsuv_stream_t *stream = (tlsuv_stream_t *)fromB_int(self->_stream);
    if ((intptr_t)stream == -1)
        return $R_CONT(c$cont, B_None);

    int r = tlsuv_stream_read_start(stream, alloc_buffer, tls_listener_on_receive);
    if (r < 0) {
        char errmsg[1024] = "Failed to start reading from TLS listen socket: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)self->on_error;
        f->$class->__asyn__(f, self, to$str(errmsg));
        return $R_CONT(c$cont, B_None);
    }

    return $R_CONT(c$cont, B_None);
}

$R netQ_TLSListenConnectionD_writeG_local (netQ_TLSListenConnection self, $Cont c$cont, B_bytes data) {
    tlsuv_stream_t *stream = (tlsuv_stream_t *)fromB_int(self->_stream);
    if ((intptr_t)stream == -1)
        return $R_CONT(c$cont, B_None);

    uv_write_t *wreq = (uv_write_t *)acton_malloc(sizeof(uv_write_t));
    wreq->data = self;
    uv_buf_t buf = uv_buf_init((char *)data->str, data->nbytes);
    int r = tlsuv_stream_write(wreq, stream, &buf, tls_listener_write_cb);
    if (r < 0) {
        char errmsg[1024] = "Failed to write to TLS listen socket: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = ($action2)self->on_error;
        f->$class->__asyn__(f, self, to$str(errmsg));
    }
    return $R_CONT(c$cont, B_None);
}

$R netQ_TLSListenConnectionD_closeG_local (netQ_TLSListenConnection self, $Cont c$cont) {
    tlsuv_stream_t *stream = (tlsuv_stream_t *)fromB_int(self->_stream);
    if ((intptr_t)stream == -1)
        return $R_CONT(c$cont, B_None);

    tls_listener_close_stream(stream);
    self->_stream = toB_int(-1);
    return $R_CONT(c$cont, B_None);
}

B_NoneType netQ_TLSListenConnectionD___resume__ (netQ_TLSListenConnection self) {
    self->server_stream = toB_int(-1);
    self->_stream = toB_int(-1);
    return B_None;
}

//netQ_DNSCap netQ_TCPConnectionD__dnscap (netQ_TCPConnection self) {
//    netQ_DNSCap c = netQ_DNSCapG_new(void);
//    return c;
//}

static void tls_on_close(uv_handle_t* stream) {
    log_debug("TLS handle closed, stream: %p  stream->data: %p", stream, stream->data);
}

static void tls_close(tlsuv_stream_t *stream) {
    log_debug("Closing TLS stream: %p  stream->data: %p", stream, stream->data);
    netQ_TLSConnection self = (netQ_TLSConnection)stream->data;
    $action on_close = self->_on_close;
    if (on_close)
        on_close->$class->__asyn__(on_close, self);
    tlsuv_stream_close(stream, tls_on_close);
    self->_stream = toB_int(-1);
}

void tls_on_receive(uv_stream_t *stream, ssize_t nread, const uv_buf_t* buf) {
    netQ_TLSConnection self = ((tlsuv_stream_t *)stream)->data;
    if (nread > 0) {
        if (stream->data) {
            $action2 f = ($action2)self->on_receive;
            B_bytes data = to$bytesD_len(buf->base, nread);
            f->$class->__asyn__(f, self, data);
            self->_bytes_in->val += nread;
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
        log_debug("TLS read error %ld: %s", nread, uv_strerror((int) nread));
        tls_close((tlsuv_stream_t *)stream);
    }
}

void tls_write_cb(uv_write_t *wreq, int status) {
    if (status < 0) {
        log_debug("TLS write error %d: %s", status, uv_strerror(status));
        char errmsg[1024] = "Failed to write to TLS TCP socket: ";
        uv_strerror_r(status, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_debug(errmsg);
        netQ_TLSConnection self = (netQ_TLSConnection)wreq->data;
        $action2 on_error = ($action2)self->on_error;
        on_error->$class->__asyn__(on_error, self, to$str(errmsg));
        tls_close((tlsuv_stream_t *)wreq->handle);
    }
}

$R netQ_TLSConnectionD_closeG_local (netQ_TLSConnection self, $Cont c$cont, $action on_close) {
    uv_stream_t *stream = (uv_stream_t *)fromB_int(self->_stream);
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
    tlsuv_stream_t *stream = (tlsuv_stream_t *)fromB_int(self->_stream);
    // fd == -1 means invalid FD and can happen after __resume__
    if ((intptr_t)stream == -1)
        return $R_CONT(c$cont, B_None);

    uv_write_t *wreq = (uv_write_t *)acton_malloc(sizeof(uv_write_t));
    wreq->data = self;
    uv_buf_t buf = uv_buf_init((char *)data->str, data->nbytes);
    int r = tlsuv_stream_write(wreq, stream, &buf, tls_write_cb);
    if (r < 0) {
        char errmsg[1024] = "Failed to write to TLS TCP socket: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_debug(errmsg);
        $action2 f = ($action2)self->on_error;
        f->$class->__asyn__(f, self, to$str(errmsg));
    }
    self->_bytes_out->val += data->nbytes;
    return $R_CONT(c$cont, B_None);
}


static void tls_on_connect(uv_connect_t *creq, int status) {
    netQ_TLSConnection self = (netQ_TLSConnection)creq->data;
    if (status != 0) {
        char errmsg[1024] = "Error in TLS TCP connect: ";
        uv_strerror_r(status, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_debug(errmsg);
        tls_close((tlsuv_stream_t *)creq->handle);
        self->$class->_on_tls_error(self, toB_int(-1), toB_int(status), to$str(errmsg));
        return;
    }

    tlsuv_stream_t *stream = (tlsuv_stream_t *) creq->handle;
    tlsuv_stream_read_start(stream, alloc_buffer, tls_on_receive);

    self->$class->_on_tls_connect(self);
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
    uv_connect_t* connect_req = (uv_connect_t*)acton_calloc(1, sizeof(uv_connect_t));
    connect_req->data = (void *)self;

    //tlsuv_set_debug(5, tlsuv_logger);
    tlsuv_stream_t *stream = (tlsuv_stream_t *)acton_malloc(sizeof(tlsuv_stream_t));
    tlsuv_stream_init(get_uv_loop(), stream, NULL);

    // Default is to verify TLS certificate. Should we disable verification?
    if (fromB_bool(self->verify_tls) == false) {
        log_debug("TLS certificate verification disabled");
        stream->authmode = 0; // 0=none, 2=require (mbedtls specific)
    }

    // TODO: take ALPN as input to TLSConnection actor
    //const char *alpn[] = { "http/1.1" };
    int num_protocols = self->protocols->length;

    if (num_protocols > 0) {
        const char **protocols = (const char **)acton_calloc(num_protocols, sizeof(char*));

        for(int i = 0; i < num_protocols; ++i) {
            protocols[i] = (const char *)fromB_str(self->protocols->data[i]);
        }
        tlsuv_stream_set_protocols(stream, num_protocols, protocols);
    }
    //tlsuv_stream_set_protocols(stream, 1, alpn);
    // No ALPN for now.
    // TODO: take SNI as input to TLSConnection actor
    stream->data = (void *)self;

    tlsuv_stream_connect(connect_req, stream, (const char *)fromB_str(self->address), fromB_int(self->port), tls_on_connect);
    self->_stream = toB_int((int64_t)(intptr_t)stream);

    return $R_CONT(c$cont, B_None);
}

$R netQ_TLSConnectionD__pin_affinityG_local (netQ_TLSConnection self, $Cont c$cont) {
    pin_actor_affinity();
    return $R_CONT(c$cont, B_None);
}
