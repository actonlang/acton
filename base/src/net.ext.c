#define GC_THREADS 1
#include <gc.h>

#include <uv.h>
#include "../rts/io.h"
#include "../rts/log.h"

void netQ___ext_init__() {
    // NOP
}


B_bool netQ_is_ipv4 (B_str address) {
    struct sockaddr_in sa;
    if (inet_pton(AF_INET, fromB_str(address), &(sa.sin_addr)) == 0) {
        return B_False;
    } else {
        return B_True;
    }
}

B_bool netQ_is_ipv6 (B_str address) {
    struct sockaddr_in6 sa;
    if (inet_pton(AF_INET6, fromB_str(address), &(sa.sin6_addr)) == 0) {
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
        $action2 f = cb_data->on_error;
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

    $action f = cb_data->on_resolve;
    f->$class->__asyn__(f, $res);

    // No free with GC, but maybe one day we do this explicitly?
    //uv_freeaddrinfo(dns_res);
    //free(cb_data->hints);
    //free(cb_data);
    //free(req);
}

B_NoneType netQ__lookup_a (B_str name, $action on_resolve, $action on_error) {
    struct addrinfo *hints = (struct addrinfo *)malloc(sizeof(struct addrinfo));
    hints->ai_family = PF_INET;
    hints->ai_socktype = SOCK_STREAM;
    hints->ai_protocol = IPPROTO_TCP;
    hints->ai_flags = 0;

    struct dns_cb_data *cb_data = (struct dns_cb_data *)malloc(sizeof(struct dns_cb_data));
    cb_data->hints = hints;
    cb_data->hostname = fromB_str(name);
    cb_data->on_resolve = on_resolve;
    cb_data->on_error = on_error;

    uv_getaddrinfo_t *req = (uv_getaddrinfo_t*)malloc(sizeof(uv_getaddrinfo_t));
    req->data = cb_data;

    int r = uv_getaddrinfo(get_uv_loop(), req, _lookup_a__on_resolve, fromB_str(name), NULL, hints);
    if (r != 0) {
        char errmsg[1024] = "Unable to run DNS query: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = cb_data->on_error;
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
        $action2 f = cb_data->on_error;
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

    $action f = cb_data->on_resolve;
    f->$class->__asyn__(f, $res);

    // No free with GC, but maybe one day we do this explicitly?
    //uv_freeaddrinfo(dns_res);
    //free(cb_data->hints);
    //free(cb_data);
    //free(req);
}

B_NoneType netQ__lookup_aaaa (B_str name, $action on_resolve, $action on_error) {
    struct addrinfo *hints = (struct addrinfo *)malloc(sizeof(struct addrinfo));
    hints->ai_family = PF_INET6;
    hints->ai_socktype = SOCK_STREAM;
    hints->ai_protocol = IPPROTO_TCP;
    hints->ai_flags = 0;

    struct dns_cb_data *cb_data = (struct dns_cb_data *)malloc(sizeof(struct dns_cb_data));
    cb_data->hints = hints;
    cb_data->hostname = fromB_str(name);
    cb_data->on_resolve = on_resolve;
    cb_data->on_error = on_error;

    uv_getaddrinfo_t *req = (uv_getaddrinfo_t*)malloc(sizeof(uv_getaddrinfo_t));
    req->data = cb_data;

    int r = uv_getaddrinfo(get_uv_loop(), req, _lookup_aaaa__on_resolve, fromB_str(name), NULL, hints);
    if (r != 0) {
        char errmsg[1024] = "Unable to run DNS query: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = cb_data->on_error;
        f->$class->__asyn__(f, name, to$str(errmsg));
        // NOTE: free() here if do manual memory management in I/O one day
        return B_None;
    }

    return B_None;
}

void netQ_TCPConnection__on_receive(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buf) {
    if (nread < 0){
        if (nread == UV_EOF) {
            uv_close((uv_handle_t *)stream, NULL);
        }
    } else if (nread > 0) {
        if (stream->data) {
            netQ_TCPConnection self = stream->data;
            $action2 f = self->on_receive;
            f->$class->__asyn__(f, self, to$bytesD_len(buf->base, nread));
            // TODO: count read bytes
            self->_bytes_in->val += nread;
        }
    }
}


$R netQ_TCPConnectionD__pin_affinity (netQ_TCPConnection self, $Cont c$cont) {
    pin_actor_affinity();
    return $R_CONT(c$cont, B_None);
}

void on_connect4(uv_connect_t *connect_req, int status) {
    netQ_TCPConnection self = (netQ_TCPConnection)connect_req->data;

    if (status != 0) {
        char errmsg[1024] = "Error in TCP connect over IPv4: ";
        uv_strerror_r(status, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action3 f = self->_fun_on_tcp_error;
        f->$class->__asyn__(f, to$int(4), to$int(status), to$str(errmsg));
        return;
    }
    $action f = self->_fun_oncon4;
    //$action f = self->$class->_on_connect4;
    f->$class->__asyn__(f, self);
}

void on_connect6(uv_connect_t *connect_req, int status) {
    netQ_TCPConnection self = (netQ_TCPConnection)connect_req->data;

    if (status != 0) {
        char errmsg[1024] = "Error in TCP connect over IPv6: ";
        uv_strerror_r(status, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action3 f = self->_fun_on_tcp_error;
        f->$class->__asyn__(f, to$int(6), to$int(status), to$str(errmsg));
        return;
    }
    $action f = self->_fun_oncon6;
    f->$class->__asyn__(f, self);
}

$R netQ_TCPConnectionD__connect4 (netQ_TCPConnection self, $Cont c$cont, B_str ip_address) {
    log_debug("TCP connecting over IPv4 to %s", fromB_str(ip_address));
    uv_tcp_t* socket = (uv_tcp_t*)malloc(sizeof(uv_tcp_t));
    uv_tcp_init(get_uv_loop(), socket);
    self->_sock4 = to$int((long)socket);

    uv_connect_t* connect_req = (uv_connect_t*)malloc(sizeof(uv_connect_t));
    connect_req->data = (void *)self;

    struct sockaddr_in dest;
    uv_ip4_addr(fromB_str(ip_address), from$int(self->port), &dest);

    uv_tcp_connect(connect_req, socket, (const struct sockaddr*)&dest, on_connect4);

    return $R_CONT(c$cont, B_None);
}

$R netQ_TCPConnectionD__connect6 (netQ_TCPConnection self, $Cont c$cont, B_str ip_address) {
    log_debug("TCP connecting over IPv6 to %s", fromB_str(ip_address));
    uv_tcp_t* socket = (uv_tcp_t*)malloc(sizeof(uv_tcp_t));
    uv_tcp_init(get_uv_loop(), socket);
    self->_sock6 = to$int((long)socket);

    uv_connect_t* connect_req = (uv_connect_t*)malloc(sizeof(uv_connect_t));
    connect_req->data = (void *)self;

    struct sockaddr_in6 dest;
    uv_ip6_addr(fromB_str(ip_address), from$int(self->port), &dest);

    uv_tcp_connect(connect_req, socket, (const struct sockaddr*)&dest, on_connect6);

    return $R_CONT(c$cont, B_None);
}

$R netQ_TCPConnectionD__read_start (netQ_TCPConnection self, $Cont c$cont) {
    uv_tcp_t* socket = (uv_tcp_t *)from$int(self->_sock);
    socket->data = self;
    int r = uv_read_start(socket, alloc_buffer, netQ_TCPConnection__on_receive);
    if (r < 0) {
        char errmsg[1024] = "Failed to start reading from TCP client socket: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = self->on_error;
        f->$class->__asyn__(f, self, to$str(errmsg));
        return $R_CONT(c$cont, B_None);
    }

    return $R_CONT(c$cont, B_None);
}

$R netQ_TCPConnectionD_writeG_local (netQ_TCPConnection self, $Cont c$cont, B_bytes data) {
    uv_stream_t *stream = (uv_stream_t *)from$int(self->_sock);
    // fd == -1 means invalid FD and can happen after __resume__
    if (stream == -1)
        return $R_CONT(c$cont, B_None);

    uv_write_t *req = (uv_write_t *)malloc(sizeof(uv_write_t));
    uv_buf_t buf = uv_buf_init(data->str, data->nbytes);
    int r = uv_write(req, stream, &buf, 1, NULL);
    if (r < 0) {
        char errmsg[1024] = "Failed to write to TCP socket: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = self->on_error;
        f->$class->__asyn__(f, self, to$str(errmsg));
    }
    self->_bytes_out->val += data->nbytes;
    return $R_CONT(c$cont, B_None);
}

B_NoneType netQ_TCPConnectionD___resume__ (netQ_TCPConnection self) {
    self->_sock = to$int(-1);
    self->_sock4 = to$int(-1);
    self->_sock6 = to$int(-1);
    $action2 f = self->on_error;
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
    $action on_close = cb_data->on_close;
    on_close->$class->__asyn__(on_close, cb_data->self);
}

$R netQ_TCPConnectionD_closeG_local (netQ_TCPConnection self, $Cont c$cont, $action on_close) {
    uv_stream_t *stream = (uv_stream_t *)from$int(self->_sock);
    // fd == -1 means invalid FD and can happen after __resume__
    if (stream == -1)
        return $R_CONT(c$cont, B_None);

    log_debug("Closing TCP connection");
    struct close_cb_data *cb_data = (struct close_cb_data *)malloc(sizeof(struct close_cb_data));
    cb_data->self = self;
    cb_data->on_close = on_close;
    uv_shutdown_t *req = (uv_shutdown_t *)malloc(sizeof(uv_shutdown_t));
    req->data = (void *)cb_data;
    int r = uv_shutdown(req, stream, after_shutdown);
    self->_sock = to$int(-1);
    self->_sock4 = to$int(-1);
    self->_sock6 = to$int(-1);
    if (r < 0) {
        // TODO: we could probably ignore most or all of these errors as the
        // purpose is to shutdown our side of the connection and many of these
        // errors are about "not connected" or "already closed", turning this
        // into a NOP
        char errmsg[1024] = "Failed to shutdown TCP client socket: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        on_close->$class->__asyn__(on_close, self);
        return $R_CONT(c$cont, B_None);
    }
    return $R_CONT(c$cont, B_None);
}

$R netQ_TCPConnectionD_ip_versionG_local (netQ_TCPConnection self, $Cont c$cont) {
    struct sockaddr_storage peername;
    int namelen = sizeof(peername);
    if (uv_tcp_getpeername((const uv_tcp_t *)from$int(self->_sock), (struct sockaddr*)&peername, &namelen) == 0) {
        if (peername.ss_family == AF_INET) {
            return $R_CONT(c$cont, to$int(4));
        } else if (peername.ss_family == AF_INET6) {
            return $R_CONT(c$cont, to$int(6));
        } else {
            log_error("Unhandled AFI");
        }
    } else {
        log_error("Failed to get peer name from handle %d", from$int(self->_sock));
    }
    return $R_CONT(c$cont, B_None);
}

$R netQ_TCPConnectionD_local_addressG_local (netQ_TCPConnection self, $Cont c$cont) {
    struct sockaddr_storage sockname;
    int namelen = sizeof(sockname);
    char addr[INET6_ADDRSTRLEN] = { '\0' };
    if (uv_tcp_getsockname((const uv_tcp_t *)from$int(self->_sock), (struct sockaddr*)&sockname, &namelen) == 0) {
        if (sockname.ss_family == AF_INET) {
            uv_ip4_name((struct sockaddr_in*)&sockname, addr, sizeof(addr));
        } else if (sockname.ss_family == AF_INET6) {
            uv_ip6_name((struct sockaddr_in6*)&sockname, addr, sizeof(addr));
        } else {
            log_error("Unhandled AFI");
        }
    } else {
        log_error("Failed to get sock name from handle %d", from$int(self->_sock));
    }
    return $R_CONT(c$cont, to$str(addr));
}

$R netQ_TCPConnectionD_remote_addressG_local (netQ_TCPConnection self, $Cont c$cont) {
    struct sockaddr_storage peername;
    int namelen = sizeof(peername);
    char addr[INET6_ADDRSTRLEN] = { '\0' };
    if (uv_tcp_getpeername((const uv_tcp_t *)from$int(self->_sock), (struct sockaddr*)&peername, &namelen) == 0) {
        if (peername.ss_family == AF_INET) {
            uv_ip4_name((struct sockaddr_in*)&peername, addr, sizeof(addr));
        } else if (peername.ss_family == AF_INET6) {
            uv_ip6_name((struct sockaddr_in6*)&peername, addr, sizeof(addr));
        } else {
            log_error("Unhandled AFI");
        }
    } else {
        log_error("Failed to get peer name from handle %d", from$int(self->_sock));
    }
    return $R_CONT(c$cont, to$str(addr));
}


void on_new_connection(uv_stream_t *server, int status) {
    netQ_TCPListener self = (netQ_TCPListener)server->data;

    if (status != 0) {
        char errmsg[1024] = "Error on new TCP client connection: ";
        uv_strerror_r(status, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = self->on_error;
        f->$class->__asyn__(f, self, to$str(errmsg));
        // NOTE: free() here if do manual memory management in I/O one day
        return;
    }

    uv_tcp_t *client = (uv_tcp_t*) malloc(sizeof(uv_tcp_t));
    uv_tcp_init(get_uv_loop(), client);
    int r = uv_accept(server, (uv_stream_t *)client);
    if (r != 0) {
        char errmsg[1024] = "Error in accepting TCP client connection: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = self->on_error;
        f->$class->__asyn__(f, self, to$str(errmsg));
        // NOTE: free() here if do manual memory management in I/O one day
        return;
    }

    self->$class->create_tcp_listen_connection(self, B_None, to$int((long)client));
    // NOTE: free() here if do manual memory management in I/O one day
}


$R netQ_TCPListenerD__init (netQ_TCPListener self, $Cont c$cont) {
    pin_actor_affinity();

    uv_tcp_t *server = (uv_tcp_t *)malloc(sizeof(uv_tcp_t));
    uv_tcp_init(get_uv_loop(), server);
    server->data = (void *)self;
    int r;
    struct sockaddr_in addr4;
    struct sockaddr_in6 addr6;
    if (inet_pton(AF_INET, fromB_str(self->address), &(addr4.sin_addr)) == 1) {
        r = uv_ip4_addr(fromB_str(self->address), from$int(self->port), &addr4);
    } else if (inet_pton(AF_INET6, fromB_str(self->address), &(addr6.sin6_addr)) == 1) {
        r = uv_ip6_addr(fromB_str(self->address), from$int(self->port), &addr6);
    } else {
        char errmsg[1024] = "Address is not an IPv4 or IPv6 address: ";
        asprintf(errmsg + strlen(errmsg), "%s", fromB_str(self->address));
        log_warn(errmsg);
        $action2 f = self->on_error;
        f->$class->__asyn__(f, self, to$str(errmsg));
        // NOTE: free() here if do manual memory management in I/O one day
        return $R_CONT(c$cont, B_None);
    }
    if (r != 0) {
        char errmsg[1024] = "Unable to parse address: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = self->on_error;
        f->$class->__asyn__(f, self, to$str(errmsg));
        // NOTE: free() here if do manual memory management in I/O one day
        return $R_CONT(c$cont, B_None);
    }

    if (inet_pton(AF_INET, fromB_str(self->address), &(addr4.sin_addr)) == 1) {
        r = uv_tcp_bind(server, (const struct sockaddr *)&addr4, 0);
    } else if (inet_pton(AF_INET6, fromB_str(self->address), &(addr6.sin6_addr)) == 1) {
        r = uv_tcp_bind(server, (const struct sockaddr6 *)&addr6, 0);
    }
    if (r != 0) {
        char errmsg[1024] = "Error in TCP bind: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = self->on_error;
        f->$class->__asyn__(f, self, to$str(errmsg));
        // NOTE: free() here if do manual memory management in I/O one day
        return $R_CONT(c$cont, B_None);
    }

    r = uv_listen((uv_stream_t *)server, 1024, on_new_connection);
    if (r != 0) {
        char errmsg[1024] = "Error in TCP listen: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = self->on_error;
        f->$class->__asyn__(f, self, to$str(errmsg));
        // NOTE: free() here if do manual memory management in I/O one day
        return $R_CONT(c$cont, B_None);
    }

    return $R_CONT(c$cont, B_None);
}

B_NoneType netQ_TCPListenerD___resume__ (netQ_TCPListener self) {
    self->_stream = to$int(-1);
    $action2 f = self->on_error;
    f->$class->__asyn__(f, self, to$str("resume"));
    return B_None;
}

$R netQ_TCPListenConnectionD__init (netQ_TCPListenConnection self, $Cont c$cont) {
    pin_actor_affinity();

    uv_stream_t *client = (uv_tcp_t *)from$int(self->server_client);
    int fd;
    uv_fileno((uv_handle_t *)client, &fd);
    uv_tcp_t* client_handle = (uv_tcp_t*) malloc(sizeof(uv_tcp_t));
    uv_tcp_init(get_uv_loop(), client_handle);
    uv_tcp_open(client_handle, fd);
    self->client = to$int((long)client_handle);

    return $R_CONT(c$cont, B_None);
}

void netQ_TCPListenConnection__on_receive(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buf) {
    if (nread < 0){
        if (nread == UV_EOF) {
            uv_close((uv_handle_t *)stream, NULL);
        }
    } else if (nread > 0) {
        if (stream->data) {
            netQ_TCPListenConnection self = stream->data;
            $action2 f = self->on_receive;
            f->$class->__asyn__(f, self, to$bytesD_len(buf->base, nread));
        }
    }

    // No free with GC, but maybe one day we do this explicitly?
    //if (buf->base)
    //    free(buf->base);
}

$R netQ_TCPListenConnectionD__read_start (netQ_TCPListenConnection self, $Cont c$cont) {
    uv_stream_t *client = (uv_stream_t *)from$int(self->client);
    client->data = self;
    int r = uv_read_start(client, alloc_buffer, netQ_TCPListenConnection__on_receive);
    if (r < 0) {
        char errmsg[1024] = "Failed to start reading from TCP socket: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = self->on_error;
        f->$class->__asyn__(f, self, to$str(errmsg));
        return $R_CONT(c$cont, B_None);
    }

    return $R_CONT(c$cont, B_None);
}

$R netQ_TCPListenConnectionD_writeG_local (netQ_TCPListenConnection self, $Cont c$cont, B_bytes data) {
    uv_stream_t *stream = (uv_stream_t *)from$int(self->client);
    // fd == -1 means invalid FD and can happen after __resume__
    if (stream == -1)
        return $R_CONT(c$cont, B_None);

    uv_write_t *req = (uv_write_t *)malloc(sizeof(uv_write_t));
    uv_buf_t buf = uv_buf_init(data->str, data->nbytes);
    int r = uv_write(req, stream, &buf, 1, NULL);
    if (r < 0) {
        char errmsg[1024] = "Failed to write to TCP socket: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = self->on_error;
        f->$class->__asyn__(f, self, to$str(errmsg));
    }
    return $R_CONT(c$cont, B_None);
}

$R netQ_TCPListenConnectionD_closeG_local (netQ_TCPListenConnection self, $Cont c$cont) {
    log_debug("Closing TCP connection, affinity=%d", self->$affinity);
    uv_stream_t *client = (uv_stream_t *)from$int(self->client);
    uv_read_stop(client);
    if (uv_is_closing((uv_handle_t *)client) == 0) {
        uv_close((uv_handle_t *)client, NULL);
    }
    return $R_CONT(c$cont, B_None);
}

B_NoneType netQ_TCPListenConnectionD___resume__ (netQ_TCPListenConnection self) {
    self->server_client = to$int(-1);
    self->client = to$int(-1);
    return B_None;
}

//netQ_DNSCap netQ_TCPConnectionD__dnscap (netQ_TCPConnection self) {
//    netQ_DNSCap c = netQ_DNSCapG_new(void);
//    return c;
//}
