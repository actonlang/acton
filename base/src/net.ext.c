
#include <uv.h>
#include "../rts/io.h"
#include "../rts/log.h"

void netQ___ext_init__() {
    // NOP
}

struct dns_cb_data {
    struct addrinfo *hints;
    $action on_resolve;
    $action on_error;
};

void netQ_DNSD_lookup_a__on_resolve (uv_getaddrinfo_t *req, int status, struct addrinfo *dns_res) {
    struct dns_cb_data *cb_data = req->data;
    B_list $res = B_listG_new(NULL, NULL);

    if (status != 0) {
        char errmsg[1024] = "DNS lookup error: ";
        uv_strerror_r(status, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        $action f = cb_data->on_error;
        f->$class->__asyn__(f, to$str(errmsg));

        uv_freeaddrinfo(dns_res);
        free(cb_data->hints);
        free(cb_data);
        free(req);
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

    uv_freeaddrinfo(dns_res);
    free(cb_data->hints);
    free(cb_data);
    free(req);
}

$R netQ_DNSD_lookup_aG_local (netQ_DNS self, $Cont c$cont, B_str name, $action on_resolve, $action on_error) {
    struct addrinfo *hints = (struct addrinfo *)malloc(sizeof(struct addrinfo));
    hints->ai_family = PF_INET;
    hints->ai_socktype = SOCK_STREAM;
    hints->ai_protocol = IPPROTO_TCP;
    hints->ai_flags = 0;

    struct dns_cb_data *cb_data = (struct dns_cb_data *)malloc(sizeof(struct dns_cb_data));
    cb_data->hints = hints;
    cb_data->on_resolve = on_resolve;
    cb_data->on_error = on_error;

    uv_getaddrinfo_t *req = (uv_getaddrinfo_t*)malloc(sizeof(uv_getaddrinfo_t));
    req->data = cb_data;

    int r = uv_getaddrinfo(get_uv_loop(), req, netQ_DNSD_lookup_a__on_resolve, fromB_str(name), NULL, hints);
    // TODO: use on_error callback instead!
    if (r != 0)
        $RAISE(((B_BaseException)B_RuntimeErrorG_new(to$str("Unable to run DNS query"))));

    return $R_CONT(c$cont, B_None);
}

void netQ_DNSD_lookup_aaaa__on_resolve (uv_getaddrinfo_t *req, int status, struct addrinfo *dns_res) {
    struct dns_cb_data *cb_data = req->data;
    B_list $res = B_listG_new(NULL, NULL);

    if (status != 0) {
        char errmsg[1024] = "DNS lookup error: ";
        uv_strerror_r(status, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        $action f = cb_data->on_error;
        f->$class->__asyn__(f, to$str(errmsg));

        uv_freeaddrinfo(dns_res);
        free(cb_data->hints);
        free(cb_data);
        free(req);
        return;
    }

    struct addrinfo *rp;
    char addr[40] = {'\0'};
    for (rp = dns_res; rp != NULL; rp = rp->ai_next) {
        //uv_ip6_name((struct sockaddr_in6*) rp->ai_addr, addr, 39);
        uv_ip6_name((struct sockaddr_in6*)(rp->ai_addr), addr, 39);
        B_SequenceD_listG_witness->$class->append(B_SequenceD_listG_witness, $res, to$str(addr));
    }

    $action f = cb_data->on_resolve;
    f->$class->__asyn__(f, $res);

    uv_freeaddrinfo(dns_res);
    free(cb_data->hints);
    free(cb_data);
    free(req);
}

$R netQ_DNSD_lookup_aaaaG_local (netQ_DNS self, $Cont c$cont, B_str name, $action on_resolve, $action on_error) {
    struct addrinfo *hints = (struct addrinfo *)malloc(sizeof(struct addrinfo));
    hints->ai_family = PF_INET6;
    hints->ai_socktype = SOCK_STREAM;
    hints->ai_protocol = IPPROTO_TCP;
    hints->ai_flags = 0;

    struct dns_cb_data *cb_data = (struct dns_cb_data *)malloc(sizeof(struct dns_cb_data));
    cb_data->hints = hints;
    cb_data->on_resolve = on_resolve;
    cb_data->on_error = on_error;

    uv_getaddrinfo_t *req = (uv_getaddrinfo_t*)malloc(sizeof(uv_getaddrinfo_t));
    req->data = cb_data;

    int r = uv_getaddrinfo(get_uv_loop(), req, netQ_DNSD_lookup_aaaa__on_resolve, fromB_str(name), NULL, hints);
    // TODO: use on_error callback instead!
    if (r != 0)
        $RAISE(((B_BaseException)B_RuntimeErrorG_new(to$str("Unable to run DNS query"))));

    return $R_CONT(c$cont, B_None);
}

$R netQ_DNSD__pin_affinity (netQ_DNS self, $Cont c$cont) {
    pin_actor_affinity();
    return $R_CONT(c$cont, B_None);
}


void netQ_TCPIPConnection__on_receive(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buf) {
    if (nread < 0){
        if (nread == UV_EOF) {
            uv_close((uv_handle_t *)stream, NULL);
        }
    } else if (nread > 0) {
        if (stream->data) {
            netQ_TCPIPConnection self = stream->data;
            $action2 f = self->on_receive;
            f->$class->__asyn__(f, self, to$bytesD_len(buf->base, nread));
        }
    }

    if (buf->base)
        free(buf->base);
}

void on_connect(uv_connect_t *connect_req, int status) {
    netQ_TCPIPConnection self = (netQ_TCPIPConnection)connect_req->data;

    if (status != 0) {
        char errmsg[1024] = "Error in TCP connect: ";
        uv_strerror_r(status, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = self->on_error;
        f->$class->__asyn__(f, self, to$str(errmsg));
        // TODO: free()
        return;
    }

    connect_req->handle->data = self;
    int r = uv_read_start(connect_req->handle, alloc_buffer, netQ_TCPIPConnection__on_receive);
    if (r < 0) {
        char errmsg[1024] = "Failed to start reading from TCP client socket: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = self->on_error;
        f->$class->__asyn__(f, self, to$str(errmsg));
        return;
    }

    $action f = self->on_connect;
    f->$class->__asyn__(f, self);
}

$R netQ_TCPIPConnectionD__init (netQ_TCPIPConnection self, $Cont c$cont) {
    pin_actor_affinity();
    uv_tcp_t* socket = (uv_tcp_t*)malloc(sizeof(uv_tcp_t));
    uv_tcp_init(get_uv_loop(), socket);
    self->_socket = to$int((long)socket);

    uv_connect_t* connect_req = (uv_connect_t*)malloc(sizeof(uv_connect_t));
    connect_req->data = (void *)self;

    struct sockaddr_in dest;
    uv_ip4_addr(fromB_str(self->address), from$int(self->port), &dest);

    uv_tcp_connect(connect_req, socket, (const struct sockaddr*)&dest, on_connect);

    return $R_CONT(c$cont, B_None);
}

$R netQ_TCPIPConnectionD_writeG_local (netQ_TCPIPConnection self, $Cont c$cont, B_bytes data) {
    uv_stream_t *stream = (uv_stream_t *)from$int(self->_socket);
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

B_NoneType netQ_TCPIPConnectionD___resume__ (netQ_TCPIPConnection self) {
    self->_socket = to$int(-1);
    $action2 f = self->on_error;
    f->$class->__asyn__(f, self, to$str("resume"));
    return B_None;
}

void on_new_connection(uv_stream_t *server, int status) {
    netQ_TCPListener self = (netQ_TCPListener)server->data;

    if (status != 0) {
        char errmsg[1024] = "Error on new TCP client connection: ";
        uv_strerror_r(status, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = self->on_error;
        f->$class->__asyn__(f, self, to$str(errmsg));
        // TODO: free()
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
        // TODO: free()
        return;
    }

    self->$class->create_tcp_listen_connection(self, B_None, to$int((long)client));
    // TODO: free()
}


$R netQ_TCPListenerD__init (netQ_TCPListener self, $Cont c$cont) {
    pin_actor_affinity();

    uv_tcp_t *server = (uv_tcp_t *)malloc(sizeof(uv_tcp_t));
    uv_tcp_init(get_uv_loop(), server);
    server->data = (void *)self;
    int r;
    struct sockaddr_in addr;
    r = uv_ip4_addr(fromB_str(self->address), from$int(self->port), &addr);
    if (r != 0) {
        char errmsg[1024] = "Unable to parse address: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = self->on_error;
        f->$class->__asyn__(f, self, to$str(errmsg));
        // TODO: free() & return
        return $R_CONT(c$cont, B_None);
    }

    r = uv_tcp_bind(server, (const struct sockaddr*)&addr, 0);
    if (r != 0) {
        char errmsg[1024] = "Error in TCP bind: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = self->on_error;
        f->$class->__asyn__(f, self, to$str(errmsg));
        // TODO: free() & return
        return $R_CONT(c$cont, B_None);
    }

    r = uv_listen((uv_stream_t*) server, 1024, on_new_connection);
    if (r != 0) {
        char errmsg[1024] = "Error in TCP listen: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $action2 f = self->on_error;
        f->$class->__asyn__(f, self, to$str(errmsg));
        // TODO: free()
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

    if (buf->base)
        free(buf->base);
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
