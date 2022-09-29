
#include <uv.h>
#include "../rts/io.h"
#include "../rts/log.h"

void net$$__ext_init__() {
    // NOP
}

struct dns_cb_data {
    struct addrinfo *hints;
    $function on_resolve;
    $function on_error;
};

void net$$DNS$lookup_a__on_resolve (uv_getaddrinfo_t *req, int status, struct addrinfo *dns_res) {
    struct dns_cb_data *cb_data = req->data;
    $list $res = $list$new(NULL, NULL);

    if (status != 0) {
        char errmsg[1024] = "DNS lookup error: ";
        uv_strerror_r(status, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        $function1 f = cb_data->on_error;
        f->$class->__call__(f, to$str(errmsg));

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
        $Sequence$list$witness->$class->append($Sequence$list$witness, $res, to$str(addr));
    }

    $function1 f = cb_data->on_resolve;
    f->$class->__call__(f, $res);

    uv_freeaddrinfo(dns_res);
    free(cb_data->hints);
    free(cb_data);
    free(req);
}

$R net$$DNS$lookup_a$local (net$$DNS __self__, $str name, $function on_resolve, $function on_error, $Cont c$cont) {
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

    int r = uv_getaddrinfo(get_uv_loop(), req, net$$DNS$lookup_a__on_resolve, from$str(name), NULL, hints);
    // TODO: use on_error callback instead!
    if (r != 0)
        $RAISE((($BaseException)$RuntimeError$new(to$str("Unable to run DNS query"))));

    return $R_CONT(c$cont, $None);
}

void net$$DNS$lookup_aaaa__on_resolve (uv_getaddrinfo_t *req, int status, struct addrinfo *dns_res) {
    struct dns_cb_data *cb_data = req->data;
    $list $res = $list$new(NULL, NULL);

    if (status != 0) {
        char errmsg[1024] = "DNS lookup error: ";
        uv_strerror_r(status, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        $function1 f = cb_data->on_error;
        f->$class->__call__(f, to$str(errmsg));

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
        $Sequence$list$witness->$class->append($Sequence$list$witness, $res, to$str(addr));
    }

    $function1 f = cb_data->on_resolve;
    f->$class->__call__(f, $res);

    uv_freeaddrinfo(dns_res);
    free(cb_data->hints);
    free(cb_data);
    free(req);
}

$R net$$DNS$lookup_aaaa$local (net$$DNS __self__, $str name, $function on_resolve, $function on_error, $Cont c$cont) {
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

    int r = uv_getaddrinfo(get_uv_loop(), req, net$$DNS$lookup_aaaa__on_resolve, from$str(name), NULL, hints);
    // TODO: use on_error callback instead!
    if (r != 0)
        $RAISE((($BaseException)$RuntimeError$new(to$str("Unable to run DNS query"))));

    return $R_CONT(c$cont, $None);
}

$R net$$DNS$_pin_affinity (net$$DNS __self__, $Cont c$cont) {
    pin_actor_affinity();
    return $R_CONT(c$cont, $None);
}


void net$$TCPIPConnection__on_receive(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buf) {
    if (nread < 0){
        if (nread == UV_EOF) {
            uv_close((uv_handle_t *)stream, NULL);
        }
    } else if (nread > 0) {
        if (stream->data) {
            net$$TCPIPConnection __self__ = stream->data;
            $function2 f = __self__->on_receive;
            f->$class->__call__(f, __self__, to$bytes_len(buf->base, nread));
        }
    }

    if (buf->base)
        free(buf->base);
}

void on_connect(uv_connect_t *connect_req, int status) {
    net$$TCPIPConnection __self__ = (net$$TCPIPConnection)connect_req->data;

    if (status != 0) {
        char errmsg[1024] = "Error in TCP connect: ";
        uv_strerror_r(status, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $function2 f = __self__->on_error;
        f->$class->__call__(f, __self__, to$str(errmsg));
        // TODO: free()
        return;
    }

    connect_req->handle->data = __self__;
    int r = uv_read_start(connect_req->handle, alloc_buffer, net$$TCPIPConnection__on_receive);
    if (r < 0) {
        char errmsg[1024] = "Failed to start reading from TCP client socket: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $function2 f = __self__->on_error;
        f->$class->__call__(f, __self__, to$str(errmsg));
        return;
    }

    $function1 f = __self__->on_connect;
    f->$class->__call__(f, __self__);
}

$R net$$TCPIPConnection$_init (net$$TCPIPConnection __self__, $Cont c$cont) {
    pin_actor_affinity();
    uv_tcp_t* socket = (uv_tcp_t*)malloc(sizeof(uv_tcp_t));
    uv_tcp_init(get_uv_loop(), socket);
    __self__->_socket = to$int(socket);

    uv_connect_t* connect_req = (uv_connect_t*)malloc(sizeof(uv_connect_t));
    connect_req->data = (void *)__self__;

    struct sockaddr_in dest;
    uv_ip4_addr(from$str(__self__->address), from$int(__self__->port), &dest);

    uv_tcp_connect(connect_req, socket, (const struct sockaddr*)&dest, on_connect);

    return $R_CONT(c$cont, $None);
}

$R net$$TCPIPConnection$write$local (net$$TCPIPConnection __self__, $bytes data, $Cont c$cont) {
    uv_write_t *req = (uv_write_t *)malloc(sizeof(uv_write_t));
    uv_buf_t buf = uv_buf_init(data->str, data->nbytes);
    uv_stream_t *stream = (uv_stream_t *)from$int(__self__->_socket);
    int r = uv_write(req, stream, &buf, 1, NULL);
    if (r < 0) {
        char errmsg[1024] = "Failed to write to TCP socket: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $function2 f = __self__->on_error;
        f->$class->__call__(f, __self__, to$str(errmsg));
    }

    return $R_CONT(c$cont, $None);
}

$NoneType net$$TCPIPConnection$__resume__ (net$$TCPIPConnection __self__) {
    __self__->_socket = to$int(-1);
    $function2 f = __self__->on_error;
    f->$class->__call__(f, __self__, to$str("resume"));
    return $None;
}

void on_new_connection(uv_stream_t *server, int status) {
    net$$TCPListener __self__ = (net$$TCPListener)server->data;

    if (status != 0) {
        char errmsg[1024] = "Error on new TCP client connection: ";
        uv_strerror_r(status, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $function2 f = __self__->on_error;
        f->$class->__call__(f, __self__, to$str(errmsg));
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
        $function2 f = __self__->on_error;
        f->$class->__call__(f, __self__, to$str(errmsg));
        // TODO: free()
        return;
    }

    __self__->$class->create_tcp_listen_connection(__self__, $None, to$int((int *)client));
    // TODO: free()
}


$R net$$TCPListener$_init (net$$TCPListener __self__, $Cont c$cont) {
    pin_actor_affinity(($Actor)__self__);

    uv_tcp_t *server = (uv_tcp_t *)malloc(sizeof(uv_tcp_t));
    uv_tcp_init(get_uv_loop(), server);
    server->data = (void *)__self__;
    int r;
    struct sockaddr_in addr;
    r = uv_ip4_addr(from$str(__self__->address), from$int(__self__->port), &addr);
    if (r != 0) {
        char errmsg[1024] = "Unable to parse address: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $function2 f = __self__->on_listen_error;
        f->$class->__call__(f, __self__, to$str(errmsg));
        // TODO: free() & return
        return $R_CONT(c$cont, $None);
    }

    r = uv_tcp_bind(server, (const struct sockaddr*)&addr, 0);
    if (r != 0) {
        char errmsg[1024] = "Error in TCP bind: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $function2 f = __self__->on_listen_error;
        f->$class->__call__(f, __self__, to$str(errmsg));
        // TODO: free() & return
        return $R_CONT(c$cont, $None);
    }

    r = uv_listen((uv_stream_t*) server, 1024, on_new_connection);
    if (r != 0) {
        char errmsg[1024] = "Error in TCP listen: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $function2 f = __self__->on_listen_error;
        f->$class->__call__(f, __self__, to$str(errmsg));
        // TODO: free()
        return $R_CONT(c$cont, $None);
    }

    return $R_CONT(c$cont, $None);
}

$NoneType net$$TCPListener$__resume__ (net$$TCPListener __self__) {
    __self__->_stream = to$int(-1);
    $function2 f = __self__->on_listen_error;
    f->$class->__call__(f, __self__, to$str("resume"));
    return $None;
}

void net$$TCPListenConnection__on_receive(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buf) {
    if (nread < 0){
        if (nread == UV_EOF) {
            uv_close((uv_handle_t *)stream, NULL);
        }
    } else if (nread > 0) {
        if (stream->data) {
            net$$TCPListenConnection __self__ = stream->data;
            $function2 f = __self__->on_receive;
            f->$class->__call__(f, __self__, to$bytes_len(buf->base, nread));
        }
    }

    if (buf->base)
        free(buf->base);
}

$R net$$TCPListenConnection$_init (net$$TCPListenConnection __self__, $Cont c$cont) {
    uv_stream_t *client = (uv_stream_t *)from$int(__self__->client);
    client->data = __self__;
    int r = uv_read_start(client, alloc_buffer, net$$TCPListenConnection__on_receive);
    if (r < 0) {
        char errmsg[1024] = "Failed to start reading from TCP socket: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $function2 f = __self__->on_error;
        f->$class->__call__(f, __self__, to$str(errmsg));
        return $R_CONT(c$cont, $None);
    }

    return $R_CONT(c$cont, $None);
}

$R net$$TCPListenConnection$write$local (net$$TCPListenConnection __self__, $bytes data, $Cont c$cont) {
    uv_write_t *req = (uv_write_t *)malloc(sizeof(uv_write_t));
    uv_buf_t buf = uv_buf_init(data->str, data->nbytes);
    uv_stream_t *stream = (uv_stream_t *)from$int(__self__->client);
    int r = uv_write(req, stream, &buf, 1, NULL);
    if (r < 0) {
        char errmsg[1024] = "Failed to write to TCP socket: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $function2 f = __self__->on_error;
        f->$class->__call__(f, __self__, to$str(errmsg));
    }
    return $R_CONT(c$cont, $None);
}

$NoneType net$$TCPListenConnection$__resume__ (net$$TCPListenConnection __self__) {
    __self__->client = to$int(-1);
    return $None;
}
