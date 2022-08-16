
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
        cb_data->on_error->$class->__call__(cb_data->on_error, to$str(errmsg));

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

    cb_data->on_resolve->$class->__call__(cb_data->on_resolve, $res);

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
        cb_data->on_error->$class->__call__(cb_data->on_error, to$str(errmsg));

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

    cb_data->on_resolve->$class->__call__(cb_data->on_resolve, $res);

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
