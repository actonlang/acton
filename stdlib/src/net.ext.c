//#include "net.ext.h"

#include <uv.h>
#include "../rts/log.h"

struct dns_cb_data {
    struct addrinfo *hints;
    $function on_resolve;
    $function on_error;
};

void net$$DNS$lookup_a__on_resolve (uv_getaddrinfo_t *req, int status, struct addrinfo *dns_res) {
    struct dns_cb_data *cb_data = req->data;
    $list $res = $list$new(NULL, NULL);

    if (status == -1) {
        cb_data->on_error->$class->__call__(cb_data->on_error, to$str("Error during DNS lookup"));

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

$NoneType net$$DNS$lookup_a (net$$DNS __self__, $str name, $function on_resolve, $function on_error) {
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

    int r = uv_getaddrinfo(uv_default_loop(), req, net$$DNS$lookup_a__on_resolve, from$str(name), NULL, hints);
    if (r != 0)
        $RAISE((($BaseException)$RuntimeError$new(to$str("Unable to run DNS query"))));

    return $None;
}

void net$$DNS$lookup_aaaa__on_resolve (uv_getaddrinfo_t *req, int status, struct addrinfo *dns_res) {
    struct dns_cb_data *cb_data = req->data;
    $list $res = $list$new(NULL, NULL);

    if (status == -1) {
        cb_data->on_error->$class->__call__(cb_data->on_error, to$str("Error during DNS lookup"));

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

$NoneType net$$DNS$lookup_aaaa (net$$DNS __self__, $str name, $function on_resolve, $function on_error) {
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

    int r = uv_getaddrinfo(uv_default_loop(), req, net$$DNS$lookup_aaaa__on_resolve, from$str(name), NULL, hints);
    if (r != 0)
        $RAISE((($BaseException)$RuntimeError$new(to$str("Unable to run DNS query"))));

    return $None;
}
