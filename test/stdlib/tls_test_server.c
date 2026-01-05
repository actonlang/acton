#include "mbedtls/build_info.h"
#include "mbedtls/platform.h"

#if !defined(MBEDTLS_BIGNUM_C) || !defined(MBEDTLS_PEM_PARSE_C) || \
    !defined(MBEDTLS_ENTROPY_C) || !defined(MBEDTLS_SSL_TLS_C) ||  \
    !defined(MBEDTLS_SSL_SRV_C) || !defined(MBEDTLS_NET_C) ||      \
    !defined(MBEDTLS_RSA_C) || !defined(MBEDTLS_CTR_DRBG_C) ||     \
    !defined(MBEDTLS_X509_CRT_PARSE_C) || !defined(MBEDTLS_FS_IO)
int main(void) {
    mbedtls_printf("Missing required mbedtls config options.\n");
    return 0;
}
#else

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(_WIN32)
#include <winsock2.h>
#include <ws2tcpip.h>
#else
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <unistd.h>
#endif

#include "mbedtls/ctr_drbg.h"
#include "mbedtls/entropy.h"
#include "mbedtls/error.h"
#include "mbedtls/net_sockets.h"
#include "mbedtls/ssl.h"
#include "mbedtls/x509.h"
#include "test/certs.h"

static int get_bound_port(mbedtls_net_context *ctx) {
    struct sockaddr_storage addr;
    socklen_t addr_len = sizeof(addr);
    if (getsockname(ctx->fd, (struct sockaddr *) &addr, &addr_len) != 0) {
        return -1;
    }
    if (addr.ss_family == AF_INET) {
        struct sockaddr_in *in = (struct sockaddr_in *) &addr;
        return (int) ntohs(in->sin_port);
    }
    if (addr.ss_family == AF_INET6) {
        struct sockaddr_in6 *in6 = (struct sockaddr_in6 *) &addr;
        return (int) ntohs(in6->sin6_port);
    }
    return -1;
}

static void print_mbedtls_error(const char *label, int ret) {
    char buf[128];
    mbedtls_strerror(ret, buf, sizeof(buf));
    fprintf(stderr, "%s: -0x%x (%s)\n", label, (unsigned int) -ret, buf);
}

static int parse_int_arg(const char *arg, int *out) {
    char *end = NULL;
    long v = strtol(arg, &end, 10);
    if (end == NULL || *end != '\0') {
        return -1;
    }
    *out = (int) v;
    return 0;
}

int main(int argc, char **argv) {
    int ret = 0;
    int port = 0;
    int connections = 0;
    char port_str[16];

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--port") == 0 && i + 1 < argc) {
            if (parse_int_arg(argv[++i], &port) != 0) {
                fprintf(stderr, "Invalid --port value\n");
                return 1;
            }
        } else if (strcmp(argv[i], "--connections") == 0 && i + 1 < argc) {
            if (parse_int_arg(argv[++i], &connections) != 0) {
                fprintf(stderr, "Invalid --connections value\n");
                return 1;
            }
        } else {
            fprintf(stderr, "Unknown argument: %s\n", argv[i]);
            return 1;
        }
    }

    snprintf(port_str, sizeof(port_str), "%d", port);

    mbedtls_net_context listen_fd;
    mbedtls_net_context client_fd;
    mbedtls_entropy_context entropy;
    mbedtls_ctr_drbg_context ctr_drbg;
    mbedtls_ssl_context ssl;
    mbedtls_ssl_config conf;
    mbedtls_x509_crt srvcert;
    mbedtls_pk_context pkey;

    const char *pers = "acton_tls_test_server";

    mbedtls_net_init(&listen_fd);
    mbedtls_net_init(&client_fd);
    mbedtls_ssl_init(&ssl);
    mbedtls_ssl_config_init(&conf);
    mbedtls_x509_crt_init(&srvcert);
    mbedtls_pk_init(&pkey);
    mbedtls_entropy_init(&entropy);
    mbedtls_ctr_drbg_init(&ctr_drbg);

    ret = mbedtls_ctr_drbg_seed(&ctr_drbg, mbedtls_entropy_func, &entropy,
                                (const unsigned char *) pers,
                                strlen(pers));
    if (ret != 0) {
        print_mbedtls_error("ctr_drbg_seed", ret);
        goto exit;
    }

    ret = mbedtls_x509_crt_parse(&srvcert,
                                 (const unsigned char *) mbedtls_test_srv_crt,
                                 mbedtls_test_srv_crt_len);
    if (ret != 0) {
        print_mbedtls_error("x509_crt_parse", ret);
        goto exit;
    }

    ret = mbedtls_x509_crt_parse(&srvcert,
                                 (const unsigned char *) mbedtls_test_cas_pem,
                                 mbedtls_test_cas_pem_len);
    if (ret != 0) {
        print_mbedtls_error("x509_crt_parse ca", ret);
        goto exit;
    }

    ret = mbedtls_pk_parse_key(&pkey,
                               (const unsigned char *) mbedtls_test_srv_key,
                               mbedtls_test_srv_key_len,
                               NULL,
                               0,
                               mbedtls_ctr_drbg_random,
                               &ctr_drbg);
    if (ret != 0) {
        print_mbedtls_error("pk_parse_key", ret);
        goto exit;
    }

    ret = mbedtls_net_bind(&listen_fd, "127.0.0.1", port_str, MBEDTLS_NET_PROTO_TCP);
    if (ret != 0) {
        print_mbedtls_error("net_bind", ret);
        goto exit;
    }

    ret = mbedtls_ssl_config_defaults(&conf,
                                      MBEDTLS_SSL_IS_SERVER,
                                      MBEDTLS_SSL_TRANSPORT_STREAM,
                                      MBEDTLS_SSL_PRESET_DEFAULT);
    if (ret != 0) {
        print_mbedtls_error("ssl_config_defaults", ret);
        goto exit;
    }

    mbedtls_ssl_conf_authmode(&conf, MBEDTLS_SSL_VERIFY_NONE);
    mbedtls_ssl_conf_rng(&conf, mbedtls_ctr_drbg_random, &ctr_drbg);
    mbedtls_ssl_conf_ca_chain(&conf, srvcert.next, NULL);

    ret = mbedtls_ssl_conf_own_cert(&conf, &srvcert, &pkey);
    if (ret != 0) {
        print_mbedtls_error("ssl_conf_own_cert", ret);
        goto exit;
    }

    ret = mbedtls_ssl_setup(&ssl, &conf);
    if (ret != 0) {
        print_mbedtls_error("ssl_setup", ret);
        goto exit;
    }

    {
        int bound_port = get_bound_port(&listen_fd);
        if (bound_port <= 0) {
            fprintf(stderr, "Failed to determine bound port\n");
            ret = 1;
            goto exit;
        }
        printf("READY %d\n", bound_port);
        fflush(stdout);
    }

    int served = 0;
    while (connections == 0 || served < connections) {
        mbedtls_net_free(&client_fd);

        ret = mbedtls_net_accept(&listen_fd, &client_fd, NULL, 0, NULL);
        if (ret != 0) {
            print_mbedtls_error("net_accept", ret);
            goto exit;
        }

        mbedtls_ssl_session_reset(&ssl);
        mbedtls_ssl_set_bio(&ssl, &client_fd, mbedtls_net_send, mbedtls_net_recv, NULL);

        while ((ret = mbedtls_ssl_handshake(&ssl)) != 0) {
            if (ret != MBEDTLS_ERR_SSL_WANT_READ &&
                ret != MBEDTLS_ERR_SSL_WANT_WRITE) {
                print_mbedtls_error("ssl_handshake", ret);
                goto exit;
            }
        }

        unsigned char buf[1024];
        memset(buf, 0, sizeof(buf));
        while ((ret = mbedtls_ssl_read(&ssl, buf, sizeof(buf))) <= 0) {
            if (ret == MBEDTLS_ERR_SSL_WANT_READ ||
                ret == MBEDTLS_ERR_SSL_WANT_WRITE) {
                continue;
            }
            print_mbedtls_error("ssl_read", ret);
            goto exit;
        }

        const char *resp = "PONG";
        size_t resp_len = strlen(resp);
        while ((ret = mbedtls_ssl_write(&ssl, (const unsigned char *) resp, resp_len)) <= 0) {
            if (ret == MBEDTLS_ERR_SSL_WANT_READ ||
                ret == MBEDTLS_ERR_SSL_WANT_WRITE) {
                continue;
            }
            print_mbedtls_error("ssl_write", ret);
            goto exit;
        }

        while ((ret = mbedtls_ssl_close_notify(&ssl)) < 0) {
            if (ret != MBEDTLS_ERR_SSL_WANT_READ &&
                ret != MBEDTLS_ERR_SSL_WANT_WRITE) {
                print_mbedtls_error("ssl_close_notify", ret);
                goto exit;
            }
        }

        served += 1;
    }

    ret = 0;

exit:
    mbedtls_net_free(&client_fd);
    mbedtls_net_free(&listen_fd);
    mbedtls_x509_crt_free(&srvcert);
    mbedtls_pk_free(&pkey);
    mbedtls_ssl_free(&ssl);
    mbedtls_ssl_config_free(&conf);
    mbedtls_ctr_drbg_free(&ctr_drbg);
    mbedtls_entropy_free(&entropy);

    return ret;
}
#endif
