/*
 * Copyright (C) 2019-2021 Data Ductus AB
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifdef __linux__
#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif
#endif

#include "env.h"

#include "../rts/log.h"

struct FileDescriptorData fd_data[MAX_FD];
int wakeup_pipe[2];

extern char rts_exit;
extern int return_val;

#ifdef IS_MACOS         // Use kqueue
int kq;
void EVENT_init() {
    kq = kqueue();
    struct kevent wakeup;
    EV_SET(&wakeup, wakeup_pipe[0], EVFILT_READ, EV_ADD, 0, 0, NULL);
    kevent(kq, &wakeup, 1, NULL, 0, NULL);
}
void EVENT_add_read(int fd) {
    EV_SET(&fd_data[fd].event_spec, fd, EVFILT_READ, EV_ADD, 0, 0, NULL);
    kevent(kq, &fd_data[fd].event_spec, 1, NULL, 0, NULL);
}
void EVENT_add_read_once(int fd) {
    EV_SET(&fd_data[fd].event_spec, fd, EVFILT_READ, EV_ADD | EV_ONESHOT, 0, 0, NULL);
    kevent(kq, &fd_data[fd].event_spec, 1, NULL, 0, NULL);
}
void EVENT_mod_read_once(int fd) {
    EV_SET(&fd_data[fd].event_spec, fd, EVFILT_READ, EV_ADD | EV_ONESHOT, 0, 0, NULL);
    kevent(kq, &fd_data[fd].event_spec, 1, NULL, 0, NULL);
}
void EVENT_add_write_once(int fd) {
    EV_SET(&fd_data[fd].event_spec, fd, EVFILT_WRITE, EV_ADD | EV_ONESHOT, 0, 0, NULL);
    kevent(kq, &fd_data[fd].event_spec, 1, NULL, 0, NULL);
}
void EVENT_del_read(int fd) {
    EV_SET(&fd_data[fd].event_spec, fd, EVFILT_READ, EV_DISABLE, 0, 0, NULL);
    kevent(kq, &fd_data[fd].event_spec, 1, NULL, 0, NULL);
}
int EVENT_wait(EVENT_type *ev, struct timespec *timeout) {
    return kevent(kq, NULL, 0, ev, 1, timeout);
}
int EVENT_fd(EVENT_type *ev) {
    return ev->ident;
}
int EVENT_is_wakeup(EVENT_type *ev) {
    return ev->filter == EVFILT_READ & ev->ident == wakeup_pipe[0];
}
int EVENT_is_eof(EVENT_type *ev) {
    return ev->flags & EV_EOF;
}
int EVENT_is_error(EVENT_type *ev) {
    return ev->flags & EV_ERROR;
}
int EVENT_errno(EVENT_type *ev) {
    return ev->data;
}
int EVENT_is_read(EVENT_type *ev) {
    return ev->filter==EVFILT_READ;
}
int EVENT_fd_is_read(int fd) {
    return fd_data[fd].event_spec.filter == EVFILT_READ;
}
#endif

#ifdef IS_GNU_LINUX             // Use epoll            
int ep;
void EVENT_init() {
    ep = epoll_create(1);
    struct epoll_event wakeup;
    wakeup.events = EPOLLIN;
    wakeup.data.fd = wakeup_pipe[0];
    epoll_ctl(ep, EPOLL_CTL_ADD, wakeup_pipe[0], &wakeup);
}
void EVENT_add_read(int fd) {
    fd_data[fd].event_spec.events = EPOLLIN | EPOLLRDHUP;
    fd_data[fd].event_spec.data.fd = fd;
    epoll_ctl(ep, EPOLL_CTL_ADD, fd, &fd_data[fd].event_spec);
}
void EVENT_add_read_once(int fd) {
    fd_data[fd].event_spec.events = EPOLLIN | EPOLLRDHUP | EPOLLONESHOT;
    fd_data[fd].event_spec.data.fd = fd;
    epoll_ctl(ep, EPOLL_CTL_ADD, fd, &fd_data[fd].event_spec);
}
void EVENT_mod_read_once(int fd) {
    fd_data[fd].event_spec.events = EPOLLIN | EPOLLRDHUP | EPOLLONESHOT;
    fd_data[fd].event_spec.data.fd = fd;
    epoll_ctl(ep, EPOLL_CTL_MOD, fd, &fd_data[fd].event_spec);
}
void EVENT_add_write_once(int fd) {
    fd_data[fd].event_spec.events = EPOLLOUT | EPOLLRDHUP | EPOLLONESHOT;
    fd_data[fd].event_spec.data.fd = fd;
    epoll_ctl(ep, EPOLL_CTL_ADD, fd, &fd_data[fd].event_spec);
}
void EVENT_del_read(int fd) {
    fd_data[fd].event_spec.events = EPOLLIN;
    fd_data[fd].event_spec.data.fd = fd;
    epoll_ctl(ep, EPOLL_CTL_DEL, fd, &fd_data[fd].event_spec);
}
void EVENT_del_write_once(int fd) {
    fd_data[fd].event_spec.events = EPOLLOUT | EPOLLRDHUP | EPOLLONESHOT;
    fd_data[fd].event_spec.data.fd = fd;
    epoll_ctl(ep, EPOLL_CTL_DEL, fd, &fd_data[fd].event_spec);
}
int EVENT_wait(EVENT_type *ev, struct timespec *timeout) {
    int msec = timeout ? timeout->tv_sec * 1000 + timeout->tv_nsec / 1000000 : -1;
    return epoll_wait(ep, ev, 1, msec);
//    return epoll_pwait2(ep, ev, 1, timeout, NULL);        // appears in linux kernel 5.11
}
int EVENT_fd(EVENT_type *ev) {
    return ev->data.fd;
}
int EVENT_is_wakeup(EVENT_type *ev) {
    return (ev->events & EPOLLIN) && ev->data.fd == wakeup_pipe[0];
}
// epoll TCP disconnect is EPOLLRDHUP (remote bla hup) and various other socket
// errors (like what?) go in EPOLLHUP so we check for both
int EVENT_is_eof(EVENT_type *ev) {
    return ev->events & (EPOLLHUP | EPOLLRDHUP);
}
int EVENT_is_error(EVENT_type *ev) {
    return ev->events & EPOLLERR;
}
int EVENT_errno(EVENT_type *ev) {
    int error = 0;
    socklen_t errlen = sizeof(error);
    getsockopt(ev->data.fd, SOL_SOCKET, SO_ERROR, (void *)&error, &errlen);
    return error;
}
int EVENT_is_read(EVENT_type *ev) {
    return ev->events & EPOLLIN;
}
int EVENT_fd_is_read(int fd) {
    return fd_data[fd].event_spec.events & EPOLLIN;
}
#endif

static void $init_FileDescriptorData(int fd) {
  fd_data[fd].kind = nohandler;
  fd_data[fd].rhandler = NULL;
  fd_data[fd].errhandler = NULL;
  fd_data[fd].chandler = NULL;
  fd_data[fd].conn = NULL;
  bzero(fd_data[fd].buffer, BUF_SIZE);
}

int new_socket ($function handler) {
  int fd = socket(PF_INET,SOCK_STREAM,0);
  fcntl(fd,F_SETFL,O_NONBLOCK);
  fd_data[fd].kind = connecthandler;
  fd_data[fd].chandler = handler;
  return fd;
}

void setupConnection (int fd) {
  $Connection conn = $Connection$newact(fd);
  fd_data[fd].conn = conn;
  fd_data[fd].chandler->$class->__call__(fd_data[fd].chandler, conn);
}

$str $getName(int fd) {
  socklen_t socklen = sizeof(struct sockaddr_in);
  char *buf = malloc(100);
  getnameinfo((struct sockaddr *)&fd_data[fd].sock_addr,socklen,buf,100,NULL,0,0);
  return to$str(buf);
}

///////////////////////////////////////////////////////////////////////////////////////////
// START GENERATED __builtin__.act
$NoneType $l$1lambda$__init__ ($l$1lambda p$self, $Env __self__, $str s) {
    p$self->__self__ = __self__;
    p$self->s = s;
    return $None;
}
$R $l$1lambda$__call__ ($l$1lambda p$self, $Cont c$cont) {
    $Env __self__ = p$self->__self__;
    $str s = p$self->s;
    return __self__->$class->stdout_write$local(__self__, s, c$cont);
}
void $l$1lambda$__serialize__ ($l$1lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
    $step_serialize(self->s, state);
}
$l$1lambda $l$1lambda$__deserialize__ ($l$1lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct $l$1lambda));
            self->$class = &$l$1lambda$methods;
            return self;
        }
        self = $DNEW($l$1lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    self->s = $step_deserialize(state);
    return self;
}
$l$1lambda $l$1lambda$new($Env p$1, $str p$2) {
    $l$1lambda $tmp = malloc(sizeof(struct $l$1lambda));
    $tmp->$class = &$l$1lambda$methods;
    $l$1lambda$methods.__init__($tmp, p$1, p$2);
    return $tmp;
}
struct $l$1lambda$class $l$1lambda$methods;
$NoneType $l$2lambda$__init__ ($l$2lambda p$self, $Env __self__, $function cb) {
    p$self->__self__ = __self__;
    p$self->cb = cb;
    return $None;
}
$R $l$2lambda$__call__ ($l$2lambda p$self, $Cont c$cont) {
    $Env __self__ = p$self->__self__;
    $function cb = p$self->cb;
    return __self__->$class->stdin_install$local(__self__, cb, c$cont);
}
void $l$2lambda$__serialize__ ($l$2lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
    $step_serialize(self->cb, state);
}
$l$2lambda $l$2lambda$__deserialize__ ($l$2lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct $l$2lambda));
            self->$class = &$l$2lambda$methods;
            return self;
        }
        self = $DNEW($l$2lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    self->cb = $step_deserialize(state);
    return self;
}
$l$2lambda $l$2lambda$new($Env p$1, $function p$2) {
    $l$2lambda $tmp = malloc(sizeof(struct $l$2lambda));
    $tmp->$class = &$l$2lambda$methods;
    $l$2lambda$methods.__init__($tmp, p$1, p$2);
    return $tmp;
}
struct $l$2lambda$class $l$2lambda$methods;
$NoneType $l$3lambda$__init__ ($l$3lambda p$self, $Env __self__, $str host, $int port, $function cb) {
    p$self->__self__ = __self__;
    p$self->host = host;
    p$self->port = port;
    p$self->cb = cb;
    return $None;
}
$R $l$3lambda$__call__ ($l$3lambda p$self, $Cont c$cont) {
    $Env __self__ = p$self->__self__;
    $str host = p$self->host;
    $int port = p$self->port;
    $function cb = p$self->cb;
    return __self__->$class->connect$local(__self__, host, port, cb, c$cont);
}
void $l$3lambda$__serialize__ ($l$3lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
    $step_serialize(self->host, state);
    $step_serialize(self->port, state);
    $step_serialize(self->cb, state);
}
$l$3lambda $l$3lambda$__deserialize__ ($l$3lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct $l$3lambda));
            self->$class = &$l$3lambda$methods;
            return self;
        }
        self = $DNEW($l$3lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    self->host = $step_deserialize(state);
    self->port = $step_deserialize(state);
    self->cb = $step_deserialize(state);
    return self;
}
$l$3lambda $l$3lambda$new($Env p$1, $str p$2, $int p$3, $function p$4) {
    $l$3lambda $tmp = malloc(sizeof(struct $l$3lambda));
    $tmp->$class = &$l$3lambda$methods;
    $l$3lambda$methods.__init__($tmp, p$1, p$2, p$3, p$4);
    return $tmp;
}
struct $l$3lambda$class $l$3lambda$methods;
$NoneType $l$4lambda$__init__ ($l$4lambda p$self, $Env __self__, $int port, $function on_connect, $function on_error) {
    p$self->__self__ = __self__;
    p$self->port = port;
    p$self->on_connect = on_connect;
    p$self->on_error = on_error;
    return $None;
}
$R $l$4lambda$__call__ ($l$4lambda p$self, $Cont c$cont) {
    $Env __self__ = p$self->__self__;
    $int port = p$self->port;
    $function on_connect = p$self->on_connect;
    $function on_error = p$self->on_error;
    return __self__->$class->listen$local(__self__, port, on_connect, on_error, c$cont);
}
void $l$4lambda$__serialize__ ($l$4lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
    $step_serialize(self->port, state);
    $step_serialize(self->on_connect, state);
    $step_serialize(self->on_error, state);
}
$l$4lambda $l$4lambda$__deserialize__ ($l$4lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct $l$4lambda));
            self->$class = &$l$4lambda$methods;
            return self;
        }
        self = $DNEW($l$4lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    self->port = $step_deserialize(state);
    self->on_connect = $step_deserialize(state);
    self->on_error = $step_deserialize(state);
    return self;
}
$l$4lambda $l$4lambda$new($Env p$1, $int p$2, $function p$3, $function p$4) {
    $l$4lambda $tmp = malloc(sizeof(struct $l$4lambda));
    $tmp->$class = &$l$4lambda$methods;
    $l$4lambda$methods.__init__($tmp, p$1, p$2, p$3, p$4);
    return $tmp;
}
struct $l$4lambda$class $l$4lambda$methods;
$NoneType $l$5lambda$__init__ ($l$5lambda p$self, $Env __self__, $int n) {
    p$self->__self__ = __self__;
    p$self->n = n;
    return $None;
}
$R $l$5lambda$__call__ ($l$5lambda p$self, $Cont c$cont) {
    $Env __self__ = p$self->__self__;
    $int n = p$self->n;
    return __self__->$class->exit$local(__self__, n, c$cont);
}
void $l$5lambda$__serialize__ ($l$5lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
    $step_serialize(self->n, state);
}
$l$5lambda $l$5lambda$__deserialize__ ($l$5lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct $l$5lambda));
            self->$class = &$l$5lambda$methods;
            return self;
        }
        self = $DNEW($l$5lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    self->n = $step_deserialize(state);
    return self;
}
$l$5lambda $l$5lambda$new($Env p$1, $int p$2) {
    $l$5lambda $tmp = malloc(sizeof(struct $l$5lambda));
    $tmp->$class = &$l$5lambda$methods;
    $l$5lambda$methods.__init__($tmp, p$1, p$2);
    return $tmp;
}
struct $l$5lambda$class $l$5lambda$methods;
$NoneType $l$6lambda$__init__ ($l$6lambda p$self, $Env __self__, $str nm) {
    p$self->__self__ = __self__;
    p$self->nm = nm;
    return $None;
}
$R $l$6lambda$__call__ ($l$6lambda p$self, $Cont c$cont) {
    $Env __self__ = p$self->__self__;
    $str nm = p$self->nm;
    return __self__->$class->openR$local(__self__, nm, c$cont);
}
void $l$6lambda$__serialize__ ($l$6lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
    $step_serialize(self->nm, state);
}
$l$6lambda $l$6lambda$__deserialize__ ($l$6lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct $l$6lambda));
            self->$class = &$l$6lambda$methods;
            return self;
        }
        self = $DNEW($l$6lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    self->nm = $step_deserialize(state);
    return self;
}
$l$6lambda $l$6lambda$new($Env p$1, $str p$2) {
    $l$6lambda $tmp = malloc(sizeof(struct $l$6lambda));
    $tmp->$class = &$l$6lambda$methods;
    $l$6lambda$methods.__init__($tmp, p$1, p$2);
    return $tmp;
}
struct $l$6lambda$class $l$6lambda$methods;
$NoneType $l$7lambda$__init__ ($l$7lambda p$self, $Env __self__, $str nm) {
    p$self->__self__ = __self__;
    p$self->nm = nm;
    return $None;
}
$R $l$7lambda$__call__ ($l$7lambda p$self, $Cont c$cont) {
    $Env __self__ = p$self->__self__;
    $str nm = p$self->nm;
    return __self__->$class->openW$local(__self__, nm, c$cont);
}
void $l$7lambda$__serialize__ ($l$7lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
    $step_serialize(self->nm, state);
}
$l$7lambda $l$7lambda$__deserialize__ ($l$7lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct $l$7lambda));
            self->$class = &$l$7lambda$methods;
            return self;
        }
        self = $DNEW($l$7lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    self->nm = $step_deserialize(state);
    return self;
}
$l$7lambda $l$7lambda$new($Env p$1, $str p$2) {
    $l$7lambda $tmp = malloc(sizeof(struct $l$7lambda));
    $tmp->$class = &$l$7lambda$methods;
    $l$7lambda$methods.__init__($tmp, p$1, p$2);
    return $tmp;
}
struct $l$7lambda$class $l$7lambda$methods;
$NoneType $l$8lambda$__init__ ($l$8lambda p$self, $Connection __self__, $bytes s) {
    p$self->__self__ = __self__;
    p$self->s = s;
    return $None;
}
$R $l$8lambda$__call__ ($l$8lambda p$self, $Cont c$cont) {
    $Connection __self__ = p$self->__self__;
    $bytes s = p$self->s;
    return __self__->$class->write$local(__self__, s, c$cont);
}
void $l$8lambda$__serialize__ ($l$8lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
    $step_serialize(self->s, state);
}
$l$8lambda $l$8lambda$__deserialize__ ($l$8lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct $l$8lambda));
            self->$class = &$l$8lambda$methods;
            return self;
        }
        self = $DNEW($l$8lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    self->s = $step_deserialize(state);
    return self;
}
$l$8lambda $l$8lambda$new($Connection p$1, $bytes p$2) {
    $l$8lambda $tmp = malloc(sizeof(struct $l$8lambda));
    $tmp->$class = &$l$8lambda$methods;
    $l$8lambda$methods.__init__($tmp, p$1, p$2);
    return $tmp;
}
struct $l$8lambda$class $l$8lambda$methods;
$NoneType $l$9lambda$__init__ ($l$9lambda p$self, $Connection __self__) {
    p$self->__self__ = __self__;
    return $None;
}
$R $l$9lambda$__call__ ($l$9lambda p$self, $Cont c$cont) {
    $Connection __self__ = p$self->__self__;
    return __self__->$class->close$local(__self__, c$cont);
}
void $l$9lambda$__serialize__ ($l$9lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
}
$l$9lambda $l$9lambda$__deserialize__ ($l$9lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct $l$9lambda));
            self->$class = &$l$9lambda$methods;
            return self;
        }
        self = $DNEW($l$9lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    return self;
}
$l$9lambda $l$9lambda$new($Connection p$1) {
    $l$9lambda $tmp = malloc(sizeof(struct $l$9lambda));
    $tmp->$class = &$l$9lambda$methods;
    $l$9lambda$methods.__init__($tmp, p$1);
    return $tmp;
}
struct $l$9lambda$class $l$9lambda$methods;
$NoneType $l$10lambda$__init__ ($l$10lambda p$self, $Connection __self__, $function cb1, $function cb2) {
    p$self->__self__ = __self__;
    p$self->cb1 = cb1;
    p$self->cb2 = cb2;
    return $None;
}
$R $l$10lambda$__call__ ($l$10lambda p$self, $Cont c$cont) {
    $Connection __self__ = p$self->__self__;
    $function cb1 = p$self->cb1;
    $function cb2 = p$self->cb2;
    return __self__->$class->on_receive$local(__self__, cb1, cb2, c$cont);
}
void $l$10lambda$__serialize__ ($l$10lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
    $step_serialize(self->cb1, state);
    $step_serialize(self->cb2, state);
}
$l$10lambda $l$10lambda$__deserialize__ ($l$10lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct $l$10lambda));
            self->$class = &$l$10lambda$methods;
            return self;
        }
        self = $DNEW($l$10lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    self->cb1 = $step_deserialize(state);
    self->cb2 = $step_deserialize(state);
    return self;
}
$l$10lambda $l$10lambda$new($Connection p$1, $function p$2, $function p$3) {
    $l$10lambda $tmp = malloc(sizeof(struct $l$10lambda));
    $tmp->$class = &$l$10lambda$methods;
    $l$10lambda$methods.__init__($tmp, p$1, p$2, p$3);
    return $tmp;
}
struct $l$10lambda$class $l$10lambda$methods;
$NoneType $l$11lambda$__init__ ($l$11lambda p$self, $RFile __self__) {
    p$self->__self__ = __self__;
    return $None;
}
$R $l$11lambda$__call__ ($l$11lambda p$self, $Cont c$cont) {
    $RFile __self__ = p$self->__self__;
    return __self__->$class->readln$local(__self__, c$cont);
}
void $l$11lambda$__serialize__ ($l$11lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
}
$l$11lambda $l$11lambda$__deserialize__ ($l$11lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct $l$11lambda));
            self->$class = &$l$11lambda$methods;
            return self;
        }
        self = $DNEW($l$11lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    return self;
}
$l$11lambda $l$11lambda$new($RFile p$1) {
    $l$11lambda $tmp = malloc(sizeof(struct $l$11lambda));
    $tmp->$class = &$l$11lambda$methods;
    $l$11lambda$methods.__init__($tmp, p$1);
    return $tmp;
}
struct $l$11lambda$class $l$11lambda$methods;
$NoneType $l$12lambda$__init__ ($l$12lambda p$self, $RFile __self__) {
    p$self->__self__ = __self__;
    return $None;
}
$R $l$12lambda$__call__ ($l$12lambda p$self, $Cont c$cont) {
    $RFile __self__ = p$self->__self__;
    return __self__->$class->close$local(__self__, c$cont);
}
void $l$12lambda$__serialize__ ($l$12lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
}
$l$12lambda $l$12lambda$__deserialize__ ($l$12lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct $l$12lambda));
            self->$class = &$l$12lambda$methods;
            return self;
        }
        self = $DNEW($l$12lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    return self;
}
$l$12lambda $l$12lambda$new($RFile p$1) {
    $l$12lambda $tmp = malloc(sizeof(struct $l$12lambda));
    $tmp->$class = &$l$12lambda$methods;
    $l$12lambda$methods.__init__($tmp, p$1);
    return $tmp;
}
struct $l$12lambda$class $l$12lambda$methods;
$NoneType $l$13lambda$__init__ ($l$13lambda p$self, $WFile __self__, $str s) {
    p$self->__self__ = __self__;
    p$self->s = s;
    return $None;
}
$R $l$13lambda$__call__ ($l$13lambda p$self, $Cont c$cont) {
    $WFile __self__ = p$self->__self__;
    $str s = p$self->s;
    return __self__->$class->write$local(__self__, s, c$cont);
}
void $l$13lambda$__serialize__ ($l$13lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
    $step_serialize(self->s, state);
}
$l$13lambda $l$13lambda$__deserialize__ ($l$13lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct $l$13lambda));
            self->$class = &$l$13lambda$methods;
            return self;
        }
        self = $DNEW($l$13lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    self->s = $step_deserialize(state);
    return self;
}
$l$13lambda $l$13lambda$new($WFile p$1, $str p$2) {
    $l$13lambda $tmp = malloc(sizeof(struct $l$13lambda));
    $tmp->$class = &$l$13lambda$methods;
    $l$13lambda$methods.__init__($tmp, p$1, p$2);
    return $tmp;
}
struct $l$13lambda$class $l$13lambda$methods;
$NoneType $l$14lambda$__init__ ($l$14lambda p$self, $WFile __self__) {
    p$self->__self__ = __self__;
    return $None;
}
$R $l$14lambda$__call__ ($l$14lambda p$self, $Cont c$cont) {
    $WFile __self__ = p$self->__self__;
    return __self__->$class->close$local(__self__, c$cont);
}
void $l$14lambda$__serialize__ ($l$14lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
}
$l$14lambda $l$14lambda$__deserialize__ ($l$14lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct $l$14lambda));
            self->$class = &$l$14lambda$methods;
            return self;
        }
        self = $DNEW($l$14lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    return self;
}
$l$14lambda $l$14lambda$new($WFile p$1) {
    $l$14lambda $tmp = malloc(sizeof(struct $l$14lambda));
    $tmp->$class = &$l$14lambda$methods;
    $l$14lambda$methods.__init__($tmp, p$1);
    return $tmp;
}
struct $l$14lambda$class $l$14lambda$methods;
$NoneType $l$15lambda$__init__ ($l$15lambda p$self, $function on_error) {
    p$self->on_error = on_error;
    return $None;
}
$R $l$15lambda$__call__ ($l$15lambda p$self, $Cont c$cont) {
    $function on_error = p$self->on_error;
    return $R_CONT(c$cont, (($Msg (*) ($function))on_error->$class->__call__)(on_error));
}
void $l$15lambda$__serialize__ ($l$15lambda self, $Serial$state state) {
    $step_serialize(self->on_error, state);
}
$l$15lambda $l$15lambda$__deserialize__ ($l$15lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct $l$15lambda));
            self->$class = &$l$15lambda$methods;
            return self;
        }
        self = $DNEW($l$15lambda, state);
    }
    self->on_error = $step_deserialize(state);
    return self;
}
$l$15lambda $l$15lambda$new($function p$1) {
    $l$15lambda $tmp = malloc(sizeof(struct $l$15lambda));
    $tmp->$class = &$l$15lambda$methods;
    $l$15lambda$methods.__init__($tmp, p$1);
    return $tmp;
}
struct $l$15lambda$class $l$15lambda$methods;
$NoneType $l$16lambda$__init__ ($l$16lambda p$self, $ListenSocket __self__) {
    p$self->__self__ = __self__;
    return $None;
}
$R $l$16lambda$__call__ ($l$16lambda p$self, $Cont c$cont) {
    $ListenSocket __self__ = p$self->__self__;
    return __self__->$class->close$local(__self__, c$cont);
}
void $l$16lambda$__serialize__ ($l$16lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
}
$l$16lambda $l$16lambda$__deserialize__ ($l$16lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct $l$16lambda));
            self->$class = &$l$16lambda$methods;
            return self;
        }
        self = $DNEW($l$16lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    return self;
}
$l$16lambda $l$16lambda$new($ListenSocket p$1) {
    $l$16lambda $tmp = malloc(sizeof(struct $l$16lambda));
    $tmp->$class = &$l$16lambda$methods;
    $l$16lambda$methods.__init__($tmp, p$1);
    return $tmp;
}
struct $l$16lambda$class $l$16lambda$methods;
$NoneType $WorldAuth$__init__ ($WorldAuth self) {
    return $None;
}
void $WorldAuth$__serialize__ ($WorldAuth self, $Serial$state state) {
}
$WorldAuth $WorldAuth$__deserialize__ ($WorldAuth self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct $WorldAuth));
            self->$class = &$WorldAuth$methods;
            return self;
        }
        self = $DNEW($WorldAuth, state);
    }
    return self;
}
struct $WorldAuth$class $WorldAuth$methods;
$Msg $Env$stdout_write ($Env __self__, $str s) {
    return $ASYNC((($Actor)__self__), (($Cont)$l$1lambda$new((($Env)__self__), s)));
}
$Msg $Env$stdin_install ($Env __self__, $function cb) {
    return $ASYNC((($Actor)__self__), (($Cont)$l$2lambda$new((($Env)__self__), cb)));
}
$Msg $Env$connect ($Env __self__, $str host, $int port, $function cb) {
    return $ASYNC((($Actor)__self__), (($Cont)$l$3lambda$new((($Env)__self__), host, port, cb)));
}
$Msg $Env$listen ($Env __self__, $int port, $function on_connect, $function on_error) {
    return $ASYNC((($Actor)__self__), (($Cont)$l$4lambda$new((($Env)__self__), port, on_connect, on_error)));
}
$Msg $Env$exit ($Env __self__, $int n) {
    return $ASYNC((($Actor)__self__), (($Cont)$l$5lambda$new((($Env)__self__), n)));
}
$Msg $Env$openR ($Env __self__, $str nm) {
    return $ASYNC((($Actor)__self__), (($Cont)$l$6lambda$new((($Env)__self__), nm)));
}
$Msg $Env$openW ($Env __self__, $str nm) {
    return $ASYNC((($Actor)__self__), (($Cont)$l$7lambda$new((($Env)__self__), nm)));
}
void $Connection$__serialize__ ($Connection self, $Serial$state state) {
    $Actor$methods.__serialize__(($Actor)self, state);
    $step_serialize(self->cb_err, state);
}
$Connection $Connection$__deserialize__ ($Connection self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct $Connection));
            self->$class = &$Connection$methods;
            return self;
        }
        self = $DNEW($Connection, state);
    }
    $Actor$methods.__deserialize__(($Actor)self, state);
    self->cb_err = $step_deserialize(state);
    return self;
}
$Msg $RFile$readln ($RFile __self__) {
    return $ASYNC((($Actor)__self__), (($Cont)$l$11lambda$new((($RFile)__self__))));
}
$Msg $RFile$close ($RFile __self__) {
    return $ASYNC((($Actor)__self__), (($Cont)$l$12lambda$new((($RFile)__self__))));
}
$Msg $WFile$write ($WFile __self__, $str s) {
    return $ASYNC((($Actor)__self__), (($Cont)$l$13lambda$new((($WFile)__self__), s)));
}
$Msg $WFile$close ($WFile __self__) {
    return $ASYNC((($Actor)__self__), (($Cont)$l$14lambda$new((($WFile)__self__))));
}
$Msg $ListenSocket$close ($ListenSocket __self__) {
    return $ASYNC((($Actor)__self__), (($Cont)$l$16lambda$new((($ListenSocket)__self__))));
}
void $ListenSocket$__serialize__ ($ListenSocket self, $Serial$state state) {
    $Actor$methods.__serialize__(($Actor)self, state);
    $step_serialize(self->cb_err, state);
}
$ListenSocket $ListenSocket$__deserialize__ ($ListenSocket self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct $ListenSocket));
            self->$class = &$ListenSocket$methods;
            return self;
        }
        self = $DNEW($ListenSocket, state);
    }
    $Actor$methods.__deserialize__(($Actor)self, state);
    self->cb_err = $step_deserialize(state);
    return self;
}
struct $ListenSocket$class $ListenSocket$methods;
// END GENERATED __builtin__.act
///////////////////////////////////////////////////////////////////////////////////////////


$WorldAuth $WorldAuth$new() {
    $WorldAuth $tmp = malloc(sizeof(struct $WorldAuth));
    $tmp->$class = &$WorldAuth$methods;
    $WorldAuth$methods.__init__($tmp);
    return $tmp;
}

// Env /////////////////////////////////////////////////////////////////////////

$NoneType $Env$__init__ ($Env __self__, $WorldAuth token, $list argv) {
    __self__->auth = token;
    __self__->argv = argv;
    __self__->$affinity = 0;
    return $None;
}
$R $Env$stdout_write$local ($Env __self__, $str s, $Cont c$cont) {
    printf("%s", s->str);
    return $R_CONT(c$cont, $None);
}
$R $Env$stdin_install$local ($Env __self__, $function cb, $Cont c$cont) {
    fd_data[STDIN_FILENO].kind = readhandler;
    fd_data[STDIN_FILENO].rhandler = cb;
    EVENT_add_read(STDIN_FILENO);
    return $R_CONT(c$cont, $None);
}
$R $Env$connect$local ($Env __self__, $str host, $int port, $function cb, $Cont c$cont) {
    struct sockaddr_in addr;
    struct in_addr iaddr;
    struct hostent *ent;
    int hostid;
    int fd = new_socket(cb);
    ent = gethostbyname((char *)host->str); //this should be replaced by calling getaddrinfo
    if(ent==NULL) {
      fd_data[fd].chandler->$class->__call__(fd_data[fd].chandler, NULL);
      //fprintf(stderr,"Name lookup error"); 
    }
    else {
      memcpy(&hostid, ent->h_addr_list[0], sizeof hostid);
      iaddr.s_addr = hostid;
      fd_data[fd].sock_addr.sin_addr = iaddr;
      fd_data[fd].sock_addr.sin_port = htons(port->val);
      fd_data[fd].sock_addr.sin_family = AF_INET;
      if (connect(fd,(struct sockaddr *)&fd_data[fd].sock_addr,sizeof(struct sockaddr)) < 0) { // couldn't connect immediately, 
        if (errno==EINPROGRESS)  {                                                             // so check if attempt continues asynchronously.
          EVENT_add_write_once(fd);
        } else {
          fd_data[fd].chandler->$class->__call__(fd_data[fd].chandler, NULL);
          //fprintf(stderr,"Connect failed");
         }
      } else // connect succeeded immediately (can this ever happen for a non-blocking socket?)
        setupConnection(fd);
    }
    return $R_CONT(c$cont, $None);
}
$R $Env$listen$local ($Env __self__, $int port, $function on_connect, $function on_error, $Cont c$cont) {
    struct sockaddr_in addr;
    int fd = new_socket(on_connect);
    $ListenSocket lsock = $ListenSocket$newact(fd, on_error);
    addr.sin_addr.s_addr = INADDR_ANY;
    addr.sin_port = htons(port->val);
    addr.sin_family = AF_INET;
    if (bind(fd,(struct sockaddr *)&addr,sizeof(struct sockaddr)) < 0)
        on_error->$class->__call__(on_error, lsock);
    if (listen(fd, 5) < 0)
        on_error->$class->__call__(on_error, lsock);
    EVENT_add_read_once(fd);

    return $R_CONT(c$cont, lsock);
}
$R $Env$exit$local ($Env __self__, $int n, $Cont c$cont) {
    return_val = n->val;
    rts_shutdown();
    return $R_CONT(c$cont, $None);
}
$R $Env$openR$local ($Env __self__, $str nm, $Cont c$cont) {
    FILE *file = fopen((char *)nm->str,"r");
    if (file)
        return $R_CONT(c$cont, $RFile$newact(file));
    else
        return $R_CONT(c$cont, $None);
}
$R $Env$openW$local ($Env __self__, $str nm, $Cont c$cont) {
    int descr = open((char *)nm->str, O_WRONLY | O_CREAT | O_APPEND, S_IWUSR|S_IRUSR|S_IRGRP|S_IROTH);
    if (descr < 0)
        return $R_CONT(c$cont, $None);
    else
        return $R_CONT(c$cont, $WFile$newact(descr));
}
void $Env$__serialize__ ($Env self, $Serial$state state) {
    $Actor$methods.__serialize__(($Actor)self, state);
    $step_serialize(self->argv, state);
}
$Env $Env$__deserialize__ ($Env self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct $Env));
            self->$class = &$Env$methods;
            return self;
        }
        self = $DNEW($Env, state);
    }
    $Actor$methods.__deserialize__(($Actor)self, state);
    self->argv = $step_deserialize(state);
    return self;
}
$Env $Env$newact($WorldAuth token, $list p$1) {
    $Env $tmp = $NEWACTOR($Env);
    $tmp->$class->__init__($tmp, token, p$1);  // Inline this message, note that $Env$__init__ is *not* CPS'ed
    serialize_state_shortcut(($Actor)$tmp);
    return $tmp;
}
struct $Env$class $Env$methods;


// Connection //////////////////////////////////////////////////////////////////

$NoneType $Connection$__init__ ($Connection __self__, int descr) {
    __self__->descriptor = descr;
    __self__->cb_err = NULL;
    return $None;
}
$NoneType $Connection$__resume__($Connection self) {
    if (self->cb_err)
        self->cb_err->$class->__call__(self->cb_err, self);
    return $None;
}
$R $Connection$write$local ($Connection __self__, $bytes s, $Cont c$cont) {
    memcpy(fd_data[__self__->descriptor].buffer,s->str,s->nbytes+1);
    int chunk_size = s->nbytes > BUF_SIZE ? BUF_SIZE : s->nbytes; 
    int r = write(__self__->descriptor,fd_data[__self__->descriptor].buffer,chunk_size);
    //  for now, assume str->nbytes < BUF_SIZE
    return $R_CONT(c$cont, $None);
}
$R $Connection$close$local ($Connection __self__, $Cont c$cont) {
    close(__self__->descriptor); 
    $init_FileDescriptorData(__self__->descriptor);
    return $R_CONT(c$cont, $None);
}
$R $Connection$on_receive$local ($Connection __self__, $function cb1, $function cb2, $Cont c$cont) {
    __self__->cb_err = cb2;
    fd_data[__self__->descriptor].kind = readhandler;
    fd_data[__self__->descriptor].rhandler = cb1;
    fd_data[__self__->descriptor].errhandler = cb2;
    EVENT_add_read(__self__->descriptor);
    return $R_CONT(c$cont, $None);
}
$Msg $Connection$write ($Connection __self__, $bytes s) {
    return $ASYNC((($Actor)__self__), (($Cont)$l$8lambda$new(__self__, s)));
}
$Msg $Connection$close ($Connection __self__) {
    return $ASYNC((($Actor)__self__), (($Cont)$l$9lambda$new(__self__)));
}
$Msg $Connection$on_receive ($Connection __self__, $function cb1, $function cb2) {
    return $ASYNC((($Actor)__self__), (($Cont)$l$10lambda$new(__self__, cb1, cb2)));
}
$Connection $Connection$newact(int descr) {
    $Connection $tmp = $NEWACTOR($Connection);
    $tmp->$class->__init__($tmp, descr);          // Inline this message, note that $Connection$__init__ is *not* CPS'ed
    serialize_state_shortcut(($Actor)$tmp);
    return $tmp;
}
struct $Connection$class $Connection$methods;


// ListenSocket ////////////////////////////////////////////////////////////////

$NoneType $ListenSocket$__init__ ($ListenSocket __self__, int fd, $function on_error) {
    __self__->fd = fd;
    __self__->cb_err = on_error;
    return $None;
}
$NoneType $ListenSocket$__resume__($ListenSocket self) {
    self->cb_err->$class->__call__(self->cb_err, self);
    return $None;
}
$R $ListenSocket$close$local ($ListenSocket __self__, $Cont c$cont) {
    close(__self__->fd);
    return $R_CONT(c$cont, $None);
}
$ListenSocket $ListenSocket$newact(int fd, $function on_error) {
    $ListenSocket $tmp = $NEWACTOR($ListenSocket);
    $tmp->$class->__init__($tmp, fd, on_error);
    serialize_state_shortcut(($Actor)$tmp);
    return $tmp;
}


// RFile ///////////////////////////////////////////////////////////////////////

$NoneType $RFile$__init__ ($RFile __self__, FILE *file) {
    __self__->file = file;
    return $None;
}
$R $RFile$readln$local ($RFile __self__, $Cont c$cont) {
    char buf[BUF_SIZE];
    char *res = fgets(buf, BUF_SIZE, __self__->file);
    if (res)
       return $R_CONT(c$cont, to$str(res));
    else
      return $R_CONT(c$cont, $None);      
}
$R $RFile$close$local ($RFile __self__, $Cont c$cont) {
    fclose(__self__->file); 
    return $R_CONT(c$cont, $None);
}
void $RFile$__serialize__ ($RFile self, $Serial$state state) {
    $Actor$methods.__serialize__(($Actor)self, state);
}
$RFile $RFile$__deserialize__ ($RFile self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct $RFile));
            self->$class = &$RFile$methods;
            return self;
        }
        self = $DNEW($RFile, state);
    }
    $Actor$methods.__deserialize__(($Actor)self, state);
    return self;
}
$RFile $RFile$newact(FILE *file) {
    $RFile $tmp = $NEWACTOR($RFile);
    $tmp->$class->__init__($tmp, file);     // Inline this message, note that $RFile$__init__ is *not* CPS'ed
    serialize_state_shortcut(($Actor)$tmp);
    return $tmp;
}
struct $RFile$class $RFile$methods;
$NoneType $WFile$__init__ ($WFile __self__, int descr) {
    __self__->descriptor = descr;
    return $None;
}


// WFile ///////////////////////////////////////////////////////////////////////

$R $WFile$write$local ($WFile __self__, $str s, $Cont c$cont) {
    memcpy(fd_data[__self__->descriptor].buffer,s->str,s->nbytes+1);
    int chunk_size = s->nbytes > BUF_SIZE ? BUF_SIZE : s->nbytes; 
    int r = write(__self__->descriptor,fd_data[__self__->descriptor].buffer,chunk_size);
    //  for now, assume str->nbytes < BUF_SIZE
    return $R_CONT(c$cont, $None);
}
$R $WFile$close$local ($WFile __self__, $Cont c$cont) {
    close(__self__->descriptor); 
    $init_FileDescriptorData(__self__->descriptor);
    return $R_CONT(c$cont, $None);
}
void $WFile$__serialize__ ($WFile self, $Serial$state state) {
    $Actor$methods.__serialize__(($Actor)self, state);
}
$WFile $WFile$__deserialize__ ($WFile self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct $WFile));
            self->$class = &$WFile$methods;
            return self;
        }
        self = $DNEW($WFile, state);
    }
    $Actor$methods.__deserialize__(($Actor)self, state);
    return self;
}
$WFile $WFile$newact(int descr) {
    $WFile $tmp = $NEWACTOR($WFile);
    $tmp->$class->__init__($tmp, descr);     // Inline this message, note that $WFile$__init__ is *not* CPS'ed
    serialize_state_shortcut(($Actor)$tmp);
    return $tmp;
}
struct $WFile$class $WFile$methods;


int $done$ = 0;
void $__init__ () {
    if ($done$) return;
    $done$ = 1;
    ///////////////////////////////////////////////////////////////////////////////////////
    // START GENERATED __builtin__.act $__init__
    {
        $l$1lambda$methods.$GCINFO = "$l$1lambda";
        $l$1lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        $l$1lambda$methods.__init__ = $l$1lambda$__init__;
        $l$1lambda$methods.__call__ = $l$1lambda$__call__;
        $l$1lambda$methods.__serialize__ = $l$1lambda$__serialize__;
        $l$1lambda$methods.__deserialize__ = $l$1lambda$__deserialize__;
        $register(&$l$1lambda$methods);
    }
    {
        $l$2lambda$methods.$GCINFO = "$l$2lambda";
        $l$2lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        $l$2lambda$methods.__init__ = $l$2lambda$__init__;
        $l$2lambda$methods.__call__ = $l$2lambda$__call__;
        $l$2lambda$methods.__serialize__ = $l$2lambda$__serialize__;
        $l$2lambda$methods.__deserialize__ = $l$2lambda$__deserialize__;
        $register(&$l$2lambda$methods);
    }
    {
        $l$3lambda$methods.$GCINFO = "$l$3lambda";
        $l$3lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        $l$3lambda$methods.__init__ = $l$3lambda$__init__;
        $l$3lambda$methods.__call__ = $l$3lambda$__call__;
        $l$3lambda$methods.__serialize__ = $l$3lambda$__serialize__;
        $l$3lambda$methods.__deserialize__ = $l$3lambda$__deserialize__;
        $register(&$l$3lambda$methods);
    }
    {
        $l$4lambda$methods.$GCINFO = "$l$4lambda";
        $l$4lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        $l$4lambda$methods.__init__ = $l$4lambda$__init__;
        $l$4lambda$methods.__call__ = $l$4lambda$__call__;
        $l$4lambda$methods.__serialize__ = $l$4lambda$__serialize__;
        $l$4lambda$methods.__deserialize__ = $l$4lambda$__deserialize__;
        $register(&$l$4lambda$methods);
    }
    {
        $l$5lambda$methods.$GCINFO = "$l$5lambda";
        $l$5lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        $l$5lambda$methods.__init__ = $l$5lambda$__init__;
        $l$5lambda$methods.__call__ = $l$5lambda$__call__;
        $l$5lambda$methods.__serialize__ = $l$5lambda$__serialize__;
        $l$5lambda$methods.__deserialize__ = $l$5lambda$__deserialize__;
        $register(&$l$5lambda$methods);
    }
    {
        $l$6lambda$methods.$GCINFO = "$l$6lambda";
        $l$6lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        $l$6lambda$methods.__init__ = $l$6lambda$__init__;
        $l$6lambda$methods.__call__ = $l$6lambda$__call__;
        $l$6lambda$methods.__serialize__ = $l$6lambda$__serialize__;
        $l$6lambda$methods.__deserialize__ = $l$6lambda$__deserialize__;
        $register(&$l$6lambda$methods);
    }
    {
        $l$7lambda$methods.$GCINFO = "$l$7lambda";
        $l$7lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        $l$7lambda$methods.__init__ = $l$7lambda$__init__;
        $l$7lambda$methods.__call__ = $l$7lambda$__call__;
        $l$7lambda$methods.__serialize__ = $l$7lambda$__serialize__;
        $l$7lambda$methods.__deserialize__ = $l$7lambda$__deserialize__;
        $register(&$l$7lambda$methods);
    }
    {
        $l$8lambda$methods.$GCINFO = "$l$8lambda";
        $l$8lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        $l$8lambda$methods.__init__ = $l$8lambda$__init__;
        $l$8lambda$methods.__call__ = $l$8lambda$__call__;
        $l$8lambda$methods.__serialize__ = $l$8lambda$__serialize__;
        $l$8lambda$methods.__deserialize__ = $l$8lambda$__deserialize__;
        $register(&$l$8lambda$methods);
    }
    {
        $l$9lambda$methods.$GCINFO = "$l$9lambda";
        $l$9lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        $l$9lambda$methods.__init__ = $l$9lambda$__init__;
        $l$9lambda$methods.__call__ = $l$9lambda$__call__;
        $l$9lambda$methods.__serialize__ = $l$9lambda$__serialize__;
        $l$9lambda$methods.__deserialize__ = $l$9lambda$__deserialize__;
        $register(&$l$9lambda$methods);
    }
    {
        $l$10lambda$methods.$GCINFO = "$l$10lambda";
        $l$10lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        $l$10lambda$methods.__init__ = $l$10lambda$__init__;
        $l$10lambda$methods.__call__ = $l$10lambda$__call__;
        $l$10lambda$methods.__serialize__ = $l$10lambda$__serialize__;
        $l$10lambda$methods.__deserialize__ = $l$10lambda$__deserialize__;
        $register(&$l$10lambda$methods);
    }
    {
        $l$11lambda$methods.$GCINFO = "$l$11lambda";
        $l$11lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        $l$11lambda$methods.__init__ = $l$11lambda$__init__;
        $l$11lambda$methods.__call__ = $l$11lambda$__call__;
        $l$11lambda$methods.__serialize__ = $l$11lambda$__serialize__;
        $l$11lambda$methods.__deserialize__ = $l$11lambda$__deserialize__;
        $register(&$l$11lambda$methods);
    }
    {
        $l$12lambda$methods.$GCINFO = "$l$12lambda";
        $l$12lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        $l$12lambda$methods.__init__ = $l$12lambda$__init__;
        $l$12lambda$methods.__call__ = $l$12lambda$__call__;
        $l$12lambda$methods.__serialize__ = $l$12lambda$__serialize__;
        $l$12lambda$methods.__deserialize__ = $l$12lambda$__deserialize__;
        $register(&$l$12lambda$methods);
    }
    {
        $l$13lambda$methods.$GCINFO = "$l$13lambda";
        $l$13lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        $l$13lambda$methods.__init__ = $l$13lambda$__init__;
        $l$13lambda$methods.__call__ = $l$13lambda$__call__;
        $l$13lambda$methods.__serialize__ = $l$13lambda$__serialize__;
        $l$13lambda$methods.__deserialize__ = $l$13lambda$__deserialize__;
        $register(&$l$13lambda$methods);
    }
    {
        $l$14lambda$methods.$GCINFO = "$l$14lambda";
        $l$14lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        $l$14lambda$methods.__init__ = $l$14lambda$__init__;
        $l$14lambda$methods.__call__ = $l$14lambda$__call__;
        $l$14lambda$methods.__serialize__ = $l$14lambda$__serialize__;
        $l$14lambda$methods.__deserialize__ = $l$14lambda$__deserialize__;
        $register(&$l$14lambda$methods);
    }
    {
        $l$15lambda$methods.$GCINFO = "$l$15lambda";
        $l$15lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        $l$15lambda$methods.__init__ = $l$15lambda$__init__;
        $l$15lambda$methods.__call__ = $l$15lambda$__call__;
        $l$15lambda$methods.__serialize__ = $l$15lambda$__serialize__;
        $l$15lambda$methods.__deserialize__ = $l$15lambda$__deserialize__;
        $register(&$l$15lambda$methods);
    }
    {
        $l$16lambda$methods.$GCINFO = "$l$16lambda";
        $l$16lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        $l$16lambda$methods.__init__ = $l$16lambda$__init__;
        $l$16lambda$methods.__call__ = $l$16lambda$__call__;
        $l$16lambda$methods.__serialize__ = $l$16lambda$__serialize__;
        $l$16lambda$methods.__deserialize__ = $l$16lambda$__deserialize__;
        $register(&$l$16lambda$methods);
    }
    {
        $WorldAuth$methods.$GCINFO = "$WorldAuth";
        $WorldAuth$methods.$superclass = ($Super$class)&$value$methods;
        ;
        $WorldAuth$methods.__init__ = $WorldAuth$__init__;
        $WorldAuth$methods.__serialize__ = $WorldAuth$__serialize__;
        $WorldAuth$methods.__deserialize__ = $WorldAuth$__deserialize__;
        $register(&$WorldAuth$methods);
    }
    {
        $Env$methods.$GCINFO = "$Env";
        $Env$methods.$superclass = ($Super$class)&$Actor$methods;
        $Env$methods.__bool__ = ($bool (*) ($Env))$Actor$methods.__bool__;
        $Env$methods.__str__ = ($str (*) ($Env))$Actor$methods.__str__;
        $Env$methods.__repr__ = ($str (*) ($Env))$Actor$methods.__repr__;
        $Env$methods.__resume__ = ($NoneType (*) ($Env))$Actor$methods.__resume__;
        $Env$methods.__init__ = $Env$__init__;
        $Env$methods.stdout_write$local = $Env$stdout_write$local;
        $Env$methods.stdin_install$local = $Env$stdin_install$local;
        $Env$methods.connect$local = $Env$connect$local;
        $Env$methods.listen$local = $Env$listen$local;
        $Env$methods.exit$local = $Env$exit$local;
        $Env$methods.openR$local = $Env$openR$local;
        $Env$methods.openW$local = $Env$openW$local;
        $Env$methods.stdout_write = $Env$stdout_write;
        $Env$methods.stdin_install = $Env$stdin_install;
        $Env$methods.connect = $Env$connect;
        $Env$methods.listen = $Env$listen;
        $Env$methods.exit = $Env$exit;
        $Env$methods.openR = $Env$openR;
        $Env$methods.openW = $Env$openW;
        $Env$methods.__serialize__ = $Env$__serialize__;
        $Env$methods.__deserialize__ = $Env$__deserialize__;
        $register(&$Env$methods);
    }
    {
        $Connection$methods.$GCINFO = "$Connection";
        $Connection$methods.$superclass = ($Super$class)&$Actor$methods;
        $Connection$methods.__bool__ = ($bool (*) ($Connection))$Actor$methods.__bool__;
        $Connection$methods.__str__ = ($str (*) ($Connection))$Actor$methods.__str__;
        $Connection$methods.__resume__ = ($NoneType (*) ($Connection))$Connection$__resume__;
        $Connection$methods.__repr__ = ($str (*) ($Connection))$Actor$methods.__repr__;
        $Connection$methods.__init__ = $Connection$__init__;
        $Connection$methods.write$local = $Connection$write$local;
        $Connection$methods.close$local = $Connection$close$local;
        $Connection$methods.on_receive$local = $Connection$on_receive$local;
        $Connection$methods.write = $Connection$write;
        $Connection$methods.close = $Connection$close;
        $Connection$methods.on_receive = $Connection$on_receive;
        $Connection$methods.__serialize__ = $Connection$__serialize__;
        $Connection$methods.__deserialize__ = $Connection$__deserialize__;
        $register(&$Connection$methods);
    }
    {
        $RFile$methods.$GCINFO = "$RFile";
        $RFile$methods.$superclass = ($Super$class)&$Actor$methods;
        $RFile$methods.__bool__ = ($bool (*) ($RFile))$Actor$methods.__bool__;
        $RFile$methods.__str__ = ($str (*) ($RFile))$Actor$methods.__str__;
        $RFile$methods.__repr__ = ($str (*) ($RFile))$Actor$methods.__repr__;
        $RFile$methods.__resume__ = ($NoneType (*) ($RFile))$Actor$methods.__resume__;
        $RFile$methods.__init__ = $RFile$__init__;
        $RFile$methods.readln$local = $RFile$readln$local;
        $RFile$methods.close$local = $RFile$close$local;
        $RFile$methods.readln = $RFile$readln;
        $RFile$methods.close = $RFile$close;
        $RFile$methods.__serialize__ = $RFile$__serialize__;
        $RFile$methods.__deserialize__ = $RFile$__deserialize__;
        $register(&$RFile$methods);
    }
    {
        $WFile$methods.$GCINFO = "$WFile";
        $WFile$methods.$superclass = ($Super$class)&$Actor$methods;
        $WFile$methods.__bool__ = ($bool (*) ($WFile))$Actor$methods.__bool__;
        $WFile$methods.__str__ = ($str (*) ($WFile))$Actor$methods.__str__;
        $WFile$methods.__repr__ = ($str (*) ($WFile))$Actor$methods.__repr__;
        $WFile$methods.__resume__ = ($NoneType (*) ($WFile))$Actor$methods.__resume__;
        $WFile$methods.__init__ = $WFile$__init__;
        $WFile$methods.write$local = $WFile$write$local;
        $WFile$methods.close$local = $WFile$close$local;
        $WFile$methods.write = $WFile$write;
        $WFile$methods.close = $WFile$close;
        $WFile$methods.__serialize__ = $WFile$__serialize__;
        $WFile$methods.__deserialize__ = $WFile$__deserialize__;
        $register(&$WFile$methods);
    }
    {
        $ListenSocket$methods.$GCINFO = "$ListenSocket";
        $ListenSocket$methods.$superclass = ($Super$class)&$Actor$methods;
        $ListenSocket$methods.__bool__ = ($bool (*) ($ListenSocket))$Actor$methods.__bool__;
        $ListenSocket$methods.__str__ = ($str (*) ($ListenSocket))$Actor$methods.__str__;
        $ListenSocket$methods.__resume__ = ($NoneType (*) ($ListenSocket))$ListenSocket$__resume__; // XXX: manually modified, do not touch
        $ListenSocket$methods.__repr__ = ($str (*) ($ListenSocket))$Actor$methods.__repr__;
        $ListenSocket$methods.__init__ = $ListenSocket$__init__;
        $ListenSocket$methods.close$local = $ListenSocket$close$local;
        $ListenSocket$methods.close = $ListenSocket$close;
        $ListenSocket$methods.__serialize__ = $ListenSocket$__serialize__;
        $ListenSocket$methods.__deserialize__ = $ListenSocket$__deserialize__;
        $register(&$ListenSocket$methods);
    }
    // END GENERATED __builtin__.act $__init__
    ///////////////////////////////////////////////////////////////////////////////////////
    int r = pipe(wakeup_pipe);
    EVENT_init();
}

void *$eventloop(void *arg) {
#if defined(IS_MACOS)
    pthread_setname_np("IO");
#else
    pthread_setname_np(pthread_self(), "IO");
#endif

    pthread_setspecific(self_key, NULL);
    while(1) {
        EVENT_type kev;                                                          // struct epoll_event epev;

        struct sockaddr_in addr;
        socklen_t socklen = sizeof(addr);
        int fd2;
        int count;

        // Blocking call
        int nready = EVENT_wait(&kev, NULL);

        if (nready<0) {
            fprintf(stderr, "EVENT error: %s\n", strerror(errno));
            continue;
        }
        if (nready == 0) {
            continue;
        }
        if (EVENT_is_error(&kev)) {
            fprintf(stderr, "EVENT error: %s\n", strerror(EVENT_errno(&kev)));
            continue;
        }
        if (EVENT_is_wakeup(&kev)) {
            char dummy;
            int r = read(wakeup_pipe[0], &dummy, 1);      // Consume dummy data, reset timer at the start of next turn
            continue;
        }
        int fd = EVENT_fd(&kev);
        if (EVENT_is_eof(&kev)) {
            $str msg = $Times$str$witness->$class->__add__($Times$str$witness,$getName(fd),to$str(" closed connection\n"));
            if (fd_data[fd].errhandler)
                fd_data[fd].errhandler->$class ->__call__(fd_data[fd].errhandler,fd_data[fd].conn,msg);
            else {
                perror("Remote host closed connection");
                exit(-1);
            }
            EVENT_del_read(fd);
            $init_FileDescriptorData(fd);
            close(fd);
        } else {
            switch (fd_data[fd].kind) {
            case connecthandler:
                if (EVENT_is_read(&kev)) {              // we are a listener and someone tries to connect
                    while ((fd2 = accept(fd, (struct sockaddr *)&fd_data[fd].sock_addr,&socklen)) != -1) {
                        fcntl(fd2,F_SETFL,O_NONBLOCK);
                        fd_data[fd2].kind = connecthandler;
                        fd_data[fd2].chandler = fd_data[fd].chandler;
                        fd_data[fd2].sock_addr = fd_data[fd].sock_addr;
                        bzero(fd_data[fd2].buffer,BUF_SIZE);
                        EVENT_add_read(fd2);
                        EVENT_mod_read_once(fd);
                        setupConnection(fd2);
                    }
                } else { // we are a client and a delayed connection attempt has succeeded
#ifdef IS_GNU_LINUX
                    // Need to clear write bit in epoll
                    EVENT_del_write_once(fd);
#endif
                    setupConnection(fd);
                }
                break;
            case readhandler:  // data has arrived on fd to fd_data[fd].buffer
                if (EVENT_fd_is_read(fd)) {
                    count = read(fd,&fd_data[fd].buffer,BUF_SIZE);
                    if (count < BUF_SIZE)
                        fd_data[fd].buffer[count] = 0;
                    if (fd==STDIN_FILENO)
                        fd_data[fd].rhandler->$class->__call__(fd_data[fd].rhandler, to$str(fd_data[fd].buffer));
                    else
                      fd_data[fd].rhandler->$class->__call__(fd_data[fd].rhandler, fd_data[fd].conn, to$bytes(fd_data[fd].buffer));
                } else {
                    fprintf(stderr,"internal error: readhandler/event filter mismatch on descriptor %d\n",fd);
                    exit(-1);
                }
                break;
            case nohandler:
                fprintf(stderr,"internal error: no event handler on descriptor %d\n",fd);
                exit(-1);
            }
        }

        wake_wt(0);

    }
    return NULL;
}
