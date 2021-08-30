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

#include "minienv.h"

int kq;
struct FileDescriptorData fd_data[MAX_FD];

static void $init_FileDescriptorData(int fd) {
  fd_data[fd].kind = nohandler;
  bzero(fd_data[fd].buffer,BUF_SIZE);
}

int new_socket ($function handler) {
  int fd = socket(PF_INET,SOCK_STREAM,0);
  fcntl(fd,F_SETFL,O_NONBLOCK);
  fd_data[fd].kind = connecthandler;
  fd_data[fd].chandler = handler;
  return fd;
}

void setupConnection (int fd) {
  $Connection conn = $NEW($Connection,fd);
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

$NoneType minienv$$l$1lambda$__init__ (minienv$$l$1lambda p$self, $Env __self__, $str s) {
    p$self->__self__ = __self__;
    p$self->s = s;
    return $None;
}
$R minienv$$l$1lambda$__call__ (minienv$$l$1lambda p$self, $Cont c$cont) {
    $Env __self__ = p$self->__self__;
    $str s = p$self->s;
    return __self__->$class->stdout_write$local(__self__, s, c$cont);
}
void minienv$$l$1lambda$__serialize__ (minienv$$l$1lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
    $step_serialize(self->s, state);
}
minienv$$l$1lambda minienv$$l$1lambda$__deserialize__ (minienv$$l$1lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct minienv$$l$1lambda));
            self->$class = &minienv$$l$1lambda$methods;
            return self;
        }
        self = $DNEW(minienv$$l$1lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    self->s = $step_deserialize(state);
    return self;
}
minienv$$l$1lambda minienv$$l$1lambda$new($Env p$1, $str p$2) {
    minienv$$l$1lambda $tmp = malloc(sizeof(struct minienv$$l$1lambda));
    $tmp->$class = &minienv$$l$1lambda$methods;
    minienv$$l$1lambda$methods.__init__($tmp, p$1, p$2);
    return $tmp;
}
struct minienv$$l$1lambda$class minienv$$l$1lambda$methods;
$NoneType minienv$$l$2lambda$__init__ (minienv$$l$2lambda p$self, $Env __self__, $function cb) {
    p$self->__self__ = __self__;
    p$self->cb = cb;
    return $None;
}
$R minienv$$l$2lambda$__call__ (minienv$$l$2lambda p$self, $Cont c$cont) {
    $Env __self__ = p$self->__self__;
    $function cb = p$self->cb;
    return __self__->$class->stdin_install$local(__self__, cb, c$cont);
}
void minienv$$l$2lambda$__serialize__ (minienv$$l$2lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
    $step_serialize(self->cb, state);
}
minienv$$l$2lambda minienv$$l$2lambda$__deserialize__ (minienv$$l$2lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct minienv$$l$2lambda));
            self->$class = &minienv$$l$2lambda$methods;
            return self;
        }
        self = $DNEW(minienv$$l$2lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    self->cb = $step_deserialize(state);
    return self;
}
minienv$$l$2lambda minienv$$l$2lambda$new($Env p$1, $function p$2) {
    minienv$$l$2lambda $tmp = malloc(sizeof(struct minienv$$l$2lambda));
    $tmp->$class = &minienv$$l$2lambda$methods;
    minienv$$l$2lambda$methods.__init__($tmp, p$1, p$2);
    return $tmp;
}
struct minienv$$l$2lambda$class minienv$$l$2lambda$methods;
$NoneType minienv$$l$3lambda$__init__ (minienv$$l$3lambda p$self, $Env __self__, $str host, $int port, $function cb) {
    p$self->__self__ = __self__;
    p$self->host = host;
    p$self->port = port;
    p$self->cb = cb;
    return $None;
}
$R minienv$$l$3lambda$__call__ (minienv$$l$3lambda p$self, $Cont c$cont) {
    $Env __self__ = p$self->__self__;
    $str host = p$self->host;
    $int port = p$self->port;
    $function cb = p$self->cb;
    return __self__->$class->connect$local(__self__, host, port, cb, c$cont);
}
void minienv$$l$3lambda$__serialize__ (minienv$$l$3lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
    $step_serialize(self->host, state);
    $step_serialize(self->port, state);
    $step_serialize(self->cb, state);
}
minienv$$l$3lambda minienv$$l$3lambda$__deserialize__ (minienv$$l$3lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct minienv$$l$3lambda));
            self->$class = &minienv$$l$3lambda$methods;
            return self;
        }
        self = $DNEW(minienv$$l$3lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    self->host = $step_deserialize(state);
    self->port = $step_deserialize(state);
    self->cb = $step_deserialize(state);
    return self;
}
minienv$$l$3lambda minienv$$l$3lambda$new($Env p$1, $str p$2, $int p$3, $function p$4) {
    minienv$$l$3lambda $tmp = malloc(sizeof(struct minienv$$l$3lambda));
    $tmp->$class = &minienv$$l$3lambda$methods;
    minienv$$l$3lambda$methods.__init__($tmp, p$1, p$2, p$3, p$4);
    return $tmp;
}
struct minienv$$l$3lambda$class minienv$$l$3lambda$methods;
$NoneType minienv$$l$4lambda$__init__ (minienv$$l$4lambda p$self, $Env __self__, $int port, $function cb) {
    p$self->__self__ = __self__;
    p$self->port = port;
    p$self->cb = cb;
    return $None;
}
$R minienv$$l$4lambda$__call__ (minienv$$l$4lambda p$self, $Cont c$cont) {
    $Env __self__ = p$self->__self__;
    $int port = p$self->port;
    $function cb = p$self->cb;
    return __self__->$class->listen$local(__self__, port, cb, c$cont);
}
void minienv$$l$4lambda$__serialize__ (minienv$$l$4lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
    $step_serialize(self->port, state);
    $step_serialize(self->cb, state);
}
minienv$$l$4lambda minienv$$l$4lambda$__deserialize__ (minienv$$l$4lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct minienv$$l$4lambda));
            self->$class = &minienv$$l$4lambda$methods;
            return self;
        }
        self = $DNEW(minienv$$l$4lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    self->port = $step_deserialize(state);
    self->cb = $step_deserialize(state);
    return self;
}
minienv$$l$4lambda minienv$$l$4lambda$new($Env p$1, $int p$2, $function p$3) {
    minienv$$l$4lambda $tmp = malloc(sizeof(struct minienv$$l$4lambda));
    $tmp->$class = &minienv$$l$4lambda$methods;
    minienv$$l$4lambda$methods.__init__($tmp, p$1, p$2, p$3);
    return $tmp;
}
struct minienv$$l$4lambda$class minienv$$l$4lambda$methods;
$NoneType minienv$$l$5lambda$__init__ (minienv$$l$5lambda p$self, $Env __self__, $int n) {
    p$self->__self__ = __self__;
    p$self->n = n;
    return $None;
}
$R minienv$$l$5lambda$__call__ (minienv$$l$5lambda p$self, $Cont c$cont) {
    $Env __self__ = p$self->__self__;
    $int n = p$self->n;
    return __self__->$class->exit$local(__self__, n, c$cont);
}
void minienv$$l$5lambda$__serialize__ (minienv$$l$5lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
    $step_serialize(self->n, state);
}
minienv$$l$5lambda minienv$$l$5lambda$__deserialize__ (minienv$$l$5lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct minienv$$l$5lambda));
            self->$class = &minienv$$l$5lambda$methods;
            return self;
        }
        self = $DNEW(minienv$$l$5lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    self->n = $step_deserialize(state);
    return self;
}
minienv$$l$5lambda minienv$$l$5lambda$new($Env p$1, $int p$2) {
    minienv$$l$5lambda $tmp = malloc(sizeof(struct minienv$$l$5lambda));
    $tmp->$class = &minienv$$l$5lambda$methods;
    minienv$$l$5lambda$methods.__init__($tmp, p$1, p$2);
    return $tmp;
}
struct minienv$$l$5lambda$class minienv$$l$5lambda$methods;
$NoneType minienv$$l$6lambda$__init__ (minienv$$l$6lambda p$self, $Env __self__, $str nm) {
    p$self->__self__ = __self__;
    p$self->nm = nm;
    return $None;
}
$R minienv$$l$6lambda$__call__ (minienv$$l$6lambda p$self, $Cont c$cont) {
    $Env __self__ = p$self->__self__;
    $str nm = p$self->nm;
    return __self__->$class->openR$local(__self__, nm, c$cont);
}
void minienv$$l$6lambda$__serialize__ (minienv$$l$6lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
    $step_serialize(self->nm, state);
}
minienv$$l$6lambda minienv$$l$6lambda$__deserialize__ (minienv$$l$6lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct minienv$$l$6lambda));
            self->$class = &minienv$$l$6lambda$methods;
            return self;
        }
        self = $DNEW(minienv$$l$6lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    self->nm = $step_deserialize(state);
    return self;
}
minienv$$l$6lambda minienv$$l$6lambda$new($Env p$1, $str p$2) {
    minienv$$l$6lambda $tmp = malloc(sizeof(struct minienv$$l$6lambda));
    $tmp->$class = &minienv$$l$6lambda$methods;
    minienv$$l$6lambda$methods.__init__($tmp, p$1, p$2);
    return $tmp;
}
struct minienv$$l$6lambda$class minienv$$l$6lambda$methods;
$NoneType minienv$$l$7lambda$__init__ (minienv$$l$7lambda p$self, $Env __self__, $str nm) {
    p$self->__self__ = __self__;
    p$self->nm = nm;
    return $None;
}
$R minienv$$l$7lambda$__call__ (minienv$$l$7lambda p$self, $Cont c$cont) {
    $Env __self__ = p$self->__self__;
    $str nm = p$self->nm;
    return __self__->$class->openW$local(__self__, nm, c$cont);
}
void minienv$$l$7lambda$__serialize__ (minienv$$l$7lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
    $step_serialize(self->nm, state);
}
minienv$$l$7lambda minienv$$l$7lambda$__deserialize__ (minienv$$l$7lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct minienv$$l$7lambda));
            self->$class = &minienv$$l$7lambda$methods;
            return self;
        }
        self = $DNEW(minienv$$l$7lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    self->nm = $step_deserialize(state);
    return self;
}
minienv$$l$7lambda minienv$$l$7lambda$new($Env p$1, $str p$2) {
    minienv$$l$7lambda $tmp = malloc(sizeof(struct minienv$$l$7lambda));
    $tmp->$class = &minienv$$l$7lambda$methods;
    minienv$$l$7lambda$methods.__init__($tmp, p$1, p$2);
    return $tmp;
}
struct minienv$$l$7lambda$class minienv$$l$7lambda$methods;
$NoneType minienv$$l$8lambda$__init__ (minienv$$l$8lambda p$self, $Connection __self__, $str s) {
    p$self->__self__ = __self__;
    p$self->s = s;
    return $None;
}
$R minienv$$l$8lambda$__call__ (minienv$$l$8lambda p$self, $Cont c$cont) {
    $Connection __self__ = p$self->__self__;
    $str s = p$self->s;
    return __self__->$class->write$local(__self__, s, c$cont);
}
void minienv$$l$8lambda$__serialize__ (minienv$$l$8lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
    $step_serialize(self->s, state);
}
minienv$$l$8lambda minienv$$l$8lambda$__deserialize__ (minienv$$l$8lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct minienv$$l$8lambda));
            self->$class = &minienv$$l$8lambda$methods;
            return self;
        }
        self = $DNEW(minienv$$l$8lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    self->s = $step_deserialize(state);
    return self;
}
minienv$$l$8lambda minienv$$l$8lambda$new($Connection p$1, $str p$2) {
    minienv$$l$8lambda $tmp = malloc(sizeof(struct minienv$$l$8lambda));
    $tmp->$class = &minienv$$l$8lambda$methods;
    minienv$$l$8lambda$methods.__init__($tmp, p$1, p$2);
    return $tmp;
}
struct minienv$$l$8lambda$class minienv$$l$8lambda$methods;
$NoneType minienv$$l$9lambda$__init__ (minienv$$l$9lambda p$self, $Connection __self__) {
    p$self->__self__ = __self__;
    return $None;
}
$R minienv$$l$9lambda$__call__ (minienv$$l$9lambda p$self, $Cont c$cont) {
    $Connection __self__ = p$self->__self__;
    return __self__->$class->close$local(__self__, c$cont);
}
void minienv$$l$9lambda$__serialize__ (minienv$$l$9lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
}
minienv$$l$9lambda minienv$$l$9lambda$__deserialize__ (minienv$$l$9lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct minienv$$l$9lambda));
            self->$class = &minienv$$l$9lambda$methods;
            return self;
        }
        self = $DNEW(minienv$$l$9lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    return self;
}
minienv$$l$9lambda minienv$$l$9lambda$new($Connection p$1) {
    minienv$$l$9lambda $tmp = malloc(sizeof(struct minienv$$l$9lambda));
    $tmp->$class = &minienv$$l$9lambda$methods;
    minienv$$l$9lambda$methods.__init__($tmp, p$1);
    return $tmp;
}
struct minienv$$l$9lambda$class minienv$$l$9lambda$methods;
$NoneType minienv$$l$10lambda$__init__ (minienv$$l$10lambda p$self, $Connection __self__, $function cb1, $function cb2) {
    p$self->__self__ = __self__;
    p$self->cb1 = cb1;
    p$self->cb2 = cb2;
    return $None;
}
$R minienv$$l$10lambda$__call__ (minienv$$l$10lambda p$self, $Cont c$cont) {
    $Connection __self__ = p$self->__self__;
    $function cb1 = p$self->cb1;
    $function cb2 = p$self->cb2;
    return __self__->$class->on_receipt$local(__self__, cb1, cb2, c$cont);
}
void minienv$$l$10lambda$__serialize__ (minienv$$l$10lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
    $step_serialize(self->cb1, state);
    $step_serialize(self->cb2, state);
}
minienv$$l$10lambda minienv$$l$10lambda$__deserialize__ (minienv$$l$10lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct minienv$$l$10lambda));
            self->$class = &minienv$$l$10lambda$methods;
            return self;
        }
        self = $DNEW(minienv$$l$10lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    self->cb1 = $step_deserialize(state);
    self->cb2 = $step_deserialize(state);
    return self;
}
minienv$$l$10lambda minienv$$l$10lambda$new($Connection p$1, $function p$2, $function p$3) {
    minienv$$l$10lambda $tmp = malloc(sizeof(struct minienv$$l$10lambda));
    $tmp->$class = &minienv$$l$10lambda$methods;
    minienv$$l$10lambda$methods.__init__($tmp, p$1, p$2, p$3);
    return $tmp;
}
struct minienv$$l$10lambda$class minienv$$l$10lambda$methods;
$NoneType minienv$$l$11lambda$__init__ (minienv$$l$11lambda p$self, $RFile __self__) {
    p$self->__self__ = __self__;
    return $None;
}
$R minienv$$l$11lambda$__call__ (minienv$$l$11lambda p$self, $Cont c$cont) {
    $RFile __self__ = p$self->__self__;
    return __self__->$class->readln$local(__self__, c$cont);
}
void minienv$$l$11lambda$__serialize__ (minienv$$l$11lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
}
minienv$$l$11lambda minienv$$l$11lambda$__deserialize__ (minienv$$l$11lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct minienv$$l$11lambda));
            self->$class = &minienv$$l$11lambda$methods;
            return self;
        }
        self = $DNEW(minienv$$l$11lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    return self;
}
minienv$$l$11lambda minienv$$l$11lambda$new($RFile p$1) {
    minienv$$l$11lambda $tmp = malloc(sizeof(struct minienv$$l$11lambda));
    $tmp->$class = &minienv$$l$11lambda$methods;
    minienv$$l$11lambda$methods.__init__($tmp, p$1);
    return $tmp;
}
struct minienv$$l$11lambda$class minienv$$l$11lambda$methods;
$NoneType minienv$$l$12lambda$__init__ (minienv$$l$12lambda p$self, $RFile __self__) {
    p$self->__self__ = __self__;
    return $None;
}
$R minienv$$l$12lambda$__call__ (minienv$$l$12lambda p$self, $Cont c$cont) {
    $RFile __self__ = p$self->__self__;
    return __self__->$class->close$local(__self__, c$cont);
}
void minienv$$l$12lambda$__serialize__ (minienv$$l$12lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
}
minienv$$l$12lambda minienv$$l$12lambda$__deserialize__ (minienv$$l$12lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct minienv$$l$12lambda));
            self->$class = &minienv$$l$12lambda$methods;
            return self;
        }
        self = $DNEW(minienv$$l$12lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    return self;
}
minienv$$l$12lambda minienv$$l$12lambda$new($RFile p$1) {
    minienv$$l$12lambda $tmp = malloc(sizeof(struct minienv$$l$12lambda));
    $tmp->$class = &minienv$$l$12lambda$methods;
    minienv$$l$12lambda$methods.__init__($tmp, p$1);
    return $tmp;
}
struct minienv$$l$12lambda$class minienv$$l$12lambda$methods;
$NoneType minienv$$l$13lambda$__init__ (minienv$$l$13lambda p$self, $WFile __self__, $str s) {
    p$self->__self__ = __self__;
    p$self->s = s;
    return $None;
}
$R minienv$$l$13lambda$__call__ (minienv$$l$13lambda p$self, $Cont c$cont) {
    $WFile __self__ = p$self->__self__;
    $str s = p$self->s;
    return __self__->$class->write$local(__self__, s, c$cont);
}
void minienv$$l$13lambda$__serialize__ (minienv$$l$13lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
    $step_serialize(self->s, state);
}
minienv$$l$13lambda minienv$$l$13lambda$__deserialize__ (minienv$$l$13lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct minienv$$l$13lambda));
            self->$class = &minienv$$l$13lambda$methods;
            return self;
        }
        self = $DNEW(minienv$$l$13lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    self->s = $step_deserialize(state);
    return self;
}
minienv$$l$13lambda minienv$$l$13lambda$new($WFile p$1, $str p$2) {
    minienv$$l$13lambda $tmp = malloc(sizeof(struct minienv$$l$13lambda));
    $tmp->$class = &minienv$$l$13lambda$methods;
    minienv$$l$13lambda$methods.__init__($tmp, p$1, p$2);
    return $tmp;
}
struct minienv$$l$13lambda$class minienv$$l$13lambda$methods;
$NoneType minienv$$l$14lambda$__init__ (minienv$$l$14lambda p$self, $WFile __self__) {
    p$self->__self__ = __self__;
    return $None;
}
$R minienv$$l$14lambda$__call__ (minienv$$l$14lambda p$self, $Cont c$cont) {
    $WFile __self__ = p$self->__self__;
    return __self__->$class->close$local(__self__, c$cont);
}
void minienv$$l$14lambda$__serialize__ (minienv$$l$14lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
}
minienv$$l$14lambda minienv$$l$14lambda$__deserialize__ (minienv$$l$14lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct minienv$$l$14lambda));
            self->$class = &minienv$$l$14lambda$methods;
            return self;
        }
        self = $DNEW(minienv$$l$14lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    return self;
}
minienv$$l$14lambda minienv$$l$14lambda$new($WFile p$1) {
    minienv$$l$14lambda $tmp = malloc(sizeof(struct minienv$$l$14lambda));
    $tmp->$class = &minienv$$l$14lambda$methods;
    minienv$$l$14lambda$methods.__init__($tmp, p$1);
    return $tmp;
}
struct minienv$$l$14lambda$class minienv$$l$14lambda$methods;
$NoneType $Env$__init__ ($Env __self__, $list argv) {
    $Actor$methods.__init__((($Actor)__self__));
    __self__->argv = argv;
    return $None;
}
$R $Env$stdout_write$local ($Env __self__, $str s, $Cont c$cont) {
    printf("%s",s->str);
    return $R_CONT(c$cont, $None);
}
$R $Env$stdin_install$local ($Env __self__, $function cb, $Cont c$cont) {
    fd_data[STDIN_FILENO].kind = readhandler;
    fd_data[STDIN_FILENO].rhandler = cb;
    EV_SET(&fd_data[STDIN_FILENO].event_spec,STDIN_FILENO,EVFILT_READ,EV_ADD,0,0,NULL);
    kevent(kq,&fd_data[STDIN_FILENO].event_spec,1,NULL,0,NULL);
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
          EV_SET(&fd_data[fd].event_spec,fd,EVFILT_WRITE,EV_ADD | EV_ONESHOT,0,0,NULL);
          kevent(kq,&fd_data[fd].event_spec,1,NULL,0,NULL);
        } else {
          fd_data[fd].chandler->$class->__call__(fd_data[fd].chandler, NULL);
          //fprintf(stderr,"Connect failed");
         }
      } else // connect succeeded immediately (can this ever happen for a non-blocking socket?)
        setupConnection(fd);
    }
    return $R_CONT(c$cont, $None);
}
$R $Env$listen$local ($Env __self__, $int port, $function cb, $Cont c$cont) {
    struct sockaddr_in addr;
    int fd = new_socket(cb);
    addr.sin_addr.s_addr = INADDR_ANY;
    addr.sin_port = htons(port->val);
    addr.sin_family = AF_INET;
    if (bind(fd,(struct sockaddr *)&addr,sizeof(struct sockaddr)) < 0)
      fd_data[fd].chandler->$class->__call__(fd_data[fd].chandler, NULL);
    listen(fd,5);
    EV_SET(&fd_data[fd].event_spec,fd,EVFILT_READ,EV_ADD | EV_ONESHOT,0,0,NULL);
    kevent(kq,&fd_data[fd].event_spec,1,NULL,0,NULL);
    return $R_CONT(c$cont, $None);
}
$R $Env$exit$local ($Env __self__, $int n, $Cont c$cont) {
    exit(n->val);
    return $R_CONT(c$cont, $None);
}
$R $Env$openR$local ($Env __self__, $str nm, $Cont c$cont) {
    FILE *file = fopen((char *)nm->str,"r");
    if (file)
        return $RFile$new(file, c$cont);
    else
        return $R_CONT(c$cont, $None);
}
$R $Env$openW$local ($Env __self__, $str nm, $Cont c$cont) {
    int descr = open((char *)nm->str, O_WRONLY | O_CREAT | O_APPEND, S_IWUSR|S_IRUSR|S_IRGRP|S_IROTH);
    if (descr < 0)
        return $R_CONT(c$cont, $None);
    else
        return $WFile$new(descr, c$cont);
}
$Msg $Env$stdout_write ($Env __self__, $str s) {
    return $ASYNC((($Actor)__self__), (($Cont)minienv$$l$1lambda$new(__self__, s)));
}
$Msg $Env$stdin_install ($Env __self__, $function cb) {
    return $ASYNC((($Actor)__self__), (($Cont)minienv$$l$2lambda$new(__self__, cb)));
}
$Msg $Env$connect ($Env __self__, $str host, $int port, $function cb) {
    return $ASYNC((($Actor)__self__), (($Cont)minienv$$l$3lambda$new(__self__, host, port, cb)));
}
$Msg $Env$listen ($Env __self__, $int port, $function cb) {
    return $ASYNC((($Actor)__self__), (($Cont)minienv$$l$4lambda$new(__self__, port, cb)));
}
$Msg $Env$exit ($Env __self__, $int n) {
    return $ASYNC((($Actor)__self__), (($Cont)minienv$$l$5lambda$new(__self__, n)));
}
$Msg $Env$openR ($Env __self__, $str nm) {
    return $ASYNC((($Actor)__self__), (($Cont)minienv$$l$6lambda$new(__self__, nm)));
}
$Msg $Env$openW ($Env __self__, $str nm) {
    return $ASYNC((($Actor)__self__), (($Cont)minienv$$l$7lambda$new(__self__, nm)));
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
$R $Env$new($list p$1, $Cont p$2) {
    $Env $tmp = malloc(sizeof(struct $Env));
    $tmp->$class = &$Env$methods;
    $Env$methods.__init__($tmp, p$1);
    return $R_CONT(p$2, $tmp);
}
struct $Env$class $Env$methods;
$NoneType $Connection$__init__ ($Connection __self__, int descr) {
    $Actor$methods.__init__((($Actor)__self__));
    __self__->descriptor = descr;
    return $None;
}
$R $Connection$write$local ($Connection __self__, $str s, $Cont c$cont) {
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
$R $Connection$on_receipt$local ($Connection __self__, $function cb1, $function cb2, $Cont c$cont) {
    fd_data[__self__->descriptor].kind = readhandler;
    fd_data[__self__->descriptor].rhandler = cb1;
    fd_data[__self__->descriptor].errhandler = cb2;
    EV_SET(&fd_data[__self__->descriptor].event_spec,__self__->descriptor,EVFILT_READ,EV_ADD,0,0,NULL);
    kevent(kq,&fd_data[__self__->descriptor].event_spec,1,NULL,0,NULL);
    return $R_CONT(c$cont, $None);
}
$Msg $Connection$write ($Connection __self__, $str s) {
    return $ASYNC((($Actor)__self__), (($Cont)minienv$$l$8lambda$new(__self__, s)));
}
$Msg $Connection$close ($Connection __self__) {
    return $ASYNC((($Actor)__self__), (($Cont)minienv$$l$9lambda$new(__self__)));
}
$Msg $Connection$on_receipt ($Connection __self__, $function cb1, $function cb2) {
    return $ASYNC((($Actor)__self__), (($Cont)minienv$$l$10lambda$new(__self__, cb1, cb2)));
}
void $Connection$__serialize__ ($Connection self, $Serial$state state) {
    $Actor$methods.__serialize__(($Actor)self, state);
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
    return self;
}
$R $Connection$new(int descr, $Cont p$1) {
    $Connection $tmp = malloc(sizeof(struct $Connection));
    $tmp->$class = &$Connection$methods;
    $Connection$methods.__init__($tmp, descr);
    return $R_CONT(p$1, $tmp);
}
struct $Connection$class $Connection$methods;
$R $RFile$__init__ ($RFile __self__, FILE *file, $Cont c$cont) {
    $Actor$methods.__init__((($Actor)__self__));
    __self__->file = file;
    $NEWACT((($Actor)__self__));
    return $R_CONT(c$cont, $None);
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
$Msg $RFile$readln ($RFile __self__) {
    return $ASYNC((($Actor)__self__), (($Cont)minienv$$l$11lambda$new(__self__)));
}
$Msg $RFile$close ($RFile __self__) {
    return $ASYNC((($Actor)__self__), (($Cont)minienv$$l$12lambda$new(__self__)));
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
$R $RFile$new(FILE *file, $Cont p$1) {
    $RFile $tmp = malloc(sizeof(struct $RFile));
    $tmp->$class = &$RFile$methods;
    return $RFile$methods.__init__($tmp, file, $CONSTCONT($tmp, p$1));
}
struct $RFile$class $RFile$methods;
$R $WFile$__init__ ($WFile __self__, int descr, $Cont c$cont) {
    $Actor$methods.__init__((($Actor)__self__));
    __self__->descriptor = descr;
    $NEWACT((($Actor)__self__));
    return $R_CONT(c$cont, $None);
}
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
$Msg $WFile$write ($WFile __self__, $str s) {
    return $ASYNC((($Actor)__self__), (($Cont)minienv$$l$13lambda$new(__self__, s)));
}
$Msg $WFile$close ($WFile __self__) {
    return $ASYNC((($Actor)__self__), (($Cont)minienv$$l$14lambda$new(__self__)));
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
$R $WFile$new(int descr, $Cont p$1) {
    $WFile $tmp = malloc(sizeof(struct $WFile));
    $tmp->$class = &$WFile$methods;
    return $WFile$methods.__init__($tmp, descr, $CONSTCONT($tmp, p$1));
}
struct $WFile$class $WFile$methods;
int minienv$$done$ = 0;
void minienv$$__init__ () {
    if (minienv$$done$) return;
    minienv$$done$ = 1;
    {
        minienv$$l$1lambda$methods.$GCINFO = "minienv$$l$1lambda";
        minienv$$l$1lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        minienv$$l$1lambda$methods.__bool__ = ($bool (*) (minienv$$l$1lambda))$value$methods.__bool__;
        minienv$$l$1lambda$methods.__str__ = ($str (*) (minienv$$l$1lambda))$value$methods.__str__;
        minienv$$l$1lambda$methods.__init__ = minienv$$l$1lambda$__init__;
        minienv$$l$1lambda$methods.__call__ = minienv$$l$1lambda$__call__;
        minienv$$l$1lambda$methods.__serialize__ = minienv$$l$1lambda$__serialize__;
        minienv$$l$1lambda$methods.__deserialize__ = minienv$$l$1lambda$__deserialize__;
        $register(&minienv$$l$1lambda$methods);
    }
    {
        minienv$$l$2lambda$methods.$GCINFO = "minienv$$l$2lambda";
        minienv$$l$2lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        minienv$$l$2lambda$methods.__bool__ = ($bool (*) (minienv$$l$2lambda))$value$methods.__bool__;
        minienv$$l$2lambda$methods.__str__ = ($str (*) (minienv$$l$2lambda))$value$methods.__str__;
        minienv$$l$2lambda$methods.__init__ = minienv$$l$2lambda$__init__;
        minienv$$l$2lambda$methods.__call__ = minienv$$l$2lambda$__call__;
        minienv$$l$2lambda$methods.__serialize__ = minienv$$l$2lambda$__serialize__;
        minienv$$l$2lambda$methods.__deserialize__ = minienv$$l$2lambda$__deserialize__;
        $register(&minienv$$l$2lambda$methods);
    }
    {
        minienv$$l$3lambda$methods.$GCINFO = "minienv$$l$3lambda";
        minienv$$l$3lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        minienv$$l$3lambda$methods.__bool__ = ($bool (*) (minienv$$l$3lambda))$value$methods.__bool__;
        minienv$$l$3lambda$methods.__str__ = ($str (*) (minienv$$l$3lambda))$value$methods.__str__;
        minienv$$l$3lambda$methods.__init__ = minienv$$l$3lambda$__init__;
        minienv$$l$3lambda$methods.__call__ = minienv$$l$3lambda$__call__;
        minienv$$l$3lambda$methods.__serialize__ = minienv$$l$3lambda$__serialize__;
        minienv$$l$3lambda$methods.__deserialize__ = minienv$$l$3lambda$__deserialize__;
        $register(&minienv$$l$3lambda$methods);
    }
    {
        minienv$$l$4lambda$methods.$GCINFO = "minienv$$l$4lambda";
        minienv$$l$4lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        minienv$$l$4lambda$methods.__bool__ = ($bool (*) (minienv$$l$4lambda))$value$methods.__bool__;
        minienv$$l$4lambda$methods.__str__ = ($str (*) (minienv$$l$4lambda))$value$methods.__str__;
        minienv$$l$4lambda$methods.__init__ = minienv$$l$4lambda$__init__;
        minienv$$l$4lambda$methods.__call__ = minienv$$l$4lambda$__call__;
        minienv$$l$4lambda$methods.__serialize__ = minienv$$l$4lambda$__serialize__;
        minienv$$l$4lambda$methods.__deserialize__ = minienv$$l$4lambda$__deserialize__;
        $register(&minienv$$l$4lambda$methods);
    }
    {
        minienv$$l$5lambda$methods.$GCINFO = "minienv$$l$5lambda";
        minienv$$l$5lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        minienv$$l$5lambda$methods.__bool__ = ($bool (*) (minienv$$l$5lambda))$value$methods.__bool__;
        minienv$$l$5lambda$methods.__str__ = ($str (*) (minienv$$l$5lambda))$value$methods.__str__;
        minienv$$l$5lambda$methods.__init__ = minienv$$l$5lambda$__init__;
        minienv$$l$5lambda$methods.__call__ = minienv$$l$5lambda$__call__;
        minienv$$l$5lambda$methods.__serialize__ = minienv$$l$5lambda$__serialize__;
        minienv$$l$5lambda$methods.__deserialize__ = minienv$$l$5lambda$__deserialize__;
        $register(&minienv$$l$5lambda$methods);
    }
    {
        minienv$$l$6lambda$methods.$GCINFO = "minienv$$l$6lambda";
        minienv$$l$6lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        minienv$$l$6lambda$methods.__bool__ = ($bool (*) (minienv$$l$6lambda))$value$methods.__bool__;
        minienv$$l$6lambda$methods.__str__ = ($str (*) (minienv$$l$6lambda))$value$methods.__str__;
        minienv$$l$6lambda$methods.__init__ = minienv$$l$6lambda$__init__;
        minienv$$l$6lambda$methods.__call__ = minienv$$l$6lambda$__call__;
        minienv$$l$6lambda$methods.__serialize__ = minienv$$l$6lambda$__serialize__;
        minienv$$l$6lambda$methods.__deserialize__ = minienv$$l$6lambda$__deserialize__;
        $register(&minienv$$l$6lambda$methods);
    }
    {
        minienv$$l$7lambda$methods.$GCINFO = "minienv$$l$7lambda";
        minienv$$l$7lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        minienv$$l$7lambda$methods.__bool__ = ($bool (*) (minienv$$l$7lambda))$value$methods.__bool__;
        minienv$$l$7lambda$methods.__str__ = ($str (*) (minienv$$l$7lambda))$value$methods.__str__;
        minienv$$l$7lambda$methods.__init__ = minienv$$l$7lambda$__init__;
        minienv$$l$7lambda$methods.__call__ = minienv$$l$7lambda$__call__;
        minienv$$l$7lambda$methods.__serialize__ = minienv$$l$7lambda$__serialize__;
        minienv$$l$7lambda$methods.__deserialize__ = minienv$$l$7lambda$__deserialize__;
        $register(&minienv$$l$7lambda$methods);
    }
    {
        minienv$$l$8lambda$methods.$GCINFO = "minienv$$l$8lambda";
        minienv$$l$8lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        minienv$$l$8lambda$methods.__bool__ = ($bool (*) (minienv$$l$8lambda))$value$methods.__bool__;
        minienv$$l$8lambda$methods.__str__ = ($str (*) (minienv$$l$8lambda))$value$methods.__str__;
        minienv$$l$8lambda$methods.__init__ = minienv$$l$8lambda$__init__;
        minienv$$l$8lambda$methods.__call__ = minienv$$l$8lambda$__call__;
        minienv$$l$8lambda$methods.__serialize__ = minienv$$l$8lambda$__serialize__;
        minienv$$l$8lambda$methods.__deserialize__ = minienv$$l$8lambda$__deserialize__;
        $register(&minienv$$l$8lambda$methods);
    }
    {
        minienv$$l$9lambda$methods.$GCINFO = "minienv$$l$9lambda";
        minienv$$l$9lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        minienv$$l$9lambda$methods.__bool__ = ($bool (*) (minienv$$l$9lambda))$value$methods.__bool__;
        minienv$$l$9lambda$methods.__str__ = ($str (*) (minienv$$l$9lambda))$value$methods.__str__;
        minienv$$l$9lambda$methods.__init__ = minienv$$l$9lambda$__init__;
        minienv$$l$9lambda$methods.__call__ = minienv$$l$9lambda$__call__;
        minienv$$l$9lambda$methods.__serialize__ = minienv$$l$9lambda$__serialize__;
        minienv$$l$9lambda$methods.__deserialize__ = minienv$$l$9lambda$__deserialize__;
        $register(&minienv$$l$9lambda$methods);
    }
    {
        minienv$$l$10lambda$methods.$GCINFO = "minienv$$l$10lambda";
        minienv$$l$10lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        minienv$$l$10lambda$methods.__bool__ = ($bool (*) (minienv$$l$10lambda))$value$methods.__bool__;
        minienv$$l$10lambda$methods.__str__ = ($str (*) (minienv$$l$10lambda))$value$methods.__str__;
        minienv$$l$10lambda$methods.__init__ = minienv$$l$10lambda$__init__;
        minienv$$l$10lambda$methods.__call__ = minienv$$l$10lambda$__call__;
        minienv$$l$10lambda$methods.__serialize__ = minienv$$l$10lambda$__serialize__;
        minienv$$l$10lambda$methods.__deserialize__ = minienv$$l$10lambda$__deserialize__;
        $register(&minienv$$l$10lambda$methods);
    }
    {
        minienv$$l$11lambda$methods.$GCINFO = "minienv$$l$11lambda";
        minienv$$l$11lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        minienv$$l$11lambda$methods.__bool__ = ($bool (*) (minienv$$l$11lambda))$value$methods.__bool__;
        minienv$$l$11lambda$methods.__str__ = ($str (*) (minienv$$l$11lambda))$value$methods.__str__;
        minienv$$l$11lambda$methods.__init__ = minienv$$l$11lambda$__init__;
        minienv$$l$11lambda$methods.__call__ = minienv$$l$11lambda$__call__;
        minienv$$l$11lambda$methods.__serialize__ = minienv$$l$11lambda$__serialize__;
        minienv$$l$11lambda$methods.__deserialize__ = minienv$$l$11lambda$__deserialize__;
        $register(&minienv$$l$11lambda$methods);
    }
    {
        minienv$$l$12lambda$methods.$GCINFO = "minienv$$l$12lambda";
        minienv$$l$12lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        minienv$$l$12lambda$methods.__bool__ = ($bool (*) (minienv$$l$12lambda))$value$methods.__bool__;
        minienv$$l$12lambda$methods.__str__ = ($str (*) (minienv$$l$12lambda))$value$methods.__str__;
        minienv$$l$12lambda$methods.__init__ = minienv$$l$12lambda$__init__;
        minienv$$l$12lambda$methods.__call__ = minienv$$l$12lambda$__call__;
        minienv$$l$12lambda$methods.__serialize__ = minienv$$l$12lambda$__serialize__;
        minienv$$l$12lambda$methods.__deserialize__ = minienv$$l$12lambda$__deserialize__;
        $register(&minienv$$l$12lambda$methods);
    }
    {
        minienv$$l$13lambda$methods.$GCINFO = "minienv$$l$13lambda";
        minienv$$l$13lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        minienv$$l$13lambda$methods.__bool__ = ($bool (*) (minienv$$l$13lambda))$value$methods.__bool__;
        minienv$$l$13lambda$methods.__str__ = ($str (*) (minienv$$l$13lambda))$value$methods.__str__;
        minienv$$l$13lambda$methods.__init__ = minienv$$l$13lambda$__init__;
        minienv$$l$13lambda$methods.__call__ = minienv$$l$13lambda$__call__;
        minienv$$l$13lambda$methods.__serialize__ = minienv$$l$13lambda$__serialize__;
        minienv$$l$13lambda$methods.__deserialize__ = minienv$$l$13lambda$__deserialize__;
        $register(&minienv$$l$13lambda$methods);
    }
    {
        minienv$$l$14lambda$methods.$GCINFO = "minienv$$l$14lambda";
        minienv$$l$14lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        minienv$$l$14lambda$methods.__bool__ = ($bool (*) (minienv$$l$14lambda))$value$methods.__bool__;
        minienv$$l$14lambda$methods.__str__ = ($str (*) (minienv$$l$14lambda))$value$methods.__str__;
        minienv$$l$14lambda$methods.__init__ = minienv$$l$14lambda$__init__;
        minienv$$l$14lambda$methods.__call__ = minienv$$l$14lambda$__call__;
        minienv$$l$14lambda$methods.__serialize__ = minienv$$l$14lambda$__serialize__;
        minienv$$l$14lambda$methods.__deserialize__ = minienv$$l$14lambda$__deserialize__;
        $register(&minienv$$l$14lambda$methods);
    }
    {
        $Env$methods.$GCINFO = "$Env";
        $Env$methods.$superclass = ($Super$class)&$Actor$methods;
        $Env$methods.__serialize__ = $Env$__serialize__;
        $Env$methods.__deserialize__ = $Env$__deserialize__;
        $Env$methods.__bool__ = ($bool (*) ($Env))$Actor$methods.__bool__;
        $Env$methods.__str__ = ($str (*) ($Env))$Actor$methods.__str__;
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
        $Connection$methods.__serialize__ = $Connection$__serialize__;
        $Connection$methods.__deserialize__ = $Connection$__deserialize__;
        $Connection$methods.__bool__ = ($bool (*) ($Connection))$Actor$methods.__bool__;
        $Connection$methods.__str__ = ($str (*) ($Connection))$Actor$methods.__str__;
        $Connection$methods.__init__ = $Connection$__init__;
        $Connection$methods.write$local = $Connection$write$local;
        $Connection$methods.close$local = $Connection$close$local;
        $Connection$methods.on_receipt$local = $Connection$on_receipt$local;
        $Connection$methods.write = $Connection$write;
        $Connection$methods.close = $Connection$close;
        $Connection$methods.on_receipt = $Connection$on_receipt;
        $Connection$methods.__serialize__ = $Connection$__serialize__;
        $Connection$methods.__deserialize__ = $Connection$__deserialize__;
        $register(&$Connection$methods);
    }
    {
        $RFile$methods.$GCINFO = "$RFile";
        $RFile$methods.$superclass = ($Super$class)&$Actor$methods;
        $RFile$methods.__bool__ = ($bool (*) ($RFile))$Actor$methods.__bool__;
        $RFile$methods.__str__ = ($str (*) ($RFile))$Actor$methods.__str__;
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
        $WFile$methods.__init__ = $WFile$__init__;
        $WFile$methods.write$local = $WFile$write$local;
        $WFile$methods.close$local = $WFile$close$local;
        $WFile$methods.write = $WFile$write;
        $WFile$methods.close = $WFile$close;
        $WFile$methods.__serialize__ = $WFile$__serialize__;
        $WFile$methods.__deserialize__ = $WFile$__deserialize__;
        $register(&$WFile$methods);
    }
}


void *$eventloop(void *arg) {
  while(1) {
    struct kevent timer;
    pthread_setspecific(self_key, NULL);
    EV_SET(&timer, 9999, EVFILT_TIMER, EV_ADD | EV_ONESHOT, 0, 500, 0);
    kevent(kq,&timer,1,0,0,0);
    struct kevent kev;
    struct sockaddr_in addr;
    socklen_t socklen = sizeof(addr);
    int fd2;
    int count;
    int nready = kevent(kq,NULL,0,&kev,1,NULL);
    if (nready<0) {
      printf("kevent error: %s. kev.ident=%lu, kq is %d\n",strerror(errno),kev.ident,kq);
      exit(-1);
    }
    int fd = kev.ident;
    if (kev.flags & EV_EOF) {
      $str msg = $Times$str$witness->$class->__add__($Times$str$witness,$getName(fd),to$str(" closed connection\n"));
      if (fd_data[fd].errhandler)
        fd_data[fd].errhandler->$class ->__call__(fd_data[fd].errhandler,msg);
      else {
        perror("Remote host closed connection");
        exit(-1);
      }
      EV_SET(&fd_data[fd].event_spec,fd,EVFILT_READ,EV_DISABLE,0,0,NULL);
      kevent(kq,&fd_data[fd].event_spec,1,NULL,0,NULL);
    }
    if (kev.flags & EV_ERROR) { 
      fprintf(stderr, "EV_ERROR: %s\n", strerror(kev.data));
      exit(-1);
    }
    if (fd==9999)
      break;
    switch (fd_data[fd].kind) {
    case connecthandler:
      if (kev.filter==EVFILT_READ) { // we are a listener and someone tries to connect
        while ((fd2 = accept(fd, (struct sockaddr *)&fd_data[fd].sock_addr,&socklen)) != -1) {
          fcntl(fd2,F_SETFL,O_NONBLOCK);
          fd_data[fd2].kind = connecthandler;
          fd_data[fd2].chandler = fd_data[fd].chandler;
          fd_data[fd2].sock_addr = fd_data[fd].sock_addr;
          bzero(fd_data[fd2].buffer,BUF_SIZE); 
          EV_SET(&fd_data[fd2].event_spec,fd2,EVFILT_READ,EV_ADD,0,0,NULL);
          kevent(kq,&fd_data[fd2].event_spec,1,NULL,0,NULL);
          kevent(kq,&fd_data[fd].event_spec,1,NULL,0,NULL);
          setupConnection(fd2);
          printf("%s %s\n","Connection from",$getName(fd2)->str);
        }
      } else { // we are a client and a delayed connection attempt has succeeded
        setupConnection(fd);
      }
      break;
    case readhandler:  // data has arrived on fd to fd_data[fd].buffer
      if (fd_data[fd].event_spec.filter == EVFILT_READ) {
        count = read(fd,&fd_data[fd].buffer,BUF_SIZE);
        if (count < BUF_SIZE)
          fd_data[fd].buffer[count] = 0;
        fd_data[fd].rhandler->$class->__call__(fd_data[fd].rhandler,to$str(fd_data[fd].buffer));
      } else {
        fprintf(stderr,"internal error: readhandler/event filter mismatch on descriptor %d\n",fd);
        exit(-1);
      }
      break;
    case nohandler:
      fprintf(stderr,"internal error: no event handler on descriptor %d\n",fd);
      exit(-1);
    }
    pthread_mutex_lock(&sleep_lock);
    pthread_cond_signal(&work_to_do);
    pthread_mutex_unlock(&sleep_lock);

  }
  return NULL;
}
