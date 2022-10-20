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

#include <uv.h>
#include "env.h"

#include "../rts/io.h"
#include "../rts/log.h"

extern char rts_exit;
extern int return_val;

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
$Msg $Env$exit ($Env __self__, $int n) {
    return $ASYNC((($Actor)__self__), (($Cont)$l$5lambda$new((($Env)__self__), n)));
}
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
void read_stdin(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buf) {
    log_info("read_stdin: %p", stream->data);

    if (nread < 0){
        if (nread == UV_EOF) {
            uv_close((uv_handle_t *)stream, NULL);
        }
    } else if (nread > 0) {
        if (stream->data) {
            $function1 cb = stream->data;
            cb->$class->__call__(cb, to$bytes_len(buf->base, nread));
        }
    }

    if (buf->base)
        free(buf->base);
}
$R $Env$stdin_install$local ($Env __self__, $function cb, $Cont c$cont) {
    // This should be the only call in env that does IO stuff, so it is safe to
    // pin affinity here (and not earlier)..
    pin_actor_affinity();
    uv_tty_t *tty = malloc(sizeof(uv_tty_t));
    uv_tty_init(get_uv_loop(), tty, 0, 1);
    tty->data = cb;
    uv_read_start((uv_stream_t*)tty, alloc_buffer, read_stdin);
    return $R_CONT(c$cont, $None);
}
$R $Env$exit$local ($Env __self__, $int n, $Cont c$cont) {
    return_val = from$int(n);
    rts_shutdown();
    return $R_CONT(c$cont, $None);
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
        $Env$methods.exit$local = $Env$exit$local;
        $Env$methods.stdout_write = $Env$stdout_write;
        $Env$methods.stdin_install = $Env$stdin_install;
        $Env$methods.exit = $Env$exit;
        $Env$methods.__serialize__ = $Env$__serialize__;
        $Env$methods.__deserialize__ = $Env$__deserialize__;
        $register(&$Env$methods);
    }
    // END GENERATED __builtin__.act $__init__
    ///////////////////////////////////////////////////////////////////////////////////////
}
