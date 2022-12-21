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
$NoneType $l$1cont$__init__ ($l$1cont p$self, $Env self, $str s) {
    p$self->self = self;
    p$self->s = s;
    return $None;
}
$R $l$1cont$__call__ ($l$1cont p$self, $Cont c$cont) {
    $Env self = p$self->self;
    $str s = p$self->s;
    return self->$class->stdout_write$local(self, c$cont, s);
}
void $l$1cont$__serialize__ ($l$1cont self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->s, state);
}
$l$1cont $l$1cont$__deserialize__ ($l$1cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct $l$1cont));
            self->$class = &$l$1cont$methods;
            return self;
        }
        self = $DNEW($l$1cont, state);
    }
    self->self = $step_deserialize(state);
    self->s = $step_deserialize(state);
    return self;
}
$l$1cont $l$1cont$new($Env p$1, $str p$2) {
    $l$1cont $tmp = malloc(sizeof(struct $l$1cont));
    $tmp->$class = &$l$1cont$methods;
    $l$1cont$methods.__init__($tmp, p$1, p$2);
    return $tmp;
}
struct $l$1cont$class $l$1cont$methods;

$NoneType $l$2cont$__init__ ($l$2cont p$self, $Env self, $action cb) {
    p$self->self = self;
    p$self->cb = cb;
    return $None;
}
$R $l$2cont$__call__ ($l$2cont p$self, $Cont c$cont) {
    $Env self = p$self->self;
    $action cb = p$self->cb;
    return self->$class->stdin_install$local(self, c$cont, cb);
}
void $l$2cont$__serialize__ ($l$2cont self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->cb, state);
}
$l$2cont $l$2cont$__deserialize__ ($l$2cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct $l$2cont));
            self->$class = &$l$2cont$methods;
            return self;
        }
        self = $DNEW($l$2cont, state);
    }
    self->self = $step_deserialize(state);
    self->cb = $step_deserialize(state);
    return self;
}
$l$2cont $l$2cont$new($Env p$1, $action p$2) {
    $l$2cont $tmp = malloc(sizeof(struct $l$2cont));
    $tmp->$class = &$l$2cont$methods;
    $l$2cont$methods.__init__($tmp, p$1, p$2);
    return $tmp;
}
struct $l$2cont$class $l$2cont$methods;

$NoneType $l$3cont$__init__ ($l$3cont p$self, $Env self, $int n) {
    p$self->self = self;
    p$self->n = n;
    return $None;
}
$R $l$3cont$__call__ ($l$3cont p$self, $Cont c$cont) {
    $Env self = p$self->self;
    $int n = p$self->n;
    return self->$class->exit$local(self, c$cont, n);
}
void $l$3cont$__serialize__ ($l$3cont self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->n, state);
}
$l$3cont $l$3cont$__deserialize__ ($l$3cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct $l$3cont));
            self->$class = &$l$3cont$methods;
            return self;
        }
        self = $DNEW($l$3cont, state);
    }
    self->self = $step_deserialize(state);
    self->n = $step_deserialize(state);
    return self;
}
$l$3cont $l$3cont$new($Env p$1, $int p$2) {
    $l$3cont $tmp = malloc(sizeof(struct $l$3cont));
    $tmp->$class = &$l$3cont$methods;
    $l$3cont$methods.__init__($tmp, p$1, p$2);
    return $tmp;
}
struct $l$3cont$class $l$3cont$methods;
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
$Msg $Env$stdout_write ($Env self, $str s) {
    return $ASYNC((($Actor)self), (($Cont)$l$1cont$new((($Env)self), s)));
}
$Msg $Env$stdin_install ($Env self, $action cb) {
    return $ASYNC((($Actor)self), (($Cont)$l$2cont$new((($Env)self), cb)));
}
$Msg $Env$exit ($Env self, $int n) {
    return $ASYNC((($Actor)self), (($Cont)$l$3cont$new((($Env)self), n)));
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

$NoneType $Env$__init__ ($Env self, $WorldAuth token, $list argv) {
    self->auth = token;
    self->argv = argv;
    self->$affinity = 0;
    return $None;
}
$R $Env$stdout_write$local ($Env self, $Cont c$cont, $str s) {
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
            $action cb = stream->data;
            cb->$class->__asyn__(cb, to$bytes_len(buf->base, nread));
        }
    }

    if (buf->base)
        free(buf->base);
}
$R $Env$stdin_install$local ($Env self, $Cont c$cont, $action cb) {
    // This should be the only call in env that does IO stuff, so it is safe to
    // pin affinity here (and not earlier)..
    pin_actor_affinity();
    uv_tty_t *tty = malloc(sizeof(uv_tty_t));
    uv_tty_init(get_uv_loop(), tty, 0, 1);
    tty->data = cb;
    uv_read_start((uv_stream_t*)tty, alloc_buffer, read_stdin);
    return $R_CONT(c$cont, $None);
}
$R $Env$exit$local ($Env self, $Cont c$cont, $int n) {
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
        $l$1cont$methods.$GCINFO = "$l$1cont";
        $l$1cont$methods.$superclass = ($Super$class)&$Cont$methods;
        $l$1cont$methods.__init__ = $l$1cont$__init__;
        $l$1cont$methods.__call__ = $l$1cont$__call__;
        $l$1cont$methods.__serialize__ = $l$1cont$__serialize__;
        $l$1cont$methods.__deserialize__ = $l$1cont$__deserialize__;
        $register(&$l$1cont$methods);
    }
    {
        $l$2cont$methods.$GCINFO = "$l$2cont";
        $l$2cont$methods.$superclass = ($Super$class)&$Cont$methods;
        $l$2cont$methods.__init__ = $l$2cont$__init__;
        $l$2cont$methods.__call__ = $l$2cont$__call__;
        $l$2cont$methods.__serialize__ = $l$2cont$__serialize__;
        $l$2cont$methods.__deserialize__ = $l$2cont$__deserialize__;
        $register(&$l$2cont$methods);
    }
    {
        $l$3cont$methods.$GCINFO = "$l$3cont";
        $l$3cont$methods.$superclass = ($Super$class)&$Cont$methods;
        $l$3cont$methods.__init__ = $l$3cont$__init__;
        $l$3cont$methods.__call__ = $l$3cont$__call__;
        $l$3cont$methods.__serialize__ = $l$3cont$__serialize__;
        $l$3cont$methods.__deserialize__ = $l$3cont$__deserialize__;
        $register(&$l$3cont$methods);
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
