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
B_NoneType $l$1contD___init__ ($l$1cont p$self, B_Env self, B_str s) {
    p$self->self = self;
    p$self->s = s;
    return B_None;
}
$R $l$1contD___call__ ($l$1cont p$self, $Cont c$cont) {
    B_Env self = p$self->self;
    B_str s = p$self->s;
    return self->$class->stdout_write$local(self, c$cont, s);
}
void $l$1contD___serialize__ ($l$1cont self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->s, state);
}
$l$1cont $l$1contD___deserialize__ ($l$1cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct $l$1cont));
            self->$class = &$l$1contG_methods;
            return self;
        }
        self = $DNEW($l$1cont, state);
    }
    self->self = $step_deserialize(state);
    self->s = $step_deserialize(state);
    return self;
}
$l$1cont $l$1contG_new(B_Env p$1, B_str p$2) {
    $l$1cont $tmp = malloc(sizeof(struct $l$1cont));
    $tmp->$class = &$l$1contG_methods;
    $l$1contG_methods.__init__($tmp, p$1, p$2);
    return $tmp;
}
struct $l$1contG_class $l$1contG_methods;

B_NoneType $l$2contD___init__ ($l$2cont p$self, B_Env self, $action cb) {
    p$self->self = self;
    p$self->cb = cb;
    return B_None;
}
$R $l$2contD___call__ ($l$2cont p$self, $Cont c$cont) {
    B_Env self = p$self->self;
    $action cb = p$self->cb;
    return self->$class->stdin_install$local(self, c$cont, cb);
}
void $l$2contD___serialize__ ($l$2cont self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->cb, state);
}
$l$2cont $l$2contD___deserialize__ ($l$2cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct $l$2cont));
            self->$class = &$l$2contG_methods;
            return self;
        }
        self = $DNEW($l$2cont, state);
    }
    self->self = $step_deserialize(state);
    self->cb = $step_deserialize(state);
    return self;
}
$l$2cont $l$2contG_new(B_Env p$1, $action p$2) {
    $l$2cont $tmp = malloc(sizeof(struct $l$2cont));
    $tmp->$class = &$l$2contG_methods;
    $l$2contG_methods.__init__($tmp, p$1, p$2);
    return $tmp;
}
struct $l$2contG_class $l$2contG_methods;

B_NoneType $l$3contD___init__ ($l$3cont p$self, B_Env self, B_int n) {
    p$self->self = self;
    p$self->n = n;
    return B_None;
}
$R $l$3contD___call__ ($l$3cont p$self, $Cont c$cont) {
    B_Env self = p$self->self;
    B_int n = p$self->n;
    return self->$class->exit$local(self, c$cont, n);
}
void $l$3contD___serialize__ ($l$3cont self, $Serial$state state) {
    $step_serialize(self->self, state);
    $step_serialize(self->n, state);
}
$l$3cont $l$3contD___deserialize__ ($l$3cont self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct $l$3cont));
            self->$class = &$l$3contG_methods;
            return self;
        }
        self = $DNEW($l$3cont, state);
    }
    self->self = $step_deserialize(state);
    self->n = $step_deserialize(state);
    return self;
}
$l$3cont $l$3contG_new(B_Env p$1, B_int p$2) {
    $l$3cont $tmp = malloc(sizeof(struct $l$3cont));
    $tmp->$class = &$l$3contG_methods;
    $l$3contG_methods.__init__($tmp, p$1, p$2);
    return $tmp;
}
struct $l$3contG_class $l$3contG_methods;
B_NoneType B_WorldAuthD___init__ (B_WorldAuth self) {
    return B_None;
}
void B_WorldAuthD___serialize__ (B_WorldAuth self, $Serial$state state) {
}
B_WorldAuth B_WorldAuthD___deserialize__ (B_WorldAuth self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_WorldAuth));
            self->$class = &B_WorldAuthG_methods;
            return self;
        }
        self = $DNEW(B_WorldAuth, state);
    }
    return self;
}
struct B_WorldAuthG_class B_WorldAuthG_methods;
B_Msg B_Env$stdout_write (B_Env self, B_str s) {
    return $ASYNC((($Actor)self), (($Cont)$l$1contG_new(((B_Env)self), s)));
}
B_Msg B_Env$stdin_install (B_Env self, $action cb) {
    return $ASYNC((($Actor)self), (($Cont)$l$2contG_new(((B_Env)self), cb)));
}
B_Msg B_Env$exit (B_Env self, B_int n) {
    return $ASYNC((($Actor)self), (($Cont)$l$3contG_new(((B_Env)self), n)));
}
// END GENERATED __builtin__.act
///////////////////////////////////////////////////////////////////////////////////////////


B_WorldAuth B_WorldAuthG_new() {
    B_WorldAuth $tmp = malloc(sizeof(struct B_WorldAuth));
    $tmp->$class = &B_WorldAuthG_methods;
    B_WorldAuthG_methods.__init__($tmp);
    return $tmp;
}

// Env /////////////////////////////////////////////////////////////////////////

B_NoneType B_EnvD___init__ (B_Env self, B_WorldAuth token, B_list argv) {
    self->auth = token;
    self->argv = argv;
    self->$affinity = 0;
    return B_None;
}
$R B_Env$stdout_write$local (B_Env self, $Cont c$cont, B_str s) {
    printf("%s", s->str);
    return $R_CONT(c$cont, B_None);
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
            cb->$class->__asyn__(cb, to$bytesD_len(buf->base, nread));
        }
    }

    if (buf->base)
        free(buf->base);
}
$R B_Env$stdin_install$local (B_Env self, $Cont c$cont, $action cb) {
    // This should be the only call in env that does IO stuff, so it is safe to
    // pin affinity here (and not earlier)..
    pin_actor_affinity();
    uv_tty_t *tty = malloc(sizeof(uv_tty_t));
    uv_tty_init(get_uv_loop(), tty, 0, 1);
    tty->data = cb;
    uv_read_start((uv_stream_t*)tty, alloc_buffer, read_stdin);
    return $R_CONT(c$cont, B_None);
}
$R B_Env$exit$local (B_Env self, $Cont c$cont, B_int n) {
    return_val = fromB_int(n);
    rts_shutdown();
    return $R_CONT(c$cont, B_None);
}
void B_EnvD___serialize__ (B_Env self, $Serial$state state) {
    $ActorG_methods.__serialize__(($Actor)self, state);
    $step_serialize(self->argv, state);
}
B_Env B_EnvD___deserialize__ (B_Env self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct B_Env));
            self->$class = &B_EnvG_methods;
            return self;
        }
        self = $DNEW(B_Env, state);
    }
    $ActorG_methods.__deserialize__(($Actor)self, state);
    self->argv = $step_deserialize(state);
    return self;
}
B_Env B_EnvG_newact(B_WorldAuth token, B_list p$1) {
    B_Env $tmp = $NEWACTOR(B_Env);
    $tmp->$class->__init__($tmp, token, p$1);  // Inline this message, note that B_EnvD___init__ is *not* CPS'ed
    serialize_state_shortcut(($Actor)$tmp);
    return $tmp;
}
struct B_EnvG_class B_EnvG_methods;


int $done$ = 0;
void D___init__ () {
    if ($done$) return;
    $done$ = 1;
    ///////////////////////////////////////////////////////////////////////////////////////
    // START GENERATED __builtin__.act D___init__
    {
        $l$1contG_methods.$GCINFO = "$l$1cont";
        $l$1contG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        $l$1contG_methods.__init__ = $l$1contD___init__;
        $l$1contG_methods.__call__ = $l$1contD___call__;
        $l$1contG_methods.__serialize__ = $l$1contD___serialize__;
        $l$1contG_methods.__deserialize__ = $l$1contD___deserialize__;
        $register(&$l$1contG_methods);
    }
    {
        $l$2contG_methods.$GCINFO = "$l$2cont";
        $l$2contG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        $l$2contG_methods.__init__ = $l$2contD___init__;
        $l$2contG_methods.__call__ = $l$2contD___call__;
        $l$2contG_methods.__serialize__ = $l$2contD___serialize__;
        $l$2contG_methods.__deserialize__ = $l$2contD___deserialize__;
        $register(&$l$2contG_methods);
    }
    {
        $l$3contG_methods.$GCINFO = "$l$3cont";
        $l$3contG_methods.$superclass = ($SuperG_class)&$ContG_methods;
        $l$3contG_methods.__init__ = $l$3contD___init__;
        $l$3contG_methods.__call__ = $l$3contD___call__;
        $l$3contG_methods.__serialize__ = $l$3contD___serialize__;
        $l$3contG_methods.__deserialize__ = $l$3contD___deserialize__;
        $register(&$l$3contG_methods);
    }
    {
        B_WorldAuthG_methods.$GCINFO = "B_WorldAuth";
        B_WorldAuthG_methods.$superclass = ($SuperG_class)&B_valueG_methods;
        ;
        B_WorldAuthG_methods.__init__ = B_WorldAuthD___init__;
        B_WorldAuthG_methods.__serialize__ = B_WorldAuthD___serialize__;
        B_WorldAuthG_methods.__deserialize__ = B_WorldAuthD___deserialize__;
        $register(&B_WorldAuthG_methods);
    }
    {
        B_EnvG_methods.$GCINFO = "B_Env";
        B_EnvG_methods.$superclass = ($SuperG_class)&$ActorG_methods;
        B_EnvG_methods.__bool__ = (B_bool (*) (B_Env))$ActorG_methods.__bool__;
        B_EnvG_methods.__str__ = (B_str (*) (B_Env))$ActorG_methods.__str__;
        B_EnvG_methods.__repr__ = (B_str (*) (B_Env))$ActorG_methods.__repr__;
        B_EnvG_methods.__resume__ = (B_NoneType (*) (B_Env))$ActorG_methods.__resume__;
        B_EnvG_methods.__init__ = B_EnvD___init__;
        B_EnvG_methods.stdout_write$local = B_Env$stdout_write$local;
        B_EnvG_methods.stdin_install$local = B_Env$stdin_install$local;
        B_EnvG_methods.exit$local = B_Env$exit$local;
        B_EnvG_methods.stdout_write = B_Env$stdout_write;
        B_EnvG_methods.stdin_install = B_Env$stdin_install;
        B_EnvG_methods.exit = B_Env$exit;
        B_EnvG_methods.__serialize__ = B_EnvD___serialize__;
        B_EnvG_methods.__deserialize__ = B_EnvD___deserialize__;
        $register(&B_EnvG_methods);
    }
    // END GENERATED __builtin__.act D___init__
    ///////////////////////////////////////////////////////////////////////////////////////
}
