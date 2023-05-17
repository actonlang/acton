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


// Env /////////////////////////////////////////////////////////////////////////

$R B_EnvD_stdout_writeG_local (B_Env self, $Cont c$cont, B_str s) {
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
$R B_EnvD_stdin_installG_local (B_Env self, $Cont c$cont, $action cb) {
    // This should be the only call in env that does IO stuff, so it is safe to
    // pin affinity here (and not earlier)..
    pin_actor_affinity();
    uv_tty_t *tty = malloc(sizeof(uv_tty_t));
    uv_tty_init(get_uv_loop(), tty, 0, 1);
    tty->data = cb;
    uv_read_start((uv_stream_t*)tty, alloc_buffer, read_stdin);
    return $R_CONT(c$cont, B_None);
}
$R B_EnvD_exitG_local (B_Env self, $Cont c$cont, B_int n) {
    return_val = from$int(n);
    rts_shutdown();
    return $R_CONT(c$cont, B_None);
}


B_Env B_EnvG_newactor(B_WorldAuth token, B_list args) {
    B_Env $tmp = $NEWACTOR(B_Env);
    $tmp->token = token;
    $tmp->args = args;
    $tmp->auth = $tmp->token;
    $tmp->argv = $tmp->args;
    $tmp->$affinity = 0;
    serialize_state_shortcut(($Actor)$tmp);
    return $tmp;
}

B_WorldAuth B_WorldAuthG_new() {
    B_WorldAuth $tmp = GC_MALLOC_EXPLICITLY_TYPED(sizeof(struct B_WorldAuth), B_WorldAuthG_methods.$GCdescr);
    $tmp->$class = &B_WorldAuthG_methods;
    //   B_WorldAuthG_methods.__init__($tmp);
    return $tmp;
}

B_NoneType B_WorldAuthD___init__ (B_WorldAuth self) {
    return B_None;
}

