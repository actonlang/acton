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

#define GC_THREADS 1
#include <gc.h>

#if defined(_WIN32) || defined(_WIN64)
#else
#include <sys/ioctl.h>
#include <termios.h>
#endif
#include <unistd.h>
#include <uv.h>

#include "env.h"

#include "../rts/io.h"
#include "../rts/log.h"

extern char rts_exit;
extern int return_val;
#if defined(_WIN32) || defined(_WIN64)
#else
extern bool stdin_attr_changed;
#endif


// Stdio ///////////////////////////////////////////////////////////////////////

$R B_StdioD_outG_local (B_Stdio self, $Cont c$cont, B_str s) {
    fwrite(s->str, 1, s->nbytes, stdout);
    return $R_CONT(c$cont, B_None);
}

$R B_StdioD_errG_local (B_Stdio self, $Cont c$cont, B_str s) {
    fwrite(s->str, 1, s->nbytes, stderr);
    return $R_CONT(c$cont, B_None);
}

$R B_StdioD_outbG_local (B_Stdio self, $Cont c$cont, B_bytes b) {
    fwrite(b->str, 1, b->nbytes, stdout);
    return $R_CONT(c$cont, B_None);
}

$R B_StdioD_errbG_local (B_Stdio self, $Cont c$cont, B_bytes b) {
    fwrite(b->str, 1, b->nbytes, stderr);
    return $R_CONT(c$cont, B_None);
}

#if defined(_WIN32) || defined(_WIN64)
#else
// While stdin is in raw mode, stdin_raw is set and stdin_cooked_attr holds the
// settings from before raw mode, which set_stdin(raw=False) puts back. Only
// the Stdio actor reads and writes them.
static bool stdin_raw = false;
static struct termios stdin_cooked_attr;

// Raw mode, like UV_TTY_MODE_RAW of libuv: every byte is passed on as it
// arrives, without echo, signals (ISIG: ^C ^Z ^\), flow control (IXON: ^S ^Q),
// literal next and delayed suspend (IEXTEN: ^V, and ^Y on macOS) or CR to NL
// mapping. The output flags are not changed, so OPOST and ONLCR keep turning
// "\n" into CR LF.
static void make_raw(struct termios *attr) {
    attr->c_iflag &= ~(BRKINT | ICRNL | IGNCR | INLCR | INPCK | ISTRIP | IXON);
    attr->c_cflag = (attr->c_cflag & ~CSIZE) | CS8;
    attr->c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
    attr->c_cc[VMIN] = 1;
    attr->c_cc[VTIME] = 0;
}
#endif

$R B_StdioD_set_stdinG_local (B_Stdio self, $Cont c$cont, B_bool canonical, B_bool echo, B_bool raw) {
#if defined(_WIN32) || defined(_WIN64)
#else
    struct termios attr;
    if (tcgetattr(STDIN_FILENO, &attr) != 0)
        return $R_CONT(c$cont, B_None);

    if (raw != NULL) {
        if (fromB_bool(raw) == true) {
            if (!stdin_raw)
                stdin_cooked_attr = attr;
            stdin_raw = true;
            make_raw(&attr);
        } else if (stdin_raw) {
            stdin_raw = false;
            attr = stdin_cooked_attr;
        }
    }

    if (canonical != NULL) {
        if (fromB_bool(canonical) == true) {
            attr.c_lflag |= ICANON; // Set ICANON flag
        } else {
            attr.c_lflag &= ~ICANON; // Remove ICANON flag
        }
    }

    if (echo != NULL) {
        if (fromB_bool(echo) == true) {
            attr.c_lflag |= ECHO;
        } else {
            attr.c_lflag &= ~ECHO;
        }
    }

    stdin_attr_changed = true;
    tcsetattr(STDIN_FILENO, TCSANOW, &attr);
#endif
    return $R_CONT(c$cont, B_None);
}

// A terminal that reports no size gives 0 columns and 0 rows, like a new
// pseudo terminal. On Windows the size is that of the visible window of the
// console, not of its scrollback buffer.
$R B_StdioD_term_sizeG_local (B_Stdio self, $Cont c$cont) {
    int64_t columns = 0;
    int64_t rows = 0;
#if defined(_WIN32) || defined(_WIN64)
    CONSOLE_SCREEN_BUFFER_INFO info;
    if (GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &info)) {
        columns = (int64_t)info.srWindow.Right - info.srWindow.Left + 1;
        rows = (int64_t)info.srWindow.Bottom - info.srWindow.Top + 1;
    }
#else
    struct winsize size;
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &size) == 0) {
        columns = size.ws_col;
        rows = size.ws_row;
    }
#endif
    if (columns <= 0 || rows <= 0)
        return $R_CONT(c$cont, B_None);
    return $R_CONT(c$cont, $NEWTUPLE(2, to$int(columns), to$int(rows)));
}

// The one stdin reader of the process, with the callback of the last
// stdin_install in its data. A second reader on the terminal would race the
// first for its bytes, since libuv opens the terminal anew for each handle.
// The reader runs on the loop of the worker that the Stdio actor is pinned
// to, so only that worker's thread reads and writes it.
static uv_tty_t *stdin_tty = NULL;

void read_stdin(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buf) {
    if (nread < 0){
        if (nread == UV_EOF) {
            uv_close((uv_handle_t *)stream, NULL);
            stdin_tty = NULL;
        }
    } else if (nread > 0) {
        if (stream->data) {
            $action cb = stream->data;
            cb->$class->__asyn__(cb, actBytesFromCStringLengthCopy(buf->base, nread));
        }
    }
}

$R B_StdioD__on_stdin_bytesG_local (B_Stdio self, $Cont c$cont, $action cb) {
    if (stdin_tty != NULL) {
        stdin_tty->data = cb;
        return $R_CONT(c$cont, B_None);
    }
    // This should be the only call in Stdio that uses the libuv loop, so it is
    // safe to pin affinity here (and not earlier).
    pin_actor_affinity();
    uv_tty_t *tty = acton_malloc_rescanned(sizeof(uv_tty_t));
    if (uv_tty_init(get_uv_loop(), tty, STDIN_FILENO, 1) != 0)
        return $R_CONT(c$cont, B_None);
    tty->data = cb;
    uv_read_start((uv_stream_t*)tty, alloc_buffer, read_stdin);
    stdin_tty = tty;
    return $R_CONT(c$cont, B_None);
}


// Env /////////////////////////////////////////////////////////////////////////

$R B_EnvD_exitG_local (B_Env self, $Cont c$cont, int64_t n) {
    return_val = n;
    rts_shutdown();
    return $R_CONT(c$cont, B_None);
}


B_WorldCap B_WorldCapG_new() {
    B_WorldCap $tmp = acton_malloc(sizeof(struct B_WorldCap));
    $tmp->$class = &B_WorldCapG_methods;
    //   B_WorldCapG_methods.__init__($tmp);
    return $tmp;
}

B_NoneType B_WorldCapD___init__ (B_WorldCap self) {
    return B_None;
}

