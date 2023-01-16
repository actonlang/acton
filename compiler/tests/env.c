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

#include "rts.h"

// protocol Connection  ///////////////////////////////////////////////////////////////////////////////////

struct Connection;
typedef struct Connection *Connection;
struct ConnectionD___class__;
typedef struct ConnectionD___class__ *ConnectionD___class__;

struct Connection {
    ConnectionD___class__ __class__;
    $WORD __impl__;
};
struct ConnectionD___class__ {
    char *$GCINFO;
    B_Msg (*deliver)(ConnectionD___class__, $WORD, B_str);
    B_Msg (*close)(ConnectionD___class__, $WORD);
    B_Msg (*receive_on)(ConnectionD___class__, $WORD, $function, $function);
};

Connection ConnectionD___pack__(ConnectionD___class__ __class__, $WORD __impl__) {
    Connection conn = malloc(sizeof(struct Connection));
    conn->__class__ = __class__;
    conn->__impl__ = __impl__;
    return conn;
}

// protocol Env  //////////////////////////////////////////////////////////////////////////////////////////

struct Env;
typedef struct Env *Env;
struct EnvD___class__;
typedef struct EnvD___class__ *EnvD___class__;

struct Env {
    EnvD___class__ __class__;
    $WORD __impl__;
};
struct EnvD___class__ {
    char *$GCINFO;
    B_Msg (*open)(EnvD___class__, $WORD, B_str, B_int, $function);
};

Env EnvD___pack__(EnvD___class__ __class__, $WORD __impl__) {
    Env env = malloc(sizeof(struct Env));
    env->__class__ = __class__;
    env->__impl__ = __impl__;
    return env;
};

// class TrueConnection ///////////////////////////////////////////////////////////////////////////////////

struct TrueConnection;
typedef struct TrueConnection *TrueConnection;

struct TrueConnection {
    int socket;
    $function input_callback;
    $function error_callback;
    // more...
};

B_Msg TrueConnection$deliver (ConnectionD___class__ cls, $WORD __impl__, B_str data) {
    TrueConnection trueSelf = (TrueConnection)__impl__;
    return NULL;
}

B_Msg TrueConnection$close (ConnectionD___class__ cls, $WORD __impl__) {
    TrueConnection trueSelf = (TrueConnection)__impl__;
    return NULL;
}

B_Msg TrueConnection$receive_on (ConnectionD___class__ cls, $WORD __impl__, $function input, $function error) {
    TrueConnection trueSelf = (TrueConnection)__impl__;
    return NULL;
}

struct ConnectionD___class__ Connection___TrueConnection = {
    .$GCINFO    = "Connection",
    .deliver    = TrueConnection$deliver,
    .close      = TrueConnection$close,
    .receive_on = TrueConnection$receive_on
};

// class TrueEnv  /////////////////////////////////////////////////////////////////////////////////////////

struct TrueEnv;
typedef struct TrueEnv *TrueEnv;

B_Msg TrueEnv$open(EnvD___class__ cls, $WORD __impl__, B_str address, B_int port, $function callback) {
    TrueEnv self = (TrueEnv)__impl__;
    
    TrueConnection trueConn = /* create socket, etc, ... */ NULL;
    Connection conn = ConnectionD___pack__(&Connection___TrueConnection, trueConn);
    B_Msg m = /* ASYNC... */ NULL;
    return m;
}

struct EnvD___class__ Env___TrueEnv = {
    .$GCINFO    = "Env",
    .open       = TrueEnv$open
};

struct TrueEnv {
    // empty? Or selector_fd?
};

// main //////////////////////////////////////////////////////////////////////////////////////////////////

int main() {
    // ...
    TrueEnv trueEnv = /* create whatever... */ NULL;
    Env env = EnvD___pack__(&Env___TrueEnv, trueEnv);
    // BOOTSTRAP($CONTINUATION(ROOT, 1, ($WORD)env));
    // ... 
}