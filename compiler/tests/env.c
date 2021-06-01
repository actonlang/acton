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
struct Connection$__class__;
typedef struct Connection$__class__ *Connection$__class__;

struct Connection {
    Connection$__class__ __class__;
    $WORD __impl__;
};
struct Connection$__class__ {
    char *$GCINFO;
    $Msg (*deliver)(Connection$__class__, $WORD, $str);
    $Msg (*close)(Connection$__class__, $WORD);
    $Msg (*receive_on)(Connection$__class__, $WORD, $function, $function);
};

Connection Connection$__pack__(Connection$__class__ __class__, $WORD __impl__) {
    Connection conn = malloc(sizeof(struct Connection));
    conn->__class__ = __class__;
    conn->__impl__ = __impl__;
    return conn;
}

// protocol Env  //////////////////////////////////////////////////////////////////////////////////////////

struct Env;
typedef struct Env *Env;
struct Env$__class__;
typedef struct Env$__class__ *Env$__class__;

struct Env {
    Env$__class__ __class__;
    $WORD __impl__;
};
struct Env$__class__ {
    char *$GCINFO;
    $Msg (*open)(Env$__class__, $WORD, $str, $int, $function);
};

Env Env$__pack__(Env$__class__ __class__, $WORD __impl__) {
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

$Msg TrueConnection$deliver (Connection$__class__ cls, $WORD __impl__, $str data) {
    TrueConnection trueSelf = (TrueConnection)__impl__;
    return NULL;
}

$Msg TrueConnection$close (Connection$__class__ cls, $WORD __impl__) {
    TrueConnection trueSelf = (TrueConnection)__impl__;
    return NULL;
}

$Msg TrueConnection$receive_on (Connection$__class__ cls, $WORD __impl__, $function input, $function error) {
    TrueConnection trueSelf = (TrueConnection)__impl__;
    return NULL;
}

struct Connection$__class__ Connection___TrueConnection = {
    .$GCINFO    = "Connection",
    .deliver    = TrueConnection$deliver,
    .close      = TrueConnection$close,
    .receive_on = TrueConnection$receive_on
};

// class TrueEnv  /////////////////////////////////////////////////////////////////////////////////////////

struct TrueEnv;
typedef struct TrueEnv *TrueEnv;

$Msg TrueEnv$open(Env$__class__ cls, $WORD __impl__, $str address, $int port, $function callback) {
    TrueEnv self = (TrueEnv)__impl__;
    
    TrueConnection trueConn = /* create socket, etc, ... */ NULL;
    Connection conn = Connection$__pack__(&Connection___TrueConnection, trueConn);
    $Msg m = /* ASYNC... */ NULL;
    return m;
}

struct Env$__class__ Env___TrueEnv = {
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
    Env env = Env$__pack__(&Env___TrueEnv, trueEnv);
    // BOOTSTRAP($CONTINUATION(ROOT, 1, ($WORD)env));
    // ... 
}