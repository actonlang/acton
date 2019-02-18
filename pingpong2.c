
const int SPAM_COUNT = 20;

/*
-------------------------------- original

actor PingStarter(N, i):
    first = None
    prev = None
    for x in range(N):
        p = Pingpong(i)
        if first == None:
            first = p
        if prev:
            prev.set_pong(p)
        prev = p
    prev.set_pong(first)
    first.ping(0, True)

actor Pingpong(i):
    var pong = None
    async def ping(count, forward):
        print('Ping %d', count)
        if forward:
            if count % 10 == 0:
                pong.ping(count, False)
                pong.ping(count, False)
            pong.ping(count + 1, True)
        return None
    def set_pong(p):
        pong = p

-------------------------------- explicit ASYNC

actor PingStarter(N, i):
    first = None
    prev = None
    for x in range(N):
        p = Pingpong(i)
        if first == None:
            first = p
        if prev:
            ASYNC(prev, lambda: prev.set_pong(p))
        prev = p
    ASYNC(prev, lambda: prev.set_pong(first))
    ASYNC(first, lambda: first.ping(0, True))

actor Pingpong(i):
    var pong = None
    async def ping(count, forward):
        print('Ping %d', count)
        if forward:
            if count % 10 == 0:
                ASYNC(pong, lambda: pong.ping(count, False))
                ASYNC(pong, lambda: pong.ping(count, False))
            ASYNC(pong, lambda: pong.ping(count + 1, True))
        return None
    def set_pong(p):
        pong = p
    
-------------------------------- explicit ACTOR

def PingStarter(N, i):
    self = ACTOR(0)
    first = None
    prev = None
    for x in range(N):
        p = Pingpong(i)
        if not first:
            first = p
        if prev:
            ASYNC(prev, lambda: prev.set_pong(p))
        prev = p
    ASYNC(prev, lambda: prev.set_pong(first))
    ASYNC(first, lambda: first.ping(0, True))
    return self

def Pingpong(i):
    self = ACTOR(1)
    self.pong = None
    async def ping(count, forward):
        print('Ping %d', count)
        if forward:
            if count % 10 == 0:
                ASYNC(self.pong, lambda: self.pong.ping(count, False))
                ASYNC(self.pong, lambda: self.pong.ping(count, False))
             ASYNC(self.pong, lambda: self.pong.ping(count + 1, True))
        return None
    def set_pong(p):
        self.pong = p
    return self

-------------------------------- CPS

def PingStarter(N, i, then):
    self = ACTOR(0)
    first = None
    prev = None
    for x in range(N):
        p = Pingpong(i)
        if not first:
            first = p
        if prev:
            ASYNC(prev, lambda: prev.set_pong(p))
        prev = p
    ASYNC(prev, lambda th: prev.set_pong(first, th))
    ASYNC(first, lambda th: first.ping(0, True, th))
    return RCONT(then, self)

def Pingpong(i, then):
    self = ACTOR(1)
    self.pong = None
    async def ping(count, forward, then):
        print('Ping %d', count)
        if forward:
            if count % 10 == 0:
                ASYNC(self.pong, lambda th: self.pong.ping(count, False, th))
                ASYNC(self.pong, lambda th: self.pong.ping(count, False, th))
            ASYNC(self.pong, lambda th: self.pong.ping(count + 1, True, th))
        return RCONT(then, None)
    def set_pong(p, then):
        self.pong = p
        return RCONT(then, None)
    return RCONT(then, self)

-------------------------------- explicit lambdas

def PingStarter(N, i, then):
    self = ACTOR(0)
    first = None
    prev = None
    for x in range(N):
        p = Pingpong(i)
        if not first:
            first = p
        if prev:
            def set_pong1(th):
                prev.set_pong(p, th)
            ASYNC(p, set_pong1)
        prev = p
    def set_pong2(th):
        prev.set_pong(first, th)
    ASYNC(prev, set_pong2)
    def first_ping(th):
        first.ping(0, True, th)
    ASYNC(first, first_ping)
    return RCONT(then, self)

def Pingpong(i, then):
    self = ACTOR(1)
    self.pong = None
    async def ping(count, forward, then):
        print('Ping %d', count)
        if forward:
            if count % 10 == 0:
                def ping_spam(th):
                    self.pong.ping(count, False, th)
                ASYNC(self.pong, ping_spam)
                ASYNC(self.pong, ping_spam)
            def ping_fw(th):
                self.pong.ping(count + 1, True, th)
            ASYNC(self.pong, ping_fw)
        return RCONT(then, None)
    def set_pong(p, then):
        self.pong = p
        return RCONT(then, None)
    return RCONT(then, self)

-------------------------------- closure-conversion

def PingStarter(N, i, then):
    self = ACTOR(0)
    first = None
    prev = None
    for x in range(N):
        p = Pingpong(i)
        if not first:
            first = p
        if prev:
            def set_pong1(prev, p, th):
                prev.set_pong(p, th)
            ASYNC(prev, CLOS1(set_pong1, prev, p))
        prev = p
    def set_pong2(prev, first, th):
        prev.set_pong(first, th)
    ASYNC(prev, CLOS2(set_pong2, prev, first))
    def first_ping(first, th):
        first.ping(0, True, th)
    ASYNC(first, CLOS1(first_ping, first))
    return RCONT(then, self)

----

def Pingpong(i, then):
    self = ACTOR(1)
    self.pong = None
    async def ping(count, forward, then):
        print('Ping %d', count)
        if forward:
            if count % 10 == 0:
                def ping_spam(pong, count, th):
                    pong.ping(count, False, th)
                ASYNC(self.pong, CLOS2(ping_spam, self.pong, count))
                ASYNC(self.pong, CLOS2(ping_spam, self.pong, count))
            def ping_fw(pong, count, th):
                pong.ping(count + 1, True, th)
            ASYNC(self.pong, CLOS2(ping_fw, self.pong, count))
        return RCONT(then, None)
    def set_pong(p, then):
        self.pong = p
        return RCONT(then, None)
    return RCONT(then, self)

-------------------------------- lambda-lifting

def set_pong1(p, prev, th):
    set_pong(p, prev, th)

def set_pong2(prev, first, th):
    set_pong(prev, first, th)

def first_ping(first, th):
    ping(first, 0, True, th)

def PingStarter(N, i, then):
    self = ACTOR(0)
    first = None
    prev = None
    for x in range(N):
        p = Pingpong(i)
        if not first:
            first = p
        if prev:
            ASYNC(prev, CLOS2(set_pong1, prev, p))
        prev = p
    ASYNC(prev, CLOS2(set_pong2, prev, first))
    ASYNC(first, CLOS1(first_ping, first))
    return RCONT(then, self)

----

def ping_spam(pong, count, th):
    ping(pong count, False, th)

def ping_fw(pong, count, th):
    ping(pong, count + 1, True, th)

def ping(self, count, forward, then):
    print('Ping %d', count)
    if forward:
        if count % 10 == 0:
            ASYNC(self.pong, CLOS2(ping_spam, self.pong, count))
            ASYNC(self.pong, CLOS2(ping_spam, self.pong, count))
        ASYNC(self.pong, CLOS2(ping_fw, self.pong, count))
    return RCONT(then, None)

def set_pong(self, p, then):
    self.pong = p
    return RCONT(then, None)

def Pingpong(i, then):
    self = ACTOR(1)
    self.pong = None
    return RCONT(then, self)

-------------------------------- explicit this

def set_pong1(this, th):
    set_pong(this.prev, this.p, th)

def set_pong2(this, th):
    set_pong(this.prev, this.first, th)

def first_ping(this, th):
    ping(this.first, 0, True, th)

def PingStarter(this, then):
    self = ACTOR(0)
    first = None
    prev = None
    for x in range(this.N):
        p = Pingpong(this.i)
        if not first:
            first = p
        if prev:
            ASYNC(prev, CLOS2(set_pong1, prev, p))
        prev = p
    ASYNC(prev, CLOS2(set_pong2, prev, first))
    ASYNC(first, CLOS1(first_ping, first))
    return RCONT(then, self)

----

def ping_spam(this, th):
    pong.ping(this.self, this.count, False, th)

def ping_fw(this, th):
    pong.ping(this.self, this.count + 1, True, th)

def ping(self, count, forward, then):
    print('Ping %d', count)
    if forward:
        if count % 10 == 0:
            ASYNC(self.pong, CLOS2(ping_spam, self.pong, count))
            ASYNC(self.pong, CLOS2(ping_spam, self.pong, count))
        ASYNC(self.pong, CLOS2(ping_fw, self.pong, count))
    return RCONT(then, None)

def set_pong(self, p, then):
    self.pong = p
    return RCONT(then, None)

def Pingpong(this, then):
    self = ACTOR(1)
    self.pong = None
    return RCONT(then, self)

-------------------------------- anonymous variable arrays

def set_pong1(this, th):
    set_pong(this[0], this[1], th)

def set_pong2(this, th):
    set_pong(this[0], this[1], th)

def first_ping(this, th):
    ping(this[0], 0, True, th)

def PingStarter(this, then):
    self = ACTOR(0)
    first = None
    prev = None
    for x in range(this[0]):
        p = Pingpong(this[1])
        if not forst:
            first = p
        if prev:
            ASYNC(prev, CLOS2(set_pong1, prev, p))
        prev = p
    ASYNC(prev, CLOS2(set_pong2, prev, first))
    ASYNC(first, CLOS1(first_ping, first))
    return RCONT(then, self)

----

def ping_spam(this, th):
    ping(this[0], this[1], False, th)

def ping_fw(this, th):
    ping(this[0], this[1] + 1, True, th)

def ping(self, count, forward, then):
    print('Ping %d', count)
    if forward:
        if count % 10 == 0:
            ASYNC(self[0], CLOS2(ping_spam, self[0], count))
            ASYNC(self[0], CLOS2(ping_spam, self[0], count))
        ASYNC(self[0], CLOS2(ping_fw, self[0], count))
    return RCONT(then, None)

def set_pong(self, p, then):
    self[0] = p
    return RCONT(then, None)

def Pingpong(this, then):
    self = ACTOR(1)
    self[0] = None
    return RCONT(then, self)

-------------------------------- explicit var and state offsets

def set_pong1(this, th):
    set_pong(this.var[0], this.var[1], th)

def set_pong2(this, th):
    set_pong(this.var[0], this.var[1], th)

def first_ping(this, th):
    ping(this.var[0], 0, True, th)

def PingStarter(this, then):
    self = ACTOR(0)
    first = None
    prev = None
    for x in range(this.var[0]):
        p = Pingpong(this.var[1])
        if not first:
            first = p
        if prev:
            ASYNC(prev, CLOS2(set_pong1, prev, p))
        prev = p
    ASYNC(prev, CLOS2(set_pong2, prev, first))
    ASYNC(first, CLOS1(first_ping, first))
    return RCONT(then, self)

----

def ping_spam(this, th):
    ping(this.var[0], this.var[1], False, th)

def ping_fw(this, th):
    ping(this.var[0], this.var[1] + 1, True, th)

def ping(self, count, forward, then):
    print('Ping %d', count)
    if forward:
        if count % 10 == 0:
            ASYNC(self.state[0], CLOS2(ping_spam, self.state[0], count))
            ASYNC(self.state[0], CLOS2(ping_spam, self.state[0], count))
        ASYNC(self.state[0], CLOS2(ping_fw, self.state[0], count))
    return RCONT(then, None)

def set_pong(self, p, then):
    self.state[0] = p
    return RCONT(then, None)

def Pingpong(this, then):
    self = ACTOR(1)
    self.state[0] = None
    return RCONT(then, self)
    
*/

R set_pong(Actor self, WORD p, WORD then);

R set_pong1(Clos this, WORD th) {
    assert(this->nvar == 2);
    return set_pong(this->var[0], this->var[1], th);
}

R set_pong2(Clos this, WORD th) {
    assert(this->nvar == 2);
    return set_pong(this->var[0], this->var[1], th);
}

R ping(Actor self, WORD count, WORD forward, WORD then);

R first_ping(Clos this, WORD th) {
    assert(this->nvar == 1);
    return ping(this->var[0], 0, (WORD)true, th);
}

// TODO: should be:
//  R Pingpong(WORD i, WORD then);
Actor Pingpong(WORD i);//, WORD then);

R PingStarter(Clos this, WORD then) {
    assert(this->nvar == 2);
    Actor self = ACTOR(0);
    WORD first = None;
    WORD prev = None;
    for(int x = 0; x < (int)this->var[0]; ++x) {
        // TODO: should be _CONT( the rest of the function )
        Actor p = Pingpong(this->var[1]);
        if (first == None) {
            first = prev;
        }
        if (prev != None) {
            ASYNC(MSG(prev, CLOS2(set_pong1, prev, p)));
        }
        prev = p;
    }
    ASYNC(MSG(prev, CLOS2(set_pong2, prev, first)));
    ASYNC(MSG(first, CLOS1(first_ping, first)));
    return _CONT(then, self);
}

////

R ping_spam(Clos this, WORD then) {
    return ping(this->var[0], this->var[1], false, then);
}

R ping_fw(Clos this, WORD then) {
    return ping(this->var[0], (WORD)(((int)this->var[1]) + 1), (WORD)true, then);
}


R timed_func(Clos this, WORD then);

R timed_func_msgwrap(Clos this, WORD then) {
    return _DONE(then, (WORD)MSG(this->var[0], CLOS1(timed_func, this->var[1])));
}


static const monotonic_duration postpone_delay = 3*MT_SECOND;
static _Thread_local TimedMsg tmsg = NULL;

R timed_func(Clos this, WORD then) {
    assert(this->var[0] != NULL);
    Msg curr_msg = ((Actor)this->var[0])->msgQ;

    const monotonic_time mt_now = monotonic_now();
    printf("\x1b[35;1mtimed_func()\x1b[m count = %d @ %lu\n", (int)this->var[1], mt_now);

    // this would normally call the timed_func_msgwrap() function
    //R msg_cont = timed_func_wrap(CLOS2(timed_func_wrap, this->var[0], (WORD)((int)this->var[1] + 1)), NULL);
    //Msg async_msg = (Msg)msg_conf.value;
    // the above, inlined:
    Msg async_msg = MSG(this->var[0], CLOS2(timed_func, this->var[0], (WORD)((int)this->var[1] + 1)));
    tmsg = POSTPONE(curr_msg->time_baseline + postpone_delay, async_msg);

    return _CONT(then, None);
}

_Atomic int next_count_print = 0;

R ping(Actor self, WORD count, WORD forward, WORD then) {
    Msg curr_msg = self->msgQ;
    if ((int)count % PRINT_INTERVAL == 0 && (_Bool)forward != false) {
        int curr_count = (int)count; // need lvalue for compare
        if (atomic_compare_exchange_weak(&next_count_print, &curr_count, curr_count + PRINT_INTERVAL))
            printf("Ping %10d\n", (int)count);
    }
    if ((int)count == 0 && (_Bool)forward != false) {
        // this would normally call the timed_func_msgwrap() function
        //R msg_cont = timed_func_wrap(CLOS2(timed_func_wrap, this->var[0], (WORD)((int)this->var[1] + 1)), NULL);
        //Msg async_msg = (Msg)msg_conf.value;
        // the above, inlined:
        Msg async_msg = MSG(self, CLOS2(timed_func, self, 0));
        tmsg = POSTPONE(curr_msg->time_baseline + 3*MT_SECOND, async_msg);
    }
    if ((int)count == 200000) {
        if (postpone_CANCEL(tmsg)) {
            printf("postpone cancelled\n");
        }
    }
    if ((int)count >= PING_LIMIT) {
        printf("\x1b[31;1mping limit reached\x1b[m\n");
        return _EXIT(then, None);
    }
    if ((_Bool)forward != false) {
        if ((int)count % 10 == 0) {
            for(int idx = 0; idx < SPAM_COUNT; ++idx) {
                ASYNC(MSG(self->state[0], CLOS2(ping_spam, self->state[0], count)));
            }
        }
        ASYNC(MSG(self->state[0], CLOS2(ping_fw, self->state[0], count)));
    }
    return _CONT(then, None);
}

R set_pong(Actor self, WORD p, WORD then) {
    self->state[0] = p;
    return _CONT(then, None);
}

// TODO: should be:
//  R Pingpong(WORD i, WORD then);
Actor Pingpong(WORD i) {
    Actor self = ACTOR(1);
    self->state[0] = None;
    //return RCONT(then, self);
    return self;
}

#define BOOSTRAP_CLOSURE CLOS2(PingStarter, (WORD)10, (WORD)0)
