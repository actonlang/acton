
/*
-------------------------------- original

actor Pingpong(i):
    var count = 0
    async def ping(q):
        count += 1
        print('Ping %d', count*q)
        _ = pong(count, q)
        return None
    def pong(n,q):
        print('     %d Pong', n*q)
        ping(q)
        return None
    ping(i)

-------------------------------- explicit ASYNC

actor Pingpong(i):
    var count = 0
    def ping(q):
        count += 1
        print('Ping %d', count*q)
        _ = pong(count, q)
        return None
    def pong(n,q):
        print('     %d Pong', n*q)
        ASYNC_(self, lambda: ping(q))
        return None
    ASYNC_(self, lambda: ping(i))

-------------------------------- explicit ACTOR

def Pingpong(i):
    self = ACTOR(1)
    self.count = 0
    def ping(q):
        self.count += 1
        print('Ping %d', self.count*q)
        _ = pong(self.count, q)
        return None
    def pong(n,q):
        print('     %d Pong', n*q)
        ASYNC_(self, lambda: ping(q))
        return None
    ASYNC_(self, lambda: ping(i))
    return self

-------------------------------- CPS

def Pingpong(i, then):
    self = ACTOR(1)
    self.count = 0
    def ping(q, then):
        self.count +=1
        print('Ping %d', self.count*q)
        return RCONT(lambda _: pong(self.count, q, then), None)
    def pong(n, q, then):
        print('     %d Pong', n*q)
        ASYNC(self, lambda th: ping(q,th))
        return RCONT(then, None)
    ASYNC(self, lambda th: ping(i,th))
    return RCONT(then, self)

-------------------------------- explicit lambdas

def Pingpong(i, then):
    self = ACTOR(1)
    self.count = 0
    def ping(q, then):
        self.count +=1
        print('Ping %d', self.count*q)
        def lam1(_):
            return pong(self.count, q, then)
        return RCONT(lam1, None)
    def pong(n, q, then):
        print('     %d Pong', n*q)
        def ping1(th):
            return ping(q, th)
        ASYNC(self, ping1)
        return RCONT(then, None)
    def ping2(th):
        return ping(i, th)
    ASYNC(self, ping2)
    return RCONT(then, self)

-------------------------------- closure-conversion

def Pingpong(i, then):
    self = ACTOR(1)
    self.count = 0
    def ping(self, q, then):
        self.count +=1
        print('Ping %d', self.count*q)
        def lam1(self, q, then, _):
            return pong(self, self.count, q, then)
        return RCONT(CLOS3(lam1,self,q,then), None)
    def pong(self, n, q, then):
        print('     %d Pong', n*q)
        def ping1(self, q, th):
            return ping(self, q, th)
        ASYNC(self, CLOS2(ping1,self,q))
        return RCONT(then, None)
    def ping2(self, i, th):
        return ping(self, i, th)
    ASYNC(self, CLOS2(ping2,self,i))
    return RCONT(then, self)

-------------------------------- lambda-lifting

def lam1(self, q, then, _):
    return pong(self, self.count, q, then)

def ping(self, q, then):
    self.count +=1
    print('Ping %d', self.count*q)
    return RCONT(CLOS3(lam1,self,q,then), None)

def ping1(self, q, th):
    return ping(self, q, th)

def pong(self, n, q, then):
    print('     %d Pong', n*q)
    ASYNC(self, CLOS2(ping1,self,q))
    return RCONT(then, None)

def ping2(self, i, th):
    return ping(self, i, th)

def Pingpong(i, then):
    self = ACTOR(1)
    self.count = 0
    ASYNC(self, CLOS2(ping2,self,i))
    return RCONT(then, self)

-------------------------------- explicit this

def lam1(this, _):
    return pong(this.self, this.self.count, this.q, this.then)

def ping(self, q, then):
    self.count +=1
    print('Ping %d', self.count*q)
    return RCONT(CLOS3(lam1,self,q,then), None)

def ping1(this, th):
    return ping(this.self, this.q, th)

def pong(self, n, q, then):
    print('     %d Pong', n*q)
    ASYNC(self, CLOS2(ping1,self,q))
    return RCONT(then, None)

def ping2(this, th):
    return ping(this.self, this.i, th)

def Pingpong(this, then):
    self = ACTOR(1)
    self.count = 0
    ASYNC(self, CLOS2(ping2,self,this.i))
    return RCONT(then, self)

-------------------------------- anonymous variable arrays

def lam1(this, _):
    return pong(this[0], this[0][0], this[1], this[2])

def ping(self, q, then):
    self[0] +=1
    print('Ping %d', self[0]*q)
    return RCONT(CLOS3(lam1,self,q,then), None)

def ping1(this, th):
    return ping(this[0], this[1], th)

def pong(self, n, q, then):
    print('     %d Pong', n*q)
    ASYNC(self, CLOS2(ping1,self,q))
    return RCONT(then, None)

def ping2(this, th):
    return ping(this[0], this[1], th)

def Pingpong(this, then):
    self = ACTOR(1)
    self[0] = 0
    ASYNC(self, CLOS2(ping2,self,this[0]))
    return RCONT(then, self)

-------------------------------- explicit var and state offsets

def lam1(this, _):
    return pong(this.var[0], this.var[0].state[0], this.var[1], this.var[2])

def ping(self, q, then):
    self.state[0] +=1
    print('Ping %d', self.state[0]*q)
    return RCONT(CLOS3(lam1,self,q,then), None)

def ping1(this, th):
    return ping(this.var[0], this.var[1], th)

def pong(self, n, q, then):
    print('     %d Pong', n*q)
    ASYNC(self, CLOS2(ping1,self,q))
    return RCONT(then, None)

def ping2(this, th):
    return ping(this.var[0], this.var[1], th)

def Pingpong(this, then):
    self = ACTOR(1)
    self.state[0] = 0
    ASYNC(self, CLOS2(ping2,self,this.var[0]))
    return RCONT(then, self)

*/

R pong(Actor self, WORD n, WORD q, Clos then);
R pong1(Clos this, WORD th);

//R lam1(Clos this, WORD _) {
//    return pong(this->var[0], ((Actor)this->var[0])->state[0], this->var[1], this->var[2]);
//}

_Atomic int next_count_print = 0;

R ping(Actor self, WORD q, Clos then) {
    self->state[0] = (WORD)((int)self->state[0] + 1);
    int j = (int)self->state[0]*(int)q;
    if (atomic_compare_exchange_weak(&next_count_print, &j, j + PRINT_INTERVAL))
        printf("Ping %'10d\n", j);
    ASYNC(MSG(self, CLOS3(pong1, self, self->state[0], q)));
    return _CONT(then, (WORD)j);
    //return (R){RCONT, CLOS3(lam1, self, q, then), None};
}

R ping1(Clos this, WORD th) {
    return ping(this->var[0], this->var[1], th);
}

R pong(Actor self, WORD n, WORD q, Clos then) {
    int j = (int)n*(int)q;
    if (j % PRINT_INTERVAL == 0) {
        if (atomic_compare_exchange_weak(&next_count_print, &j, j + PRINT_INTERVAL))
            printf("     %'10d Pong\n", j);
    }
    if(j >= PING_LIMIT) {
        printf("\x1b[31;1mping limit reached\x1b[m\n");
        return _EXIT(NULL, 0);
    }
    ASYNC(MSG(self, CLOS2(ping1, self, q)));
    return _CONT(then, None);
}

R ping2(Clos this, WORD th) {
    return ping(this->var[0], this->var[1], th);
}

R pong1(Clos this, WORD th) {
    return pong(this->var[0], this->var[1], this->var[2], th);
}

R Pingpong(Clos this, WORD then) {
    Actor self = ACTOR(1);
    self->state[0] = 0;
    ASYNC(MSG(self, CLOS2(ping2,self,this->var[0])));
    return _CONT(then, self);
}

#define BOOSTRAP_CLOSURE CLOS1(Pingpong, (WORD)1)
