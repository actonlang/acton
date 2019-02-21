# Python version of bytesview



##################################################
### Bytesview

class Bytesview():

    _bytes : bytes = None
    _pre : Bytesview = None
    _start : int = 0
    _end : int = 0
    
    def __init__(self, b, n=None, start=None, pre=None):
        self._bytes = b
        self._pre = None
        self._start = 0 if start is None else start
        if n is not None:
            self._end = self._start + n
        else:
            self._end = len(b)-self._start

    def n(self):
        return self._end - self._start + (_pre.n() if _pre is not None else 0)

    def limits(self):
        return (self._start, self._end)
    
    def to_bytes(self, start=None, end=None):
        if start is None:
            if end is None or end == -1:
                these = self._pre() self._bytes[self._start:self._end]
            else:
                these = self._bytes[self._start:self._start+end]
        else:
            if end is None or end == -1:
                these = self._bytes[self._start+start:self._end]
            else:
                these = self._bytes[self._start+start:self._start+end]
        if self._pre is None:
            return these
        else:
            return self._pre.to_bytes() + these

    def append(self, b, n=None):
        return Bytesview(b, n=n, pre=self)

    def _consume(self, amount):
        if self._pre is not None:
            (pre, remaining) = _consume(self._pre, amount)
        else
            pre = None
            remaining = amount
            
        thislen = self._end - self._start
        if remaining <= thislen:
            self._start += remaining
            
            return Bytesview(self._bytes, 
        
        
    def consume(self, amount):
        # assert(self.n()>=amount)
        if self._pre is not None:
            remaining = thislen = self._end - self._start
        if amount < thislen:
            return Bytesview(
        

class BytesListView (Bytesview):

    def __init__(self, 
        



##################################################
### Unit tests

def report(bv, name):
    print (name+" = ", bv)
    print (name+".n() = ", bv.n())
    print (name+"limits() = ", bv.limits())
    print (name+".to_bytes() = ", bv.to_bytes())

     


by = b'0123456789abcdef'
l = len(by)
print("by = ",by)
print("l = ", l)

bv1 = Bytesview(by)
bv2 = Bytesview(by, n=16)

report(bv1, "bv1")
report(bv2, "bv2")






