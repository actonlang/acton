# Python version of bytesview


##################################################
### Bytesview

class Bytesview():

    _bytes : bytes = None
    _pre = None
    _start : int = 0
    _end : int = 0
    _eof : bool = False
    
    def __init__(self, b, n=None, start=None, pre=None, closed=False):
        self._bytes = b
        self._pre = pre 
        self._start = 0 if start is None else start
        if n is not None:
            self._end = self._start + n
        else:
            self._end = len(b)-self._start
        self._eof = closed

    ### analysis of Bytesview objects
    
    def n(self):
        return self._end - self._start + (self._pre.n() if self._pre is not None else 0)

    def limits(self):
        return (self._start, self._end)
    
    def to_bytes(self):
        these = self._bytes[self._start:self._end]
        if self._pre is None:
            return these
        else:
            return self._pre.to_bytes() + these

    ### de-lousing

    def show(self):
        if self._pre is not None:
            self._pre.show()
        print(self._bytes, "[{}:{}]".format(self._start,self._end))
        if self._eof:
            print("_eof = {}".format(self._eof))
            
    ### consuming data

    def _consume(self, amount):
        if self._pre is not None:
            (pre, remaining) = self._pre._consume(amount)
        else:
            pre = None
            remaining = amount
            
        thislen = self._end - self._start
        if remaining < thislen:
            newthis = Bytesview(self._bytes, n=thislen-remaining, start=self._start+remaining, pre=pre)
            newremaining = 0
        else:
            newthis = pre
            newremaining = remaining - thislen
        
        return(newthis,newremaining)
        
    def consume(self, amount):
        # assert(self.n()>=amount)
        (newview, remaining) = self._consume(amount)
        if remaining > 0:
            raise IndexError

        if newview is not None:
            return newview
        else:
            return Bytesview(b'')

    def _trim(self, start=0, startskip=0, end=-1, endskip=0):
        

    def trim(self, start=0, startskip=0, end=None, endskip=0):
        # end == None implies no trimming at end
        # end == -1 implies trimming from eof (so error if -1 and not closed)
        # remove start+startskip bytes at beginning
        # remove endskip bytes before end.
        if end == -1 and not self._eof:
            raise ValueError("Trim(end=-1) on non-closed Bytesview")
        thislen = self._end - self._start
        if self._pre is None:
            newstartskip = startskip
            newend = end
            newenskip = endskip
            return Bytesview(self._bytes, 
        else:
            
        if start is None:
            if end is None or end == -1:
                these = self._bytes[self._start:self._end]
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

    ### reading out data from a Bytesview object (creating a new)
    ###
    ### Raises IncompleteReadError if Bytesview object does not contain
    ### enough data to fulfill request.

    def read(self, n=-1):                  # (int) -> (bytes, Bytesview) 
       if n == -1:
           if not self._eof:
               raise IncompleteRead("read(-1) on non-closed Bytesview")
           else:
               (self.to_bytes(),Bytesview(b'',closed=True))
       else:
           # deliver n bytes, if there are n bytes.
           # if not, deliver available bytes if closed, raise if not closed
           
               
        
    def readline(self):

    def readexactly(self, n):

    def readuntil(self, separator=b'\n'):

    def at_eof(self):
        return self._eof and self._pre is None and self._start == self._end
    
    ### feeding data & closing
    
    def append(self, b, n=None):
        if self._eof:
            raise ValueError("append() on closed Bytesview")
        if self._start < self._end:
            return Bytesview(b, n=n, pre=self)
        else:
            return Bytesview(b, n=n)

    def write(self, b, n=None):  	# same as append(self, b, n=None)
        self._eof:
            raise ValueError("write() on closed Bytesview")
        return self.append(b, n)
        
    def writelines(self,data):
        if self._eof:
            raise ValueError("writelines() on closed Bytesview")
        for b in data:
            self.append(b)
        
    def close(self):
        if self._eof:
            return self
        return Bytesview(self._bytes, n=self._end-self._start, start=self._start, pre=self._pre, closed=True)

##################################################
### Unit tests

def report(bv, name):
    print (name+" = ", bv)
    print (name+".n() = ", bv.n())
    print (name+"limits() = ", bv.limits())
    print (name+".to_bytes() = ", bv.to_bytes())
    print (name+".show():")
    bv.show()

by = b'0123456789abcdef'
l = len(by)
print("by = ",by)
print("l = ", l)

bv1 = Bytesview(by)
bv2 = Bytesview(by, n=16)
                 
report(bv1, "bv1")
report(bv2, "bv2")

bv3 = bv2.append(by, n=16)
report(bv2, "bv2")
report(bv3, "bv3")

bv4 = bv3.append(by)
report(bv4,"bv4")

bv = bv4
n=0
while True: 
   res = bv.consume(n)
   print("")
   res.show()
   #print(res.to_bytes())
   n += 1

   
