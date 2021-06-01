# Copyright (C) 2019-2021 Data Ductus AB
#
# Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
#
# 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
#
# 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#

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
            # raise IndexError
            return None

        if newview is not None:
            return newview
        else:
            return Bytesview(b'')

#    def _initialbytes(self, amount=a, end=-1, endskip=0):
#        
#
#    def initialbytes(self, end=-1, endskip=0):
#        # remove data at end of bytesview.
#        #
#        # end == -1 implies trimming from eof (so error if -1 and not closed)
#        # end >= 0 implies end-endskip is the index where returned bytesview object ends.
#        # remove endskip bytes before end.
#        if end == -1 and not self._eof:
#            raise ValueError("Trim(end=-1) on non-closed Bytesview")
#
#        if self._pre is None:
#            
#        thislen = self._end - self._start
#        thislen -= 
#        if self._pre is None:
#            newstartskip = startskip
#            newend = end
#            newenskip = endskip
#            return Bytesview(self._bytes, start=self._start+ 
#        else:
#            
#        if start is None:
#            if end is None or end == -1:
#                these = self._bytes[self._start:self._end]
#            else:
#                these = self._bytes[self._start:self._start+end]
#        else:
#            if end is None or end == -1:
#                these = self._bytes[self._start+start:self._end]
#            else:
#                these = self._bytes[self._start+start:self._start+end]
#        if self._pre is None:
#            return these
#        else:
#            return self._pre.to_bytes() + these

    ### reading out data from a Bytesview object (creating a new)
    ###
    ### Raises IncompleteReadError if Bytesview object does not contain
    ### enough data to fulfill request.

    def _read(self, n):
       # n is always >= 0
       if self._pre is not None:
           (remaining, initial) = self._pre._read(n)
       else:
           remaining = n
           initial = None
 
       thislen = self._end - self._start
       if remaining == 0:
           return (remaining,initial)
       elif remaining >= thislen:
           return (remaining-thislen, self)
       else:  # 0 < remaining < thislen
           return (0, Bytesview(self._bytes, start=self._start, n=remaining, pre=initial))
                             

    def read(self, n=-1):                  # (int) -> (bytes, Bytesview) 
       if n == -1:
           if not self._eof:
               raise IncompleteReadError("read(-1) on non-closed Bytesview")
               return None
           else:
               return self
       else:
           # deliver n bytes, if there are n bytes.
           # if not, deliver available bytes if closed, error if not closed
           thislen = self._end - self._start
           if self._pre is not None:
               (remaining, initial) = self._pre._read(n)
           else:
               remaining = n
               initial = None

           if remaining == 0:
               return initial if initial is not None else Bytesview(b'')
           elif remaining < thislen:
               return Bytesview(self._bytes, n=remaining, start=self._start, pre=initial)
           elif remaining > thislen:
               if self._eof:
                   return self
               else:
                   #raise IncompleteReadError("read(n) on non-closed Bytesview with less than n bytes")
                   return None
           else:
               # remaining == thislen
               return self
        
    def readline(self):
        return self.readuntil (separator=b'\n')

    def _readexactly(self, n):
        thislen = self._end - self._start
        if self._pre:
            (initial, remains) = self._pre._readexactly(n)
        else:
            (initial, remains) = (None, n)

        if remains == 0:
            return (initial, remains)
        elif remains >= thislen:
            return (self, remains-thislen)
        else:
            # remains < thislen
            return (Bytesview(self._bytes, start=self._start, n=remains, pre=initial), 0)
        
    def readexactly(self, n):
        (initial, remains) = self._readexactly(n)
        if remains > 0:
            #raise IncompleteReadError("readexactly(): Too few bytes available")
            return None
        if initial is None:
            initial = Bytesview(b'')
        return initial 
    
    def _readuntil(self, separator, seplen):
        # len(last) < len(separator)
        if self._pre is not None:
            (last, initial, found) = self._pre._readuntil(separator, seplen)
        else:
            (last, initial, found) = (b'', None, False)

        # last is potential beginning of separator at end of previous Bytesview(s)

        if found:
            return (b'',initial, found)
        else:
            buf = last+self._bytes[self._start:self._start+seplen-1]
            idx = buf.find(separator)
            if idx >= 0:   # found it!
                # end of separator at ix+seplen-len(last)
                #print (buf, len(buf), last, len(last), sep, idx)
                return (b'', Bytesview(self._bytes, n=idx+seplen-len(last),
                                       start=self._start,
                                       pre=self._pre), True)
            idx = self._bytes.find(separator, self._start)
            if idx >= 0:   # found it!
                return (b'', Bytesview(self._bytes, n=idx-self._start+seplen, start=self._start, pre=self._pre), True)
            thislen = self._end-self._start
            if thislen >= seplen-1:
                last = self._bytes[self._end-(seplen-1):self._end]
            else:
                last = last[thislen-(seplen-1):] + self._bytes[self._start:self._end]
            return (last, self, False)
                    
    def readuntil(self, separator=b'\n'):
        seplen = len(separator)
        (_, initial, found) = self._readuntil(separator, seplen)
        if found:
            return initial if initial is not None else Bytesview(b'')
        else:
            if self._eof:
                #raise IncompleteReadError("Separator not found and Bytesview closed")
                return None
            else:
                #raise IncompleteReadError("Separator not found")
                return None
    
    def at_eof(self):
        return self._eof and self._pre is None and self._start == self._end
    
    ### feeding data & closing
    
    def append(self, b, n=None):
        if self._eof:
            #raise ValueError("append() on closed Bytesview")
            return None
        if self._start < self._end:
            return Bytesview(b, n=n, pre=self)
        else:
            return Bytesview(b, n=n)

    def write(self, b, n=None):  	# same as append(self, b, n=None)
        if self._eof:
            #raise ValueError("write() on closed Bytesview")
            return None
        return self.append(b, n)
        
    def writelines(self,data):
        if self._eof:
            #raise ValueError("writelines() on closed Bytesview")
            return None
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
#    print (name+"limits() = ", bv.limits())
    print (name+".to_bytes() = ", bv.to_bytes())
    print (name+".show():")
    bv.show()

by = b'0123456789abcdef'

#l = len(by)
#print("by = ",by)
#print("l = ", l)
#
#bv1 = Bytesview(by)
#bv2 = Bytesview(by, n=16)
#                 
#report(bv1, "bv1")
#report(bv2, "bv2")

bv3 = bv2.append(by, n=16)
#report(bv2, "bv2")
#report(bv3, "bv3")

bv4 = bv3.append(by)
#report(bv4,"bv4")

#bv = bv4
#n=0
#while True: 
#   res = bv.consume(n)
#   print("")
#   res.show()
#   #print(res.to_bytes())
#   n += 1   

###########
### read()
#
#bv = bv4
#n=0
#while True: 
#   res = bv.read(n)
#   print("")
#   res.show()
#   #print(res.to_bytes())
#   n += 1   

#print("read() test")

#bv = bv4
#n=0
#while n < 60: 
#   res = bv.read(n)
#   print(res.to_bytes())
#   n += 1

#bv = bv4.close()
#print("bv length: ", bv.n())
#
#n=0
#while n < 60: 
#   res = bv.read(n)
#   print(res.to_bytes())
#   n += 1

#bv = bv4.close()
#print("bv length: ", bv.n())
#
#n=0
#while n < 60: 
#   res = bv.read(n)
#   print(res.to_bytes())
#   res.show()
#   n += 1

###########
### readuntil()
#

#separators = by+b'x'+by+b'y'+by+b'z'+by+b'w'
#bv = Bytesview(by+b'x').append(by+b'y').append(by+b'z').append(by)
#bv.close()
#print("bv length: ", bv.n())
#
#n=0
#while n < 4*16+3+5:
#    sep = separators[n:n+7]
#    res = bv.readuntil(separator=sep)
#    print(sep, " ", res.to_bytes())
#    res.show()
#    n += 1

def bv_list(bv=None):
    bvl = bv if bv is not None else Bytesview(b'')
    n = 0
    while n < 16:
        bvl = bvl.append(by[n:n+1])
        n += 1
    return bvl

bvl = bv_list().append(b'x')
bvl = bv_list(bvl).append(b'y')
bvl = bv_list(bvl).append(b'z')
bvl = bv_list(bvl)

print ("bvl: ", bvl.to_bytes())
bvl.show()

#bv.close()
print("bv length: ", bvl.n())

separators = by+b'x'+by+b'y'+by+b'z'+by+b'w'
n=0
while n < 4*16+3+5:
    sep = separators[n:n+7]
    res = bvl.readuntil(separator=sep)
    print(sep, " ", res.to_bytes())
    res.show()
    n += 1   

###########
### readexactly()
#

#bv = Bytesview(by+b'x').append(by+b'y').append(by+b'z').append(by)
#bv.close()
#print("bv length: ", bv.n())
#
#n=0
#while n < 4*16+3+5:
#    res = bv.readexactly(n)
#    print(res.to_bytes())
#    #res.show()
#    n += 1
#


