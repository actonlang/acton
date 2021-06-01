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

import pickle

def test():
    d = dict();
    for i in range(1,1000000): d[str(i)] = str(i+1)
    r = 17
    s=0
    for i in range(1,100000):
        r = r*r % 1000000
        b = d[str(r)]
        s += int(b);
    print("in dict_test after summation; last value retrieved should be",r+1,", was ",b)
    print("Retrieved and summed 100000 values; sum is ",s)
    fileptr = open('pytest.bin','wb')
    b = pickle.dumps(d)
    print("pickled dict. Length of bytes object is",len(b))
    t1 = "678" in d
    t2 = "-1" in d
    if (t1 and not t2):
        print("contains test ok")
    else:
        print("contains test failed")
    for i in range(1,1000000):
        if (i%100 > 0): del d[str(i)]
    print("Size of dictionary after popping is ",len(d))
    t = 0
    for i in iter(d): t += int(i)
    print ("Sum of remaining keys is ",t)
    deflt = d.get("100",666)
    print("dict_get on existing key 100; should return 101. Returned ",deflt)
    deflt = d.get("37",666)
    print("dict_get on non-existing key; should return default value 666. Returned ",deflt)
    other = dict()
    for j in range(11,200,20):
      other[str(j)] = 2*j
    d.update(other)
    items = iter(d.items())
    for k in range(0,10):
        key,value = next(items)
        print("item #",k,"is: key=",key,", value=",value)
    key,value = d.popitem()
    print("popitem gives: key=",key,", value=",value)
    key,value = d.popitem()
    print("popitem gives: key=",key,", value=",value)
    print("Size of dictionary should be 10007; is ",len(d));
    return
    
test()


        
    
        
