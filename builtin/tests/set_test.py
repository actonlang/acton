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

s = set ()
s2 = set()
print("sets created")
for i in range(13,1000):
    s.add(i*i)

s.discard(64)
s.discard(225)
s.discard(10000)

n = 0
for k in range(0,1000):
    if k in s:
      n+=1
print("#elements <1000 is",n,"(should be 18)")

for i in range(0,500):
  s2.add(i*i*i*i)
print("size of s is",len(s),"(should be 985)")
print("size of s2 is",len(s2),"(should be 500)")
s3 = s & s2
print("size of intersection is",len(s3),"(should be 27)")
print("checking if intersection is lt both operands; returns",s3<s,"and",s3<s2)
it = iter(s3)
print("Iterating over intersection:")
for w in s3:
    print(w)
s4 = s | s2
print("size of union is",len(s4),"(should be 1458)")
print("checking if union is gt both operands; returns",s>s2,"and",s4>s2)
s5 = s ^ s2
print("size of symmetric difference is",len(s5))
print("popping and printing again elements in intersection")
while(s3):
    print("popped",s3.pop());
print("size of intersection is now",len(s3))
