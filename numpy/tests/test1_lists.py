import sys

n = int(sys.argv[1])
iters =  int(sys.argv[2])
x = [i/(n-1) for i in range(n)]

for i in range(iters):
    r = [[a + b for a in x] for b in x]
    print(sum([sum(a) for a in r]))
