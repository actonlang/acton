import numpy as np
import sys

n = int(sys.argv[1])
iters = int(sys.argv[2])

x = np.linspace(0,1,n)

for i in range(iters):
    r = np.empty((n,n))
    
    for i in range(n):
        for j in range(n):
            r[i,j] = x[i] + x[j]
            
    print(np.sum(r))
