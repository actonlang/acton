import numpy as np
import sys

n = int(sys.argv[1])
iters= int(sys.argv[2])
for i in range(iters):
    x = np.linspace(i,i+1,n)
    r = x[:,None]+x
    print(np.sum(r))


