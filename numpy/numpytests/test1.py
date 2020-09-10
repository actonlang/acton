import numpy as np
import sys

n = int(sys.argv[1])
iters= int(sys.argv[2])

x = np.linspace(0,1,n)

for i in range(iters):
    r = x[:,None]+x
    print(np.sum(r))


