import Loess as A
import Loess_with_np as B
import Loess_simple as C
import Loess_prune as D

import sys
import time
import numpy as np
import math
import basic_numpy.ListNp as ListNp

def main():
    n = int(sys.argv[1])

    xx0 = ListNp.array([0.5578196, 2.0217271, 2.5773252, 3.4140288, 4.3014084,
                   4.7448394, 5.1073781, 6.5411662, 6.7216176, 7.2600583,
                   8.1335874, 9.1224379, 11.9296663, 12.3797674, 13.2728619,
                   14.2767453, 15.3731026, 15.6476637, 18.5605355, 18.5866354,
                   18.7572812])
    yy0 = ListNp.array([18.63654, 103.49646, 150.35391, 190.51031, 208.70115,
                   213.71135, 228.49353, 233.55387, 234.55054, 223.89225,
                   227.68339, 223.91982, 168.01999, 164.95750, 152.61107,
                   160.78742, 168.55567, 152.42658, 221.70702, 222.69040,
                   243.18828])

    xx = np.array(xx0)

    yy = np.array(yy0)

    win = 7
    if n>21:
#        np.random.seed(1)
        win = n//4
        xx = np.linspace(0, 2 * math.pi, n)
        yy = np.sin(xx) + 0.3 * np.random.randn(n)
        xx0 = xx.tolist()
        yy0 = yy.tolist()
        print("using random vectors of length",n,"and window of size",win)
    else:
        print("using input vectors of length 21 and window of size 7")
        
    print("Loess.py")
    t = time.time()
    loess = A.Loess(xx0, yy0)
    yest = [loess.estimate(x, window=win, use_matrix=False, degree=1) for x in xx0]
    t1 = time.time()
    print('[',round(yest[0],3),round(yest[1],3),'...',round(yest[-2],3),round(yest[-1],3),'], time =',round(t1-t,4))

    print("Loess_with_np.py")
    t = time.time()
    loess = B.Loess(xx, yy)
    yest = [loess.estimate(x, window=win, use_matrix=False, degree=1) for x in xx]
    t1 = time.time()
    print('[',round(yest[0],3),round(yest[1],3),'...',round(yest[-2],3),round(yest[-1],3),'], time =',round(t1-t,4))

    print("Loess_simple.py")
    t = time.time()
    yest = C.loess(xx,yy,xx,win)
    t1 = time.time()
    print('[',round(yest[0],3),round(yest[1],3),'...',round(yest[-2],3),round(yest[-1],3),'], time =',round(t1-t,4))

    print("Loess_prune.py")
    t = time.time()
    yest = D.loess(xx,yy,xx,win)
    t1 = time.time()
    print('[',round(yest[0],3),round(yest[1],3),'...',round(yest[-2],3),round(yest[-1],3),'], time =',round(t1-t,4))

    if n<21:
        print("Command line arg > 21 give random input vectors of that length")

#    import matplotlib.pyplot as pl
#    pl.clf()
#    pl.plot(xx, yy, label='y')
#    pl.plot(xx, yest, label='yest')
#    pl.legend()
#    pl.show()

if __name__ == "__main__":
    main()
