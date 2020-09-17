import numpy as np

# Here arrays are pruned to the size of the current window.
# Seems faster for array length greater than ca 400.
def loess(x, y, xin, win):
    win1 = win-1
    xd  = np.abs(xin[:,None] - x)
    p   = np.argpartition(xd,win1)
    ix  = np.min(p[:,0:win1],1)
    w   = np.vstack([xd[i][j:j+win1] for i,j in enumerate(ix)])/np.take_along_axis(xd,p[:,win1][:,None],1)
    ws  = (1 - w ** 3) ** 3
    x1  = np.vstack([x[i:i+win1] for i in ix])
    y1  = np.vstack([y[i:i+win1] for i in ix])
    a00 = np.sum(ws,1)
    a01 = np.sum(ws*x1,1) # = a10
    a11 = np.sum(ws*(x1*x1),1)
    b0  = np.sum(ws*y1,1)
    b1  = np.sum(ws*(x1*y1),1)
    det = a00*a11-a01*a01
    return ((a11*b0 - a01*b1) + (-a01*b0 + a00*b1)*xin)/det
 
