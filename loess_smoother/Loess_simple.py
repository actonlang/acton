import numpy as np

# Here we do not prune arrays to the current window size, but keep full-size arrays.
# This seems faster for length of x and y up to ca 3-400, depending on window size.
def loess(x, y, xin, win):
    win1 = win-1       # to be compatible with Loess.py and Loess_with_np.py
    xd  = np.abs(xin[:,None] - x)
    w   = np.clip(xd/np.sort(xd)[:,win1][:,None], 0.0, 1.0)
    ws  = (1 - w ** 3) ** 3
    a00 = np.sum(ws,1)
    a01 = ws @ x
    a11 = ws @ (x*x)
    b0  = ws @ y
    b1  = ws @ (x*y)
#   For 2x2 matrices it's not worthwhile to call np.linalg methods.
    det = a00*a11-a01*a01
    return ((a11*b0 - a01*b1) + (-a01*b0 + a00*b1)*xin)/det
