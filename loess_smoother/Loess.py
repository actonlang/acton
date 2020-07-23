import sys
sys.path.append('..')

import basic_numpy.ListNp as ListNp

def tricubic(x):
    y = ListNp.zeros_like(x)    
    idx = ListNp.bitwise_and(ListNp.gte(x, -1), ListNp.lte(x, 1))
    ListNp.set_bindex(y, idx, ListNp.power(ListNp.plus(1.0, ListNp.negate(ListNp.power(ListNp.abs(ListNp.bindex(x, idx)), 3))), 3))
    
    return y


class Loess(object):

    @staticmethod
    def normalize_array(array):
        min_val = ListNp.min(array)
        max_val = ListNp.max(array)
        return ListNp.div(ListNp.minus(array, min_val), (max_val - min_val)), min_val, max_val

    def __init__(self, xx, yy, degree=1):
        self.n_xx, self.min_xx, self.max_xx = self.normalize_array(xx)
        self.n_yy, self.min_yy, self.max_yy = self.normalize_array(yy)
        self.degree = degree

    @staticmethod
    def get_min_range(distances, window):
        min_idx = ListNp.argmin(distances)
        n = len(distances)
        if min_idx == 0:
            return ListNp.arange(0, window)
        if min_idx == n-1:
            return ListNp.arange(n - window, n)

        min_range = [min_idx]
        while len(min_range) < window:
            i0 = min_range[0]
            i1 = min_range[-1]
            if i0 == 0:
                min_range.append(i1 + 1)
            elif i1 == n-1:
                min_range.insert(0, i0 - 1)
            elif distances[i0-1] < distances[i1+1]:
                min_range.insert(0, i0 - 1)
            else:
                min_range.append(i1 + 1)
        return ListNp.array(min_range)

    @staticmethod
    def get_weights(distances, min_range):
        max_distance = ListNp.max(ListNp.index(distances, min_range))
        weights = tricubic(ListNp.div(ListNp.index(distances, min_range), max_distance))
        return weights

    def normalize_x(self, value):
        return (value - self.min_xx) / (self.max_xx - self.min_xx)

    def denormalize_y(self, value):
        return value * (self.max_yy - self.min_yy) + self.min_yy

    def estimate(self, x, window, use_matrix=False, degree=1):
        n_x = self.normalize_x(x)
#        print('n_x=', n_x)
        distances = ListNp.abs(ListNp.minus(self.n_xx, n_x))
#        print('distances=', distances)
        min_range = self.get_min_range(distances, window)
#        print('min_range=', min_range)
        weights = self.get_weights(distances, min_range)
#        print('weights=', weights)

        # Note: use_matrix is not supported yet, because computing the (Moore-Penrose) pseudo-inverse of a matrix is not yet implemented in ListNp.
        # So we are limited to degree 1 arrays, thus linear local regression for Loess :
        if use_matrix or degree > 1:
            wm = ListNp.multiply(ListNp.eye(window), weights)
            xm = ListNp.ones((window, degree + 1))

            xp = ListNp.array([[math.pow(n_x, p)] for p in range(degree + 1)])
            for i in range(1, degree + 1):
                xm[:, i] = ListNp.power(ListNp.index(self.n_xx, min_range), i)

            ym = ListNp.index(self.n_yy, min_range)
            xmt_wm = ListNp.multiply(ListNp.transpose(xm), wm)
            beta = ListNp.multiply(ListNp.multiply(ListNp.linalg.pinv(ListNp.multiply(xmt_wm, xm)), xmt_wm), ym) # This doesn't work yet, as we have no ListNp.linalg.pinv
            y = ListNp.multiply(beta, xp)[0]
        else:
            xx = ListNp.index(self.n_xx, min_range)
            yy = ListNp.index(self.n_yy, min_range)
            sum_weight = ListNp.sum(weights)
            sum_weight_x = ListNp.dot(xx, weights)
            sum_weight_y = ListNp.dot(yy, weights)
            sum_weight_x2 = ListNp.dot(ListNp.multiply(xx, xx), weights)
            sum_weight_xy = ListNp.dot(ListNp.multiply(xx, yy), weights)

            mean_x = sum_weight_x / sum_weight
            mean_y = sum_weight_y / sum_weight

            b = (sum_weight_xy - mean_x * mean_y * sum_weight) / \
                (sum_weight_x2 - mean_x * mean_x * sum_weight)
            a = mean_y - b * mean_x
            y = a + b * n_x
        return self.denormalize_y(y)


def main():
    xx = ListNp.array([0.5578196, 2.0217271, 2.5773252, 3.4140288, 4.3014084,
                   4.7448394, 5.1073781, 6.5411662, 6.7216176, 7.2600583,
                   8.1335874, 9.1224379, 11.9296663, 12.3797674, 13.2728619,
                   4.2767453, 15.3731026, 15.6476637, 18.5605355, 18.5866354,
                   18.7572812])
    yy = ListNp.array([18.63654, 103.49646, 150.35391, 190.51031, 208.70115,
                   213.71135, 228.49353, 233.55387, 234.55054, 223.89225,
                   227.68339, 223.91982, 168.01999, 164.95750, 152.61107,
                   160.78742, 168.55567, 152.42658, 221.70702, 222.69040,
                   243.18828])

    loess = Loess(xx, yy)

    for x in xx:
        y = loess.estimate(x, window=7, use_matrix=False, degree=1)
        print(x, y)


if __name__ == "__main__":
    main()
