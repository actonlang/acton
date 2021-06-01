'''
@author: aagapi
'''

import numpy as np

import pandas as pd
from optparse import OptionParser

import sys
sys.path.append('python_libs/stl/')
from stl import decompose, forecast, naive, drift, mean, seasonal_naive

TRAIN_RATIO=0.75
DEFAULT_EMA_SMOOTHING_FACTOR = 0.2
DEFAULT_EMA_WINDOW_SIZE_PCT = 0.2
# Percentage threshold on anomaly score below which is considered noises.
DEFAULT_NOISE_PCT_THRESHOLD = 0.001

parser = OptionParser()
parser.add_option("-x", "--input-file-x", type="str", dest="file_name_x", default=None, help="Path to input file name X")
parser.add_option("-n", "--feature-name", type="str", dest="feature_name", default=None, help="Input feature name")
parser.add_option("-o", "--output-dir", type="str", dest="output_dir", default=None, help="Path to output dir for plots")
parser.add_option("-R", "--train-ratio", dest="train_ratio", type="float", default=0.75, help="Train ratio")

options, args = parser.parse_args()

file_name_x=options.file_name_x
feature=options.feature_name
output_dir=options.output_dir
TRAIN_RATIO=options.train_ratio

if feature is None:
    feature = file_name_x[(file_name_x.rfind('/')+1):file_name_x.rfind('.')]
## print(feature)

def np_sign(arr):
    """  Stand-in for numpy sign()"""
    signs = []
    for i in range(len(arr)):
        signs.append(1 if arr[i] > 0 else -1)
    return np.array(signs)

def np_ones(size):
    """  Stand-in for numpy ones()"""
    ones = []
    for i in range(size):
        ones.append(1)
    return np.array(ones)

def np_argwhere(arr):
    """  Stand-in for numpy argwhere()"""
    indices = []
    for i in range(len(arr)):
        if arr[i] > 0:
            indices.append(i)
    return np.array(indices)

def np_fmax_const(arr, const):
    """ Stand-in for numpy fmax with a constant """
    values = []
    for i in range(len(arr)):
        values.append(arr[i] if arr[i] > const else const)
    return np.array(values)

def np_convolve_valid(arr1, arr2):
    """ Stand-in for numpy convolve with mode = valid (no extrapolation on edges) """
    flipped_arr2 = []
    for i in range(len(arr2)):
        flipped_arr2.append(arr2[len(arr2) - i - 1])
    
    if len(arr2) > len(arr1):
        return np.array([])
    
    convolution = []
    for index in range(len(arr1) - len(arr2) + 1):
        sum = 0
        for i in range(len(arr2)):
            sum += arr1[index + i] * flipped_arr2[i]
        convolution.append(sum)
    
    return np.array(convolution)    

def merge_ranges(ranges, max_gap):
    """
        Merge ranges which are closer than max_gap
    """
    merged_ranges = []
    for range in ranges:
        if merged_ranges:
            curr_start, curr_end = range
            # compare against last interval in merged_ranges
            pre_start, pre_end = merged_ranges[-1]
            if curr_start - pre_end < max_gap:
                # merge current range with the last range in the list
                merged_ranges[-1] = (pre_start, max(curr_end, pre_end))
            else:
                # append the new range to current one
                merged_ranges.append(range)
        else:
            # no merged ranges - just add this one
            merged_ranges.append(range)

    return merged_ranges


def sign_test_anomaly_detector(x, y, k, alpha, offset, conf, gap):
    if len(x) != len(y) or len(x) < k:
        return list()

    # our filter to convolve with - just counts
    f = np_ones(k)

    # Threshold below calculated as quantile function for a binomial distribution with:
    #   - probability of success=0.5
    #   - confidence level p=0.01
    #   - window size = 24 (2 hours at 5 minutes intervals)
    # == Smallest k such that Prob(X <= k) >= p
    # == scipy.stats.binom.ppf(1 - 0.01, 24, 0.5) - 1
    
    qthresh = 17.0
    
    alpha1 = 1. + alpha
    diff_array = x - offset
    diff_array = diff_array - (alpha1 * y)

    # this is 1 if bigger 0 otherwise
    d = np_fmax_const(np_sign(diff_array), 0)
#    d = np_fmax_const(np_sign(x - offset - (1. + alpha) * y), 0)
#    d = np.fmax(np.sign(x - offset - (1. + alpha) * y), 0)

    con = np_convolve_valid(d, f)
#    con = np.convolve(f, d, mode='valid')

    a = np_fmax_const(con - qthresh, 0)
#    a = np.fmax(con - qthresh, 0)

    ranges = []
    for t in np_argwhere(a):
        ranges.append((t, t + k))        

    ranges = merge_ranges(ranges, gap)

    return ranges

class Sensor(object):
    def __init__(self, file_name_x, feature, anomaly_detector, read_window_size, repeats, trend):
        self.repeats = repeats
        self.trend = trend
        self.read_window_size = read_window_size
        self.anomaly_detector = anomaly_detector
        df = pd.read_csv(file_name_x)
        self.observed_idx = df['timestamp'].values
        self.observed = df['value'].values
        print(len(self.observed))
        self.first_ts = self.observed_idx[0]
        self.last_ts = self.observed_idx[-1]
        self.timestep = self.observed_idx[1] - self.observed_idx[0]

    def fix_timestamps(self, observed_idx, add_lag):
        return (observed_idx + add_lag)

    def fix_trend(self, observed, repeat_count, trend):
        fixed_observed = np.zeros(len(observed))
        for i in range(len(observed)):
            fixed_observed[i] = observed[i] + (repeat_count*len(observed)+i)*trend
        return fixed_observed

    def run(self):
        for i in range(self.repeats):
            observed_idx = self.fix_timestamps(self.observed_idx, i*(self.last_ts - self.first_ts + self.timestep))
            if self.trend > 0:
                self.observed = self.fix_trend(self.observed, i, self.trend)
            num_windows = len(observed_idx) // self.read_window_size + (len(observed_idx) % self.read_window_size)

            for j in range(num_windows):
                observed_idx_w = observed_idx[j*self.read_window_size:(j+1)*self.read_window_size]
                observed_w = self.observed[j*self.read_window_size:(j+1)*self.read_window_size]
                print('Sensor: Sending window ', j, ' of repeat ', i, ', length=', len(observed_idx_w), len(observed_w))
                self.anomaly_detector.update(observed_idx_w, observed_w)

class AnomalyDetector(object):
    def __init__(self, period, loess_window, min_train_window_size):
        self.observed_idx = None
        self.observed = None
        self.min_train_window_size = min_train_window_size
        self.period = period
        self.loess_window = loess_window
        
    def update(self, observed_idx_w, observed_w):
        # Append received window:
        if self.observed is None:
            assert self.observed_idx is None
            self.observed_idx = observed_idx_w
            self.observed = observed_w
        else:
            assert self.observed_idx is not None
            self.observed_idx = np.append(self.observed_idx, observed_idx_w)
            self.observed = np.append(self.observed, observed_w)
        # Truncate training window to last min_train_window_size entries:
        if len(self.observed_idx) > self.min_train_window_size:
            self.observed_idx = self.observed_idx[-self.min_train_window_size:]
            self.observed = self.observed[-self.min_train_window_size:]
            print('AnomalyDetector: Forecasting on window of size', len(self.observed_idx), ', [', self.observed_idx[0], ', ' , self.observed_idx[-1], ']')
            self.detect_anomalies(self.observed_idx, self.observed)

    def detect_anomalies(self, observed_idx, observed):
        no_periods=int(len(observed)/self.period)
        observed_short = observed[:int(TRAIN_RATIO*no_periods)*self.period]
        observed_idx_short = observed_idx[:int(TRAIN_RATIO*no_periods)*self.period]
        observed_test = observed[int(TRAIN_RATIO*no_periods)*self.period:]
        observed_idx_test = observed_idx[int(TRAIN_RATIO*no_periods)*self.period:]
        (trend, seasonal, resid, period_averages, phase) = decompose(observed_short, period=self.period, lo_window_frac=self.loess_window)
        (forecast_array, forecast_idx) = forecast(observed_short, observed_idx_short,
                                                     trend, seasonal, resid, period_averages, phase,
                                                     steps=len(observed_test), fc_func=drift,
                                                     forecast_seasonal=True, correlate_phase=False)

        anomaly_windows = sign_test_anomaly_detector(observed_test, forecast_array, k=24, alpha=0.2, offset=0.0, conf=0.01, gap=0)
        anomaly_timestamps = []
        for (start, end) in anomaly_windows:
            if end >= len(observed_idx_test):
                end = len(observed_idx_test) - 1
            anomaly_timestamps.append((observed_idx_test[start], observed_idx_test[end])) 
        print('DETECTED ANOMALY WINDOWS:', anomaly_timestamps)
                
anomaly_detector = AnomalyDetector(period=int(24 * 60 / 5), loess_window = 0.8, min_train_window_size=4032)
sensor = Sensor(file_name_x, feature, anomaly_detector, read_window_size = 126, repeats=2, trend=0.0)
sensor.run()





