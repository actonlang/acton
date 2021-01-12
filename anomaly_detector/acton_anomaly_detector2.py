'''
@author: aagapi
'''

import numpy as np

import pandas as pd
from optparse import OptionParser

do_plots = True
if do_plots:
    import matplotlib.pyplot as plt
    from statsmodels.tsa.seasonal import DecomposeResult

import sys
sys.path.append('python_libs/stl/')
from stl import decompose, forecast, naive, drift, mean, seasonal_naive
import datetime

TRAIN_RATIO=0.75

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

anomaly_examples = {
                    'level_change' : ['ambient_temperature_system_failure.csv', 'ec2_cpu_utilization_825cc2.csv', 'ec2_cpu_utilization_ac20cd.csv', 'grok_asg_anomaly.csv', 'rds_cpu_utilization_cc0c53.csv', 'rds_cpu_utilization_e47b3b.csv'],
                    'outliers' : ['ec2_cpu_utilization_fe7f93.csv', 'rogue_agent_key_hold.csv'],
                    'seasonal' : ['Twitter_volume_GOOG.csv', 'art_daily_jumpsdown.csv', 'art_daily_jumpsup.csv', 'art_daily_nojump.csv', 'exchange-2_cpc_results.csv', 'exchange-2_cpm_results.csv', 'exchange-3_cpm_results.csv', 'nyc_taxi.csv', 'occupancy_6005.csv', 'occupancy_t4013.csv'],
                    'trending' : ['ambient_temperature_system_failure.csv'],
                    'variance_change' : ['exchange-3_cpm_results.csv']    
    }

def plot_feature(observed_idx, observed, col_name):
    fig, ax1 = plt.subplots(figsize=(12, 5))
    plt.title('{} time plot'.format(col_name))
    plt.plot(observed, color='b')
    ax1.set_xlabel('time')
    ax1.set_ylabel('{}'.format(col_name), color='k')
    plt.legend(['{}'.format(col_name)], loc=(0.01, 0.95))
    plt.savefig('%s/feature_plot_%s.png' % (output_dir, col_name))
    plt.show()

def plot_decomp(trend, seasonal, resid, observed, period_averages, feature):
    
    decomp = DecomposeResult(seasonal=seasonal,
                         trend=trend,
                         resid=resid, 
                         observed=observed,
                         period_averages=period_averages)
    
    decomp.plot()
    plt.savefig('%s/decomp_%s.png' % (output_dir, feature))
    plt.show()

def plot_forecast(observed, observed_short, observed_idx, observed_idx_short, trend, seasonal, resid, forecast_array, forecast_idx, feature):
    plt.title('Forecast plot')
    
    plt.plot(observed_idx, observed, label='truth') ## '-'
    plt.plot(observed_idx_short, observed_short, label='obs') ## '-'
    plt.plot(observed_idx_short, trend, label='decomp.trend') ## ':'
    plt.plot(forecast_idx, forecast_array, label='FCAST' ) ## '-'
    plt.legend();    
    plt.savefig('%s/forecast_%s.png' % (output_dir, feature))
    plt.show()

def plot_forecast_and_anomalies(observed, observed_short, trend, seasonal, resid, forecast_array, forecast_idx, feature, anomalies, tag):
    plt.title('Anomaly algorithm: %s'%(tag))
    
    plt.plot(observed_idx, observed, label='truth') ## '-'
    plt.plot(observed_idx_short, observed_short, label='obs') ## '-'
    plt.plot(observed_idx_short, trend, label='decomp.trend') ## ':'
    plt.plot(forecast_idx, forecast_array, label='FCAST' ) ## '-'
    
    for (start, end) in anomalies:
        print(start - forecast_idx[0], end - forecast_idx[0])
        plt.plot(start, 10, end, 10, marker = 'o')

    plt.legend();    
    plt.savefig('%s/forecast_%s_%s.png' % (output_dir, feature, tag))

    plt.show()


def arrays_to_dict(observed_idx, observed):
    return {observed_idx[i]: observed[i] for i in range(len(observed_idx))}

df = pd.read_csv(file_name_x)

if feature is None:
    feature = file_name_x[(file_name_x.rfind('/')+1):file_name_x.rfind('.')]
## print(feature)


DEFAULT_EMA_SMOOTHING_FACTOR = 0.2
DEFAULT_EMA_WINDOW_SIZE_PCT = 0.2
# Percentage threshold on anomaly score below which is considered noises.
DEFAULT_NOISE_PCT_THRESHOLD = 0.001

def denoise_scores(scores):
    if scores:
        maximal = max(scores.values())
        if maximal:
            for key in scores:
                if scores[key] < DEFAULT_NOISE_PCT_THRESHOLD * maximal:
                    scores[key] = 0
    return scores

def compute_ema(smoothing_factor, points):
    ema = []
    if(len(points) > 0):
        ema.append(points[0])
    for i in range(1, len(points)):
        ema.append(smoothing_factor * points[i] + (1 - smoothing_factor) * ema[i - 1])
    return ema

def compute_anom_score_ema(lag_window_points, point, smoothing_factor):
    ema = compute_ema(smoothing_factor, lag_window_points)[-1]
    return abs(point - ema)

def ema_anomaly_detector(timestamps, values, smoothing_factor = DEFAULT_EMA_SMOOTHING_FACTOR, lag_window_size = None, use_lag_window = True):
    """
    Compute anomaly scores based on an exponential moving average.
    """
    
    lag_window_size = lag_window_size if lag_window_size is not None else int(len(timestamps) * DEFAULT_EMA_WINDOW_SIZE_PCT)
    anom_scores = {}

    if use_lag_window:
        stdev = np.std(values)
        for i in range(len(timestamps)):
            timestamp = timestamps[i]
            value = values[i]

            if i < lag_window_size:
                anom_score = compute_anom_score_ema(values[:i + 1], value, smoothing_factor)
            else:
                anom_score = compute_anom_score_ema(values[i - lag_window_size: i + 1], value, smoothing_factor)
            if stdev:
                anom_scores[timestamp] = anom_score / stdev
            else:
                anom_scores[timestamp] = anom_score
    else:
        ema = compute_ema(smoothing_factor, values)
        stdev = np.std(values)
        for i in range(len(timestamps)):
            timestamp = timestamps[i]
            value = values[i]
            anom_score = abs((value - ema[i]) / stdev) if stdev else value - ema[i]
            anom_scores[timestamp] = anom_score

    return denoise_scores(anom_scores)

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


def sign_test_anomaly_detector(x, y, k=24, alpha=0.05, offset=0.0, conf=0.01, gap=0):
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


## df['timestamp']=pd.to_datetime(df['timestamp'])
## df = df.set_index('timestamp')

observed_idx = df['timestamp'].values
observed = df['value'].values

if do_plots:
    plot_feature(observed_idx, observed, feature)  

period=int(24 * 60 / 5)
loess_window = 0.8

(trend, seasonal, resid, period_averages, phase) = decompose(observed, period=period, lo_window_frac=loess_window)
if do_plots:
    plot_decomp(trend, seasonal, resid, observed, period_averages, feature)

no_periods=int(len(observed)/period)
observed_short = observed[:int(TRAIN_RATIO*no_periods)*period]
observed_idx_short = observed_idx[:int(TRAIN_RATIO*no_periods)*period]
observed_test = observed[int(TRAIN_RATIO*no_periods)*period:]
observed_idx_test = observed_idx[int(TRAIN_RATIO*no_periods)*period:]

(trend, seasonal, resid, period_averages, phase) = decompose(observed_short, period=period, lo_window_frac=loess_window)
(forecast_array, forecast_idx) = forecast(observed_short, observed_idx_short,
                 trend, seasonal, resid, period_averages, phase,
                 steps=len(observed_test), fc_func=drift,
                 forecast_seasonal=True, correlate_phase=False)

if do_plots:
    plot_forecast(observed, observed_short, observed_idx, observed_idx_short, trend, seasonal, resid, forecast_array, forecast_idx, feature)

df_observed_dict=arrays_to_dict(observed_idx_test, observed_test)
fcast_dict=arrays_to_dict(forecast_idx, forecast_array)

anomaly_windows = sign_test_anomaly_detector(observed_test, forecast_array, k=24, alpha=0.2, offset=0.0, conf=0.01, gap=0)
anomaly_timestamps = []
for (start, end) in anomaly_windows:
   anomaly_timestamps.append((observed_idx_test[start], observed_idx_test[end])) 
print('Detected anomaly windows:', anomaly_timestamps)

if do_plots:
    plot_forecast_and_anomalies(observed, observed_short, trend, seasonal, resid, forecast_array, forecast_idx, feature, anomaly_timestamps, 'anomaly_detector')

## anomaly_scores = ema_anomaly_detector(observed_idx, observed)
## print(anomaly_scores)

if do_plots:
    plt.close('all')
    

