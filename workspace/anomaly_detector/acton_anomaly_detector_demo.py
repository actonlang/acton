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

def arrays_to_dict(observed_idx, observed):
    return {observed_idx[i]: observed[i] for i in range(len(observed_idx))}

if feature is None:
    feature = file_name_x[(file_name_x.rfind('/')+1):file_name_x.rfind('.')]
## print(feature)

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

class Sensor(object):
    def __init__(self, file_name_x, feature, anomaly_detector, read_window_size = 100, repeats=100, trend=0.0):
        self.repeats = repeats
        self.trend = trend
        self.read_window_size = read_window_size
        self.anomaly_detector = anomaly_detector
        df = pd.read_csv(file_name_x)
        self.observed_idx = df['timestamp'].values
        self.observed = df['value'].values
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
    def __init__(self, period, loess_window, min_train_window_size=3000):
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
        (trend, seasonal, resid, period_averages, phase) = decompose(observed_short, period=self.period, lo_window_frac=self.loess_window)
        (forecast_array, forecast_idx) = forecast(observed_short, observed_idx_short,
                                                     trend, seasonal, resid, period_averages, phase,
                                                     steps=(len(observed)-len(observed_short)), fc_func=drift,
                                                     forecast_seasonal=True, correlate_phase=False)
                
        df_observed_dict=arrays_to_dict(observed_idx, observed)
        fcast_dict=arrays_to_dict(forecast_idx, forecast_array)
        
        anomaly_scores = ema_anomaly_detector(observed_idx, observed)
        print(anomaly_scores)

anomaly_detector = AnomalyDetector(period=int(24 * 60 / 5), loess_window = 0.8, min_train_window_size=3000)
sensor = Sensor(file_name_x, feature, anomaly_detector, read_window_size = 100, repeats=2, trend=0.0)
sensor.run()





