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

def plot_forecast_and_anomalies(observed, observed_short, trend, seasonal, resid, forecast_array, forecast_idx, feature, anomalies, min_ts, tag):
    plt.title('Anomaly algorithm: %s'%(tag))
    
#    plt.plot(observed, '-', label='truth')
#    plt.plot(observed_short, '-', label='obs')
#    plt.plot(trend, ':', label='decomp.trend')
#    plt.plot(forecast_array, '-', label='FCAST' ) # fcast.columns[0]
    plt.plot(observed_idx, observed, label='truth') ## '-'
    plt.plot(observed_idx_short, observed_short, label='obs') ## '-'
    plt.plot(observed_idx_short, trend, label='decomp.trend') ## ':'
    plt.plot(forecast_idx, forecast_array, label='FCAST' ) ## '-'
    
    for anomaly in anomalies:
        window = anomaly.get_time_window()
        print(window[0] - forecast_idx[0], window[1] - forecast_idx[0])
        plt.plot(window[0], 10, window[1], 10, marker = 'o')
##        plt.plot(datetime.datetime.fromtimestamp(window[0]), 10, datetime.datetime.fromtimestamp(window[1]), 10, marker = 'o')

    plt.legend();    
    plt.savefig('%s/forecast_%s_%s.png' % (output_dir, feature, tag))

    plt.show()


def arrays_to_dict(observed_idx, observed):
    return {observed_idx[i]: observed[i] for i in range(len(observed_idx))}

def forecast_to_dict(df):
    dict = {}
    
    for idx,row in df.iterrows():
#        print(idx.timestamp())
#        print(df.loc[idx][0])
        dict[idx.timestamp()]=df.loc[idx][0]        

    return dict

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
df_short = df.head(int(TRAIN_RATIO*no_periods)*period)
df_observed = df.tail(len(df)-len(df_short))
(trend, seasonal, resid, period_averages, phase) = decompose(observed_short, period=period, lo_window_frac=loess_window)
(forecast_array, forecast_idx) = forecast(observed_short, observed_idx_short,
                 trend, seasonal, resid, period_averages, phase,
                 steps=(len(df)-len(df_short)), fc_func=drift,
                 forecast_seasonal=True, correlate_phase=False)

if do_plots:
    plot_forecast(observed, observed_short, observed_idx, observed_idx_short, trend, seasonal, resid, forecast_array, forecast_idx, feature)

min_ts = observed_idx[0]
df_observed_dict=arrays_to_dict(observed_idx, observed)
fcast_dict=arrays_to_dict(forecast_idx, forecast_array)

# print(len(df_observed_dict), len(fcast_dict))

# print(list(set(df_observed_dict.keys()) - set(fcast_dict.keys())))

anomaly_scores = ema_anomaly_detector(observed_idx, observed)
print(anomaly_scores)

if do_plots:
    plt.close('all')
    

