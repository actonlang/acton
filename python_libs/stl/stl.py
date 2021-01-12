import numpy as np

import sys
sys.path.append('python_libs/loess_smoother/')
from loess_smoother import loess

def naive(data):
    """The naive forecast for the next point is the value of the previous point. 
    """
    forecast = data[-1]
    return forecast

def seasonal_naive(data, n=7):
    """The seasonal naive forecast for the next point is the value observed ``n`` points
    prior in the series. 
    """
    forecast = data[-n]
    return forecast


def mean(data, n=3):
    """The mean forecast for the next point is the mean value of the previous ``n`` points in 
    the series.
    """
    # don't start averaging until we've seen n points
    if len(data[-n:]) < n:
        forecast = np.nan
    else:
        # nb: we'll keep the forecast as a float
        forecast = np.mean(data[-n:])
    return forecast


def drift(data, n=3):
    """The drift forecast for the next point is a linear extrapolation from the previous ``n`` 
    points in the series.
    """
    yi = data[-n]
    yf = data[-1]
    slope = (yf - yi) / (n - 1)
    forecast = yf + slope
    return forecast

def roll_array(arr, roll_by):
    # roll_by s'd be negative
    copy = np.zeros(len(arr))
    for i in range(len(arr)):
        if i-roll_by < len(arr):
            copy[i] = arr[i-roll_by]
        else:
            copy[i] = arr[i-roll_by-len(arr)]
    return copy

def tile_array(arr, no_tiles):
    # roll_by s'd be negative
    copy = np.zeros(len(arr) * no_tiles)
    for tile in range(no_tiles):
        for i in range(len(arr)):
            copy[tile * len(arr) + i] = arr[i]
    return copy

def decompose(observed, period=365, lo_window_frac=0.6):
    """ STL decomposition """
    # calc trend, remove from observation
    loess_window = int(lo_window_frac * len(observed))
##    print('period=', period, ', loess_window=', loess_window, ', len(observed)=', len(observed))
    xin = np.array([x for x in range(len(observed))])
    trend = loess(xin, observed, xin, loess_window) # 40
    detrended = observed - trend

    # period must not be larger than size of series to avoid introducing NaNs
    period = min(period, len(observed))
    max_no_period_values = len(observed) // period + 1

    # calc one-period seasonality, remove tiled array from detrended

    period_averages = []
    for i in range(period):
        period_averages.append(np.mean(detrended[i::period]))
    period_averages = np.array(period_averages)
    
##    period_averages = np.array([np.mean(detrended[i::period]) for i in range(period)])
    
#    period_averages = np.zeros(period) # []
#    for i in range(period):
#        no_period_values = 0
#        period_sum = 0
#        for j in range(max_no_period_values):
#            if j * period < len(observed):
#                period_sum += detrended[j * period]
#                no_period_values += 1
#        period_averages[i] = period_sum / no_period_values
    
    # 0-center the period avgs
    period_averages -= np.mean(period_averages)
##    print('period_averages=', period_averages)

##    seasonal = np.tile(period_averages, len(observed) // period + 1)[:len(observed)] 
    seasonal = tile_array(period_averages, len(observed) // period + 1)[:len(observed)]
    
##    seasonal = np.zeros(len(observed))
##    for i in range(len(observed)): 
##        seasonal[i] = period_averages[i % period]
        
##    print('seasonal=', seasonal)    
        
    resid = detrended - seasonal
    
    phase = len(observed) % period

    return (trend, seasonal, resid, period_averages, phase)

def forecast(observed, observed_idx,
             trend, seasonal, resid, period_averages, phase,
             fc_func, steps=10, forecast_seasonal=True, correlate_phase=False):
    """Forecast the given decomposition ``stl`` forward by ``steps`` steps using the forecasting 
    function ``fc_func``, optionally including the calculated seasonality.    """

    # iteratively forecast trend:
    forecast_array = np.array([])
    for step in range(steps):
        # make this prediction on all available data
        pred = fc_func(np.append(trend, forecast_array))
        # add this prediction to current array
        forecast_array = np.append(forecast_array, pred)

    # forecast start and index are determined by observed data 
    observed_timedelta = observed_idx[-1] - observed_idx[-2]
    forecast_idx_start = observed_idx[-1] + observed_timedelta
##    print('forecast_idx_start=', forecast_idx_start, ', forecast_idx_end=', (forecast_idx_start + steps * observed_timedelta), ', steps=', (steps + 1))
    forecast_idx = np.linspace(forecast_idx_start, forecast_idx_start + steps * observed_timedelta, steps)

    # (optionally) forecast seasonal & combine 
    if forecast_seasonal:
        detrended_array = observed - trend # np.asanyarray(observed - trend).squeeze()

        if correlate_phase:
            # track index and value of max correlation
            seasonal_ix = 0
            max_correlation = -np.inf
            # loop over indexes=length of period avgs
            for i, x in enumerate(period_averages):
                # work slices backward from end of detrended observations
                if i == 0:
                    # slicing w/ [x:-0] doesn't work
                    detrended_slice = detrended_array[-len(period_averages):]        
                else:
                    detrended_slice = detrended_array[-(len(period_averages) + i):-i]
                # calculate corr b/w period_avgs and detrend_slice
                this_correlation = np.correlate(detrended_slice, period_averages)[0]
                if this_correlation > max_correlation:
                    # update ix and max correlation
                    max_correlation = this_correlation
                    seasonal_ix = i
        else:
            seasonal_ix = phase
            
        # roll seasonal signal to matching phase
        rolled_period_averages = roll_array(period_averages, -seasonal_ix)
        # tile as many time as needed to reach "steps", then truncate
        tiled_averages = tile_array(rolled_period_averages, (steps // len(period_averages) + 1))[:steps]
        # add seasonal values to previous forecast
        forecast_array += tiled_averages                
    
    return (forecast_array, forecast_idx)
