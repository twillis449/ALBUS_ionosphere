import numpy
from copy import deepcopy

# Hampel filter to find and replace outliers -  algorithm from Ronald Pearson
def hampel(data,win,t):
  filtered_data = deepcopy(data)
  print('win, t ', win,t)
  print('shape ', data.shape[0])
  print('bounds ', win, data.shape[0]-win-1)
  for i in range(win, data.shape[0]-win):
    upper_bound = i+1+win
    lower_bound = i-win
    median_wk = numpy.median(data[lower_bound:upper_bound])
    diff =  t * 1.4826 * numpy.median(numpy.abs(data[lower_bound:upper_bound] - median_wk))
    difference = numpy.abs(data[i] - median_wk)
    if difference > diff: 
      print ('*** outlier: i, data difference, diff', i, data[i], difference, diff)
      filtered_data[i] = median_wk
  return filtered_data
