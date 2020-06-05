#!/usr/bin/env python

import os
import sys
import numpy
import math 
from copy import deepcopy
from pylab import *


# Hampel filter to find and replace outliers -  algorithm from Ronald Pearson
def hampel(data,window,t):

  filtered_data = deepcopy(data)
# can replace the following lines by the 'pad' function in numpy 1.7
  length = data.shape[0]
  zeroes = numpy.zeros((length+2*window,), dtype=numpy.float64)
# fill in range before and after actual data points
  for i in range(window):
    zeroes[i] = data[0]
    zeroes[i+length+window] = data[length-1]
  zeroes[window:window+length] = data
  data = zeroes
# numpy.lib.pad(data,(window,window),'edge')
  print('window, t ', window,t)
  print('shape ', data.shape[0])
  print('bounds ', window, data.shape[0]-window-1)
  for i in range(window, data.shape[0]-window):
    upper_bound = i+1+window
    lower_bound = i-window
#   print 'data range',lower_bound, upper_bound
#   print 'data', data[lower_bound:upper_bound]
    median_wk = numpy.median(data[lower_bound:upper_bound])
#   print i, median_wk
#   print data[i:upper_bound] - median_wk
#   print numpy.abs(data[lower_bound:upper_bound] - median_wk)
    diff =  t * 1.4826 * numpy.median(numpy.abs(data[lower_bound:upper_bound] - median_wk))
    difference = numpy.abs(data[i] - median_wk)
#   print i, data[i], difference, median_wk
    if difference > diff: 
      print('*** outlier: i, data difference, diff', i-window, data[i], difference, diff)
      filtered_data[i-window] = median_wk
  return filtered_data
# return data

def getdata( filename ):
        text = open(filename, 'r').readlines()
        L = len(text)
        i = 0
        # skip over all stuff before actual data
        while(text[i][0:13] != 'seq  rel_time'):
           i = i+1

        stec = []
        rel_time = []
        start = i+1
        # get actual data
        for i in range( start,len(text)):
          try:
            info = text[i].split()
            if int(info[2]) == 0:
              rel_time.append(float(info[3]) / 3600)
              stec.append(float(info[8]))
          except:
            pass
        stec_arr = numpy.array(stec)
        filtered_data= hampel(stec_arr, 5, 3)
        diff = filtered_data - stec_arr
        return rel_time, filtered_data
#       return rel_time, stec_arr

def main( argv ):
  print('processing ALBUS files ', argv[1], ' ', argv[2])
  x_data, y_data  = getdata(argv[1])
  x_data, y_data1  = getdata(argv[2])
  plot(x_data, y_data,'bo')
  y_data = y_data-y_data1
# y_data = y_data/y_data1
  filtered_data= hampel(y_data, 10, 1) 
  filtered_data= hampel(y_data, 10, 1) + y_data1
# filtered_data= hampel(y_data, 10, 1) * y_data1

  plot(x_data, filtered_data,'ro')
  xlabel('relative time (hours)')
  ylabel('STEC (TEC Units')
  title_string = argv[1] + ' : STEC as a function of time'
  title(title_string)
  grid(True)

  plot_file =  argv[1] + '_stec_plot'
# remove and "." in this string
  pos = plot_file.find('.')
  if pos > -1:
    plot_file = plot_file.replace('.','_')
  savefig(plot_file)
  show()


#=============================
# argv[1]  incoming ALBUS results file 
if __name__ == "__main__":
  main(sys.argv)
