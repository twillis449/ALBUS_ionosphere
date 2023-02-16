#!/usr/bin/env python
from __future__ import (print_function)
import os
import sys
import numpy
import math 
import csv
from copy import deepcopy
from pylab import *

from string import split, strip
try:
   import cPickle as pickle
except:
   import pickle

# Hampel filter to find and replace outliers -  algorithm from Ronald Pearson
def hampel(data,win,t):

  filtered_data = deepcopy(data)
  print 'win, t ', win,t
  print 'shape ', data.shape[0]
  print 'bounds ', win, data.shape[0]-win-1
  for i in range(win, data.shape[0]-win):
    upper_bound = i+1+win
    lower_bound = i-win
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
      print  '*** outlier: i, data difference, diff', i, data[i], difference, diff
      filtered_data[i] = median_wk
#     data[i] = median_wk
  return filtered_data
# return data

def getdata( filename, VTEC):
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
            info = split(strip(text[i]))
            if int(info[2]) == 0:
              latest = float(info[3]) / 3600
              rel_time.append(latest)
#             if VTEC:
#               vtec_factor = float(info[9])
#               stec.append(float(info[7]) * vtec_factor)
#             else:
              stec.append(float(info[7]))
          except:
            pass
        stec_arr = numpy.array(stec)
        filtered = hampel(stec_arr, 5, 4)
        filtered_data= hampel(filtered, 10, 1)
        diff = filtered_data - stec_arr
#       print diff
#       return rel_time, filtered_data
        return rel_time, stec_arr, latest

def main( argv ):
  VTEC = True
# print 'processing ALBUS global file ', argv[1]
# x_data, y_data, latest  = getdata(argv[1],VTEC)
# plot(x_data, y_data,'ro')
# plot(x_data, y_data,'r', label='All')
# plot(x_data, y_data,'ro')
  print 'processing ALBUS local file ', argv[1]
  x_data, y_data, latest  = getdata(argv[1],VTEC)
  plot(x_data, y_data,'b', linewidth=2.0,label='Nearest')
  xticks([12,24,36,48,60,72])
  xlim(12,72)
  xlabel('UT (hours)')
  xlabel('relative time (days)')
  xlabel('relative time (hours)')
  xlabel('time (UT hours)')
  if VTEC:
    ylabel('VTEC (TEC Units)')
    title_string = 'VTEC as a function of time'
    plot_file =  argv[1] + '_vtec_plot'
  else:
    ylabel('STEC (TEC Units)')
    title_string = 'STEC as a function of time'
    plot_file =  argv[1] + '_stec_plot'
  title(title_string)
  grid(True)

  try:
    getTEC_times  = numpy.array(pickle.load(open('getTEC_times_MILLSTONE')))
    getTEC_times = getTEC_times / 3600.0 
#   getTEC_times = getTEC_times / 3600.0 / 24.0
    getTEC_times = getTEC_times - getTEC_times[0]
    getTEC_values = numpy.array(pickle.load(open('getTEC_tec_values_MILLSTONE')))
    times = []
    values = []
    for i in range (getTEC_values.shape[0]):
      if getTEC_values[i] > -1.0:
        values.append(getTEC_values[i])
        times.append(getTEC_times[i])
    plot(times, values,'r--', label = 'CODE')
  except:
    pass
  opp_stec = []
  opp_time = []
# with open('Opperman_thesis_p94_2.csv', 'rb') as f:
# with open('Opperman_may2005.csv', 'rb') as f:
# with open('Opperman_thesis_p94_3.csv', 'rb') as f:
# with open('xiamen_gps.csv', 'rb') as f:
# with open('xiamen_gps_1.csv', 'rb') as f:
  with open('millstone_hill.csv', 'rb') as f:
    reader = csv.reader(f)
    for row in reader:
      try:
        opp_stec.append(float(row[1]))
        opp_time.append(float(row[0]))
      except:
        pass
# plot(opp_time, opp_stec,'g--', linewidth=3.0,label='Opp')
  plot(opp_time, opp_stec,'g--', linewidth=3.0,label='Mill')

# legend(loc=2)
  legend()

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
