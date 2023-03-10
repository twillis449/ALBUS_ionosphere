#!/usr/bin/env python

import os
import sys
import numpy
import math 
import hampel
from pylab import *
from copy import deepcopy
# Savitzky-Golay filte
from scipy.signal import savgol_filter

def getdata( filename ):
        text = open(filename, 'r').readlines()
        L = len(text)
        i = 0
        # skip over all stuff before actual data
#       reference time for rel_time=0: year,month,day,hr,min,sec  2022 8 8 14 0 0.0
        while(text[i][0:14] != 'reference time'):
           i = i+1
        info = text[i].split()
        sec = float(info[-1])
        min = float(info[-2])
        hour = float(info[-3])
        ref_time = hour + min/60.0 + sec/3600.0
        while(text[i][0:13] != 'seq  rel_time'):
           i = i+1
        stec = []
        stec_err = []
        rel_time = []
        start = i+1
        # get actual data
        for i in range( start,len(text)):
          try:
            info = text[i].split()
            if int(info[2]) == 0:
              elev = float(info[5])
              if elev >= 15.0:
                latest = ref_time + float(info[3]) / 3600
                rel_time.append(latest)
                stec.append(float(info[7]))
                try:
                  stec_err.append(float(info[10]))
                except:
                  pass
          except:
            pass
        stec_arr = numpy.array(stec)
        stec_err = numpy.array(stec_err)
        rel_time = numpy.array(rel_time)
        return rel_time, stec_arr, stec_err, latest, ref_time

def main( argv ):
  STEC = True
  print('processing ALBUS file ', argv[1])
  x_data, y_data, y_err, latest, ref_time  = getdata(argv[1])
# Savitzky-Golay filter
  y_data = savgol_filter(y_data, 7, 1)
# print('shapes ', x_data.shape, y_data.shape, y_err.shape)
  xlim(ref_time,latest)
  if y_err.shape[0] == 0:
    print('calling plot')
    plot(x_data, y_data,'ro')
  else:
    print('calling errorbar')
    plot(x_data, y_data,'ro')
    errorbar(x_data, y_data,yerr=y_err, fmt='ro')
  xlabel('hours (UT)')
  if STEC:
    ylabel('STEC (TEC Units)')
    title_string = 'STEC as a function of time'
    plot_file =  argv[1] + '_stec_plot'
  else:
    ylabel('VTEC (TEC Units)')
    title_string = 'VTEC as a function of time'
    plot_file =  argv[1] + '_vtec_plot'
  title(title_string)
  grid(True)

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
