#!/usr/bin/env python

import os
import sys
import numpy
import math 
from hampel import *
from pylab import *
from copy import deepcopy
from optparse import OptionParser
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
        rm = []
        rel_time = []
        rm_error=[]
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
                rm_val = float(info[8])
                rm.append(rm_val)
                try:
                  error_ratio = rm_val * float(info[10]) / float(info[7])
                  rm_error.append(error_ratio)
                except:
                  continue
          except:
            pass
        rm_arr = numpy.array(rm)
        rel_time = numpy.array(rel_time)
        return rel_time, rm_arr, rm_error, latest, ref_time

def main( argv ):
  RM = True
  parser = OptionParser(usage = '%prog [options] ')
  parser.add_option('-f', '--file', dest = 'filename', help = 'Name of ALbus file to be processed  (default = None)', default = None)
  parser.add_option('-s', '--smooth', dest = 'smooth', help = 'Type of smoothing, sg , h, or None  (default = None)', default = None)
  (options,args) = parser.parse_args()
  filename = options.filename
  print('processing ALBUS file ', filename)
  smoothing = str(options.smooth).lower()
  x_data, y_data, error_vals, latest, ref_time  = getdata(filename)
# Savitzky-Golay filter
  if smoothing == 'sg':
    print('Doing Savitzky-Golay smoothing')
    y_data = savgol_filter(y_data, 7, 1)
  elif smoothing == 'h':
    print('Doing Hampel filtering')
    filtered = hampel(y_data, 5, 4)
    y_data = hampel(filtered, 10, 1)
  
# print('shapes ', x_data.shape, y_data.shape, y_err.shape)
  RM = True
  xlim(ref_time,latest)
  if len(error_vals) == 0:
    plot(x_data, y_data,'ro')
  else:
    plot(x_data, y_data,'ro')
    errorbar(x_data, y_data,yerr=error_vals, fmt='ro')
  if RM:
    ylabel('RM (rad/m^2)')
    xlabel('UT (hours)')
    title_string = 'RM as a function of time'
    plot_file =  filename + '_rm_plot'
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
