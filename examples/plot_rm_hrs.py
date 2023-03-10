#!/usr/bin/env python

import os
import sys
import numpy
import math 
#import hampel
# Savitzky-Golay filte
from scipy.signal import savgol_filter

from pylab import *

#from string import split, strip

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
                error_ratio = rm_val * float(info[10]) / float(info[7])
                rm_error.append(error_ratio)
          except:
            pass
        rm_arr = numpy.array(rm)
        rel_time = numpy.array(rel_time)
        return rel_time, rm_arr, rm_error, latest, ref_time

def main( argv ):
  RM = True
  print('processing ALBUS file ', argv[1])
  x_data, y_data, error_vals, latest, ref_time  = getdata(argv[1])
  y_data = savgol_filter(y_data, 7, 1)
# print('shapes ', x_data.shape, y_data.shape, y_err.shape)
  xlim(ref_time,latest)
  print('calling plot')
  plot(x_data, y_data,'ro')
  xlabel('hours (UT)')
  if RM:
    ylabel('RM ')
    title_string = 'RM as a function of time'
    plot_file =  argv[1] + '_rm_plot'
    errorbar(x_data, y_data,yerr=error_vals, fmt='ro')
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
