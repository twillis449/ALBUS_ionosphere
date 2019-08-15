#!/usr/bin/env python

import os
import sys
import numpy
import math 
from pylab import *

from string import split, strip


def getdata( filename, filename1 ):
        text = open(filename, 'r').readlines()
        L = len(text)
        i = 0
        # skip over all stuff before actual data
        while(text[i][0:13] != 'seq  rel_time'):
           i = i+1

        rm = []
        rel_time = []
        start = i+1
        # get actual data
        for i in range( start,len(text)):
          try:
            info = split(strip(text[i]))
            if int(info[2]) == 0:
              rel_time.append(float(info[3])/3600.0)
              rm.append(float(info[7]))
          except:
            pass

        text = open(filename1, 'r').readlines()
        L = len(text)
        i = 0
        # skip over all stuff before actual data
        while(text[i][0:13] != 'seq  rel_time'):
           i = i+1

        rm1 = []
        rel_time1 = []
        start = i+1
        # get actual data
        for i in range( start,len(text)):
          try:
            info = split(strip(text[i]))
            if int(info[2]) == 0:
              rel_time1.append(float(info[3]))
              rm1.append(float(info[7]))
          except:
            pass
        return rel_time, rm, rel_time1, rm1




def main( argv ):
  print 'processing ALBUS file ', argv[1]
  x_data, y_data, x_data1, y_data1  = getdata(argv[1], argv[2])
# for i in range(4,len(y_data)):
  signal = 0.0
  num_values = 0
  for i in range(len(y_data)):
     try:
       y_data[i] = y_data[i] - y_data1[i]
       signal = signal + y_data[i]
       num_values = num_values + 1
     except:
       pass
  print 'using number of points ', num_values
  print ' mean is ', signal / num_values
  data = numpy.array(y_data)
  std_dev = numpy.std(data)
  print 'rms', std_dev
  plot(x_data, y_data,'ro')
  xlabel('relative time (seconds)')
  xlabel('time (UT hours)')
  xlabel('relative time (UT hours)')
  ylabel('VTEC difference (TEC Units)')
  ylabel('STEC difference (TEC Units)')
  title_string = 'TEC difference as a function of time'
  title(title_string)
  xlim(0,24)
  grid(True)

  plot_file =  'stations_diff_stec_diff_plot'
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
