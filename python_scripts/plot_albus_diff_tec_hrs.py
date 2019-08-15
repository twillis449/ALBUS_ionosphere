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
              latest = float(info[3])/3600.0
              rel_time1.append(float(info[3])/3600.0)
              rm1.append(float(info[7]))
          except:
            pass
        return rel_time, rm, rel_time1, rm1, latest




def main( argv ):
  print 'processing ALBUS file ', argv[1]
  x_data, y_data, x_data1, y_data1, latest  = getdata(argv[1], argv[2])
# for i in range(len(y_data)):
#_data[i] = (y_data[i] - y_data1[i]) * (-1)
# for i in range(4,len(y_data)):
  for i in range(len(y_data)):
     try:
#      print i, i-5
#      y_data[i] = (y_data[i] - y_data1[i-4])
       y_data[i] = (y_data[i] - y_data1[i])
     except:
       pass
  xlim(0, latest)
  plot(x_data, y_data,'ro')
# plot(x_data[4:len(y_data)-4], y_data[4:len(y_data)-4],'ro')
# plot(x_data1,y_data1,'bo')
  xlabel('UT Time (hours)')
  ylabel('STEC difference (rad/m^2)')
  title_string = 'STEC difference as a function of time'

# plot(x_data, y_data,'ro')
# plot(x_data1,y_data1,'bo')
# xlabel('relative time (seconds)')
# ylabel('RM (rad/m^2)')
# title_string = argv[1] + ' : RM difference as a function of time'
# title_string = '3C286 Dec 2012 : RM difference as a function of time'
# title_string = 'Sobey Pulsar 1: RM difference as a function of time'
# title_string = 'J0636-2041: RM difference as a function of time'
  title(title_string)
  grid(True)

  plot_file =  'stations_diff_tec_plot'
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
