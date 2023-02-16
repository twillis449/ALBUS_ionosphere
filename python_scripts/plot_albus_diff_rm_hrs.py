#!/usr/bin/env python
from __future__ import (print_function)
import os
import sys
import numpy
import math 
from pylab import *

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
            info = text[i].split()
            if int(info[2]) == 0:
              rel_time.append(float(info[3])/3600.0)
              rm.append(float(info[8]))
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
            info = text[i].split()
            if int(info[2]) == 0:
              latest = float(info[3])/3600.0
              rel_time1.append(float(info[3])/3600.0)
              rm1.append(float(info[8]))
          except:
            pass
        return rel_time, rm, rel_time1, rm1, latest




def main( argv ):
  print('processing ALBUS file ', argv[1], ' ',argv[2])
  x_data, y_data, x_data1, y_data1, latest  = getdata(argv[1], argv[2])
  for i in range(len(y_data)):
     try:
       y_data[i] = (y_data[i] - y_data1[i])
     except:
       pass
  xlim(0, latest)
  plot(x_data, y_data,'ro')
  xlabel('UT Time (hours)')
  ylabel('RM (rad/m^2)')
  title_string = 'RM difference as a function of time'

  grid(True)

  plot_file =  'stations_diff_rm_diff_plot'
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
