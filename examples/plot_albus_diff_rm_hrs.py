#!/usr/bin/env python

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
        start = i+1
        # get actual data
        for i in range( start,len(text)):
          try:
            info = text[i].split()
            if int(info[2]) == 0:
              latest = ref_time + float(info[3]) / 3600
              rel_time.append(latest)
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
              rel_time1.append(float(info[3])/3600.0)
              rm1.append(float(info[8]))
          except:
            pass
        return rel_time, rm, rm1


def main( argv ):
  print('processing ALBUS files ', argv[1], ' ', argv[2])
  x_data, y_data, y_data1  = getdata(argv[1], argv[2])
  for i in range(len(y_data)):
     try:
       y_data[i] = (y_data[i] - y_data1[i])
     except:
       pass
  plot(x_data, y_data,'ro')
  xlabel('UT Time (hours)')
  ylabel('RM difference (rad/m^2)')
  title_string = 'RM difference as a function of time'
  title(title_string)
  grid(True)

  plot_file =  'stations_diff_rm_plot'
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
