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
              rel_time.append(float(info[3]))
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
              rm1.append(float(info[7])-0.5)
          except:
            pass
        return rel_time, rm, rel_time1, rm1




def main( argv ):
  print 'processing ALBUS file ', argv[1]
  print 'processing ALBUS file ', argv[2]
  x_data, y_data, x_data1, y_data1  = getdata(argv[1], argv[2])

  plot(x_data, y_data)
  plot(x_data1,y_data1)
  xlabel('relative time (seconds)')
  ylabel('STEC (TEC Units)')
# title_string = argv[1] + ' : RM difference as a function of time'
# title_string = '3C286 Dec 2012 : RM difference as a function of time'
  title_string = 'STEC s a function of time'
  title(title_string)
  grid(True)

  plot_file =  'stations_stec_plot'
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
