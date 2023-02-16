#!/usr/bin/env python
from __future__ import (print_function)
import os
import sys
import numpy
import math 
import hampel
from pylab import *

# set following to True if you want to run data 
# through a Hampel filter to remove outliers
use_hampel = True

def getdata( filename ):
        text = open(filename, 'r').readlines()
        L = len(text)
        i = 0
        # skip over all stuff before actual data
        while(text[i][0:13] != 'seq  rel_time'):
           i = i+1

        rm = []
        rm_err = []
        rel_time = []


        start = i+1
        # get actual data
        for i in range( start,len(text)):
          try:
            info = text[i].split()
            if int(info[2]) == 0:
              latest = float(info[3])/3600.0
              rel_time.append(latest)
              rm_val = float(info[8])
              rm.append(rm_val)
              try:
                rm_error =  rm_val * (float(info[10]) / float(info[7]))
                rm_err.append(rm_error)
              except:
                pass
          except:
            pass
        rm_arr = numpy.array(rm)
        rm_err = numpy.array(rm_err)
        if use_hampel:
          print('running hampel filter')
          filtered_data= hampel.hampel(rm_arr, 5, 4)
          filtered_data= hampel.hampel(filtered_data, 10, 1)
          rm_arr = filtered_data
        return rel_time, rm_arr, rm_err, latest


def main( argv ):
  print('processing ALBUS file ', argv[1])
  x_data, y_data, y_err, latest  = getdata(argv[1])
  xlim(0,latest)
  if y_err.shape[0] == 0:
    plot(x_data, y_data,'ro')
  else:
    plot(x_data, y_data,'ro')
#   errorbar(x_data, y_data,yerr=y_err, fmt='o')
  xlabel('UT time (hours)')
  ylabel('RM (rad/m^2)')
  title_string = argv[1] + ' : RM as a function of time'
  title(title_string)
  grid(True)

  plot_file =  argv[1] + '_rm_plot'
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
