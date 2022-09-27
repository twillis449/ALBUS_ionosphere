#!/usr/bin/env python

import os
import sys
import numpy
import math 
import hampel
from pylab import *

#from string import split, strip

def getdata( filename ):
        text = open(filename, 'r').readlines()
        L = len(text)
        i = 0
        # skip over all stuff before actual data
        while(text[i][0:13] != 'seq  rel_time'):
           i = i+1

        stec = []
        stec_err = []
        rel_time = []
        start = i+1
        # get actual data
        for i in range( start,len(text)):
          try:
#           info = text[i].split().strip()
            info = text[i].split()
            if int(info[2]) == 0:
              latest = float(info[3]) / 3600
              rel_time.append(latest)
              stec.append(float(info[5]))
              try:
                stec_err.append(float(info[10]))
              except:
                pass
          except:
            pass
        stec_arr = numpy.array(stec)
        stec_err = numpy.array(stec_err)
        rel_time = numpy.array(rel_time)
        filtered = hampel.hampel(stec_arr, 5, 4)
        filtered_data= hampel.hampel(filtered, 10, 1)
        diff = filtered_data - stec_arr
#       return rel_time, filtered_data, stec_err, latest
        return rel_time, stec_arr, stec_err, latest

def main( argv ):
  STEC = True
  print('processing ALBUS file ', argv[1])
  x_data, y_data, y_err, latest  = getdata(argv[1])
  print('shapes ', x_data.shape, y_data.shape, y_err.shape)

  xlim(0,latest)
  if y_err.shape[0] == 0:
    print('calling plot')
    plot(x_data, y_data,'ro')
  else:
#   print('calling errorbar')
    plot(x_data, y_data,'ro')
#   errorbar(x_data, y_data,yerr=y_err, fmt='ro')
  xlabel('UT time (hours)')
  if STEC:
    ylabel('Elevation (degrees)')
    title_string = 'Elevation as a function of time'
    plot_file =  argv[1] + '_elev_plot'
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
