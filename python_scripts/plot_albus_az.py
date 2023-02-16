#!/usr/bin/env python
from __future__ import (print_function)
import os
import sys
import numpy
import math 
from pylab import *


def getdata( filename ):
        text = open(filename, 'r').readlines()
        L = len(text)
        i = 0
        # skip over all stuff before actual data
        while(text[i][0:13] != 'seq  rel_time'):
           i = i+1

        az = []
        rel_time = []
        start = i+1
        # get actual data
        for i in range( start,len(text)):
          try:
            info = text[i].split()
            if int(info[2]) == 0:
              rel_time.append(float(info[3]))
              azimuth = float(info[6])
              if azimuth < 0.0:
                azimuth = -1.0 * azimuth
              az.append(azimuth)
          except:
            pass
        return rel_time, az

def main( argv ):
  print('processing ALBUS file ', argv[1])
  x_data, y_data  = getdata(argv[1])

  plot(x_data, y_data,'ro')
  xlabel('relative time (seconds)')
  ylabel('Azimuth (radians)')
  title_string = 'Azimuth as a function of time'
  title(title_string)
  grid(True)

  plot_file =  argv[1] + '_az_plot'
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
