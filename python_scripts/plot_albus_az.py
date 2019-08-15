#!/usr/bin/env python

import os
import sys
import numpy
import math 
from pylab import *

from string import split, strip


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
            info = split(strip(text[i]))
            if int(info[2]) == 0:
              rel_time.append(float(info[3]))
              azimuth = 57.2957795 * float(info[6])
              if azimuth < 0.0:
                azimuth = -1.0 * azimuth
              az.append(azimuth)
          except:
            pass
        return rel_time, az




def main( argv ):
  print 'processing ALBUS file ', argv[1]
  x_data, y_data  = getdata(argv[1])

  plot(x_data, y_data,'ro')
  xlabel('relative time (seconds)')
  ylabel('Azimuth (radians)')
# title_string = argv[1] + ' : STEC as a function of time'
  title_string = 'RI_G03: Azimuth as a function of time'
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
