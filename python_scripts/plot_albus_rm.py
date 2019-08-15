#!/usr/bin/env python

import os
import sys
import numpy
import math 
import hampel
from pylab import *

from string import split, strip

def getdata( filename ):
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
              rm.append(float(info[8]))
          except:
            pass
        rm_arr = numpy.array(rm)
        filtered_data= hampel.hampel(rm_arr, 5, 4)
        filtered_data= hampel.hampel(filtered_data, 10, 1)
        diff = filtered_data - rm_arr
        print diff
        return rel_time, filtered_data
#       return rel_time, rm_arr


def main( argv ):
  print 'processing ALBUS file ', argv[1]
  x_data, y_data  = getdata(argv[1])

  plot(x_data, y_data,'ro')
  xlabel('relative time (seconds)')
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
