#!/usr/bin/env python

import os
import sys
import numpy
import math 
from copy import deepcopy
from pylab import *

from string import split, strip

def getdata( filename ):
        text = open(filename, 'r').readlines()
        L = len(text)
        i = 0
        # skip over all stuff before actual data
        while(text[i][0:13] != 'seq  rel_time'):
           i = i+1

        list_elev = []
        list_rel_time = []
        start = i+1
        rel_time = []
        # get actual data
        for i in range( start,len(text)):
          try:
            info = split(strip(text[i]))
            if int(info[2]) == 0:
              time = float(info[3])
              if time == -300.0:
                print 'starting a new list'
                if len(rel_time) > 0:
                  print 'appending lists of length', len(elev)
                  list_rel_time.append(rel_time)
                  list_elev.append(elev)
                elev = []
                rel_time = []
              rel_time.append(float(info[3]) / 3600)
              elev.append(math.degrees(float(info[5])))
          except:
            pass
        return list_rel_time, list_elev

def main( argv ):
  print 'processing ALBUS file ', argv[1]
  x_data, y_data  = getdata(argv[1])
  
  for i in range(len(x_data)):
    plot(x_data[i], y_data[i])
  xlabel('relative time (hours)')
  ylabel('Elevation (deg)')
#  title_string = argv[1] + ' : Elevation as a function of time'
  title_string = 'Elevation as a function of time'
  title(title_string)
  grid(True)

  plot_file =  argv[1] + '_elev_plot'
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
