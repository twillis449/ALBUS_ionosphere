#!/usr/bin/env python

import os
import sys
import numpy
import math 
from hampel import *
from pylab import *
from copy import deepcopy
from optparse import OptionParser

def getdata( filename ):
        text = open(filename, 'r').readlines()
        L = len(text)
        i = 0
        # skip over all stuff before actual data
#       reference time for rel_time=0: year,month,day,hr,min,sec  2022 8 8 14 0 0.0
        while(text[i][0:14] != 'reference time'):
           i = i+1
        info = text[i].split()
        sec = float(info[-1])
        min = float(info[-2])
        hour = float(info[-3])
        ref_time = hour + min/60.0 + sec/3600.0
        while(text[i][0:13] != 'seq  rel_time'):
           i = i+1
        elev_val = []
        rel_time = []
        start = i+1
        # get actual data
        for i in range( start,len(text)):
          try:
            info = text[i].split()
            if int(info[2]) == 0:
              elev = float(info[5])
              elev_val.append(elev)
              latest = ref_time + float(info[3]) / 3600
              rel_time.append(latest)
          except:
            pass
        elev_arr = numpy.array(elev_val)
        rel_time = numpy.array(rel_time)
        return rel_time, elev_arr, latest, ref_time

def main( argv ):
  parser = OptionParser(usage = '%prog [options] ')
  parser.add_option('-f', '--file', dest = 'filename', help = 'Name of ALbus file to be processed  (default = None)', default = None)
  (options,args) = parser.parse_args()
  filename = options.filename
  print('processing ALBUS file ', filename)
  x_data, y_data, latest, ref_time  = getdata(filename)
  
# print('shapes ', x_data.shape, y_data.shape, y_err.shape)
  xlim(ref_time,latest)
  plot(x_data, y_data,'ro')
  ylabel('Elevation (degrees)')
  xlabel('UT (hours)')
  title_string = 'Elevation as a function of time'
  plot_file =  filename + '_elev_plot'
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
