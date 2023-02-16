#!/usr/bin/env python
from __future__ import (print_function)
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
    list_tec = []
    list_rel_time = []
    while (i < L) :
        # skip over all stuff before actual data
#       print text[i]
        try:
          while(text[i][0:13] != 'seq  rel_time'):
             i = i+1
          start = i+1
          # get actual data
          new_list = True
          for j in range( start,len(text)):
            try:
              info = split(strip(text[j]))
              if int(info[2]) == 0:
                time = float(info[3])
                if new_list:
                  print ('starting a new list')
                  try:
                    list_rel_time.append(rel_time)
                    list_tec.append(tec)
                    print ('*** appending lists of length', len(tec))
                  except:
                    pass
                  rel_time = []
                  tec = []
                new_list = False
                rel_time.append(float(info[3]) / 3600)
                tec.append(float(info[7]))
              i = j
            except:
              break
        except:
          break
# append final list
    list_rel_time.append(rel_time)
    list_tec.append(tec)
    return list_rel_time, list_tec
def main( argv ):
  print ('processing ALBUS file ', argv[1])
  xlabel('relative time (hours)')
  ylabel('STEC')
# title_string = argv[1] + ' : STEC as a function of time'
  title_string = 'STEC as a function of time'
  title(title_string)
  grid(True)
  total_x_data, total_y_data  = getdata(argv[1])
  for j in range(len(total_x_data)): 
    x_data = total_x_data[j]
    y_data = total_y_data[j]
    plot(x_data, y_data)

  plot_file =  argv[1] + '_stec_multi_plot'
# remove any "." in this string
  pos = plot_file.find('.')
  if pos > -1:
    plot_file = plot_file.replace('.','_')
  savefig(plot_file)
  show()


#=============================
# argv[1]  incoming ALBUS results file 
if __name__ == "__main__":
  main(sys.argv)
