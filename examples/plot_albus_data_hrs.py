#!/usr/bin/env python

# plot all output data as a function of time
import os
import sys
import numpy
import math 
import matplotlib.pyplot as plt


def getdata(filename):
        text = open(filename, 'r').readlines()
        L = len(text)
        i = 0
        # skip over all stuff before actual data
        while(text[i][0:13] != 'seq  rel_time'):
           i = i+1

        data = []
        rm = []
        stec = []
        elev = []
        vtec = []
        data_err = []
        rel_time = []
        start = i+1
        # get actual data
        for i in range( start,len(text)):
          try:
            info = text[i].split()
            if int(info[2]) == 0:
#             print('info', info)
              latest = float(info[3]) / 3600
              rel_time.append(latest)
              elev.append(float(info[5]))
              stec.append(float(info[7]))
              rm.append(float(info[8]))
              vtec.append(float(info[9]))
              try:
                data_err.append(float(info[10]))
              except:
                pass
          except:
            pass
        return rel_time,elev, stec, rm, vtec, data_err, latest

# columns for data
# 5 - elevation in degrees
# 6 - azimuth in degrees
# 7 - TEC (in tec units) in the current azimuth/elevation direction
# 8 - Rotation Measure (radians/m^2) in the current azimuth/elevation direction
# 9 - correction factor to convert STEC to value at the zenith
# 10 - formal error in STEC fit (personally I find this error to be too
#    large but I am not sure how to adjust it)

def main( argv ):
  data_file = argv[1]
  rel_time,elev, stec, rm, vtec, data_err, latest = getdata(data_file)
  print('final time', latest)
  plt.xlim(0,latest)
  x_label = 'relative time (hours)'
  if len(data_err) > 0:
    fig, axs = plt.subplots(5)
    axs[4].set(ylabel='data_err')
    axs[4].set(xlabel=x_label)
    axs[4].plot(rel_time, data_err,'bo')
  else:
    fig, axs = plt.subplots(4)
    axs[3].set(xlabel=x_label)
  fig.suptitle = 'Vertically stacked sublots'
  axs[0].set(ylabel='Elev (deg)')
  axs[1].set(ylabel='STEC (TEC U)')
  axs[2].set(ylabel='RM (rad/m^2)')
  axs[3].set(ylabel='Vtec corr')
  axs[0].plot(rel_time, elev,'ro')
  axs[1].plot(rel_time, stec,'bo')
  axs[2].plot(rel_time, rm,'bo')
  axs[3].plot(rel_time, vtec,'go')

  plot_file =  argv[1] + '_data_plot'

# remove and "." in this string
  pos = plot_file.find('.')
  if pos > -1:
    plot_file = plot_file.replace('.','_')
  plt.savefig(plot_file)
  plt.show()
#=============================
# argv[1]  incoming ALBUS results file 
if __name__ == "__main__":
  main(sys.argv)
