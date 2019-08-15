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
        stec = []
        rm = []
        az = []
        el = []
	am = []
        rel_time = []
	stat_pos=[]
	nrstat=0
	started=False
	for line in text:
		if line.split() and line.split()[0].strip()=='Prediction':
			stat_pos.append([float(line.split()[-3]),float(line.split()[-2]),float(line.split()[-1])])
			stec.append([])
			rm.append([])
			az.append([])
			el.append([])
			am.append([])
			rel_time.append([])
			nrstat+=1
			started=False
		# skip over all stuff before actual data
		if line[0:13] == 'seq  rel_time':
			started=True
			continue
		
		if not started:
			continue
		info = line.split()
		if int(info[2]) == 0:
			rel_time[-1].append(float(info[3]))
			az[-1].append(float(info[5]))
			el[-1].append(float(info[6]))
			stec[-1].append(float(info[7]))
			rm[-1].append(float(info[8]))
			am[-1].append(float(info[9]))

	my_dict={}
	my_dict['time']=np.array(rel_time)
	my_dict['stec']=np.array(stec)
	my_dict['az']=np.array(az)
	my_dict['el']=np.array(el)
	my_dict['rm']=np.array(rm)
	my_dict['am']=np.array(am)
	my_dict['stations']=np.array(stat_pos)

        return my_dict




def main( argv ):
  print 'processing ALBUS file ', argv[1]
  if len(argv)<2:
	  argv.append('stec')
  print 'plotting',argv[2]
  my_dict  = getdata(argv[1])
  nrstat=my_dict['stations'].shape[0]
  mylegend=[]
  for istat in range(nrstat):
	  x_data=my_dict['time'][istat]
	  y_data=my_dict[argv[2]][istat]
	  plot(x_data, y_data,'o')
	  mylegend.append('station %d'%istat)
  legend(mylegend)
  xlabel('relative time (seconds)')
  ylabel(argv[2])
  
  title_string = argv[1] + ' :' +argv[2]+' as a function of time'
  title(title_string)
  grid(True)

  plot_file =  argv[1] + '_'+argv[2]+'_plot'
# remove and "." in this string
  pos = plot_file.find('.')
  if pos > -1:
    plot_file = plot_file.replace('.','_')
  savefig(plot_file)
  show()
  #return my_dict

#=============================
# argv[1]  incoming ALBUS results file 
if __name__ == "__main__":
  main(sys.argv)
  
