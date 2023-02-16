from __future__ import (print_function)
import os
import sys
import numpy
import math
import matplotlib
import matplotlib.pyplot as plt


try:
   import cPickle as pickle
except:
   import pickle


def analyse_iono(rmextract_pickle, albus_pickle):
# read in pickle files
  rmextract_data = pickle.load(open(rmextract_pickle))
  rmextract_iono_RM_array = rmextract_data['rm_array']
  rmextract_iono_STEC_array = rmextract_data['stec_array']
  print 'rmextract shape ', rmextract_iono_STEC_array.shape


  albus_data = pickle.load(open(albus_pickle))
  albus_iono_RM_array = albus_data[0]
  albus_iono_STEC_array = albus_data[1]
  print 'albus shape ', albus_iono_STEC_array.shape
  iono_RM_diff_array = rmextract_iono_RM_array - albus_iono_RM_array
  iono_STEC_diff_array = rmextract_iono_STEC_array - albus_iono_STEC_array

  plt.imshow(iono_RM_diff_array)
  plt.colorbar()
  plt.title('ionosphere RM difference')
  plt.xlabel('time (hours)')
  plt.ylabel('elevation Sequence north to south in 5 deg steps')
  plt.savefig('iono_plot_RM_diff')
  plt.show()
  plt.imshow(iono_STEC_diff_array)
  plt.colorbar()
  plt.xlabel('time (hours)')
  plt.ylabel('elevation Sequence north to south in 5 deg steps')
  plt.title('ionosphere STEC difference')
  plt.savefig('iono_plot_TEC_diff')
  plt.show()

def main( argv ):
# argv[1] = name of RMextract pickle file
# argv[2] = name of ALBUS pickle file 
  analyse_iono(argv[1], argv[2])
  return
#=============================
if __name__ == "__main__":
  main(sys.argv)

