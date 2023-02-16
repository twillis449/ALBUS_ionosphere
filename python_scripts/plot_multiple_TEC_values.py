#!/usr/bin/env python

# a python script to read in two fits files containing L, M and N tracks
# 2018/06/11 - comment - I have no idea what I was using this for !!
from __future__ import (print_function)
import os
import sys
import numpy
import pyfits
#import matplotlib
import matplotlib.pyplot as plt

def main( argv ):
  fits_label = 'test_of_conj_observed_'
  fits_label = 'test_of_conj_observed_-58_L_M_0_'
  fits_label = 'test_of_conj_observed_-58_'
  fits_label = 'test_of_central_observed_-58_'
# fits_label = 'test_of_central_beam_rotation-58_'
  fig = plt.figure()
  title  = 'Stokes Variation along FPA track in Sky Reference Frame'
  fig.text(.22,.95, title)
  # get arguments
  i = []
  q = []
  u = []
  v = []
  x = []
  for m in range(120):
    fits_file = fits_label + str(m) + '.fits'
    hdulist = pyfits.open(fits_file)
    i.append(hdulist[0].data)
    q.append(hdulist[1].data)
    u.append(hdulist[2].data)
    v.append(hdulist[3].data)
    x.append(m * 1.0)
  i_array = numpy.array(i)
  q_array = numpy.array(q)
  u_array = numpy.array(u)
  v_array = numpy.array(v)
  x_array = numpy.array(x)
  x_array = (x_array - 59.0) / 15.0

  i1 = fig.add_subplot(221)
  i1.set_ylabel('I')
# i1.plot(x_array, i_array,'bo')
  i1.plot(x_array, i_array)
  i1.grid(True)
# plt.xlabel('Sample Number')
# plt.title('FPA track in Az-El telescope reference frame')
  q1 = fig.add_subplot(223)
  q1.set_ylabel('Q / I')
  q1.set_xlabel('HA')
# plt.ylim(0.,0.1)
# q1.plot(x_array, q_array/i_array,'bo')
  q1.plot(x_array, q_array/i_array)
  q1.grid(True)
  u1 = fig.add_subplot(224)
  u2 = u1.twinx()
# plt.setp(plt.gca().get_yticklabels(), visible=False)
# plt.setp(plt.gca().yaxis.tick_left(visible=False))
  for tick in u1.yaxis.get_major_ticks():
    tick.tick1On = False
    tick.label1On = False
  u2.set_ylabel('U / I')
  u2.set_xlabel('HA')
# u2.plot(x_array, u_array/i_array,'bo')
  u2.plot(x_array, u_array/i_array)
  u2.grid(True)
  v1 = fig.add_subplot(222)
  v2 = v1.twinx()
  for tick in v1.yaxis.get_major_ticks():
    tick.tick1On = False
    tick.label1On = False
  v2.set_ylabel('V / I')
# v2.plot(x_array, v_array/i_array,'bo')
  v2.plot(x_array, v_array/i_array)
# plt.ylim(0.,0.1)
  v2.grid(True)
  plt.savefig(fits_label + 'track.png')
  plt.show()
#=============================
# argv[1] first L,M N track
# argv[2] second L,M N track
if __name__ == "__main__":
  main(sys.argv)
