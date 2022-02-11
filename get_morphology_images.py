#!/usr/bin/env python

# script to generate dilated images
import os
import os.path
import sys
import numpy
import math
import timeit
import subprocess
from astropy.coordinates import SkyCoord
from read_input_table import process_input_file
from make_morphology_mask import make_mask

def process_images(filename, filter_size, filter_type, offset_flux, use_conv_l, do_batch):
        print('processing file ', filename)
        if use_conv_l =='T':
          print('using convolved_images')
          use_conv = True
        else:
          use_conv = False
        text = open(filename, 'r').readlines()
        info = text[0].split()
        print('opening info ', info)
        freq = info[1]
        print('specified freq',freq)

# not all these parameters are used here
        freq, names, ra_deg, dec_deg = process_input_file(filename, True)
        num_proc = len(ra_deg)
        for i in range (num_proc):
          position = SkyCoord(ra_deg[i], dec_deg[i], unit='deg',frame='icrs')
          print('i, position', i, position)
          ra_dec = position.to_string('hmsdms')
          print('input ra_dec ', ra_dec)
          blank =  " "
          underscore = "_"
          out_ra_dec = ra_dec.replace(blank, underscore)
          print('output ra_dec ', out_ra_dec)
          if use_conv:
            field_name = names[i] +'_conv'
          else:
            field_name = names[i] 
          print('i, field_name_i ', i, field_name)
          parameter_list = []
          parameter_list.append(' ')
          parameter_list.append(field_name)
          parameter_list.append(offset_flux)
          parameter_list.append(filter_size)
          parameter_list.append(filter_type)
          parameter_list.append(do_batch)
          make_mask(parameter_list)

def main( argv ):
# argv[1] = name of pipeline input file with information such as 
#           frequency, positions
# argv[2] = size of structure element
# argv[3] = type of structure element D(isk) or R(ectangle)
# argv[4] = value of mask offset (multiplied by noise determined from 
#           breizorro) 
# arvg[5] = T (use a convolved image) or F (used unconvolved image)
# argv[6] = T (use dilated image) or F (use eroded image)
# argv[7] = numerical value passed to subtract_polygon_data script
  print('in get_morphology_images script')
  print('argv', argv)
  filename = argv[1]       # name of file containing object positions
  filter_size = argv[2]    # interger, should be an odd number
  filter_type = argv[3]    # 'D' or 'R'   
  print('incoming filter type', filter_type)
  offset_flux = argv[4]    # = factor by which to multiply breizorro noise
  use_conv = argv[5]       # if T, look for a convolved image
  do_batch = argv[6]   # if T do batch processing, else do not
  print('do_batch', do_batch)

# e.g. sample run as 'get_morphology_images.py 3C236.csv 3 D 6 F T 

  start_time = timeit.default_timer()
  process_images(filename, filter_size, filter_type, offset_flux, use_conv, do_batch)
  elapsed = timeit.default_timer() - start_time
  print("Run Time:",elapsed,"seconds")



#=============================
# argv[1]  incoming positions file
if __name__ == "__main__":
  main(sys.argv)

