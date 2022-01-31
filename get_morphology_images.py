#!/usr/bin/env python

# script to generate dilated images
import os
import os.path
import sys
import numpy
import math
import subprocess
from astropy.coordinates import SkyCoord
from read_input_table import process_input_file

def process_images(filename, filter_size, filter_type, offset_flux, use_conv_l, use_dilation, offset_value_flux):
        print('processing file ', filename)
        if use_conv_l =='T':
          print('using convolved_images')
          use_conv = True
        else:
          use_conv = False
        if use_dilation =='T':
          print('using dilated images')
          use_dilation = True
        else:
          use_dilation = False
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
          if use_dilation:
            cmd =  'make_morphology_mask.py ' + field_name + ' ' + offset_flux + ' T ' + filter_size + ' ' + filter_type 
          else:
            cmd =  'make_morphology_mask.py ' + field_name + ' ' + offset_flux + ' F ' + filter_size + ' ' + filter_type 
          print('processing ', cmd)
          returned_value = subprocess.call(cmd, shell=True)  # returns the exit code in unix
          cmd = ' '
          if use_dilation:
            json_file = field_name + '-dilated.json_polygons_data'
            print('sending json file ', json_file)
            cmd = 'combine_images.py ' + field_name  +  ' ' + offset_value_flux + ' ' + json_file + ' T'
          else:
            json_file = field_name + '-eroded.json_polygons_data'
            cmd = 'combine_images.py ' + field_name +  ' ' + offset_value_flux + ' ' + json_file + ' F'
          print('************* processing ', cmd)
          if len(cmd) > 2:
            returned_value = subprocess.call(cmd, shell=True)  # returns the exit code in unix

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
  use_dilation = argv[6]   # if T do dilation, else do only erosion
  try:
    offset_value_flux  = argv[7] # amount (in mJy) to remove from source 
                                 # subtraction (default = 0.0)
                                 # only used in script combine_images.py
  except:
    offset_value_flux = '0.0'

# e.g. sample run as 'get_morphology_images.py 3C236.csv 3 D 6 F T 

  process_images(filename, filter_size, filter_type, offset_flux, use_conv, use_dilation, offset_value_flux)

#=============================
# argv[1]  incoming positions file
if __name__ == "__main__":
  main(sys.argv)

