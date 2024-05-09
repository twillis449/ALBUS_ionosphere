#!/usr/bin/env python

# script to generate dilated images
import os
import os.path
import sys
import numpy
import math
import timeit
from astropy.coordinates import SkyCoord
from read_input_table import process_input_file
from make_morphology_mask import make_mask
from optparse import OptionParser

def process_images(filename, filter_size, filter_type, offset_flux, use_conv, double_erode, do_batch):
        print('processing file ', filename)
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
            field_name = names[i] +'_conv' + '.fits'
          else:
            field_name = names[i] + '.fits'
          print('i, field_name_i ', i, field_name)
          parameter_list = []
          parameter_list.append(' ')
          parameter_list.append(field_name)
          parameter_list.append(offset_flux)
          parameter_list.append(filter_size)
          parameter_list.append(filter_type)
          parameter_list.append(do_batch)
          parameter_list.append(double_erode)
          make_mask(parameter_list)

def main( argv ):
   parser = OptionParser(usage = '%prog [options] ')
   parser.add_option('-f', '--file', dest = 'filename', help = 'Filename with radio source names, positions, redshit etc (default = None)', default = None)
   parser.add_option('-s', '--filter_size', dest = 'filter_size', help = 'Size of structure element (default = 0)', default = 0)
   parser.add_option('-e', '--filter_type', dest = 'filter_type', help = 'Type of structure element D(isk) or R(ectangle) (default = D)', default = 'D')
   parser.add_option('-t', '--threshold', dest = 'threshold', help = 'Threshhold value for mask, in units of noise (default = 6)', default = 6)
   parser.add_option('-c','--use_conv', dest = 'use_conv', help = 'Select a convolved image (default = F)', default = False)
   parser.add_option('-b','--batch', dest = 'use_batch', help = 'Run in batch mode (no interactive response) (default = F)', default = False)
   parser.add_option('-d','--double_e', dest = 'use_double', help = 'Use second erode (no interactive response) (default = T)', default = True)
   (options,args) = parser.parse_args()
   print('options', options)
   filename = options.filename
   filter_size = int(options.filter_size)
   filter_type = options.filter_type
   offset_flux = float(options.threshold)
   use_conv = options.use_conv
   if use_conv != False:
     use_conv = True
   use_batch = options.use_batch
   if use_batch != False:
     use_Batch = True
   use_double_erode = options.use_double
   if use_double_erode != True:
     use_double_erode = False

   start_time = timeit.default_timer()
   process_images(filename, filter_size, filter_type, offset_flux, use_conv, use_double_erode, use_batch)
   elapsed = timeit.default_timer() - start_time
   print("Run Time:",elapsed,"seconds")

#=============================
#
# e.g. example: run as 'get_morphology_images.py -f abellsouth.csv -t D --th 6'
# 
if __name__ == "__main__":
  main(sys.argv)

