#!/usr/bin/env python

# script to generate galaxy polygons and then pass polygons
# to calaculate galaxy parameters
import os
import os.path
import sys
import numpy as np
import math 
import timeit
import subprocess
from astropy.io import fits
from astropy.coordinates import SkyCoord
from read_input_table import  process_input_file
from calculate_source_parms import analyze_image
from optparse import OptionParser


def process_images(filename, use_b_mask, use_conv, use_multi, do_subt, threshold_value,noise):
        print('processing file ', filename)
        print('use_b_mask', use_b_mask)
        print('use_conv', use_conv)
        print('use_conv', use_conv)
        print('use_multi', use_multi)
        print('do_subt', do_subt)
        print('threshold value', threshold_value)
        print('default noise (Jy)', noise)
        freq, names, ra_deg, dec_deg, las, las_raw, red_shift, spec_index = process_input_file(filename)
# for testing
        perform_test = False
        if perform_test:
          num_proc = 2
        else:
          num_proc = len(ra_deg)
        for i in range (num_proc):
          position = SkyCoord(ra_deg[i], dec_deg[i], unit='deg',frame='icrs')
          print('i, position', i, position)
          ra_dec = position.to_string('hmsdms')
          print('input ra_dec ', ra_dec)
          blank =  " "
          underscore = "_"
          if names[i] != 'None':
            out_ra_dec = names[i]
          else:
            out_ra_dec = ra_dec.replace(blank, underscore)
#         print('output ra_dec ', out_ra_dec)
          if use_conv:
            field_name = out_ra_dec +'_conv'
          else:
            field_name = out_ra_dec 

          print('i, field_name_i ', i, field_name)
          print('fits_file is ', field_name)
          location = las_raw[i].find('>')
          if location > -1:
            print('original LAS', las_raw[i])
            other_string = las_raw[i].replace('>',' ')
            print('las other string', other_string)
            las_raw[i] = other_string 
            print('replacement las',las_raw[i]) 
          las_HA = str(float(las_raw[i])*60 )  #convert heinz values to arcsec
          location = red_shift[i].find('s')
          if location > -1:
            other_string = red_shift[i].replace('s' , ' ')
            red_shift[i] = other_string 
            print('replacement z',red_shift[i]) 

#add .fits to name
          location = field_name.find('.fits')
          if location < 0:
            object_name = field_name + '.fits'
          print('looking for ', object_name)
          if os.path.isfile(object_name):
            print ("File exists")
          else:
            print ("File does not exist")
            continue
          location = object_name.find('.fits')
          analyze_image(field_name,freq,red_shift[i],spec_index[i],las_HA,use_b_mask,do_subt, threshold_value, noise, use_multi)


def main( argv ):
   parser = OptionParser(usage = '%prog [options] ')
   parser.add_option('-f', '--file', dest = 'filename', help = 'Filename with radio source names, positions, redshift etc (default = None)', default = None)
   parser.add_option('-m', '--use_m', dest = 'use_mask', help = 'use mask (default = T)', default = True)
   parser.add_option('-u', '--use_multi', dest = 'use_multi', help = 'use multi polygon (default = T)', default = True)
   parser.add_option('-c','--use_conv', dest = 'use_conv', help = 'use convolved image (default = F)', default = False)
   parser.add_option('-s','--subt', dest = 'do_subt', help = 'subtract central pixel flux density for equipartition (default = T)', default = True)
   parser.add_option('-t', '--threshold', dest = 'threshold', help = 'Threshold value forsoure detection in units of noise (default = 6)', default = 6)
   parser.add_option('-n', '--noise', dest = 'noise', help = 'noise specification in mJy, where noise cannot be found from image (default = 0)', default = 0)
   (options,args) = parser.parse_args()
   print('options', options)
   filename = options.filename
   use_conv = options.use_conv
   do_subt = options.do_subt
   print('do_subt', do_subt)
   use_mask = options.use_mask
   use_multi = options.use_multi
   noise = float(options.noise) / 1000.0
   signal_flux = float(options.threshold)
   if use_conv != False:
     use_conv = True
   if use_mask != True:
     use_mask = False
   if use_multi != True:
     use_multi = False
   if do_subt != True:
     do_subt = False

   start_time = timeit.default_timer()
# nominally use a .csv file such as '3C236.csv' as input foe argv[1]
  # argv[1] = name of pipeline input file with information such as frequency, positions, redshifts
  # argv[2] T = skip the mask making process 
  # argv[3] T = make mask usinge breizorro moise, F = make mask polygon manually
  # argv[4] T = use a convolved radio image
  # argv[5] threshold value for signal derection ( = factor by which to 
  #         multiply breizorro noise)
   process_images(filename, use_mask, use_conv, use_multi, do_subt,signal_flux, noise)

   print('get_galaxy_parameters: finished \n')
   elapsed = timeit.default_timer() - start_time
   print("Run Time:",elapsed,"seconds")
#=============================
# argv[1]  incoming positions file 
if __name__ == "__main__":
  main(sys.argv)
