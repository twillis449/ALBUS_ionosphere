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
from check_array import check_array
from breizorro_extract import make_noise_map
from read_input_table import  process_input_file
from calculate_source_parms import analyze_image


def process_images(filename, skip_generating_mask, use_breizorro_mask_l, use_conv_l, threshold_value):
        print('processing file ', filename)
        if use_breizorro_mask_l =='T':
          use_b_mask = True
          print('using mask')
        else:
          use_b_mask = False
        if use_conv_l =='T':
          print('using convolved_images')
          use_conv = True
        else:
          use_conv = False
        if skip_generating_mask == 'T':
          skip_generating_mask = True
          print('skipping mask')
        else:
          skip_generating_mask = False
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
          if not skip_generating_mask:  
            if not use_b_mask:
              print('generating manual mask')
# use breizorro to get noise
              info_1 = object_name[:location] + '.fits'
              cmd = 'generate_manual_polygon.py '  + info_1
              print('processing cmd', cmd)
              returned_value = subprocess.call(cmd, shell=True)  # returns the exit code in unix
              info_1 = object_name[:location] 
              cmd = 'mv selected_polygons.png ' + info_1 + '.simple_mask.png'
              print('processing cmd ', cmd)
              returned_value = subprocess.call(cmd, shell=True)  # returns the exit code in unix
          analyze_image(field_name,freq,red_shift[i],spec_index[i],las_HA,use_breizorro_mask_l,threshold_value)



def main( argv ):
  start_time = timeit.default_timer()
# nominally use a .csv file such as '3C236.csv' as input foe argv[1]
  # argv[1] = name of pipeline input file with information such as frequency, positions, redshifts
  # argv[2] T = skip the mask making process 
  # argv[3] T = make mask usinge breizorro moise, F = make mask polygon manually
  # argv[4] T = use a convolved radio image
  # argv[5] threshold value for signal derection ( = factor by which to 
  #         multiply breizorro noise)
  process_images(argv[1], argv[2], argv[3], argv[4], argv[5])

  print('get_galaxy_parameters: finished \n')
  elapsed = timeit.default_timer() - start_time
  print("Run Time:",elapsed,"seconds")
#=============================
# argv[1]  incoming positions file 
if __name__ == "__main__":
  main(sys.argv)
