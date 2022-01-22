#!/usr/bin/env python

# a script to subtract specified areas from a radio_cutout field
from astropy.io import fits
from check_array import check_array
from process_polygon_data import *

import os.path
import numpy as np
import math
import sys


def analyze_image(filename, json_file, offset_flux,use_dilation):
# get input pounts interior to polygons
    if use_dilation =='T':
      print('using dilated images')
      use_dilated = True
    else:
      use_dilated = False
    offset_flux = float(offset_flux) / 1000.00
    print('subt_polygon_data: offset flux (Jy):', offset_flux)
    print('subt_polygon_data: use_dilation:', use_dilation)
    points =  json_file
    print('os.path.isfile(points)', os.path.isfile(points))
    interior_points = []

    # Download the polygon points
    if os.path.isfile(points):
        print ("File exists")
        polygon_list, coords = process_json_file(points)
        num_polygons = len(polygon_list)
        interior_points = get_interior_locations(polygon_list)
        polygons_dict = {}
        for i in range(len(interior_points)):
          result = interior_points[i]
          polygon_number = result[0]
          print('giving polygons_dict key', polygon_number)
          polygons_dict[polygon_number] = result[1]
    else:
      print('Json file not found - so no processing to be done. Exiting')
      return

    location =  filename.find('.fits')
    fits_file =  filename
    filename = filename[:location]
    fits_file_subt = filename[:location] +'.filtered_data.fits'
    if use_dilated:
      fits_file_out = filename[:location] + '_Final-image_using_selected_dilation.fits'
    else:
      fits_file_out = filename[:location] + '_Final-image_using_selected_erosion.fits'
    print('subt_polygon_data: processing input fits image file ', fits_file,' \n')
    print('opening orig_file', fits_file)
    hdu_list = fits.open(fits_file)
    hdu = hdu_list[0]
    data = check_array(hdu.data)
    print('raw input max', data.max())
    print('opening subt file', fits_file_subt)
    hdu_list_subt = fits.open(fits_file_subt)
    hdu = hdu_list_subt[0]
    data_subt = check_array(hdu.data)
    for key in polygons_dict.keys():
       result = polygons_dict[key]
       x = result[0]
       y = result[1]
       if offset_flux > 0.0:
           data[x,y] = data[x,y] - data_subt[x,y] + offset_flux
       else:
           data[x,y] = data[x,y] - data_subt[x,y] 
    hdu.data = data
    nans = np.isnan(hdu.data)
    hdu.data[nans] = 0.0
    hdu.header['DATAMAX'] =  hdu.data.max()
    hdu.header['DATAMIN'] =  hdu.data.min()
    hdu.writeto(fits_file_out, overwrite=True)

def main( argv ):
  filename = argv[1]
  offset_flux = argv[2] # value in mJy to be omitted from source subtraction
  json_file = argv[3]
  use_dilated_image = argv[4] 
  analyze_image(filename, json_file, offset_flux, use_dilated_image)

if __name__ == '__main__':
    main(sys.argv)

