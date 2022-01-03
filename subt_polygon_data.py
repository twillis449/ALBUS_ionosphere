#!/usr/bin/env python

# a script to subtract specified areas from a radio_cutout field
from astropy.io import fits
from check_array import check_array
from process_polygon_data import *

import os.path
import numpy as np
import math
import sys


def analyze_image(filename, json_file, use_dilation):
# get input pounts interior to polygons
    if use_dilation =='T':
      print('using dilated images')
      use_dilated = True
    else:
      use_dilated = False
    points =  json_file
    print('os.path.isfile(points)', os.path.isfile(points))
    interior_points = []
    if os.path.isfile(points):
        print ("File exists")
        polygon_list, coords = process_json_file(points)
        print('******** coords', coords)
        try:
          interior_points = get_interior_locations(polygon_list)
#         print('interior points', interior_points)
        except:
          interior_points = []
    else:
      print('Json file not found - so no processing to be done. Exiting')
      return

    location =  filename.find('.fits')
    fits_file =  filename
    filename = filename[:location]
#   fits_file_subt = filename[:location] + '-dilated.fits'
    fits_file_subt = filename[:location] +'.filtered_data.fits'
    if use_dilated:
      fits_file_out = filename[:location] + '_Final-dilated.fits'
    else:
      fits_file_out = filename[:location] + '_Final-eroded.fits'
    print('subt_polygon_data: processing input fits image file ', fits_file,' \n')
    # Download the polygon points
    hdu_list = fits.open(fits_file)
    hdu = hdu_list[0]
    data = check_array(hdu.data)
    hdu_list_subt = fits.open(fits_file_subt)
    hdu = hdu_list_subt[0]
    data_subt = check_array(hdu.data)
    if len(interior_points) > 0:
      for i in range(len(interior_points)):
        result = interior_points[i]
        for j in range(len(result[1])):
           x,y = result[1][j]
           data[y,x] = data[y,x] - data_subt[y,x]
#     hdu.data = np.where(data>0.0,data,0.0)
      hdu.data = data
      nans = np.isnan(hdu.data)
      hdu.data[nans] = 0.0
      hdu.header['DATAMAX'] =  hdu.data.max()
      hdu.header['DATAMIN'] =  hdu.data.min()
      hdu.writeto(fits_file_out, overwrite=True)
    else:
      print('subt_polygon_data: no interior points found')

def main( argv ):
  filename = argv[1]
  json_file = argv[2]
  use_dilated_image = argv[3] 
  analyze_image(filename, json_file, use_dilated_image)

if __name__ == '__main__':
    main(sys.argv)

