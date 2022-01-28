#!/usr/bin/env python

# a script to subtract specified areas from a radio_cutout field
from astropy.io import fits
from check_array import check_array
from process_polygon_data import *
from skimage.draw import  polygon

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

    # Download the polygon points
    if os.path.isfile(points):
        print ("File exists")
        polygon_list, coords = process_json_file(points)
        num_polygons = len(polygon_list)
    else:
      print('Json file not found - so no processing to be done. Exiting')
      return

    location =  filename.find('.fits')
    fits_file =  filename
    print('original image data  in file ', fits_file)
    filename = filename[:location]
    print('subt_polygon_data: processing input fits image file ', fits_file,' \n')
    print('opening orig_file', fits_file)
    hdu_list = fits.open(fits_file)
    hdu = hdu_list[0]
    data = check_array(hdu.data)
    print('raw input max', data.max())
    img = np.zeros(data.shape, dtype=np.float)
    for i in range(len(polygon_list)):
       result = polygon_list[i]
       x, y = result.exterior.coords.xy
# switch x, y as retrieved x, y are matplotlib display coordinates
       temp = x
       x = y
       y = temp
       rr, cc = polygon(x,y, data.shape)
       img[rr, cc] = 1
       if offset_flux > 0.0:
           data = data - (data - offset_flux) * img 
       else:
           data = data - data * img

    hdu.data = data
    nans = np.isnan(hdu.data)
    hdu.data[nans] = 0.0
    hdu.header['DATAMAX'] =  hdu.data.max()
    hdu.header['DATAMIN'] =  hdu.data.min()
    fits_file_out = filename[:location] + '_image_with_selected_emission_removed.fits'
    print('sending output to ', fits_file_out)
    hdu.writeto(fits_file_out, overwrite=True)

    hdu.data = img
    nans = np.isnan(hdu.data)
    hdu.data[nans] = 0.0
    hdu.header['DATAMAX'] =  hdu.data.max()
    hdu.header['DATAMIN'] =  hdu.data.min()
    hdu.writeto('scikit_mask.fits', overwrite=True)

def main( argv ):
  print('subt_polygon_data: doing data subtraction')
  filename = argv[1]
  offset_flux = argv[2] # value in mJy to be omitted from source subtraction
  json_file = argv[3]
  use_dilated_image = argv[4] 
  analyze_image(filename, json_file, offset_flux, use_dilated_image)

if __name__ == '__main__':
    main(sys.argv)

