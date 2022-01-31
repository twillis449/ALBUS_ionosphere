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


def combine_images(filename, json_file, offset_flux,use_dilation):
# get input pounts interior to polygons
    if use_dilation =='T':
      print('using dilated images')
      use_dilated = True
    else:
      use_dilated = False
    offset_flux = float(offset_flux) / 1000.00
    print('combine_images: offset flux (Jy):', offset_flux)
    print('combine_images: use_dilation:', use_dilation)
    points =  json_file
    print('os.path.isfile(points)', os.path.isfile(points))

    json_file_exists = False
    # Download the polygon points
    if os.path.isfile(points):
        print ("File exists")
        polygon_list, coords = process_json_file(points)
        num_polygons = len(polygon_list)
        print('num_polygons', num_polygons)
        json_file_exists = True
    else:
      print('Json file not found - so no processing to be done.')
      print('Copying diffuse image to final image and then exiting')
    print('json file status',  json_file_exists )
    if json_file_exists:
      diffuse_fits_file = filename + '_diffuse_structure_dilated.fits'
      compact_fits_file = filename + '_compact_structure_dilated.fits'
      print('original compact image data  in file ', compact_fits_file)
      print('original diffuse image data  in file ', diffuse_fits_file)

      hdu_list = fits.open(diffuse_fits_file)
      hdu = hdu_list[0]
      diffuse_data = check_array(hdu.data) 

      hdu_list = fits.open(compact_fits_file)
      hdu = hdu_list[0]
      compact_data = check_array(hdu.data)

      img = np.zeros(compact_data.shape, dtype=np.float)
      print('creating addition mask')
      for i in range(len(polygon_list)):
         result = polygon_list[i]
         x, y = result.exterior.coords.xy
# switch x, y as retrieved x, y are matplotlib display coordinates
         temp = x
         x = y
         y = temp
         rr, cc = polygon(x,y, compact_data.shape)
         img[rr, cc] = 1
      if offset_flux > 0.0:
        diffuse_data = diffuse_data + (compact_data - offset_flux) * img 
      else:
        compact_data_masked = compact_data * img
        diffuse_data = diffuse_data + compact_data_masked

      hdu.data = diffuse_data
      fits_file_out = filename + '_final_processed_image.fits'
      print('sending output to ', fits_file_out)
      hdu.writeto(fits_file_out, overwrite=True)
    else:
# just copy the diffuse file to final processed file
      diffuse_fits_file = filename + '_diffuse_structure_dilated.fits'
      hdu_list = fits.open(diffuse_fits_file)
      hdu = hdu_list[0]
      fits_file_out = filename + '_final_processed_image.fits'
      hdu.writeto(fits_file_out, overwrite=True)

def main( argv ):
  print('combine_images: doing image addition')
  filename = argv[1]
  offset_flux = argv[2] # value in mJy to be omitted from source subtraction
  json_file = argv[3]
  use_dilated_image = argv[4] 
  combine_images(filename, json_file, offset_flux, use_dilated_image)

if __name__ == '__main__':
    main(sys.argv)

