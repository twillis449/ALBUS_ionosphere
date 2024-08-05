#!/usr/bin/env python

# a script to subtract specified areas from a radio_cutout field
from astropy.io import fits
from check_array import check_array, update_dimensions
from process_polygon_data import *
from skimage.draw import polygon as skimage_polygon
from conv_image import convolve_image

import os
import numpy as np
import math
import sys
import shutil


def modify_masks(filename, json_polygons):
# get input pounts interior to polygons
    try:
      polygon_list, coords = process_json_file(json_polygons)
      num_polygons = len(polygon_list)
      print('**************number of polygons', num_polygons)
    except:
      num_polygons = 0

    
    if num_polygons > 0:
      original_mask_file = filename +'-white_tophat.mask.fits'

      hdu_list = fits.open(original_mask_file)
      hdu = hdu_list[0]
      diffuse_data = check_array(hdu.data) 
      incoming_dimensions = hdu.header['NAXIS']
      compact_data = np.zeros(diffuse_data.shape, dtype=np.float)
      img = np.zeros(compact_data.shape, dtype=np.float)
      mask_update = np.ones(compact_data.shape, dtype=np.float)
      print('creating separation mask')
      for i in range(len(polygon_list)):
         result = polygon_list[i]
         x, y = result.exterior.coords.xy
         rr, cc = skimage_polygon(x,y, compact_data.shape)
         img[rr, cc] = 1
         mask_update[rr, cc] = 0
      img = img.astype('float32')
      mask_update = mask_update.astype('float32')
      compact_data_masked = img 
      diffuse_data = diffuse_data - compact_data_masked 

# update compact data image as some compact data moved to diffuse image
      compact_data = compact_data_masked
      compact_data = update_dimensions(compact_data,incoming_dimensions)
      hdu.data = compact_data
      hdu.header['DATAMIN'] = hdu.data.min()
      hdu.header['DATAMAX'] = hdu.data.max()

      diffuse_data = update_dimensions(diffuse_data,incoming_dimensions)
      hdu.data = diffuse_data
      hdu.header['DATAMIN'] = hdu.data.min()
      hdu.header['DATAMAX'] = hdu.data.max()

      fits_file_out = original_mask_file
      print('sending final diffuse data output to ', fits_file_out)
      hdu.writeto(fits_file_out, overwrite=True)
      
def main( argv ):
  filename = argv[1]
  json_file = argv[3]
  noise = argv[3]
  combine_images(filename, json_polygons)

if __name__ == '__main__':
    main(sys.argv)

