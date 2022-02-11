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


def make_simulated_noise(fits_file,std_dev):
    hdu_list = fits.open(fits_file)
    hdu = hdu_list[0]
    conv_factor = 1
    use_fft = 'T'
    use_fft = 'F'
    shape = hdu.data.shape
    mean = 0.0   # some constant
    print('std dev is ', std_dev)
    if std_dev > 0.0:
      std_dev = std_dev * 6.0
      noise_image = np.random.normal(mean, std_dev, shape)
      outfile = 'simulated_radio_noise.fits'
      if os.path.isfile(outfile):
          os.remove(outfile)
      hdu.data = noise_image 
      hdu.writeto(outfile, overwrite=True)
      convolve_image(outfile, conv_factor, use_fft)
    else:
      print('no noise specified so nothing returned')
      return

def combine_images(filename, json_polygons, original_noise=0.0):
# get input pounts interior to polygons
    try:
      polygon_list, coords = process_json_file(json_polygons)
      num_polygons = len(polygon_list)
      print('**************number of polygons', num_polygons)
    except:
      num_polygons = 0

    
    if num_polygons > 0:
      diffuse_fits_file = filename + '_diffuse_structure_dilated.fits'
      compact_fits_file = filename + '_compact_structure_dilated.fits'
      print('original compact image data  in file ', compact_fits_file)
      print('original diffuse image data  in file ', diffuse_fits_file)

      hdu_list = fits.open(diffuse_fits_file)
      hdu = hdu_list[0]
      incoming_dimensions = hdu.header['NAXIS']
      diffuse_data = check_array(hdu.data) 
      hdu_list = fits.open(compact_fits_file)
      hdu = hdu_list[0]
      compact_data = check_array(hdu.data)

      img = np.zeros(compact_data.shape, dtype=np.float)
      print('creating addition mask')
      for i in range(len(polygon_list)):
         result = polygon_list[i]
         x, y = result.exterior.coords.xy
         rr, cc = skimage_polygon(x,y, compact_data.shape)
         img[rr, cc] = 1
      compact_data_masked = compact_data * img 
      diffuse_data = diffuse_data + compact_data_masked 


      diffuse_data = update_dimensions(diffuse_data,incoming_dimensions)
      hdu.data = diffuse_data
      hdu.header['DATAMIN'] = hdu.data.min()
      hdu.header['DATAMAX'] = hdu.data.max()

      fits_file_out = filename + '_final_image.fits'
      print('sending output to ', fits_file_out)
      hdu.writeto(fits_file_out, overwrite=True)
    else:
# just link the diffuse file to final processed file
      diffuse_fits_file = filename + '_diffuse_structure_dilated.fits'
      fits_file_out = filename + '_final_processed_image.fits'
      print('making a symbolic link')
      if os.path.isfile(fits_file_out):
        os.remove(fits_file_out)
      os.symlink(diffuse_fits_file , fits_file_out)

# following is not worth the effort
#   make_simulated_noise(diffuse_fits_file, std_dev=original_noise)


def main( argv ):
  print('combine_images: doing image addition')
  filename = argv[1]
  json_file = argv[3]
  noise = argv[3]
  combine_images(filename, json_polygons, original_noise=0.0)

if __name__ == '__main__':
    main(sys.argv)

