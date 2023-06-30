#!/usr/bin/env python

# This program selects individual planes from a multiplane/ cube FITS file

import math
import sys
import numpy as np
import subprocess
from datetime import date
from astropy.io import fits


def extract_plane(data,plane_num):
  orig_shape = data.shape
# for i in range (orig_shape[1]):
#   array = data[0,i,:,:]
#   print ('array range', array.min(),array.max())
# print('incoming data shape',orig_shape)
  if len(orig_shape) == 2:
        new_array =  data
  elif len(orig_shape) == 3:
        data = np.array(data[plane_num, :, :])
        new_array =  np.zeros((1,orig_shape[1],orig_shape[2]), dtype=np.float32)
        new_array[0,:,:] = data
  else:
        data = np.array(data[0, plane_num, :, :])
#       print ('selected array range', data.min(),data.max())
#       print ('selected array shape', data.shape)
        new_array =  np.zeros((1,1,orig_shape[2],orig_shape[3]), dtype=np.float32)
        new_array[0,0,:,:] = data

  return data, new_array


def select_plane(fits_input_image, plane_number=1):
# do we need to unzip?
  location = fits_input_image.find('.gz')
  if location > -1:
    cmd = 'gunzip ' + fits_input_image
    print('processing ', cmd)
    returned_value = subprocess.call(cmd, shell=True)  # returns the exit code in unix
    fits_input_image = fits_input_image[:location]

  hdu_list = fits.open(fits_input_image)
  hdu = hdu_list[0]
# for zero-based python
  plane_num = int(plane_number) - 1
  original_array = hdu.data
# print('original data array shape', original_array.shape)
  img_source, new_array  = extract_plane(original_array,plane_num)
# print('img_source array shape', img_source.shape)
# print('new array shape', new_array.shape)
  end_point = fits_input_image.find('.fits')
  hdu.data = new_array 
  print('final range', hdu.data.max(), hdu.data.min())
  hdu.header['NAXIS3'] =  1
  hdu.header['DATAMIN'] = hdu.data.min()
  hdu.header['DATAMAX'] = hdu.data.max()

  today = date.today()
  d4 = today.strftime("%b-%d-%Y")
  hdu.header['HISTORY'] = d4 + ' selected plane number ' + str(plane_number)

  outfile = fits_input_image[:end_point]+ '_plane_' + str(plane_number) + '.fits'
  print('writing out fits file: ', outfile)
  hdu.writeto(outfile, overwrite=True)
  
def main( argv ):
# argv[1] = incoming fits image
# argv[2] = plane number to select  (default is 1)
# for example, run as 'python select_plane.py example_file.fits 1'
  if len(argv) == 3:
    print('selecting plane ' + argv[2])
    select_plane(argv[1], int(argv[2]))
  else:
    print('selecting plane from ', argv[1])
    print('using default pnane number of 1')
    select_plane(argv[1])

if __name__ == '__main__':
    main(sys.argv)

