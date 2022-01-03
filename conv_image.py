#!/usr/bin/env python

# This program convolves/smooths an image from a high resolution to a lower
# resolution. This program works similar to a radio astronomy program where
# we do convolution by cutting out the higher frequency components of the
# UV plane, so point soutces appear broader but retain the same 'peak' 
# response. It uses the astropy convolution module

import math
import sys
import numpy as np
import matplotlib.pyplot as plt
from datetime import date
from astropy.wcs import WCS
from astropy.io import fits
from astropy.convolution import convolve, convolve_fft,Gaussian2DKernel, Tophat2DKernel
from astropy.modeling.models import Gaussian2D
import astropy.visualization as vis
from check_array import check_array


def convolve_image(fits_input_image, conv_factor, use_fft='F'):
# Load the image to be convolved
  hdu_list = fits.open(fits_input_image)
  hdu = hdu_list[0]

# get the pixel size - square images assumed
  pixel_size = hdu.header['CDELT2'] * 3600
  print ('pixel size', pixel_size)
  try:
    bmaj_old = hdu.header['BMAJ'] *3600
    bmin_old = hdu.header['BMIN'] *3600
    bpa_old =  hdu.header['BPA']
    print('input beam parameters', bmaj_old, bmin_old, bpa_old)
    bmaj = bmaj_old * conv_factor
    bmin = bmin_old * conv_factor
    bpa = bpa_old
    beam_size_gain = (bmaj * bmin) / (bmaj_old * bmin_old )
    print('beam size gain', beam_size_gain)
    x_conv = math.sqrt(bmaj*bmaj - bmaj_old * bmaj_old)
    y_conv = math.sqrt(bmin*bmin - bmin_old * bmin_old)
    print('x conv', x_conv)
    print('y conv', y_conv)
  except:
    print('Sorry - your input image must contain the FITS kewords BMAJ, BIN, and BPA')
    return



# need to adjust for new beam vs old beam size ratio
  x_stddev = x_conv / (2.355 *pixel_size) 
  y_stddev = y_conv / (2.355 *pixel_size) 
  print('conv x stddev', x_stddev)
  print('conv y stddev', y_stddev)
  theta = math.radians(bpa + 90.0) # theta from E-W line 
  kernel = Gaussian2DKernel(x_stddev, y_stddev, theta  )


  original_array = hdu.data
  print('original data array shape', original_array.shape)
  num_axes = len(hdu.data.shape)
# now convolved the image
  img_source = check_array(hdu.data)
# set NaNs to zero
  img_source = np.nan_to_num(img_source)
  print ('source initial max and min', img_source.max(), img_source.min())
  if use_fft == 'T':
    print('**** using fft')
    astropy_conv = convolve_fft(img_source, kernel, allow_huge=True)
  else:
    astropy_conv = convolve(img_source, kernel)
# apply gain factor
  astropy_conv = astropy_conv * beam_size_gain
# Now we plot te orginal and convolved image.  
#plt.figure(1, figsize=(12, 12)).clf()
  ax1 = plt.subplot(1,2,1, projection=WCS(hdu.header).celestial)
  interval = vis.PercentileInterval(99.9)
  vmin,vmax = interval.get_limits(img_source)
  norm = vis.ImageNormalize(vmin=vmin, vmax=vmax, stretch=vis.LogStretch(1000))
  im = ax1.imshow(img_source, cmap =plt.cm.gray_r, norm = norm, origin = 'lower')
  ax1.coords['ra'].set_axislabel('Right Ascension')
  ax1.coords['dec'].set_axislabel('Declination')
  ax1.set_title('Original Image')
  #plt.colorbar(im)
  
  ax4 = plt.subplot(1,2,2, projection=WCS(hdu.header).celestial)
  interval = vis.PercentileInterval(99.9)
  vmin,vmax = interval.get_limits(astropy_conv)
  norm = vis.ImageNormalize(vmin=vmin, vmax=vmax, stretch=vis.LogStretch(1000))
  im=ax4.imshow(astropy_conv, cmap =plt.cm.gray_r, norm = norm, origin = 'lower')
  ax4.coords['ra'].set_axislabel('Right Ascension')
  ax4.coords['dec'].set_axislabel('Declination')
  ax4.set_title('Convolved Image')
  ax4.set_xticklabels([])
  ax4.set_yticklabels([])

  end_point = fits_input_image.find('.fits')
  plt.suptitle (fits_input_image[:end_point]+ '_conv')
  outfile = fits_input_image[:end_point]+ '_conv.fits'
  if num_axes > 2:
    if num_axes == 3:
      original_array[0,:,:] = astropy_conv
    else:
      original_array[0,0,:,:] = astropy_conv
    hdu.data = original_array
  else:
    hdu.data = astropy_conv
  hdu.header['BMAJ']  = bmaj / 3600
  hdu.header['BMIN']  = bmin / 3600
  hdu.header['BPA']  = bpa
  hdu.header['DATAMAX'] =  astropy_conv.max()
  hdu.header['DATAMIN'] =  astropy_conv.min()

  today = date.today()
  d4 = today.strftime("%b-%d-%Y")
  hdu.header['HISTORY'] = d4 + ' convolved by a factor ' + str(conv_factor)

  hdu.writeto(outfile, overwrite=True)
  plt.savefig(fits_input_image[:end_point]+ '_conv.png')
  
# plt.show()

def main( argv ):
# argv[1] = incoming fits image
# argv[2] convolution factor
  print('convolving image ', argv[1])
  conv_factor = float(argv[2])
  try:
   use_fft = argv[3]
  except:
   use_fft = 'F'
  convolve_image(argv[1], conv_factor, use_fft)

if __name__ == '__main__':
    main(sys.argv)

