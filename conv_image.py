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
from astropy.convolution import convolve, convolve_fft,Gaussian2DKernel
import astropy.visualization as vis
from check_array import check_array, update_dimensions
from optparse import OptionParser
import timeit


def convolve_image(fits_input_image, conv_factor, use_fft=False, do_downsize = False):
# Load the image to be convolved
  print('loading input_image', fits_input_image)
  hdu_list = fits.open(fits_input_image)
  hdu = hdu_list[0]
  incoming_dimensions = hdu.header['NAXIS']

# get the pixel size - square images assumed
  pixel_size = hdu.header['CDELT2'] * 3600
  try:
    bmaj_old = hdu.header['BMAJ'] *3600
    bmin_old = hdu.header['BMIN'] *3600
    bpa_old =  hdu.header['BPA']
    bmaj = bmaj_old * conv_factor
    bmin = bmin_old * conv_factor
    bpa = bpa_old
    beam_size_gain = (bmaj * bmin) / (bmaj_old * bmin_old )
    if conv_factor > 1:
      x_conv = math.sqrt(bmaj*bmaj - bmaj_old * bmaj_old)
      y_conv = math.sqrt(bmin*bmin - bmin_old * bmin_old)
# NOTE: if conv_factor == 1, we still do a convolution,
# BUT the system will be convolving with respect to individual pixels,
# so can be used to 'smooth' simulated noise
    else:
      x_conv = bmaj
      y_conv = bmin

# get reference position
    w = WCS(hdu.header)
    w =w.celestial
    ref_ra = hdu.header['CRVAL1']
    ref_dec = hdu.header['CRVAL2']
# determine ra, and dec of new reference pixel, which will be the midpoint
# of output image    
    cen_pos_x = hdu.header['NAXIS1'] // 2
    cen_pos_y = hdu.header['NAXIS2'] // 2
    lon, lat = w.all_pix2world(cen_pos_x,cen_pos_y,0)

  except:
    print('Sorry - your input image must contain the FITS kewords BMAJ, BIN, and BPA')
    return



# first convert from FWHM to sigma for use with Gaussian kernel
# see https://en.wikipedia.org/wiki/Full_width_at_half_maximum
  convert = 2.0 * math.sqrt(2.0 * np.log(2))
  x_stddev = x_conv / (convert * pixel_size) 
  y_stddev = y_conv / (convert * pixel_size) 
  theta = math.radians(bpa + 90.0) # theta from E-W line 
  kernel = Gaussian2DKernel(x_stddev, y_stddev, theta )

  original_array = hdu.data
  num_axes = len(hdu.data.shape)
# now convolve the image
  img_source = check_array(hdu.data)
# set NaNs to zero
  img_source = np.nan_to_num(img_source)
  if use_fft:
    print('**** using FFT for convolution')
    astropy_conv = convolve_fft(img_source, kernel, allow_huge=True)
  else:
    print('**** using image plane convolution')
    astropy_conv = convolve(img_source, kernel)
# need to adjust for new beam vs old beam size ratio
  astropy_conv = astropy_conv * beam_size_gain
# Now we plot te orginal and convolved image.  
#plt.figure(1, figsize=(12, 12)).clf()
  ax1 = plt.subplot(1,2,1, projection=WCS(hdu.header).celestial)
  interval = vis.PercentileInterval(99.9)
  vmin,vmax = interval.get_limits(img_source)
  vmin = 0.0
  norm = vis.ImageNormalize(vmin=vmin, vmax=vmax, stretch=vis.LogStretch(1000))
  im = ax1.imshow(img_source, cmap =plt.cm.gray_r, norm = norm, origin = 'lower')
  ax1.coords['ra'].set_axislabel('Right Ascension')
  ax1.coords['dec'].set_axislabel('Declination')
  ax1.set_title('Original Image')
  #plt.colorbar(im)
  
  ax4 = plt.subplot(1,2,2, projection=WCS(hdu.header).celestial)
  interval = vis.PercentileInterval(99.9)
  vmin,vmax = interval.get_limits(astropy_conv)
  vmin = 0.0
  norm = vis.ImageNormalize(vmin=vmin, vmax=vmax, stretch=vis.LogStretch(1000))
  im=ax4.imshow(astropy_conv, cmap =plt.cm.gray_r, norm = norm, origin = 'lower')
  ax4.coords['ra'].set_axislabel('Right Ascension')
  ax4.coords['dec'].set_axislabel('Declination')
  ax4.set_title('Convolved Image')
  ax4.set_xticklabels([])
  ax4.set_yticklabels([])

  end_point = fits_input_image.find('.fits')
  if end_point > -1:
    plt.suptitle (fits_input_image[:end_point] + ' convolved')
  else:
    plt.suptitle (fits_input_image + ' convolved')
  shape = astropy_conv.shape
  conv_factor_int = int(conv_factor)
# see  https://moonbooks.org/Articles/How-to-downsampling-a-matrix-by-averaging-elements-nn-with-numpy-in-python-/
  if conv_factor_int > 1:
    if end_point > -1:
      outfile = fits_input_image[:end_point] + '_conv.fits'
    else:
      outfile = fits_input_image +  '_conv.fits'
    if do_downsize:
      print('shrinking convolved image')
      smaller_astropy_conv = astropy_conv[::conv_factor_int, ::conv_factor_int] 
    else:
      smaller_astropy_conv = astropy_conv
  else:
    outfile = fits_input_image
    smaller_astropy_conv = astropy_conv
  shape = smaller_astropy_conv.shape
  shape_x = shape[0] // 2
  shape_y = shape[1] // 2
# hdu.data = flush_fits(smaller_astropy_conv,hdu_list)
  output_image = update_dimensions(smaller_astropy_conv,incoming_dimensions)
  hdu.data = output_image
  hdu.header['BMAJ']  = bmaj / 3600
  hdu.header['BMIN']  = bmin / 3600
  hdu.header['BPA']  = bpa
  hdu.header['DATAMAX'] =  hdu.data.max()
  hdu.header['DATAMIN'] =  hdu.data.min()
  if do_downsize:
    hdu.header['CDELT1'] = hdu.header['CDELT1']  * conv_factor_int
    hdu.header['CDELT2'] = hdu.header['CDELT2']  * conv_factor_int
# need to flip array references vs what's seen on the display
  hdu.header['CRPIX1'] = int(shape_y)
  hdu.header['CRPIX2'] = int(shape_x)
# no idea why I have to explicity wrap a float inside a float here
  hdu.header['CRVAL1'] = float(lon) 
  hdu.header['CRVAL2'] = float(lat)
  hdu.header.set('CONVFACT', conv_factor, 'factor used to convolve input image')
  today = date.today()
  d4 = today.strftime("%b-%d-%Y")
  hdu.header['HISTORY'] = d4 + ' convolved by a factor ' + str(conv_factor)
  hdu.writeto(outfile, overwrite=True)


#We can examine the two images (this makes use of the wcsaxes package behind the scenes):



  plt.savefig(fits_input_image[:end_point]+ '_conv.png')
  
# plt.show()

def main( argv ):
   start_time = timeit.default_timer()
   parser = OptionParser(usage = '%prog [options] ')
   parser.add_option('-f', '--file', dest = 'filename', help = 'input FITS file radio image  (default = None)', default = None)
   parser.add_option('-c', '--conv', dest = 'conv_factor', help = 'convolution factor (default = 2)', default = 2)
   parser.add_option('--ft', '--fft', dest = 'use_fft', help = 'do convolution with fft (default = False)', default = False)
   parser.add_option('-d', '--downsize', dest = 'do_downsize', help = 'downsize image sampling interval (default = False)', default = False)
   (options,args) = parser.parse_args()
   print('conv_image options', options)
   filename = options.filename
   conv_factor = float(options.conv_factor)
   use_fft = options.use_fft
   do_downsize = options.do_downsize
   print('do_downsize', do_downsize)
   if use_fft != False:
     use_fft = True
   if do_downsize != False:
     do_downsize = True

   print('conv_image incoming image', filename)
   print('conv_image calling convolve with do_downsize', do_downsize)
   convolve_image(filename, conv_factor, use_fft,do_downsize)
   elapsed = timeit.default_timer() - start_time
   print("conv_image Run Time:",elapsed,"seconds")


if __name__ == '__main__':
    main(sys.argv)

