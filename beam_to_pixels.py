#!/usr/bin/env python

# FWHM = 2.355 σ = 2* sqrt(2*ln(2))
# see https://en.wikipedia.org/wiki/Full_width_at_half_maximum   and
# https://www.eaobservatory.org/jcmt/faq/how-can-i-convert-from-mjybeam-to-mjy/
# or https://www.eaobservatory.org/jcmt/?s=beam+to+pixels
# for a description of what's going on

# This script derives the beam area response in pixels required to normalize the
# summed signal from an image area in pixels i.e. you divide the summed
# signal by the beam area

import numpy as np
import math
import sys

def calculate_area(fwhm_arcsec, fwhm_min,pixel = 2):
# calculation of beam area in pixel sizes, where default pixel size is 2 arcsec
#fwhm_arcsec = fwhm_arcsec *2
# print('FWHM (arcsec)', fwhm_arcsec)
# print('FWHM_min (arcsec)', fwhm_min)
# print('pixel width (arcsec)', pixel)

  fwhm_arcsec = float(fwhm_arcsec)
  FWHM_r = math.radians(fwhm_arcsec /3600) 
  fwhm_min_arcsec = float(fwhm_min)
  FWHM_min_r = math.radians(fwhm_min_arcsec /3600) 
# following line implicitly converts FWHM to sigma and does the squaring  
  area = (math.pi * FWHM_r * FWHM_min_r) / (4.0 * np.log(2))
# print ('method 1 initial beam area - stradian squared', area)
  pixel = float(pixel)

# first method

  delta = math.radians(pixel / 3600)
  area_pixels = area / (delta * delta)
# print ('method 1 beam area in pixels', area_pixels)

############
# second method

  convert = 2.0 * math.sqrt(2.0 * np.log(2))
  sigma =  fwhm_arcsec/ convert
  sigma_min =  fwhm_min_arcsec/ convert
  beam_area = 2 * math.pi * sigma * sigma_min
# print ('method 2 beam area - arcsec squared', beam_area)
  num_pixels_beam = beam_area / (pixel * pixel)
# print ('method 2 beam area in pixels', num_pixels_beam)
  return num_pixels_beam

def main( argv ):
# pixel = width of pixel in arcsec, default is 2 arcsec
  beam = argv[1]   # FWHM of beam major axis in arcsec
  beam_min = argv[2]   # FWHM of beam minor axis in arcsec
  if len(argv) >= 4:
     pixel = argv[3]
     num_pixels_beam = calculate_area(beam, beam_min,pixel)
  else:
     num_pixels_beam = calculate_area(beam)
  print ('beam area in pixels', num_pixels_beam)
  

if __name__ == '__main__':
    main(sys.argv)

