#!/usr/bin/env python

# This program is adapted from the conv_image script and adjusts the FITS
# reference pixels and coordinates of a cutout image so that the reference
# coordinates and pixels are at the centre of the image.

import math
import sys
import numpy as np
from datetime import date
from astropy.wcs import WCS
from astropy.io import fits
from check_array import check_array


def adjust_reference_position(fits_input_image):
# Load the image to be convolved
  hdu_list = fits.open(fits_input_image)
  hdu = hdu_list[0]
  incoming_dimensions = hdu.header['NAXIS']

# get the pixel size - square images assumed
  pixel_size = hdu.header['CDELT2'] * 3600

# get reference position
  w = WCS(hdu.header)
  w =w.celestial
# determine ra, and dec of new reference pixel, which will be the midpoint
# of output image    
  cen_pos_x = hdu.header['NAXIS1'] // 2
  cen_pos_y = hdu.header['NAXIS2'] // 2
  lon, lat = w.all_pix2world(cen_pos_x,cen_pos_y,0)

  shape = hdu.data.shape
  shape_x = shape[0] // 2
  shape_y = shape[1] // 2
# need to flip array references vs what's seen on the display
  hdu.header['CRPIX1'] = int(shape_y)
  hdu.header['CRPIX2'] = int(shape_x)
# no idea why I have to explicity wrap a float inside a float here
  hdu.header['CRVAL1'] = float(lon) 
  hdu.header['CRVAL2'] = float(lat)
  hdu.writeto(fits_input_image, overwrite=True)

def main( argv ):
# argv[1] = incoming fits image
  adjust_reference_position(argv[1])

if __name__ == '__main__':
    main(sys.argv)

