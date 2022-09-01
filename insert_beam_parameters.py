#!/usr/bin/env python

# a simple script to insert aperture synthesis beam parameters into a fits image
# which does not have them.

from astropy.io import fits
import sys

def insert_beam_parameters(argv):
# Load the image for which beam parameters ar to be inserted
  hdu_list = fits.open(argv[1])
  hdu = hdu_list[0]

# get the pixel size - square images assumed
  hdu.header['BMAJ']  = float(argv[2])/3600.0
  hdu.header['BMIN']  = float(argv[3])/3600.0
  hdu.header['BPA']  = float(argv[4])

  outfile = argv[1]
# overwrite the imput file
  hdu.writeto(outfile, overwrite=True)

def main( argv ):
# argv[1] = incoming fits image
# argv[2] = bmaj
# argv[3] = bmin
# argv[4] = bpa
  insert_beam_parameters(argv)

if __name__ == '__main__':
    main(sys.argv)

