#!/usr/bin/env python

# a script to simple math operations (add, subtract, multiply. divide) on two 
# images with same parameters - pixels, pxel separation etc
# extract specified areas from a radio_cutout field
from astropy.io import fits
from check_array import check_array
from process_polygon_data import *

import os.path
import numpy as np
import math
from optparse import OptionParser
import sys


def analyze_image(filename, second_file,  use_math):
    print('image_math operator = ', use_math)
    hdu_list = fits.open(filename)
    hdu = hdu_list[0]
    data = hdu.data
    print('image_math first input array min and max',data.min(), data.max())
    try:
      hdu_list_mask = fits.open(second_file)
      hdu = hdu_list_mask[0]
      data1 = hdu.data
      print('image_math second array min and max',data1.min(), data1.max())
    except:
      data1 = float(second_file)
      print('image math second parameter has numerical value', data1)
    if use_math == 'a':
      hdu.data =  data + data1
      out_file = 'image_math_add.fits'
    if use_math == 's':
      hdu.data =  data - data1
      out_file = 'image_math_subt.fits'
    if use_math == 'm':
      hdu.data =  data * data1
      out_file = 'image_math_mult.fits'
    if use_math == 'd':
      hdu.data =  data / data1
      out_file = 'image_math_div.fits'
    hdu.header['DATAMAX'] = hdu.data.max()
    hdu.header['DATAMIN'] = hdu.data.min()
    hdu.writeto(out_file, overwrite=True)

def main( argv ):
   parser = OptionParser(usage = '%prog [options] ')
   parser.add_option('-f', '--file', dest = 'filename', help = 'Filename with radio source names, positions, redshift etc (default = None)', default = None)
   parser.add_option('-s', '--second_file', dest = 'filename2', help = 'Filename with radio source names, positions, redshift etc OR numerical value (default = None)', default = None)
   a = 'a'
   parser.add_option('-m', '--use_m', dest = 'use_math', help = 'use math (default = a)', default = a)
   (options,args) = parser.parse_args()
   print('image_math options', options)
   filename = options.filename
   filename2 = options.filename2
   use_math = options.use_math
   analyze_image(filename, filename2, use_math)

if __name__ == '__main__':
    main(sys.argv)

