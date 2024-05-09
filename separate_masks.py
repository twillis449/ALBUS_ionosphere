#!/usr/bin/env python

# A script that examines an image and generates polygons (== contours) at
# locations where the signal in the image is above a specified level.
# It outputs the contours for analysis in later acripts

import numpy as np
import matplotlib.pyplot as plt

from check_array import check_array, update_dimensions
from astropy.io import fits
from astropy.wcs import WCS
from optparse import OptionParser
from breizorro_extract import make_noise_map
from generate_morphology_image import make_morphology_image
from modify_masks import modify_masks
from beam_to_pixels import calculate_area
from process_polygon_data import *
from optparse import OptionParser
import generate_separate_masks as gen_p
import math
import os
import sys


def main( argv ):
   parser = OptionParser(usage = '%prog [options] ')
   parser.add_option('-f', '--file', dest = 'filename', help = 'top hat mask file)', default = None)
   (options,args) = parser.parse_args()
   filename = options.filename
   hdu_list = fits.open(filename)
#   print ('info',hdu_list.info())
   hdu = hdu_list[0]
# we may want to add some 'compact' features back into the diffuse image ...
# get locations of the features we want to add to the diffuse image
# with the polygon selection tool - to obtaim mask m_c
   print('calling generate_separate_masks for', filename)
   do_batch = False
   if not do_batch:
      polygon_gen = gen_p.make_separate_masks(hdu,filename)
      polygons = polygon_gen.out_data
      coords = polygons['coords']
      print('coords', coords)
      if len(coords) > 0:
        end_point = filename.find('.fits')
        if end_point > -1:
          filename = filename[:end_point]
        print('calling modify_masks with filename', filename)
        modify_masks(filename, polygons)  # gives a modified image o* = o_d + m_c * o_c
        return

if __name__ == '__main__':
    main(sys.argv)

