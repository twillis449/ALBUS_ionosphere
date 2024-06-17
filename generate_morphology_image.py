#!/usr/bin/env python

# This script performs morphological erosion and / or dilation on
# an imput image
# see https://en.wikipedia.org/wiki/Mathematical_morphology

import sys
import numpy as np
import astropy.visualization as vis
import scipy.special
from astropy.io import fits
from skimage.morphology import erosion, dilation, opening, white_tophat
from skimage.morphology import disk, rectangle
from check_array import check_array
from astropy.io import fits
from astropy.wcs import WCS


def create_circular_mask(h, w, center=None, radius=None):

    if center is None: # use the middle of the image
        center = (int(w/2), int(h/2))
    if radius is None: # use the smallest distance between the center and image walls
        radius = min(center[0], center[1], w-center[0], h-center[1])

    Y, X = np.ogrid[:h, :w]
    dist_from_center = np.sqrt((X - center[0])**2 + (Y-center[1])**2)

    mask = dist_from_center <= radius
    return mask

def make_morphology_image(filename, filter_size, filter_type, double_erode):

    print('*** double_erode =', double_erode)
    # Download the image
    # Load the image and the WCS
#   file_name = filename+'.fits'
    file_name = filename
    hdu_list = fits.open(file_name)
    hdu = hdu_list[0]
    data = check_array(hdu.data)
    data = np.nan_to_num(data)
    input_data = data
    shape = data.shape
    size_spec = int(filter_size)
    if filter_type == 'R':
      print('using rectangle for structure element')
      structure_element = rectangle(size_spec,size_spec)
    else:
      print('using disk for structure element')
#     structure_element = create_circular_mask(size_spec, size_spec, center=None, radius=None)
      structure_element = disk(size_spec)
    print('structure_element', structure_element)
    if double_erode:
      print('double erode is ',  double_erode)
      if filter_type == 'R':
        double_structure_element = rectangle(2*size_spec, 2*size_spec)
      else:
        double_structure_element = disk(2 * size_spec)
      print('double structure_element', double_structure_element)
# see wikipedia article referenced above
# this will be faster as we only have to do one erode for the entire image
#     double_structure_element = dilation(structure_element,structure_element)
# double erosion equites to erosion with structure element having 4x the area of
# single erosion element
      eroded = erosion(data, double_structure_element)
    else:
# doing one erosion gets those wierd structures in the Rudnick paper
      eroded = erosion(data, structure_element)
# doing a second erosion helps get rid of stuff missed in a first erosion
    filter_image = dilation(eroded, structure_element)
    hdu.data = filter_image
    hdu.header['DATAMAX'] =  filter_image.max()
    hdu.header['DATAMIN'] =  filter_image.min()
    loc = filename.find('.fits')
#   don't bother to write this image out at the moment
#   outfile = filename[:loc] + '-dilated.fits'
#   hdu.writeto(outfile, overwrite=True)
    return filter_image

def main( argv ):
  filename = argv[1]       # image test file with model point sources
  filter_size = argv[2]    # size of structure element
                           # = radius for 'D' element, width for 'R' element
  filter_type = argv[3]    # 'D' or 'R'
  make_morphology_image(filename, filter_size, filter_type)

if __name__ == '__main__':
    main(sys.argv)
