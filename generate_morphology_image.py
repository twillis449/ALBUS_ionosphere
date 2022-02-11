#!/usr/bin/env python

# This script performs morphological erosion and / or dilation on
# an imput image

import sys
import numpy as np
import astropy.visualization as vis
import scipy.special
from astropy.io import fits
from skimage.morphology import erosion, dilation, opening, closing, white_tophat
from skimage.morphology import black_tophat, skeletonize, convex_hull_image
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

def make_morphology_image(filename, filter_size, filter_type):

    # Download the image
    # Load the image and the WCS
    file_name = filename+'.fits'
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
# doing one erosion gets those wierd structures in the Rudnick paper
    eroded = erosion(data, structure_element)
# doing a second erosion helps get rid od stuff missed in a first erosion
    double_erode = True
    if double_erode:
      eroded = erosion(eroded, structure_element)
    dilated = dilation(eroded, structure_element)
    filter_image = dilated
    hdu.data = dilated
    hdu.header['DATAMAX'] =  dilated.max()
    hdu.header['DATAMIN'] =  dilated.min()
    outfile = filename + '_dilated.fits'
# don't bother writing this image to disk
#    hdu.writeto(outfile, overwrite=True)
    return filter_image

def main( argv ):
  filename = argv[1]       # image test file with model point sources
  filter_size = argv[2]    # size of structure element
                           # = radius for 'D' element, width for 'R' element
  filter_type = argv[3]    # 'D' or 'R'
  make_morphology_image(filename, filter_size, filter_type)

if __name__ == '__main__':
    main(sys.argv)
