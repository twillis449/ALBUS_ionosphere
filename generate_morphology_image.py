#!/usr/bin/env python

import sys
import numpy as np
import astropy.visualization as vis
import scipy.special
from astropy.io import fits
from skimage.morphology import erosion, dilation, opening, closing, white_tophat
from skimage.morphology import black_tophat, skeletonize, convex_hull_image
from skimage.morphology import disk, rectangle
from check_array import check_array


# Download an example FITS file, create a 2D cutout, and save it to a
# new FITS file, including the updated cutout WCS.
from astropy.io import fits
from astropy.wcs import WCS

# from https://stackoverflow.com/questions/44865023/how-can-i-create-a-circular-mask-for-a-numpy-array

def create_circular_mask(h, w, center=None, radius=None):

    if center is None: # use the middle of the image
        center = (int(w/2), int(h/2))
    if radius is None: # use the smallest distance between the center and image walls
        radius = min(center[0], center[1], w-center[0], h-center[1])

    Y, X = np.ogrid[:h, :w]
    dist_from_center = np.sqrt((X - center[0])**2 + (Y-center[1])**2)

    mask = dist_from_center <= radius
    return mask

def filter_images(filename, filter_size, filter_type, use_dilation):
    if use_dilation =='T':
       print('generate_morphology_image produces dilated images')
       use_dilation = True
    else:
       use_dilation = False
       print('generate_morphology_image produces eroded images')

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
#   print('filter_selection type',  filter_type)
    if filter_type == 'R':
      print('using rectangle for structure element')
      structure_element = rectangle(size_spec,size_spec)
    else:
      print('using disk for structure element')
#     structure_element = create_circular_mask(size_spec, size_spec, center=None, radius=None)
      structure_element = disk(size_spec)
#   print('structure_element', structure_element)
#   print('original max and min', np.max(data), np.min(data))
# doing one erosion get those wierd structures in the Rudnick paper
    eroded = erosion(data, structure_element)
#   print('eroded')
# doing a second erosion then basically gets rid of most suuff except for point sources
    double_erode = True
    if double_erode:
      eroded = erosion(eroded, structure_element)
#     print('eroded')
#   print('eroded max and min', np.max(eroded), np.min(eroded))
    dilated = dilation(eroded, structure_element)
#   print('dilated max and min', np.max(dilated), np.min(dilated))
    if use_dilation:
      hdu.data = dilated
      hdu.header['DATAMAX'] =  dilated.max()
      hdu.header['DATAMIN'] =  dilated.min()
      outfile = filename + '_dilated.fits'
      hdu.writeto(outfile, overwrite=True)
      data = input_data - dilated
      data = np.nan_to_num(data)
#     print('processed signal (input data - dilated)  max and min', np.max(data), np.min(data))
      hdu.data = data
      hdu.header['DATAMAX'] =  data.max()
      hdu.header['DATAMIN'] =  data.min()
#     print('output orig-dilated max and min', np.max(data), np.min(data))
      outfile = filename + '-dilated.fits'
      hdu.writeto(outfile, overwrite=True)
    else:
      hdu.data = eroded
      hdu.header['DATAMAX'] =  eroded.max()
      hdu.header['DATAMIN'] =  eroded.min()
      outfile = filename + '_eroded.fits'
      hdu.writeto(outfile, overwrite=True)
      data = input_data - eroded
      data = np.nan_to_num(data)
      hdu.header['DATAMAX'] =  data.max()
      hdu.header['DATAMIN'] =  data.min()
#     print('processed signal (input data - eroded)  max and min', np.max(data), np.min(data))
      hdu.data = data
      outfile = filename + '-eroded.fits'
      hdu.writeto(outfile, overwrite=True)

def main( argv ):
  filename = argv[1] # image test file with model point sources
  filter_size = argv[2] # integer number - should be odd
  filter_type = argv[3] # 'D' or 'R'
  use_dilation = argv[4] # T or F
  filter_images(filename, filter_size, filter_type, use_dilation)

if __name__ == '__main__':
    main(sys.argv)

