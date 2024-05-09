#!/usr/bin/env python

# Multi-resolution filtering of radio images
#    technique described in Rudnick, 2002 https://iopscience.iop.org/article/10.1086/342499/pdf
#	larry@umn.edu -- please contact for assistance, as needed
#    code below courtesy of Viral Parekh, SARAO , vparekh@ska.ac.za
#
# technique creates a diffuse emission map, called “open”; 
#      for small scale features,  filtered = original_map - open

#

#  pick a box size 3x the beam-size or 3x size of features you want to remove 

#  open map has an offset zero level - determine it and correct

#  open map is at original resolution - units are the same in Jy/beam

#  open map will show sharp edges to diffuse regions, but is boxy; convolve for aesthetics 

#

import numpy as np
from astropy.io import fits
from skimage.morphology import erosion, dilation
from skimage.morphology import  rectangle

import sys

# copied from breizorro
def check_array(data):
  if len(data.shape) == 2:
        data = np.array(data[:, :])
  elif len(data.shape) == 3:
        data = np.array(data[0, :, :])
  else:
        data = np.array(data[0, 0, :, :])
  return data

def generate_morphology_images(argv):
  file=sys.argv[1]
  X=int(sys.argv[2])
  Y=int(sys.argv[3])

  hdu_list = fits.open(file) # Note: fits.getdata causes security problems
                           # as it can get remote data
  hdu = hdu_list[0]
  data = check_array(hdu.data)

# do morphology imaging
  structure_element = rectangle(X, Y)
  eroded = erosion(data, structure_element)
  eroded = erosion(eroded, structure_element)
  dilated = dilation(eroded, structure_element)
  openmp = dilated
  newmp = data - dilated

# write out results
# Note: fits.writeto('min.fits',mins,clobber=True) writes out the
# absolute minimum fits file with just about zero header information

  location =  file.find('.fits')
  location =  file.find('.FITS')
  hdu.data = openmp
  hdu.header['DATAMAX'] =  hdu.data.max()
  hdu.header['DATAMIN'] =  hdu.data.min()
  outfile = file[:location] + '_opn.fits'
  hdu.writeto(outfile,overwrite=True) # diffuse emission - correct 0 level as needed

  hdu.data = newmp
  hdu.header['DATAMAX'] =  hdu.data.max()
  hdu.header['DATAMIN'] =  hdu.data.min()
  outfile = file[:location] + '_filt.fits'
  hdu.writeto(outfile,overwrite=True) # fine-scale emission - apply -1*(open zero level)

# end

def main( argv ):
    generate_morphology_images(argv)

# example of command:  'larrys_script.py AbellS1063 7 11'
if __name__ == '__main__':
    main(sys.argv)
