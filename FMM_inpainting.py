#!/usr/bin/env python

# gratefully derived from https://github.com/olvb/pyheal
# by replacing  RGB stuff with floating point stuff for
# FITS images

# import the necessary packages
import pyheal
from astropy.io import fits
import numpy as np
import sys
import timeit
from check_array import check_array
import astropy.visualization as vis
from read_input_table import  process_input_file
import matplotlib.pyplot as plt

def paint_image(filename):
   freq, names, ra_deg, dec_deg, las, las_raw, red_shift, spec_index = process_input_file(filename)
   num_proc = len(names)
   print('number of images to process', num_proc)
   for i in range(num_proc):
     print('input inage', names[i]+'.fits')
     hdu_list = fits.open(names[i]+'.fits') 
     hdu = hdu_list[0]
     image = check_array(hdu.data)
     orig_image = image.astype(np.float, copy=True)

     hdu_list_m = fits.open(names[i] + '-dilated_tophat.mask.fits')
     hdu1 = hdu_list_m[0]
     mask = check_array(hdu1.data)
#mask = mask.astype(np.uint8)
     mask = mask.astype(bool, copy=False)
     print('image max and min', image.max(), image.min())
     print('mask max and min', mask.max(), mask.min())

     print('mask shape', mask.shape)
     print('image shape', image.shape)
     print(' ')

# initialize the inpainting algorithm to be the Telea et al. method
# load the (1) input image (i.e., the image we're going to perform
# inpainting on) and (2) the  mask which should have the same input
# dimensions as the input image -- zero pixels correspond to areas
# that *will not* be inpainted while non-zero pixels correspond to
# "damaged" areas that inpainting will try to correct
# perform inpainting using OpenCV

     pyheal.inpaint(image, mask, 5)
     hdu.data = image
     hdu.header['DATAMIN'] = hdu.data.min()
     hdu.header['DATAMAX'] = hdu.data.max()
     hdu.writeto(names[i] +'_FMM_inpaint_result.fits', overwrite=True)

# Image inpainting with OpenCV and Python
# show the original input image, mask, and output image after
# applying inpainting

     fig, axes = plt.subplots(ncols=2, nrows=2)
     ax = axes.ravel()

     ax[0].set_title('Original image')
     ax[0].imshow(orig_image, cmap =plt.cm.gray_r, origin='lower')

     ax[1].set_title('Mask')
     ax[1].imshow(mask, cmap=plt.cm.gray, origin='lower')

     ax[2].set_title('inpainted_diffuse_source')
     ax[2].imshow(image,origin='lower')

     for a in ax:
       a.axis('off')

     fig.tight_layout()
     plt.savefig(names[i] +'-InPaint_comparison.png')
     plt.show()
 
def main( argv ):
  start_time = timeit.default_timer()
# nominally use a .csv file such as '3C236.csv' as input foe argv[1]
  # argv[1] = name of pipeline input file with information such as frequency, positions, redshifts
  #         multiply breizorro noise)
  paint_image(argv[1])
  elapsed = timeit.default_timer() - start_time
  print("Run Time:",elapsed,"seconds")
#=============================
# argv[1]  incoming positions file
if __name__ == "__main__":
  main(sys.argv)

