#!/usr/bin/env python

# script to use inpainting to fill in holes of FITS images
# FITS images

# import the necessary packages
import pyheal
from astropy.io import fits
import numpy as np
import sys
import timeit
from check_array import check_array, update_dimensions
import astropy.visualization as vis
from read_input_table import  process_input_file
import matplotlib.pyplot as plt
from optparse import OptionParser
use_cv2 = True
try:
  import cv2 as cv
except:
  use_cv2 = False
#use_cv2 = False

def paint_image(filename, maskname):
   print('incoming file name ', filename)
   if filename.find('csv') > -1 and maskname == None:
     freq, names, ra_deg, dec_deg, las, las_raw, red_shift, spec_index = process_input_file(filename)
     num_proc = len(names)
   else:
     num_proc = 1
     names= []
   for i in range(num_proc):
     if filename.find('csv') > -1:
        input_image = names[i]+'-final_image.fits'
        mask_image =  names[i] + '-white_tophat.mask.fits'
     else:
        input_image = filename
        mask_image = maskname 
     print('input image ',  input_image)
     print('mask image ',  mask_image)
     hdu_list = fits.open(input_image)
     hdu = hdu_list[0]
     incoming_dimensions = hdu.header['NAXIS']
     image = check_array(hdu.data)
     image = image.astype(np.float32)
     orig_image = image.astype(np.float32, copy=True)
     x_min = image.min()
     x_max = image.max()
     print('incoming image max and min', x_max, x_min)
# Although openCV claims that floating point arrays can be processed
# by the impainting algorithms it still seems necesssary to scale the 
# images so that the actual values are in the range 0.0 to 255.0
# (See ex 8 at https://www.programcreek.com/python/example/89305/cv2.inpaint)
     if use_cv2:
       print('Inpainting using openCV') 
       image = 255 * (image - x_min) / (x_max - x_min)
     print('incoming image type', image.dtype)
     print('normalized incoming image max and min', image.max(), image.min())
     hdu_list_m = fits.open(mask_image)
     hdu1 = hdu_list_m[0]
     mask = check_array(hdu1.data)
     mask = mask.astype(np.uint8)
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

     if use_cv2:
# Occasionally the openCV TELEA algorithm generates speckles in the inpainted areas,
# so there is a bug somewhere.
#      inpainted = cv.inpaint(image,mask,inpaintRadius=5, flags=cv.INPAINT_TELEA)

       inpainted = cv.inpaint(image,mask,inpaintRadius=5, flags=cv.INPAINT_NS)
       inpainted = inpainted / 255.0 * (x_max - x_min) + x_min
       print('inpainted image max and min', inpainted.max(), inpainted.min())
       hdu.data = update_dimensions(inpainted, incoming_dimensions)
       hdu.header['DATAMIN'] = hdu.data.min()
       hdu.header['DATAMAX'] = hdu.data.max()
       if len(names) > 0:
         inpainted_result_file =  names[i] +'-CV_NS_inpaint_result.fits'
       else:
         loc = input_image.find('.fits')
         inpainted_result_file =  input_image[:loc] + '-CV_NS_inpaint_result.fits'
       print('inpainted_result_file ',  inpainted_result_file)
       hdu.writeto(inpainted_result_file, overwrite=True)
     else:
       print('Inpainting using pyheal') 
       print('calling pyheal')
       pyheal.inpaint(image, mask, 5)
       hdu.data = update_dimensions(image, incoming_dimensions)
       hdu.header['DATAMIN'] = hdu.data.min()
       hdu.header['DATAMAX'] = hdu.data.max()
       print('inpainted image max and min', hdu.data.max(), hdu.data.min())
       if len(names) > 0:
         inpainted_result_file =  names[i] +'-pyheal_FMM_inpaint_result.fits'
       else:
         loc = input_image.find('.fits')
         inpainted_result_file = input_image[:loc] + '-pyheal_FMM_inpaint_result.fits'
       hdu.writeto(inpainted_result_file, overwrite=True)

# show the original input image, mask, and output image after
# applying inpainting

     fig, axes = plt.subplots(ncols=2, nrows=2)
     ax = axes.ravel()

     interval = vis.PercentileInterval(99.9)
     vmin,vmax = interval.get_limits(orig_image)
     vmin = 0.0
     vmax = 0.25 * vmax
     norm = vis.ImageNormalize(vmin=vmin, vmax=vmax, stretch=vis.LogStretch(1000))

     ax[0].set_title('Original image')
     ax[0].imshow(orig_image, cmap =plt.cm.gray_r, norm = norm, origin='lower')

     ax[1].set_title('Mask')
     ax[1].imshow(mask, cmap=plt.cm.gray, origin='lower')

     ax[2].set_title('inpainted_diffuse_source')
     if use_cv2:
       vmin,vmax = interval.get_limits(inpainted)
       vmin =0.0
       vmax = 0.25 * vmax
       norm = vis.ImageNormalize(vmin=vmin, vmax=vmax, stretch=vis.LogStretch(1000))
       ax[2].imshow(inpainted, cmap =plt.cm.gray_r, norm=norm,origin='lower')
     else:
       vmin,vmax = interval.get_limits(image)
       vmin =0.0
       vmax = 0.25 * vmax
       norm = vis.ImageNormalize(vmin=vmin, vmax=vmax, stretch=vis.LogStretch(1000))
       ax[2].imshow(image, cmap=plt.cm.gray_r, norm=norm,origin='lower')

     for a in ax:
       a.axis('off')

     fig.tight_layout()

     if len(names) > 0:
       if use_cv2:
           inpainted_png_file =  names[i] +'-CV_NS_inpaint_result.png'
       else:
           inpainted_png_file =  names[i] +'-pyheal_inpaint_result.png'
     else:
       loc = input_image.find('.fits')
       if use_cv2:
           inpainted_png_file =  filename[:loc] +'-CV_NS_inpaint_result.png'
       else:
           inpainted_png_file =  filename[:loc] +'-pyheal_inpaint_result.png'
     plt.savefig(inpainted_png_file)
#    plt.show()
 
def main( argv ):
  parser = OptionParser(usage = '%prog [options] ')
  parser.add_option('-f', '--file', dest = 'filename', help = 'Filename of image to be inpainted  (default = None)', default = None)
  parser.add_option('-m', '--mask', dest = 'maskname', help = 'Filename with mask for inpainting (default = None)', default = None)
  (options,args) = parser.parse_args()
  print('options', options)
  filename = options.filename
  maskname = options.maskname
  paint_image(filename, maskname)
#=============================
# argv[1]  incoming positions file
if __name__ == "__main__":
  main(sys.argv)

