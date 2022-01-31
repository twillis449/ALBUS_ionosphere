#!/usr/bin/env python

import sys
import numpy as np
import subprocess
from astropy.io import fits
from check_array import check_array
from astropy.io import fits
from astropy.wcs import WCS
from breizorro_extract import make_noise_map
from generate_morphology_image import make_morphology_image
from larrys_script import generate_morphology_images

def make_mask(argv):
    """
    The parameters for doing morphological erosion
    filename: name of fits file  to process
    limiting_sigma: amount by which noise to to be multiplied for mask cutoff
    use_dialate: T = do dilation, F = just do erosion
    filter_size: size for structure element = radius of D or size of with for R
    filter_type: D = Disk, R = Rectangle
    """
    filename = argv[1] # fits file name without '.fits' extension
    limiting_sigma = argv[2]
    use_dilate = argv[3]
    filter_size = argv[4] # integer number
    filter_type = argv[5] # 'D' or 'R'
    
    if use_dilate == 'T':
      use_dilation = True
      use_eroded = False
    else:
      use_dilation = False
      use_eroded = True
    limiting_sigma = float(limiting_sigma)
    print('make_mask: incoming file name ', filename)
    print('make_mask: limiting_sigma ', limiting_sigma)
    print('make_mask: use_eroded ', use_eroded)
    print('make_mask: filter size', filter_size)

#   print('make_mask: processing original file', filename+'.fits')
    hdu_list = fits.open(filename+'.fits')
#   print ('info',hdu_list.info())
    hdu = hdu_list[0]
#   print('original image type =', hdu.data.dtype)
    orig_image = check_array(hdu.data)
    nans = np.isnan(orig_image)
    orig_image[nans] = 0
#   print('original image  data max and min', hdu.data.max(), hdu.data.min())
# get noise from breizorro
    median_noise = make_noise_map(orig_image)
    print('make_mask: median noise ', median_noise)
    limiting_flux = median_noise * limiting_sigma
    print('make_mask: limiting_flux ', limiting_flux)
    # Download the morphology image
    # Load the image and the WCS
    print('calling make_morphology_image with size', filter_size)
    morphology_image = make_morphology_image(filename, filter_size, filter_type, use_dilate)
#   print('morphology image data max and min', morphology_image.max(), morphology_image.min())

    white_tophat = orig_image - morphology_image
    hdu.data = white_tophat
    hdu.header['DATAMIN'] = hdu.data.min()
    hdu.header['DATAMAX'] = hdu.data.max()
#   print('tophat image  data max and min', hdu.data.max(), hdu.data.min())
    if use_eroded:
      out_tophat = filename +'-eroded.fits'
      out_tophat = filename +'-eroded_tophat.fits'
    else:
      out_tophat = filename +'-dilated_tophat.fits'
#   print('make_mask: tophat output to ', out_tophat )
    hdu.writeto(out_tophat, overwrite=True)

    
# create mask from filtered image, where filtered image signal > limiting flux
    mask = np.where(white_tophat>limiting_flux,1.0,0.0)
    mask = mask.astype('float32')
    hdu.data = mask
    hdu.header['DATAMIN'] = 0.0
    hdu.header['DATAMAX'] = 1.0
    if use_eroded:
      outfile = filename +'-eroded_tophat.mask.fits'
    else:
      outfile = filename +'-dilated_tophat.mask.fits'
#   print('mask output to ', outfile )
    hdu.writeto(outfile, overwrite=True)

# create filtered image from morphology image  * mask
# so we have filtered data which will be subtracted from original image
    filtered_data = white_tophat * mask
    filtered_morphology_image = morphology_image * mask
    nans = np.isnan(filtered_data)
    filtered_data[nans] = 0

#   print('filtered_data min and max', filtered_data.min(),  filtered_data.max())
    hdu.data = filtered_data
#   print('filtered data max and min', hdu.data.max(), hdu.data.min())
    hdu.header['DATAMAX'] =  filtered_data.max()
    hdu.header['DATAMIN'] =  filtered_data.min()

    outfile = filename +'.filtered_data.fits'
#   print('filtered_data image output to ', outfile )
    hdu.writeto(outfile, overwrite=True)
# the user can select individual compact objects to delete
#   if use_eroded:
#     cmd = 'display_mask_data.py ' + filename +'-eroded_tophat T'
#   else:
#     cmd = 'display_mask_data.py ' + filename +'-dilated_tophat T' 
#   print('processing cmd', cmd)
#   returned_value = subprocess.call(cmd, shell=True)  # returns the exit code in unix

# don't write the following out anymore - the appropriate information can be 
# gotten from the diffuse and compact images written out below

# create image from original image - filtered_data
#   data = orig_image - filtered_data
#   median_noise = make_noise_map(data)
#   print('output noise ', median_noise)
#   hdu.data = data
#   print('final output image data max and min', hdu.data.max(), hdu.data.min())
#   hdu.header['DATAMAX'] =  data.max()
#   hdu.header['DATAMIN'] =  data.min()

#   print('hdu.data max and main', hdu.data.max(), hdu.data.min())
#   if use_eroded:
#     outfile = filename +'_Final-image_using_all_erosion.fits'
#   else:
#     outfile = filename +'_Final-image_using_all_dilation.fits'
#   print('********** final difference file', outfile)
#   hdu.writeto(outfile, overwrite=True)
#   print('wrote out', outfile)

    masked_image = orig_image *mask
#   print('compact image data max and min', hdu.data.max(), hdu.data.min())
    hdu.data = masked_image
    hdu.header['DATAMAX'] =  hdu.data.max()
    hdu.header['DATAMIN'] =  hdu.data.min()

#   print('hdu.data max and main', hdu.data.max(), hdu.data.min())
    if use_eroded:
      compact_outfile = filename +'_compact_structure_eroded.fits'
    else:
      compact_outfile = filename +'_compact_structure_dilated.fits'
#   print('********** final compact file', outfile)
    hdu.writeto(compact_outfile, overwrite=True)
#   print('wrote out', outfile)

#   diffuse_image = orig_image - masked_image + limiting_flux
    diffuse_image = orig_image - masked_image 
    hdu.data = diffuse_image
#   print('diffuse image data max and min', hdu.data.max(), hdu.data.min())
    hdu.header['DATAMAX'] =  hdu.data.max()
    hdu.header['DATAMIN'] =  hdu.data.min()

#   print('hdu.data max and main', hdu.data.max(), hdu.data.min())
    if use_eroded:
      outfile = filename +'_diffuse_structure_eroded.fits'
    else:
       outfile = filename +'_diffuse_structure_dilated.fits'
#   print('********** final diffuse', outfile)
    hdu.writeto(outfile, overwrite=True)
#   print('wrote out', outfile)

    hdu.data = filtered_morphology_image
#   print('diffuse image data max and min', hdu.data.max(), hdu.data.min())
    hdu.header['DATAMAX'] =  hdu.data.max()
    hdu.header['DATAMIN'] =  hdu.data.min()

#   print('hdu.data max and main', hdu.data.max(), hdu.data.min())
    if use_eroded:
      outfile = filename +'_masked_eroded_image.fits'
    else:
      outfile = filename +'_masked_dilated_image.fits'
    hdu.writeto(outfile, overwrite=True)
#   print('wrote out', outfile)

# do we need to combine some of compact structure back into diffuse image?
    cmd = 'generate_mask_polygons.py ' + filename  + ' T'
    print('processing cmd', cmd)
    returned_value = subprocess.call(cmd, shell=True)  # returns the exit code in unix

def main( argv ):
  """
   The parameters for doing morphological erosion
   filename: name of fits file  to process
   limiting_sigma: amount by which noise to to be multiplied for mask cutoff
   use_dialate: T = do dilation, F = just do erosion
   filter_size: size for structure element = radius of D or size of with for R
   filter_type: D = Disk, R = Rectangle
  """
  if len(argv) > 4 :
# run AGW's code
    make_mask(argv)
  else:
# otherwise run larry's code
# for the function call
# argv[1] = input file name
# argv[2] = X size of rectangle
# argv[3] = Y size of rectangle
    generate_morphology_images(argv)

# example of command:  'make_morphology_mask.py AbellS1063 6 T 3 D'
if __name__ == '__main__':
    main(sys.argv)



