#!/usr/bin/env python

import sys
import numpy as np
import subprocess
from astropy.io import fits
from check_array import check_array
from astropy.io import fits
from astropy.wcs import WCS
from breizorro_extract import make_noise_map

#def subt_image(img, dilated_image):
#    sampFunc = np.fft.fft2(img) # visibility of raw sky
#    skyModelVis = np.fft.fft2(dilated_image) # visibilities of dilated image
#    sampFunc = sampFunc - skyModelVis # subtracts dilated from raw
#    return np.real((np.fft.ifft2(sampFunc))) 


def make_mask(filename,limiting_sigma, use_dilation):
    if use_dilation == 'T':
      use_dilation = True
      use_eroded = False
    else:
      use_dilation = False
      use_eroded = True
    limiting_sigma = float(limiting_sigma)
    print('make_mask: incoming file name ', filename)
    print('make_mask: limiting_sigma ', limiting_sigma)
    print('make_mask: use_eroded ', use_eroded)
    

    print('make_mask: processing original file', filename+'.fits')
    hdu_list = fits.open(filename+'.fits')
    print ('info',hdu_list.info())
    hdu = hdu_list[0]
    print('original image type =', hdu.data.dtype)
    orig_image = check_array(hdu.data)
    nans = np.isnan(orig_image)
    orig_image[nans] = 0
# get noise from breizorro
    median_noise = make_noise_map(orig_image)
    print('make_mask: median noise ', median_noise)
    limiting_flux = median_noise * limiting_sigma
    print('make_mask: limiting_flux ', limiting_flux)
    # Download the morphology image
    if use_eroded:
      file_name = filename + '-eroded.fits'
    else:
      file_name = filename + '-dilated.fits'
    # Load the image and the WCS
    print('make_mask: loading morphology image', file_name)
    hdu_list = fits.open(file_name)
    print ('info',hdu_list.info())
    hdu = hdu_list[0]
    print('original image type =', hdu.data.dtype)
    image = check_array(hdu.data)
# create mask from filtered image, where filtered image signal > limiting flux
    mask = np.where(image>limiting_flux,1.0,0.0)
    mask = mask.astype('float32')
    hdu.data = mask
    hdu.header['DATAMIN'] = 0.0
    hdu.header['DATAMAX'] = 1.0
    if use_eroded:
      outfile = filename +'-eroded.mask.fits'
    else:
      outfile = filename +'-dilated.mask.fits'
    print('mask output to ', outfile )
    hdu.writeto(outfile, overwrite=True)

# create filtered image from morphology image  * mask
# so we have filtered data which will be subtracted from original image
    filtered_data = image * mask
    nans = np.isnan(filtered_data)
    filtered_data[nans] = 0

    print('filtered_data min and max', filtered_data.min(),  filtered_data.max())
    hdu.data = filtered_data
    hdu.header['DATAMAX'] =  filtered_data.max()
    hdu.header['DATAMIN'] =  filtered_data.min()

    outfile = filename +'.filtered_data.fits'
    print('filtered_data image output to ', outfile )
    hdu.writeto(outfile, overwrite=True)
    if use_eroded:
      cmd = 'generate_mask_polygons.py ' + filename +'-eroded T'
    else:
      cmd = 'generate_mask_polygons.py ' + filename +'-dilated T' 
    print('processing cmd', cmd)
    returned_value = subprocess.call(cmd, shell=True)  # returns the exit code in unix

# create image from original image - filtered_data
    data = orig_image - filtered_data
    median_noise = make_noise_map(data)
    print('output noise ', median_noise)
    hdu.data = data
    hdu.header['DATAMAX'] =  data.max()
    hdu.header['DATAMIN'] =  data.min()

    print('hdu.data max and main', hdu.data.max(), hdu.data.min())
    if use_eroded:
      outfile = filename +'_final_minus-filtered_eroded_all.fits'
    else:
      outfile = filename +'_final_minus-filtered_dilated_all.fits'
    print('********** final difference file', outfile)
    hdu.writeto(outfile, overwrite=True)
    print('wrote out', outfile)


def main( argv ):
  filename = argv[1]
  limiting_sigma = argv[2]
# offset_flux_factor = argv[3]
  use_dilation = argv[3]
  
  make_mask(filename, limiting_sigma, use_dilation)

if __name__ == '__main__':
    main(sys.argv)

