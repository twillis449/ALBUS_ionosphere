#!/usr/bin/env python

# a script to align radio and optical fields 
# and, then write out final aligned radio field for further processing

from astropy.io import fits
from astropy.wcs import WCS
from reproject import reproject_interp
import astropy.visualization as vis
import matplotlib.pyplot as plt
from astropy.coordinates import SkyCoord
import astropy.units as u
import numpy as np
import sys
import os

def align_fields(radio, optical, show_plot, out_png):
  hdu1 = fits.open(optical)[0]
  hdu2 = fits.open(radio)[0]

#We can examine the two images (this makes use of the wcsaxes package behind the scenes):

  end_point = out_png.find('_align')
  plt.suptitle (out_png[:end_point])

  nans = np.isnan(hdu1.data)
  hdu1.data[nans] = 0
  print('hdu1.data shape', hdu1.data.shape)
  print('hdu2.data shape', hdu2.data.shape)
  #hdu2.data = hdu2.data[0,0,:,:]


  new_header = hdu2.header.copy()
  array, footprint = reproject_interp(hdu2, hdu1.header)

  nans = np.isnan(array)
  array[nans] = 0
  shapes = array.shape
  print('reproject radio array shape ',shapes)
  cutout = array[int(shapes[0]/4):int(shapes[0]*3/4),int(shapes[1]/4):int(shapes[1]*3/4)]
  sigma = np.std(cutout)
  temp_data = np.zeros(array.shape,np.float32)
  print ('array shape',array.shape)
  print ('cutout shape',cutout.shape)
  print('nominal std deviation (mJy) of array ', 1000*sigma)
  sigma =  sigma * 0.5
  levels = [sigma,2*sigma,3*sigma,4*sigma]
  print ('defined indices', int(shapes[0]/4),int(shapes[0]*3/4),int(shapes[1]/4),int(shapes[1]*3/4))
  temp_data[int(shapes[0]/4):int(shapes[0]*3/4),int(shapes[1]/4):int(shapes[1]*3/4)] = cutout
  
  
#The reproject_interp() function above returns the reprojected array as well as an array that provides information on the footprint of the first image in the new reprojected image plane (essentially which pixels in the new image had a corresponding pixel in the old image). We can now visualize the reprojected data and footprint:


# fig, (ax1, ax2) = plt.subplots(ncols=2, figsize=(8, 4), sharex=True, sharey=True, projection=WCS(hdu1.header))
# fig = plt.subplots(ncols=2, figsize=(8, 4))

  wcs = WCS(hdu1.header)
  print('wcs', wcs)
  ax1 = plt.subplot(1,2,1, projection=WCS(hdu1.header))
  interval = vis.PercentileInterval(99.9)
  vmin,vmax = interval.get_limits(hdu1.data)
  print('original optical intensities', vmin,vmax)
  norm = vis.ImageNormalize(hdu1.data, interval=vis.MinMaxInterval(),
                      stretch=vis.SqrtStretch())
  norm = vis.ImageNormalize(vmin=vmin, vmax=vmax, stretch=vis.LogStretch(1000))
  ax1.imshow(hdu1.data, cmap =plt.cm.gray, norm = norm, origin = 'lower') 
  ax1.contour(temp_data, levels, linewidths = 1.0)
  ax1.coords['ra'].set_axislabel('Right Ascension')
  ax1.coords['dec'].set_axislabel('Declination')
  ax1.set_title('Optical Image')

  ax2 = plt.subplot(1,2,2, projection=WCS(hdu1.header))
  interval = vis.PercentileInterval(99.9)
  vmin,vmax = interval.get_limits(cutout)
  vmin,vmax = interval.get_limits(array)
  print('adjusted radio intensities', vmin,vmax)
  norm = vis.ImageNormalize(vmin=vmin, vmax=vmax, stretch=vis.LogStretch(1000))
  ax2.imshow(array, cmap =plt.cm.gray, norm = norm, origin = 'lower') 
  ax2.coords['ra'].set_axislabel('Right Ascension')
  ax2.coords['dec'].set_axislabel('Declination')
  ax2.coords['dec'].set_axislabel_position('r')
  ax2.coords['dec'].set_ticklabel_position('r')
  ax2.set_title('Reprojected Radio Image')
  plt.savefig(out_png)
  if show_plot == 'T':
    plt.show()

  end_point = out_png.find('_align')
  new_header['CRVAL1'] = hdu1.header['CRVAL1']
  new_header['CRVAL2'] = hdu1.header['CRVAL2']

# now reproject back to original radio data
  hdu1.data = array
  array, footprint = reproject_interp(hdu1, new_header)
  nans = np.isnan(array)
  array[nans] = 0
  new_header.data = array
  new_header['DATAMAX'] =  array.max()
  new_header['DATAMIN'] =  array.min()

  fits_out = out_png[:end_point] + '.fits'
  fits.writeto(fits_out, array, new_header, overwrite=True)

# finally delete original 'radio_cutout' file
  if os.path.isfile(radio):
    os.remove(radio)


def main( argv ):
  radio_field = argv[1]
  optical_field = argv[2]
  show_plot = argv[3]
  out_png = argv[4]
  align_fields(radio_field, optical_field, show_plot, out_png)


if __name__ == '__main__':
    main(sys.argv)

