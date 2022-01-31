#!/usr/bin/env python

# A script that examines an image and generates polygons (== contours) at 
# locations where the signal in the image is above a specified level.
# It outputs the contours for analysis in later acripts

import sys
import os
import json
import math
import subprocess
import numpy as np
import matplotlib.pylab as plt
import matplotlib.cm as cm
import astropy.visualization as vis
from astropy.utils.data import get_pkg_data_filename
from shapely.geometry import Polygon, Point
from astropy.io import fits
from astropy.wcs import WCS
from check_array import check_array

coords = []
qannotate = []
morph_sign = 'Y'
# Simple mouse click function to store coordinates

def onclick(event):
    if event.button == 1:
      ix, iy = event.xdata, event.ydata
#     print('*** raw pos', ix, iy)

    # assign global variable to access outside of function
      global coords
      global qannotate
      loc = (ix, iy)
      print('even_loc', loc)
      coords.append(loc)
      print('*** updated coords', coords)
      if len(coords) > 1:
        x = []
        y = []
        for i in range(len(coords)):
          x.append(coords[i][0])
          y.append(coords[i][1])
          if i == 0:
            x_ref = coords [i][0]
            y_ref = coords [i][1]
          if i > 0:
            x_pos = coords [i][0]
            y_pos = coords [i][1]
        x =  np.array(x)
        y =  np.array(y)
        print('x,y', x,y)
        ax = plt.gca()
        ax.lines = plt.plot(x, y)
        labels = ['lobe {0}'.format(i+1) for i in range(len(coords))]
        for i in range(len(qannotate)):
         qannotate[i].remove()
        qannotate = []
        for label, x, y in zip(labels, x, y):
          qannotate.append( plt.annotate(
          label,
          xy=(x, y), xytext=(-25, 25),
          textcoords='offset points', ha='right', va='bottom',
          bbox=dict(boxstyle='round,pad=0.5', fc='yellow', alpha=0.5),
          arrowprops=dict(arrowstyle = '->', connectionstyle='arc3,rad=0')) )

        ax.figure.canvas.draw()
      else: 
        ax = plt.gca()
        label  = 'main lobe'
        qannotate.append(plt.annotate(
          label,
          xy=(coords[0][0], coords[0][1]), xytext=(-20, 20),
          textcoords='offset points', ha='right', va='bottom',
          bbox=dict(boxstyle='round,pad=0.5', fc='yellow', alpha=0.5),
          arrowprops=dict(arrowstyle = '->', connectionstyle='arc3,rad=0')) )
        ax.figure.canvas.draw()
#     print('saving png file')
#     print('morph_sign', morph_sign)
      if morph_sign == 'T':
        outpic = 'selected_polygons_for_morphology_analysis.png'
        try:
         os.remove(outpic)
        except:
         pass
        plt.savefig(outpic)
#       print('saved png file')
      else:
        outpic = 'selected_polygons_for_flux_density_analysis.png'
        try:
         os.remove(outpic)
        except:
         pass
        plt.savefig(outpic)
    if event.button == 3:
      ax = plt.gca()
      for i in range(len(qannotate)):
         qannotate[i].remove()
#     print('resetting coords to zero')
      qannotate = []
      coords = []
      ax.lines = []
      ax.figure.canvas.draw()
    return

def compare_fields(radio, mask):
# print('compare_fields: mask file', mask)
# print('compare_fields: radio file', radio)
  hdu_list = fits.open(radio)
# print ('info',hdu_list.info())
  hdu1 = hdu_list[0]
  hdu_list = fits.open(mask)
# print ('info',hdu_list.info())
  hdu2 = hdu_list[0]
  cen_x = hdu2.header['CRPIX1']
  cen_y = hdu2.header['CRPIX2']

#We can examine the two images (this makes use of the wcsaxes package behind the scenes):

  location = mask.find('.mask')
  image = check_array(hdu2.data)
  hdu2.data = image
  nans = np.isnan(hdu2.data)
  hdu2.data[nans] = 0


  image = check_array(hdu1.data)
  hdu1.data = image
  nans = np.isnan(hdu1.data)
  hdu1.data[nans] = 0

  wcs = WCS(hdu1.header)
# print('wcs', wcs)

# print('starting plot')
  fig, (ax1, ax2) = plt.subplots(ncols=2, figsize=(8, 4), sharex=True, sharey=True)
  if morph_sign  == 'T':
    location = radio.find('_')
    plt.suptitle (radio[:location] + ' Comparison of Diffuse and Compact Structures')
  else:
    location = radio.find('.fits')
    plt.suptitle (radio[:location]+ ' image' )
  interval = vis.PercentileInterval(99.9)
  vmin,vmax = interval.get_limits(hdu1.data)
# print('original intensities', vmin,vmax)
  vmin = 0.0
  norm = vis.ImageNormalize(vmin=vmin, vmax=vmax, stretch=vis.LogStretch(1000))
  im = ax1.imshow(hdu1.data, cmap =plt.cm.gray_r, norm = norm, origin = 'lower')
# im = ax1.imshow(astropy_conv_kernel, cmap =plt.cm.gray_r, norm = norm, origin = 'lower')

  ax1.imshow(hdu1.data, cmap =plt.cm.gray, norm = norm, origin = 'lower') 
  ax1.scatter(cen_x-1, cen_y-1, s=40, marker='+')
  if morph_sign == 'T':
    ax1.set_title('Diffuse Image')
  else:
    ax1.set_title('Radio Image')

  levels = [0.5]
# ax2 = plt.subplot(1,2,2, projection=WCS(hdu1.header))
  interval = vis.PercentileInterval(99.9)
  vmin,vmax = interval.get_limits(hdu2.data)
# print('adjusted radio intensities', vmin,vmax)
  norm = vis.ImageNormalize(vmin=vmin, vmax=vmax, stretch=vis.LogStretch(1000))
  ax2.imshow(hdu2.data, cmap =plt.cm.gray_r, norm = norm, origin = 'lower') 
# ax2.imshow(hdu2.data, cmap =plt.cm.gray, norm = norm, origin = 'lower') 
  ax2.scatter(cen_x-1, cen_y-1, s=40, marker='+')
  cs = ax2.contour(hdu2.data, levels, linewidths = 1.0)
  if morph_sign == 'T':
    ax2.set_title('Compact Mask')
  else:
    ax2.set_title('Radio Mask')
  cid = fig.canvas.mpl_connect('button_press_event', onclick)

# Get one of the contours from the plot.
  outer_list =  []
  out_data = {}
# only use first level as all levels are really the same here
  print('mask is', mask)
  if mask.find('mask') > -1:
      print('**** processing data for level',levels[0])
      contour = cs.collections[0]
      num_contours = len(contour.get_paths())
      out_data['num_contours'] = num_contours
      print('number of separate contours', num_contours)
      max_area = 0.0
      max_cntr = 0
      for j in range(num_contours):
        vs = contour.get_paths()[j]
        v = vs.vertices
        x = v[:,0]
        y = v[:,1]
        poly_coord = [ ]
        for k in range(x.shape[0]):
          x_coord = float(x[k])
          y_coord = float(y[k])
          poly_coord.append((x_coord,y_coord))
        if len(poly_coord) > 2:
          p = Polygon(poly_coord)
          out_data[str(j)] = poly_coord
          inner_list = [j,p.area]
          outer_list.append(inner_list)
  print('number of contour elements', len(outer_list))

# set up json file for polygons

# first 'standard' situation
  location =  mask.find('.mask.fits')
  json_data_file = mask[:location] + '.json_polygons_data'

# over-ride if mask for eroded or dilated image
  location =  mask.find('-dilated')
  if location > -1:
    json_data_file = mask[:location] + '-dilated.json_polygons_data'
  location =  mask.find('-eroded')
  if location > -1:
    json_data_file = mask[:location] + '-eroded.json_polygons_data'
  print ('json file should be ', json_data_file)
  try:
    os.remove(json_data_file)
    print('generate mask polygons should have deleted ', json_data_file)
  except:
#   print('file not found, so could not be deleted',json_data_file)
    pass
  length = len(outer_list)
  if length > 0:
    arr2d = np.array(outer_list)
    columnIndex = 1
    sortedArr = arr2d[arr2d[:,columnIndex].argsort()[::-1]]   # sorts in ascending order
#   print('sorted arr2d', sortedArr)
      
#   find n largest values for plotting
    n = 10
    for l in range(length):
      rslt = sortedArr[l] 
#     print('rslt', rslt)
#   show the output 
#     print('index rslt values ', l, rslt[0], rslt[1])
      max_cntr = int(rslt[0])
      max_area = rslt[1]
#     print('max area ', max_area)
#     print('max cntr ', max_cntr)
#     print('parameters for max contour',max_cntr)
      vs = contour.get_paths()[max_cntr]
      v = vs.vertices
      x = v[:,0]
      y = v[:,1]
#     print('length of contour', x.shape)
      if l < n:
        plt.scatter(x, y)
      poly_coord = [ ]
      for k in range(x.shape[0]):
        x_coord = float(x[k])
        y_coord = float(y[k])
        poly_coord.append((x_coord,y_coord))

# weird - to get the global variable stuff printed out I have to shut down the display
# via the mpl_disconnect function
  mask_location = mask.find('mask')
  if mask_location >= 0:
      plt.show()
      fig.canvas.mpl_disconnect(cid)

  if len(coords) > 0:
      print('************* coords to be dumped', coords)
      out_data['coords'] = coords
      with open(json_data_file, 'w') as json_file:
        json.dump(out_data, json_file)
      print('number of bounds coord', len(coords) )
      json_file.close()
  else:
    try:
      os.remove(json_data_file)
    except:
#     print('file not found, so could not be deleted',json_data_file)
      pass

def main( argv ):
  global morph_sign
  morph_sign = argv[2]
  if morph_sign == 'T':
    radio_field = argv[1] + '_diffuse_structure_dilated.fits'
    mask_field = argv[1] + '-dilated_tophat.mask.fits'
  else:
    radio_field = argv[1] + '.fits'
    mask_field = argv[1] + '.mask.fits'
  print('generate_mask_polygon for ' + radio_field + ' ' + mask_field)
  compare_fields(radio_field, mask_field)
if __name__ == '__main__':
    main(sys.argv)
