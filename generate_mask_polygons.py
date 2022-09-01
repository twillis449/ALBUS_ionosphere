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

class make_polygon:
  def __init__(self,hdu,mask,morph_signal, file_name):
    self.coords = []
    self.qannotate = []
    self.out_data = {}
    print('morph_signal', morph_signal)
    self.morph_sign = morph_signal
    self.hdu = hdu
    self.hdu.data = check_array(self.hdu.data)
    self.mask = check_array(mask)
    self.file_name = file_name

    self.compare_fields()

  def __str__(self):
        print("polygons =", self.out_data)

  def write(self, d=None):
        print(self.__dict__)

# Simple mouse click function to store coordinates
  def onclick(self,event):
    if event.button == 1:
      ix, iy = event.xdata, event.ydata
#     print('*** raw pos', ix, iy)

    # assign global variable to access outside of function
      loc = (ix, iy)
#     print('even_loc', loc)
      self.coords.append(loc)
      if len(self.coords) > 1:
        x = []
        y = []
        for i in range(len(self.coords)):
          x.append(self.coords[i][0])
          y.append(self.coords[i][1])
        x =  np.array(x)
        y =  np.array(y)
#       print('x,y', x,y)
        ax = plt.gca()
        ax.lines = plt.plot(x, y)
        labels = ['lobe {0}'.format(i+1) for i in range(len(self.coords))]
        for i in range(len(self.qannotate)):
          self.qannotate[i].remove()
        self.qannotate = []
        for label, x, y in zip(labels, x, y):
          self.qannotate.append( plt.annotate(
          label,
          xy=(x, y), xytext=(-25, 25),
          textcoords='offset points', ha='right', va='bottom',
          bbox=dict(boxstyle='round,pad=0.5', fc='yellow', alpha=0.5),
          arrowprops=dict(arrowstyle = '->', connectionstyle='arc3,rad=0')) )
#       ax.figure.canvas.draw()
      else: 
        ax = plt.gca()
        label  = 'main lobe'
        self.qannotate.append(plt.annotate(
          label,
          xy=(self.coords[0][0], self.coords[0][1]), xytext=(-20, 20),
          textcoords='offset points', ha='right', va='bottom',
          bbox=dict(boxstyle='round,pad=0.5', fc='yellow', alpha=0.5),
          arrowprops=dict(arrowstyle = '->', connectionstyle='arc3,rad=0')) )
      ax.figure.canvas.draw()
      self.outpic = self.image_title.replace(" ", "_") + '.png'
      if os.path.isfile(self.outpic):
        os.remove(self.outpic)
      plt.savefig(self.outpic)
    if event.button == 3:
      ax = plt.gca()
      for i in range(len(self.qannotate)):
         self.qannotate[i].remove()
#     print('resetting coords to zero')
      self.qannotate = []
      self.coords = []
      ax.lines = []
      ax.figure.canvas.draw()
    return

  def compare_fields(self):
# print('compare_fields: mask file', mask)
# print('compare_fields: radio file', radio)
# print ('info',hdu_list.info())
    cen_x = self.hdu.header['CRPIX1']
    cen_y = self.hdu.header['CRPIX2']

#We can examine the two images (this makes use of the wcsaxes package behind the scenes):

    image = check_array(self.hdu.data)
    nans = np.isnan(image)
    image[nans] = 0

    wcs = WCS(self.hdu.header)
# print('wcs', wcs)

# print('starting plot')
    fig, (ax1, ax2) = plt.subplots(ncols=2, figsize=(8, 4), sharex=True, sharey=True)
    end_point = self.file_name.find('.fits')
    if self.morph_sign  == 'T':
      if end_point > -1:
        self.image_title = self.file_name[:end_point] + ' Comparison of Diffuse and Compact Structures'
      else:
        self.image_title = self.file_name + ' Comparison of Diffuse and Compact Structures'
    else:
      if end_point > -1:
        self.image_title = self.file_name[:end_point] + ' Polygons for Flux Density Analysis'
      else:
        self.image_title = self.file_name + ' Polygons for Flux Density Analysis'
    plt.suptitle(self.image_title)
    interval = vis.PercentileInterval(99.9)
    vmin,vmax = interval.get_limits(self.hdu.data)
 # print('original intensities', vmin,vmax)
    vmin = 0.0
    norm = vis.ImageNormalize(vmin=vmin, vmax=vmax, stretch=vis.LogStretch(1000))
    im = ax1.imshow(self.hdu.data, cmap =plt.cm.gray_r, norm = norm, origin = 'lower')
# im = ax1.imshow(astropy_conv_kernel, cmap =plt.cm.gray_r, norm = norm, origin = 'lower')

    ax1.imshow(image, cmap =plt.cm.gray, norm = norm, origin = 'lower') 
    ax1.scatter(cen_x-1, cen_y-1, s=40, marker='+')
    if self.morph_sign == 'T':
      ax1.set_title('Diffuse Image')
    else:
      ax1.set_title('Radio Image')

    levels = [0.5]
# ax2 = plt.subplot(1,2,2, projection=WCS(hdu1.header))
    interval = vis.PercentileInterval(99.9)
    vmin,vmax = interval.get_limits(self.mask)
# print('adjusted radio intensities', vmin,vmax)
    norm = vis.ImageNormalize(vmin=vmin, vmax=vmax, stretch=vis.LogStretch(1000))
    ax2.imshow(self.mask, cmap =plt.cm.gray_r, norm = norm, origin = 'lower') 
# ax2.imshow(hdu2.data, cmap =plt.cm.gray, norm = norm, origin = 'lower') 
    ax2.scatter(cen_x-1, cen_y-1, s=40, marker='+')
    cs = ax2.contour(self.mask, levels, linewidths = 1.0)
    if self.morph_sign == 'T':
      ax2.set_title('Compact Mask')
    else:
      ax2.set_title('Radio Mask')

    cid = fig.canvas.mpl_connect('button_press_event', self.onclick)

# Get one of the contours from the plot.
    outer_list =  []
# only use first level as all levels are really the same here
    generate_contours = True
    if generate_contours:
#     print('**** processing data for level',levels[0])
      contour = cs.collections[0]
      num_contours = len(contour.get_paths())
      self.out_data['num_contours'] = num_contours
#     print('number of separate contours', num_contours)
      max_area = 0.0
      max_cntr = 0
      for j in range(num_contours):
        vs = contour.get_paths()[j]
        v = vs.vertices
        y = v[:,0]
        x = v[:,1]
        poly_coord = [ ]
        for k in range(x.shape[0]):
          x_coord = float(x[k])
          y_coord = float(y[k])
          poly_coord.append((x_coord,y_coord))
        if len(poly_coord) > 2:
          p = Polygon(poly_coord)
          self.out_data[str(j)] = poly_coord
          inner_list = [j,p.area]
          outer_list.append(inner_list)
# print('number of contour elements', len(outer_list))

# set up json file for polygons

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
          y_coord = float(x[k])
          x_coord = float(y[k])
          poly_coord.append((x_coord,y_coord))
# weird - to get the global variable stuff printed out I have to shut down the display
# via the mpl_disconnect function
    mask_location = 1
    if mask_location >= 0:
      plt.show()
      fig.canvas.mpl_disconnect(cid)

    length = len(self.coords) 
    if length > 0:
      for i in range(length):
#        print('initial', self.coords[i])
         x = self.coords[i][0]
         y = self.coords[i][1]
         self.coords[i] = (y,x)
#        print('final', self.coords[i])
#     print('************* coords to be dumped', self.coords)
      self.out_data['coords'] = self.coords
    else:
      self.out_data['coords'] = []
    self.out_data['manual'] = False

    return self.out_data

def main(argv ):
  print('args ', argv) 
  if len(args) == 5:
    polygon_gen = make_polygon(argv[1], argv[2], argv[3], argv[4])
  else:
    polygon_gen = make_polygon(argv[1], argv[2], argv[3], ' ')
if __name__ == '__main__':
  print('generate_mask_polygons argv', sys.argv)
  main(sys.argv)
