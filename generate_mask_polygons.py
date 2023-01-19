#!/usr/bin/env python

# A script that examines an image and generates polygons (== contours) at 
# locations where the signal in the image is above a specified level.
# It outputs the contours for analysis in later acripts

# see https://stackoverflow.com/questions/48446351/distinguish-button-press-event-from-drag-and-zoom-clicks-in-matplotlib for original suggestion about how to handle zoom etc in matplotlib

import numpy as np
import matplotlib.pyplot as plt

import sys
import os
#import json
#import math
#import subprocess
import numpy as np
import matplotlib.pylab as plt
import matplotlib.cm as cm
import astropy.visualization as vis
from astropy.utils.data import get_pkg_data_filename
from shapely.geometry import Polygon
from astropy.io import fits
from astropy.wcs import WCS
from check_array import check_array

class make_polygon:
  def __init__(self,hdu,mask,morph_signal, file_name) :
    self.out_data = {}
    print('morph_signal', morph_signal)
    self.morph_sign = morph_signal
    self.hdu = hdu
    self.hdu.data = check_array(self.hdu.data)
    self.mask = check_array(mask)
    self.file_name = file_name
    self.press=False
    self.move = False
    self.button = 1  # left button
    self.coords = []
    self.qannotate = []
    self.pic = 1
    self.press_x = 0.0
    self.press_y = 0.0
    self.release_x = 0.0
    self.release_y = 0.0
    
# finally load images
    self.compare_fields()

  def __str__(self):
        print("polygons =", self.out_data)

  def write(self, d=None):
        print(self.__dict__)

  def onclick(self,event):
#       print('\n onclick: event location', event.xdata, event.ydata)
        if event.button == 3: # right button
#          print('calling delete_event_data')
           self.delete_event_data()
#          print('data has been deleted')
           self.ax.figure.canvas.draw()
        if event.inaxes == self.ax and event.button == self.button:
            self.add_event_data(event)
            self.ax.figure.canvas.draw()

  def onpress(self,event):
        if event.button == 2: # middle button
           self.take_a_pic()
           return
        self.press_x = event.xdata 
        self.press_y = event.ydata
        self.press=True

  def onmove(self,event):
        if self.press:
            self.move=True

  def onrelease(self,event):
        self.release_x = event.xdata 
        self.release_y = event.ydata
        if abs(self.release_x - self.press_x) < 1.0 and  abs(self.release_y - self.press_y) < 1.0:
#        if self.press and not self.move:
            self.onclick(event)
        self.press=False; self.move=False

  def take_a_pic(self):
#    print('taking a pic')
     self.outpic = self.image_title.replace(" ", "_") + '_' + str(self.pic) + '.png'
     if os.path.isfile(self.outpic):
        os.remove(self.outpic)
     plt.savefig(self.outpic)
     self.pic = self.pic + 1
 
     return

  def delete_event_data(self):
     for i in range(len(self.qannotate)):
         self.qannotate[i].remove()
     for line in self.ax.get_lines(): # ax.lines:
         line.remove()
     self.coords = []
     self.qannotate = []

  

  def add_event_data(self,event):
      ix =event.xdata 
      iy = event.ydata
#     print('*** event raw pos', ix, iy)

      loc = (ix, iy)
      self.coords.append(loc)
      if len(self.coords) >= 1:
        x = []
        y = []
        for i in range(len(self.coords)):
          x.append(float(self.coords[i][0]))
          y.append(float(self.coords[i][1]))
        x =  np.array(x)
        y =  np.array(y)
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


  def compare_fields(self):
#   print('in compare_fields')
# print ('info',hdu_list.info())
    cen_x = self.hdu.header['CRPIX1']
    cen_y = self.hdu.header['CRPIX2']

#We can examine the two images (this makes use of the wcsaxes package behind the scenes):

    image = check_array(self.hdu.data)
    self.image_shape = image.shape
    print('image shape',  self.image_shape)
    nans = np.isnan(image)
    image[nans] = 0

    wcs = WCS(self.hdu.header)
# print('wcs', wcs)
    fig, (ax1, ax2) = plt.subplots(ncols=2, figsize=(8, 4), sharex=True, sharey=True)
    self.ax = ax2
    self.c1=self.ax.figure.canvas.mpl_connect('button_press_event', self.onpress)
    self.c2=self.ax.figure.canvas.mpl_connect('button_release_event', self.onrelease)
    self.c3=self.ax.figure.canvas.mpl_connect('motion_notify_event', self.onmove)

# print('starting plot')
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
    vmin = 0.0
    norm = vis.ImageNormalize(vmin=vmin, vmax=vmax, stretch=vis.LogStretch(1000))
    ax1.imshow(image, cmap =plt.cm.gray, norm = norm, origin = 'lower') 
    ax1.scatter(cen_x-1, cen_y-1, s=40, marker='+')
    if self.morph_sign == 'T':
      ax1.set_title('Diffuse Image')
    else:
      ax1.set_title('Radio Image')

    levels = [0.5]
    interval = vis.PercentileInterval(99.9)
    vmin,vmax = interval.get_limits(self.mask)
    norm = vis.ImageNormalize(vmin=vmin, vmax=vmax, stretch=vis.LogStretch(1000))
    self.ax.imshow(self.mask, cmap =plt.cm.gray_r, norm = norm, origin = 'lower') 
    self.ax.scatter(cen_x-1, cen_y-1, s=40, marker='+')
    cs = self.ax.contour(self.mask, levels, linewidths = 1.0)
    if self.morph_sign == 'T':
      self.ax.set_title('Compact Mask')
    else:
      self.ax.set_title('Radio Mask')

# Get one of the contours from the plot.
    outer_list =  []
# only use first level as all levels are really the same here
    generate_contours = True
    if generate_contours:
      contour = cs.collections[0]
      num_contours = len(contour.get_paths())
      self.out_data['num_contours'] = num_contours
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

# set up polygon files

    length = len(outer_list)
    if length > 0:
      arr2d = np.array(outer_list)
      columnIndex = 1
      sortedArr = arr2d[arr2d[:,columnIndex].argsort()[::-1]]   # sorts in ascending order
      
#   find n largest values for plotting
      n = 10
      for l in range(length):
        rslt = sortedArr[l] 
        max_cntr = int(rslt[0])
        max_area = rslt[1]
        vs = contour.get_paths()[max_cntr]
        v = vs.vertices
        x = v[:,0]
        y = v[:,1]
        if l < n:
          plt.scatter(x, y)
        poly_coord = [ ]
        for k in range(x.shape[0]):
          y_coord = float(x[k])
          x_coord = float(y[k])
          poly_coord.append((x_coord,y_coord))

    print('exiting compare_fields')

    print('showing plot')
    plt.show()  


# To get the coordinates returned I have to shut down and exit the display 
# When that is done we end up here

    fig.canvas.mpl_disconnect(self.c1)
    length = len(self.coords) 
    if length > 0:
      print('number of polygns selected', length)
      for i in range(length):
         x = self.coords[i][0]
         y = self.coords[i][1]
         self.coords[i] = (y,x) # need to interchange x,y locations to interact with underlying image
      print('************* coords to be dumped', self.coords)
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
