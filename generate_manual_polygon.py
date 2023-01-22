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
import weakref
import matplotlib.pylab as plt
import matplotlib.cm as cm
import astropy.visualization as vis
from astropy.utils.data import get_pkg_data_filename
from shapely.geometry import Polygon, Point
from astropy.io import fits
from astropy.wcs import WCS
from check_array import check_array

class make_manual_polygon:
  def __init__(self,file_name):
    self.coords = []
    self.out_data = {}
    self.file_name = file_name
    hdu_list = fits.open(self.file_name)
    self.hdu = hdu_list[0]
    self.image = check_array(self.hdu.data)
    self.press_x = 0.0
    self.press_y = 0.0
    self.release_x = 0.0
    self.release_y = 0.0
    self.pic = 1

    self.compare_fields()

# def __str__(self):
#       print("polygons =", self.out_data)

  def write(self, d=None):
        print(self.__dict__)

  def take_a_pic(self):
#    print('taking a pic')
     self.title = self.file_name + ' Manual Polygon for Flux Density Analysis'
     self.outpic = self.title.replace(" ", "_") + str(self.pic) + '.png'
     if os.path.isfile(self.outpic):
        os.remove(self.outpic)
     plt.savefig(self.outpic)
     self.pic = self.pic + 1
     return


  def onpress(self,event):
     if event.button == 2: # middle button
       print('taking a pic')
       self.take_a_pic()
       return

     self.press_x = event.xdata
     self.press_y = event.ydata
     self.press=True

  def onrelease(self,event):
     self.release_x = event.xdata
     self.release_y = event.ydata
     if abs(self.release_x - self.press_x) < 5.0 and  abs(self.release_y - self.press_y) < 5.0:
         self.onclick(event)


# Simple mouse click function to store coordinates
  def onclick(self,event):
    if event.button == 3:
      ax = plt.gca()
      self.coords = []
      for line in ax.get_lines(): # ax.lines:
         line.remove()
      ax.figure.canvas.draw()
    if event.button == 1:
      ix, iy = event.xdata, event.ydata

    # assign global variable to access outside of function
      loc = (ix, iy)
      self.coords.append(loc)
      if len(self.coords) > 1:
        x = []
        y = []
        for i in range(len(self.coords)):
          x.append(self.coords[i][0])
          y.append(self.coords[i][1])
        x =  np.array(x)
        y =  np.array(y)
        ax = plt.gca()
        ax.plot(x, y,'y')
        ax.figure.canvas.draw()
    return

  def compare_fields(self):
    cen_x = self.hdu.header['CRPIX1']
    cen_y = self.hdu.header['CRPIX2']

#We can examine the two images (this makes use of the wcsaxes package behind the scenes):

    wcs = WCS(self.hdu.header)
    print('orginal wcs', wcs)
    fig = plt.figure(1)
    self.c1=fig.canvas.mpl_connect('button_press_event', self.onpress)
    self.c2=fig.canvas.mpl_connect('button_release_event', self.onrelease)
#   self.c3=fig.canvas.mpl_connect('motion_notify_event', self.onmove)

# set NaNs to zero
    self.image = np.nan_to_num(self.image)
    plt.subplot(projection=wcs.celestial)
    interval = vis.PercentileInterval(99.9)
    vmin,vmax = interval.get_limits(self.image)
    vmin = 0.0
    norm = vis.ImageNormalize(vmin=vmin, vmax=vmax, stretch=vis.LogStretch(1000))
#   plt.grid(color='white', ls='solid')
    plt.xlabel('RA')
    plt.ylabel('DEC')
    plt.title(self.file_name)
    
    plt.imshow(self.image, cmap =plt.cm.gray, norm = norm, origin = 'lower')
    plt.show()
    fig.canvas.mpl_disconnect(self.c1)
    length = len(self.coords) 
    if length > 2:
      locations = []
      for i in range(length):
         x = self.coords[i][0]
         y = self.coords[i][1]
         loc = (y,x)
         locations.append(loc)
      print('************* coords to be dumped', locations)
      num_contours = 1
      self.out_data['num_contours'] = num_contours
      self.out_data['0'] = locations
      p = Polygon(locations)
      self.out_data['manual'] = True
    return self.out_data

def main(argv):
  print('args ', argv) 
  polygon_gen = make_manual_polygon(argv[1])
# print('polygon generated ', polygon_gen.out_data)
if __name__ == '__main__':
  print('generate_manual_polygons argv', sys.argv)
  main(sys.argv)
