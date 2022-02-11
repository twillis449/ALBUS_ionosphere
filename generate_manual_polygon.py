#!/usr/bin/env python

import sys
import os
import numpy as np
import matplotlib.pylab as plt
import matplotlib.cm as cm
import astropy.visualization as vis
from shapely.geometry import Polygon, Point
from astropy.io import fits
from astropy.wcs import WCS
from check_array import check_array
import subprocess

coords = []
# Simple mouse click function to store coordinates
def onclick(event):
    if event.button == 1:
#     global ix, iy
      ix, iy = event.xdata, event.ydata
      print('*** raw pos', ix, iy)
      print('*** x = %d, y = %d'%(ix, iy))

    # assign global variable to access outside of function
      global coords
      loc = (ix, iy)
      coords.append(loc)
#     print('*** updated coords', coords)
      if len(coords) > 1:
        x = []
        y = []
        for i in range(len(coords)):
          x.append(coords[i][0])
          y.append(coords[i][1])
        x =  np.array(x)
        y =  np.array(y)
        ax = plt.gca()
        if len(coords) > 1:
          ax.lines = plt.plot(x, y, 'b')
        ax.figure.canvas.draw()
        plt.savefig('selected_polygons.png')

    if event.button == 3:
      print('resetting coords to zero')
      coords = []
      ax = plt.gca()
      ax.lines = []
      ax.figure.canvas.draw()
      plt.savefig('selected_polygons.png')
    return


def create_polygon(filename):
    # Download the image
    fig = plt.figure(1)

    # Load the image and the WCS
    hdu_list = fits.open(filename)
    print ('info',hdu_list.info())
    hdu = hdu_list[0]
    data = check_array(hdu.data)

# set NaNs to zero
    data = np.nan_to_num(data)
# set all negative numbers to zero
    print('data type ', data.dtype.name)
    print('data shape', data.shape)
    shape = data.shape
    print(type(data))
    wcs = WCS(hdu.header)
    print('orginal wcs', wcs)
  
    plt.subplot(projection=wcs.celestial)
    interval = vis.PercentileInterval(99.9)
    vmin,vmax = interval.get_limits(data)
    vmin = 0.0
    norm = vis.ImageNormalize(vmin=vmin, vmax=vmax, stretch=vis.LogStretch(1000))
#   plt.grid(color='white', ls='solid')
    plt.xlabel('RA')
    plt.ylabel('DEC')
    plt.title(filename)
    
    plt.imshow(data, cmap =plt.cm.gray, norm = norm, origin = 'lower')
    cid = fig.canvas.mpl_connect('button_press_event', onclick)
    plt.show()
    fig.canvas.mpl_disconnect(cid)
    if len(coords) > 2:
        location =  filename.find('.fits')
        outfile = filename[:location] + '.simple_polygon'
        if os.path.isfile(outfile):
          os.remove(outfile)
        f = open(outfile, 'w')
        output = 'polygon outer boundary \n'
        f.write(output)
        print('number of bounds coord', len(coords) )
        for i in range(len(coords)):
          a = round(coords[i][0])
          b = round(coords[i][1])
          output = str(a) + ' ' +  str(b)  + '\n'
          print(output)
          f.write(output)
        f.close()
    print('***************** interior exit')
    return

def main( argv ):
  filename = argv[1]
  create_polygon(filename)

if __name__ == '__main__':
    main(sys.argv)

