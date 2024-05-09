#!/usr/bin/env python

# scripe to generate polygons from contours and then use polygons
# to get source positions and flux densities

import sys
import numpy as np
import math 
import csv
import timeit
from coords import rad_to_hms, rad_to_dms
from astropy import units as u
from astropy.io import fits
from optparse import OptionParser
from check_array import check_array
from astropy.wcs import WCS
from astropy import wcs
from breizorro_extract import make_noise_map
from beam_to_pixels import calculate_area
from skimage import measure
from skimage.draw import polygon as skimage_polygon
from shapely.geometry import Polygon
from shapely.ops import polylabel
from process_polygon_data import maxDist, simple_distance
from operator import itemgetter, attrgetter
from multiprocessing import Process, Queue

# 'boiler plate' function for handling Queue-based 
# parallel processing
def contour_worker(input, output):
  for func, args in iter(input.get, 'STOP'):
    result = func(*args)
    output.put(result)

# function for processing individual polygons / contours to 
# get contained flux densities  as well as determine if a source is
# resolved or extended
def Process_contour(x,y):
   source_pos = (-10000.0, 0) # return something even if our result is garbage
   rr, cc = skimage_polygon(x,y)
   data_result = orig_image[rr,cc]
#  print('raw sum', data_result.sum())
   sum =  data_result.sum() / pixels_beam
   contained_points = len(rr) # needed if we want a flux density error
   point_source = False
   try:
      max_signal = data_result.max()
   except: 
      print('failure to get maximum within contour ')
      print('probably a contour at edge of image - skipping')
      max_signal = 0.0
   if max_signal >= limiting_flux:
      beam_error = contained_points/pixels_beam  * noise_out 
      ten_pc_error = 0.1 * sum 
      flux_density_error = math.sqrt(ten_pc_error * ten_pc_error + beam_error * beam_error)
      contour = []
      for i in range(len(x)):
         contour.append([x[i],y[i]])
      p = Polygon(contour)
      centroid = p.centroid     # gives position of object
      bounds = p.bounds
#     print(' ')
#     print('bounds', bounds)
      use_max = 0
      if p.contains(centroid):
# as usual, stupid x and y cordinate switching problem 
         lon, lat = w.all_pix2world(centroid.y, centroid.x,0)
      else:
         polylabel_location = polylabel(p, tolerance=1)
#        print('polylabel_location', polylabel_location.x, polylabel_location.y)
         x_pos =  int(polylabel_location.x)
         y_pos =  int(polylabel_location.y)
         data_max = orig_image[x_pos,y_pos]
#        print('polylabel modified x_pos,y_pos, data_max', x_pos, y_pos, data_max)

         use_max = 1
#        print('centroid is ', centroid)
#        print('centroid lies outside polygon - looking for maximum')
         location = np.unravel_index(np.argmax(data_result, axis=None), data_result.shape)
#        print('location', location)
         x_pos = rr[location]
         y_pos = cc[location]
         data_max = orig_image[x_pos,y_pos]
#        print('agw modified x_pos,y_pos, data_max', x_pos, y_pos, data_max)
         lon, lat = w.all_pix2world(y_pos, x_pos, 0)

# do some formatting
      h,m,s = rad_to_hms(math.radians(lon))
      if h < 10:
         h = '0' + str(h)
      else:
         h = str(h)
      if m < 10:
         m = '0' + str(m)
      else:
         m = str(m)
      s = round(s,2)
      if s < 10:
         s = '0' + str(s)
      else:
         s = str(s)
      if len(s) < 5:
         s = s + '0'
      h_m_s = h + ':' + m + ':' + s
      d,m,s = rad_to_dms(math.radians(lat))
      if d >= 0 and d < 10:
         d = '0' + str(d)
      elif d < 0 and abs(d) < 10:
         d = '-0' + str(d)
      else:
         d = str(d)
      if m < 10:
         m = '0' + str(m)
      else:
         m = str(m)
      s = round(s,2)
      if s < 10:
         s = '0' + str(s)
      else:
         s = str(s)
      if len(s) < 5:
         s = s + '0'
      d_m_s = d + ':' + m + ':' + s
      source = h_m_s + ', ' + d_m_s  + ', ' +str(lon) + ', ' + str(lat)
#     print('source', source)

# get source size
      result = maxDist(contour,pixel_size)
      ang_size = result[0]
      pos_angle = result[1]
       
      ratio = np.abs(sum / max_signal)
# test for point source
      if (ratio <= extended_source_ratio) :
        point_source = True
        ang_size = 0.0
        if ratio < 0.8:
          sum  = max_signal 
      else:
        mean_beam = 0.5 * (bmaj+bmin)
        ang_size = ang_size - mean_beam
        if ang_size < 0.0:
          print('ang size < 0')
          ang_size = 0.0
          point_source = True
# also test for point source
#       print('orignal sum,max_signal is', sum,max_signal)
      if point_source:
         output = source + ',' + str(sum) + ',' + str(flux_density_error) + ',' +str(0.0) + ',' +str(0.0)
      else:
         ang = round(ang_size,2)
         pa = round(pos_angle,2)
         output = source + ',' + str(sum) + ',' + str(flux_density_error)  + ',' + str(ang) + ',' + str(pa)
      source_pos = (lon, output, use_max) # we will sort on the lon (ra)
   return source_pos

# function that uses input parameters to set up things for distributed
# and parallel data processing
def generate_source_list(filename, threshold_value, noise,source_ratio):
    global orig_image, pixels_beam , w, pixel_size, bmaj, bmin, noise_out , limiting_flux, extended_source_ratio
    lowercase_fits = filename.lower()
    loc = lowercase_fits.find('.fits')
    outfile = filename[:loc] + '_source_list.txt' 
    f = open(outfile, 'w')
    output = '# processing fits image ' +  filename + ' \n'
    print(output)
    f.write(output)
    print('threshold value', threshold_value)
    print('default noise (Jy)', noise)
    hdu_list = fits.open(filename)
    hdu = hdu_list[0]
    w = WCS(hdu.header)
    w = w.celestial
    orig_image = check_array(hdu.data)
    nans = np.isnan(orig_image)
    orig_image[nans] = 0
#   print('original image max signal', orig_image.max())

# the 'orig_image' can be defined as a global variable as it is only
# used in read-only mode. So it can be shared over separate cores / threads
# without worrying that something can get over-written during processing
    incoming_dimensions = hdu.header['NAXIS']
    pixel_size = hdu.header['CDELT2'] * 3600.0 
    print('pixel size arcsec = ', pixel_size)
    bmaj = hdu.header['BMAJ'] * 3600.0
    bmin = hdu.header['BMIN'] * 3600.0
    mean_beam = 0.5 * (bmaj+bmin)
    output ='# mean beam size (arcsec) ' + str(round(mean_beam,2)) + '\n' 
    f.write(output)
    pixels_beam = calculate_area(bmaj, bmin, pixel_size)
    output ='# calculated pixels per beam ' + str(round(pixels_beam,2)) + '\n'
    print('pixels per beam', output)
    f.write(output)
    if noise == 0.0:
       print('determining noise in image - this may take some time ...')
       noise_out = make_noise_map(orig_image)
       print('make_mask: breizorro median noise ', noise_out)
    else:
       noise_out = noise
    output = '# noise out ' +  str(noise_out) + ' Jy\n'
    f.write(output)
    limiting_flux = noise_out * threshold_value
    print('limiting flux', limiting_flux)
    output = '# limiting_flux ' + str(round(limiting_flux*1000,2)) +' mJy\n'
    f.write(output)
    extended_source_ratio = source_ratio
    print('extended_source_ratio', extended_source_ratio)
    output = '# extended_source_ratio test '+ str(extended_source_ratio) +'\n'
    f.write(output)
    mask = np.where(orig_image>limiting_flux,1.0,0.0)
    mask = mask.astype('float32')
#   print('image mask hdu_header', hdu.header)
    hdu.data = mask
    hdu.header['DATAMIN'] = 0.0
    hdu.header['DATAMAX'] = 1.0
    outfile = 'orig_image_mask.fits'
    hdu.writeto(outfile, overwrite=True)
    print('generated image mask')

    contours = measure.find_contours(mask, 0.5)
    print('number of potential sources ', len(contours))
# Start worker processes
    num_processors = 1
    if num_processors <= 2:
      try:
        import multiprocessing
        processors =  multiprocessing.cpu_count()
        if processors > num_processors:
          num_processors = processors
          print('*** setting number of processors to',num_processors)
      except:
        pass

# set up tasks and queues
    TASKS = []
    for i in range(len(contours)):
       contour = contours[i]
      
       if len(contour) > 2:
         x = []
         y = []
         for j in range(len(contour)):
           x.append(contour[j][0])
           y.append(contour[j][1])
         TASKS.append((Process_contour,(x,y)))

    task_queue = Queue()
    done_queue = Queue()

  # Submit tasks
    print('submitting parallel tasks')
    for task in TASKS:
       task_queue.put(task)
    for i in range(num_processors):
       Process(target=contour_worker, args=(task_queue, done_queue)).start()

    source_list = []
    num_max = 0
# Get the results from parallel processing
    for i in range(len(TASKS)):
       source_pos= done_queue.get(timeout=300)
       if source_pos[0] > -10000.0:
         source_list.append(source_pos)
         num_max =  num_max + source_pos[2]

 # Tell child processes to stop
    for i in range(num_processors):
      task_queue.put('STOP')


    print('sorting ')
    ra_sorted_list = sorted(source_list, key = itemgetter(0))
    num_detected_sources = len(ra_sorted_list)
    output = '# number of sources detected ' + str( num_detected_sources) +'\n'
    f.write(output)
    output = '# number of souces using maximum for source position: ' + str(num_max) + '\n'
    f.write(output)
    f.write('#\n')
    output = '#  source    ra_hms  dec_dms ra(deg)  dec(deg)    flux(mJy)  error ang_size_(arcsec)  pos_angle_(deg)\n'
    output = 'source,ra_hms,dec_dms,ra(deg),dec(deg),flux(Jy),error,ang_size_(arcsec),pos_angle_(deg)\n'
    f.write(output)
    for i in range(len(ra_sorted_list)):
       output = str(i) + ', ' + ra_sorted_list[i][1] + '\n'
       f.write(output)
    f.close()
 # we are done
    return

def main( argv ):
   parser = OptionParser(usage = '%prog [options] ')
   parser.add_option('-f', '--file', dest = 'filename', help = 'FITS file with radio image  (default = None)', default = None)
   parser.add_option('-t', '--threshold', dest = 'threshold', help = 'Threshold value for source detection in units of noise (default = 6)', default = 6)
   parser.add_option('-n', '--noise', dest = 'noise', help = 'noise specification in mJy, where noise cannot be found from image (default = 0)', default = 0.0)
   parser.add_option('-e', '--extend', dest = 'extend', help = 'ratio to determine if a source is extended (default = 1.2)', default = 1.2)
   (options,args) = parser.parse_args()
   print('options', options)
   filename = options.filename
   noise = float(options.noise) 
   if noise > 0.0:
      noise = noise / 1000.0   # convert to Jy
   extension_ratio = float(options.extend) 
   print('extend ratio', extension_ratio) 
   signal_flux = float(options.threshold)
   start_time = timeit.default_timer()
   generate_source_list(filename, signal_flux, noise,extension_ratio)
   print('get_source_list finished \n')
   elapsed = timeit.default_timer() - start_time
   print("Run Time:",elapsed,"seconds")
#=============================
#
# example: run as 'get_simple_source_list.py -f xyz.fits -t 6.5'
#
if __name__ == "__main__":
  main(sys.argv)
