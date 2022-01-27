#!/usr/bin/env python

# This program calculates equipartition and other parameters for 
# radio sources. 

from datetime import datetime, timedelta
from astropy import wcs
from astropy.wcs import WCS
from astropy.time import Time
from coords import *
import astropy.visualization as vis
from astropy.io import fits
from shapely.geometry import Polygon, MultiPolygon, Point
from beam_to_pixels import calculate_area
from process_polygon_data import *
from breizorro_extract import make_noise_map
from skimage.draw import polygon

import os.path
import numpy as np
import math
import sys
import json
import time

from equipartition import *


def process_simple_polygon_file(points):
  print('processing polygon file ', points,' \n')
  poly_coord = []
  text = open(points, 'r').readlines()
  L = len(text)
  for i in range(1,L):
    info = text[i].split()
#   print('i info ', i, info)
    x_coord = float(info[0])
    y_coord = float(info[1])
    poly_coord.append((x_coord,y_coord))
  return poly_coord


def analyze_image(filename, freq, z_str, alpha_str, specified_las, use_mask_l):
# note - no '.fits' extension expected ... but
    if filename.find('conv'):
      use_conv = True
    else :
      use_conv = False
    use_mask = True
    if use_mask_l == 'F':
      use_mask = False
    multi = False
    location =  filename.find('.fits')
    if location > 0:
        fits_file =  filename 
        filename = filename[:location]
    if location < 0:
      fits_file =  filename + '.fits'
#   print('processing fits image file ', fits_file,' \n')
    hdu_list = fits.open(fits_file)
    hdu = hdu_list[0]
    w = WCS(hdu.header)
    w =w.celestial
    ref_ra = hdu.header['CRVAL1']
    ref_dec = hdu.header['CRVAL2']
#   print('ref_ra, ref_dec', ref_ra, ref_dec)
    pixel_size = hdu.header['CDELT2'] * 3600.0
#   print('pixel size (arcsec)', pixel_size)
    freq = float(freq)
    bmaj = hdu.header['BMAJ'] * 3600.0
    bmin = hdu.header['BMIN'] * 3600.0
    pixels_beam = calculate_area(bmaj, bmin, pixel_size)
#   print('calculated pixels per beam', pixels_beam)

    # Download the polygon points
    poly_coord = []
    total_area = 0.0
# figure out name of polygon points file
    num_polygons = 0
    if use_mask:
      points = filename + '.json_polygons_data'
#     print('looking for json file ', points)
#     print('os.path.isfile(points)', os.path.isfile(points))
      if os.path.isfile(points):
#       print ("File exists")
        polygon_list, coords, ang_size_all, lobe_las, max_pa, lobe_pa = process_json_file(points,pixel_size)
#       print('****** angular sizes ', lobe_las)
        num_polygons = len(polygon_list)
        if len(polygon_list) > 1:
          p = MultiPolygon(polygon_list)
          multi = True
          total_area = p.area
          computer_las = ang_size_all
        else:
          p = Polygon(polygon_list[0])
          total_area = p.area
          computer_las = lobe_las[0]
          max_pa = lobe_pa[0]
#         print('we have one polygon')
#         print('we have a single polygon with las = ', computer_las)
        #interior_points = get_interior_locations(polygon_list)
#         print('setting polygons_dict to ', polygons_dict.keys())
      else:
         print(points, ':This file does not exist!')
         print('So unable to do calculations, so exiting')
         return
    else:
      points = filename + '.simple_polygon'
#     print('looking for test file ', points)
      polygon_list = [] 
      if os.path.isfile(points):
        poly_coords = process_simple_polygon_file(points)
        multi = False
#       print('simple coords', poly_coords)
        list_p = []
        p = Polygon(poly_coords)
        polygon_list.append(p)
        centroid = p.centroid
#       print('centroid', centroid)
        result = maxDist(poly_coords,pixel_size)
        computer_las = result[0]
        lobe_las = computer_las
        max_pa = result[1]
    if multi:
      n = len(polygon_list) + 1
    else:
      n = 1
# open reporting file
    if use_mask:
      outfile = filename + '.source_parameters'
      output = 'Parameters derived from breizorro mask \n'
    else:
      outfile = filename + '.manual_source_parameters'
      output = 'Parameters derived from manually specified polygon \n'
    f = open(outfile, 'w')
#   print('opended output file', outfile)
    f.write(output)
    output = 'pixels per beam ' + str(pixels_beam) + '\n \n'
    f.write(output)

    average_B = 0.0
    average_U = 0.0
    orig_B = 0.0
    orig_U = 0.0
#   print('*** lobe_las', lobe_las)
    n_flux_mjy = 0.0 
    for i in range(n):
      if i > 0:
       p =  polygon_list[i-1]
       relative_area = p.area / total_area
       centroid = p.centroid
#      print('centroid location', centroid.x,centroid.y)
       lon, lat = w.all_pix2world(centroid.x,centroid.y,0)
#      print('*********  lon, lat ',  lon, lat )
#      print('centroid ra, dec', lon, lat)
       h,m,s = rad_to_hms(math.radians(lon))
       h_m_s = str(h) + 'h'+str(m)+'m'+ str(round(s,2))+'s'
#      print('h m s',h,m,round(s,2))
       d,m,s = rad_to_dms(math.radians(lat))
#      print('d m s',d,m,round(s,2))
       d_m_s = str(d) + 'd'+str(m)+'m'+ str(round(s,2))+'s'
       source = h_m_s + ' ' + d_m_s

#     print('polygon iteration', i)
#     print('p area ', p.area)
#     print('p length ', p.length)
      (minx, miny, maxx, maxy) = p.bounds
#     print('poly minx, miny, maxx, maxy', minx, miny, maxx, maxy)

      if i == 0:
    # Load the image and the WCS
# note there doesn't seem to be any 'standard' way to describe
# frequency in FITS
# note: RACS frequency = 887.5 MHz
        freq_str = str(round(freq,3))
#       print('using frequency (ghz)', freq_str)
        data = hdu.data
        if len(data.shape) == 2:
           data = np.array(data[:, :])
        elif len(data.shape) == 3:
           data = np.array(data[0, :, :])
        else:
           data = np.array(data[0, 0, :, :])
        rms_noise = make_noise_map(data)
#       print('rms_noise will tested for value 1e-10')
        breizorro_noise = True
        if rms_noise <= 1e-10:
          print('no estimate of noise from breizorro - using np.std')
          breizorro_noise = False
          rms_noise = np.std(data)
#         print('revised noise', rms_noise)
        shape = data.shape
        hypotenuse = computer_las / pixel_size
        ellipse_half_size = p.area / (math.pi * 0.5 * hypotenuse )
#       print('hypotenuse , minor ellipse p.area', hypotenuse, ellipse_half_size, p.area)
        theta_small =  ellipse_half_size *2 * pixel_size   # twice size times pixel_size in arcsec
#       print('initial theta_small', theta_small)
# now adjust thetas by beam smearing factor
        theta_big = computer_las
#       print('theta_small arcsec', theta_small)
        try:
#         print('reference ra and dec', ref_ra, ref_dec)
          pixels = w.all_world2pix([[ref_ra,ref_dec]],0)
#         print ('pixels', pixels)
#         print('pixels[0][0]', pixels[0][0])
#         print('pixels[0][1]', pixels[0][1])
          nuclear_pos = Point(pixels[0][0], pixels[0][1])
#         print('nuclear_pos', nuclear_pos)
          y = int(pixels[0][1])
          x = int(pixels[0][0])
#         print('y x', y,x)
          n_flux_mjy = data[y,x] * 1000.0
#         print('signal (mJy) at nuclear_position', n_flux_mjy)
        except:
          print('failure to get pixels from central source coordinates')
          ref_ra = hdu.header['CRPIX1']
          ref_dec = hdu.header['CRPIX2']
          nuclear_pos = Point(ref_ra, ref_dec)
#         print('*** nuclear_pos', nuclear_pos)

# set NaNs to zero
        data = np.nan_to_num(data)
#       print('data range ', data.max(), data.min())

# get mask
        if points.find('mask') > -1:
          try:
            mask_file = filename + '.mask.fits'
#           print('reading in mask file ', mask_file, '\n')
            hdu_list_mask = fits.open(mask_file)
            hdu_mask = hdu_list_mask[0]
            data_mask = hdu_mask.data
#           print('data mask shape', data_mask.shape)
            data = data * data_mask
          except:
            print('failure to get mask file ', mask_file, '\n')
            pass
  # check on calculation
      else:
        try:
#         print('\n calculating individual lobe parameters')
#         print('i, lobe_las[i-1]', i, lobe_las[i-1])
          hypotenuse = float(lobe_las[i-1]) / pixel_size 
          ellipse_half_size = p.area / (math.pi * 0.5 * hypotenuse )
#         print('i hypotenuse , minor ellipse',i, hypotenuse, ellipse_half_size)
          theta_small =  ellipse_half_size *2 * pixel_size   # twice size times pixel size
          theta_big = float(lobe_las[i-1])
#         print('area based theta_big and theta_small', theta_big, theta_small)
        except:
          theta_big = pixel_size * pixel_size *math.sqrt(p.area / math.pi)
          theta_small = theta_big
      new_area = theta_big * theta_small * math.pi /(4.0 *pixel_size * pixel_size)
#     print('calc_check: theta_big theta_small new area', theta_big,theta_small, new_area)

      sum = 0.0
      max_signal = 0.0
      std_list = []
#     print('poly minx, miny, maxx, maxy', minx, miny, maxx, maxy)
#     print('x range fast  = ', int(minx-1), int(maxx+1))
#     print('y range slow  = ', int(miny-1), int(maxy+1))
#     print('data shape', data.shape)
#     print('polygons_dict has keys', polygons_dict.keys())
      img_mask = np.zeros(data.shape, dtype=np.float)
      if i == 0:
        for ii in range(len(polygon_list)):
          result = polygon_list[ii]
          x, y = result.exterior.coords.xy
# Switch x, y  because retrieved x, y values are matplotlib display coordinates
# which are opposite to numpy array coordinates
          temp = x
          x = y
          y = temp
# Use the scikit polygon function to fill the area inside a polygon derived
# from a given contour level. This is much fasted than using shapely to 
# determine all points inside a polygon.
          rr, cc = polygon(x,y, data.shape)
          img_mask[rr, cc] = 1
        data_result = data *img_mask 
        sum =  data_result.sum()
        max_signal = data_result.max()
        contained_points = int(img_mask.sum())
        print('contained points', contained_points)
        print('sum (mJy)', sum* 1000)
      else:
        result = polygon_list[i-1]
        x, y = result.exterior.coords.xy
# switch x, y as retrieved x, y are matplotlib display coordinates
        temp = x
        x = y
        y = temp
        rr, cc = polygon(x,y, data.shape)
        img_mask[rr, cc] = 1
        data_result = data *img_mask
        max_val = data_result.max()
        sum = sum + data_result.sum()
        if max_val > max_signal:
          max_signal = max_val
        contained_points = int(img_mask.sum())
      flux = sum / pixels_beam
#     print('raw total flux density', flux)
      n_flux = 0.0
      p_cont = False
#     print('max_signal vs total flux ', max_signal, flux)
      max_flux_ratio = np.abs((max_signal - flux) / max_signal) 
#     print('max_flux_ratio ', max_flux_ratio)
      if max_flux_ratio < 0.2:
         point_source = True
      else:
         point_source = False
      if p.contains(nuclear_pos):
#       print('polygon containing nuclear soure ', i)
        n_flux = n_flux_mjy / 1000.0 # convert to Jy
#       print('nuclear flux', n_flux)
        p_cont = True
        p_subt = False
#       print('i comparison fluxes ', i, flux, n_flux) # fluxes in units of Jy
        if n_flux > 0.0:
#         flux = flux - n_flux
#         p_subt = True
          n_flux = 0.0
#         print('i adjusted comparison fluxes ', i, flux, n_flux) # fluxes in units of Jy
#     print('points in polygon', contained_points)
#     print ('sums good', sum)
#     print ('flux_density', flux)
#     print ('las, small dist in arcsec ', theta_big, theta_small)
      z = float(z_str)
      alpha = float(alpha_str)
      flux_good = True
      if flux > n_flux:

# adjust angular sizes for convolution
#        print('calling equipartition')
         B_me, u_me, LAS_dist, LAP, LUM_dist, lum, source_size, volume = equipartition_accurate(freq, theta_big, theta_small, flux, n_flux, z, alpha)
#        print('magnetic field (gauss)',  B_me)
#        print('energy_density (erg/cm^3', u_me)
         if i > 0:
           average_U = average_U + u_me * relative_area
           average_B = average_B + B_me * relative_area
         else:
           orig_U = u_me
           orig_B = B_me
      else:
        print('nuclear_flux >= integrated flux !')
        print('unable to calculate extended source parameters')
        flux_good = False

      if i == 0 and flux_good:
        source = filename
        output = 'source: ' +  source + '\n'
        f.write(output)
#       print ('opened and wrote to output ', output)
        output = 'source redshift : ' + z_str + '\n'
        f.write(output)
        output = 'angular size distance (Mpc) : ' + str(round(LAS_dist/1000.0,2)) + '\n'
        f.write(output)
        output = 'luminosity distance (Mpc) : ' + str(round(LUM_dist/1000.0,2)) + '\n'
        f.write(output)
        if not point_source:
          output = 'Computer generated source angular size (arcsec): ' + str(round(computer_las,2)) +'\n'
          f.write(output)
          output = '  At position angle (degrees): ' + str(round(max_pa,0)) +'\n'
          f.write(output)
        specified_las = float(specified_las)
        output = 'Initial specified source angular size (arcsec): ' + str(round(specified_las,2)) + '\n'
        f.write(output)
      else: 
        output = 'polygon source centroid : ' +  source + '\n'
        f.write(output)
      if point_source:
        max_signal = round(max_signal * 1000, 3)
        max_signal = str(max_signal)
        integrated_flux = round(flux * 1000, 3)
        integrated_flux = str(integrated_flux)
#       print('**************** object is probably a point_soure')
        output ='Probably a point source  -  flux densities are similar \n'
        f.write(output)
        output = 'peak flux density = ' + max_signal + ' (mJy),  integrated flux = ' + integrated_flux + ' (mJy) \n\n'
        f.write(output)
        continue
      if not flux_good:
#       print('**************** flux_good is False')
        output ='Nuclear_flux >= integrated flux !' + '\n\n'
        f.write(output)
        continue


      output = 'source apparent projected size (kpc) : ' + str(round(source_size,2)) + '\n'
      f.write(output)
      output = 'model ellipse major and minor axis sizes (arcsec) : ' + str(round(theta_big,2)) + ' ' + str(round(theta_small,2)) + '\n'
      f.write(output)
      output = 'nominal path length through source (kpc) : ' + str(round(LAP,2)) + '\n'
      f.write(output)
      if i == 0:
        if breizorro_noise:
          if round(rms_noise*1000.0,3) <= 0.001:
            output = 'breizorro median noise (microJy) : ' + str(round(rms_noise*1000000.0,3)) + ' \n'
          else:
            output = 'breizorro median noise (mJy) : ' + str(round(rms_noise*1000.0,3)) + ' \n'
        else:
          if round(rms_noise*1000.0,3) <= 0.001:
            output = 'rms noise (microJy) : ' + str(round(rms_noise*1000000.0,3)) + ' \n'
          else: 
            output = 'rms noise (mJy) : ' + str(round(rms_noise*1000.0,3)) + ' \n'
        f.write(output)
      if i == 0:
        output = 'source flux density (mJy) : ' + str(round(flux*1000.0,2)) + ' \n'
      else:
        output = 'lobe flux density (mJy) : ' + str(round(flux*1000.0,2)) + ' \n'
      f.write(output)
      flux_density_error = math.sqrt(contained_points) * rms_noise * 1000 
      if i == 0:
        output = 'source flux density error (mJy) : ' + str(round(flux_density_error,2)) + ' \n'
      else:
        output = 'lobe flux density error (mJy) : ' + str(round(flux_density_error,2)) + ' \n'
      f.write(output)
      if p_cont:
#         print('polygon containing nuclear soure ', i)
          output = 'polygon contains nuclear source \n'
          f.write(output)
          if p_subt:
            output = 'nuclear flux density  (mJy) ' + str(round(n_flux_mjy,3)) + ' subtracted from total flux density \n'
          else:
            output = 'nuclear flux density  (mJy) ' + str(round(n_flux_mjy,3)) + ' not subtracted from total flux density \n'
          f.write(output)
      output = 'equipartition calculations etc assume a spectral index of ' + alpha_str + ' \n'
      f.write(output)
      B_me = B_me * 1.0e6
      output = 'lobe magnetic field (micro gauss) : ' +str(round(B_me,2)) + ' \n'
      f.write(output)
      u_me_ergs = u_me
      u_me = round(u_me * 1.0e14,4)
      output = 'lobe energy_density(* 1.0e14) :  (erg/cm^3) ' + str(round(u_me,2)) + ' | (Joules/m^3) '  + str(round(0.1*u_me,2)) + ' \n'
      f.write(output)
      obs_lum = lum[0] / 1.0e24
      output = freq_str +' GHz luminosity (/ 1.0e24) (W/Hz): ' + str(round(obs_lum,3)) + ' \n'
      f.write(output)
      lum_1_4 = lum[1] / 1.0e24
      output = '1.4 GHz luminosity (/ 1.0e24) (W/Hz): ' + str(round(lum_1_4,3)) + ' with assumed spectral index ' + alpha_str + ' \n'

      f.write(output)
      volume1 = volume /1.0e72
      output = 'lobe volume raw (/ 1.0e72) cm^3 ' + str(volume1) + ' \n'
      f.write(output)
      output = 'lobe volume rounded (/ 1.0e72) cm cubed : ' + str(round(volume1,3)) + ' \n'
      f.write(output)
      total_energy = 2.0 * u_me_ergs * volume / 1.0e58  # add equipartition and magnetic and partical energy together 
#     print('total energy (/ 1.0e58) ', total_energy)
      output = 'total energy in source lobes  (/1.0e58) (ergs) : ' + str(round(total_energy,2)) + ' \n'
      f.write(output)
#     print('flux - n_flux (Jy) ', flux - n_flux)
#     print('pixel_size', pixel_size)
      lobe_surface_brightness = (1000.0 *(flux - n_flux )) / (p.area *pixel_size*pixel_size ) # convert polygon area into units of arcsec
      lobe_surface_brightness = lobe_surface_brightness * 1.0e3
      output = 'lobe surface brightness (*1.0e3) (mJy / arcsec^2 : ' + str(round(lobe_surface_brightness,2)) + ' \n'
      f.write(output)
      if i == (n-1):
        if average_B > 0.0 and average_U > 0.0:
          f.write(' \n')
          output =  'Difference (summed polygons - area weighted average) \n'
          f.write(output)
          average_B = (orig_B - average_B) * 1.0e6
          output = 'mean difference lobe magnetic field (micro gauss) : ' +str(round(average_B,4)) + ' \n'
          f.write(output)
          average_U = (orig_U - average_U) * 1.0e14
          output = 'mean difference lobe energy_density(* 1.0e14) :  (erg/cm^3) ' + str(round(average_U,4)) + ' | (Joules/m^3) '  + str(round(0.1*average_U,4)) + ' \n'
          f.write(output)
        f.close()
        return
      else:
        output = ' \n'
        f.write(output)

def main( argv ):

  os.system('date')
  startime = time.strftime("%a, %d %b %Y %H:%M:%S", time.localtime())
  print("calc_source_parameters Start at %s" % startime)

# note - the input parameters should be given in the sequemce shown in
# the ASCII file ending in .csv

  freq = argv[1]        # frequency in GHz
  filename = argv[2]    # name of data file 
  z = argv[3]           # redshift
  alpha = argv[4]       # spectral index
  specified_las = argv[5] # initial angular size estimate for data  retrieval
  use_mask_l = argv[6]  # use mask or simple polygon
  length = len(argv)
# print ('length of parameters', length)
# print('******************')
  print('**** incoming parameters', freq, filename, z, alpha, specified_las, use_mask_l)
  analyze_image(filename, freq, z, alpha, specified_las, use_mask_l)
  os.system('date')
  endtime = time.strftime("%a, %d %b %Y %H:%M:%S", time.localtime())
  print("calc_source_parameters End at %s" % endtime)
  return
if __name__ == '__main__':
    main(sys.argv)

