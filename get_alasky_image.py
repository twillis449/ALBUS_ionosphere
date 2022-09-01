#!/usr/bin/env python3

# This script will download astronomical images in FITS format
# from the Alasky server. Potential HIPS data sets can be found 
# at https://aladin.u-strasbg.fr/hips/list
# One annoyance is that if the specified data/image cannot be retrieved
# it will still return a FITS file but with nothing useful in it.

# In theory one should be able to retrieve a file by giving a full
# URL to the astropy file name, but that does not seem to allow
# specification of timeouts and I find PyCurl more reliable.

import os
import sys
import numpy
import math

#from astroquery.simbad import Simbad
from astropy.io import fits
from astropy.coordinates import SkyCoord
from urllib.parse import quote
import pycurl


def  download_image_save_cutout(field_name, ra , dec, fov, pixel_size, use_racs):
   print('alasky using pixel size ', pixel_size)
   if use_racs == 'T':
# get radio image from RACS survey
     print('getting radio image')
     hips_list = ['https://www.atnf.csiro.au/research/RACS/RACS_I1/']
   else:
# get optical image from some optical survey  - select the one you want
     print('getting optical image')
     hips_list = ['AllWISE/W3']
     hips_list = ['PanSTARRS/DR1/z']
     hips_list = ['DSS2/red']
   for hips in hips_list:
        fov = float(fov) / 60.0      # convert from arcmin to deg
        pixel_increment = float(pixel_size) / 3600  # convert from arcsec to deg
        width = height = int(fov / pixel_increment) # number of pixel in each direction
        print('alasky parameters fov pixel_inc width', fov,pixel_increment,width)
        position = SkyCoord(ra, dec, frame='icrs', unit='deg')
        print('retrieving data for file ', field_name)
        url = 'http://alasky.u-strasbg.fr/hips-image-services/hips2fits?hips={}&width={}&height={}&fov={}&projection=TAN&coordsys=icrs&ra={}&dec={}'.format(quote(hips), width, height, fov, ra, dec)
        timeout = 600
        with open(field_name, 'wb') as f:
           c = pycurl.Curl()
           c.setopt(c.URL, url)
           c.setopt(pycurl.CONNECTTIMEOUT, timeout)
           c.setopt(pycurl.TIMEOUT, timeout)
           c.setopt(c.WRITEDATA, f)
           print('curl getting data at ',url)
           c.perform()
           print('curl closing for ', url)
           c.close()
           if field_name == 'None':
             print('position', position)
             ra_dec = position.to_string('hmsdms')
             blank =  " "
             underscore = "_"
             out_ra_dec = ra_dec.replace(blank, underscore)
             field_name = out_ra_dec + '_optical_cutout.fits'
           else:
             field_name = field_name + '.fits' 

        print('Saving optical field {}'.format(field_name))
        
def main( argv ):
# argv[1] = name of file image being downloaded by PyCurl
# argv[2] = ra (deg)
# argv[3] = dec (deg)
# argv[4] = field of view, arcmin
# argv[5] = pixel_size arcsec
# argv[6] = use_racs
  filename = argv[1]
  download_image_save_cutout(filename, argv[2], argv[3], argv[4], argv[5], argv[6])

if __name__ == '__main__':
    main(sys.argv)


