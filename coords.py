from datetime import datetime, timedelta
from astropy.io import fits
from astropy.wcs import WCS
from astropy.time import Time
import math

################################################################################
def rad_to_hms(rad):
    """ convert radians to sexagesimal degrees

    This function converts a positive angle in radians to a sexagesimal
    angle in degrees, minutes, and seconds.

    It works for positive angles (take care of the negative part
    yourself.

INPUTS:
rad      I  input angle in radians

OUTPUTS: deg min sec
deg      O  degrees (integer)
min      O  minutes (integer)
sec      O  seconds (float)

    """
#   print('in  rad_to_hms')
    M_RAD2DEG = 180.0/math.pi
    if rad < 0:
      rad = 2.0 * math.pi + rad
    d = math.fabs(rad) * M_RAD2DEG * 24.0 / 360.0
    deg = int(d+2E-13)
    m = (d-deg) * 60.0
    min = int(m+1E-11)
    sec = (m - min) * 60.0
    return deg, min, sec

################################################################################
def rad_to_dms(rad):
    """ convert radians to sexagesimal degrees

    This function converts a positive angle in radians to a sexagesimal
    angle in degrees, minutes, and seconds.

INPUTS:
rad      I  input angle in radians

OUTPUTS: deg min sec
deg      O  degrees (integer)
min      O  minutes (integer)
sec      O  seconds (float)

    """
#   print('in  rad_to_dms')
    M_RAD2DEG = 180.0/math.pi
    d = math.fabs(rad) * M_RAD2DEG
    deg = int(d+2E-13)
    m = (d-deg) * 60.0
    min = int(m+1E-11)
    sec = (m - min) * 60.0
    if rad < 0:
      deg = int(-1 * deg)
    return deg, min, sec


def main():
  hdu_list = fits.open('22h18m37.88s_-51d09m50.5s_radio_cutout.mask.fits')
  hdu = hdu_list[0]
  w = WCS(hdu.header)
  lon, lat = w.all_pix2world(236.5, 132.4,0)
  print('lon, lat',  lon, lat)

  h,m,s = rad_to_hms(math.radians(lon))
  print('h m s',h,m,round(s,2))

  d,m,s = rad_to_dms(math.radians(lat))
  print('d m s',d,m,round(s,2))

if __name__ == '__main__':
    main()

