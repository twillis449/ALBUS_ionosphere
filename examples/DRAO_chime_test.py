#
# A script to calculate the ionosphere effects over the CHIME telescope from
# the southern to northern horizon (elevation from 0 to 90 degrees and
# then back to 0 at azimuths 0 and 180 degrees in 300 second (5 min) 
# timesteps over a three day period in August 2017 

import MS_Iono_functions as iono
import astropy.io.fits as astropyfits
import math
try:
   import cPickle as pickle
except:
   import pickle

if __name__ == "__main__":

# DRAO experiment
  LAT="49:19:21.425"       # geodetic
  LONG="-119:37:29.94"
  HEIGHT= 541.878
  OBJECT="DRAO_CHIME_test_Aug_2017"
  TIME_STEP=300.0
  MAX_DIST = 350E3
  START_TIME="2017/08/22 00:00:00"
  END_TIME="2017/08/24 23:59:59"
  print (' ******* STARTING UP ********')
  result = iono.process_chime_ionosphere(time_step=TIME_STEP,object=OBJECT,Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,processing_option="RI_G03",do_serial=0,do_plot=1,max_dist=MAX_DIST,num_processors=8, gps_data_directory="/home/twillis/twillis1/DRAO_chime_Aug_2017");

