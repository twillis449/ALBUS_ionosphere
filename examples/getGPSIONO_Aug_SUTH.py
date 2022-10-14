#
# A basic python script that tracks a specified position on the 
# sky over the time range from START_TIME to END_TIME from
# a specific location on the Earth's surface.

# The output is a text file giving Slant Tec (STEC) and
# ionosphere rotation measure (RM) as a function of time

import os
import time

import MS_Iono_functions as iono 
import math

if __name__ == "__main__":
  os.system('date')
  process_start = time.time()
  startime = time.strftime("%a, %d %b %Y %H:%M:%S", time.localtime())
  print("getGPSIONO Start at %s" % startime)

# VLA position
  LAT = "34:04:43.75" 	
# = 107:37:05.91 W 
  LONG = "252:22:54.09"  
  HEIGHT = 2115 	

# mean of Sutherland ZA GPS receiver coordinates
  LONG="20:48:37.7"
  LAT="-32:22:48.8" 
  HEIGHT=1799.8

# MEEREKAT telescope location
# mean of MeerKAT_Cartesian_coordinate.csv coordinates
  LONG="21:26:35.736"
  LAT="-30:42:44.838"
  HEIGHT=1059.662443

# 3C286 experiment
  RA = "13:31:08.28811"  
  DEC = "30:30:32.9600"

  OBJECT="VLA_3C286_Aug_14_2022"
  OBJECT="Sutherland_MeerKat_3C286_Aug_14_2022"
  OBJECT="MeerKat_3C286_Aug_14_2022"

  START_TIME="2022/08/14 11:43:33.4"
  END_TIME="2022/08/14 16:55:20.9"

  DATA_DIR = '/home/twillisDownloads/VLA_3C286_test_Sept_2022'
  DATA_DIR = '/home/twillis/Downloads/MeerKat_3C286_test_Aug_14_2022'
  RED_TYPE = 'RI_G01'
  TIME_STEP = 300
  MAX_DIST = 250E3
  NUM_PROCESSORS = 6
  # Note: we set NUM_PROCESSORS = 1 as we are getting data from Geosciences 
  # Australia, which seems to have difficulty responding to a number of ftp
  # requests being received in parallel 
  # After the GPS data have been collected the system will increase the
  # number of processors for the final ionosphere modelling
  iono. process_ionosphere(time_step=TIME_STEP,object=OBJECT,Ra=RA,Dec=DEC,Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,max_dist=MAX_DIST,processing_option=RED_TYPE,do_serial=0,num_processors=NUM_PROCESSORS, gps_data_directory=DATA_DIR);

  os.system('date')
  endtime = time.strftime("%a, %d %b %Y %H:%M:%S", time.localtime())
  print("getGPSIONO End at %s" % endtime)
  print (' ')
  process_end = time.time()
  duration = (process_end - process_start)/60.0
  print("getGPSIONO Total run time: %7.2f minutes" % duration)

