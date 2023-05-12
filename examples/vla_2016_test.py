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
  print("program start at %s" % startime)

  use_elev = False # to monitor variations of the ionosphere directly overhead
                   # set to False to track a given RA/DEC

# vla position
  LAT="+34:4:43.49"
  LONG="-107:37:3.82"
  HEIGHT=2124

# we compare ALBUS result with that of Malins et al 2018  - available at
# https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2018RS006559
# we track Pulsar B0950+08

  RA = "09:53:09.310"  # J2000 coordinates of pulsar
  DEC = "+07:55:35.75"

  START_TIME="2016/10/14 10:00:00.00"
  END_TIME="2016/10/14 17:00:00.00"
  home_dir = os.path.expanduser('~')
  data_dir = 'VLA_test_2016'
  DATA_DIR = home_dir + '/' + data_dir

  RED_TYPE = 'RI_G03'
  TIME_STEP = 300
  MAX_DIST = 300E3
  NUM_PROCESSORS = 6
  DO_SER = 1
  DO_SER = 0


# call the following function if you want to just track changes in the ionosphere at an elevation of 90 deg
# i.e. direcly overhead
  if use_elev:
    OBJECT="VLA_zenith_test_2016"
    iono.process_ionosphere(time_step=TIME_STEP,object=OBJECT,El=90.0,Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,max_dist=MAX_DIST,processing_option=RED_TYPE,do_serial=DO_SER,num_processors=NUM_PROCESSORS, gps_data_directory=DATA_DIR);

#  use the following function if you want to track pulsar position
  else:
    OBJECT="VLA_pulsar_test_2016"
    iono.process_ionosphere(time_step=TIME_STEP,object=OBJECT,Ra=RA,Dec=DEC,Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,max_dist=MAX_DIST,processing_option=RED_TYPE,do_serial=DO_SER,num_processors=NUM_PROCESSORS, gps_data_directory=DATA_DIR);

  os.system('date')
  endtime = time.strftime("%a, %d %b %Y %H:%M:%S", time.localtime())
  print("program end at %s" % endtime)
  print (' ')
  process_end = time.time()
  duration = (process_end - process_start)/60.0
  print("program total run time: %7.2f minutes" % duration)

