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

  use_elev = True # to monitor variations of the ionosphere directly overhead
                   # set to False to track a given RA/DEC

# potential positions to test

# DEAR station position
  LONG="23:59:33.5"
  LAT= "-30:39:54.7"
  HEIGHT=1321.7

# Upington
  LONG="21:15:27.27"
  LAT="-28:24:25.99"
  HEIGHT=861.71

# MEEREKAT telescope location
# mean of MeerKAT_Cartesian_coordinate.csv coordinates
  LONG="21:26:35.736"
  LAT="-30:42:44.838"
  HEIGHT=1059.662443


# 3C286 source direction
  RA = "13:31:08.28811"
  DEC= "30:30:32.96"

# various start times
  START_TIME="2021/8/8 14:00:00.00"
  END_TIME="2021/8/11 14:00:00.00"
  START_TIME="2021/8/8 14:00:00.00"
  END_TIME="2021/8/9 03:00:00.00"
  START_TIME="2005/5/5 00:00:00"
  END_TIME="2005/5/5 23:59:59.00"
  START_TIME="2023/05/02 00:00:00"
  END_TIME="2023/05/02 23:59:59"


  home_dir = os.path.expanduser('~')
  DATA_DIR = 'meerkat_data'
  DATA_DIR = home_dir + '/' + DATA_DIR

  RED_TYPE = 'RI_G03'
  TIME_STEP = 300
  MAX_DIST = 750E3

  NUM_PROCESSORS = 6
  DO_SER = 1
  DO_SER = 0


# call the following function if you want to just track changes in the ionosphere at an elevation of 90 deg
# i.e. direcly overhead
  if use_elev:
    OBJECT="meerkaT_zenith_2023"
    iono.process_ionosphere(time_step=TIME_STEP,object=OBJECT,El=90.0,Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,max_dist=MAX_DIST,processing_option=RED_TYPE,do_serial=DO_SER,num_processors=NUM_PROCESSORS, gps_data_directory=DATA_DIR);

#  use the following function if you want to track pulsar position
  else:
    OBJECT="meerkat_3c286_2023"
    iono.process_ionosphere(time_step=TIME_STEP,object=OBJECT,Ra=RA,Dec=DEC,Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,max_dist=MAX_DIST,processing_option=RED_TYPE,do_serial=DO_SER,num_processors=NUM_PROCESSORS, gps_data_directory=DATA_DIR);

  os.system('date')
  endtime = time.strftime("%a, %d %b %Y %H:%M:%S", time.localtime())
  print("program end at %s" % endtime)
  print (' ')
  process_end = time.time()
  duration = (process_end - process_start)/60.0
  print("program total run time: %7.2f minutes" % duration)

