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

  use_elev = False

# Warkworth telescope location
  LONG="174:40:01.0"
  LAT="-36:25:55.0"
  HEIGHT=111.2        
# height above based on WARK GPS location
# WARK  GPS receiver is at 174 39 46.0  -36 26 03.9    111.2731

# PSR experiment
  RA = "11:41:07.02"  
  DEC = "-65:45:19.1"

  START_TIME="2014/12/01 00:00:00"
  END_TIME="2014/12/01 23:59:59"
  DATA_DIR = '/home/twillis/PSR_J1145_data_2014'
  RED_TYPE = 'RI_G03'
  TIME_STEP = 300
  MAX_DIST = 300E3
  NUM_PROCESSORS = 8
  DO_SER = 0
  DO_SER = 1


# call the following function if you want to just track changes in the ionosphere at an elevation of 90 deg
# i.e. direcly overhead
  if use_elev:
    OBJECT="PSR_J1141-6545_300km_300s_2014_90El"
    iono.process_ionosphere(time_step=TIME_STEP,object=OBJECT,El=90.0,Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,max_dist=MAX_DIST,processing_option=RED_TYPE,do_serial=DO_SER,num_processors=NUM_PROCESSORS, gps_data_directory=DATA_DIR);

#  use the following function if you want to track pulsar position
  else:
    OBJECT="PSR_J1141-6545_300km_300s_2014"
    iono.process_ionosphere(time_step=TIME_STEP,object=OBJECT,Ra=RA,Dec=DEC,Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,max_dist=MAX_DIST,processing_option=RED_TYPE,do_serial=DO_SER,num_processors=NUM_PROCESSORS, gps_data_directory=DATA_DIR);

  os.system('date')
  endtime = time.strftime("%a, %d %b %Y %H:%M:%S", time.localtime())
  print("program end at %s" % endtime)
  print (' ')
  process_end = time.time()
  duration = (process_end - process_start)/60.0
  print("program total run time: %7.2f minutes" % duration)

