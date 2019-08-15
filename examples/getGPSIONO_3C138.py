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


# ASKAP telescope location
  LONG="116:38:15.0"
  LAT="-26:41:47.9"
  HEIGHT=354.1

# 3C138 experiment
  RA = "05:21:09.886021"  
  DEC = "16:38:22.051220"

  OBJECT="ASKAP_3C138_Dec_03_2018_1_day_350km"
  START_TIME="2018/12/03 00:00:00"
  END_TIME="2018/12/03 23:59:59"
  DATA_DIR = '/home/twillis/twillis1/ASKAP_3C138_Dec_2018'
  RED_TYPE = 'RI_G01'
  TIME_STEP = 300
  MAX_DIST = 350E3
  NUM_PROCESSORS = 1
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

