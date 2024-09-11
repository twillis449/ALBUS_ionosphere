#
# A basic python script that tracks a specified position on the 
# sky over the time range from START_TIME to END_TIME from
# a specific location on the Earth's surface.

# The output is a text file giving Slant Tec (STEC) and
# ionosphere rotation measure (RM) as a function of time

# tests of various objects and times as seen from the VLA

import os
import time

import MS_Iono_functions as iono 
import math

if __name__ == "__main__":
  os.system('date')
  process_start = time.time()
  startime = time.strftime("%a, %d %b %Y %H:%M:%S", time.localtime())
  print("program start at %s" % startime)

  use_elev = True

# vla position
  LAT="+34:4:43.49"
  LONG="-107:37:3.82"
  HEIGHT=2124


  START_TIME="2018/12/30 08:13:00.00"
  END_TIME="2018/12/30 20:00:00.00"

  START_TIME="2022/12/13 07:38:00.00"
  END_TIME="2022/12/13 15:30:00.00"

  START_TIME="2017/08/06 02:28:00.00"
  END_TIME="2017/08/06 10:20:00.00"

  START_TIME="2023/11/04 09:50:00.00"
  END_TIME="2023/11/04 15:15:00.00"

  START_TIME="2017/05/10 03:07:00.00"
  END_TIME="2017/05/10 08:59:00.00"

  START_TIME="2024/01/20 22:37:00.00"
  END_TIME="2024/01/21 06:40:00.00"

  START_TIME="2024/08/10 22:37:00.00"
  END_TIME="2024/08/11 06:40:00.00"

  home_dir = os.path.expanduser('~')
  data_dir = 'VLA_test_2024'
  DATA_DIR = home_dir + '/' + data_dir

  RED_TYPE = 'RI_G01'
  RED_TYPE = 'RI_G02'
  RED_TYPE = 'RI_G04'
  RED_TYPE = 'RI_G05'
  RED_TYPE = 'RI_G06'
  RED_TYPE = 'RI_G07'
  RED_TYPE = 'RI_G08'
  RED_TYPE = 'RI_G09'
  RED_TYPE = 'RI_G03'
  TIME_STEP = 300
  MAX_DIST = 400E3
  MAX_DIST = 225E3
  NUM_PROCESSORS = 1
  DO_SER = 1
  DO_SER = 0

  use_special_body = True
  use_special_body = False

  if use_special_body:
    Special_Body = 'Sun'
    Special_Body = 'Moon'
    OBJECT='Moon_2017_RI_G03_tracking'
#  use the following function if you want to track a  position
    iono.process_ionosphere(time_step=TIME_STEP,object=OBJECT,Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,processing_option=RED_TYPE,do_serial=DO_SER,num_processors=NUM_PROCESSORS,gps_data_directory=DATA_DIR,max_dist=MAX_DIST,special_body=Special_Body)

  else:
# Source positions

    RA = "13:31:08.28811" #  # J2000 coordinates of 3C286
    DEC= "30:30:32.96"

    RA = "14:43:02.758"  # J2000 coordinates of 3C303
    DEC = "52:01:37.16"
    OBJECT="3C303_2022_tracking"


    RA = "12:29:06.7"  # J2000 coordinates of 3C273
    DEC = "02:03:09.0"
    OBJECT="3C273_2022_tracking"


    RA = "16:41:17.607"  # J2000 coordinates of 3C345
    DEC = "39:54:10.71"
    OBJECT="3C345_2017_300km_tracking"

    RA = "07:49:00.0"  # J2000 coordinates of DA240
    DEC = "55:54:00.0"
    OBJECT="DA240_2017_tracking"

    if use_elev:
      print('tracking at zenith')
      OBJECT="VLA_Zenith_Track_2024_with_225km_receiver_range"
      iono.process_ionosphere(time_step=TIME_STEP,object=OBJECT,El=90.0,Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,max_dist=MAX_DIST,processing_option=RED_TYPE,do_serial=DO_SER,num_processors=NUM_PROCESSORS, gps_data_directory=DATA_DIR);
#  use the following function if you want to track a  position
    else:
      iono.process_ionosphere(time_step=TIME_STEP,object=OBJECT,Ra=RA,Dec=DEC,Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,processing_option=RED_TYPE,do_serial=DO_SER,num_processors=NUM_PROCESSORS,gps_data_directory=DATA_DIR,max_dist=MAX_DIST)

  os.system('date')
  endtime = time.strftime("%a, %d %b %Y %H:%M:%S", time.localtime())
  print("program end at %s" % endtime)
  print (' ')
  process_end = time.time()
  duration = (process_end - process_start)/60.0
  print("program total run time: %7.2f minutes" % duration)

