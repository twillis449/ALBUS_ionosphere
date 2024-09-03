# A script for (hopefully) standard collection and processing of GPS data
# to get TEC and RM values for an ASKAP observation. The positions of the
# 36 FPS beams are read in from a ASCII file.

# The script uses python multiprocessing to calculate the ionosphere 
# effects in each beam direction after the GPS receiver data has been 
# collected and corrected for bias etc

import numpy as np
import os
import time
os.system('date')
process_start = time.time()
startime = time.strftime("%a, %d %b %Y %H:%M:%S", time.localtime())
print ("getGPSIONO_ASKAP Start at %s" % startime)

import MS_Iono_functions as iono

# vla position
LAT="+34:4:43.49"
LONG="-107:37:3.82"
HEIGHT=2124

positions_file = 'vla_source_positions'
positions_ascii = np.loadtxt(positions_file, delimiter=',', dtype=str)
print('positions ascii', positions_ascii)

START_TIME="2021/12/12 04:14:35.0"
END_TIME="2021/12/12  08:11:35.0"
OBJECT="VLA-multi_positions"

# Data storage location for GPS files 
gps_data_directory="/home/twillis/VLA_test_data_2021"


# the way to process the GPS data if the data has already been collected
num_processors = 8      

# the way to get and process the data if data has to be obtained from GPS servers
do_serial = 1
do_serial = 0
NUM_PROCESSORS = 1  

iono.process_ionosphere_multi_dir(Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,do_serial=do_serial,num_processors=NUM_PROCESSORS,gps_data_directory=gps_data_directory,object=OBJECT,positions_file=positions_ascii)

# if you have a CASA measurement set you can get the antenna postion(s), start 
# and  end times directly from the meaurement set


os.system('date')
endtime = time.strftime("%a, %d %b %Y %H:%M:%S", time.localtime())
print ("getGPSIONO End at %s" % endtime)
print (' ')
process_end = time.time()
duration = (process_end - process_start)/60.0

print ("getGPSIONO_ASKAP Total run time: %7.2f minutes" % duration)
 
