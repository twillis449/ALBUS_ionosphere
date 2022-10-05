#
# A basic python script that tracks a specified position on the 
# sky over the time range from START_TIME to END_TIME from
# a specific location on the Earth's surface.

# The output is a text file giving Slant Tec (STEC) and
# ionosphere rotation measure (RM) as a function of time

# Example by Tony Willis

import os
import time
import matplotlib
matplotlib.use("agg") # display should not be set

import MS_Iono_functions as iono 
import math

def __run_rinex_predict(LONG, LAT, HEIGHT, OBJECT, DATA_DIR, MAX_DIST, START_TIME, END_TIME):
    os.system('date')
    process_start = time.time()
    startime = time.strftime("%a, %d %b %Y %H:%M:%S", time.localtime())
    print("getGPSIONO Start at %s" % startime)

    # 3C286 experiment
    RA = "13:31:08.28811"  
    DEC = "30:30:32.9600"

    RED_TYPE = 'RI_G01'
    TIME_STEP = 300

    NUM_PROCESSORS = 1
    # Note: we set NUM_PROCESSORS = 1 as we are getting data from Geosciences 
    # Australia, which seems to have difficulty responding to a number of ftp
    # requests being received in parallel 
    # After the GPS data have been collected the system will increase the
    # number of processors for the final ionosphere modelling
    iono. process_ionosphere(time_step=TIME_STEP,
                             object=OBJECT,
                             Ra=RA,
                             Dec=DEC,
                             Lat=LAT,
                             Long=LONG,
                             Height=HEIGHT,
                             start_time=START_TIME,
                             end_time=END_TIME,
                             max_dist=MAX_DIST,
                             processing_option=RED_TYPE,
                             do_serial=0,
                             num_processors=NUM_PROCESSORS,
                             gps_data_directory=DATA_DIR)

    os.system('date')
    endtime = time.strftime("%a, %d %b %Y %H:%M:%S", time.localtime())
    print("getGPSIONO End at %s" % endtime)
    print (' ')
    process_end = time.time()
    duration = (process_end - process_start)/60.0
    print("getGPSIONO Total run time: %7.2f minutes" % duration)

def __checkmake_outputdir():
    testoutdir = os.environ.get("ALBUS_TESTCASE_OUTPUT", "/albus_waterhole")
    if os.path.exists(testoutdir):
        outputhiddendir = os.path.join(testoutdir, '.output')  
        if not os.path.exists(outputhiddendir):
            os.mkdir(outputhiddendir)
    else:
        raise RuntimeError(f"'{testoutdir}' must be a valid directory -- are you running inside Docker, check mount path?")
    return outputhiddendir

def test_sutherland():
    # Sutherland ZA GPS SUTM receiver coordinates
    LONG="20:48:39.3"
    LAT = "-32:22:53.2"
    HEIGHT=1797.6
    OBJECT="Sutherland_3C286_Aug_14_2022"
    START_TIME="2022/08/14 14:43:33.4"
    END_TIME="2022/08/14 15:55:20.9"
    DATA_DIR = os.path.join(__checkmake_outputdir(), 'Sutherland_3C286_test_Aug_14_2022')

    MAX_DIST = 350E3
    __run_rinex_predict(LONG, LAT, HEIGHT, OBJECT, DATA_DIR, MAX_DIST, START_TIME, END_TIME)

def test_meerkat():
    # MEEREKAT telescope location
    # mean of MeerKAT_Cartesian_coordinate.csv coordinates
    LONG="21:26:35.736"
    LAT="-30:42:44.838"
    HEIGHT=1059.662443
    OBJECT="MeerKat_3C286_Aug_14_2022"
    START_TIME="2022/08/14 14:43:33.4"
    END_TIME="2022/08/14 15:55:20.9"
    DATA_DIR = os.path.join(__checkmake_outputdir(), 'MeerKat_3C286_test_Aug_14_2022')

    MAX_DIST = 350E3
    __run_rinex_predict(LONG, LAT, HEIGHT, OBJECT, DATA_DIR, MAX_DIST, START_TIME, END_TIME)

# def test_vla():
#     # VLA position
#     LAT = "34:04:43.75" 	
#     LONG = "252:22:54.09"  
#     # == 107:37:05.91 W 
#     HEIGHT = 2115 	
#     OBJECT="VLA_3C286_Aug_14_2022"
#     DATA_DIR = os.path.join(__checkmake_outputdir(), 'VLA_3C286_test_Aug_14_2022')
#     START_TIME="2022/08/14 11:43:33.4"
#     END_TIME="2022/08/14 16:55:20.9"
    
#     MAX_DIST = 850E3
#     __run_rinex_predict(LONG, LAT, HEIGHT, OBJECT, DATA_DIR, MAX_DIST, START_TIME, END_TIME)

