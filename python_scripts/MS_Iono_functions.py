# various functions for processing ionosphere data

global DEBUG_SET
DEBUG_SET = False

import math
import numpy
import sys
import os
import time
import copy
import errno
import jma_tools
import astropy.io.fits as astropyfits

#from string import split, strip
from multiprocessing import Process, Queue, current_process
#from Queue import Empty

HAS_EPHEM = True
try:
  import ephem
  print ('*** Successful import of PyEphem')
except:
  HAS_EPHEM = False

# You need to have casacore and pyrap installed if you want to get
# observing paramenters from a Measurement Set
HAS_PYRAP = True
try:
  import pyrap.tables as casatab
  import pyrap.quanta as qu
  from pyrap.measures import measures
  print ('*** Successful import of PYRAP')
except:
  HAS_PYRAP = False

if not HAS_EPHEM and not HAS_PYRAP:
  print ('You must install one of pyrap or PyEphem in order') 
  print ('to handle astronomical dates. Unable to continue processing.')
  sys.exit(-1)

# the following parameter is used to extend the observation range by 0 sec
# before and after the actual specified time. If we want to correct an
# actual data set, this is required for the scipy 1d interpolation routine to 
# ensure that the calculated range of data exceeds the time range actually 
# observed
TIME_OFFSET = 0.0

################################################################################
# JMA's ionosphere stuff

from Albus_iono_object import AlbusIono

Iono_agw = AlbusIono()

import GPS_stations
import Albus_Coordinates as ac
import Albus_RINEX
import Albus_RINEX_2

# The types of fits that can be selected for processing GPS data sets
# The valid RINEX groups are
PROCESSING_OPTION_RINEX_GROUPS = ["RI_G00",  # Single station, nearest sat
                                  "RI_G01",  # single station, all sats
                                  "RI_G02",  # simple 2D
                                  "RI_G03",  # 3D mult (same lat,lon for all h)
                                  "RI_G04",  # 3D many (different lat,lon for h)
                                  "RI_G05",  # 3D spherical harmonics
                                  "RI_G06",  # 2D with time dependance
                                  "RI_G07",  # 2D with gradient LSQ
                                  "RI_G08",  # 2D with time, gradient
                                  "RI_G09",  # 3D spherical, gradient
                                  ]
def set_debug_option(debug_option):
# debug_option can be True or False

  DEBUG_SET = debug_option
  Albus_RINEX_2.set_debug_option(debug_option)
  Albus_RINEX.set_debug_option(debug_option)
  print('MS_Iono_functions setting debug option to ',debug_option)

################################################################################
def get_valid_stations(filename):
# valid_stations = {}
  valid_stations_list = []
  try:
    text = open(filename, 'r').readlines()
    L = len(text)
    # get actual data
    for i in range(L):
      try:
        info = str.split(str.strip(text[i]))
        valid_stations_list.append(info[0])
      except:
        pass
  except:
    pass
# print('valid_stations dict', valid_stations)
# print('valid_stations list', valid_stations_list)
  return valid_stations_list

################################################################################
def get_observation_time_range(MS):
    if not HAS_PYRAP:
      print ('Sorry - you must have PYRAP installed in order to read measurement set parameters')
      return -1
    times = casatab.table(MS.name()).getcol('TIME_CENTROID')
    start_time = numpy.amin(times)
    end_time = numpy.amax(times)
    print('raw start and end time', start_time, end_time)
    return start_time, end_time
################################################################################
# Get the day of year from the Year, month, day
def get_day_of_year(year, month, day):
    """Get the day-of-year integer from the year/month/day

    year   I  YYYY
    month  I  MM starting from January == 1
    day    I  DD starting from 1 == 1
    """
    day_of_year_list = [0,31,59,90,120,151,181,212,243,273,304,334]
    doy = day_of_year_list[month-1] + day
    if(month>2):
        if((year&0x3)==0):
            if((year % 100 != 0) or (year % 400 == 0)):
                doy = doy+1
    return doy

################################################################################
# Get the day of year from the Year, month, day for the start of observations
def get_observation_year_month_day(MS):
    start_time, end_time = get_observation_time_range(MS)
    date_list = qu.quantity(str(start_time)+'s').formatted("YMD").split("/")
    year = int(date_list[0])
    month = int(date_list[1])
    day = int(date_list[2])
    return (year, month, day)

################################################################################
# Get the day of year from the Year, month, day for the end of observations
def get_observation_end_year_month_day(MS):
    start_time, end_time = get_observation_time_range(MS)
    date_list = qu.quantity(str(end_time)+'s').formatted("YMD").split("/")
    year = int(date_list[0])
    month = int(date_list[1])
    day = int(date_list[2])
    return (year, month, day)

################################################################################
# Get the day of year from the Year, month, day for the start of observations
def get_observation_year_month_day_hms(MS):
    start_time, end_time = get_observation_time_range(MS)
    date_list = qu.quantity(str(start_time)+'s').formatted("YMD").split("/")
    print('date list as quantity ', date_list)
    year = int(date_list[0])
    month = int(date_list[1])
    day = int(date_list[2])
    time_list=date_list[3].split(":")
    return (year, month, day,int(time_list[0]),int(time_list[1]),float(time_list[2]))


################################################################################
# Get the day of year from the Year, month, day for the start of observations
def obtain_observation_year_month_day_hms(start_time):
    if HAS_PYRAP:
      date_list = qu.quantity(str(start_time)+'s').formatted("YMD").split("/")
      year = int(date_list[0])
      month = int(date_list[1])
      day = int(date_list[2])
      time_list=date_list[3].split(":")
      return (year, month, day,int(time_list[0]),int(time_list[1]),float(time_list[2]))
    else:
      julian_day = (start_time / 86400.0) + 2400000.5
      year, month, day, hour, minute, second  = jma_tools.get_ymdh_from_JD(julian_day)
      print ('get_ymdh_from_JD gives start time: ', year, month, day, hour, minute, second)
      return (year, month, day,hour, minute, second)
################################################################################
# Get the day of year from the Year, month, day for the start of observations
def convert_observation_year_month_day_hms(date_quantity):
    date_list = date_quantity.formatted("YMD").split("/")
    year = int(date_list[0])
    month = int(date_list[1])
    day = int(date_list[2])
    time_list=date_list[3].split(":")
    return (year, month, day,int(time_list[0]),int(time_list[1]),float(time_list[2]))


################################################################################
# Get the day of year from the Year, month, day for the start of observations
def get_observation_day_of_year(MS):
    (year, month, day) = get_observation_year_month_day(MS)
    return get_day_of_year(year, month, day)


################################################################################
# Get the number of days of observations
def get_number_days_observations(MS):
    """Find out how long the observations are.  Assumes the data has been sorted

    """
    start_time, end_time =  get_observation_time_range(MS)
    date1 = qu.quantity(str(start_time)+'s').formatted("YMD").split("/")
    date2 = qu.quantity(str(end_time)+'s').formatted("YMD").split("/")
    date1=[int(d) for d in date1]
    date2=[int(d) for d in date2]
    day1=get_day_of_year(*date1)
    day2=get_day_of_year(*date2)
    if date2[0]!=date1[0]:
        year=date1[0];
        while year<date2[0]:
            day2+=365;
            if((year&0x3)==0):
                if((year % 100 != 0) or (year % 400 == 0)):
                    day2+=1;
            year+=1;
        if((year&0x3)==0) and (date2[1]>1):
            if((year % 100 != 0) or (year % 400 == 0)):
                day2+=1;
           
    return day2-day1;

################################################################################
# Get the mean X,Y,Z position of the array
def get_mean_array_position(MS):
    """get mean X,Y,Z position of array
    """
    if not HAS_PYRAP:
      print ('Sorry - you must have PYRAP installed in order to read measurement set ANTENNA parameters')
      return -1
    ants = casatab.table(MS.name()+"/ANTENNA").getcol("POSITION");
    x = numpy.mean(ants[:,0])
    y = numpy.mean(ants[:,1])
    z = numpy.mean(ants[:,2])
    pos_list = [x,y,z]
    position = numpy.asarray(pos_list)
    return position;

################################################################################
# Get the  observing direction
def get_observing_position(MS):
    """get reference observing direction in radians
    """
    if not HAS_PYRAP:
      print ('Sorry - you must have PYRAP installed in order to read measurement set PHASE_DIR parameters')
      return -1
    phase_dir = casatab.table(MS.name()+"/FIELD").getcell("PHASE_DIR",0)[0];
    return phase_dir

################################################################################
def get_observation_start_end(MS):
    """Find out what the start, stop times are, for whole dataset.

INPUTS:
MS          I  CASA measurement set to check on

OUTPUTS: start_time, end_time
start_time  O  beginning of initial scan, in seconds of a day from the
               dataset date
end_time    O  end of final scan, in seconds of a day from start_time

    """
    st, et = get_observation_time_range(MS)
    end_time= et - st
    start_time = st/(24*3600)
      
    return start_time, end_time, st, et

################################################################################

def lm_to_radec(L,M, ra0, dec0):
#first convert L,M from deg to rad
  L = math.radians(L)
  M = math.radians(M)
# convert offsets to ra, dec
  N = math.sqrt(1.0 - L*L-M*M)
  div = math.cos(dec0) * N - M * math.sin(dec0)
  ra = ra0 + math.atan2(L,div)
  dec = math.asin(M*math.cos(dec0) + math.sin(dec0) * N)
  return ra,dec

################################################################################
def radec_to_lmn(ra,dec,ra0, dec0):
# everything in radians
    l = math.cos(dec) * math.sin(ra - ra0)
    m = math.sin(dec) * math.cos(dec0) - math.cos(dec) * math.sin(dec0) * math.cos(ra - ra0)
    n = math.sqrt(1 - l**2 - m**2)
    return (l, m, n)


def GeodeticToGeocentricLat(geodetic_lat, height):
# WGS 84 earth parameters
  sm_a = 6378137.0
  invf = 298.257223563
  f = 1.0 / invf

  l_sin = math.sin(geodetic_lat)
  e2 = 2.0 * f - f * f
  div = math.sqrt(1 - e2 * l_sin**2)
  rn =  sm_a / div
  rn_div = rn + height
  ratio = 1 - e2 * rn / rn_div
  tan_geocentric_lat = ratio * math.tan(geodetic_lat)
  geocentric_lat = math.atan(tan_geocentric_lat)
# print ('diff (arcmin) = ', math.degrees(geodetic_lat - geocentric_lat) * 60)
  return geocentric_lat


################################################################################
# parallel processing stuff
def worker(input, output):
    for func, args in iter(input.get, 'STOP'):
        result = func(*args)
#       print ('worker got result ', result[0],result[1])
        output.put(result)

def iono_worker(input, output):
    for func, args in iter(input.get, 'STOP'):
        result = func(*args)
        output.put(result)

################################################################################
def ionosphere_GPS_set_criteria(Max_Sat_Sky_Angle_         = 2.0*math.pi,
                                Min_Sat_Elev_              = 0.1745,     
#                               Max_Rec_Dist_From_Tele_    =  600E3,     
                                Max_Rec_Dist_From_Tele_    =  1500E3,     
                                Max_Iono_Pierce_Dist_      = 2000E3,     
                                Default_Iono_Height_       =  300E3,     
                                Averaging_Time_Half_Width_ = 0.5,        
                                Num_Ionosphere_Parameters_ = 25,         
                                Num_Ionosphere_Heights_    =  3,
                                Num_Time_Terms_            =  1,
                                Theo_Model_Type_           =  3,
#                               Bias_Fit_Type_             =  3
                                Bias_Fit_Type_             =  1
                                ):
    """Set the GPS calibration satellite criteria - function not called at the moment
    
INPUTS:
Max_Sat_Sky_Angle_         = 2.0*math.pi, angular separation between target
                                          and satellite directions, in radians
Min_Sat_Elev_              = 0.1745,      minimul satellite elevation angle, rad
Max_Rec_Dist_From_Tele_    =  300E3,      maximum GPS receiver distance from
                                          telescope, in m
Max_Iono_Pierce_Dist_      = 1000E3,      maximum distance of satellite pierce
                                          point from target pierce point, in m
Default_Iono_Height_       =  300E3,      default ionosphere height for
                                          2-D calculations (also used for
                                          some calculations in 3-D cases too)
                                          in m
Averaging_Time_Half_Width_ = 0.5,         half width of time averaging window
                                          for GPS data, in s
Num_Ionosphere_Parameters_ = 40,          number of parameters to use in fits
Num_Ionosphere_Heights_    =  5           number of heights for 3-D models
Num_Time_Terms_            =  1           The maximum order of time polynomials
                                     to apply.  The ionosphere is made to have
                                     a (a_0 t^0 + a_1 t^1 + ... + a_N t^N)
                                     dependence, where N \equiv NUM_TIME_TERMS-1
                                     Thus, 1 means constant, 2 for linear, 3
                                     for quadratic, and so on.
Theo_Model_Type_           =  0      The theoretical model type to apply in the
                                     ionopshere fitting process.
                                     0 None  -- do not use a theoretical model 
                                             -- also does not seem to do anything useful !!
                                     1 IRI
                                     2 IRI_Plus -- seems to give huge jitter
                                     3 PIM
                                     4 Fake0
Bias_Fit_Type_             =  0      The bias fitting process to use
                               //    0 Use main fit type
                               //    1 Use main fitting, but no theorectical mod
                               //    2 Use global fitting (spherical 3-D)
                               //    3 Use global, but without theoretical model
                               //    4 Use global track fitting (spherical 3-D)
                               //    5 Use global track without theoretical model
                               //    Note that if you have theoretical models 
                               //    turned on, global fitting may take hours
                               //    or days on a normal conputer.

    """
    ionosphere_GPS_set_criteria.Max_Sat_Sky_Angle          = Max_Sat_Sky_Angle_         
    ionosphere_GPS_set_criteria.Min_Sat_Elev               = Min_Sat_Elev_              
    ionosphere_GPS_set_criteria.Max_Rec_Dist_From_Tele     = Max_Rec_Dist_From_Tele_    
    ionosphere_GPS_set_criteria.Max_Iono_Pierce_Dist       = Max_Iono_Pierce_Dist_      
    ionosphere_GPS_set_criteria.Default_Iono_Height        = Default_Iono_Height_       
    ionosphere_GPS_set_criteria.Averaging_Time_Half_Width  = Averaging_Time_Half_Width_ 
    ionosphere_GPS_set_criteria.Num_Ionosphere_Parameters  = Num_Ionosphere_Parameters_ 
    ionosphere_GPS_set_criteria.Num_Ionosphere_Heights     = Num_Ionosphere_Heights_
    ionosphere_GPS_set_criteria.Num_Time_Terms             = Num_Time_Terms_
    ionosphere_GPS_set_criteria.Theo_Model_Type            = Theo_Model_Type_
    ionosphere_GPS_set_criteria.Bias_Fit_Type              = Bias_Fit_Type_
    return
# ***** actual parameters are assigned here
ionosphere_GPS_set_criteria.Max_Sat_Sky_Angle          = 2.0*math.pi
ionosphere_GPS_set_criteria.Min_Sat_Elev               = 0.1745
ionosphere_GPS_set_criteria.Max_Rec_Dist_From_Tele     =  400E3 # drao 50 stns
ionosphere_GPS_set_criteria.Max_Rec_Dist_From_Tele     =  300E3 # drao 21 stns 
ionosphere_GPS_set_criteria.Max_Rec_Dist_From_Tele     =  1500E3 # askap
ionosphere_GPS_set_criteria.Max_Rec_Dist_From_Tele     =  700E3 # lofar, meerkat
ionosphere_GPS_set_criteria.Max_Rec_Dist_From_Tele     =  350E3 # drao 33 stns 
ionosphere_GPS_set_criteria.Max_Iono_Pierce_Dist       = 2000E3
ionosphere_GPS_set_criteria.Default_Iono_Height        =  300E3
ionosphere_GPS_set_criteria.Averaging_Time_Half_Width  = 0.5
ionosphere_GPS_set_criteria.Num_Ionosphere_Parameters  = 25
ionosphere_GPS_set_criteria.Num_Ionosphere_Heights     =  3
ionosphere_GPS_set_criteria.Num_Time_Terms             =  1
ionosphere_GPS_set_criteria.Theo_Model_Type            =  3
ionosphere_GPS_set_criteria.Theo_Model_Type            =  1
print ('starting ionosphere_GPS_set_criteria.Max_Rec_Dist_From_Tele (km) ', ionosphere_GPS_set_criteria.Max_Rec_Dist_From_Tele / 1.0E3)


################################################################################
def get_GPS_data(i,GPS_receivers,Obs_Start,Obs_End,MJD_array,sat_XYZ,output_directory,overwrite,raise_bias_error):

    try:
      GPS = GPS_receivers[i]
#     print ("get_GPS_data Getting data from receiver ", GPS)
      MJD_array, Sat_array, obs_data, sta_XYZ, sat_XYZ, sta_bias_valid, sat_block_pos = \
                Albus_RINEX_2.get_multiple_station_base_observations(Obs_Start,
                                                                     Obs_End,
                                                                     GPS[0],
                                                                     MJD_array,
                                                                     sat_XYZ,
                                                                     output_directory=output_directory,
                                                                     overwrite=overwrite,
                                                                     use_bin_files=1, raise_bias_error = raise_bias_error)
      print ('get_GPS_data finished for sequence, receiver ', i, GPS[0])
      return (i,GPS,MJD_array, Sat_array, obs_data, sta_XYZ, sat_XYZ, sta_bias_valid, sat_block_pos) 
    except Albus_RINEX.No_RINEX_File_Error as error:
            # try to ignore this error for now
            print(str(error))
            print ("Failed to get data for GPS receiver '%s', skipping"%(GPS[0]))
            return(-1,GPS[0])
    except Albus_RINEX.RINEX_Data_Barf as error:
            # ignore the stupid header problems for now
            print(str(error))
            print ("Failed to get data for GPS receiver '%s' because of header/data problems, skipping"%(GPS[0]))
            return(-1,GPS[0])

################################################################################
def predict_iono_corrections(index, times,location_ra,location_dec):
  start = int(times[index][0])
  end = int(times[index][1])
  print('start and end', start, end)
  processor = copy.deepcopy(Iono_agw)
  print ('in predict_iono_corrections - range = ', start, end)
  iono_predict = []
  iono_predict.append(index)
  use_location = False
  assign_pos = True
  if len(location_ra) > 0:
    use_location = True
# print (' predict_iono_corrections use_location = ', use_location)
  for i in range(start,end):
    if use_location and assign_pos:
      if len(location_ra) > 1:
        retval = processor.set_source_position(location_ra[i],location_dec[i])
        print ('assigning position for i, ra, dec, retval', i, math.degrees(location_ra[i]),math.degrees(location_dec[i]), retval)
      else:
        retval = processor.set_source_position(location_ra[0],location_dec[0])
        assign_pos = False
        print ('assigning position for i, ra, dec, retval', i, math.degrees(location_ra[0]),math.degrees(location_dec[0]), retval)
    retval,pred_time,time_width,El,Az,STEC,SRM,VTEC_factor,STEC_ERR = processor.get_ionospheric_prediction(i)
    if(retval < 0):
      raise RuntimeError("Error: Iono_agw.get_ionospheric_prediction gave %d"%retval)
    if STEC_ERR == -999.0:
      print ('*** predction failure:', retval,pred_time,time_width,El,Az,STEC,SRM,VTEC_factor,STEC_ERR)
      retval = 1
# convert SRM to radians/m^2, and STEC into TEC units
# from pimvlbi4 note
#   RM = 1.31e-13 * SRM
# from various other papers - weight seems to be for this value!!
    if retval == 0:
      RM = 2.63e-13 * SRM
# convert STEC into TEC units
      STEC = STEC * 1.0e-16
      STEC_ERR = STEC_ERR * 1.0e-16
    else:
      RM = SRM
    iono_predict.append((i,retval,pred_time,time_width,El,Az,STEC,RM,VTEC_factor,STEC_ERR))
    if i%10 == 0:
      print ('processed time sequence', i)

  del processor
  return iono_predict

################################################################################
def predict_chime_iono_corrections(index, location_ra,location_dec,location_az,location_el):
  processor = copy.deepcopy(Iono_agw)
  iono_predict = []
  iono_predict.append(index)
  use_location = False
# print (' predict_iono_corrections use_location = ', use_location)
  for j in range(len(location_ra)):
    retval = processor.set_source_position(location_ra[j],location_dec[j])
    print ('assigning position for i, j, ra, dec, retval', index, j, math.degrees(location_ra[j]),math.degrees(location_dec[j]), retval)
    retval,pred_time,time_width,El,Az,STEC,SRM,VTEC_factor,STEC_ERR = processor.get_ionospheric_prediction(index)
    if(retval < 0):
      raise RuntimeError("Error: Iono_agw.get_ionospheric_prediction gave %d"%retval)

#**************
# over-ride Anderson computed Az and El as they are based on a geocentric earth.
#   El = location_el[j]
#   Az = float(location_az[j])

# convert SRM to radians/m^2, and STEC into TEC units
# from pimvlbi4 note
#   RM = 1.31e-13 * SRM
# from various other papers - weight seems to be for this value!!
    RM = 2.63e-13 * SRM
# convert STEC into TEC units
    STEC = STEC * 1.0e-16
    STEC_ERR = STEC_ERR * 1.0e-16
    print ('predict_chime_iono_corrections index,retval,pred_time,time_width,El,Az,STEC,RM,VTEC_factor,STEC_ERR', index,retval,pred_time,time_width,El,Az,STEC,RM,VTEC_factor,STEC_ERR)
    iono_predict.append((retval,pred_time,time_width,El,Az,STEC,RM,VTEC_factor,STEC_ERR))
  if index%10 == 0:
    print ('processed time sequence', index)
  del processor
  return iono_predict


################################################################################


################################################################################
def process_GPS_data(input,SIG_pos,PL_pos):
    j = input[0]
    GPS = input[1]
    MJD_array = input[2]
    Sat_array = input[3]
    obs_data = input[4]
    sta_XYZ = input[5]
    sat_XYZ = input[6]
    sta_bias_valid = input[7]
    sat_block_pos = input[8]

    try: 
      result = 1
      Sat_small = Albus_RINEX_2.convert_Sat_array_to_small_array(Sat_array,
                                                                 obs_data)
      block_small = \
          Albus_RINEX_2.convert_sat_block_pos_to_small_array(sat_block_pos,
                                                                   Sat_array,
                                                                   obs_data)
      sh = obs_data.shape
      # convert from TECU to m^{-2}
      STEC = obs_data[:,:,PL_pos]
      sigma = obs_data[:,:,SIG_pos]
      for t in range(sh[0]):
        for s in range(sh[1]):
              if((STEC[t,s] != Albus_RINEX_2.BAD_DATA_CODE)
                 and(sigma[t,s] != Albus_RINEX_2.BAD_DATA_CODE)):
                  STEC[t,s] *= Albus_RINEX_2.TECU_TO_M_N2
                  sigma[t,s] *= Albus_RINEX_2.TECU_TO_M_N2
              else:
                  STEC[t,s] = Albus_RINEX_2.BAD_DATA_CODE
                  sigma[t,s] = Albus_RINEX_2.BAD_DATA_CODE
      if(Iono_agw.cal_observations_set_cal_obs(
          GPS[0], GPS[1], sh[0], sh[1], Sat_small, block_small,
          STEC, sigma, sta_bias_valid)):
          raise RuntimeError("could not set receiver data for Iono_agw")
    except: 
      result = -1
    return result   


################################################################################
def ionosphere_GPS_setup_GPS(MS, MSlabel, processing_option,telescope_pos=0, 
                             start_time=0,end_time=0,
                             overwrite=1,raise_bias_error=0,do_serial=0,num_processors=2,output_directory = "."):
    """set up for a GPS fitting model

INPUTS:
MS          I  a pyrap.tables.table of an  MS 
processing_option  I  a string defining which model to use
overwrite          I  flag as to whether the given table version may be
                          overwritten
output_directory   I  Where to put things


OUTPUTS: None
    """
    assert(processing_option in PROCESSING_OPTION_RINEX_GROUPS)
    print ('do serial set to ', do_serial)

    # does output directory exist?
    try:
      if do_serial == 0:
        if output_directory != ".":
          command = "/bin/rm -rf " + output_directory
          os.system(command)
        os.makedirs(output_directory)
    except OSError as exception:
        if exception.errno != errno.EEXIST:
            raise

    # get rid of old data files?
    if len(MSlabel) > 0:
      station_file = output_directory +"/" + 'albus_good_stations' + '_'+ MSlabel
    else:
      station_file = output_directory +"/" + 'albus_good_stations' 
    if do_serial == 0:
      if output_directory == ".":
        os.system("/bin/rm -rf *blo *XYZ *obs *MJD *.sat *.*d *.*o P1* *.*i *.eph *.pyc *.Z")
      if os.path.exists(station_file):
        os.remove(station_file)
      station_log = open(station_file, 'a')
    else:
      valid_stations = get_valid_stations(station_file)
      if len(valid_stations) == 0:
        print ('no valid stations list found for data processing')
        print ('will try to collect new data')
# reset to do collection of new data
        do_serial = 0
        if output_directory == ".":
          os.system("/bin/rm -rf *blo *XYZ *obs *MJD *.sat *.*d *.*o P1* *.*i *.eph *.pyc *.Z")
        else:
          command = "/bin/rm -rf " + output_directory
          os.system(command)
          os.makedirs(output_directory)
          station_log = open(station_file, 'a')

    # First, get a list of all telescope positions
    if len(MSlabel) > 0:
      ant_table=casatab.table(MS.name()+'/ANTENNA');
      num_ant = ant_table.nrows();
      telescope_pos = ant_table.getcol("POSITION")
    print ('getting GPS station data for distance <', ionosphere_GPS_set_criteria.Max_Rec_Dist_From_Tele /1.0E3)
    GPS_receivers = \
        GPS_stations.get_stations_within_distance_2( 
            telescope_pos,
            ionosphere_GPS_set_criteria.Max_Rec_Dist_From_Tele)
    print ('initial number of potential GPS stations', len(GPS_receivers))
    print ('Ionosphere Bias_Fit_Type ', ionosphere_GPS_set_criteria.Bias_Fit_Type)
    
    if(ionosphere_GPS_set_criteria.Bias_Fit_Type>=2):
        print ("Adding global stations to improve bias fitting")
        GPS_receivers = GPS_stations.add_global_stations_to_list(GPS_receivers)
    potential_number_GPS = len(GPS_receivers)
    print ('potential number of GPS stations', potential_number_GPS)
# convert strings to MJD
    if start_time > 0 and end_time > 0:
      print ('*** provided start and end time are ',start_time, end_time)
      Obs_Start = start_time
      Obs_End = end_time
    else:
      st, et = get_observation_time_range(MS)
      Obs_Start = st / 86400.0
      Obs_End = et / 86400.0
    
    print ('Obs start and end time ',Obs_Start, Obs_End)

    # Ok, start initializing the GPS data area
    if(Iono_agw.clear_everything()):
        raise RuntimeError("could not clear Iono_agw")
    else:
        print ('^^^^^^^^^ memory should have been cleared !!!')
    if(Iono_agw.cal_observations_init(len(GPS_receivers))):
        raise RuntimeError("could not init Iono_agw")
    fit_type = int(processing_option[4:6])
    if(Iono_agw.cal_observations_set_parameters(
        fit_type,
        ionosphere_GPS_set_criteria.Max_Sat_Sky_Angle,
        ionosphere_GPS_set_criteria.Min_Sat_Elev,
        ionosphere_GPS_set_criteria.Max_Rec_Dist_From_Tele,
        ionosphere_GPS_set_criteria.Max_Iono_Pierce_Dist,
        ionosphere_GPS_set_criteria.Default_Iono_Height,
        ionosphere_GPS_set_criteria.Averaging_Time_Half_Width,
        ionosphere_GPS_set_criteria.Num_Ionosphere_Parameters,
        ionosphere_GPS_set_criteria.Num_Ionosphere_Heights,
        ionosphere_GPS_set_criteria.Num_Time_Terms,
        ionosphere_GPS_set_criteria.Theo_Model_Type,
        ionosphere_GPS_set_criteria.Bias_Fit_Type)):
        raise RuntimeError("could not set criteria for Iono_agw")
    MJD_array = None
    sat_XYZ   = None
    receiver_count = 0
    SIG_pos = Albus_RINEX_2._DATA_POS['SIGMA']
    PL_pos = Albus_RINEX_2._DATA_POS['STECPL']
    for i in range(len(GPS_receivers)):
        counter = i
        GPS = GPS_receivers[i]
        print ("Getting data from receiver %d/%d   '%s'"%(i,len(GPS_receivers),GPS[0]),MJD_array,Obs_Start,Obs_End)
        if do_serial and len(valid_stations) > 0:
          if not GPS[0] in valid_stations:
            print ('no data for station ', GPS[0], ' so skipping')
            continue
        print ('Obs_start Obs_end ', Obs_Start, Obs_End)
        result = get_GPS_data(i,GPS_receivers,Obs_Start,Obs_End, MJD_array,sat_XYZ,output_directory,overwrite,raise_bias_error)
        if result[0] == i:
          if(receiver_count == 0):
            MJD_array = result[2]
            if(Iono_agw.cal_observations_set_times(
                len(MJD_array), MJD_array)):
                raise RuntimeError("could not set MJDs for Iono_agw")
            sat_XYZ = result[6]
            sh = sat_XYZ.shape
            if(Iono_agw.cal_observations_set_sat_pos(
                sh[0], sh[1], sh[2], sat_XYZ)):
                raise RuntimeError("could not set sat pos for Iono_agw")
          receiver_count += 1
          result = process_GPS_data(result,SIG_pos,PL_pos)
          if do_serial == 0:
            print (GPS[0], file=station_log)
            break   # we should now be able to process remaining stations in parallel

    if do_serial == 0:
      print ('*********** finished initialization!!! ***************')
      if DEBUG_SET:
         print (' initial counter value ', counter)
      # collect data in parallel
      NUMBER_OF_PROCESSES = num_processors
      TASKS = [(get_GPS_data, (i,GPS_receivers,Obs_Start, Obs_End, MJD_array, sat_XYZ, output_directory,overwrite,raise_bias_error)) for i in range(counter+1,len(GPS_receivers))]
#     TASKS = [(get_GPS_data, (i,GPS_receivers,Obs_Start, Obs_End, MJD_array, sat_XYZ, output_directory,overwrite)) for i in range(counter+1,50)]

      if DEBUG_SET:
        print ('number of tasks ', len(TASKS))

   # Create queues
      task_queue = Queue()
      done_queue = Queue()

      # Submit tasks
      for task in TASKS:
        task_queue.put(task)

      # Start worker processes
      for i in range(NUMBER_OF_PROCESSES):
        Process(target=worker, args=(task_queue, done_queue)).start()

      # Get and print results
      result_sum = 0
      for i in range(len(TASKS)):
        # Put a 300 sec (5 min) timeout when retrieving stuff from queue.
        # Needed in case some get_GPS_data function hangs somewhere and
        # never returns - can happen, but luckily rarely.
        try:
          result = done_queue.get(timeout=300)
          result_sum = result_sum + 1
          if result[0] > -1:
            if DEBUG_SET:
              print ('good result_sum  ', result_sum, 'station is ', result[1][0])
            print ('get_GPS data OK so calling process_GPS_data for sequence and station = ', result[0], result[1][0])
            print (result[1][0], file=station_log)
            result = process_GPS_data(result,SIG_pos,PL_pos)
            receiver_count += 1
          else:
            if DEBUG_SET:
              print ('bad result_sum', result_sum, 'station is ', result[1])
              print ('get_GPS_data failed for station ', result[1])
        except:
          print (' ')
          print ('****** TIMEOUT in Get_GPS_data queue !!!')
          print ('****** Assuming queue is empty or there is a hangup with some data retrieval')
          print (' ')
          break

      # Tell child processes to stop
      for i in range(NUMBER_OF_PROCESSES):
        task_queue.put('STOP')
 
      print ('*********** finished collecting station data !!! ***************')

      # explicitly close list of usable GPS stations as soon as possible
      # otherwisr have to wait till entire script finishes
      station_log.close()

    if(receiver_count == 0):
        # somehow we have no good GPS receivers with data.  bail
        raise RuntimeError("could not find any good GPS receivers for Iono_agw")
    if(Iono_agw.cal_observations_init2()):
        raise RuntimeError("could not correct bias levels in Iono_agw")


    return receiver_count, potential_number_GPS, station_file

#############################################################################

def setup_AlbusIonosphere_for_ref_date(log, MSname="",MSdir=".",Lat=0, Long=0, Height=0,start_time="",end_time="", object="",time_step=300.0,telescope_pos=None,station_pos_x=0,station_pos_y=0,station_pos_z=0, max_dist=700E3, processing_option="MO_PIM",tolerance=0.1,overwrite=0,do_serial=0,raise_bias_error=0,num_processors=2,use_pim=1,use_global_data=0, gps_data_directory="."):

# Inputs:
# MSname - name of Measurement Set (MS) to use
# MSdir  - name of directory where MS is stored. Default = current
# station_pos - XYZ location of antenna. Default = get mean position of array from MS
# processing_option - type of solution. Default = MO_PIM
# tolerance - not sure what changing this will actually do. Default = 0.1
# overwrite - download new data if previous data exists. Default = do not dowmload
# do_serial - get GPS data sequentially. Default -  get GPS data in parallel
# raise_bias_error - ignore data with no GPS bias correction. Default - use bad bias data
# num_processors - number of sub-tasks to run when getting data in parallel. Default = 2
# max_dist = maximum distance (in metres) from specified location to seach for GPS stations
# use_global_data - 1: include data from global GNSS network, 0: ignore data from global network
# use_pim - 1: use PIM model in fit (default) 0: use IRI model in fit

# gps_data_directory - directory where the GPS data is to be stored. Default = current one.

# Lat - latitude of the observatory
# Long - longitude of the observatory
# Height - height of the observatory
# If latitude and longitude are specified, then these locations will be used 
# find the location of the observing site
#
# start_time - start date and time of the observation (string)
# end_time - enddate and time of the observation (string)
# if these two parameters are specified then the dates are not obatined from a
# measurement set
# time_step - seconds to increment in each cycle of ionosphere prediction
# object - name of what is being observed - can be left blank 

    
    lat_set = False # set to True if we derive Lat, Long, Ht in this function
    if len(object) > 0:
      print ('Observing ', object,file=log)
      print ('ALBUS data processing option:', processing_option)
    print ('ALBUS data processing option:', processing_option, file=log)
    if len(MSname) > 0:
      if HAS_PYRAP:
        MSlabel = MSname
        MSname= MSdir + '/' + MSname
        print ('Analysing Measurement Set:', MSname)
        print ('Analysing Measurement Set:', MSname,file=log)
        MS=casatab.table(MSname);
      else:
        print ('you must have pyrap installed in order to use data from ')
        print ('Measurement Sets. Unable to continue processing.')
        sys.exit(-1)
    else:
      MSlabel = ""
      MS = ""

    # Tell Albus stuff where the station is
    print ('extracting station_position')
    if telescope_pos is None:
      if station_pos_x== 0 and station_pos_y == 0 and station_pos_x ==0:
        if Lat == 0 and Long ==0:
          station_pos = get_mean_array_position(MS)
          print ('mean array station_pos',station_pos)
        else:
# convert latitude and longitude to ITRF X,Y,Z
          Long = ac.deg_str_to_rad(Long)
          Lat = ac.deg_str_to_rad(Lat)
          WGS84=1
          x,y,z = GPS_stations.cartesian_coord(Long, Lat, Height, WGS84)
          station_pos= numpy.zeros((1,3), dtype='float64')
          station_pos[0,0] = x
          station_pos[0,1] = y
          station_pos[0,2] = z
      else:
        station_length = len(station_pos_x)
        station_pos= numpy.zeros((station_length,3), dtype='float64')
        for i in range(station_length):
          station_pos[i,0] = station_pos_x[i]
          station_pos[i,1] = station_pos_y[i]
          station_pos[i,2] = station_pos_z[i]
    else:
      station_pos = telescope_pos
   
    print ('station_pos',station_pos)
    print ('station position',station_pos,file=log)

    try: 
      if len(start_time) > 0 and len(end_time) > 0:
        if HAS_PYRAP:
          start_time = qu.quantity(start_time).get_value()
          end_time = qu.quantity(end_time).get_value()
          print ('quantity start and end time ', start_time, end_time)
          st = start_time * 86400.0 - TIME_OFFSET
          et = end_time * 86400.0 +  TIME_OFFSET
          num_seconds = et - st
        elif HAS_EPHEM:
          print('EPHEM start and end times ', start_time, ' ', end_time)
          start_time = ephem.julian_date(ephem.Date(start_time)) - 2400000.5
          end_time = ephem.julian_date(ephem.Date(end_time)) - 2400000.5
          print ('ephem start and end time ', start_time, end_time)
          st = start_time * 86400.0 - TIME_OFFSET
          et = end_time * 86400.0 +  TIME_OFFSET
          num_seconds = et - st
          print('number of seconds', num_seconds)
        else:
          print ('It appears that neither PyRap nor PyEphem is installed!')
          print ('Unable to extract start and end times. Exiting')
          return 
        start_time = st / 86400.0
        end_time = et / 86400.0
      else:
        # must have failed to specify time somewhere
        start_time_quantity = 0
        start_time = 0
        end_time = 0
        st = 0
        et = 0
        num_seconds = 0
    except: 
      print ('**** exception - possibly caused by giving PyEphem wrong date format')
      print ('PyEphem requires dates/times to be in format yyyy/mo/da hh:mm:ss')
      print ('start_time is ', start_time)
      print ('end_time is ', end_time)

# time in MJD seconds
      st = start_time - TIME_OFFSET
      et = end_time +  TIME_OFFSET
      start_time = st/(24 * 3600)
      end_time = et/(24 * 3600)
      num_seconds = et - st
    
    ionosphere_GPS_set_criteria.Max_Rec_Dist_From_Tele = max_dist
    print ('maximum search radius for GPS stations from telescope (km) ', 0.001 * ionosphere_GPS_set_criteria.Max_Rec_Dist_From_Tele,file=log)
#   print ('maximum search radius for GPS stations from telescope (km) ', 0.001 * ionosphere_GPS_set_criteria.Max_Rec_Dist_From_Tele)

    if(processing_option == 'MO_IRI'):
        retval = Iono_agw.set_ionosphere_IRI(tolerance,tolerance*100.0)
    elif(processing_option == 'MO_PIM'):
        retval = Iono_agw.set_ionosphere_PIM(tolerance,tolerance*100.0)
    elif(processing_option in PROCESSING_OPTION_RINEX_GROUPS):
        if use_global_data == 1:
           ionosphere_GPS_set_criteria.Bias_Fit_Type = 3
        else:
           ionosphere_GPS_set_criteria.Bias_Fit_Type = 1
        if use_pim == 1:
           ionosphere_GPS_set_criteria.Theo_Model_Type = 3
        else:
           ionosphere_GPS_set_criteria.Theo_Model_Type = 1
        print ('starting ionosphere_GPS_set_criteria.Bias_Fit_Type ', ionosphere_GPS_set_criteria.Bias_Fit_Type)
        print ('starting ionosphere_GPS_set_criteria.Theo_Model_Type ', ionosphere_GPS_set_criteria.Theo_Model_Type)
        num_receivers, potential_num_receivers,stations_file = ionosphere_GPS_setup_GPS(MS,MSlabel, processing_option,overwrite=overwrite,start_time=start_time, end_time=end_time,do_serial=do_serial,raise_bias_error=raise_bias_error,num_processors=num_processors, telescope_pos=station_pos, output_directory=gps_data_directory)
        print ('Potential number of RINEX receivers for GPS fit ', potential_num_receivers,file=log)
        print ('Final number of receivers for GPS fit ', num_receivers,file=log)
        actual_valid_stations = get_valid_stations(stations_file)
        print ('Valid receivers used:',file=log)
        list_size = len(actual_valid_stations)
        step_size = 7
        loc =   0
        while (loc < list_size):
           print(actual_valid_stations[loc:loc+step_size],file=log)
           loc= loc+step_size
        retval = Iono_agw.set_ionosphere_GPS(tolerance,tolerance*100.0)
    print ('setup_AlbusIonosphere_for_ref_date time step is ', time_step)
    if time_step != 300.0:
      retval = Iono_agw.set_time_step(time_step)
    if len(MSname) > 0:
      (year_ref,month_ref,day_ref,hr_ref,min_ref,sec_ref) = \
        get_observation_year_month_day_hms(MS)
    else:
      print ('converting this start time in seconds to Y,M,D', st)
      (year_ref,month_ref,day_ref,hr_ref,min_ref,sec_ref) = \
         obtain_observation_year_month_day_hms(st)
    print ('reference time for rel_time=0: year_ref,month_ref,day_ref,hr_ref,min_ref,sec_ref',year_ref,month_ref,day_ref,hr_ref,min_ref,sec_ref)
    reference_start_MJD = jma_tools.get_MJD_hms(year_ref,month_ref,day_ref,hr_ref,min_ref,sec_ref)
        
    retval = Iono_agw.set_reference_time(year_ref,
                                                month_ref,
                                                day_ref,
                                                hr_ref,
                                                min_ref,
                                                sec_ref)
    if(retval < 0):
        raise RuntimeError("Error: Iono_agw.set_reference_time gave %d"%retval)

    if len(MSname) > 0 and station_pos_x== 0 and station_pos_y == 0 and station_pos_x ==0 and Lat == 0 and Long ==0:
      retval = Iono_agw.set_station_position(station_pos[0],station_pos[1],station_pos[2])
      print ('set ground station position to', station_pos[0], station_pos[1], station_pos[2])
      print ('setting observation ground station position to', station_pos[0], station_pos[1], station_pos[2],file=log)
    else:
      x_stn_mean = numpy.mean(station_pos[:,0])
      y_stn_mean = numpy.mean(station_pos[:,1])
      z_stn_mean = numpy.mean(station_pos[:,2])
      retval = Iono_agw.set_station_position(x_stn_mean,y_stn_mean,z_stn_mean)
      print ('set ground station position to', x_stn_mean, y_stn_mean, z_stn_mean)
      print ('set ground station position to', x_stn_mean, y_stn_mean, z_stn_mean,file=log)
      if Lat == 0 and Long ==0:
         WGS84=1
         Long, Lat, Ht = GPS_stations.ellipsoidal_coord(x_stn_mean, y_stn_mean, z_stn_mean, WGS84)
         lat_set = True
         print ('we are setting longitude and latitude to ', Long, Lat, Ht)
    print ("station Pos",retval)
    if start_time == 0:
      start_time, num_seconds, st, et = get_observation_start_end(MS)
    print ('reference time for rel_time=0: year,month,day,hr,min,sec ',year_ref,month_ref,day_ref,hr_ref,min_ref,sec_ref,file=log)
    print ('Measurement set actual integration centroid start and end times ', st, et,file=log)
    print ('times', start_time, num_seconds)
    Iono_agw.set_scan_times(0, num_seconds);
    if lat_set:
      return MS, start_time, end_time, lat_set, Lat, Long, Ht
    else:
      return MS, start_time, end_time, lat_set, None, None, None



# script to generate ionosphere corrections for CHIME

def process_chime_ionosphere(Az=180.0, El=90.0, Lat=0, Long=0, Height=0,start_time="",end_time="", object="",time_step=300.0,telescope_pos=None,station_pos_x=0,station_pos_y=0,station_pos_z=0,processing_option="MO_PIM",tolerance=0.1,overwrite=0,do_serial=0,do_plot=0,raise_bias_error=0, max_dist=700E3, num_processors=2,use_pim=1,use_global_data=0,gps_data_directory="."):

# Inputs:
# Az     - azimuth, north through east. Default = derive from Ra and Dec for tracking interferometer 
# El     - elevation, Default = 90 deg for CHIME observations
# station_pos - XYZ location of antenna. Default = get mean position of array from MS
# processing_option - type of solution. Default = MO_PIM
# tolerance - not sure what changing this will actually do. Default = 0.1
# overwrite - download new data if previous data exists. Default = do not dowmload
# do_serial - get GPS data sequentially. Default -  get GPS data in parallel
# num_processors - number of sub-tasks to run when getting data in parallel. Default = 2, which is OK for core2duo machine
# use_pim - 1 Use PIM model (default) in fit, 0: Use IRI model
# use_global_data - 1: include data from global GNSS network, 0: ignore data from global network
# gps_data_directory - directory where the GPS data is to be stored. Default = current one.

# Lat - latitude of the observatory
# Long - longitude of the observatory
# Height - height of the observatory
# If latitude and longitude are specified, then these locations will be used 
# find the location of the observing site. Geodetic (WGS84) latitude is assumed
#
# start_time - start date and time of the observation (string)
# end_time - enddate and time of the observation (string)
# if these two parameters are specified then the dates are not obtained from a
# measurement set
# time_step - seconds to increment in each cycle of ionosphere prediction
# object - name of what is being observed - can be left blank 
# raise_bias_error - if set to 1, will ignore all GPS observations where bias 
#   has not been corrected for

    os.system('date')
    process_start = time.time()
    startime = time.strftime("%a, %d %b %Y %H:%M:%S", time.localtime())
    print ("process_chime_ionosphere Start at %s" % startime)

    # Measurement Sets not being used for CHIME so set to blank
    MSname = ''
    MSdir = ''

    if len(MSname) == 0:
        print ('*************')
        print (' No measurement set specified - assuming that specifications')
        print (' can be obtained from command line')
        print ('*************')

    if do_serial:
      if len(MSname) > 0:
        out_file = 'albus_report_serial_' + processing_option + '_'+ MSname
      else:
        out_file = 'albus_report_serial_' + processing_option
    else:
      if len(MSname) > 0:
        out_file = 'albus_report_parallel_' + processing_option  + '_'+ MSname
      else:
        out_file = 'albus_report_parallel_' + processing_option

    if len(object) > 0:
      out_file = out_file + '_' + object

    # get rid of uppercase characters because MeqTrees PyNodes cannot
    # handle them
    # out_file = out_file.lower()

    if os.path.exists(out_file):
      os.remove(out_file)
    log = open(out_file, 'a')

    MS,start_time,end_time, lat_set,Lat1,Long1,Ht1 = setup_AlbusIonosphere_for_ref_date(log,MSname,MSdir,Lat,Long,Height,start_time,end_time, object,time_step,telescope_pos,station_pos_x,station_pos_y,station_pos_z, max_dist, processing_option,tolerance,overwrite,do_serial,raise_bias_error,num_processors,use_pim,use_global_data, gps_data_directory)
# can we use more than two processors?
    if num_processors <= 2:
      try:
        import multiprocessing
        processors =  multiprocessing.cpu_count()
        if processors > num_processors:
          num_processors = processors
          print ('*** setting number of processors to',num_processors) 
      except:
        pass

    if lat_set:
      Lat = math.degrees(Lat1)
      Long = math.degrees(Long1)
      Height = math.degrees(Ht1)
      Lat = Lat1
      Long = Long1
      Height = Ht1
      print ('setting Latitude, Longitude, Height to ', Lat, Long, Height)
      print ('setting Latitude, Longitude, Height to ', Lat, Long, Height,file=log)

    print ('time step ', time_step)
    print ('at location Lat, Long, height ', Lat, Long, Height)
    loc_time = start_time * 86400.0 - time_step
    if HAS_PYRAP:
      me=measures();
      try:
        p = me.position('WGS84',str(Long)+'rad', str(Lat)+'rad', str(Height) +'m')
      except:
        Long = ac.deg_str_to_rad(Long)
        Lat = ac.deg_str_to_rad(Lat)
        p = me.position('WGS84',str(Long)+'rad', str(Lat)+'rad', str(Height) +'m')
      me.do_frame(p);
    elif HAS_EPHEM:
      observer = ephem.Observer()
      observer.lat = Lat
      observer.lon = Long
      observer.elevation = Height
      observer.pressure = 0.0
      # convert to Dublin Julian Date for PyEphem and 
      # subtract 300 sec for first integration because of offset 
      # in data_calibrator.cxx code
      print ('start_time dub time ', start_time, start_time - 15019.5) 
      observer.date =  start_time - 15019.5 - time_step/86400.0

    TASKS = []
    numi=Iono_agw.get_Num_Ionospheric_Predictions()
    print ('*** number of predictions: ', numi)
    # make sure global location lists are reset to empty
    for k in range(numi):
      location_ra = []
      location_dec = []
      location_az = []
      location_el = []
      if HAS_PYRAP:
        location_time =  str(loc_time) + 's'
        print ('starting location_time ', location_time)
        t=me.epoch("UTC",qu.quantity(location_time));
        me.do_frame(t);
        print ('pyrap time ', t)
        Az = 0.0
        az=str(Az)+'deg';
        for i in range(5,95,5):
          el=str(i)+'deg';
          obsdir=me.direction('AZELGEO',az,el)
#         obsdir=me.direction('AZEL',az,el)
          radec = me.measure(obsdir,'J2000');
          ra=radec['m0']['value'];
          dec=radec['m1']['value'];
#         print ('k, i pyrap ra, dec ', k, obsdir, math.degrees(ra),math.degrees(dec))
          location_ra.append(ra)
          location_dec.append(dec)
          location_az.append(Az)
          location_el.append(i)
        Az = 180.0
        az=str(Az)+'deg';
        for i in range(85,0,-5):
          el=str(i)+'deg';
          obsdir=me.direction('AZELGEO',az,el)
#         obsdir=me.direction('AZEL',az,el)
          radec = me.measure(obsdir,'J2000');
          ra=radec['m0']['value'];
          dec=radec['m1']['value'];
#         print ('k, i pyrap ra, dec ', k, i, math.degrees(ra),math.degrees(dec))
          location_ra.append(ra)
          location_dec.append(dec)
          location_az.append(Az)
          location_el.append(i)
        loc_time =  loc_time + time_step
      else:
        print ('observer contents', observer)
        Az = 0.0
        az = str(Az)
        for i in range(5,95,5):
          el = str(i)
          print ('az el ', az, el)
          ra,dec = observer.radec_of(az, el)
#         print ('observing at a fixed azimuth and elevation (deg) of', az,el)
          ra_float = float(ra)
          dec_float = float(dec)
#         print ('observing at a corresponding ra and dec of', ra_float, dec_float)
          location_ra.append(ra_float)
          location_dec.append(dec_float)
          location_az.append(Az)
          location_el.append(i)
        Az = 180.0
        az = str(Az)
        for i in range(85,0,-5):
          el = str(i)
          print ('az el ', az, el)
#         print ('observer.date', observer.date)
          ra,dec = observer.radec_of(az, el)
#         print ('observing at a fixed azimuth and elevation (deg) of', az,el)
          ra_float = float(ra)
          dec_float = float(dec)
#         print ('observing at a corresponding ra and dec of', ra_float, dec_float)
          location_ra.append(ra_float)
          location_dec.append(dec_float)
          location_az.append(Az)
          location_el.append(i)
        observer.date = observer.date + time_step / 86400.0
      TASKS.append((predict_chime_iono_corrections, (k,location_ra,location_dec,location_az,location_el)))

    # do ionosphere predictions in parallel
    NUMBER_OF_PROCESSES = num_processors
#   NUMBER_OF_PROCESSES = 1

   # Create queues
    task_queue = Queue()
    done_queue = Queue()

  # Submit tasks
    for task in TASKS:
      task_queue.put(task)

    # Start worker processes
    for i in range(NUMBER_OF_PROCESSES):
      Process(target=iono_worker, args=(task_queue, done_queue)).start()

    report_list = {}
    # Get and print results
    for i in range(len(TASKS)):
      result = done_queue.get()
      print ('caught result',  result[0])
      report_list[result[0]] = result[1:]

    # Tell child processes to stop
    for i in range(NUMBER_OF_PROCESSES):
      task_queue.put('STOP')
    print ('*********** finished ionosphere predictions ***************')

    print ('     ',file=log)
    print ('ALBUS report column explanation (zero relative):',file=log)
    print ('0 - sequence number (zero relative)',file=log)
    print ('1 - separator colon',file=log)
    print ('2 - value of 0 - valid data/calculation',file=log)
    print ('    value of 1 - invalid data/calculation (usually, but not always, because elevation < 0 )',file=log)
    print ('3 - relative time in seconds relative to reference time',file=log)
    print ('4 - time step between sequence calculations (default is 300 sec)',file=log)
    print ('5 - elevation in degrees',file=log)
    print ('6 - azimuth in degrees',file=log)
    print ('7 - TEC (in tec units) in the current azimuth/elevation direction',file=log)
    print ('8 - Rotation Measure (radians/m^2) in the current azimuth/elevation direction',file=log)
    print ('9 - correction factor to convert STEC to value at the zenith',file=log)
    print ('10 - formal error in STEC',file=log)
    print ('     ',file=log)

    # report results
    num_seq = len(report_list)
    print ('length of report_list', num_seq)
    first_time = True
    for i in range(num_seq):
      data_list = report_list[i]
      num_to_report = len(data_list)
      if first_time:
        iono_array_STEC = numpy.zeros((num_to_report,num_seq),numpy.float32)
        iono_array_RM = numpy.zeros((num_to_report,num_seq),numpy.float32)
        print ('ionosphere array dimensions: ', iono_array_STEC.shape, file=log)
        print (' ', file=log)
        print ('seq  rel_time time_width El         Az         STEC           RM (rad/m2)   VTEC factor   STEC_error', file=log)
        first_time = False
      for j in range(num_to_report):
        if j == 0:
          print (' ', file=log)
        data = data_list[j]
        try:
#         print (i,':',data[0],data[1], data[2], data[3], data[4],data[5], data[6], data[7], data[8], file=log)
          print (i,':',data[0],data[1], data[2], math.degrees(data[3]), math.degrees(data[4]),data[5], data[6], data[7], data[8], file=log)

          if data[0] == 0:
            iono_array_STEC[j,i] = data[5]
            iono_array_RM[j,i] = data[6]
        except:
          print ('*** failure to extract from data_list! location: ',i,j)
          pass
    print (' ')
#   hdu = astropyfits.PrimaryHDU(numpy.fliplr(iono_array_STEC))
    hdu = astropyfits.PrimaryHDU(iono_array_STEC)
  # note: defining M as the fastest moving axis (FITS uses
  # FORTRAN-style indexing) produces an image that when
  # viewed with kview / ds9 etc looks correct on the sky 
  # with M increasing to top and L increasing toward left
  # of displayed image
    hdu.header['CTYPE1'] = ('RA---SIN')
    hdu.header['CDELT1'] = (5.0, 'in degrees')
    hdu.header['CRPIX1'] = (1, 'reference pixel (one relative)')
#   hdu.header['CRVAL1'] = (0.0, 'Reference RA')
    hdu.header['CUNIT1'] = ('deg     ')

    hdu.header['CTYPE2'] = ('DEC--SIN')
    hdu.header['CDELT2'] = (5.0, 'in degrees')
    hdu.header['CRPIX2'] = (1, 'reference pixel (one relative)')
    hdu.header['CRVAL2'] = (0.0, 'Reference DEC')
    hdu.header['CUNIT2'] = ('deg     ')

#   hdu.header['CTYPE3'] = ('STOKES')
#   hdu.header['CDELT3'] = (1)
#   hdu.header['CRPIX3'] = (1)
#   hdu.header['CRVAL3'] = (1)
#   hdu.header['CTYPE4'] = ('FREQ')
#   hdu.header['CDELT4'] = (1, 'in Hz')
#   hdu.header['CRPIX4'] = (1, 'in pixels (one relative)')
#   hdu.header['CRVAL4'] = (freq, 'Reference frequeny in Hz')
#   hdu.header['CPLX'] = (0, 'false as data is real ')
#   hdu.header['CELLS'] = (1, 'true as we want cells')
#   hdu.header['RESTFRQ'] = (freq, 'Rest frequency (Hz)')
    hdu.header['EQUINOX'] = (2000.0, 'Assumed equinox')

#   date_list = qu.quantity(str(start_time)+'s').formatted("YMD").split("/")
#   year = date_list[0]
#   month = date_list[1]
#   day = date_list[2]
#   obs_date = year + '-' + month + '-' + day

#   hdu.header['DATE-OBS'] = (obs_date, 'start of observation')

  # create initial HDUList
    hdulist = astropyfits.HDUList([hdu])

  # create output FITS file name
    if len(object) > 0:
     outfile = object +'_STEC.FITS'
    else:
     outfile = 'albus_STEC.FITS'
    print('outfile has name ', outfile)
  # delete any previous file
    if os.path.exists(outfile):
      print('deleting previous version of ', outfile)
      os.remove(outfile)
    hdulist.writeto(outfile)

    hdu = astropyfits.PrimaryHDU(iono_array_RM)
    hdu.header['CTYPE1'] = ('RA---SIN')
    hdu.header['CDELT1'] = (5.0, 'in degrees')
    hdu.header['CRPIX1'] = (1, 'reference pixel (one relative)')
#   hdu.header['CRVAL1'] = (0.0, 'Reference RA')
    hdu.header['CUNIT1'] = ('deg     ')

    hdu.header['CTYPE2'] = ('DEC--SIN')
    hdu.header['CDELT2'] = (5.0, 'in degrees')
    hdu.header['CRPIX2'] = (1, 'reference pixel (one relative)')
    hdu.header['CRVAL2'] = (0.0, 'Reference DEC')
    hdu.header['CUNIT2'] = ('deg     ')

#   hdu.header['CTYPE3'] = ('STOKES')
#   hdu.header['CDELT3'] = (1)
#   hdu.header['CRPIX3'] = (1)
#   hdu.header['CRVAL3'] = (1)
#   hdu.header['CTYPE4'] = ('FREQ')
#   hdu.header['CDELT4'] = (1, 'in Hz')
#   hdu.header['CRPIX4'] = (1, 'in pixels (one relative)')
#   hdu.header['CRVAL4'] = (freq, 'Reference frequeny in Hz')
#   hdu.header['CPLX'] = (0, 'false as data is real ')
#   hdu.header['CELLS'] = (1, 'true as we want cells')
#   hdu.header['RESTFRQ'] = (freq, 'Rest frequency (Hz)')
    hdu.header['EQUINOX'] = (2000.0, 'Assumed equinox')
    hdulist = astropyfits.HDUList([hdu])

  # create output FITS file name
    if len(object) > 0:
     outfile = object +'_RM.FITS'
    else:
     outfile = 'albus_RM.FITS'
    print('outfile has name ', outfile)
  # delete any previous file
    if os.path.exists(outfile):
      print('deleting previous version of ', outfile)
      os.remove(outfile)
    hdulist.writeto(outfile)

    stoptime = time.strftime("%a, %d %b %Y %H:%M:%S", time.localtime())
    print ("Stop at %s" % stoptime)
    process_end = time.time()
    duration = (process_end - process_start)/3600.0
    print ("Total run time: %7.2f hours" % duration)
    print ("Total run time: %7.2f hours" % duration,file=log)

    if do_plot:
      print (' ')
      size = iono_array_RM.shape[1]
      x1 = (numpy.arange(0, size) * 300.0 ) -300.0
      x1 = x1 /3600.0 -8.0

      import matplotlib
      import matplotlib.pyplot as plt

      plt.imshow(iono_array_RM,extent=(x1[0],x1[x1.shape[0]-1],90-2.5,90-177.5),cmap=plt.cm.jet,aspect="auto",interpolation='none')
      if len(object) > 0:
        outfile = object +'_RM'
      else:
        outfile = 'albus_RM'
      plt.colorbar()
      plt.title('ionosphere RM')
      plt.xlabel('time (hours)')
      plt.ylabel('elevation Sequence north to south in 5 deg steps')
      plt.savefig(outfile)
      plt.show()

      plt.imshow(iono_array_STEC,extent=(x1[0],x1[x1.shape[0]-1],90-2.5,90-177.5),cmap=plt.cm.jet,aspect="auto",interpolation='none')
      plt.colorbar()
      plt.xlabel('time (hours)')
      plt.ylabel('elevation Sequence north to south in 5 deg steps')
      plt.title('ionosphere STEC')
      if len(object) > 0:
        outfile = object +'_STEC'
      else:
        outfile = 'albus_STEC'
      plt.savefig(outfile)
      plt.show()

    print ('*********** finished task!! ***************')

# The following function should be the only one which needs changes that depend
# on the type of observation that one is doing

def process_ionosphere(MSname="",
                       MSdir=".",
                       Ra=0,
                       Dec=0,
                       Az=180.0,
                       El=-90.0,
                       Lat=0,
                       Long=0,
                       Height=0,
                       start_time="",
                       end_time="",
                       object="iono",
                       time_step=300.0,
                       telescope_pos=None,
                       station_pos_x=0,
                       station_pos_y=0,
                       station_pos_z=0,
                       max_dist=700E3,
                       processing_option="RI_G03",
                       tolerance=0.1,
                       overwrite=0,
                       do_serial=0,
                       num_processors=4,
                       use_pim=1,
                       use_global_data=0,
                       gps_data_directory=".",
                       special_body=None):

# Inputs:
# MSname - name of Measurement Set (MS) to use
# MSdir  - name of directory where MS is stored. Default = current
# Ra     - Right Ascension of field centre. Default = get it from MS
# Dec    - Declination of field centre. Default = get it from MS
# Az     - azimuth, north through east. Default = derive from Ra and Dec for tracking interferometer 
# El     - elevation, Default = derive from Ra and Dec for tracking interferometer 
# special_body - Name of special body to override ra dec ephemeris of database.
#                This should be a body as specified by PyEphem.
#                If specified none of Ra, Dec, Az or El will have any effect
# station_pos - XYZ location of antenna. Default = get mean position of array from MS
# processing_option - type of solution. Default = MO_PIM
# tolerance - not sure what changing this will actually do. Default = 0.1
# overwrite - download new data if previous data exists. Default = do not dowmload
# do_serial - get GPS data sequentially. Default -  get GPS data in parallel
# num_processors - number of sub-tasks to run when getting data in parallel. 
# Default = 2, which is OK for core2duo machine. Also see comments below for 
# Geosciences Australia
# max_dist = maximum distance in metres from telescope location to search for gps_stations
# use_pim - 1 Use PIM model (default) in fit, 0: Use IRI model
# use_global_data - 1: include data from global GNSS network, 0: ignore data from global network
# gps_data_directory - directory where the GPS data is to be stored. Default = current one.

# Lat - latitude of the observatory
# Long - longitude of the observatory
# Height - height of the observatory
# If latitude and longitude are specified, then these locations will be used 
# find the location of the observing site
#
# start_time - start date and time of the observation (string)
# end_time - enddate and time of the observation (string)
# if these two parameters are specified then the dates are not obtained from a
# measurement set
# time_step - seconds to increment in each cycle of ionosphere prediction
# object - name of what is being observed - can be left blank 

    
    os.system('date')
    process_start = time.time()
    startime = time.strftime("%a, %d %b %Y %H:%M:%S", time.localtime())
    print ("process_ionosphere Start at %s" % startime)

    raise_bias_error = 0  # All RINEX files have uncorrected biases!

    if len(MSname) == 0:
        print ('*************')
        print (' No measurement set specified - assuming that specifications')
        print (' can be obtained from command line')
        print ('*************')

    if do_serial:
      if len(MSname) > 0:
        out_file = 'albus_report_serial_' + processing_option + '_'+ MSname + '_' + object
      else:
        out_file = 'albus_report_serial_' + processing_option + '_' + object
    else:
      if len(MSname) > 0:
        out_file = 'albus_report_parallel_' + processing_option  + '_'+ MSname + '_' + object
      else:
        out_file = 'albus_report_parallel_' + processing_option + '_' + object 

    if os.path.exists(out_file):
      os.remove(out_file)
    log = open(out_file, 'a')

# setting num_processors == 1 seems to work better for getting GPS data from
# Geosciences Australia. Anyway most time for Australia data is spent at the
# bias / clock correction fitting stuff

    MS,start_time,end_time, lat_set,Lat1,Long1,Ht1 = setup_AlbusIonosphere_for_ref_date(log,MSname,MSdir,Lat,Long,Height,start_time,end_time, object,time_step,telescope_pos,station_pos_x,station_pos_y,station_pos_z,max_dist, processing_option,tolerance,overwrite,do_serial,raise_bias_error,num_processors,use_pim,use_global_data, gps_data_directory)

# can we use more than two processors for Ionosphere TEC / RM predictions?
    if num_processors <= 2:
      try:
        import multiprocessing
        processors =  multiprocessing.cpu_count()
        if processors > num_processors:
          num_processors = processors
          print ('*** setting number of processors to',num_processors) 
      except:
        pass
#   num_processors = 1
    if lat_set:
      Lat = math.degrees(Lat1)
      Long = math.degrees(Long1)
      Height = math.degrees(Ht1)
      Lat = Lat1
      Long = Long1
      Height = Ht1
      print ('setting Latitude, Longitude, Height to ', Lat, Long, Height)
      print ('setting Latitude, Longitude, Height to ', Lat, Long, Height, file=log)
    if special_body is not None:
      if HAS_EPHEM:
        observer = ephem.Observer()
        observer.lat = Lat 
        observer.lon = Long
        observer.elevation = Height
        observer.pressure = 0.0
        print ('observer contents', observer)
        print ('start_time dublin time ', start_time, start_time - 15019.5) 
        observer.date =  start_time - 15019.5 - time_step/86400.0
        fieldEphem = getattr(ephem, special_body, None)()
        if not fieldEphem:
          raise RuntimeError("Body {} not defined by PyEphem".format(special_body))
        print ('computing for special body', special_body)
        fieldEphem.compute(observer)
        direction = (fieldEphem.a_ra, fieldEphem.a_dec)
        retval = Iono_agw.set_source_position(direction[0],direction[1])
      else: # only provide this for ephem for now
        print ('pyephem not installed - cannot compute Ra and Dec', file=log)
        print ('pyephem not installed - cannot compute Ra and Dec')
        sys.exit(-1)
    elif Ra == 0 and Dec == 0 and len(MS) > 0:
      direction = get_observing_position(MS)
      print ('observation direction ', direction)
      if direction[0] < 0.0:
        direction_0 = 2 * math.pi - direction[0]
      else:
        direction_0 = direction[0]
      print ('observation direction ', math.degrees(direction_0), math.degrees(direction[1]), file=log)
      retval = Iono_agw.set_source_position(direction[0],direction[1])
    else:
      try:
        if El < 0.0:
          print('observation direction ', Ra, Dec, file=log)
          (Ra,Dec)=ac.radec_str_to_rad2(Ra,Dec)
        else:
          print ('observation in fixed Az El direction ', Az, El, file=log)
      except:
        if El < 0.0:
          print('observation direction ', Ra, Dec, file=log)
          (Ra,Dec)=ac.radec_str_to_rad2(Ra,Dec)
        else:
          print ('observation in fixed Az El direction ', Az, El, file=log)
      retval = Iono_agw.set_source_position(Ra, Dec)

    print ("source",retval)

    location_ra = []
    location_dec = []
    numi=Iono_agw.get_Num_Ionospheric_Predictions()
    print ('*** number of predictions: ', numi)
    # make sure global location lists are reset to empty
    if special_body:
      if HAS_EPHEM:
        observer = ephem.Observer()
        observer.lat = Lat 
        observer.lon = Long
        observer.elevation = Height
        observer.pressure = 0.0
        print ('observer contents', observer)
        print ('start_time dublin time ', start_time, start_time - 15019.5) 
        observer.date =  start_time - 15019.5 - time_step/86400.0
        fieldEphem = getattr(ephem, special_body, None)()
        if not fieldEphem:
          raise RuntimeError("Body {} not defined by PyEphem".format(special_body))
        print ('computing for special body', special_body)
        print ('computing for special body', special_body, file=log)
        fieldEphem.compute(observer)
        ra, dec = (fieldEphem.a_ra, fieldEphem.a_dec)
        print ('starting observation direction ', float(ra), float(dec))
        print ('starting observation direction ', float(ra), float(dec), file=log)
        for i in range(numi):
          fieldEphem.compute(observer)
          ra, dec = (fieldEphem.a_ra, fieldEphem.a_dec)
          location_ra.append(float(ra))
          location_dec.append(float(dec))
          observer.date = observer.date + time_step / 86400.0
        print ('ending observation direction ', float(ra), float(dec))
        print ('ending observation direction ', float(ra), float(dec), file=log)
      else: # only provide this for ephem for now
        print ('pyephem not installed - cannot compute Ra and Dec', file=log)
        print ('pyephem not installed - cannot compute Ra and Dec')
        sys.exit(-1)
    elif El >= 0.0:
      print ('time step ', time_step)
      print ('fixed azimuth and elevation are ', Az, El)
      print ('at location Lat, Long, height ', Lat, Long, Height)
#     HAS_PYRAP = False
      if HAS_PYRAP:
        me=measures();
        if type(Az)!=str:
            az=str(Az)+'deg';
        if type(El)!=str:
            el=str(El)+'deg';
        obsdir=me.direction('AZELGEO',az,el)
        print (Long, Lat, Height)
        try:
          p = me.position('WGS84',str(Long)+'rad', str(Lat)+'rad', str(Height) +'m')
        except:
          Long = ac.deg_str_to_rad(Long)
          Lat = ac.deg_str_to_rad(Lat)
          p = me.position('WGS84',str(Long)+'rad', str(Lat)+'rad', str(Height) +'m')
        me.do_frame(p);
        loc_time = start_time * 86400.0 - time_step
        location_time =  str(loc_time) + 's'
        print ('starting location_time ', location_time)
        t=me.epoch("UTC",qu.quantity(location_time));
        print ('pyrap time ', t)
#  myme.doframe(myme.epoch('mjd', qa.quantity(mjd, 'd')))
#   myme.doframe(myme.observatory(observatory))
#   myradec = myme.measure(mydir,'J2000')
        me.do_frame(t);
        for i in range(numi):
#         radec = me.measure(obsdir,'RADEC');
#  the following call gives the same result as the one above
          radec = me.measure(obsdir,'J2000');
          ra=radec['m0']['value'];
          dec=radec['m1']['value'];
          print ('i pyrap ra, dec ', i, math.degrees(ra),math.degrees(dec))
          location_ra.append(ra)
          location_dec.append(dec)
          loc_time = loc_time + time_step
          location_time =  str(loc_time) + 's'
          t=me.epoch("UTC",qu.quantity(location_time));
          me.do_frame(t);
      elif HAS_EPHEM:
        observer = ephem.Observer()
        observer.lat = Lat 
        observer.lon = Long
        observer.elevation = Height
        observer.pressure = 0.0
        print ('observer contents', observer)
        # convert to Dublin Julian Date for PyEphem and 
        # subtract 300 sec for first integration because of offset 
        # in data_calibrator.cxx code
        print ('start_time dublin time ', start_time, start_time - 15019.5) 
        observer.date =  start_time - 15019.5 - time_step/86400.0
        az = str(Az)
        el = str(El)
        print ('az el ', az, el)
        ra,dec = observer.radec_of(az, el)
        print ('observing at a fixed azimuth and elevation (deg) of', Az,El, file=log)
        print ('starting observation direction ', ra, dec)
        print ('starting observation direction ', ra, dec, file=log)
        print ('observing at a fixed azimuth and elevation (deg) of', Az,El)
        
        for i in range(numi):
          ra,dec = observer.radec_of(az, el)
#         dec = dec + math.radians(0.15)
          print('i ephem ra, dec ', i, math.degrees(ra),math.degrees(dec))
          ra_float = float(ra)
          dec_float = float(dec)
          location_ra.append(ra_float)
          location_dec.append(dec_float)
          observer.date = observer.date + time_step / 86400.0
      else:
        print ('pyephem not installed - cannot compute Ra and Dec', file=log)
        print ('pyephem not installed - cannot compute Ra and Dec')
        sys.exit(-1)
    else: # ordinary radec body
      pass
    # do ionosphere predictions in parallel
    NUMBER_OF_PROCESSES = num_processors
#   NUMBER_OF_PROCESSES = 1
    print ('for report using NUMBER_OF_PROCESSES', NUMBER_OF_PROCESSES)
    increment = numi / NUMBER_OF_PROCESSES
    times = []
    end_pos = 0
    start_pos = 0 - increment
    try:
      for i in range(NUMBER_OF_PROCESSES):
        start_pos = start_pos + increment
        print ('i start_pos', i, start_pos)
        if i == NUMBER_OF_PROCESSES-1:
          end_pos = numi
        else:
          end_pos = end_pos + increment
        print ('i end_pos', i, end_pos)
        times.append((start_pos,end_pos))
    except:
      pass
    print ('using times in tasks ', times)

    TASKS = [(predict_iono_corrections, (i,times,location_ra,location_dec)) for i in range(NUMBER_OF_PROCESSES)]
    print ('number of prediction tasks ', len(TASKS))

   # Create queues
    task_queue = Queue()
    done_queue = Queue()

    # Submit tasks
    for task in TASKS:
      task_queue.put(task)

    # Start worker processes
    for i in range(NUMBER_OF_PROCESSES):
      Process(target=iono_worker, args=(task_queue, done_queue)).start()

    report_list = {}
    # Get and print results
    for i in range(len(TASKS)):
      result = done_queue.get()
      report_list[result[0]] = result

    # Tell child processes to stop
    for i in range(NUMBER_OF_PROCESSES):
      task_queue.put('STOP')

    print ('*********** finished ionosphere predictions ***************')

    print ('     ',file=log)
    print ('ALBUS report column explanation (zero relative):',file=log)
    print ('0 - sequence number (zero relative)',file=log)
    print ('1 - separator colon',file=log)
    print ('2 - value of 0 - valid data/calculation',file=log)
    print ('    value of 1 - invalid data/calculation (usually, but not always, because elevation < 0 )',file=log)
    print ('3 - relative time in seconds relative to reference time',file=log)
    print ('4 - time step between sequence calculations (default is 300 sec)',file=log)
    print ('5 - elevation in degrees',file=log)
    print ('6 - azimuth in degrees',file=log)
    print ('7 - TEC (in tec units) in the current azimuth/elevation direction',file=log)
    print ('8 - Rotation Measure (radians/m^2) in the current azimuth/elevation direction',file=log)
    print ('9 - correction factor to convert STEC to value at the zenith',file=log)
    print ('10 - formal error in STEC',file=log)
    print ('     ',file=log)


    # report results
    print ('seq  rel_time time_width El         Az         STEC           RM (rad/m2)   VTEC factor   STEC_error', file=log)
    for i in range(NUMBER_OF_PROCESSES):
      data_list = report_list[i]
      num_to_report = len(data_list)
      for j in range(1,num_to_report):
        data = data_list[j]
        print (data[0],':',data[1],data[2], data[3], math.degrees(data[4]), math.degrees(data[5]),data[6], data[7], data[8], data[9], file=log)

    print (' ')
    print ('*********** finished task!! ***************')

    stoptime = time.strftime("%a, %d %b %Y %H:%M:%S", time.localtime())
    print ("Stop at %s" % stoptime)
    process_end = time.time()
    duration = (process_end - process_start)/60.0
    print ('process_ionosphere: total run time: %7.2f minutes' % duration)
    print ('process_ionosphere: total run time: %7.2f minutes' % duration, file=log)

#

#########################################################################
def get_askap_beam_locations( filename ):
# get ASKAP FPA phased up positions from ASCII file
# decode ASKAP file having lines with form: 0  (-0.450  0.450)  22:10:35.412,-44:49:50.70
# we want the last two coordinates

  text = open(filename, 'r').readlines()
# skip first line
# get actual data
  start = 1
  positions_ascii = []
  for i in range( start,len(text)):
    info = text[i].split(',')         # gets declination
    ra = info[0]
    dec = info[1]
    comment = info[2]
    positions_ascii.append([ra, dec, comment])
  print('positions ascii', positions_ascii)

  return positions_ascii

#########################################################################

# The following function should be the only one which needs changes that depend
# on the type of observation that one is doing

def process_ionosphere_multi_dir(MSname="",MSdir=".", Ra=0, Dec=0, Az=180.0, El=-90.0, Lat=0, Long=0, Height=0,start_time="",end_time="", object="iono",time_step=300.0,telescope_pos=None,station_pos_x=0,station_pos_y=0,station_pos_z=0,max_dist=350E3,processing_option="RI_G03",tolerance=0.1,overwrite=0,do_serial=0,raise_bias_error=0, num_processors=2,use_pim=1,use_global_data=0,gps_data_directory="./",positions_file=None):

# Inputs:
# MSname - name of Measurement Set (MS) to use
# MSdir  - name of directory where MS is stored. Default = current
# Ra     - Right Ascension of field centre. Default = get it from MS
# Dec    - Declination of field centre. Default = get it from MS
# Az     - azimuth, north through east. Default = derive from Ra and Dec for tracking interferometer 
# El     - elevation, Default = derive from Ra and Dec for tracking interferometer 
# station_pos - XYZ location of antenna. Default = get mean position of array from MS
# processing_option - type of solution. Default = MO_PIM
# tolerance - not sure what changing this will actually do. Default = 0.1
# overwrite - download new data if previous data exists. Default = do not dowmload
# do_serial - get GPS data sequentially. Default -  get GPS data in parallel
# num_processors - number of sub-tasks to run when getting data in parallel. Default = 2
# max_dist = maximum distance in metres from telescope location to search for gps_stations

# use_pim - 1 Use PIM model (default) in fit, 0: Use IRI model
# use_global_data - 1: include data from global GNSS network, 0: ignore data from global network
# gps_data_directory - directory where the GPS data is to be stored. Default = current one.
# positions_file - ascii data file of ASKAP focal plane array pointing positions

# Lat - latitude of the observatory
# Long - longitude of the observatory
# Height - height of the observatory
# If latitude and longitude are specified, then these locations will be used 
# find the location of the observing site
#
# start_time - start date and time of the observation (string)
# end_time - enddate and time of the observation (string)
# if these two parameters are specified then the dates are not obtained from a
# measurement set
# time_step - seconds to increment in each cycle of ionosphere prediction
# object - name of what is being observed - can be left blank 
# raise_bias_error - if set to 1, will ignore all GPS observations where bias 
#   has not been corrected for


# get start time
    os.system('date')
    process_start = time.time()
    startime = time.strftime("%a, %d %b %Y %H:%M:%S", time.localtime())
    print ("process_ionosphere Start at %s" % startime)

    raise_bias_error = 0          # all RINEX files have uncorrected biases!

    if positions_file is None:
        print ('*************')
        print (' No postions given, so exiting !! ')
        print ('*************')
        return


    if len(MSname) == 0:
        print ('*************')
        print (' No measurement set specified - assuming that specifications')
        print (' can be obtained from command line')
        print ('*************')


    if do_serial:
      if len(MSname) > 0:
        out_file = 'albus_report_serial_' + processing_option + '_'+ MSname + '_' + object
      else:
        out_file = 'albus_report_serial_' + processing_option + '_' + object
    else:
      if len(MSname) > 0:
        out_file = 'albus_report_parallel_' + processing_option  + '_'+ MSname + '_' + object
      else:
        out_file = 'albus_report_parallel_' + processing_option + '_' + object

    if os.path.exists(out_file):
      os.remove(out_file)

    log= open(out_file, 'w')

    MS,start_time,end_time, lat_set,Lat1,Long1,Ht1 = setup_AlbusIonosphere_for_ref_date(log,MSname,MSdir,Lat,Long,Height,start_time,end_time, object,time_step,telescope_pos,station_pos_x,station_pos_y,station_pos_z,max_dist,processing_option,tolerance,overwrite,do_serial,raise_bias_error,num_processors,use_pim,use_global_data, gps_data_directory)

    print (' ')
    print ('!!!!!!!!!!!!!!!!!!!!! finished setup_AlbusIonosphere_for_ref_date !!!!!!!!!!')
    print (' ')

# can we use more than two processors?
# necessary for ASKAP, as initial data collection seems to work best with
# just one processor
    if num_processors <= 2:
      try:
        import multiprocessing
        processors =  multiprocessing.cpu_count()
        if processors > num_processors:
          num_processors = processors
          print ('*** setting number of processors to',num_processors) 
      except:
        pass

    # We assume we are using a focal plane array and want to compute the TEC/RM
    # needed to correct observations in each of the beams
    # do ionosphere predictions in parallel
    numi=Iono_agw.get_Num_Ionospheric_Predictions()
    print ('*** number of predictions: ', numi)
    NUMBER_OF_PROCESSES = num_processors
    increment = numi / NUMBER_OF_PROCESSES
    times = []
    end_pos = 0
    start_pos = 0 - increment
    for i in range(NUMBER_OF_PROCESSES):
       start_pos = start_pos + increment
       if i == NUMBER_OF_PROCESSES-1:
         end_pos = numi
       else:
         end_pos = end_pos + increment
       times.append((start_pos,end_pos))
    
    print ('     ',file=log)
    print ('ALBUS report column explanation (zero relative):',file=log)
    print ('0 - sequence number (zero relative)',file=log)
    print ('1 - separator colon',file=log)
    print ('2 - value of 0 - valid data/calculation',file=log)
    print ('    value of 1 - invalid data/calculation (usually, but not always, because elevation < 0 )',file=log)
    print ('3 - relative time in seconds relative to reference time',file=log)
    print ('4 - time step between sequence calculations (default is 300 sec)',file=log)
    print ('5 - elevation in degrees',file=log)
    print ('6 - azimuth in degrees',file=log)
    print ('7 - TEC (in tec units) in the current azimuth/elevation direction',file=log)
    print ('8 - Rotation Measure (radians/m^2) in the current azimuth/elevation direction',file=log)
    print ('9 - correction factor to convert STEC to value at the zenith',file=log)
    print ('10 - formal error in STEC fit ',file=log)
    print ('     ',file=log)


# read in positions from data file
    max_j = 1
    try:
      positions_ascii  = positions_file
      print(' positions_ascii', positions_ascii)
      if len(positions_ascii) > 0:
        max_j = len(positions_ascii)
    except:
         print('file not found', positions_file)
         pass
    # 
    for j in range(max_j):
      location_ra = []
      location_dec = []
      if max_j == 1: 
        if Ra == 0 and Dec == 0 and len(MS) > 0: 
          direction = get_observing_position(MS)
          if direction[0] < 0.0:
            direction_0 = 2 * math.pi - direction[0]
          else:
            direction_0 = direction[0]
          print ('observation direction ', math.degrees(direction_0), math.degrees(direction[1]), file=log)
          retval = Iono_agw.set_source_position(direction[0],direction[1])
 
        else:
          try:
            if El < 0.0:
              print('observation direction ', Ra, Dec, file=log)
              (Ra,Dec)=ac.radec_str_to_rad2(Ra,Dec)
            else:
              print ('observation in fixed Az El direction ', Az, El, file=log)
          except:
            if El < 0.0:
              print('observation direction ', Ra, Dec, file=log)
              (Ra,Dec)=ac.radec_str_to_rad2(Ra,Dec)
            else:
              print ('observation in fixed Az El direction ', Az, El, file=log)
          retval = Iono_agw.set_source_position(Ra, Dec)

      else:
        try:
          (obs_ra, obs_dec)  = ac.radec_str_to_rad2(positions_ascii[j][0], positions_ascii[j][1])
          print ('positions_ascii ra dec ', obs_ra, obs_dec)
        except:
          pass
      location_ra.append(obs_ra)
      location_dec.append(obs_dec)

      print ('  ',file=log)
      print ('#######################################################',file=log)
      print ('observation:', j,' direction ', positions_ascii[j][0],positions_ascii[j][1], positions_ascii[j][2], file=log)
      print ('  ',file=log)
      print ('observation:', j,' direction ', positions_ascii[j][0],positions_ascii[j][1])

      TASKS = [(predict_iono_corrections, (i, times, location_ra, location_dec)) for i in range(NUMBER_OF_PROCESSES)]
      print ('number of prediction tasks ', len(TASKS))
      # Create queues
      task_queue = Queue()
      done_queue = Queue()

      # Submit tasks
      for task in TASKS:
        task_queue.put(task)

     # Start worker processes
      for i in range(NUMBER_OF_PROCESSES):
        Process(target=iono_worker, args=(task_queue, done_queue)).start()

      report_list = {}
     # Get and print results
      for i in range(len(TASKS)):
        result = done_queue.get()
        report_list[result[0]] = result

     # Tell child processes to stop
      for i in range(NUMBER_OF_PROCESSES):
        task_queue.put('STOP')
      print ('****** finished ionosphere predictions for ra and dec', math.degrees(location_ra[0]), math.degrees(location_dec[0]), '*******************')

     # report results
      print ('seq  rel_time time_width El         Az         STEC           RM (rad/m2)   VTEC factor   STEC_error',file=log)
      for i in range(NUMBER_OF_PROCESSES):
       data_list = report_list[i]
       num_to_report = len(data_list)
       for k in range(1,num_to_report):
         data = data_list[k]
         print (data[0],':',data[1],data[2], data[3], math.degrees(data[4]), math.degrees(data[5]),data[6], data[7], data[8], data[9],file=log)

      print (' ',file=log)
    print ('*********** finished task!! ***************')

# compute total run time
    stoptime = time.strftime("%a, %d %b %Y %H:%M:%S", time.localtime())
    print ("Stop at %s" % stoptime)
    process_end = time.time()
    duration = (process_end - process_start)/60.0
    print ('process_ionosphere: total run time: %7.2f minutes' % duration)
    print ('process_ionosphere: total run time: %7.2f minutes' % duration,file=log)
