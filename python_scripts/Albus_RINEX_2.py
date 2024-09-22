# Python stuff for dealing with Ionosphere RINEX stuff
# 2006 Jul 28  James M Anderson  --JIVE  start
# 2007 Feb 14  JMA  --revise RINEX reader to accept MJD check
# 2007 Apr 18  JMA  --updates for more satellite block position handling
# 2023 Mar  4  AGW  --updates for igs long product file names

global DEBUG_SET
DEBUG_SET = False

################################################################################
# some import commands  The User should not need to change this.
################################################################################


# satellite ephemeris files eph and sp3
# by gps week
# ephemeris eph and sp3 files
# http://igs.ifag.de/root_ftp/IGS/products/orbits/1383/

# ftp://igs.ensg.ign.fr/pub/igs/products/1172
# ftp://garner.ucsd.edu/pub/products/1172/

################################################################################
# miscellaneous stuff
import copy, optparse, os, sys
import re, string
import numpy as np
import inspect
import warnings
import math
import time as systime

################################################################################
# JMA's ionosphere stuff
import AlbusIonosphere
import Albus_Coordinates
import Albus_RINEX
import jma_tools
import GPS_stations








################################################################################
# Global variables
SECONDS_PER_DAY = 24.0*60.0*60.0
DAYS_PER_SECOND = 1.0/SECONDS_PER_DAY
M_DEG2RAD = math.pi/180.0
M_RAD2DEG = 180.0/math.pi

RADIUS_EARTH = 6378137.0  # in m





# See JMA Notes from 2006 Jan 10 and some of Bob Campbell's notes
SPEED_OF_LIGHT = 299792458.0    # m s^{-1}
IONOSPHERE_Kp_2 = 80.6163848    # m^3 s^{-2}
IONOSPHERE_KB = 2.79924925E10   # s^{-1} T^{-1}






# Note, if you add 'C2' to this list, check the code in read_RINEX_obs_file
# which checks for C, P, L, and S data, and makes C2 a pass
_DATA_POS_LIST = ['C1','P1','P2','L1','L2','L3','L4','L5','L6','L7','L8','S1','S2','SF1','SF2','LL',
                  'STECP', 'SIGMA', 'STECL', 'EL', 'AZ', 'STECPL']

_DATA_POS = {}
_DATA_POS_SIZE = len(_DATA_POS_LIST)
for i in range(_DATA_POS_SIZE): _DATA_POS[_DATA_POS_LIST[i]] = i
assert(not('C2' in _DATA_POS))
MAX_POSSIBLE_SATELLITES = 300 # GPS 0--99, GLONASS 100-199, Galileo 200--299
BAD_DATA_CODE = -999.0
SKIP_DATA_CODE = -998.0
MAX_SPIP_POINTS = 7            # How many time points may be skipped?
MIN_CONTIGUOUS_POINTS_GOOD = 5 # How many consecutive points must be present to
                               # use the phase data?
assert(MIN_CONTIGUOUS_POINTS_GOOD >= 2)
# The GPS satellites are in 20 000 km orbits.  Any pseudorange distance
# significantly less than that is obviously garbage for ground-based receivers.
# But sometimes the receivers are really bleeped up.  Don't trust small distances
MIN_PSEUDORANGE_DISTANCE = 5.0E6 # in meters
MAX_PSEUDORANGE_DIFFERENCE = 1.0E4 # in meters
assert(MIN_PSEUDORANGE_DISTANCE > BAD_DATA_CODE)

TECU_TO_M_N2 = 1.0E16 # convert from TECU to m^{-2}


# Satellite specifics
############# GPS
nu_L1_GPS = 1575.42E6  # in Hz
nu_L2_GPS = 1227.60E6  # in Hz
freq_factor_GPS = nu_L1_GPS*nu_L1_GPS*nu_L2_GPS*nu_L2_GPS \
                  / (nu_L1_GPS*nu_L1_GPS - nu_L2_GPS*nu_L2_GPS)
# STEC_factor converts distance difference to TEC, and has units TECU m^{-1}
STEC_factor_GPS = 2.0 / IONOSPHERE_Kp_2 * freq_factor_GPS / TECU_TO_M_N2
MJD_OF_GPS_C1_SWITCH = 51670
############# GLONASS
nu_G1_GLONASS = 1602.0E6 # in Hz, approximate, as GLONASS keeps changing ....
nu_G2_GLONASS = 1246.0E6 # in Hz, approximate
freq_factor_GLONASS = nu_G1_GLONASS*nu_G1_GLONASS*nu_G2_GLONASS*nu_G2_GLONASS \
                      / (nu_G1_GLONASS*nu_G1_GLONASS-nu_G2_GLONASS*nu_G2_GLONASS)
STEC_factor_GLONASS = 2.0 / IONOSPHERE_Kp_2 * freq_factor_GLONASS / TECU_TO_M_N2
#                                                            in TECU m^{-1}
############## Galileo
nu_E1_Gal = 1575.42E6  # in Hz
nu_E5_Gal = 1176.45E6  # in Hz
nu_E6_Gal = 1278.75E6  # in Hz
nu_E7_Gal = 1207.140E6 # in Hz
nu_E8_Gal = 1191.795E6 # in Hz
STEC_factor_Gal = None # prevent software from working with Galileo until
#                        we figure out what stations put into their RINEX files.







PRINT_DEBUG_LEVEL = 0













##################################
def set_debug_option(debug_option):
  DEBUG_SET = debug_option
  print('Albus_RINEX_2 setting debug_option to',debug_option)



################################################################################
def fix_RINEX_obs_file_undersampling(step_max, Sat_array, obs_data):
    """fix up RINEX observations when undersampled using linear interpolation

All changes are done internally to Sat_array and obs_data, which, as they are
numpy arrays, should propagate back to the caller


INPUTS:
step_max   I  Maximum number of steps in time to look away
Sat_array  I  numpy array of satellite data positions, as
              Sat_array[num_times,MAX_POSSIBLE_SATELLITES]
              If the value is -1, then no observation of that satellite was made
obs_data   I  The output data array, as
              obs_data[num_times, max_sat, _DATA_POS_SIZE]
              Assuming you have found the best index i in MJD for
              a time you desire, and you want data for satellite s, then
              you get data from
              obs_data[i,Sat_array[i,s], x]
              where x is the index of the data kind you want, as specified
              in the _DATA_POS dict

OUTPUTS: None

"""
    if(step_max < 1):
        return
    sh = obs_data.shape
    NUM_TIMES = sh[0]
    NUM_SAT = sh[1]
    for i in range(NUM_TIMES):
        if(Sat_array[i,:].sum() == -MAX_POSSIBLE_SATELLITES):
            # completely blank data entry, try to fix
            index_low = None
            index_high = None
            for j in range(1,step_max+1):
                k = i-j
                if(k >= 0):
                    if(Sat_array[k,:].sum() != -MAX_POSSIBLE_SATELLITES):
                        index_low = k
                        break
            else:
                continue
            for j in range(1,step_max+1):
                k = i+j
                if(k < NUM_TIMES):
                    if(Sat_array[k,:].sum() != -MAX_POSSIBLE_SATELLITES):
                        index_high = k
                        break
            else:
                continue
            # Ok, if we are here then we have a low index and a high index
            # So search for matching satellites
            sat_pos = 0
            for s in range(MAX_POSSIBLE_SATELLITES):
                sat_pos_low = Sat_array[index_low,s]
                sat_pos_high = Sat_array[index_high,s]
                if((sat_pos_low < 0) or (sat_pos_high < 0)):
                    continue
                assert(sat_pos < NUM_SAT)
                mult = float(i-index_low)/float(index_high-index_low)
                for p in range(_DATA_POS_SIZE):
                    val_high = obs_data[index_high,sat_pos_high,p]
                    val_low = obs_data[index_low,sat_pos_low,p]
                    if((val_high != BAD_DATA_CODE) and
                       (val_low != BAD_DATA_CODE)):
                        obs_data[i,sat_pos,p] = val_low+mult*(val_high - val_low)
                Sat_array[i,s] = sat_pos
                sat_pos += 1
    return

            







################################################################################
def read_RINEX_obs_file(filename, MJD_check = None,num_times = -1, max_sat = -1,
                        One_Day_Limit = 0):
    """read a RINEX observation file for useful data for ionospheric work

See the RIENX manuals at ftp://igs.org/pub/data/format


store satellite data as indicated by
_DATA_POS



INPUTS:
filename   I  The full path+file of the observation RINEX file to read
MJD_check  I  Start date as MJD of data.  If None, then any date is allowed.
              Otherwise, check that the TIME OF FIRST OBS matches to within
              0.75 days.  If not valid, raise a DataError
num_times  I  The number of different data times in the file.  The user should
              never change from the default.
max_sat    I  The maximum number of different satellites up at once.  The
              user should never use this.
One_Day_Limit I Should the RINEX data be limited to one day?  If 0, then no, and
              allow all data from the file to be read.  If not 0, then limit
              the observations to just one day.  (This is useful to comply
              with the DCB bias correction algorithm in Albus_RINEX_2.)


OUTPUTS: MJD, Sat_array, obs_data, time_offset, XYZ
MJD        O  numpy array of Modified Julian Dates for each observation time
              as MJD[num_times]
Sat_array  O  numpy array of satellite data positions, as
              Sat_array[num_times,MAX_POSSIBLE_SATELLITES]
              If the value is -1, then no observation of that satellite was made
obs_data   O  The output data array, as
              obs_data[num_times, max_sat, _DATA_POS_SIZE]
              Assuming you have found the best index i in MJD for
              a time you desire, and you want data for satellite s, then
              you get data from
              obs_data[i,Sat_array[i,s], x]
              where x is the index of the data kind you want, as specified
              in the _DATA_POS dict
time_offset O Offset, in s, to ADD to the times written in the file to get UTC
XYZ        O  Cartesian station position in Earth centered coodriantes, in m
              A numpy array array.
"""
    assert((num_times * max_sat) > 0)
    LOSS_OF_LOCK = _DATA_POS['LL']
    STRENGTH_1 = _DATA_POS['SF1']
    STRENGTH_2 = _DATA_POS['SF2']
    P1_POS = _DATA_POS['P1']
    C1_POS = _DATA_POS['C1']
    S1_POS = _DATA_POS['S1']
    S2_POS = _DATA_POS['S2']

    try:
        num_data = None     # Number of data elements per sat
        lines_per_sat = None
        obs_info = None
        obs_info_code = None
        time_offset = None  # ADD this to the file time to get UTC, in seconds
        MJD_start = None
        ymdhms = None       # start year, month, day, hour, minute, second
        time_system = 'GPS' # Which time system?  Defaults to GPS
        interval = 30.0 * DAYS_PER_SECOND # days
        interval_undersampled = 0 # for now assume not undersampled
        XYZ_set = False
        XYZ = None
        try:
            fp = open(filename, "rb")
        except IOError:
            raise Albus_RINEX.No_RINEX_File_Error("Error: RINEX file '%s' cannot be opened"%filename)
        line = fp.readline()
        line = ''.join(map(chr, map(lambda x: x if x < 127 else ord(' '), line)))
        if((line[60:80] != "RINEX VERSION / TYPE")
           or (line[20] != 'O')):
            raise Albus_RINEX.RINEX_Data_Barf("Error: RINEX file '%s' not a proper observation file"%filename)
        version = float(line[0:9])
        # int() truncates!
        if((int(version) != 2) or (version > 2.11)):
            raise Albus_RINEX.RINEX_Data_Barf("Error: RINEX file '%s' has unsupporter version type %.2f"%(filename, version))
        while(1):
            line = fp.readline()
            line = ''.join(map(chr, map(lambda x: x if x < 127 else ord(' '), line)))
            if(line[60:73] == "END OF HEADER"):
                break
            elif(line[60:79] == "APPROX POSITION XYZ"):
                XYZ = np.zeros((6), dtype='float64')
                XYZ[0:3] = list(map(float,line.strip().split(None,3)[0:3]))
                XYZ[3] = math.sqrt(XYZ[0]*XYZ[0] + XYZ[1]*XYZ[1] + XYZ[2]*XYZ[2])
                XYZ_set = True
                if(XYZ[3] == 0.0):
                    # G$*&@#!  Someone from the GPS group didn't bother to
                    # give us a position.  Don't believe a center of the Earth
                    # receiver position.  Try the GPS_stations list
                    warnings.warn("XYZ position set to center of Earth in '%s'"%filename)
                    station_name = filename[-12:-8].lower()
                    if(station_name in GPS_stations.GPS_stations):
                        XYZ[0:3] = GPS_stations.GPS_stations[station_name]
                        XYZ[3] = math.sqrt(XYZ[0]*XYZ[0] + XYZ[1]*XYZ[1] + XYZ[2]*XYZ[2])
                        warnings.warn("Using XYZ position from GPS Stations file")
                    else:
                        raise Albus_RINEX.RINEX_Data_Barf("No valid station position for file '%s'"%(filename))
                try:
                    XYZ[4] = math.atan2(XYZ[1],XYZ[0])
                except ArithmeticError:
                    XYZ[4] = 0.0
                try:
                    XYZ[5] = XYZ[2]/XYZ[3]
                    if(math.fabs(XYZ[5]) > 1.0):
                        XYZ[5] /= math.fabs(XYZ[5])
                    XYZ[5] = math.acos(XYZ[5])
                except ArithmeticError:
                    XYZ[5] = 0.0
            elif(line[60:80] == "ANTENNA: DELTA H/E/N"):
                offset = list(map(float,line.strip().split(None,3)[0:3]))
                if(XYZ_set == False):
                    warnings.warn("XYZ position not set yet in '%s'"%filename)
                else:
                    r = XYZ[3]
                    r_ratio = (r+ offset[0]) / r
                    r = r+ offset[0]
                    XYZ[3] = r
                    for i in range(3):
                        XYZ[i] *= r_ratio
                    if((offset[1] != 0.0) or (offset[2] != 0.0)):
                        r_xy = math.sqrt(XYZ[0]*XYZ[0] + XYZ[1]*XYZ[1])
                        phi = XYZ[4]
                        theta2 = math.pi*0.5 - XYZ[5]
                        XYZ[2] += offset[2] * math.cos(theta2)
                        if(r_xy > 0.0):
                            r_ratio = (r_xy - offset[2] * math.sin(theta2)) / r_xy
                            XYZ[0] *= r_ratio
                            XYZ[1] *= r_ratio
                        XYZ[0] -= offset[1] * math.sin(phi)
                        XYZ[1] += offset[1] * math.cos(phi)
                    XYZ[3] = math.sqrt(XYZ[0]*XYZ[0]+XYZ[1]*XYZ[1]+XYZ[2]*XYZ[2])
                    try:
                        XYZ[4] = math.atan2(XYZ[1],XYZ[0])
                    except ArithmeticError:
                        XYZ[4] = 0.0
                    try:
                        XYZ[5] = XYZ[2]/XYZ[3]
                        if(math.fabs(XYZ[5]) > 1.0):
                            XYZ[5] /= math.fabs(XYZ[5])
                        XYZ[5] = math.acos(XYZ[5])
                    except ArithmeticError:
                        XYZ[5] = 0.0
            elif(line[60:77] == "TIME OF FIRST OBS"):
                ymdhms = list(map(int,line.strip().split(None,5)[0:5]))
                ymdhms.append(float(line[30:43]))
                if(MJD_check is not None):
                    MJD_start=jma_tools.get_MJD_hms(ymdhms[0],ymdhms[1],ymdhms[2],
                                                    ymdhms[3],ymdhms[4],ymdhms[5])
                    if(math.fabs(MJD_start-MJD_check) > 0.75):
                        raise Albus_RINEX.RINEX_Data_Barf("File MJD %.2f does not match check MJD %.2f for '%s'"%(MJD_start, MJD_check, filename))
                    MJD_start = float(int(MJD_check+0.5)) # int truncates!
                else:
                    MJD_start = jma_tools.get_MJD_hms(ymdhms[0],ymdhms[1],
                                                     ymdhms[2],
                                                     ymdhms[3],ymdhms[4],
                                                     ymdhms[5])
                    MJD_start = float(int(MJD_start+0.5)) # int truncates!
                time_system = line[48:51]
                if(time_system == '   '):
                    time_system = 'GPS'
            elif(line[60:72] == "LEAP SECONDS"):
                time_offset = -int(line[0:6])
                if(time_offset == 0):
                    # Bleeping buggy GPS software!  Set back to invalid value.
                    time_offset = None
            elif(line[60:68] == "INTERVAL"):
                data_interval = float(line[0:10]) * DAYS_PER_SECOND
                if(data_interval != interval):
                    warnings.warn("RINEX file '%s' has nonstandard data interval of %.1f seconds, forcing to standard interval of %.1f seconds"%(filename,data_interval*SECONDS_PER_DAY,interval*SECONDS_PER_DAY))
                    if(data_interval > 1.99 * interval):
                        interval_undersampled = int(data_interval/interval)-1
            elif(line[60:79] == "# / TYPES OF OBSERV"):
                num_data = int(line[0:6])
                lines_per_sat = int((num_data-1)/5) +1
                obs_info = [None]*num_data
                obs_info_code = [None]*num_data
                line_pos = 0
                count = 0
                while(count < num_data):
                    line_pos += 6
                    if(line_pos >= 60):
                        line_pos = 6
                        line = fp.readline()
                        line = ''.join(map(chr, map(lambda x: x if x < 127 else ord(' '), line)))
                    code = line[line_pos+4:line_pos+6]
                    obs_info_code[count] = code
                    if(code in list(_DATA_POS.keys())):
                        obs_info[count] = _DATA_POS[code]
                    elif((code[0] == 'P') or (code[0] == 'L') or (code[0] == 'S')
                         or ((code[0] == 'C') and(code[1] >= '5'))):
                        warnings.warn("The system does not handle observation frequency codes greater than C2")
                        pass
                    else:
                        pass
                    count += 1
        # Now check that we got everything necessary
        if((num_data == None) or (ymdhms == None) or (XYZ_set == False)):
            raise Albus_RINEX.RINEX_Data_Barf("Error: missing header information in '%s'"%filename)
        retval, TAI_UTC = AlbusIonosphere.get_TAI_UTC(ymdhms[0], ymdhms[1],
                                                      ymdhms[2], 0.5)
        if(time_offset == None):
            if(time_system == 'GPS'):
                time_offset = -(TAI_UTC - 19.0) # GPS time
            elif(time_system == 'GLO'):
                time_offset = 0.0 # GLONASS works on UTC
            elif(time_system == 'GAL'):
                time_offset = -TAI_UTC # Galileo is on TAI
            else:
                raise Albus_RINEX.RINEX_Data_Barf("Unsupported Time system in file '%s'"%filename)
        # Read in the data
        if(num_times > 0):
            MJD = np.zeros((num_times), dtype='float64')
            Sat_array = np.zeros((num_times,MAX_POSSIBLE_SATELLITES),
                                       dtype='int16')
            obs_data = np.zeros((num_times, max_sat, _DATA_POS_SIZE),
                                      dtype='float64')
            Sat_array -= 1
            obs_data += BAD_DATA_CODE
            MJD_start = MJD_start + time_offset * DAYS_PER_SECOND
            for i in range(num_times):
                MJD[i] = MJD_start + i*interval
        while(1):
            line = fp.readline()
            line = ''.join(map(chr, map(lambda x: x if x < 127 else ord(' '), line)))
            l = len(line)
            if(l <= 1):
                break
            # Check for comment or extra header junk
            if(l > 60):
                if(line[60].isalpha()):
                    continue
            # Check for epoch flag
            if(line[28].isdigit()):
                epoch_flag = int(line[26:29])
                if(epoch_flag > 1):
                    if(PRINT_DEBUG_LEVEL > 0):
                        warnings.warn("This current RINEX code cannot handle parameter changes, but the RINEX file '%s' may be trying to do so.  Please investigate"%filename)
                        sys.stderr.write(line)
                    # Grumble, grumble, grumble.
                    # Some writing software does not follow the spec, and places
                    # the number of lines in the wrong place
                    num_list_to_skip = 0
                    if(len(line) >= 32):
                        try:
                            num_list_to_skip = int(line[29:32])
                        except ValueError:
                            warnings.warn("ValueError: failure to process num_list_to_skip = int(line[29:32])")
#                           raise Albus_RINEX.RINEX_Data_Barf("ValueError")
                            continue
                    elif(len(line) < 30):
                        continue
                    else:
                        try:
                            num_list_to_skip = int(line[29:len(line)])
                        except ValueError:
                            warnings.warn("ValueError: failure to process num_list_to_skip = int(line[29:len(line)])")
#                           raise Albus_RINEX.RINEX_Data_Barf("ValueError")
                            continue
                    for skip in range(num_list_to_skip):
                        line = fp.readline()
                        line = ''.join(map(chr, map(lambda x: x if x < 127 else ord(' '), line)))
                        if(PRINT_DEBUG_LEVEL > 0):
                            sys.stderr.write(line)
                    continue
            if(line[0:26] == "                          "):
                continue
            year = int(line[1:3])
            if(year > 79):
                year += 1900
            else:
                year += 2000
            month = int(line[4:6])
            day = int(line[7:9])
            hour = int(line[10:12])
            minute = int(line[13:15])
            second = float(line[16:26])
            if(num_times > 0):
                second = second + time_offset
                MJD_now = jma_tools.get_MJD_hms(year, month, day,
                                               hour, minute, second)
                index_f = (MJD_now - MJD_start) / interval
                time_index = int(index_f + 0.25)
                if(math.fabs(index_f - time_index) > 0.1):
                    # Hey, this does not match the time interval!  Some
                    # GPS site did not properly decimate their data.  Warn the
                    # user, but try to perform our own self-decimation.
                    #warnings.warn("Warning: time stamp does not match proper measurement interval of %.1f seconds in file '%s', trying to self-decimate.\nData line reads:\n%s"%(interval*SECONDS_PER_DAY,filename,line))
                    if(PRINT_DEBUG_LEVEL > 0):
                        warnings.warn("Warning: time stamp does not match proper measurement interval of %.1f seconds in file '%s', trying to self-decimate.\nFile has measurement at seconds value %s\n"%(interval*SECONDS_PER_DAY,filename,line[16:26]))
                    time_index = num_times
                if(time_index < 0):
                    time_index = num_times
            num_sat = int(line[29:32])
            if(num_sat > max_sat):
                if(num_times > 0):
                    raise Albus_RINEX.RINEX_Data_Barf("Function called with too little satellite space")
                max_sat = num_sat
            line_pos = 32-3
            sat_count = 0
            SBAS_warned = 0
            while(sat_count < num_sat):
                line_pos += 3
                if(line_pos >= 68):
                    line_pos = 32
                    line = fp.readline()
                    line = ''.join(map(chr, map(lambda x: x if x < 127 else ord(' '), line)))
                if(num_times > 0):
                    code = line[line_pos]
                    sat = int(line[line_pos+1:line_pos+3])
                    if((code == ' ') or (code == 'G')):
                        # GPS satellite
                        pass
                    elif(code == 'R'):
                        # GLONASS
                        sat += 100
                    elif(code == 'E'):
                        # Galileo
                        sat += 200
                    elif(code == 'S'):
                        # SBAS
                        if(SBAS_warned == 0):
                            warnings.warn("SBAS Warning!\nEncountered SBAS payload satellite information\nUnable to deal with SBAS payload data at this time --- skipping all SBAS data.\nContact software developer to implement SBAS options\n")
                            SBAS_warn = 1
                        sat_count += 1
                        continue
                    else:
                        raise Albus_RINEX.RINEX_Data_Barf("Unsupported satellite type '%s' in observation at line '%s'"%(code,line))
                    assert(sat >= 0)
                    assert(sat < MAX_POSSIBLE_SATELLITES)
                    if(time_index < num_times):
                        Sat_array[time_index,sat] = sat_count
                sat_count += 1
            if((num_times < 0) or (time_index >= num_times)):
                # skipping over data values
                for i in range(num_sat*lines_per_sat):
                   line = fp.readline()
                   line = ''.join(map(chr, map(lambda x: x if x < 127 else ord(' '), line)))
            else:
                for sat in range(num_sat):
                    line = fp.readline()
                    line = ''.join(map(chr, map(lambda x: x if x < 127 else ord(' '), line)))
                    l = len(line)
                    line_pos = -16
                    obs_count = 0
                    while(obs_count < num_data):
                        line_pos += 16
                        if(line_pos >= 80):
                            line_pos = 0
                            line = fp.readline()
                            line = ''.join(map(chr, map(lambda x: x if x < 127 else ord(' '), line)))
                            l = len(line)
                        if((l >= line_pos+14) and (obs_info[obs_count] != None)):
                            try:
                                val = float(line[line_pos:line_pos+14])
                                obs_data[time_index,sat,obs_info[obs_count]] =val
                                if(val == 0.0):
                                    # Whoa, for some reason, some GPS programs
                                    # write out '     .000  ' when they have
                                    # no data
                                    obs_data[time_index,sat,obs_info[obs_count]]=BAD_DATA_CODE
                                    # Missing data is almost always a bad sign
                                    if((obs_info[obs_count] != S1_POS) and
                                       (obs_info[obs_count] != S2_POS) and
                                       (obs_info[obs_count] != P1_POS)):
                                        obs_data[time_index,sat,LOSS_OF_LOCK] = 1
                            except ValueError:
                                warnings.warn("ValueError: probable satellite LOSS OF LOCK")
#                               raise Albus_RINEX.RINEX_Data_Barf("ValueError")
                                # Missing data is almost always a bad sign
                                #if(obs_info[obs_count] != P1_POS):
                                #    obs_data[time_index,sat,LOSS_OF_LOCK] = 1
                                if((obs_info[obs_count] != S1_POS) and
                                   (obs_info[obs_count] != S2_POS) and
                                   (obs_info[obs_count] != P1_POS)):
                                    obs_data[time_index,sat,LOSS_OF_LOCK] = 1
                        if(l >= line_pos+15):
                            if((line[line_pos+14].isspace()) or
                               (line[line_pos+14] == '0') or
                               ((int(line[line_pos+14])&0x3) == 0)):
                                pass
                            else:
                                # The loss of lock indicator (or some other gunk)
                                # is set.  Ignore this obs.
                                obs_data[time_index,sat,LOSS_OF_LOCK] = 1
                        if(l >= line_pos+16):
                            if((line[line_pos+15].isspace()) or
                               (line[line_pos+15] == '0')):
                                pass
                            else:
                                signal_strength_flag = int(line[line_pos+15])
                                freq = int(obs_info_code[obs_count][1])
                                if(freq == 1):
                                    obs_data[time_index,sat,STRENGTH_1] = \
                                        signal_strength_flag
                                elif(freq == 2):
                                    obs_data[time_index,sat,STRENGTH_2] = \
                                        signal_strength_flag
                                else:
                                    # remember - we only handle C1 and C2 Frequency codes
                                    pass
                        obs_count += 1
                    if(obs_data[time_index,sat,LOSS_OF_LOCK] != BAD_DATA_CODE):
                        obs_data[time_index,sat,:] = BAD_DATA_CODE
        if((num_times > 0) and (interval_undersampled)):
            fix_RINEX_obs_file_undersampling(interval_undersampled,
                                             Sat_array, obs_data)
    finally:
        fp.close()
    if(num_times < 0):
        if(One_Day_Limit == 0):
            MJD_end = jma_tools.get_MJD(year, month, day) +1.0
            # If the last time is just past midnight UT, then skip it.  This will
            # eliminate a few datapoints, but makes life so much easier.
            if((hour == 0) and (minute <10)):
                MJD_end -= 1.0
        else:
            MJD_end = MJD_start + 1
        num = int((MJD_end - MJD_start) / interval + 0.25)
        return read_RINEX_obs_file(filename, MJD_check, num, max_sat)
    return MJD, Sat_array, obs_data, time_offset, XYZ






################################################################################
def _clean_up_phi_terms(obs_data):
    """takes in an obs_data object from read_RIMEX_sp3_file and makes phi ordered

phi wraps around for satellites.  Make it be continually increasing or
decreasing by adding on 2\pi values as necessary.  This routine assumes that
If there is data for a satellite, there is ALWAYS data for that satellite.
Also, assume that spacecraft orbit in +\phi direction

INPUTS:
obs_data   I  The input data array, as
              obs_data[num_times, MAX_POSSIBLE_SATELLITES, 6]
              Assuming you have found the best index i in MJD for
              a time you desire, and you want data for satellite s, then
              you get data from
              obs_data[i,s, x]
              where x is the index of the XYZ value you want, X=0, Y=1, Z=2.
              r=3, \phi=4,\theta=5
              If X==Y==Z==0, then no data.

OUTPUTS: obs_data
"""
    sh = obs_data.shape
    NUM_OBS = sh[0]
    NUM_SAT = sh[1]
    circle = 2.0 * math.pi
    assert(sh[2] >= 6)
    for s in range(NUM_SAT):
        if((obs_data[0,s,0] == 0.0)and(obs_data[0,s,1] == 0.0)
           and(obs_data[0,s,2] == 0.0)):
            continue
        last = obs_data[0,s,4]
        for i in range(NUM_OBS):
            while(obs_data[i,s,4] < last-math.pi):
                obs_data[i,s,4] += circle
            last = obs_data[i,s,4]
    return obs_data


                

################################################################################
def get_orbital_parameters(MJD, sat_pos, sat):
    """get mean orbital parameters for GPS-like observations

Taking in sme satellite positions in sat_pos (from read_RINEX_sp3_file,
corrected in _clean_up_phi_terms), calculate the mean orbital terms.
See JMA notes from 2006 Aug 03.

INPUTS:
MJD            O  numpy array of Modified Julian Dates for each observation time
                  as MJD[num_times]
sat_pos        I  The input data array, as
                  sat_pos[num_times, MAX_POSSIBLE_SATELLITES, 6]
                  Assuming you have found the best index i in MJD for
                  a time you desire, and you want data for satellite s, then
                  you get data from
                  sat_pos[i,s, x]
                  where x is the index of the XYZ value you want, X=0, Y=1, Z=2.
                  r=3, \phi=4,\theta=5
                  If X==Y==Z==0, then no data.
sat            I  The particular satellite to get the mean elements for


OUTPUTS: [i,T,phi_0,beta,r_0,r_A,t_00,t_01,t_02]
i              O  Orbital inclination, in radians
i_0            O  mean orbital inclination offset in radians
T              O  Orbital period, in days
phi_0          O  initial \phi offset
beta           O  phi wave amplitude
r_0            O  mean radius
r_A            O  radius wave amplitude
t_00           O  theta phase offset
t_01           O  phi phase offset
t_02           O  r phase offset
"""
    iterations = 20
    if((sat_pos[0,sat,0] == 0.0)and(sat_pos[0,sat,0] == 0.0)
       and(sat_pos[0,sat,0] == 0.0)):
        return [0.0,0.0,0.5,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
    sat_r = sat_pos[:,sat,3]
    sat_phi = sat_pos[:,sat,4]
    sat_theta = sat_pos[:,sat,5]
    if((sat >= 0)and(sat <= 99)):
        # GPS
        i = 55.0 * math.pi/180.0
        T = 43200.0
        T_day = T * DAYS_PER_SECOND
    elif((sat >= 100)and(sat <= 199)):
        # GLONASS
        i = 64.8 * math.pi/180.0
        T = 40544.0
        T_day = T * DAYS_PER_SECOND
    elif((sat >= 200)and(sat <= 299)):
        # GPS
        raise Albus_RINEX.RINEX_Data_Barf("Galileo orbits unkown")
    else:
        raise Albus_RINEX.RINEX_Data_Barf("Unknown satellite %d"%sat)
    i_0 = 0.0
    r_0 = sat_r.mean()
    r_A = (sat_r.max() - sat_r.min()) * 0.5
    phi_2 = sat_phi.copy()
    phi_2 -= 2.0*math.pi * (1.0 / T_day - 1.0) * MJD
    phi_0 = phi_2.mean()
    beta = (phi_2.max() - phi_2.min()) * 0.5
    t_01 = MJD[np.argmax(phi_2)]
    t_02 = MJD[np.argmax(sat_r)]
    t_00 = MJD[np.argmax(sat_theta)]
    last_i = i
    last_i_0 = i_0
    last_t_00 = t_00
    last_chi_2 = 1E300
    #print ( "Starting theta")
    factor = 0.1
    for j in range(iterations):
        inside = 2.0*math.pi * (MJD-t_00)/T_day
        c_term = np.cos(inside)
        diff = sat_theta - 0.5*math.pi - i * c_term
        new_i_0 = diff.mean()
        diff -= new_i_0
        diff2 = diff*diff
        chi_2 = diff2.sum()
        if(chi_2 > last_chi_2):
            i = last_i
            i_0 = last_i_0
            t_00 = last_t_00
            if((factor < 1E-3)or(j==iterations-1)):
                break
            else:
                factor *= 0.1
        else:
            factor *= 1.75
            if(factor > 0.5):
                factor = 0.5
        slope = 2.0 * math.pi * i / T_day * np.sin(inside)
        slope = -2.0 * diff * slope
        d_sum = slope.sum()
        if(d_sum == 0.0):
            break
        change = -chi_2 / d_sum
        if(math.fabs(change) > 0.2 * T_day):
            if(change > 0.0):
                change = 0.2 * T_day
            else:
                change = -0.2 * T_day
        app = change * factor
        c2 = 0.0
        if(j>5):
            slope = -2.0 * diff * c_term
            d_sum = slope.sum()
            if(d_sum != 0.0):
                c2 = -chi_2 / d_sum * (j-5) / j
        a2 = c2 * 0.05 * factor
        #print ( "%2d %12.4E %10.4f %10.2E %10.4f %10.4f %12.3E %12.3E %12.3E %10.4f"%(j, i_0, t_00, chi_2, change, app, i, c2, a2,factor))
        last_i = i
        last_i_0 = i_0
        last_t_00 = t_00
        last_chi_2 = chi_2
        t_00 += app
        i += a2
        i_0 = new_i_0
    last_t_01 = t_01
    last_chi_2 = 1E300
    #print ( "Starting phi")
    factor = 0.1
    for j in range(iterations):
        inside = 4.0*math.pi * (MJD-t_01)/T_day
        diff = sat_phi - phi_0 - 2.0 * math.pi * (1.0/T_day - 1.0) * MJD
        diff = diff - beta * np.cos(inside)
        diff2 = diff*diff
        chi_2 = diff2.sum()
        if(chi_2 > last_chi_2):
            t_01 = last_t_01
            if((factor < 1E-3)or(j==iterations-1)):
                break
            else:
                factor *= 0.1
        else:
            factor *= 1.75
            if(factor > 0.5):
                factor = 0.5
        slope = 4.0 * math.pi * beta / T_day * np.sin(inside)
        slope = -2.0 * diff * slope
        d_sum = slope.sum()
        if(d_sum == 0.0):
            break
        change = -chi_2 / d_sum
        if(math.fabs(change) > 0.2 * T_day):
            if(change > 0.0):
                change = 0.2 * T_day
            else:
                change = -0.2 * T_day
        app = change * factor
        #print ( j, t_01, chi_2, change, app, t_01+app)
        last_t_01 = t_01
        last_chi_2 = chi_2
        t_01 += app
    last_t_02 = t_02
    last_r_0 = r_0
    last_r_A = r_A
    last_chi_2 = 1E300
    factor = 0.1
    #print ( "Starting r")
    for j in range(iterations):
        inside = 2.0*math.pi * (MJD-t_02)/T_day
        c_term = np.cos(inside)
        diff = sat_r - r_A * c_term
        new_r_0 = diff.mean()
        diff -= new_r_0
        diff2 = diff*diff
        chi_2 = diff2.sum()
        if(chi_2 > last_chi_2):
            t_02 = last_t_02
            r_0 = last_r_0
            r_A = last_r_A
            if((factor < 1E-3)or(j==iterations-1)):
                break
            else:
                factor *= 0.1
        else:
            factor *= 1.75
            if(factor > 0.5):
                factor = 0.5
        slope = 2.0 * math.pi * r_A / T_day * np.sin(inside)
        slope = -2.0 * diff * slope
        d_sum = slope.sum()
        if(d_sum == 0.0):
            break
        change = -chi_2 / d_sum
        app = change * factor
        c2 = 0.0
        if(j>5):
            slope = -2.0 * diff * c_term
            d_sum = slope.sum()
            if(d_sum != 0.0):
                c2 = -chi_2 / d_sum * (j-5) / j
        a2 = c2 * 0.05 * factor
        #print ( "%2d %12.4E %10.4f %10.2E %10.4f %10.4f %12.3E %12.3E %12.3E %10.4f"%(j, r_0, t_02, chi_2, change, app, r_A, c2, a2,factor))
        last_t_02 = t_02
        last_r_0 = r_0
        last_r_A = r_A
        last_chi_2 = chi_2
        t_02 += app
        r_0 = new_r_0
        r_A += a2
    return [i,i_0,T_day,phi_0,beta,r_0,r_A,t_00,t_01,t_02]

        






def read_RINEX_sp3_file(filename, time_offset = None):
    """read the satellite position information from an SP3 file

See the RINEX manuals at ftp://igs.org/pub/data/format

store satellite XYZ positions as X=0, Y=1, Z=2, r=3, phi=4, theta=5



INPUTS:
filename   I  The full path+file of the observation RINEX file to read
time_offset I Offset, in s, to ADD to the times written in the file to get UTC.
              The probably comes from reading a RINEX obs file.  If None, assume
              GPS time, and calculate from the number of leap seconds according
              to the start time in the file.  You should probably just leave
              as None unless you know what you are doing.


OUTPUTS: MJD, obs_data, time_offset
MJD        O  numpy array of Modified Julian Dates for each observation time
              as MJD[num_times]
obs_data   O  The output data array, as
              obs_data[num_times, MAX_POSSIBLE_SATELLITES, 6]
              Assuming you have found the best index i in MJD for
              a time you desire, and you want data for satellite s, then
              you get data from
              obs_data[i,s, x]
              where x is the index of the XYZ value you want, X=0, Y=1, Z=2.
              r=3, \phi=4,\theta=5
              If X==Y==Z==0, then no data.
"""
    MAX_POSSIBLE_SATELLITES = 300
    try:
        try:
            if DEBUG_SET:
               print ( 'read_RINEX_sp3_file: trying to open ', filename)
            fp = open(filename, "r")
        except IOError:
            raise Albus_RINEX.No_RINEX_File_Error("Error: sp3 file '%s' cannot be opened"%filename)
        line = fp.readline()    # Line 1
        version = line[1]
        if DEBUG_SET:
          print('****rinex SP3 version', version)
        if((version == 'a') or (version == 'b') or (version == 'c') or (version == 'd')):
            # things are ok
            pass
        else:
            warnings.warn("Unknown SP3 version '%s' in file '%s', trying anyway ...."%(version, filename))
        if(line[2] != 'P'):
            warnings.warn("Not a position file for '%s', trying anyway ...."%filename)
        year = int(line[3:7])
        month = int(line[8:10])
        day = int(line[11:13])
        reference_day = day
        hour = int(line[14:16])
        minute = int(line[17:19])
        second = float(line[20:31])
        MJD_start = jma_tools.get_MJD_hms(year, month, day, hour, minute, second)
        Num_Data = int(line[32:39])
        if Num_Data > 100:
          Num_Data = Num_Data - 1
        if(Num_Data < 10):
            raise Albus_RINEX.RINEX_Data_Barf("Too few data times in file '%s' to get accurate interpolation"%filename)
        # skip to line 13
        for i in range(2,14):
            line = fp.readline()
        time_system = line[9:12]
        # int() truncates!
        retval, TAI_UTC = AlbusIonosphere.get_TAI_UTC(year, month, day, 0.5)
        if(time_offset == None):
            if(time_system == 'GPS'):
                time_offset = -(TAI_UTC - 19.0) # GPS time
            elif(time_system == 'ccc'):
                assert(version == 'a')
                time_offset = -(TAI_UTC - 19.0) # GPS by default for version a
            elif(time_system == 'UTC'):
                time_offset = 0.0 
            elif(time_system == 'GLO'):
                time_offset = 0.0 # GLONASS works on UTC
            elif(time_system == 'GAL'):
                time_offset = -TAI_UTC # Galileo is on TAI
            else:
                raise Albus_RINEX.RINEX_Data_Barf("Unsupported Time system in file '%s'"%filename)
        # read through line 22
        for i in range(14,23):
            line = fp.readline()
        if version == 'd':   # read an extra line
            line = fp.readline()
        # Now to the data
        MJD = np.zeros((Num_Data), dtype='float64')
        obs_data = np.zeros((Num_Data, MAX_POSSIBLE_SATELLITES, 6))
        time_count = -1
        time_start = 0 
        get_sat = False
        while(1):
            line = fp.readline()
            if(len(line) <= 1):
                break
            if(line[0] == '*'):
                year = int(line[3:7])
                month = int(line[8:10])
                day = int(line[11:13])
                # check if we are starting a new day
                if day != reference_day: # needed for new large SP3 files
                  break 
                hour = int(line[14:16])
                minute = int(line[17:19])
                second = float(line[20:31]) + time_offset
                time_ref = hour * 3600 + minute * 60
                time_count += 1
                MJD[time_count] = jma_tools.get_MJD_hms(year, month, day,
                                                       hour, minute, second)
            elif(line[0] == 'P'):
                code = line[1]
                sat = int(line[2:4])
                if((code == ' ') or (code == 'G')):
                    # GPS satellite
                    pass
                elif(code == 'R'):
                    # GLONASS
                    sat += 100
                elif(code == 'E'):
                    # Galileo
                    sat += 200
                else:
                    raise Albus_RINEX.RINEX_Data_Barf("Unsupported satellite type '%s' in SP3 at line '%s'"%(code,line))
                assert(sat >= 0)
                assert(sat < MAX_POSSIBLE_SATELLITES)
                obs_data[time_count, sat, 0] = float(line[ 4:18]) * 1000.0 # in m
                obs_data[time_count, sat, 1] = float(line[18:32]) * 1000.0 # in m
                obs_data[time_count, sat, 2] = float(line[32:46]) * 1000.0 # in m
                x = obs_data[time_count, sat, 0]
                y = obs_data[time_count, sat, 1]
                z = obs_data[time_count, sat, 2]
                r = math.sqrt(x*x + y*y + z*z)
                try:
                    theta = math.acos(z/r)
                    phi = math.atan2(y,x)
                except:
                    if(r == 0.0):
                        phi = 0.0
                        theta = 0.0
                    elif(x == 0.0):
                        phi = 0.0
                    else:
                        raise Albus_RINEX.RINEX_Data_Barf("Unknown failure for %E %E %E"%(x,y,z))
                obs_data[time_count, sat, 3] = r
                obs_data[time_count, sat, 4] = phi
                obs_data[time_count, sat, 5] = theta
            elif(line[0:2] == 'EP'):
                pass
            elif(line[0] == 'V'):
                pass
            elif(line[0:2] == 'EV'):
                pass
            elif(line[0:3] == 'EOF'):
                break # end of file
            else:
                pass
#               raise Albus_RINEX.RINEX_Data_Barf("Unsupported data type in line '%s'"%line)
    finally:
        fp.close()
    if DEBUG_SET:
       print('Num_Data time_count', Num_Data, time_count)
#   assert(Num_Data == time_count+1)
    obs_data = _clean_up_phi_terms(obs_data)
    if DEBUG_SET:
      print('obs_data shape', obs_data.shape)
      print('MJD shape', MJD.shape)
      print('time_offset', time_offset)
    return MJD, obs_data


################################################################################
def read_RINEX_sp3_file_old(filename, time_offset = None):
    """read the satellite position information from an SP3 file

See the RINEX manuals at ftp://igs.org/pub/data/format

store satellite XYZ positions as X=0, Y=1, Z=2, r=3, phi=4, theta=5



INPUTS:
filename   I  The full path+file of the observation RINEX file to read
time_offset I Offset, in s, to ADD to the times written in the file to get UTC.
              The probably comes from reading a RINEX obs file.  If None, assume
              GPS time, and calculate from the number of leap seconds according
              to the start time in the file.  You should probably just leave
              as None unless you know what you are doing.


OUTPUTS: MJD, Sat_array, obs_data, time_offset
MJD        O  numpy array of Modified Julian Dates for each observation time
              as MJD[num_times]
obs_data   O  The output data array, as
              obs_data[num_times, MAX_POSSIBLE_SATELLITES, 6]
              Assuming you have found the best index i in MJD for
              a time you desire, and you want data for satellite s, then
              you get data from
              obs_data[i,s, x]
              where x is the index of the XYZ value you want, X=0, Y=1, Z=2.
              r=3, \phi=4,\theta=5
              If X==Y==Z==0, then no data.
"""
    try:
        try:
            if DEBUG_SET:
                print ( 'read_RINEX_sp3_file: trying to open ', filename)
            fp = open(filename, "r")
        except IOError:
            raise Albus_RINEX.No_RINEX_File_Error("Error: sp3 file '%s' cannot be opened"%filename)
        line = fp.readline()    # Line 1
        version = line[1]
        if DEBUG_SET:
          print('****rinex SP3 version', version)
        if((version == 'a') or (version == 'b') or (version == 'c') or (version == 'd')):
            # things are ok
            pass
        else:
            warnings.warn("Unknown SP3 version '%s' in file '%s', trying anyway ...."%(version, filename))
        if(line[2] != 'P'):
            warnings.warn("Not a position file for '%s', trying anyway ...."%filename)
        year = int(line[3:7])
        month = int(line[8:10])
        day = int(line[11:13])
        hour = int(line[14:16])
        minute = int(line[17:19])
        second = float(line[20:31])
        MJD_start = jma_tools.get_MJD_hms(year, month, day, hour, minute, second)
        Num_Data = int(line[32:39])
        if DEBUG_SET:
          print('Num_Data:', Num_Data)
        if(Num_Data < 10):
            raise Albus_RINEX.RINEX_Data_Barf("Too few data times in file '%s' to get accurate interpolation"%filename)
        # skip to line 13
        for i in range(2,14):
            line = fp.readline()
        time_system = line[9:12]
        # int() truncates!
        retval, TAI_UTC = AlbusIonosphere.get_TAI_UTC(year, month, day, 0.5)
        if(time_offset == None):
            if(time_system == 'GPS'):
                time_offset = -(TAI_UTC - 19.0) # GPS time
            elif(time_system == 'ccc'):
                assert(version == 'a')
                time_offset = -(TAI_UTC - 19.0) # GPS by default for version a
            elif(time_system == 'UTC'):
                time_offset = 0.0 
            elif(time_system == 'GLO'):
                time_offset = 0.0 # GLONASS works on UTC
            elif(time_system == 'GAL'):
                time_offset = -TAI_UTC # Galileo is on TAI
            else:
                raise Albus_RINEX.RINEX_Data_Barf("Unsupported Time system in file '%s'"%filename)
        # read through line 22
        for i in range(14,23):
            line = fp.readline()
        if version == 'd':   # read an extra line
            line = fp.readline()
        # Now to the data
        MJD = np.zeros((Num_Data), dtype='float64')
        obs_data = np.zeros((Num_Data, MAX_POSSIBLE_SATELLITES, 6),
                                  dtype='float64')
        time_count = -1
        while(1):
            line = fp.readline()
            if(len(line) <= 1):
                break
            if(line[0] == '*'):
                time_count += 1
                year = int(line[3:7])
                month = int(line[8:10])
                day = int(line[11:13])
                hour = int(line[14:16])
                minute = int(line[17:19])
                second = float(line[20:31]) + time_offset
                MJD[time_count] = jma_tools.get_MJD_hms(year, month, day,
                                                       hour, minute, second)
            elif(line[0] == 'P'):
                code = line[1]
                sat = int(line[2:4])
                if((code == ' ') or (code == 'G')):
                    # GPS satellite
                    pass
                elif(code == 'R'):
                    # GLONASS
                    sat += 100
                elif(code == 'E'):
                    # Galileo
                    sat += 200
                else:
                    raise Albus_RINEX.RINEX_Data_Barf("Unsupported satellite type '%s' in SP3 at line '%s'"%(code,line))
                assert(sat >= 0)
                assert(sat < MAX_POSSIBLE_SATELLITES)
                obs_data[time_count, sat, 0] = float(line[ 4:18]) * 1000.0 # in m
                obs_data[time_count, sat, 1] = float(line[18:32]) * 1000.0 # in m
                obs_data[time_count, sat, 2] = float(line[32:46]) * 1000.0 # in m
                x = obs_data[time_count, sat, 0]
                y = obs_data[time_count, sat, 1]
                z = obs_data[time_count, sat, 2]
                r = math.sqrt(x*x + y*y + z*z)
                try:
                    theta = math.acos(z/r)
                    phi = math.atan2(y,x)
                except:
                    if(r == 0.0):
                        phi = 0.0
                        theta = 0.0
                    elif(x == 0.0):
                        phi = 0.0
                    else:
                        raise Albus_RINEX.RINEX_Data_Barf("Unknown failure for %E %E %E"%(x,y,z))
#               print('sat, x,y,z,r,theta,phi', sat,x,y,z,r,theta,phi)
                obs_data[time_count, sat, 3] = r
                obs_data[time_count, sat, 4] = phi
                obs_data[time_count, sat, 5] = theta
            elif(line[0:2] == 'EP'):
                pass
            elif(line[0] == 'V'):
                pass
            elif(line[0:2] == 'EV'):
                pass
            elif(line[0:3] == 'EOF'):
                break # end of file
            else:
                raise Albus_RINEX.RINEX_Data_Barf("Unsupported data type in line '%s'"%line)
    finally:
      fp.close()
    if DEBUG_SET:
       print('final time_count', time_count)
    assert(Num_Data == time_count+1)
    obs_data = _clean_up_phi_terms(obs_data)
    return MJD, obs_data



        
################################################################################
def interpolate_sat_pos(MJD, sat_pos, MJD_need, sat_need, index_start, num_terms,
                        work_c, work_d):
    """Use Neville's algorithm to interpolate spacecraft position

based on Numerical Recipes polint routine

MJD         I  numpy array of times, as MJD[]
sat_pos     I  numpy array of satellite positin information, as
               sat_pos[t,s,x], where t is the time index, matched to MJD, s
               is the satellite id number, and x is the position information.
               This is expected to come from the obs_data output from
               read_RINEX_sp3_file.
MJD_need    I  The desired MJD of the satellite position
sat_need    I  The desired satellite
index_start I  The starting index to use for times of MJD and sat_pos
num_terms   I  How many terms to use in the interpolation
work_c      I  A numpy array work space, of double type.  Must be
               of the form work_c[num_terms+], where the
               plus signes indicate minimum dimension size
work_d      I  A numpy array work space, of double type.  Must be
               of the form work_d[num_terms+], where the
               plus signes indicate minimum dimension size

OUTPUTS:
XYZ         O  numpy array of the satellite position
               x=0,y=1,z=2
"""
    assert(work_c.shape[0] >= num_terms)
    assert(work_d.shape[0] >= num_terms)
    XYZ = np.zeros((6), dtype='float64')
   
    for p in range(3,6):
        dif = math.fabs(MJD_need-MJD[index_start])
        ns = 0
        for i in range(num_terms):
            dif2 = math.fabs(MJD_need-MJD[index_start+i])
            if(dif2 < dif):
                ns = i
                dif = dif2
            work_c[i] = sat_pos[index_start+i,sat_need,p]
            work_d[i] = work_c[i]
        best = ns
        y = work_c[ns]
        ns -= 1
        try:
            for m in range(1,num_terms):
                for i in range(num_terms-m):
                    ho = MJD[index_start+i]-MJD_need
                    hp = MJD[index_start+i+m]-MJD_need
                    w = work_c[i+1]-work_d[i]
                    den = w / (ho-hp)
                    work_d[i] = hp*den
                    work_c[i] = ho*den
                if(2*ns+2 < (num_terms-m)):
                    y += work_c[ns+1]
                else:
                    y += work_d[ns]
                    ns -= 1
        except ZeroDivisionError:
            warnings.warn("Zero divide error in satellite position interpolation")
            y = sat_pos[index_start+best,sat_need,p]
        XYZ[p] = y
    rstheta = XYZ[3] * math.sin(XYZ[5])
    XYZ[0] = rstheta * math.cos(XYZ[4])
    XYZ[1] = rstheta * math.sin(XYZ[4])
    XYZ[2] = XYZ[3] * math.cos(XYZ[5])
    return XYZ







################################################################################
def find_bottom_index_for_interp(index, width, MAX):
    """Find a proper interpolatin starting index for interpolating over values

Take in an index from Albus_RINEX.find_interpolation_point, which gives the
lower index of a pair of indices which surround a desired value in an array.
You want to interpolate over many of the array values to this desired value
with surrounding points.

index-3 index-2 index-1 index index+1 index+2 index+3
                            x

You could just subtract off width/2 from index and go.  But this routine
makes sure that the final starting index is >= 0 and that the final index is
< MAX

INPUTS
index       I  starting index from Albus_RINEX.find_interpolation_point
width       I  desired with of interpolation, in number
MAX         I  maximum index in array.


OUTPUTS:  index_use
index_start O  The index to use for interpolation over many values
"""
    assert(width>0)
    assert(width <= MAX)
    half = int(width/2)
    index_start = index-half
    if(index_start < 0):
        index_start = 0
    index_end = index_start + width
    if(index_end > MAX):
        index_start -= index_end - MAX
    return index_start






################################################################################
def interpolate_sat_positions(MJD, sat_pos, MJD_need):
    """Use a combination of fitting and interpolation to get satellite positions

Note: assumes that if a satellite is present at one time, it is present
for all times in the sat_pos array.


MJD         I  numpy array of times, as MJD[]
sat_pos     I  numpy array of satellite positin information, as
               sat_pos[t,s,x], where t is the time index, matched to MJD, s
               is the satellite id number, and x is the position information.
               This is expected to come from the obs_data output from
               read_RINEX_sp3_file.
MJD_need    I  The desired MJDs of the satellite positions, as a numpy array
               MJD_need[need_times]

OUTPUTS:
XYZ         O  numpy array of the satellite positions
               as XYZ[need_times,s,3]
               x=0,y=1,z=2.  If x==y==z==0, the no data
"""
    sh = sat_pos.shape
    NUM_OBS = sh[0]
    NUM_SAT = sh[1]
    NUM_DIR = sh[2]
    assert(NUM_DIR >= 6)
    NUM_TERMS = 10
    assert(NUM_OBS > NUM_TERMS)
    assert(NUM_OBS == MJD.shape[0])
    NUM_NEED = MJD_need.shape[0]
    assert(NUM_NEED>0)
    XYZ = np.zeros((NUM_NEED,NUM_SAT,6), dtype='float64')
    work_c = np.zeros((NUM_TERMS), dtype='float64')
    work_d = np.zeros((NUM_TERMS), dtype='float64')
    for s in range(NUM_SAT):
        if((sat_pos[0,s,0] == 0.0)and(sat_pos[0,s,0] == 0.0)
           and(sat_pos[0,s,0] == 0.0)):
            continue
        orbital_param = get_orbital_parameters(MJD, sat_pos, s)
        # Get the residuals to the measured orbits
        pos_res = sat_pos[:,s,:].copy()
        pos_res = np.reshape(pos_res, (NUM_OBS,1,NUM_DIR))
        inside = 2.0*math.pi * (MJD-orbital_param[9])/orbital_param[2]
        pos_res[:,0,3] += - orbital_param[5] - orbital_param[6] * np.cos(inside)
        inside = 4.0*math.pi * (MJD-orbital_param[8])/orbital_param[2]
        pos_res[:,0,4] += - orbital_param[3] - 2.0 * math.pi * (1.0/orbital_param[2] - 1.0) * MJD - orbital_param[4] * np.cos(inside)
        inside = 2.0*math.pi * (MJD-orbital_param[7])/orbital_param[2]
        pos_res[:,0,5] += - 0.5*math.pi - orbital_param[0] * np.cos(inside) - orbital_param[1]
        # Get the predicted orbit values
        inside = 2.0*math.pi * (MJD_need-orbital_param[9])/orbital_param[2]
        XYZ[:,s,3] = orbital_param[5] + orbital_param[6] * np.cos(inside)
        inside = 4.0*math.pi * (MJD_need-orbital_param[8])/orbital_param[2]
        XYZ[:,s,4] = orbital_param[3] + 2.0 * math.pi * (1.0/orbital_param[2] - 1.0) * MJD_need + orbital_param[4] * np.cos(inside)
        inside = 2.0*math.pi * (MJD_need-orbital_param[7])/orbital_param[2]
        XYZ[:,s,5] = 0.5*math.pi + orbital_param[0] * np.cos(inside) + orbital_param[1]
        # Now form the interpolated corrections
        index = None
        for i in range(NUM_NEED):
            index = Albus_RINEX.find_interpolation_point(MJD, MJD_need[i], index)
            index_start = find_bottom_index_for_interp(index, NUM_TERMS, NUM_OBS)
            pos_int = interpolate_sat_pos(MJD, pos_res, MJD_need[i], 0,
                                          index_start, NUM_TERMS, work_c, work_d)
            pos_int[3:6] += XYZ[i,s,3:6]
            rstheta = pos_int[3] * math.sin(pos_int[5])
            XYZ[i,s,0] = rstheta * math.cos(pos_int[4])
            XYZ[i,s,1] = rstheta * math.sin(pos_int[4])
            XYZ[i,s,2] = pos_int[3] * math.cos(pos_int[5])
            XYZ[i,s,3] = pos_int[3]
            XYZ[i,s,4] = pos_int[4]
            XYZ[i,s,5] = pos_int[5]
    return XYZ





################################################################################
def interpolate_sat_positions2(MJD, sat_pos, MJD_need):
    """Use only interpolation to get satellite positions

Note: assumes that if a satellite is present at one time, it is present
for all times in the sat_pos array.


MJD         I  numpy array of times, as MJD[]
sat_pos     I  numpy array of satellite positin information, as
               sat_pos[t,s,x], where t is the time index, matched to MJD, s
               is the satellite id number, and x is the position information.
               This is expected to come from the obs_data output from
               read_RINEX_sp3_file.
MJD_need    I  The desired MJDs of the satellite positions, as a numpy array
               MJD_need[need_times]

OUTPUTS: XYZ
XYZ         O  numpy array of the satellite positions
               as XYZ[need_times,s,6]
               x=0,y=1,z=2.  If x==y==z==0, the no data
               r=3,\phi=4,\theta=5
"""
    #print('In interpolate_sat_positions -  calculating Satellite positions')
    #Print('The calculations will take a minute or two')
    sh = sat_pos.shape
    NUM_OBS = sh[0]
    NUM_SAT = sh[1]
    NUM_DIR = sh[2]
    assert(NUM_DIR >= 6)
    NUM_TERMS_MAX = 10
    NUM_TERMS_GLONASS = 7
    assert(NUM_OBS > NUM_TERMS_MAX)
    assert(NUM_TERMS_GLONASS <= NUM_TERMS_MAX)
    assert(NUM_OBS == MJD.shape[0])
    NUM_NEED = MJD_need.shape[0]
    if DEBUG_SET:
      print('NUM_NEED', NUM_NEED)
    assert(NUM_NEED>0)
    XYZ = np.zeros((NUM_NEED,NUM_SAT,6), dtype='float64')
    work_c = np.zeros((NUM_TERMS_MAX), dtype='float64')
    work_d = np.zeros((NUM_TERMS_MAX), dtype='float64')
    for s in range(NUM_SAT):
        if((sat_pos[0,s,0] == 0.0)and(sat_pos[0,s,0] == 0.0)
           and(sat_pos[0,s,0] == 0.0)):
            continue
        print ( "Calculating positions for satellite %3d"%s)
        index = None
        NUM_TERMS = NUM_TERMS_MAX
        if((s>=100)and(s<=199)):
            if((MJD_need[i] < MJD[0])or(MJD_need[i]>MJD[NUM_OBS-1])):
                NUM_TERMS = NUM_TERMS_GLONASS # GLONASS needs smaller
                                              ## interpolation number
        for i in range(NUM_NEED):
            index = Albus_RINEX.find_interpolation_point(MJD, MJD_need[i], index)
            index_start = find_bottom_index_for_interp(index, NUM_TERMS, NUM_OBS)
            pos_int = interpolate_sat_pos(MJD, sat_pos, MJD_need[i], s,
                                          index_start, NUM_TERMS, work_c, work_d)
            XYZ[i,s,:] = pos_int
    return XYZ









################################################################################
def _compute_AzEl(sat_XYZ, sta_XYZ):
    """compute the azimuth and elevatin angle from XYZ coordinates

Be careful when there is no satellite data

sat_XYZ    I  satellite position in Earth centered coodriantes, in m
              Must be a numpy array with 6 elements(x,y,z,r,phi,theta)
sta_XYZ    I  station position in Earth centered coodriantes, in m
              Must be a numpy array with 6 elements(x,y,z,r,phi,theta)


OUTPUTS: Az, El
Az         O  Azimuth, in radians
El         O  Elevation angle, in radians
"""
    r_sta = sta_XYZ[3]
    r_sat = sat_XYZ[3]
    dot_p = sat_XYZ[0]*sta_XYZ[0]+sat_XYZ[1]*sta_XYZ[1]+sat_XYZ[2]*sta_XYZ[2]
    try:
        zenith_angle = math.acos(dot_p/(r_sta*r_sat))
    except ArithmeticError:
        if(dot_p > 0.0):
            zenith_angle = 0.0
        elif(dot_p < 0.0):
            zenith_angle = math.pi
        else:
            # Hey, no satellite or station data
            return 0.0,-math.pi*0.5
    El = math.pi*0.5 - zenith_angle
    dec = math.pi*0.5 - sat_XYZ[5]
    phi = math.pi*0.5 - sta_XYZ[5]
    h = sta_XYZ[4] - sat_XYZ[4]
    x = sat_XYZ[2]/r_sat*math.cos(phi)-math.cos(dec)*math.cos(h)*sta_XYZ[2]/r_sat
    y = -math.cos(dec) * math.sin(h)
    try:
        Az = math.atan2(y,x)
    except AritmeticError:
        Az = 0.0
    return Az,El
    
    





################################################################################
def fill_in_obs_AzEl_values(MJD, Sat_array, obs_data, sta_XYZ, sat_XYZ):
    """compute the azimuth and elevation angles for GPS observations

Using GPS observational data in MJD, Sat_array, and obs_data (as you would
read in using read_RINEX_obs_file), and satellite position information as you
would get from read_RINEX_sp3_file followed by a call to
interpolate_sat_positions2 using the observation MJDs), compute the
elevation angle and the azimuth angle of the satellites

MJD        I  numpy array of Modified Julian Dates for each observation time
              as MJD[num_times]
Sat_array  I  numpy array of satellite data positions, as
              Sat_array[num_times,MAX_POSSIBLE_SATELLITES]
              If the value is -1, then no observation of that satellite was made
obs_data   I  The GPS data array, as
              obs_data[num_times, max_sat, _DATA_POS_SIZE]
              Assuming you have found the best index i in MJD for
              a time you desire, and you want data for satellite s, then
              you get data from
              obs_data[i,Sat_array[i,s], x]
              where x is the index of the data kind you want, as specified
              in the _DATA_POS dict
sta_XYZ    I  Cartesian station position in Earth centered coodriantes, in m
              Must be a numpy array
sat_XYZ    I  numpy array of the satellite positions
              as XYZ[num_times,MAX_POSSIBLE_SATELLITES,3]
              x=0,y=1,z=2.  If x==y==z==0, the no data

OUTPUTS: None
"""
    NUM_OBS = obs_data.shape[0]
    assert(NUM_OBS == MJD.shape[0])
    assert(NUM_OBS == Sat_array.shape[0])
    assert(NUM_OBS == sat_XYZ.shape[0])
    El_pos = _DATA_POS['EL']
    Az_pos = _DATA_POS['AZ']
    for i in range(NUM_OBS):
        for s in range(MAX_POSSIBLE_SATELLITES):
            sat_index = Sat_array[i,s]
            if(sat_index < 0): continue
            sat_pos = sat_XYZ[i,s,:]
            Az,El = _compute_AzEl(sat_pos, sta_XYZ)
            obs_data[i,Sat_array[i,s], El_pos] = El
            obs_data[i,Sat_array[i,s], Az_pos] = Az
    return






################################################################################
def calculate_STECs(Sat_array, obs_data, MJD):
    """calculate the slant TEC values from P and L data

convert the raw GPS measurements to slant TEC values, and also calculate
uncertainties

Sat_array  I  numpy array of satellite data positions, as
              Sat_array[num_times,MAX_POSSIBLE_SATELLITES]
              If the value is -1, then no observation of that satellite was made
obs_data   I  The GPS data array, as
              obs_data[num_times, max_sat, _DATA_POS_SIZE]
              Assuming you have found the best index i in MJD for
              a time you desire, and you want data for satellite s, then
              you get data from
              obs_data[i,Sat_array[i,s], x]
              where x is the index of the data kind you want, as specified
              in the _DATA_POS dict
MJD        I  The Modified Julian Date of the start of observations

OUTPUTS: None
"""
    C1_pos = _DATA_POS['C1']
    P1_pos = _DATA_POS['P1']
    P2_pos = _DATA_POS['P2']
    L1_pos = _DATA_POS['L1']
    L2_pos = _DATA_POS['L2']
    S1_pos = _DATA_POS['S1']
    S2_pos = _DATA_POS['S2']
    SF1_pos = _DATA_POS['SF1']
    SF2_pos = _DATA_POS['SF2']
    TP_pos = _DATA_POS['STECP']
    SIG_pos = _DATA_POS['SIGMA']
    TL_pos = _DATA_POS['STECL']
    # Quatization error is \sqrt{1/12} of the quatized digit, for each
    # measurment.  For RINEX stuff, we subtract two measurements which are
    # quantized at 0.001 = 10^{-3}, so the full variance is
    # 2 (10^{-3})^2 / 12 
    Quantization_Variance = 2E-6/12.0
    # RINEX versions 1 and 2 have no consistent S/N values.
    # Some people use S/N, some use dBHz, and so on.
    # Try to figure out what is going on.
    SNR_FLAG = 0
    for i in range(obs_data.shape[0]):
        for sat in range(obs_data.shape[1]):
            SN = obs_data[i,sat, S1_pos]
            if(SN != BAD_DATA_CODE):
                if(SN > 100.0):
                    # assume this is actual S/N ratio, rather than dBHz
                    SNR_FLAG = 1
        if(SNR_FLAG == 1): break
    for i in range(obs_data.shape[0]):
        for sat in range(MAX_POSSIBLE_SATELLITES):
            s = Sat_array[i,sat]
            if(s < 0):
                continue
            # Calculate the uncertainty
            SN_1 = obs_data[i,s, S1_pos]
            if(SN_1 == BAD_DATA_CODE):
                if(obs_data[i,s, SF1_pos] == BAD_DATA_CODE):
                    SN_1 = 12.0
                else:
                    SN_1 = obs_data[i,s, SF1_pos]*6.0 + 6.0
            elif(SN_1 <= 0.0):
                # Grumble, grumble, grumble
                # Some people let weird values into their RINEX
                # files, and SNR = -1000 is insane, both for
                # SNR or SNR = dBHz
                SN_1 = 1E-4
            if(SNR_FLAG == 1):
                SN_1 = 10.0 * math.log10(SN_1)
            SN_2 = obs_data[i,s, S2_pos]
            if(SN_2 == BAD_DATA_CODE):
                if(obs_data[i,s, SF2_pos] == BAD_DATA_CODE):
                    SN_2 = 12.0
                else:
                    SN_2 = obs_data[i,s, SF2_pos]*6.0 + 6.0
            elif(SN_2 <= 0.0):
                # Grumble, grumble, grumble
                # Some people let weird values into their RINEX
                # files, and SNR = -1000 is insane, both for
                # SNR or SNR = dBHz
                SN_2 = 1E-4
            if(SNR_FLAG == 1):
                SN_2 = 10.0 * math.log10(SN_2)
            sigma_raw = math.sqrt(math.pow(10.0,-0.2*SN_1)
                                  + math.pow(10.0,-0.2*SN_2)
                                  + Quantization_Variance)
            # Get the pseudorange values, and check for sanity
            P1 = obs_data[i,s, P1_pos]
            P2 = obs_data[i,s, P2_pos]
            C1 = obs_data[i,s, C1_pos]
            if(P1 < MIN_PSEUDORANGE_DISTANCE):
                #if(P1 != BAD_DATA_CODE):
                #    warnings.warn("Bad pseudorange distance")
                P1 = BAD_DATA_CODE
                obs_data[i,s, P1_pos] = BAD_DATA_CODE
            if(P2 < MIN_PSEUDORANGE_DISTANCE):
                #if(P2 != BAD_DATA_CODE):
                #    warnings.warn("Bad pseudorange distance")
                P2 = BAD_DATA_CODE
                obs_data[i,s, P2_pos] = BAD_DATA_CODE
            if(C1 < MIN_PSEUDORANGE_DISTANCE):
                #if(C1 != BAD_DATA_CODE):
                #    warnings.warn("Bad pseudorange distance")
                C1 = BAD_DATA_CODE
                obs_data[i,s, C1_pos] = BAD_DATA_CODE
            # Calculate the range ionosphere value            
            # 2007 Mar 22 JMA  Is C1 more accurate than P1?  It seems to be.
            # But there are many suble issues with bias levels and
            # for some receivers, P1-P2 seems better.
            #if((MJD > MJD_OF_GPS_C1_SWITCH) and (sat < 100)):
            #    P1 = obs_data[i,s, C1_pos]
            #    if(P1 == BAD_DATA_CODE):
            #        P1 = obs_data[i,s, P1_pos]
            #else:
            if(P1 == BAD_DATA_CODE):
                P1 = C1
            if((P1 != BAD_DATA_CODE)and(P2 != BAD_DATA_CODE)):
                STEC = (P2 - P1)
                if(math.fabs(STEC) < MAX_PSEUDORANGE_DIFFERENCE):
                    #print ( "Got sat %2d for time %4d good, %.3f %.3f %E"%(sat,i,P1,P2,STEC))
                    sigma = sigma_raw
                    if((sat >= 0)and(sat < 100)):
                        STEC *= STEC_factor_GPS
                        sigma *= STEC_factor_GPS
                    elif((sat >= 100)and(sat < 200)):
                        STEC *= STEC_factor_GLONASS
                        sigma *= STEC_factor_GLONASS
                    elif((sat >= 200)and(sat < 300)):
                        STEC *= STEC_factor_Gal
                        sigma *= STEC_factor_Gal
                    else:
                        raise Albus_RINEX.RINEX_Data_Barf("Unknown satellite type %d"%sat)
                    obs_data[i,s, TP_pos] = STEC
                    obs_data[i,s, SIG_pos] = sigma
            # Calculate the ionosphere value from the phase values
            L1 = obs_data[i,s, L1_pos]
            L2 = obs_data[i,s, L2_pos]
            if((L1 != BAD_DATA_CODE)and(L2 != BAD_DATA_CODE)):
                sigma = sigma_raw
                # The correct error estimate involves the two frequencies,
                # but since they are close to each other, the error in assuming
                # they are the same for the sigma calculation is small.
                if((sat >= 0)and(sat < 100)):
                    STEC = (L1/nu_L1_GPS - L2/nu_L2_GPS)
                    STEC *= SPEED_OF_LIGHT*STEC_factor_GPS
                    sigma *= SPEED_OF_LIGHT*STEC_factor_GPS/nu_L2_GPS
                elif((sat >= 100)and(sat < 200)):
                    STEC = (L1/nu_G1_GLONASS - L2/nu_G2_GLONASS)
                    STEC *= SPEED_OF_LIGHT*STEC_factor_GLONASS
                    sigma *= SPEED_OF_LIGHT*STEC_factor_GLONASS/nu_G2_GLONASS
                elif((sat >= 200)and(sat < 300)):
                    STEC = (L1/nu_E1_Gal - L2/nu_E5_Gal)
                    STEC *= SPEED_OF_LIGHT*STEC_factor_Gal
                    sigma *= SPEED_OF_LIGHT*STEC_factor_Gal/nu_E5_Gal
                else:
                    raise Albus_RINEX.RINEX_Data_Barf("Unknown satellite type %d"%sat)
                obs_data[i,s, TL_pos] = STEC
                if(obs_data[i,s, SIG_pos] == BAD_DATA_CODE):
                    obs_data[i,s, SIG_pos] = sigma
    return












################################################################################
def bias_in_dicts(key, bias_IONEX, bias_CODE_monthly):
    """Finds the bias key in the IONEX or the monthly dictionary"""
    if(key in bias_IONEX):
        bias = bias_IONEX[key]
        # Now, check against monthly.  If wildly different, use monthly
        # Wildly different is probably 1 ns
        if(key in bias_CODE_monthly):
            bias_month = bias_CODE_monthly[key]
            if(math.fabs(bias - bias_month) > 1.0E-9):
                bias = bias_month
    elif(key in bias_CODE_monthly):
        bias = bias_CODE_monthly[key]
    else:
        bias = None
    return bias
    






################################################################################
def bias_range_wrapper(MJD, sat, station, bias_IONEX, bias_CODE_monthly,
                       bias_CODE_C1_monthly):
    """Find the appropriate bias values and neighbors for some MJD



INPUTS: 
MJD                        I  The INTEGER MJD you want to consider as
                              the center value.
sat                        I  The satellite ID number, used as the key in the
                              bias dictionaries
station                    I  the station code, used as the key in the bias
                              dictionaries.
bias_IONEX                 I  dictionary for biases from IONEX dataset,
                              in s.  The satellite dictionary keys are integer
                              values, with GLONASS satellites having 100 added to
                              them.  Station keys are names of the statiosn, with
                              '_r' appended for GLONASS, '_e' for Galileo
                              Access as bias_IONEX[MJD][0|1][key]
                              where MJD is the (integer) modified Julian Date,
                              and 0 is for satellites, 1 is for stations
bias_CODE_monthly          I  dictionary for biases from CODE monthly
                              average files, in s.
                              The satellite dictionary keys are integer
                              values, with GLONASS satellites having 100 added to
                              them.  Station keys are names of the statiosn, with
                              '_r' appended for GLONASS, '_e' for Galileo
                              Access as bias_CODE_monthly[MJD][0|1][key]
                              where MJD is the (integer) modified Julian Date,
                              and 0 is for satellites, 1 is for stations
bias_CODE_C1_monthly       I  dictionary for biases from CODE monthly
                              average P1-C1 files, in s.
                              The satellite dictionary keys are integer
                              values, with GLONASS satellites having 100 added to
                              them.  Station keys are names of the statiosn, with
                              '_r' appended for GLONASS, '_e' for Galileo
                              Access as bias_CODE_C1_monthly[MJD][0|1][key]
                              where MJD is the (integer) modified Julian Date,
                              and 0 is for satellites, 1 is for stations


OUTPUTS: P2_low, P2_mid, P2_high, C1_low, C1_mid, C1_high
P2_low                    O  The low satellite bias, in s
P2_mid                    O  the center satellite bias, in s
P2_high                   O  the high satellite bias, in s
C1_low                    O  The low P1-C2 bias, in s
C1_mid                    O  the center P1-C2 bias, in s
C1_high                   O  the high P1-C2 bias, in s
"""
    try:
    # Station P1-P2
      sta_mid = bias_in_dicts(station, bias_IONEX[MJD][1],
                            bias_CODE_monthly[MJD][1])
      if(sta_mid == None):
        sta_mid = 0.0
      sta_low = bias_in_dicts(station, bias_IONEX[MJD-1][1],
                            bias_CODE_monthly[MJD-1][1])
      if(sta_low == None):
        sta_low = sta_mid
      sta_high = bias_in_dicts(station, bias_IONEX[MJD+1][1],
                             bias_CODE_monthly[MJD+1][1])
      if(sta_high == None):
        sta_high = sta_mid
    # Satellite P1-C1
      sat_mid = bias_in_dicts(sat, bias_IONEX[MJD][0],
                            bias_CODE_monthly[MJD][0])
      if(sat_mid == None):
        sat_mid = 0.0
      sat_low = bias_in_dicts(sat, bias_IONEX[MJD-1][0],
                            bias_CODE_monthly[MJD-1][0])
      if(sat_low == None):
        sat_low = sat_mid
      sat_high = bias_in_dicts(sat, bias_IONEX[MJD+1][0],
                             bias_CODE_monthly[MJD+1][0])
      if(sat_high == None):
        sat_high = sat_mid
      P2_low = sat_low + sta_low
      P2_mid = sat_mid + sta_mid
      P2_high = sat_high + sta_high
    # Station P1-P2
      sta_mid = bias_in_dicts(station, {},
                            bias_CODE_C1_monthly[MJD][1])
      if(sta_mid == None):
        sta_mid = 0.0
      sta_low = bias_in_dicts(station, {},
                            bias_CODE_C1_monthly[MJD-1][1])
      if(sta_low == None):
          sta_low = sta_mid
      sta_high = bias_in_dicts(station, {},
                             bias_CODE_C1_monthly[MJD+1][1])
      if(sta_high == None):
        sta_high = sta_mid
      # Satellite P1-C1
      sat_mid = bias_in_dicts(sat, {},
                            bias_CODE_C1_monthly[MJD][0])
      if(sat_mid == None):
        sat_mid = 0.0
      sat_low = bias_in_dicts(sat, {},
                            bias_CODE_C1_monthly[MJD-1][0])
      if(sat_low == None):
        sat_low = sat_mid
      sat_high = bias_in_dicts(sat, {},
                             bias_CODE_C1_monthly[MJD+1][0])
      if(sat_high == None):
        sat_high = sat_mid
      C1_low = sat_low + sta_low
      C1_mid = sat_mid + sta_mid
      C1_high = sat_high + sta_high
      return P2_low, P2_mid, P2_high, C1_low, C1_mid, C1_high
    except:
      raise Albus_RINEX.RINEX_Data_Barf("failure in bias_range_wrapper function")













################################################################################
def DCB_bias_correction(MJD,
                        Sat_array,
                        obs_data,
                        station_code,
                        bias_IONEX,
                        bias_CODE_monthly,
                        bias_CODE_C1_monthly,
                        raise_bias_error=0
                        ):
    """DCB bias corrections for RINEX data read in with JMA's RINEX stuff

This function will perform standard bias corrections, using bias time
dictionaries read in from other datasets.  The satellite transmitters and
the station receivers have time biases in their signal transmission/reception,
which introduces offsets into the measurement of the propagation delay
through the ionosphere (troposphere, etc.)  These timeing biases are
supposedly relatively stable over timescales of a few days to a month, so
hopefully we can correct for this by applying a set of calibration biases
measured by some of the standard GPS groups.

Note, however, that there is also evidence that temperature affects the station
biaes, so there could be an uncorrected diurnal effect remaining.

This function assumes that you have already loaded into memory the two principle
DCB bias correction sources.  The IONEX DCB values, which are typically placed
by GPS groups for their IONEX products, should be proper for the specified day.
In addition, CODE provides a monthly average bias value for lots of stations,
in case you can't find your station in one of the IONEX files.

If the biases exist in the IONEX dictionaries, then this function uses them.
Otherwise, it tries to get them from the monthly average files from CODE.
If both of these fail, then it defaults to 0.0 for the bias.

See also JMA notes from 2006 Aug 09

Note: this DCB correction function only works on 1 day's worth of data at a time.
You should call this function for each individual day's worth of data.



INPUTS:
MJD                        I  numpy array of Modified Julian Dates for each
                              observation time as MJD[num_times]
Sat_array                  I  numpy array of satellite data positions, as
                              Sat_array[num_times,MAX_POSSIBLE_SATELLITES]
                              If the value is -1, then no observation of that
                              satellite was made
obs_data                   I  The GPS data array, as
                              obs_data[num_times, max_sat, _DATA_POS_SIZE]
                              Assuming you have found the best index i in MJD for
                              a time you desire, and you want data for satellite
                              s, then you get data from
                              obs_data[i,Sat_array[i,s], x]
                              where x is the index of the data kind you want,
                              as specified
                              in the _DATA_POS dict
station_code               I  4 letter code for a station position.
bias_IONEX                 I  dictionary for biases from IONEX dataset,
                              in s.  The satellite dictionary keys are integer
                              values, with GLONASS satellites having 100 added to
                              them.  Station keys are names of the statiosn, with
                              '_r' appended for GLONASS, '_e' for Galileo
                              Access as bias_IONEX[MJD][0|1][key]
                              where MJD is the (integer) modified Julian Date,
                              and 0 is for satellites, 1 is for stations
bias_CODE_monthly          I  dictionary for biases from CODE monthly
                              average files, in s.
                              The satellite dictionary keys are integer
                              values, with GLONASS satellites having 100 added to
                              them.  Station keys are names of the statiosn, with
                              '_r' appended for GLONASS, '_e' for Galileo
                              Access as bias_CODE_monthly[MJD][0|1][key]
                              where MJD is the (integer) modified Julian Date,
                              and 0 is for satellites, 1 is for stations
bias_CODE_C1_monthly       I  dictionary for biases from CODE monthly
                              average P1-C1 files, in s.
                              The satellite dictionary keys are integer
                              values, with GLONASS satellites having 100 added to
                              them.  Station keys are names of the statiosn, with
                              '_r' appended for GLONASS, '_e' for Galileo
                              Access as bias_CODE_C1_monthly[MJD][0|1][key]
                              where MJD is the (integer) modified Julian Date,
                              and 0 is for satellites, 1 is for stations

OUTPUTS: sta_bias_valid
sta_bias_valid             O  An array of flags to indicate whether station bias
                              information is present (1) or not (0) for this
                              day.  The array is (MAX_POSSIBLE_SATELLITES/100)
                              in length, for the known receiver systems
raise_bias_error           I  reject data with no bias correction 0 No, else yes

"""
    # print ( 'bias_IONEX',bias_IONEX)
    # print ('bias_CODE_monthly', bias_CODE_monthly)
    # print ('bias_CODE_C1_monthly', bias_CODE_C1_monthly)
    station_code = station_code.lower()
    C1_pos = _DATA_POS['C1']
    P2_pos = _DATA_POS['P2']
    L2_pos = _DATA_POS['L2']
    NUM_OBS = obs_data.shape[0]
    assert(MJD[NUM_OBS-1] - MJD[0] < 1.5)
    MJD_MID = int(MJD[NUM_OBS//2])
    MJD_MID_TIME = MJD_MID + 0.5
    for sat in range(MAX_POSSIBLE_SATELLITES):
        # get the station bias
        if(sat < 100):
            this_station_code = station_code
            nu_2 = nu_L2_GPS
        elif(sat < 200):
            this_station_code = station_code + '_r'
            nu_2 = nu_G2_GLONASS
        elif(sat < 300):
            this_station_code = station_code + '_e'
            nu_5 = nu_E5_Gal
        else:
            raise Albus_RINEX.RINEX_Data_Barf("Unknown satellite type for satellite %d"%sat)
        P2_low, P2_mid, P2_high, C1_low, C1_mid, C1_high = \
               bias_range_wrapper(MJD_MID, sat, this_station_code,
                                  bias_IONEX, bias_CODE_monthly,
                                  bias_CODE_C1_monthly)
        # The total bias is the satellite plus the station bias
        total_bias = P2_mid
        slope = P2_mid - P2_low
        total_C1_bias = C1_mid
        slope_C1 = C1_mid - C1_low
        # Now check for any observations
        NEXT_START = NUM_OBS
        for i in range(NUM_OBS):
            s = Sat_array[i,sat]
            if(s >= 0):
                if(MJD[i] > MJD_MID_TIME):
                    NEXT_START = i
                    break
                if(obs_data[i,s, P2_pos] != BAD_DATA_CODE):
                    obs_data[i,s, P2_pos] += SPEED_OF_LIGHT \
                         * (total_bias + slope * (MJD[i] - MJD_MID_TIME))
                if(obs_data[i,s, L2_pos] != BAD_DATA_CODE):
                    obs_data[i,s, L2_pos] -=  nu_2 \
                         * (total_bias + slope * (MJD[i] - MJD_MID_TIME))
                if(obs_data[i,s, C1_pos] != BAD_DATA_CODE):
                    obs_data[i,s, C1_pos] += SPEED_OF_LIGHT \
                        * (total_C1_bias + slope_C1 *(MJD[i] - MJD_MID_TIME))
        slope = P2_high - P2_mid
        slope_C1 = C1_high - C1_mid
        for i in range(NEXT_START, NUM_OBS):
            s = Sat_array[i,sat]
            if(s >= 0):
                if(obs_data[i,s, P2_pos] != BAD_DATA_CODE):
                    obs_data[i,s, P2_pos] += SPEED_OF_LIGHT \
                         * (total_bias + slope * (MJD[i] - MJD_MID_TIME))
                if(obs_data[i,s, L2_pos] != BAD_DATA_CODE):
                    obs_data[i,s, L2_pos] -=  nu_2 \
                         * (total_bias + slope * (MJD[i] - MJD_MID_TIME))
                if(obs_data[i,s, C1_pos] != BAD_DATA_CODE):
                    obs_data[i,s, C1_pos] += SPEED_OF_LIGHT \
                        * (total_C1_bias + slope_C1 *(MJD[i] - MJD_MID_TIME))
    # Now, figure out whether station bias information was present or not
    sta_bias_valid = np.zeros((MAX_POSSIBLE_SATELLITES//100),dtype='int32')+1
    if (bias_in_dicts(station_code, bias_IONEX[MJD_MID][1],bias_CODE_monthly[MJD_MID][1]) is None):
        sta_bias_valid[0] = 0
        if DEBUG_SET and raise_bias_error:
          print ( '******************** rejecting data with no bias corection!!! ********')
          raise Albus_RINEX.RINEX_Data_Barf("Station '%s' has no bias correction"%station_code)
    if (bias_in_dicts(station_code + '_r', bias_IONEX[MJD_MID][1],bias_CODE_monthly[MJD_MID][1]) is None):
        sta_bias_valid[1] = 0
    if (bias_in_dicts(station_code + '_e', bias_IONEX[MJD_MID][1],bias_CODE_monthly[MJD_MID][1]) is None):
        sta_bias_valid[2] = 0
    return sta_bias_valid







################################################################################
def correct_for_cycle_slips_0(Sat_array, obs_data, sat, i, nu_1_slip, nu_2_slip,
                              diff, adiff, end_pos, TL_pos):
    """correct for cycle slips internal code

INPUTS:
Sat_array  I  numpy array of satellite data positions, as
              Sat_array[NUM_OBS,MAX_POSSIBLE_SATELLITES]
              If the value is -1, then no observation of that satellite was made
obs_data   I  The GPS data array, as
              obs_data[NUM_OBS, max_sat, _DATA_POS_SIZE]
              Assuming you have found the best index i in MJD for
              a time you desire, and you want data for satellite s, then
              you get data from
              obs_data[i,Sat_array[i,s], x]
              where x is the index of the data kind you want, as specified
              in the _DATA_POS dict
sat        I  The satellite number to try to fix
i          I  Position at which to start fixing data
nu_1_slip  I  How much does frequency 1 slip
nu_2_slip  I  How much does frequency 2 slip
diff       I  value of slip
adiff      I  magnitude of slip
end_pos    I  Where to stp correcting data
TL_pos     I  Position in obs_data array for L data

OUTPUTS: None
    """
    Num_1 = adiff / nu_1_slip
    Num_2 = adiff / nu_2_slip
    # int truncates!
    diff_1 = int(Num_1*2.0 + 0.5)*0.5 - Num_1
    diff_2 = int(Num_2*2.0 + 0.5)*0.5 - Num_2
    slip = 0.0
    if(math.fabs(diff_1) < math.fabs(diff_2)):
        slip = int(Num_1*2.0 + 0.5)*0.5 * nu_1_slip
    else:
        slip = int(Num_2*2.0 + 0.5)*0.5 * nu_2_slip
    if(diff > 0):
        slip = -slip
    adiff = math.fabs(diff + slip)
    if(adiff > 0.2) :
        # try a few different variations
        for xx1 in range(-2,3):
            for xx2 in range(-2,3):
                ss = xx1*0.5*nu_1_slip + xx2*0.5*nu_2_slip
                xxdiff = math.fabs(diff+ss)
                if(xxdiff < adiff):
                    #print ( "%3d %3d %10.3f    %10.3f"%(xx1,xx2,xxdiff,diff+slip))
                    adiff = xxdiff
                    slip = ss
    #print ( "Got internal difference at %d %10.3f %10.3f %10.3f %10.3f %10.3f"%(i+1,diff, slip, diff+slip,Num_1, Num_2))
    for j in range(i,end_pos):
        s = Sat_array[j,sat]
        if(s < 0):
            continue
        L = obs_data[j,s, TL_pos]
        if(L != BAD_DATA_CODE):
            obs_data[j,s, TL_pos] = L + slip
    return




################################################################################
def correct_for_phase_data_gaps(Sat_array, obs_data, sat, NUM_OBS, work):
    """Use the phase STEC to search for small time gaps and fix them

Note: intended to be run during correct_for_cycle_slips_1

The carrier phase is usually a much better measured quantity, but
suffers from cycle slips on rare occasions.  This can lead to errors
on the order of 2 TECU at times, or greatly affect averages over large
periods.  Search for these over small gaps in data (MAX_SPIP_POINTS, 1
or 2 timesteps normally) where the receiver may have been out or lost
phase track for a moment.  Note that because many of the new GPS
receivers are storing data at much faster intervals and then being
spliced together, there are often artificial gaps in the data with no
cycle slips, so check for that too.

At the same time, this function will blank out any phase data where there are
only fewer than MIN_CONTIGUOUS_POINTS_GOOD datapoints.  This typically happens
at the extreme low elevation angle ranges where the data are noisy anyway, and
it is best to scrap them.


Sat_array  I  numpy array of satellite data positions, as
              Sat_array[NUM_OBS,MAX_POSSIBLE_SATELLITES]
              If the value is -1, then no observation of that satellite was made
obs_data   I  The GPS data array, as
              obs_data[NUM_OBS, max_sat, _DATA_POS_SIZE]
              Assuming you have found the best index i in MJD for
              a time you desire, and you want data for satellite s, then
              you get data from
              obs_data[i,Sat_array[i,s], x]
              where x is the index of the data kind you want, as specified
              in the _DATA_POS dict
sat        I  The satellite number to try to fix
NUM_OBS    I  The number of observations
work       W  a double numpy array of size work[NUM_OBS] for workspace

OUTPUTS: sat_found, block_pos
sat_found  O  0 means no satellite data, 1 means there is satellite data for sat
              but no L phase data, and 2 means there is full data
block_pos  O  An array of continguous block position ranges, as 2 element
              arrays.  So, block_pos[i][0] is the start position of the block
              and block_pos[i][1] is the end position (standard Python 1 past)
              for block. i
"""
    if(sat >= MAX_POSSIBLE_SATELLITES):
        raise Albus_RINEX.RINEX_Data_Barf("Unsupported satellite number %d, contact software provider"%(sat))
    TP_pos = _DATA_POS['STECP']
    TL_pos = _DATA_POS['STECL']
    if(sat < 100):
        nu_1_slip = SPEED_OF_LIGHT * STEC_factor_GPS / nu_L1_GPS
        nu_2_slip = SPEED_OF_LIGHT * STEC_factor_GPS / nu_L2_GPS
    elif(sat < 200):
        nu_1_slip = SPEED_OF_LIGHT * STEC_factor_GLONASS / nu_G1_GLONASS
        nu_2_slip = SPEED_OF_LIGHT * STEC_factor_GLONASS / nu_G2_GLONASS
    else:
        nu_1_slip = None
        nu_2_slip = None
    block_pos = []
    #print ( "working on satellite %d in fix gaps"%sat)
    # start by scrapping the short blocks of data)
    sat_found = 0
    start_pos = 0
    end_pos = 0
    last_L = BAD_DATA_CODE
    for i in range(NUM_OBS):
        work[i] = BAD_DATA_CODE
        s = Sat_array[i,sat]
        if(s < 0):
            if(last_L != BAD_DATA_CODE):
                end_pos = i
                num = end_pos - start_pos
                for j in range(start_pos,end_pos):
                    work[j] = num
            last_L = BAD_DATA_CODE
            continue
        if(sat_found == 0):
            if(obs_data[i,s, TP_pos] != BAD_DATA_CODE):
                sat_found = 1
        L = obs_data[i,s, TL_pos]
        #print ( i,L,start_pos)
        if(L == BAD_DATA_CODE):
            if(last_L != BAD_DATA_CODE):
                end_pos = i
                num = end_pos - start_pos
                for j in range(start_pos,end_pos):
                    work[j] = num
            last_L = BAD_DATA_CODE
            continue
        if(last_L == BAD_DATA_CODE):
            # This position good, the previous position was bad.  Update
            start_pos = i
            if(start_pos - end_pos <= MAX_SPIP_POINTS):
                if(end_pos != 0):
                    # this is small enough to be a skip area
                    for j in range(end_pos,start_pos):
                        work[j] = SKIP_DATA_CODE
        else:
            # Look for huge slips
            diff = L - last_L
            adiff = math.fabs(diff)
            if(adiff > 5.0):
                correct_for_cycle_slips_0(Sat_array, obs_data, sat, i,
                                          nu_1_slip, nu_2_slip,
                                          diff, adiff, NUM_OBS, TL_pos)
                L = obs_data[i,s, TL_pos]
        last_L = L
        sat_found = 2
    if(last_L != BAD_DATA_CODE):
        end_pos = NUM_OBS
        num = end_pos - start_pos
        for j in range(start_pos,end_pos):
            work[j] = num
    #print ( "In correcting phase gaps, finished first loop")
#    for i in xrange(NUM_OBS):
#        if(work[i] != BAD_DATA_CODE):
#            print ( i, work[i], sat)
    if(sat_found < 2):
        block_pos = [[0, NUM_OBS]]
        return sat_found, block_pos
    # Now remove all short segments
    i=0
    while (i < NUM_OBS):
        if(work[i] < 0.0):
            i += 1
            continue
        start_pos = i
        while ((i < NUM_OBS) and (work[i] != BAD_DATA_CODE)):
            i += 1
        end_pos = i
        num = end_pos - start_pos
        if(num < MIN_CONTIGUOUS_POINTS_GOOD):
            # too few points, blank
            #print ( "blanking short segment from %d to %d"%(start_pos,end_pos))
            for j in range(start_pos, end_pos):
                s = Sat_array[j,sat]
                if(s >= 0):
                    obs_data[j,s, TL_pos] = BAD_DATA_CODE
                work[j] = BAD_DATA_CODE
        else:
            # remember this area
            #print ( "saving segment from %d to %d"%(start_pos,end_pos))
            pos = [start_pos, end_pos]
            block_pos.append(pos)
        i += 1
        continue
    for pos in block_pos:
        # are there any gaps?
        start_pos = pos[0]
        end_pos = pos[1]
        this_start = start_pos
        while(this_start < end_pos):
            this_end = this_start + int(work[this_start])
            if(this_end != end_pos):
                # there must be a gap
                next_start = this_end +1
                next_end = None
                while(next_start <  end_pos):
                    if(work[next_start] > 0):
                        next_end = next_start + int(work[next_start])
                        break
                    next_start += 1
                    continue
                #print ( "gap found from %d to %d"%(this_end, next_start))
                # Assume that points are equidistant in time
                upper_intercept = None
                lower_intercept = None
                midpoint = 0.5 * (next_start + this_end -1)
                if(next_end - next_start > 1):
                    # extrapolate linearly
                    Ll = obs_data[next_start,Sat_array[next_start,sat], TL_pos]
                    Lu = obs_data[next_start+1,Sat_array[next_start+1,sat], TL_pos]
                    slope = Lu - Ll
                    intercept = Ll - slope * (next_start - midpoint)
                    upper_intercept = intercept
                else:
                    # only 1 point, take same
                    upper_intercept = obs_data[next_start,Sat_array[next_start,sat], TL_pos]
                if(this_end - this_start > 1):
                    # extrapolate linearly
                    Ll = obs_data[this_end-2,Sat_array[this_end-2,sat], TL_pos]
                    Lu = obs_data[this_end-1,Sat_array[this_end-1,sat], TL_pos]
                    slope = Lu - Ll
                    intercept = Ll - slope * (this_end -1 - midpoint)
                    lower_intercept = intercept
                else:
                    # only 1 point, take same
                    lower_intercept = obs_data[this_end-1,Sat_array[this_end-1,sat], TL_pos]
                diff = upper_intercept - lower_intercept
                adiff = math.fabs(diff)
                Num_1 = adiff / nu_1_slip
                Num_2 = adiff / nu_2_slip
                # int truncates!
                diff_1 = int(Num_1*2.0 + 0.5)*0.5 - Num_1
                diff_2 = int(Num_2*2.0 + 0.5)*0.5 - Num_2
                slip = 0.0
                if(math.fabs(diff_1) < math.fabs(diff_2)):
                    slip = int(Num_1*2.0 + 0.5)*0.5 * nu_1_slip
                else:
                    slip = int(Num_2*2.0 + 0.5)*0.5 * nu_2_slip
                if(diff > 0.0):
                    slip = -slip
                adiff = math.fabs(diff + slip)
                #print ( "Got first block difference at %d %10.3f %10.3f %10.3f %10.3f %10.3f"%(next_start,diff, slip, diff+slip,Num_1, Num_2))
                if(adiff > 0.2) :
                    # try a few different variations
                    for xx1 in range(-2,3):
                        for xx2 in range(-2,3):
                            ss = xx1*0.5*nu_1_slip + xx2*0.5*nu_2_slip
                            xxdiff = math.fabs(diff+ss)
                            if(xxdiff < adiff):
                                #print ( "%3d %3d %10.3f    %10.3f"%(xx1,xx2,xxdiff,diff+slip))
                                adiff = xxdiff
                                slip = ss
                #print ( "Got block difference for sat %3d at %4d %10.3f %10.3f %10.3f %10.3f %10.3f"%(sat,next_start,diff, slip, diff+slip,Num_1, Num_2))
                if(slip != 0.0):
                    for j in range(next_start,end_pos):
                        s = Sat_array[j,sat]
                        if(s < 0):
                            continue
                        L = obs_data[j,s, TL_pos]
                        if(L != BAD_DATA_CODE):
                            obs_data[j,s, TL_pos] = L + slip
            this_start = this_end
            while(this_start < end_pos):
                if(work[this_start] > 0):
                    break
                this_start += 1
                continue
    return sat_found, block_pos
    








################################################################################
def correct_for_cycle_slips_1(Sat_array, obs_data, sat, NUM_OBS, work):
    """Use the phase STEC to search for cycle slips

Note: intended to be run during calculate_phase_corrected_STEC

The carrier phase is usually a much better measured quantity, but suffers
from cycle slips on rare occasions.  This can lead to errors on the order of
2 TECU at times, or greatly affect averages over large periods.  Search for these
and fix.


Sat_array  I  numpy array of satellite data positions, as
              Sat_array[NUM_OBS,MAX_POSSIBLE_SATELLITES]
              If the value is -1, then no observation of that satellite was made
obs_data   I  The GPS data array, as
              obs_data[NUM_OBS, max_sat, _DATA_POS_SIZE]
              Assuming you have found the best index i in MJD for
              a time you desire, and you want data for satellite s, then
              you get data from
              obs_data[i,Sat_array[i,s], x]
              where x is the index of the data kind you want, as specified
              in the _DATA_POS dict
sat        I  The satellite number to try to fix
NUM_OBS    I  The number of observations
work       W  a double numpy array of size work[NUM_OBS] for workspace

OUTPUTS: sat_found, block_pos
sat_found  O  0 means no satellite data, 1 means there is satellite data for sat
              but no L phase data, and 2 means there is full data
block_pos  O  An array of continguous block position ranges, as 2 element
              arrays.  So, block_pos[i][0] is the start position of the block
              and block_pos[i][1] is the end position (standard Python 1 past)
              for block. i
"""
    if(sat >= MAX_POSSIBLE_SATELLITES):
        raise Albus_RINEX.RINEX_Data_Barf("Unsupported satellite number %d, contact software provider"%(sat))
    # check for any data at all
    if(np.sum(Sat_array[:,sat]) == -NUM_OBS):
        return 0, []
    TP_pos = _DATA_POS['STECP']
    TL_pos = _DATA_POS['STECL']
    if(sat < 100):
        nu_1_slip = SPEED_OF_LIGHT * STEC_factor_GPS / nu_L1_GPS
        nu_2_slip = SPEED_OF_LIGHT * STEC_factor_GPS / nu_L2_GPS
    elif(sat < 200):
        nu_1_slip = SPEED_OF_LIGHT * STEC_factor_GLONASS / nu_G1_GLONASS
        nu_2_slip = SPEED_OF_LIGHT * STEC_factor_GLONASS / nu_G2_GLONASS
    else:
        nu_1_slip = None
        nu_2_slip = None
    #print ( "slip levels %E %E"%(nu_1_slip,nu_2_slip))
    sat_found, block_pos = correct_for_phase_data_gaps(Sat_array, obs_data, sat,
                                                       NUM_OBS, work)
    if(sat_found < 2):
        return sat_found, block_pos
    # Now form difference array in workspace
    work[:] = BAD_DATA_CODE
    num = 0
    sum = 0.0
    sum_sqr = 0.0
    for pos in block_pos:
        last_L = BAD_DATA_CODE
        # are there any gaps?
        start_pos = pos[0]
        end_pos = pos[1]
        for i in range(start_pos,end_pos):
            s = Sat_array[i,sat]
            if(s < 0):
                last_L = BAD_DATA_CODE
                continue
            L = obs_data[i,s, TL_pos]
            if(L == BAD_DATA_CODE):
                last_L = BAD_DATA_CODE
                continue
            if(last_L != BAD_DATA_CODE):
                diff = L - last_L
                work[i] = diff
                sum += diff
                sum_sqr += diff*diff
                num += 1
            last_L = L
    if(num < 2):
        return sat_found, block_pos
    ave = sum / num
    std_dev = (sum_sqr - sum*sum / num) / (num-1)
    if(std_dev > 0.0):
        std_dev = math.sqrt(std_dev)
    else:
        # no changes, so no slips
        return sat_found, block_pos
    if(num > 10):
        diff_sort = np.sort(work.copy())
        start_d = -1
        end_d = len(diff_sort)
        for index in range(end_d):
            if(diff_sort[index] > BAD_DATA_CODE):
                start_d = index
                break
        else:
            # No good data
            return sat_found, block_pos
        count = end_d - start_d
        three_percent = int(0.03*count)
        if(three_percent < 0):
            three_percent = 1
        diff_sort = diff_sort[start_d + three_percent:-three_percent]
        if(len(diff_sort) > 1):
            ave = diff_sort.mean()
            std_dev = diff_sort.std()
    # look for points more than 5-sigma away from average
    # note that the math of what I do is actually
    # \sqrt{3/2}\sigma, and \sqrt{3/2} \approx 1.2247
    # But don't check for absurdly small slips
    check_level = 1.2247 * 5.0 * std_dev
    #print ( "In cycle check, check for sat %3d is %E"%(sat,check_level))
    if((check_level < 0.35 * nu_1_slip) and (check_level < 0.35 * nu_2_slip)):
        if(nu_1_slip < nu_2_slip):
            check_level = 0.35 * nu_1_slip
        else:
            check_level = 0.35 * nu_2_slip
    #print ( "In cycle check, check for sat %3d is %E"%(sat,check_level))
    for pos in block_pos:
        # are there any gaps?
        start_pos = pos[0]
        end_pos = pos[1]
        for i in range(start_pos,end_pos):
            if(work[i] == BAD_DATA_CODE):            
                start_pos = i+1
                if((i > 1) and (work[i-1] != BAD_DATA_CODE)
                   and (work[i-2] != BAD_DATA_CODE)):
                    # can check for differencing
                    diff = work[i-1] - work[i-2]
                    adiff = math.fabs(diff)
                    if((adiff > check_level) and (math.fabs(work[i-2]) < 3.0*std_dev)):
                        correct_for_cycle_slips_0(Sat_array, obs_data, sat, i-1,
                                                  nu_1_slip, nu_2_slip,
                                                  diff, adiff, end_pos, TL_pos)
                continue
            if(i - start_pos >= 2):
                # can check for differencing
                ave = (work[i] + work[i-2]) * 0.5
                diff = work[i-1] - ave
                adiff = math.fabs(diff)
                if((adiff > check_level)
                   and (math.fabs(work[i]-work[i-2]) < 3.0*std_dev)
                   and (math.fabs(work[i]) < 5.0*std_dev)):
                    correct_for_cycle_slips_0(Sat_array, obs_data, sat, i-1,
                                              nu_1_slip, nu_2_slip,
                                              diff, adiff, end_pos, TL_pos)
                    work[i-1] = ave
            if(i - start_pos >= 3):
                ave = (work[i-3] + work[i-2]) * 0.5
                diff = work[i-1] - ave
                adiff = math.fabs(diff)
                if((adiff > 2.0*check_level)
                   and (math.fabs(work[i-2]-work[i-3]) < 3.0*std_dev)
                   and (math.fabs(work[i-2]) < 5.0*std_dev)):
                    # Whoa, a 10-\sigma event, so fix
                    correct_for_cycle_slips_0(Sat_array, obs_data, sat, i-1,
                                              nu_1_slip, nu_2_slip,
                                              diff, adiff, end_pos, TL_pos)
                    work[i-1] = ave
            if((i == start_pos) and (i < NUM_OBS-1)
                 and (work[i+1] != BAD_DATA_CODE)):
                # can check for differencing
                diff = work[i] - work[i+1]
                adiff = math.fabs(diff)
                if((adiff > check_level) and (math.fabs(work[i+1])<3.0*std_dev)):
                    correct_for_cycle_slips_0(Sat_array, obs_data, sat, i,
                                              nu_1_slip, nu_2_slip,
                                              diff, adiff, end_pos, TL_pos)
            if((i == end_pos-1) and (i > start_pos)):
                diff = work[i] - work[i-1]
                adiff = math.fabs(diff)
                if((adiff > check_level) and (math.fabs(work[i-1])<3.0*std_dev)):
                    correct_for_cycle_slips_0(Sat_array, obs_data, sat, i,
                                              nu_1_slip, nu_2_slip,
                                              diff, adiff, end_pos, TL_pos)
                
    return sat_found, block_pos

                
            
        
        
        



# import Albus_RINEX_2
# MJD,S2, O1, to, sta_XYZ = Albus_RINEX_2.read_RINEX_obs_file("wtzz1940.06o")
# MJDs,Os = Albus_RINEX_2.read_RINEX_sp3_file("cod13834.eph")
# sat_XYZ = Albus_RINEX_2.interpolate_sat_positions2(MJDs,Os,MJD)
# Albus_RINEX_2.fill_in_obs_AzEl_values(MJD,S2,O1,sta_XYZ, sat_XYZ)
# Albus_RINEX_2.calculate_STECs(S2, O1)
# Albus_RINEX_2.calculate_phase_corrected_STEC(S2, O1)
#
# reload(Albus_RINEX_2)


################################################################################
def calculate_phase_corrected_STEC(Sat_array, obs_data):
    """Use the phase STEC to smooth out the pseudo-range STEC

Note: must have run calculate_STECs before this function.

The carrier phase is usually a much better measured quantity, but suffers
from having unknown phase offsets.  The code range information is more
noisy, but is more accurate.  So use the average code to carrier
difference to correct the carrier range each time the satellite comes up.

Note: This routine needs to be improved to try to detect cycle slips.


Sat_array  I  numpy array of satellite data positions, as
              Sat_array[num_times,MAX_POSSIBLE_SATELLITES]
              If the value is -1, then no observation of that satellite was made
obs_data   I  The GPS data array, as
              obs_data[num_times, max_sat, _DATA_POS_SIZE]
              Assuming you have found the best index i in MJD for
              a time you desire, and you want data for satellite s, then
              you get data from
              obs_data[i,Sat_array[i,s], x]
              where x is the index of the data kind you want, as specified
              in the _DATA_POS dict

OUTPUTS: sat_block_pos
sat_block_pos O  An array of continguous block position ranges, as 3 element
              arrays.  So, sat_block_pos[s][i][0] is the start position
              of the block and sat_block_pos[s][i][1] is the end position
              (standard Python 1 past) for block i for satellite s
"""
    TP_pos = _DATA_POS['STECP']
    SIG_pos = _DATA_POS['SIGMA']
    TL_pos = _DATA_POS['STECL']
    PL_pos = _DATA_POS['STECPL']
    NUM_OBS = obs_data.shape[0]
    work = np.zeros((NUM_OBS), dtype='float64')
    sat_avg_arr = [None]*MAX_POSSIBLE_SATELLITES
    sat_block_pos = [None]*MAX_POSSIBLE_SATELLITES
    total_sat_count = 0
    for sat in range(MAX_POSSIBLE_SATELLITES):
        code, block_pos = correct_for_cycle_slips_1(Sat_array, obs_data, sat,
                                                    NUM_OBS, work)
        if(code == 0):
            continue
        sat_block_pos[sat] = block_pos
        if(code == 1):
            # There is no L phase information, so just copy over the pseudorange
            sum = 0.0
            count = 0
            for i in range(NUM_OBS):
                s = Sat_array[i,sat]
                if(s < 0):
                    continue
                obs_data[i,s, PL_pos] = obs_data[j,s, TP_pos]
                if(obs_data[i,s, SIG_pos] != BAD_DATA_CODE):
                    obs_data[i,s, SIG_pos] += 10.0 # rough guess for TECU uncert
                    sum += obs_data[i,s, PL_pos]
                    count += 1
            average = np.zeros((1), dtype='float64')
            if(count):
                average[0] = sum/count
                total_sat_count += 1
            else:
                average[0] = BAD_DATA_CODE
            sat_avg_arr[sat] = average
            continue
        average = np.zeros((len(block_pos)), dtype='float64')
        average += BAD_DATA_CODE
        for pos in range(len(block_pos)):
            start_pos = block_pos[pos][0]
            end_pos = block_pos[pos][1]
            sum_diff = 0.0
            sum_d2 = 0.0
            sum_weight = 0.0
            for i in range(start_pos, end_pos):
                s = Sat_array[i,sat]
                if(s < 0):
                    continue
                P = obs_data[i,s, TP_pos]
                L = obs_data[i,s, TL_pos]
                if((P == BAD_DATA_CODE)or(L == BAD_DATA_CODE)):
                    continue
                weight = 1.0 / obs_data[i,s, SIG_pos]
                diff = P-L
                sum_diff += diff*weight
                sum_d2 += diff*diff*weight
                sum_weight += weight
            if(sum_weight == 0.0):
                # no data here
                continue
            diff_ave = sum_diff / sum_weight
            if(end_pos > start_pos+1):
                diff_var = (sum_d2 - sum_diff*sum_diff / sum_weight) / sum_weight
                if(diff_var < 0.0):
                    diff_var = 0.0
                # Note that because of the weighting scheme, there are fewer
                # effective points than the simple count, so fudge by 2.0
                diff_uncert = math.sqrt(diff_var / (end_pos - start_pos) *2.0)
                if(end_pos > start_pos+50):
                    pass
                else:
                    # For very short segments, which typically show up at low
                    # elevations, the bias characteristics seem to behave
                    # strangely.  Bump up the uncertaitny
                    diff_uncert += 3.0
            else:
                diff_uncert = 10.0 # JMA guess for typical uncertainty when
                                  # only 1 measurement available (normally at
                                  # low elevation, so ionosphere bad)
            #print ( "for sat %3d, estimated phase bias error is %E from %4d points %4d--%4d"%(sat,diff_uncert,(end_pos - start_pos),start_pos,end_pos))
            sum = 0.0
            count = 0
            for j in range(start_pos, end_pos):
                s = Sat_array[j,sat]
                if(s < 0):
                    continue
                if(obs_data[j,s, TL_pos] == BAD_DATA_CODE):
                    continue
                obs_data[j,s, PL_pos] = obs_data[j,s, TL_pos] + diff_ave
                obs_data[j,s, SIG_pos] += diff_uncert
                sum += obs_data[j,s, PL_pos]
                count += 1
            if(count):
                average[pos] = sum/count
                total_sat_count += 1
            else:
                average[pos] = BAD_DATA_CODE
        sat_avg_arr[sat] = average
    # Now check that the average satellite values are reasonable
    if(total_sat_count < 3):
        return sat_block_pos
    average_arr = np.zeros((total_sat_count), dtype='float64')
    count = 0
    for sat in range(MAX_POSSIBLE_SATELLITES):
        if((sat_avg_arr[sat] is not None) and (sat_block_pos[sat] is not None)):
            for pos in range(len(sat_avg_arr[sat])):
                #print ( "For sat %3d I have average value from pos %2d of %10.3f from %4d to %4d"%(sat,pos,sat_avg_arr[sat][pos],sat_block_pos[sat][pos][0],sat_block_pos[sat][pos][1]))
                if(sat_avg_arr[sat][pos] != BAD_DATA_CODE):
                    average_arr[count] = sat_avg_arr[sat][pos]
                    count += 1
    #print ( "Got %d sat averages from %d total"%(count,total_sat_count))
    average_use = average_arr.copy()
    average_use = np.sort(average_use)
    if(count >= 20):
        ten_percent = int(0.1*count)
        average_use = average_use[ten_percent:-ten_percent]
    elif(count > 5):
        average_use = average_use[1:-1]
    mean = average_use.mean()
    std_dev = average_use.std()
    #print ( "For this receiver, got mean %10.3f std_dev %10.3f"%(mean,std_dev))
    # Find all satellites more than 4-\sigma from mean (plus a fudge factor)
    check = 4.0*std_dev + 10.0
    for sat in range(MAX_POSSIBLE_SATELLITES):
        if((sat_avg_arr[sat] is not None) and (sat_block_pos[sat] is not None)):
            for pos in range(len(sat_avg_arr[sat])):
                if((sat_avg_arr[sat][pos] == BAD_DATA_CODE) or
                   (math.fabs(sat_avg_arr[sat][pos] - mean) > check)):
                    #print ( "sat %3d fails mean test, with %10.3f %10.3f"%(sat,sat_avg_arr[sat][pos] - mean,check))
                    for i in range(sat_block_pos[sat][pos][0],
                                    sat_block_pos[sat][pos][1]):
                        s = Sat_array[i,sat]
                        if(s < 0):
                            continue
                        obs_data[i,s, SIG_pos] = BAD_DATA_CODE
    return sat_block_pos


    




################################################################################
def concatenate_MJDs(MJDs):
    """Concatenate a set of modified Julian Date numpy arrays into a single one.

You will frequently want to do GPS things over more than 1 day, but the RINEX
data files are typically only 1 day long.  This function helps to concatenate
the returned arrays.  You really ought to go in time order for this stuff.
And try to not skip days.

Data is concatenated in the order called in the function, so the
caller is responsible for getting things right.

This one is specifically for MJDs.  It will check for increasing time order, and
for no gaps larger than 1 hour.  If somethign is out of order, it only
produces warnings.

INPUTS:
MJDs      I  list[] of the MJD numpy arrays you want to concat.

OUTPUTS: MJD_out
MJD_OUT   O  the whole numpy array of MJD values
"""
    assert(len(MJDs) > 0)
    NUM_TOTAL = 0
    for M in MJDs:
        NUM_TOTAL += len(M)
    MJD_OUT = np.zeros((NUM_TOTAL), dtype='float64')
    last_val = MJDs[0][0]
    gap = 0
    hour = 1.0 / 24.0
    reverse = 0
    pos = 0
    counter = -1
    for M in MJDs:
        counter = counter + 1
        l = len(M)
        MJD_OUT[pos:pos+l] = M[:]
        pos += l
        for i in range(l):
            val = M[i]
            if(val < last_val):
                reverse = 1
            if(val > last_val + hour):
                gap = 1
            last_val = val
    if(gap):
        warnings.warn("Time gap of more than 1 hour in MJD concatenation")
    if(reverse):
        warnings.warn("MJD times are not increasing in MJD concatenation")
    return MJD_OUT


################################################################################
def concatenate_Sat_Arrs(Sat_Arrs):
    """Concatenate a set of satellite observation numpy arrays into a single one.

You will frequently want to do GPS things over more than 1 day, but the RINEX
data files are typically only 1 day long.  This function helps to concatenate
the returned arrays.  You really ought to go in time order for this stuff.
And try to not skip days.

Data is concatenated in the order called in the function, so the
caller is responsible for getting things right.

This one is specifically for Sat_Arrays, which hold information about which
satellites were observed at which times, and where they are in the compact data
formats.  These arrays are generated by read_RINEX_obs_file as the Sat_array
product.

INPUTS:
Sat_Arrs      I  list[] of the satellite arrays to concatenate

OUTPUTS: MJD_out
Sat_Arr_OUT   O  the whole numpy array of satellite stuff
                 numpy array of satellite data positions, as
                 Sat_array[num_times,MAX_POSSIBLE_SATELLITES]
                 If the value is -1, then no observation of that satellite
                 was made
"""
    assert(len(Sat_Arrs) > 0)
    NUM_TOTAL = 0
    for S in Sat_Arrs:
        sh = S.shape
        NUM_TOTAL += sh[0]
        assert(sh[1] == MAX_POSSIBLE_SATELLITES)
    Sat_Arr_OUT = np.zeros((NUM_TOTAL, MAX_POSSIBLE_SATELLITES),
                                 dtype='int16')
    pos = 0
    for S in Sat_Arrs:
        l = S.shape[0]
        Sat_Arr_OUT[pos:pos+l,:] = S[:,:]
        pos += l
    return Sat_Arr_OUT


################################################################################
def concatenate_Obs_Datas(Obs_Datas):
    """Concatenate a set of GPS observation numpy arrays into a single one.

You will frequently want to do GPS things over more than 1 day, but the RINEX
data files are typically only 1 day long.  This function helps to concatenate
the returned arrays.  You really ought to go in time order for this stuff.
And try to not skip days.

Data is concatenated in the order called in the function, so the
caller is responsible for getting things right.

This one is specifically for Obs_Datas, which hold the the actual observation
data for each time, for each of the satellites above the horizon.  These
arrays are produced by read_RINEX_obs_file and modified by several other
routines to fill in extra information.  

INPUTS:
Obs_Datas  I  list[] of the satellite arrays to concatenate

OUTPUTS: MJD_out
Obs_Data_OUT O  the whole numpy array of observation stuff
              The output data array, as
              obs_data[num_times, max_sat, _DATA_POS_SIZE]
              Assuming you have found the best index i in MJD for
              a time you desire, and you want data for satellite s, then
              you get data from
              obs_data[i,Sat_array[i,s], x]
              where x is the index of the data kind you want, as specified
              in the _DATA_POS dict
"""
    assert(len(Obs_Datas) > 0)
    NUM_TOTAL = 0
    NUM_MAX = 0
    for o in Obs_Datas:
        sh = o.shape
        NUM_TOTAL += sh[0]
        if(NUM_MAX < sh[1]):
            NUM_MAX = sh[1]
        assert(sh[2] == _DATA_POS_SIZE)
    Obs_Data_OUT = np.zeros((NUM_TOTAL, NUM_MAX, _DATA_POS_SIZE),
                                  dtype='float64')
    Obs_Data_OUT += BAD_DATA_CODE
    pos = 0
    for o in Obs_Datas:
        sh = o.shape
        l = sh[0]
        w = sh[1]
        Obs_Data_OUT[pos:pos+l,0:w,:] = o[:,:,:]
        pos += l
    return Obs_Data_OUT


################################################################################
def concatenate_Sat_Obss(Sat_Obss):
    """Concatenate a set of GPS observation numpy arrays into a single one.

You will frequently want to do GPS things over more than 1 day, but the RINEX
data files are typically only 1 day long.  This function helps to concatenate
the returned arrays.  You really ought to go in time order for this stuff.
And try to not skip days.

Data is concatenated in the order called in the function, so the
caller is responsible for getting things right.

This one is specifically for the satellite position obs_data information which
is produced by read_RINEX_sp3_file.  These data contain the satellite positions
about the center of the Earth for discrete times for all GPS satellites
hopefully.  This function assumes that you are giving the satellite
positions in time order, and adjusts the \phi value to make a continually
increasing sequence to aid with interpolation to get positions at intermediate
times.


INPUTS:
Sat_Obss   I  list[] of the satellite arrays to concatenate

OUTPUTS: MJD_out
Sat_Obss_OUT O  the whole numpy array of satellite observation stuff
              The output data array, as
              obs_data[num_times, MAX_POSSIBLE_SATELLITES, 6]
              Assuming you have found the best index i in MJD for
              a time you desire, and you want data for satellite s, then
              you get data from
              obs_data[i,s, x]
              where x is the index of the XYZ value you want, X=0, Y=1, Z=2.
              r=3, \phi=4,\theta=5
              If X==Y==Z==0, then no data.
"""
    assert(len(Sat_Obss) > 0)
    NUM_TOTAL = 0
    for S in Sat_Obss:
        sh = S.shape
        NUM_TOTAL += sh[0]
        assert(sh[1] == MAX_POSSIBLE_SATELLITES)
        assert(sh[2] == 6)
    Sat_Obss_OUT = np.zeros((NUM_TOTAL, MAX_POSSIBLE_SATELLITES, 6),
                                  dtype='float64')
    pos = 0
    for S in Sat_Obss:
        l = S.shape[0]
        Sat_Obss_OUT[pos:pos+l,:,:] = S[:,:,:]
        pos += l
    _clean_up_phi_terms(Sat_Obss_OUT)
    return Sat_Obss_OUT


################################################################################
def calc_station_position_max_shift(sta_XYZs):
    """find the maximum distance difference among a group of station positions

When you read individual station RINEX files for each day, each gives back
an approximate station XYZ position.  But these positions change slightly
from day to day (possibly) as people update the positions or move
the receivers a bit.  This function takes in a bunch of the reported
station positions, and calculates the maximum difference in position, in m.

For now, 500 m is probably a reasonable estimate of a maximum limit to allow
for shifts for a single station.

INPUTS:
sta_XYZs     I  list[] of station positions as numpy arrays of X,Y,Z coordinates

OUTPUTS:
max_dist     O  the maximum station position change, in m
"""
    NUM_TOTAL =  len(sta_XYZs)
    assert(NUM_TOTAL > 0)
    max_dist = 0.0
    for i in range(NUM_TOTAL):
        p_i = sta_XYZs[i]
        for j in range(i+1,NUM_TOTAL):
            d = p_i - sta_XYZs[j]
            diff = math.sqrt(d[0]*d[0] + d[1]*d[1] + d[2]*d[2])
            if(diff > max_dist):
                max_dist = diff
    return max_dist





################################################################################
def check_MJD_arrays_differ(MJD_1, MJD_2, epsilon = 1.0):
    """check whether two modified julian date arrays are the same or not

In dealing with the RINEX data, it will often happen that you get a
modified Julian Date array from different things, such as different
station data files.  But you also frequently end up with the same arrays
fom different stations, and so on.  Some of the calculations, such as finding
the satellite positions at every MJD value, are expensive, so you don't want
to recalculate them unless absolutely necessary.

This function figures out whether two MJD arrays are sufficiently
the same such that recalculations are not necessary.

INPUTS:
MJD_1      I  the first array of MJD values
MJD_2      I  the second array of MJD values
epsilon    I  The amount of fudge time to allow the two arrays to have
              individual values differ by, but still be considered
              to be the same, in s

OUTPUTS: ret_code
ret_core   O  the return code.
               0  the same
              -1  arrays differ in size
              +1  arrays have different times
"""
    assert((len(MJD_1.shape)==1) and (len(MJD_2.shape) ==1))
    NUM_DATA = len(MJD_1)
    if(NUM_DATA != len(MJD_2)):
        #print ( len(MJD_1), len(MJD_2))
        return -1
#   epsilon *= DAYS_PER_SECOND 
    for i in range(NUM_DATA):
        diff = math.fabs(MJD_1[i] - MJD_2[i]) * SECONDS_PER_DAY
#       print ( "MJD calc diff at seq number ", i, diff, MJD_1[i], MJD_2[i])
        if (diff > epsilon):
#         print ( "Warning: MJD calc diff greater than nominal allowed value")
#         print ( "Testing if greater than double nominal allowed value")
          if(diff > 2* epsilon):
            print ( "Failure: MJD error greater than twice allowable difference ", i, diff, MJD_1[i], MJD_2[i])
            return +1
#       else:
#           print ( "MJD diff acceptable")
    return 0




################################################################################
def convert_Sat_array_to_small_array(Sat_array,
                                     obs_data):
    """Convert a Sat_array to a smaller size array with obs_data behavior

The Sat_array is a large integer array which has values for all possible
satellites, to indicate which column of the obs_data area belongs to that
satellite (or no columns if given as -1).  This takes up a huge area of
memory, but is convenient for some purposes.

But the ionosphere fitting stuff does not need this huge array.  So shrink
it down to the obs_data format.



INPUTS:
Sat_array    I  numpy array of satellite data positions, as
                Sat_array[num_times,MAX_POSSIBLE_SATELLITES]
                If the value is -1, then no observation of that satellite was
                made
obs_data     I  The output data array, as
                obs_data[num_times, max_sat, _DATA_POS_SIZE]
                Assuming you have found the best index i in MJD for
                a time you desire, and you want data for satellite s, then
                you get data from
                obs_data[i,Sat_array[i,s], x]
                where x is the index of the data kind you want, as specified
                in the _DATA_POS dict

OUTPUTS: Sat_small
Sat_small    O  numpy array of satellite data positions, as
                Sat_array[num_times, max_sat]
                If the value is -1, then no observation is present for that
                position in obs_data.  If >= 0, this gives the satellite number.
"""
    osh = obs_data.shape
    ssh = Sat_array.shape
    assert(osh[0] == ssh[0])
    Sat_small = np.zeros((osh[0],osh[1]), dtype='int16')
    Sat_small -= 1
    for t in range(osh[0]):
        for s in range(ssh[1]):
            col = Sat_array[t,s]
            if(col >= 0):
                Sat_small[t,col] = s
    return Sat_small

################################################################################
def convert_sat_block_pos_to_small_array(sat_block_pos, Sat_array, obs_data):
    """Convert a Sat_array to a smaller size array with obs_data behavior

The sat_block_pos object holds information about the locations of individual
satellite tracks for each satellite for a singel receiver.  This is stored
in a compact form of start and end positions.  But this is not so convenient
for later processign where specific queries are made by single time indexes,
rather than sequential processing.  So convert to a compact form.

This data is normally only useful in conjunction with the information from
convert_Sat_array_to_small_array.



INPUTS:
sat_block_pos  I  An array of continguous block position ranges, as 3 element
                arrays.  So, sat_block_pos[s][i][0] is the start position
                of the block and sat_block_pos[s][i][1] is the end position
                (standard Python 1 past) for block i for satellite s
Sat_array    I  numpy array of satellite data positions, as
                Sat_array[num_times,MAX_POSSIBLE_SATELLITES]
                If the value is -1, then no observation of that satellite was
                made
obs_data     I  The output data array, as
                obs_data[num_times, max_sat, _DATA_POS_SIZE]
                Assuming you have found the best index i in MJD for
                a time you desire, and you want data for satellite s, then
                you get data from
                obs_data[i,Sat_array[i,s], x]
                where x is the index of the data kind you want, as specified
                in the _DATA_POS dict

OUTPUTS: Sat_small
block_small  O  numpy array of satellite track positions, as
                block_small[num_times, max_sat]
                If the value is -1, then no observation is present for that
                position in obs_data.  If >= 0, this gives the satellite track,
                for the specific satellite observed.
"""
    osh = obs_data.shape
    ssh = Sat_array.shape
    assert(osh[0] == ssh[0])
    assert(ssh[1] == len(sat_block_pos))
    block_small = np.zeros((osh[0],osh[1]), dtype='int16')
    block_small -= 1
    for s in range(ssh[1]):
        if(sat_block_pos[s] is None):
            continue
        for p in range(len(sat_block_pos[s])):
            for t in range(sat_block_pos[s][p][0],sat_block_pos[s][p][1]):
                col = Sat_array[t,s]
                if(col >= 0):
                    block_small[t,col] = p
    return block_small







################################################################################
def get_satellite_ephemeris_positions(MJD_start,
                                      MJD_end,
                                      output_directory = ".",
                                      overwrite = 0
                                      ):
    """get the RINEX ephemeris satellite positions for all sats for timerange

This function will get the GPS satellite ephemeris positions, normally
associated with the RINEX sp3 datasets, for all GPS (and GLONASS and Galileo)
satellites, for the time range specified.

The sp3 files are stored at many places.  They are generated by many groups.
But, at this time (2006 Aug 08), only the CODE group appears to regularly
include the satellite positions for GLONASS as well as GPS.  So use their data,
if possible.

This is the ephermiers position stuff, which is (as of 2006 Aug 08) given
every 15 minutes.  You will need to make an interpolation in this dataset
to get the satellite positions for the typical RINEX observation files with
30 s (or faster) sampling rates.

Note that the time range is actually extended as much as necessary to cover full
days.

If the routine cannot find data from the FTP sites, this routine throws
a DataError exception

INPUTS:
MJD_start   I  The MJD of the starting time needed
MJD_end     I  The MJD of the ending time needed
output_directory  I  Where to put things
overwrite   I  May files be overwritten?  0 No, else yes

OUTPUTS: sat_MJD, sat_pos
sat_MJD     O  numpy array of Modified Julian Dates for each ephemeris time
               as MJD[num_times]
sat_pos     O  The output data array, as
               sat_pos[num_times, MAX_POSSIBLE_SATELLITES, 6]
               Assuming you have found the best index i in MJD for
               a time you desire, and you want data for satellite s, then
               you get data from
               obs_data[i,s, x]
               where x is the index of the XYZ value you want, X=0, Y=1, Z=2.
               r=3, \phi=4,\theta=5
               If X==Y==Z==0, then no data.
"""
    assert(MJD_start <= MJD_end)
    MJD_start = int(MJD_start) # int truncates!
    MJD_end = int(MJD_end) + 1
    sat_MJD_list = []
    sat_pos_list = []
    for m in range(MJD_start, MJD_end):
        year, month, day, frac = jma_tools.get_ymdf_from_JD(jma_tools.get_JD_from_MJD(m))
        doy = jma_tools.get_day_of_year(year, month, day)
        gps_week, gps_dow, gps_seconds = jma_tools.get_GPS_from_MJD(m)
#       print ( 'MJD, gps_week, gps_dow, gps_seconds, year, doy', m, gps_week, gps_dow, gps_seconds, year, doy)
        home_dir = os.path.expanduser('~')
        file = home_dir + '/.netrc'
        if os.path.isfile(file): # we can access CDDIS
           group_name = 'cod'
        else:
           group_name = 'jpl'
        group_name = 'cod'
        best_estimate = False
        filename = Albus_RINEX.make_RINEX_ephemeris_filename(group_name, best_estimate, gps_week, gps_dow, year, doy)
        ret_code = Albus_RINEX.get_GPS_ephemeris_file_from_web(filename,year,
                                                               gps_week,
                                                               output_directory,
                                                               overwrite)
        if(ret_code < 0):
            best_estimate = True
            filename = Albus_RINEX.make_RINEX_ephemeris_filename(group_name, best_estimate, gps_week, gps_dow, year, doy)
            ret_code = Albus_RINEX.get_GPS_ephemeris_file_from_web(filename,year,
                                                               gps_week,
                                                               output_directory,
                                                               overwrite)
        if(ret_code < 0):
            print ( '*****' )
            print ( 'Unable to continue: could not download needed ephemeris data!!!!')
            print ( '*****' )
            raise Albus_RINEX.RINEX_Data_Barf("Unable to continue: could not download ephemeris data for MJD %d"%m)
            os._exit(2)
        filename = output_directory + '/' + filename
        MJD, pos = read_RINEX_sp3_file(filename)
        sat_MJD_list.append(MJD)
        sat_pos_list.append(pos)
    sat_MJD = concatenate_MJDs(sat_MJD_list)
    sat_pos = concatenate_Sat_Obss(sat_pos_list)
    return sat_MJD, sat_pos







################################################################################
def get_station_base_file(MJD,
                          station_code,
                          output_directory = ".",
                          overwrite = 0
                          ):
    """get the RINEX file of station observations for some station for time MJD

This function 

If the routine cannot find data from the FTP sites, this routine throws
a DataError exception.


INPUTS:
MJD          I  The MJD of the day needed
station_code I  4 letter code for a station position.
output_directory  I  Where to put things
overwrite    I  May files be overwritten?  0 No, else yes

OUTPUTS: data_file
data_file    O  The name of the RINEX datafile on disk

Note that if a failure happens, this routine will raise an exception, most
likely of the form Albus_RINEX.No_RINEX_File_Error
"""
    MJD = int(MJD) # int truncates!
    year, month, day, frac = jma_tools.get_ymdf_from_JD(jma_tools.get_JD_from_MJD(MJD))
    doy = jma_tools.get_day_of_year(year, month, day)
    # Now try to get the compressed observation file for this day
    rinex_parameters = Albus_RINEX.make_RINEX_filename(station_code,'d', year, doy)
    data_comp =  rinex_parameters[0]
    try:
        Albus_RINEX.get_RINEX_obs_file_from_web(rinex_parameters,year,month,day,
                                                doy,
                                                output_directory,
                                                overwrite = overwrite)
        data_comp = output_directory + '/' + data_comp
        assert(os.path.isfile(data_comp))
    except Albus_RINEX.No_RINEX_File_Error:
        raise Albus_RINEX.No_RINEX_File_Error("Cannot find observation data for MJD %d station '%s'"%(m,station_code))
    # check o data file
    data_file = data_comp[0:-1] + 'o'
    if((os.path.isfile(data_file)) and (not overwrite)):
        pass
    else:
        if(os.path.isfile(data_file)):
            warnings.warn("File '%s' already exists.  Deleting."%data_file)
            os.remove(data_file)
        # make a command string to uncompress the data
        command = "crx2rnx %s - > %s"%(data_comp,data_file)
        retcode = os.system(command)
        if(retcode):
            raise Albus_RINEX.No_RINEX_File_Error("Could not run '%s'"%command)
    return data_file












################################################################################
def get_station_base_observations(MJD_start,
                                  MJD_end,
                                  station_code,
                                  output_directory = ".",
                                  overwrite = 0,
                                  raise_bias_error=0
                                  ):
    """get the RINEX STEC observations for some station for some time range

This function gets the slant TEC information and other possibly useful
information from RINEX observation files for a specified station for a specified
time range.  The routine will calculate slant TEC values from the GPS
measurements in the RINEX data, and apply the standard corrections for
DCB offsets, and calculate an initial code distance to carrier phase offset.

The caller is responsible for filling in the elevation and azimuth data.

Note that the time range is actually extended as much as necessary to cover full
days.

If the routine cannot find data from the FTP sites, this routine throws
a DataError exception.

This function holds a bunch of static data for station and satellite frequency
offsets.  If you would like to clear this information, call the routine with
MJD_start = 0.0
MJD_end = 0.0
station_code = '////'




INPUTS:
MJD_start    I  The MJD of the starting time needed
MJD_end      I  The MJD of the ending time needed
station_code I  4 letter code for a station position.
output_directory  I  Where to put things
overwrite    I  May files be overwritten?  0 No, else yes
rasie_bias_error I reject data with no bias correction 0 No, else yes

OUTPUTS: sta_MJD, Sat_array, obs_data, sta_XYZ, sta_bias_valid, sat_block_pos
sta_MJD      O  numpy array of Modified Julian Dates for each observation time
                as MJD[num_times]
Sat_array    O  numpy array of satellite data positions, as
                Sat_array[num_times,MAX_POSSIBLE_SATELLITES]
                If the value is -1, then no observation of that satellite was
                made
obs_data     O  The output data array, as
                obs_data[num_times, max_sat, _DATA_POS_SIZE]
                Assuming you have found the best index i in MJD for
                a time you desire, and you want data for satellite s, then
                you get data from
                obs_data[i,Sat_array[i,s], x]
                where x is the index of the data kind you want, as specified
                in the _DATA_POS dict
sta_XYZ      O  Cartesian station position in Earth centered coodriantes, in m
                A numpy array array.
sta_bias_valid O  An array of flags to indicate whether station bias
                  information is present (1) or not (0).
                  The array is (MAX_POSSIBLE_SATELLITES/100)
                  in length, for the known receiver systems
sat_block_pos  O  An array of continguous block position ranges, as 3 element
                arrays.  So, sat_block_pos[s][i][0] is the start position
                of the block and sat_block_pos[s][i][1] is the end position
                (standard Python 1 past) for block i for satellite s
"""
    assert(MJD_start <= MJD_end)
    if((MJD_start == 0.0)and(MJD_end == 0.0)):
        if(station_code == '////'):
            # special code to clear static variables to free memory
            get_station_base_observations.bias_IONEX = {}
            get_station_base_observations.bias_CODE_monthly = {}
            get_station_base_observations.bias_CODE_C1_monthly = {}
    MJD_start = int(MJD_start) # int truncates!
    MJD_end = int(MJD_end) + 1
    for m in range(MJD_start-1, MJD_end+1):
        year, month, day, frac = jma_tools.get_ymdf_from_JD(jma_tools.get_JD_from_MJD(m))
        doy = jma_tools.get_day_of_year(year, month, day)
        gps_week, gps_dow, gps_seconds = jma_tools.get_GPS_from_MJD(m)

        # check for IONEX bias data
        if(m in get_station_base_observations.bias_IONEX):
            pass
        else:
            group_name = 'cod'
            IONEX_name = Albus_RINEX.make_IONEX_filename(group_name,0,gps_week,year,doy)
            print ( 'initial IONEX_name', IONEX_name)
            return_code = Albus_RINEX.get_IONEX_file_from_web(IONEX_name,
                                                              year, month,
                                                              day, doy,
                                                              output_directory,
                                                              overwrite = overwrite)
            if(return_code < 0):
                IONEX_name = Albus_RINEX.make_IONEX_filename(group_name,1,gps_week,year,doy)
                print ( 'second IONEX_name', IONEX_name)
                return_code = Albus_RINEX.get_IONEX_file_from_web(IONEX_name,
                                                                  year, month,
                                                                  day, doy,
                                                                  output_directory,
                                                                  overwrite = overwrite)
            if(return_code >= 0):
                IONEX_filename = output_directory + '/' + IONEX_name
                print ( 'IONEX_filename + directory ', IONEX_filename)
                print ( 'rinex_2 calling find_DCB_info_from_IONEX')
                sat,sta = Albus_RINEX.find_DCB_info_from_IONEX(IONEX_filename)
            else:
                sat = {}
                sta = {}
            get_station_base_observations.bias_IONEX[m] = [sat,sta]
        # check for CODE P1P2 DCB correction
        # Store this by the MJD as well, even though this will waste some space.
        # It is not *too* much space to waste.
        if(m in get_station_base_observations.bias_CODE_monthly):
            pass
        else:
            P1P2_filename = Albus_RINEX.get_CODE_P1P2_file_from_web(year, month,
                                                                    'P2',
                                                                    output_directory,
                                                                    overwrite)
            if(P1P2_filename is not None):
                sat,sta = Albus_RINEX.find_DCB_info_from_CODE_P1P2(P1P2_filename)
            else:
                sat = {}
                sta = {}
            get_station_base_observations.bias_CODE_monthly[m] = [sat,sta]
        # Now check for CODE P1C1 DCB correction
        if(m in get_station_base_observations.bias_CODE_C1_monthly):
            pass
        elif(MJD_start > MJD_OF_GPS_C1_SWITCH):
            P1C1_filename = Albus_RINEX.get_CODE_P1P2_file_from_web(year, month,
                                                                    'C1',
                                                                    output_directory,
                                                                    overwrite)
            if(P1C1_filename is not None):
                sat,sta = Albus_RINEX.find_DCB_info_from_CODE_P1P2(P1C1_filename)
            else:
                sat = {}
                sta = {}
            get_station_base_observations.bias_CODE_C1_monthly[m] = [sat,sta]
    # Now for the observation data
    sta_MJD_list = []
    Sat_array_list = []
    obs_data_list = []
    sta_XYZ_list = []
    sta_bias_valid = np.zeros((MAX_POSSIBLE_SATELLITES//100),dtype='int32')+1
    for m in range(MJD_start, MJD_end):
        year, month, day, frac = jma_tools.get_ymdf_from_JD(jma_tools.get_JD_from_MJD(m))
        doy = jma_tools.get_day_of_year(year, month, day)
        # Now try to get the compressed observation file for this day
        rinex_parms = Albus_RINEX.make_RINEX_filename(station_code,'d', year, doy)
        data_comp = rinex_parms[0]
        try:
            Albus_RINEX.get_RINEX_obs_file_from_web(rinex_parms, year,month,day,
                                                    doy,
                                                    output_directory,
                                                    overwrite = overwrite)
            data_comp = output_directory + '/' + data_comp
            assert(os.path.isfile(data_comp))
        except Albus_RINEX.No_RINEX_File_Error:
            raise Albus_RINEX.No_RINEX_File_Error("Cannot find observation data for MJD %d station '%s'"%(m,station_code))
        # check o data file
        data_file = data_comp[0:-1] + 'o'
        if((os.path.isfile(data_file)) and (not overwrite)):
            pass
        else:
            if(os.path.isfile(data_file)):
                warnings.warn("File '%s' already exists.  Deleting."%data_file)
                os.remove(data_file)
            # make a command string to uncompress the data
            command = "crx2rnx %s - > %s"%(data_comp,data_file)
            retcode = os.system(command)
            if(retcode):
                raise Albus_RINEX.No_RINEX_File_Error("Could not run '%s'"%command)
        # Ok, now read in the data
        MJD, Sat_array, obs_data,time_offset,XYZ = \
             read_RINEX_obs_file(data_file, m, One_Day_Limit=1)
        # perform the bias corrections
        bias_valid = DCB_bias_correction(MJD,
                                         Sat_array,
                                         obs_data,
                                         station_code,
                                         get_station_base_observations.bias_IONEX,
                                         get_station_base_observations.bias_CODE_monthly,
                                         get_station_base_observations.bias_CODE_C1_monthly,
                                         raise_bias_error
                                         )
        sta_MJD_list.append(MJD)
        Sat_array_list.append(Sat_array)
        obs_data_list.append(obs_data)
        sta_XYZ_list.append(XYZ)
        for v in range(len(sta_bias_valid)):
            sta_bias_valid[v] = sta_bias_valid[v] & bias_valid[v]
    # Combine everything together
    sta_MJD = concatenate_MJDs(sta_MJD_list)
    Sat_array = concatenate_Sat_Arrs(Sat_array_list)
    obs_data = concatenate_Obs_Datas(obs_data_list)
    if(calc_station_position_max_shift(sta_XYZ_list) < 100.0):
        sta_XYZ = sta_XYZ_list[-1]
    else:
        raise Albus_RINEX.RINEX_Data_Barf("Hey, the station is moving, maximum station difference is %.3f meters"%calc_station_position_max_shift(sta_XYZ_list))
    # Now start calculating the TEC stuff
    calculate_STECs(Sat_array, obs_data,MJD_start)
    sat_block_pos = calculate_phase_corrected_STEC(Sat_array, obs_data)
    return sta_MJD, Sat_array, obs_data, sta_XYZ, sta_bias_valid, sat_block_pos
get_station_base_observations.bias_IONEX = {}
get_station_base_observations.bias_CODE_monthly = {}
get_station_base_observations.bias_CODE_C1_monthly = {}


















###################################
###################################
######### Code to write intermediate files
###################################






################################################################################
def _read_Albus_MJD(MJD_start, MJD_end, station_code,
                    input_directory="."):
    """read in an ALbus binary MJD array file from disk

INPUTS:
MJD_start    I  The MJD of the starting time needed
MJD_end      I  The MJD of the ending time needed
station_code I  String of letters indicating the origin of the times
input_directory  I  Where to get things

OUTPUTS: MJD
MJD          O  A numpy array of MJD times.  If no valid data, this comes back as
                None
    """
    MJD = None
    filename = "%s/%s.%d.%d.MJD"%(input_directory,station_code,MJD_start,MJD_end)
    if(not os.path.isfile(filename)):
        return MJD
    Num_El = np.memmap(filename, mode="r", dtype='uint32',shape=(1,))
    MJD_SIZE = Num_El[0]
    del Num_El;
    MJDf = np.memmap(filename, mode="r", offset=4,dtype='float64',shape=(MJD_SIZE,))
    MJD = MJDf.copy()
    del MJDf;
    return MJD

################################################################################
def _write_Albus_MJD(MJD, MJD_start, MJD_end, station_code,
                    output_directory=".", overwrite=0):
    """write out an ALbus binary MJD array file to disk

INPUTS:
MJD          I  A numpy array of MJD times.  If no valid data, this comes back as
                None
MJD_start    I  The MJD of the starting time needed
MJD_end      I  The MJD of the ending time needed
station_code I  String of letters indicating the origin of the times
output_directory  I  Where to put things
overwrite    I  May files be overwritten?  0 No, else yes

OUTPUTS: None
    """
    filename = "%s/%s.%d.%d.MJD"%(output_directory,station_code,MJD_start,MJD_end)
    if(os.path.isfile(filename)):
        if(overwrite):
            warnings.warn("File '%s' already exists.  Deleting."%filename)
            os.remove(filename)
        else:
            return
    sh = MJD.shape
    MJD_SIZE = sh[0]
# write out the header
    Num_El= np.memmap(filename, mode="w+", shape=(1,),dtype='uint32')
    Num_El[0] = sh[0];
    del Num_El
# write out the data
    MJD_out =np.memmap(filename, mode="r+", shape=(sh[0],),dtype='float64',offset=4)
    MJD_out[:] = MJD[:]
    del MJD_out
    return





################################################################################
def _read_Albus_sat_XYZ(MJD_start, MJD_end,
                        input_directory="."):
    """read in an ALbus binary satellite position file from disk

INPUTS:
MJD_start    I  The MJD of the starting time needed
MJD_end      I  The MJD of the ending time needed
input_directory  I  Where to get things

OUTPUTS: sat_MJD, sat_XYZ
sat_MJD      O  A numpy array of MJD times corresponding to the satellite positions
                If no valid data, this comes back as None
sat_XYZ      O  numpy array of the satellite positions
                as XYZ[need_times,MAX_POSSIBLE_SATELLITES,6]
                x=0,y=1,z=2.  If x==y==z==0, then no data
                r=3,\phi=4,\theta=5
                If no valid data, this comes back as None
    """
    station_code = "sat"
    sat_MJD = _read_Albus_MJD(MJD_start, MJD_end, station_code,
                              input_directory)
    if(sat_MJD is None):
        return None, None
    filename = "%s/%s.%d.%d.XYZ"%(input_directory,station_code,MJD_start,MJD_end)
    if(not os.path.isfile(filename)):
        return None, None
        # read the header
    Num_Elf =np.memmap(filename, mode="r", shape=(3,),dtype='uint32')
    Num_El = Num_Elf.copy()
    assert(Num_El[0] == len(sat_MJD))
    assert(Num_El[1] == MAX_POSSIBLE_SATELLITES)
    assert(Num_El[2] == 6)
        #XYZ_SIZE = Num_El[0]*MAX_POSSIBLE_SATELLITES*6*8
    del Num_Elf
    m_fp = np.memmap(filename, mode="r", shape=(Num_El[0],Num_El[1],Num_El[2]),dtype='float64',offset=12)
# read the data
    sat_XYZ = m_fp.copy()
    del m_fp
    return sat_MJD, sat_XYZ

################################################################################
def _write_Albus_sat_XYZ(sat_MJD, sat_XYZ, MJD_start, MJD_end,
                         output_directory=".", overwrite=0):
    """write out an ALbus binary satellite position file to disk

INPUTS:
sat_MJD      I  A numpy array of MJD times corresponding to the satellite positions
                If no valid data, this comes back as None
sat_XYZ      I  numpy array of the satellite positions
                as XYZ[need_times,MAX_POSSIBLE_SATELLITES,6]
                x=0,y=1,z=2.  If x==y==z==0, then no data
                r=3,\phi=4,\theta=5
                If no valid data, this comes back as None
MJD_start    I  The MJD of the starting time needed
MJD_end      I  The MJD of the ending time needed
output_directory  I  Where to put things
overwrite    I  May files be overwritten?  0 No, else yes

OUTPUTS: None
    """
    sh = sat_XYZ.shape
    assert(sh[0] == len(sat_MJD))
    assert(sh[1] == MAX_POSSIBLE_SATELLITES)
    assert(sh[2] == 6)
    station_code = "sat"
    _write_Albus_MJD(sat_MJD, MJD_start, MJD_end, station_code,
                    output_directory, overwrite)
    filename = "%s/%s.%d.%d.XYZ"%(output_directory,station_code,MJD_start,MJD_end)
    if(os.path.isfile(filename)):
        if(overwrite):
            warnings.warn("File '%s' already exists.  Deleting."%filename)
            os.remove(filename)
        else:
            return
    XYZ_SIZE = sh[0]*MAX_POSSIBLE_SATELLITES*6*8
# write out the header
    Num_El = np.memmap(filename,mode="w+",shape=(3,),dtype='uint32')
    Num_El[0] = sh[0]
    Num_El[1] = sh[1]
    Num_El[2] = sh[2]
    del Num_El
        # write out the data
    sat_out = np.memmap(filename,mode="r+",shape=(sh[0],sh[1],sh[2]),dtype='float64',offset=12)
    sat_out[:,:,:] = sat_XYZ[:,:,:]
    del sat_out;
    return





################################################################################
def _read_Albus_sat_array(MJD_start, MJD_end, station_code,
                          input_directory="."):
    """read in an ALbus binary satellite index file from disk

INPUTS:
MJD_start    I  The MJD of the starting time needed
MJD_end      I  The MJD of the ending time needed
station_code I  String of letters indicating the RINEX receiver
input_directory  I  Where to get things

OUTPUTS: Sat_array
Sat_array    O  numpy array of satellite data positions, as
                Sat_array[num_times,MAX_POSSIBLE_SATELLITES]
                If the value is -1, then no observation of that satellite was
                made
                If no valid data, this comes back as None
    """
    filename = "%s/%s.%d.%d.sat"%(input_directory,station_code,MJD_start,MJD_end)
    if(not os.path.isfile(filename)):
        return None
    m_fp = np.memmap(filename,mode="r", shape=(2,),dtype='uint32')
        # read the header
    Num_El = m_fp.copy()
    #print ( Num_El);
    assert(Num_El[1] == MAX_POSSIBLE_SATELLITES)
    del m_fp;
    m_fp = np.memmap(filename,mode="r",shape=(Num_El[0],Num_El[1]),
                          dtype='int16',offset=8)
    # read the data
    Sat_array = m_fp.copy()
    del m_fp
    return Sat_array

################################################################################
def _write_Albus_sat_array(Sat_array, MJD_start, MJD_end, station_code,
                           output_directory=".", overwrite=0):
    """write out an ALbus binary satellite position file to disk

INPUTS:
Sat_array    I  numpy array of satellite data positions, as
                Sat_array[num_times,MAX_POSSIBLE_SATELLITES]
                If the value is -1, then no observation of that satellite was
                made
MJD_start    I  The MJD of the starting time needed
MJD_end      I  The MJD of the ending time needed
station_code I  String of letters indicating the RINEX receiver
output_directory  I  Where to put things
overwrite    I  May files be overwritten?  0 No, else yes

OUTPUTS: None
    """
    sh = Sat_array.shape
    assert(sh[1] == MAX_POSSIBLE_SATELLITES)
    filename = "%s/%s.%d.%d.sat"%(output_directory,station_code,MJD_start,MJD_end)
    if(os.path.isfile(filename)):
        if(overwrite):
            warnings.warn("File '%s' already exists.  Deleting."%filename)
            os.remove(filename)
        else:
            return
 
    # write out the header
    Num_El = np.memmap(filename,mode="w+", shape=(2,),dtype='uint32');
    Num_El[0] = sh[0]
    Num_El[1] = sh[1]
    del Num_El;
# write out the data
    sat_out = np.memmap(filename,mode="r+", shape=(sh[0],sh[1]),dtype='int16',offset=8);
    sat_out[:,:] = Sat_array[:,:]
    del sat_out;
    return





################################################################################
def _read_Albus_obs_data(MJD_start, MJD_end, station_code,
                         input_directory="."):
    """read in an ALbus binary RIENX observation file from disk

INPUTS:
MJD_start    I  The MJD of the starting time needed
MJD_end      I  The MJD of the ending time needed
station_code I  String of letters indicating the RINEX receiver
input_directory  I  Where to get things

OUTPUTS: obs_data
obs_data     O  The output data array, as
                obs_data[num_times, max_sat, _DATA_POS_SIZE]
                Assuming you have found the best index i in MJD for
                a time you desire, and you want data for satellite s, then
                you get data from
                obs_data[i,Sat_array[i,s], x]
                where x is the index of the data kind you want, as specified
                in the _DATA_POS dict
                If no valid data, this comes back as None
    """
    filename = "%s/%s.%d.%d.obs"%(input_directory,station_code,MJD_start,MJD_end)
    if(not os.path.isfile(filename)):
        return None
    # read the header
    m_fp = np.memmap(filename,mode="r", shape=(3,),dtype='uint32')
    Num_El = m_fp.copy()
    assert(Num_El[2] == _DATA_POS_SIZE)
    del m_fp
    m_fp = np.memmap(filename,mode="r", shape=(Num_El[0],Num_El[1], _DATA_POS_SIZE),dtype='float64',offset=12)
    # read the data
    obs_data = m_fp.copy()
    del m_fp;
    return obs_data

################################################################################
def _write_Albus_obs_data(obs_data, MJD_start, MJD_end, station_code,
                          output_directory=".", overwrite=0):
    """write out an ALbus binary RINEX observation file to disk

INPUTS:
obs_data     I  The output data array, as
                obs_data[num_times, max_sat, _DATA_POS_SIZE]
                Assuming you have found the best index i in MJD for
                a time you desire, and you want data for satellite s, then
                you get data from
                obs_data[i,Sat_array[i,s], x]
                where x is the index of the data kind you want, as specified
                in the _DATA_POS dict
MJD_start    I  The MJD of the starting time needed
MJD_end      I  The MJD of the ending time needed
station_code I  String of letters indicating the RINEX receiver
output_directory  I  Where to put things
overwrite    I  May files be overwritten?  0 No, else yes

OUTPUTS: None
    """
    sh = obs_data.shape
    assert(sh[2] == _DATA_POS_SIZE)
    filename = "%s/%s.%d.%d.obs"%(output_directory,station_code,MJD_start,MJD_end)
    if(os.path.isfile(filename)):
        if(overwrite):
            warnings.warn("File '%s' already exists.  Deleting."%filename)
            os.remove(filename)
        else:
            return
    # write out the header
    Num_El = np.memmap(filename,mode="w+",shape=(3,),dtype='uint32')
    Num_El[0] = sh[0]
    Num_El[1] = sh[1]
    Num_El[2] = _DATA_POS_SIZE
    del Num_El;
        # write out the data
    obs_out = np.memmap(filename,mode="r+",shape=(sh[0],sh[1],_DATA_POS_SIZE),dtype='float64',offset=12)
    obs_out[:,:,:] = obs_data[:,:,:]
    del obs_out
    return





################################################################################
def _read_Albus_obs_XYZ(MJD_start, MJD_end, station_code,
                        input_directory="."):
    """read in an ALbus binary RIENX observation position file from disk

INPUTS:
MJD_start    I  The MJD of the starting time needed
MJD_end      I  The MJD of the ending time needed
station_code I  String of letters indicating the RINEX receiver
input_directory  I  Where to get things

OUTPUTS: sta_XYZ, sta_bias_valid
sta_XYZ      O  Cartesian station position in Earth centered coodriantes, in m
                A numpy array array.
                If no valid data, this comes back as None
sta_bias_valid O  An array of flags to indicate whether station bias
                information is present (1) or not (0).
                The array is (MAX_POSSIBLE_SATELLITES/100)
                in length, for the known receiver systems
                If no valid data, this comes back as None
    """
    filename = "%s/%s.%d.%d.XYZ"%(input_directory,station_code,MJD_start,MJD_end)
    if(not os.path.isfile(filename)):
        return None, None
    BIAS_LENGTH = int(MAX_POSSIBLE_SATELLITES/100)
    # read the position
    m_fp = np.memmap(filename,mode="r", shape=(6,),dtype='float64')
    sta_XYZ = m_fp.copy()
    del m_fp;
    # read the bias descriptors
    m_fp = np.memmap(filename,mode="r", shape=(BIAS_LENGTH,),dtype='int32',offset=6*8)
    sta_bias_valid = m_fp.copy();
    del m_fp;
    return sta_XYZ, sta_bias_valid

################################################################################
def _write_Albus_obs_XYZ(sta_XYZ, sta_bias_valid, MJD_start, MJD_end,
                         station_code,
                         output_directory=".", overwrite=0):
    """write out an ALbus binary RINEX observation position file to disk

INPUTS:
sta_XYZ      I  Cartesian station position in Earth centered coodriantes, in m
                A numpy array array.
sta_bias_valid I  An array of flags to indicate whether station bias
                  information is present (1) or not (0).
                  The array is (MAX_POSSIBLE_SATELLITES/100)
                  in length, for the known receiver systems
MJD_start    I  The MJD of the starting time needed
MJD_end      I  The MJD of the ending time needed
station_code I  String of letters indicating the RINEX receiver
output_directory  I  Where to put things
overwrite    I  May files be overwritten?  0 No, else yes

OUTPUTS: None
    """
    assert(len(sta_XYZ) == 6)
    BIAS_LENGTH = int(MAX_POSSIBLE_SATELLITES/100)
    assert(len(sta_bias_valid) == BIAS_LENGTH)
    filename = "%s/%s.%d.%d.XYZ"%(output_directory,station_code,MJD_start,MJD_end)
    if(os.path.isfile(filename)):
        if(overwrite):
            warnings.warn("File '%s' already exists.  Deleting."%filename)
            os.remove(filename)
        else:
            return
    # write out the position
    XYZ_out =  np.memmap(filename,mode="w+", shape=(6,),dtype='float64')
    XYZ_out[:] = sta_XYZ[:]
    del XYZ_out;
    # write out the bias descriptors
    bias_out = np.memmap(filename,mode="r+", offset=6*8,dtype='int32',shape=(BIAS_LENGTH,))
    bias_out[:] = sta_bias_valid[:]
    del bias_out
    return

################################################################################
def _read_Albus_obs_block(MJD_start, MJD_end, station_code,
                        input_directory="."):
    """read in an ALbus RINEX satellite block position datafile

INPUTS:
MJD_start    I  The MJD of the starting time needed
MJD_end      I  The MJD of the ending time needed
station_code I  String of letters indicating the RINEX receiver
input_directory  I  Where to get things

OUTPUTS: sat_block_pos
sat_block_pos  O  An array of continguous block position ranges, as 3 element
                arrays.  So, sat_block_pos[s][i][0] is the start position
                of the block and sat_block_pos[s][i][1] is the end position
                (standard Python 1 past) for block i for satellite s
    """
    sat_block_pos = [None]*MAX_POSSIBLE_SATELLITES
    filename = "%s/%s.%d.%d.blo"%(input_directory,station_code,MJD_start,MJD_end)
    if(not os.path.isfile(filename)):
        return None
    fp = open(filename, "r")
    try:
        for s in range(MAX_POSSIBLE_SATELLITES):
            line = fp.readline()
            assert(line[0] == 'S')
            array = list(map(int,line[1:-1].strip().split(None,2)[0:2]))
            assert(array[0] == s)
            sat_block_pos[s] = [None]*array[1]
            for p in range(array[1]):
                line = fp.readline()
                assert(line[0] == 'T')
                track = list(map(int,line[1:-1].strip().split(None,4)[0:4]))
                assert((track[0] == s)and(track[1] == p))
                sat_block_pos[s][p] = [track[2],track[3]]
    finally:
        fp.close()
    return sat_block_pos

################################################################################
def _write_Albus_obs_block(sat_block_pos, MJD_start, MJD_end,
                           station_code,
                           output_directory=".", overwrite=0):
    """write out an ALbus RINEX satellite block position datafile

INPUTS:
sat_block_pos  I  An array of continguous block position ranges, as 3 element
                arrays.  So, sat_block_pos[s][i][0] is the start position
                of the block and sat_block_pos[s][i][1] is the end position
                (standard Python 1 past) for block i for satellite s
MJD_start    I  The MJD of the starting time needed
MJD_end      I  The MJD of the ending time needed
station_code I  String of letters indicating the RINEX receiver
output_directory  I  Where to put things
overwrite    I  May files be overwritten?  0 No, else yes

OUTPUTS: None
    """
    assert(len(sat_block_pos) == MAX_POSSIBLE_SATELLITES)
    filename = "%s/%s.%d.%d.blo"%(output_directory,station_code,MJD_start,MJD_end)
    if(os.path.isfile(filename)):
        if(overwrite):
            warnings.warn("File '%s' already exists.  Deleting."%filename)
            os.remove(filename)
        else:
            return
    fp = open(filename, "w")
    try:
        for s in range(MAX_POSSIBLE_SATELLITES):
            if(sat_block_pos[s] is None):
                line = "S%4d %5d\n"%(s,0)
                fp.write(line)
            else:
                line = "S%4d %5d\n"%(s,len(sat_block_pos[s]))
                fp.write(line)
                for p in range(len(sat_block_pos[s])):
                    line = "T%4d %5d %8d %8d\n"%(s,p,sat_block_pos[s][p][0],
                                                 sat_block_pos[s][p][1])
                    fp.write(line)
    finally:
        fp.close()
    return













################################################################################
def get_satellite_full_positions(MJD_start, MJD_end, sta_MJD,
                                 output_directory=".", overwrite=0,
                                 use_bin_files=0):
    """interpolate the RINEX satellite positions for all sats for a time array

    
INPUTS:
MJD_start    I  The MJD of the starting time needed
MJD_end      I  The MJD of the ending time needed
sta_MJD      I  The array of Modified Julian Dates for each observation
                time as MJD[num_times], as a numpy array.  This should be from
                a station observation
output_directory  I  Where to put things
overwrite    I  May files be overwritten?  0 No, else yes
use_bin_files I Should the function read from/write to binary files? 0 No,
                else yes

OUTPUTS: sat_XYZ
sat_XYZ      O  numpy array of the satellite positions
                as XYZ[need_times,MAX_POSSIBLE_SATELLITES,6]
                x=0,y=1,z=2.  If x==y==z==0, then no data
                r=3,\phi=4,\theta=5
    """
    if(use_bin_files):
        sat_MJD, sat_XYZ = _read_Albus_sat_XYZ(MJD_start,
                                               MJD_end,
                                               output_directory)
        if((sat_MJD is not None) and (sat_XYZ is not None)):
            diff_val = 2.0
            retval = check_MJD_arrays_differ(sat_MJD, sta_MJD,diff_val)
            if(retval == 0):
                return sat_XYZ
    # Otherwise, calculate from the web data
    sat_MJD, sat_pos = get_satellite_ephemeris_positions(MJD_start,
                                                         MJD_end,
                                                         output_directory,
                                                         overwrite
                                                         )
    sat_XYZ = interpolate_sat_positions2(sat_MJD, sat_pos, sta_MJD)
    if(use_bin_files):
        _write_Albus_sat_XYZ(sta_MJD, sat_XYZ, MJD_start, MJD_end,
                             output_directory, overwrite)
    return sat_XYZ


################################################################################
def get_station_base_observations_with_bin(MJD_start,
                                           MJD_end,
                                           station_code,
                                           output_directory = ".",
                                           overwrite = 0,
                                           use_bin_files=0,
                                           raise_bias_error=0
                                           ):
    """get the RINEX STEC observations for some station for some time range

This function is a wrapper function for get_station_base_observations.
This function adds on the ability to read from and write to binary storage
files to speed up repeated processing using the same RINEX datasets.

For full documantation of the observational files, see
get_station_base_observations


INPUTS:
MJD_start    I  The MJD of the starting time needed
MJD_end      I  The MJD of the ending time needed
station_code I  4 letter code for a station position.
output_directory  I  Where to put things
overwrite    I  May files be overwritten?  0 No, else yes
use_bin_files I Should the function read from/write to binary files? 0 No,
                else yes

OUTPUTS: sta_MJD, Sat_array, obs_data, sta_XYZ, sta_bias_valid, sat_block_pos
sta_MJD      O  numpy array of Modified Julian Dates for each observation time
                as MJD[num_times]
Sat_array    O  numpy array of satellite data positions, as
                Sat_array[num_times,MAX_POSSIBLE_SATELLITES]
                If the value is -1, then no observation of that satellite was
                made
obs_data     O  The output data array, as
                obs_data[num_times, max_sat, _DATA_POS_SIZE]
                Assuming you have found the best index i in MJD for
                a time you desire, and you want data for satellite s, then
                you get data from
                obs_data[i,Sat_array[i,s], x]
                where x is the index of the data kind you want, as specified
                in the _DATA_POS dict
sta_XYZ      O  Cartesian station position in Earth centered coodriantes, in m
                A numpy array array.
sta_bias_valid O  An array of flags to indicate whether station bias
                  information is present (1) or not (0).
                  The array is (MAX_POSSIBLE_SATELLITES/100)
                  in length, for the known receiver systems
sat_block_pos  O  An array of continguous block position ranges, as 3 element
                arrays.  So, sat_block_pos[s][i][0] is the start position
                of the block and sat_block_pos[s][i][1] is the end position
                (standard Python 1 past) for block i for satellite s
"""
    if(use_bin_files):
        sta_MJD = _read_Albus_MJD(MJD_start, MJD_end, station_code,
                                  output_directory)
        if(sta_MJD is not None):
            Sat_array = _read_Albus_sat_array(MJD_start, MJD_end, station_code,
                                              output_directory)
            obs_data = _read_Albus_obs_data(MJD_start, MJD_end, station_code,
                                            output_directory)
            sta_XYZ, sta_bias_valid = _read_Albus_obs_XYZ(MJD_start, MJD_end,
                                                          station_code,
                                                          output_directory)
            sat_block_pos = _read_Albus_obs_block(MJD_start, MJD_end,
                                                  station_code, output_directory)
            if((Sat_array is None) or (obs_data is None) or
               (sta_XYZ is None) or (sta_bias_valid is None) or
               (sat_block_pos is None)):
                pass
            else:
                return sta_MJD, Sat_array, obs_data, sta_XYZ, sta_bias_valid, sat_block_pos
    # Need to get the data from the web files
    sta_MJD, Sat_array, obs_data, sta_XYZ, sta_bias_valid, sat_block_pos = \
             get_station_base_observations(MJD_start,
                                           MJD_end,
                                           station_code,
                                           output_directory,
                                           overwrite,
                                           raise_bias_error
                                           )
    if(use_bin_files):
        _write_Albus_MJD(sta_MJD, MJD_start, MJD_end, station_code,
                         output_directory)
        _write_Albus_sat_array(Sat_array, MJD_start, MJD_end, station_code,
                               output_directory)
        _write_Albus_obs_data(obs_data, MJD_start, MJD_end, station_code,
                              output_directory)
        _write_Albus_obs_XYZ(sta_XYZ, sta_bias_valid, MJD_start, MJD_end,
                             station_code, output_directory)
        _write_Albus_obs_block(sat_block_pos, MJD_start, MJD_end, station_code,
                              output_directory)
    return sta_MJD, Sat_array, obs_data, sta_XYZ, sta_bias_valid, sat_block_pos

























################################################################################
def get_single_station_base_observations(MJD_start,
                                         MJD_end,
                                         station_code,
                                         output_directory = ".",
                                         overwrite = 0,
                                         use_bin_files = 0,
                                         compute_AzEl = 0,
                                         raise_bias_error = 0
                                         ):
    """For a single station, get RINEX STEC observations for some time range

For ONE station, get the standard, bias-corrected slant TEC values from
RINEX observations, in addition to the standard satellite position information.

This function is designed for single station calls.  If you will have many
different stations investigated for this same time range, you should NOT
use this function.  Use get_multiple_station_base_observations instead.
This function will call several things which require lots of Python CPU time.
If you have multiple stations, this CPU overhead can be reduced, since the
satellites are in the same places.


INPUTS:
MJD_start    I  The MJD of the starting time needed
MJD_end      I  The MJD of the ending time needed
station_code I  4 letter code for a station position.
output_directory  I  Where to put things
overwrite    I  May files be overwritten?  0 No, else yes
use_bin_files I Should the function read from/write to binary files? 0 No,
                else yes
compute_AzEl I  Should the Satellite Azimuths and Elevation angles be computed?
                0 No, else yes.

OUTPUTS: sta_MJD, Sat_array, obs_data, sta_XYZ, sat_XYZ, sta_bias_valid, sat_block_pos
sta_MJD      O  numpy array of Modified Julian Dates for each observation time
                as MJD[num_times]
Sat_array    O  numpy array of satellite data positions, as
                Sat_array[num_times,MAX_POSSIBLE_SATELLITES]
                If the value is -1, then no observation of that satellite was
                made
obs_data     O  The output data array, as
                obs_data[num_times, max_sat, _DATA_POS_SIZE]
                Assuming you have found the best index i in MJD for
                a time you desire, and you want data for satellite s, then
                you get data from
                obs_data[i,Sat_array[i,s], x]
                where x is the index of the data kind you want, as specified
                in the _DATA_POS dict
sta_XYZ      O  Cartesian station position in Earth centered coodriantes, in m
                A numpy array array.
sat_XYZ      O  numpy array of the satellite positions
                as XYZ[need_times,MAX_POSSIBLE_SATELLITES,6]
                x=0,y=1,z=2.  If x==y==z==0, then no data
                r=3,\phi=4,\theta=5
sta_bias_valid O  An array of flags to indicate whether station bias
                  information is present (1) or not (0).
                  The array is (MAX_POSSIBLE_SATELLITES/100)
                  in length, for the known receiver systems
sat_block_pos  O  An array of continguous block position ranges, as 3 element
                arrays.  So, sat_block_pos[s][i][0] is the start position
                of the block and sat_block_pos[s][i][1] is the end position
                (standard Python 1 past) for block i for satellite s
"""
    assert(MJD_start <= MJD_end)
    sta_MJD, Sat_array, obs_data, sta_XYZ, sta_bias_valid, sat_block_pos = \
             get_station_base_observations_with_bin(MJD_start,
                                                    MJD_end,
                                                    station_code,
                                                    output_directory,
                                                    overwrite,
                                                    use_bin_files,
                                                    raise_bias_error
                                                    )
    sat_XYZ = get_satellite_full_positions(MJD_start, MJD_end, sta_MJD,
                                           output_directory, overwrite,
                                           use_bin_files)
    if(compute_AzEl):
        fill_in_obs_AzEl_values(sta_MJD, Sat_array, obs_data, sta_XYZ, sat_XYZ)
    return sta_MJD, Sat_array, obs_data, sta_XYZ, sat_XYZ, sta_bias_valid, sat_block_pos

















################################################################################
def get_multiple_station_base_observations(MJD_start,
                                           MJD_end,
                                           station_code,
                                           sta_MJD_in,
                                           sat_XYZ_in,
                                           output_directory = ".",
                                           overwrite = 0,
                                           use_bin_files = 0,
                                           compute_AzEl = 0,
                                           raise_bias_error=0
                                           ):
    """For multiple stations, get RINEX STEC observations for some time range

This function is designed to get the standard, bias corrected slat TEC values
from multiple GPS stations, one station at a time per call.  It is intended
that the user call this function repeatedly to get each different station
dataset.

This function will look at the input time array and satellite position arrays
to make sure that the new datasets match previous calls to this function.  If
the user gives a valid time array which does not match the new time array,
an exception will be thrown.  If the user does not give a valid satellite
position array, a new one will be computed.  And if the user does give a
valid satellite position array, this will not be recomputed, as the satellite
position array is very expensive to compute.

This function is designed for multiple station calls.  If you only want one
station investigated for this time range, you should NOT
use this function.  Use get_single_station_base_observations instead.


INPUTS:
MJD_start    I  The MJD of the starting time needed
MJD_end      I  The MJD of the ending time needed
station_code I  4 letter code for a station position.
sta_MJD_in   I  The previous array of Modified Julian Dates for each observation
                time as MJD[num_times], as a numpy array.  May also be None
sat_XYZ_in   I  The previous numpy array of the satellite positions
                as XYZ[need_times,MAX_POSSIBLE_SATELLITES,6]
                x=0,y=1,z=2.  If x==y==z==0, then no data
                r=3,\phi=4,\theta=5
                If None, then the sat_XYZ positions will be calculated anew.
output_directory  I  Where to put things
overwrite    I  May files be overwritten?  0 No, else yes
use_bin_files I Should the function read from/write to binary files? 0 No,
                else yes
compute_AzEl I  Should the Satellite Azimuths and Elevation angles be computed?
                0 No, else yes.

OUTPUTS: sta_MJD, Sat_array, obs_data, sta_XYZ, sat_XYZ,sta_bias_valid, sat_block_pos
sta_MJD      O  numpy array of Modified Julian Dates for each observation time
                as MJD[num_times]
Sat_array    O  numpy array of satellite data positions, as
                Sat_array[num_times,MAX_POSSIBLE_SATELLITES]
                If the value is -1, then no observation of that satellite was
                made
obs_data     O  The output data array, as
                obs_data[num_times, max_sat, _DATA_POS_SIZE]
                Assuming you have found the best index i in MJD for
                a time you desire, and you want data for satellite s, then
                you get data from
                obs_data[i,Sat_array[i,s], x]
                where x is the index of the data kind you want, as specified
                in the _DATA_POS dict
sta_XYZ      O  Cartesian station position in Earth centered coodriantes, in m
                A numpy array array.
sat_XYZ      O  numpy array of the satellite positions
                as XYZ[need_times,MAX_POSSIBLE_SATELLITES,6]
                x=0,y=1,z=2.  If x==y==z==0, then no data
                r=3,\phi=4,\theta=5
sta_bias_valid O  An array of flags to indicate whether station bias
                  information is present (1) or not (0).
                  The array is (MAX_POSSIBLE_SATELLITES/100)
                  in length, for the known receiver systems
sat_block_pos  O  An array of continguous block position ranges, as 3 element
                arrays.  So, sat_block_pos[s][i][0] is the start position
                of the block and sat_block_pos[s][i][1] is the end position
                (standard Python 1 past) for block i for satellite s
"""
    assert(MJD_start <= MJD_end)
    sat_XYZ_set = True
    if DEBUG_SET:
      print ( "getting in RINEX2",MJD_start,get_multiple_station_base_observations.MJD_start_last )  
      print ( "AND:",MJD_end,get_multiple_station_base_observations.MJD_end_last)
    if((MJD_start != get_multiple_station_base_observations.MJD_start_last)
       or (MJD_end != get_multiple_station_base_observations.MJD_end_last)):
        # force a brand new calculation
        sta_MJD_in = None
        sat_XYZ_in = None
        sat_XYZ_set = False
        get_multiple_station_base_observations.MJD_start_last = MJD_start
        get_multiple_station_base_observations.MJD_end_last   = MJD_end
    sta_MJD, Sat_array, obs_data, sta_XYZ, sta_bias_valid, sat_block_pos = \
             get_station_base_observations_with_bin(MJD_start,
                                                    MJD_end,
                                                    station_code,
                                                    output_directory,
                                                    overwrite,
                                                    use_bin_files,
                                                    raise_bias_error
                                                    )
    # compare the MJD arrays
    if(sta_MJD_in is not None):
        diff_val = 2.
        if DEBUG_SET:
          print ( "SAME? sta_MJD_in vs sta_MJD ",sta_MJD_in, "\n", sta_MJD)
        retval = check_MJD_arrays_differ(sta_MJD, sta_MJD_in, diff_val)
        if(retval):
            print ( 'difference in MJD sec > ', diff_val, ' seconds at ', retval-1, sta_MJD[retval-1],sta_MJD_in[retval-1])
            raise Albus_RINEX.RINEX_Data_Barf("Error: MJD dates differ (%d)"%(retval))
    else:
        sat_XYZ_in = None
        sat_XYZ_set = False
    if(sat_XYZ_set == False):    
        sat_XYZ = get_satellite_full_positions(MJD_start, MJD_end, sta_MJD,
                                               output_directory, overwrite,
                                               use_bin_files)
        sat_XYZ_set = True
    else:
        sat_XYZ = sat_XYZ_in
    if(compute_AzEl):
        fill_in_obs_AzEl_values(sta_MJD, Sat_array, obs_data, sta_XYZ, sat_XYZ)
    return sta_MJD, Sat_array, obs_data, sta_XYZ, sat_XYZ, sta_bias_valid, sat_block_pos
get_multiple_station_base_observations.MJD_start_last  = 0.0
get_multiple_station_base_observations.MJD_end_last    = 0.0



###########################
###########################
### MeqTree Area ##########

MAX_POSSIBLE_MEQTREE_SATELLITES = MAX_POSSIBLE_SATELLITES / 2



################################################################################
def MeqTree_Sat_Number(sat_number):
    """convert a RINEX satellite number to a MeqTree satellite number

This converts a RINEX satellite number to a MeqTree satellite number, which, for
the time being, is just cutting off the 50--99 RINEX numbers.

INPUTS:
sat_number  I  the RINEX satellite number

OUTPUTS: M_s
M_s         O  The MeqTree satellite number.  Numbers >= 0 are valid.
               a value of -1 indicates that there is no valid MeqTree number
               corresponding to the RINEX number given.
    """
    for mytype in range(MAX_POSSIBLE_SATELLITES/100):
        offset = sat_number - mytype*100
        if((offset >= 0) and (offset < 50)):
            M_s = mytype*50 + offset
            return M_s
    return -1

################################################################################
def RINEX_Sat_Number_from_MeqTree(sat_number):
    """convert a MeqTree satellite number to a RINEX satellite number

INPUTS:
sat_number  I  the MeqTree satellite number

OUTPUTS: R_s
R_s         O  The RINEX satellite number.  Numbers >= 0 are valid.
               a value of -1 indicates that there is no valid RINEX number
               corresponding to the MeqTree number given.
    """
    if(sat_number < 0):
        return -1
    mytype = int(sat_number/50)
    offset = sat_number - mytype*50
    R_s = offset + mytype*100
    if(R_s >= MAX_POSSIBLE_SATELLITES):
        return -1
    return R_s
    





################################################################################
def write_MeqTree_Sat_File(sat_XYZ,
                           MJD_start,
                           MJD_end,
                           output_directory = ".",
                           overwrite = 0
                           ):
    """ write out the sat XYZ positions in a MeqTree format

See get_multiple_station_MeqTree_observations for details

INPUTS:
sat_XYZ      I  numpy array of the satellite positions
                as XYZ[need_times,MAX_POSSIBLE_SATELLITES,6]
                x=0,y=1,z=2.  If x==y==z==0, then no data
                r=3,\phi=4,\theta=5
MJD_start    I  The MJD of the start day (an integer)
MJD_end      I  The MJD of the end day (an integer)
output_directory  I  Where to put things
overwrite    I  May files be overwritten?  0 No, else yes

OUTPUTS: None

    """
    NUM_TIMES = int(SECONDS_PER_DAY/30.0) * (MJD_end - MJD_start +1)
    sh = sat_XYZ.shape
    assert(sh[0] == NUM_TIMES)
    assert(sh[1] == MAX_POSSIBLE_SATELLITES)
    assert(sh[2] >= 3)
    SATELLITE_SIZE = NUM_TIMES * MAX_POSSIBLE_MEQTREE_SATELLITES * 3 * 8
    filename = "%s/sat.%d.%d.dat"%(output_directory,MJD_start,MJD_end)
    if(os.path.isfile(filename)):
        if(overwrite):
            warnings.warn("File '%s' already exists.  Deleting."%filename)
            os.remove(filename)
        else:
            return
    sat_out = np.memmap(filename,mode="w+", shape=(NUM_TIMES,
                                                    MAX_POSSIBLE_MEQTREE_SATELLITES,
                                                    3),
                             dtype='float64')
    sat_out[:,:,:] = 0.0  # place blank satellites at the center of the Earth
    for s in range(MAX_POSSIBLE_MEQTREE_SATELLITES):
        s_r = RINEX_Sat_Number_from_MeqTree(s)
        sat_out[:,s,:] = sat_XYZ[:,s_r,0:3]
        
    del sat_out
    return


################################################################################
def write_MeqTree_STEC_File(station_name,
                            STEC,
                            sigma,
                            Sat_array,
                            sta_XYZ,
                            MJD_start,
                            MJD_end,
                            output_directory = ".",
                            overwrite = 0
                            ):
    """ write out the sat XYZ positions in a MeqTree format

See get_multiple_station_MeqTree_observations for details

INPUTS:
station_name I  Four-letter RINEX station code
STEC         I  numpy array of STEC values in m^{-2}, of the form
                STEC[num_times, max_sat]
sigma        I  numpy array of STEC uncertainty values in m^{-2}, of the form
                SIGMA[num_times, max_sat]
Sat_array    I  numpy array of satellite data positions, as
                Sat_array[num_times,MAX_POSSIBLE_SATELLITES]
                If the value is -1, then no observation of that satellite was
                made
sta_XYZ      I  Cartesian station position in Earth centered coodriantes, in m
                A numpy array array.
MJD_start    I  The MJD of the start day (an integer)
MJD_end      I  The MJD of the end day (an integer)
output_directory  I  Where to put things
overwrite    I  May files be overwritten?  0 No, else yes

OUTPUTS: None

    """
    NUM_TIMES = int(SECONDS_PER_DAY/30.0) * (MJD_end - MJD_start +1)
    sh = Sat_array.shape
    assert(sh[0] == NUM_TIMES)
    assert(sh[1] == MAX_POSSIBLE_SATELLITES)
    STATION_STEC_SIZE = NUM_TIMES * MAX_POSSIBLE_MEQTREE_SATELLITES * 2 * 4
    STATION_TOTAL_SIZE = STATION_STEC_SIZE + 3 * 8
    filename = "%s/%4.4s.%d.%d.dat"%(output_directory,station_name,
                                     MJD_start,MJD_end)
    if(os.path.isfile(filename)):
        if(overwrite):
            warnings.warn("File '%s' already exists.  Deleting."%filename)
            os.remove(filename)
        else:
            return
    sta_out = np.memmap(filename,mode="w+", shape=(NUM_TIMES,
                                               MAX_POSSIBLE_MEQTREE_SATELLITES,
                                               2))
    sta_out[:,:,:] = BAD_DATA_CODE  # Mark data as bad at first
    for s in range(MAX_POSSIBLE_MEQTREE_SATELLITES):
        s_r = RINEX_Sat_Number_from_MeqTree(s)
        for t in range(NUM_TIMES):
            sat_index = Sat_array[t,s_r]
            if(sat_index >= 0):
                sta_out[t,s,0] = STEC[t,sat_index]
                sta_out[t,s,1] = sigma[t,sat_index]
    del sta_out
    # now put on the position data
    sta_pos = np.memmap(filename,mode="r+", shape=(3,),dtype='float64',offset=STATION_STEC_SIZE)
    sta_pos[:] = sta_XYZ[0:3]
    del sta_pos;
    return

################################################################################
def write_MeqTree_obs_block(station_name, sat_block_pos, MJD_start, MJD_end,
                           output_directory=".", overwrite=0):
    """write out an ALbus RINEX satellite block position datafile

INPUTS:
station_name I  Four-letter RINEX station code
sat_block_pos  I  An array of continguous block position ranges, as 3 element
                arrays.  So, sat_block_pos[s][i][0] is the start position
                of the block and sat_block_pos[s][i][1] is the end position
                (standard Python 1 past) for block i for satellite s
MJD_start    I  The MJD of the start day (an integer)
MJD_end      I  The MJD of the end day (an integer)
station_code I  String of letters indicating the RINEX receiver
output_directory  I  Where to put things
overwrite    I  May files be overwritten?  0 No, else yes

OUTPUTS: None
    """
    assert(len(sat_block_pos) == MAX_POSSIBLE_SATELLITES)
    filename = "%s/%4.4s.%d.%d.blo"%(output_directory,station_name,
                                     MJD_start,MJD_end)
    if(os.path.isfile(filename)):
        if(overwrite):
            warnings.warn("File '%s' already exists.  Deleting."%filename)
            os.remove(filename)
        else:
            return
    fp = open(filename, "w")
    try:
        for ms in range(MAX_POSSIBLE_MEQTREE_SATELLITES):
            s = RINEX_Sat_Number_from_MeqTree(ms)
            if(s < 0):
                continue
            if(sat_block_pos[s] is None):
                line = "S%4d %5d\n"%(ms,0)
                fp.write(line)
            else:
                line = "S%4d %5d\n"%(ms,len(sat_block_pos[s]))
                fp.write(line)
                for p in range(len(sat_block_pos[s])):
                    line = "T%4d %5d %8d %8d\n"%(ms,p,sat_block_pos[s][p][0],
                                                 sat_block_pos[s][p][1])
                    fp.write(line)
    finally:
        fp.close()
    return





################################################################################
def get_multiple_station_MeqTree_observations(MJD_start,
                                              MJD_end,
                                              station_code_list,
                                              output_directory = ".",
                                              overwrite = 0,
                                              raise_bias_error=1
                                              ):
    """For multiple stations, make MeqTree GPS datafiles

This function is an interface to MeqTree software to help read GPS datafiles.
It reads in a series of GPS datasets using get_multiple_station_base_observations
and dumps data out into binary files.

Two types of datafiles are written out.  The first is the station STEC
measurement file.  This has the form of an array STEC[T,S,2] of Real32s followed
by an array XYZ[3] of Real64s which is the station position in Earth-fixed
coordinates.  Units of the STEC array are electrons per square meter.  Units of
the XYZ array are meters.  The STEC array has two entries for each time and
satellite combination.  Element 0 is the STEC value, and element 1 is the
uncertainty in that measurement.  Each data file is written out as
a series of times, at 30 second intervals.  Therefore, data at time 1 hour
0 minutes and 0 seconds into the day is written out starting at bye offset
3600/30                  * MAX_POSSIBLE_MEQTREE_SATELLITES * 2        * 4
Time                       Number satellites                 Num_data   Num_bytes
or byte number 144000 for MAX_POSSIBLE_MEQTREE_SATELLITES==150.

Each station STEC file will occupy about 3.5 MB of disk space.

There should be 2880 time entries per day, for 30 second intervals through one
day.  

The satellite position file is arranged as an array POS[T,S,3], where the 3
indicates x==0, y==1, z==2.  The data are Real64 values.

Here, T is the time, which should have 2880 entries per day.

S is the MeqTree satellite number, which should have
MAX_POSSIBLE_MEQTREE_SATELLITES entries, 150 at this time


The station datafile is named by the 4 character station code, followed by a
., and then the MJD of the start day, then a . and the MJD of the end day
(inclusive), and then the extension .dat  This gives
rrrr.MMMMM.MMMMM.dat

The satellite datafile is named by sat.MMMMM.MMMMM.dat  where the sat is literal.

INPUTS:
MJD_start    I  The MJD of the starting time needed
MJD_end      I  The MJD of the ending time needed
station_code_list I  an array of station arrays, with
                4 letter codes for station positions.  Should be as from
                GPS_stations.get_stations_within_distance_2
output_directory  I  Where to put things
overwrite    I  May files be overwritten?  0 No, else yes

OUTPUTS: None

    """
    global _DATA_POS
    SIG_pos = _DATA_POS['SIGMA']
    PL_pos = _DATA_POS['STECPL']
    MJD_start_i = int(MJD_start)
    MJD_end_i = int(MJD_end)
    # 
    MJD_array = None
    sat_XYZ   = None
    for i in range(len(station_code_list)):
        GPS = station_code_list[i][0]
        print ( "Getting data from receiver %d/%d   '%s'"%(i,len(station_code_list),GPS))
        filename = "%s/%4.4s.%d.%d.dat"%(output_directory,GPS,
                                         MJD_start_i,MJD_end_i)
        if(os.path.isfile(filename)):
            if(not overwrite):
                continue
        try:
            MJD_array, Sat_array, obs_data, sta_XYZ, sat_XYZ, sta_bias_valid, sat_block_pos = \
                       get_multiple_station_base_observations(MJD_start_i,
                                                              MJD_end_i,
                                                              GPS,
                                                              MJD_array,
                                                              sat_XYZ,
                                                              overwrite,
                                                              raise_bias_error)
            if(i==0):
                write_MeqTree_Sat_File(sat_XYZ,MJD_start_i, MJD_end_i,
                                       output_directory,
                                       overwrite)
            sh = obs_data.shape
            # convert from TECU to m^{-2}
            STEC = obs_data[:,:,PL_pos]
            sigma = obs_data[:,:,SIG_pos]
            for t in range(sh[0]):
                for s in range(sh[1]):
                    if((STEC[t,s] != BAD_DATA_CODE)
                        and(sigma[t,s] != BAD_DATA_CODE)):
                        STEC[t,s] *= TECU_TO_M_N2
                        sigma[t,s] *= TECU_TO_M_N2
                    else:
                        STEC[t,s] = BAD_DATA_CODE
                        sigma[t,s] = BAD_DATA_CODE
            write_MeqTree_STEC_File(GPS,
                                    STEC,
                                    sigma,
                                    Sat_array,
                                    sta_XYZ,
                                    MJD_start_i,
                                    MJD_end_i,
                                    output_directory,
                                    overwrite
                                    )
            write_MeqTree_obs_block(GPS, sat_block_pos, MJD_start_i, MJD_end_i,
                                    output_directory,overwrite)
        except Albus_RINEX.No_RINEX_File_Error as error:
            # try to ignore this error for now
            print ((str(error)))
            print ( "Failed to get data for GPS receiver '%s', skipping"%(GPS))
        except Albus_RINEX.RINEX_Data_Barf as error:
            # ignore the stupid header problems for now
            print ((str(error)))
            print ( "Failed to get data for GPS receiver '%s' because of header/data problems, skipping"%(GPS))
    return
