# Albus_Iono.py
# Python stuff for dealing with Ionosphere information in AIPS AI tables
# 2006 Jan 05  James M Anderson  --JIVE  start
# 2006 Jun 22  JMA  --add some stuff for Emil Lenc for wide-field imaging
# 2006 Jul 04  JMA  --add some more stuff for wide-file imaging.



################################################################################
# some import commands  The User should not need to change this.
################################################################################

################################################################################
# Mark's ParselTongue stuff
from AIPS import AIPS, AIPSDisk
from AIPSTask import AIPSTask, AIPSList
from AIPSData import AIPSUVData, AIPSImage
from AIPSTV import AIPSTV
import Wizardry.AIPSData

################################################################################
# miscellaneous stuff
import copy, optparse, os, sys
import re, string
import numarray
import numarray.ma
import numarray.ieeespecial
import numarray.nd_image
import inspect
import warnings
import math
import time as systime

################################################################################
# JMA's ionosphere stuff
import AlbusIonosphere
import Albus_Coordinates
import jma_aips
import Albus_RINEX
import Albus_RINEX_2
import GPS_stations


#import AIPSUVData, AIPSTableRow
if(jma_aips.get_ParselTonuge_version_tuple().__lt__((1,0,6))):
    raise RuntimeError("Unsupported version of ParselTongue '%s', please upgrade to at least 1.0.6"%(get_ParselTonuge_version_tuple()))










################################################################################
# Global variables
SECONDS_PER_DAY = 24.0*60.0*60.0
DAYS_PER_SECOND = 1.0/SECONDS_PER_DAY
M_DEG2RAD = math.pi/180.0
M_RAD2DEG = 180.0/math.pi


# See JMA Notes from 2006 Jan 10 and some of Bob Campbell's notes
SPEED_OF_LIGHT = 299792458.0    # m s^{-1}
IONOSPHERE_Kp_2 = 80.6163848    # m^3 s^{-2}
IONOSPHERE_KB = 2.79924925E10   # s^{-1} T^{-1}
# AIPS defines things at \lambda = 1.0 m
# TEC in units of electrons m^{-2}
# RM in units of electrons m^{-2} T
AIPS_TEC_TO_DISP = IONOSPHERE_Kp_2 / (2.0 * SPEED_OF_LIGHT**3)
AIPS_RM_TO_RM = math.pi * IONOSPHERE_Kp_2 * IONOSPHERE_KB / (SPEED_OF_LIGHT**3)







################################################################################
# processing_option
# the function argument processing_option is used to determine what sort
# of ionospheric data/model to use.  It is a 6 character string, made up of
# a 2 character data/model indicator, followed by _, and then a three character
# specifier.  At this time, the recognized data/model indicators are
PROCESSING_OPTION_MODEL_INDICATORS = ["IO", # IONEX data
                                      "MO", # Model
                                      "RI"  # RINEX data
                                      ]
# The valid IONEX groups are
PROCESSING_OPTION_IONEX_GROUPS = ["IO_COD",  # CODE
                                  "IO_ESA",  # ESA
                                  "IO_IGS",  # the IGS service
                                  "IO_JPL",  # JPL
                                  "IO_UPC"   # UPC ???
                                  ]
# Note that the three letter group codes reflect the standard IONEX
# file naming conventions for the final datasets.  Replacing the last letter
# in the group name with 'r' will give the rapid files.
# The valid model groups are
PROCESSING_OPTION_MODEL_GROUPS = ["MO_IRI",  # IRI
                                  "MO_PIM"   # PIM
                                  ]
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


# And a full list is
PROCESSING_OPTION_FULL_LIST = []
PROCESSING_OPTION_FULL_LIST += PROCESSING_OPTION_IONEX_GROUPS
PROCESSING_OPTION_FULL_LIST += PROCESSING_OPTION_MODEL_GROUPS
PROCESSING_OPTION_FULL_LIST += PROCESSING_OPTION_RINEX_GROUPS













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
def get_observation_year_month_day(aips_data):
    date_string = aips_data.header.date_obs
    date_list = date_string.split('-')
    year = int(date_list[0])
    month = int(date_list[1])
    day = int(date_list[2])
    return (year, month, day)

################################################################################
# Get the day of year from the Year, month, day for the start of observations
def get_observation_day_of_year(aips_data):
    (year, month, day) = get_observation_year_month_day(aips_data)
    return get_day_of_year(year, month, day)


################################################################################
# Get the number of days of observations
def get_number_days_observations(aips_data):
    """Find out how long the observations are.  Assumes the data has been sorted

    """
    nx_table = aips_data.table('AIPS NX', 0)
    num_rows = len(nx_table)
    # The NX table is FORTRANish
    # Check the first observation
    first_row = nx_table[0]
    start_time = first_row['TIME'][0] - 0.5 * first_row['TIME INTERVAL'][0]
    if(start_time < 0.0):
        raise RuntimeError, "Starting observation before starting date: Run FXTIM!"
    # Now check the last scan
    last_row = nx_table[num_rows-1]
    end_time = last_row['TIME'][0] + 0.5 * last_row['TIME INTERVAL'][0]
    return int(math.ceil(end_time))

################################################################################
def get_observation_start_end(aips_data):
    """Find out what the start, stop times are, for whole dataset.

INPUTS:
aips_data   I  dataset to check on

OUTPUTS: start_time, end_time
start_time  O  beginning of initial scan, in fractions of a day from the
               dataset date
end_time    O  end of final scan, in fractions of a day from dataset date.

    """
    nx_table = aips_data.table('AIPS NX', 0)
    num_rows = len(nx_table)
    # Check the first observation
    first_row = nx_table[0]
    start_time = first_row['TIME'][0] - 0.5 * first_row['TIME INTERVAL'][0]
    # Now check the last scan
    last_row = nx_table[num_rows-1]
    end_time = last_row['TIME'][0] + 0.5 * last_row['TIME INTERVAL'][0]
    return start_time, end_time






################################################################################
def get_MJD_from_GPS_week(week, seconds_of_week = 0):
    """returns the MJD for the GPS week and seconds_of_week

Note that the GPS week system begins with week 0.0000 at
1980 Jan 06 00h00m00s UT.

This is the full GPS week, not the version modulo 1024.
"""
    MJD = week * 7.0 + 44244.0
    MJD += seconds_of_week * DAYS_PER_SECOND
    return MJD

################################################################################
def get_GPS_from_MJD(MJD):
    """Get GPS week, day_of_week, and seconds_of_week from MJD.

Note that the GPS week system begins with week 0.0000 at
1980 Jan 06 00h00m00s UT.

This is the full GPS week, not the version modulo 1024.
"""
    days = MJD - 44244.0
    w = days/7.0
    weeks = int(w+1E-9)
    d = (w-weeks) * 7.0
    days = int(d+1E-9)
    seconds = d * SECONDS_PER_DAY
    
    return weeks, days, seconds















################################################################################
def check_table_version_exists(aips_data, table, version):
    """Check whether a table version exists, as 'AIPS CL', 'AIPS BP', etc.

    if version == 0, then check for ANY of that table, 
    """
    # If version is zero, then accept any of that table
    if(version == 0):
        for t in aips_data.tables:
            if(t[1] == table): break
        else: return 0
        return 1
    # Otherwise, check for the version number too
    for t in aips_data.tables:
        if((t[0] == version) and (t[1] == table)): break
    else: return 0
    return 1


################################################################################
def verify_table_version_free(aips_data, table, version, overwrite):
    """check that a table version is vacant, and possibly delete if ok"""
    # check for an existing table version
    if(check_table_version_exists(aips_data, table, version)):
        # Are we allowed to delete it?
        if(overwrite):
            aips_data.zap_table(table, version)
        else:
            raise RuntimeError, "Table %s %d exists in dataset '%s', but cannot delete"%(table, version, aips_data.__str__())

################################################################################
def clear_table_version_maybe(aips_data, table, version, overwrite):
    """if we are overwriting and the table exists, zap it"""
    if(overwrite):
        # check for an existing table version
        if(check_table_version_exists(aips_data, table, version)):
            aips_data.zap_table(table, version)




################################################################################
# Get the Year, month, day for the start of observations
def get_observation_year_month_day(aips_data):
    date_string = aips_data.header.date_obs
    date_list = date_string.split('-')
    year = int(date_list[0])
    month = int(date_list[1])
    day = int(date_list[2])
    return (year, month, day)

################################################################################
# Get the reference day from the AN table
def get_AN_reference_year_month_day(an_table):
    date_string = an_table.keywords['RDATE']
    if(len(date_string) == 1):
        raise RuntimeError, "Error: The Obit string bug has finally been fixed"
    year = int(date_string[0][0:4])
    month = int(date_string[0][4:6])
    day = int(date_string[0][6:8])
    return (year,month,day)








################################################################################
# Find the highest numbered antenna
def find_antenna_highnumber(aips_data):
    """Find the highest numbered antenna in the dataset
    """
    highnum = 0
    for an in xrange(1,aips_data.table_highver('AIPS AN')+1):
        # check that this exists
        if not(check_table_version_exists(aips_data, 'AIPS AN', an)): continue
        # Ok, grab the AN table
        an_table = aips_data.table('AIPS AN', an)
        for row in an_table:
            if(highnum < row.nosta): highnum = row.nosta
    return highnum

################################################################################
# Find the highest numbered source
def find_source_highnumber(aips_data):
    """Find the highest numbered source in the dataset
    """
    highnum = 0
    SU_table = aips_data.table('AIPS SU', 1)
    for row in SU_table:
        if(highnum < row.id__no): highnum = row.id__no
    return highnum










################################################################################
# Count visibilities
def count_visibilities(aips_data_orig):
    """Count the number of visibilities per antenna in a dataset

    aips_data_orig  I  a non-Wizardry object to use
    """
    aips_data = Wizardry.AIPSData.AIPSUVData(aips_data_orig.name,
                                             aips_data_orig.klass, 
                                             aips_data_orig.disk,
                                             aips_data_orig.seq)
    # What is the highest valued antenna number?
    high_ant = find_antenna_highnumber(aips_data_orig)
    antennas = numarray.zeros((high_ant+1))
    for row in aips_data:
        baseline = row.baseline
        a1 = baseline[0]
        a2 = baseline[1]
        if(a1 != a2):
            antennas[a1] = antennas[a1] +1
            antennas[a2] = antennas[a2] +1
    return antennas










################################################################################
# Get sources and positions
def get_source_positions(aips_data, SU_in = 1):
    """Reads in source numbers, names, positions from SU table"""
    SU_table = aips_data.table('AIPS SU', SU_in)
    highnum = 0
    for row in SU_table:
        if(highnum < row.id__no): highnum = row.id__no
    sources = (highnum+1)*[None]
    for row in SU_table:
        # verify that EPOCH is 2000.0
        EPOCH = row.epoch
        if(EPOCH != 2000.0):
            raise RuntimeError, "Error: The AlbusIonosphere can only deal with J2000 coordinates right now."
        RA = row.raepo * M_DEG2RAD
        Dec = row.decepo * M_DEG2RAD
        name = row.source
        id = row.id__no
        sources[id] = [id,name,RA,Dec,EPOCH]
    # now return the table
    return sources





################################################################################
def set_source_positions(aips_data_in, new_source_info, SU_in = 1, SU_out = 2,
                         aips_data_out = None):
    """Sets source names, positions in a new SU table

aips_data_in     I  The UV dataset to get the info from
new_source_info  I  A list of source info, as would be returned from
                    get_source_positions (probably updated for position or name)
SU_in            I  Integer of the original SU table to work on
SU_out           I  Integer of the new SU table to create
aips_data_out    I  The aips dataset to put the new SU table to.  If None,
                    will use aips_data_in
    """

    if(aips_data_out == None): aips_data_out = aips_data_in
    assert(jma_aips.check_table_version_valid(aips_data_in, 'AIPS SU', SU_in))
    verify_table_version_free(aips_data_out, 'AIPS SU', SU_out,0)

    
    # Get a Wizardy object
    W_dataset_in = Wizardry.AIPSData.AIPSUVData(aips_data_in.name,
                                                aips_data_in.klass, 
                                                aips_data_in.disk,
                                                aips_data_in.seq)
    W_dataset_out = Wizardry.AIPSData.AIPSUVData(aips_data_out.name,
                                                 aips_data_out.klass, 
                                                 aips_data_out.disk,
                                                 aips_data_out.seq)
    # Get the old table and the new one
    SU_table_old = W_dataset_in.table('AIPS SU', SU_in)
    SU_table_new = W_dataset_out.attach_table('SU', SU_out)
    SU_table_new.keywords['NO_IF'] = SU_table_old.keywords['NO_IF']
    SU_table_new.keywords['VELTYP'] = SU_table_old.keywords['VELTYP']
    SU_table_new.keywords['VELDEF'] = SU_table_old.keywords['VELDEF']
    warnings.warn("""Caution: this subroutine does not properly calculate the
apparent Right Ascension and Declination for the AIPS reference day.  Use
with caution and do not use for precise imaging.""")
    for row in SU_table_old:
        # verify that EPOCH is 2000.0
        EPOCH = row.epoch
        if(EPOCH != 2000.0):
            raise RuntimeError, "Error: The AlbusIonosphere can only deal with J2000 coordinates right now."
        id = row.id__no
        if(new_source_info[id] == None):
            raise RuntimeError, "Error: source "+id.__str__()+" not found in new source info"
        # Need to fudge the apparent RA and Dec
        RA_orig = row.raepo
        Dec_orig = row.decepo
        row.raepo = new_source_info[id][2] * M_RAD2DEG
        row.decepo = new_source_info[id][3] * M_RAD2DEG
        RA_app = row.raapp + (row.raepo-RA_orig)
        Dec_app = row.decapp + (row.decepo-Dec_orig)
        if(Dec_app > 90.0):
            Dec_app = 180.0 - Dec_app
            RA_app = RA_app + 180.0
        elif(Dec_app < -90.0):
            Dec_app = -180 - Dec_app
            RA_app = RA_app + 180.0
        if(RA_app < 0.0): RA_app = RA_app + 360.0
        while(RA_app > 360.0):
            RA_app = RA_app - 360.0
        row.raapp = RA_app
        row.decapp = Dec_app
        row.source = new_source_info[id][1]
        SU_table_new.append(row)
    # Close the file to make sure the rows get written.
    SU_table_new.close()
    return





################################################################################
# set up the AlbusIonosphere for the AIPS reference information
# and get antenna info
def setup_ALbusIonosphere_for_ref_date(aips_data, AN_version):
    """set up the AlbusIonosphere for the AIPS reference information and get antenna info

    aips_data     I  the incoming UV dataset
    AN_version    I  the antenna table version to use.  You probably want 1
                     0 should give the highest.

    OUTPUT  antenna_info,subarray_info
    antenna_info  O  A list of antenna stuff, in mixed order, as
                     [station_number,name,pos[x,y,z]]
    subarray_info O  A list of keyword values for this subarray, as
                     [year,month,day,hour, minute,second,POLARX,POLARY,UT1UTC]
    """
    # Ok, grab the AN table
    aips_w_data = Wizardry.AIPSData.AIPSUVData(aips_data)
    an_table = aips_w_data.table('AIPS AN', AN_version)
    # Now, get the center of the array, as a numarray so we can do math
    # AIPS seems to be using a left-handed XYZ coordinate system,
    # SOMETIMES, for certain antennas.  ARRRGHHH!!!
    array_name = an_table.keywords['ARRNAM']
    if(len(array_name) == 1):
        raise RuntimeError, "Error: the keyword string bug is finally fixed"
    array_center = numarray.array([an_table.keywords['ARRAYX'],
                                   an_table.keywords['ARRAYY'],
                                   an_table.keywords['ARRAYZ']])
    MAGIC_SIZE = 1000.
    MAGIC_MULTIPLIER = 1.0
    if( ( (abs(array_center[0]) < MAGIC_SIZE) and
          (abs(array_center[1]) < MAGIC_SIZE) and
          (abs(array_center[2]) < MAGIC_SIZE)
          ) and not( (array_name == 'ATCA    ') or
                     (array_name == 'ATLBA   ') ) ):
        # using left-handed system
        MAGIC_MULTIPLIER = -1.0
    array_center[1] = array_center[1] * MAGIC_MULTIPLIER
    # AIPS dataset reference date
    (year_ref,month_ref,day_ref) = get_observation_year_month_day(aips_data)
    # The AN reference date
    (year_an, month_an, day_an) = get_AN_reference_year_month_day(an_table)
    if((year_ref,month_ref,day_ref) != (year_an, month_an, day_an)):
        #raise RuntimeError, "The reference dates don't match!"
        warnings.warn("The reference dates don't match!")
    # Now, get the time offset from the AIPS time to UTC
    UTC_offset = -an_table.keywords['DATUTC']
    # Now, set the AlbusIonosphere reference time
    retval = AlbusIonosphere.set_reference_time_AIPS(year_ref,
                                                     month_ref,
                                                     day_ref,
                                                     0,
                                                     0,
                                                     UTC_offset,
                                                     an_table.keywords['POLARX'],
                                                     an_table.keywords['POLARY'],
                                                     an_table.keywords['UT1UTC'])
    if(retval < 0):
        raise RuntimeError, "Error: AlbusIonosphere.set_reference_time_AIPS gave %d"%retval
    # Make a list of these parameters to send back to the caller
    subarray_info = [year_ref,
                     month_ref,
                     day_ref,
                     0,
                     0,
                     UTC_offset,
                     an_table.keywords['POLARX'],
                     an_table.keywords['POLARY'],
                     an_table.keywords['UT1UTC']
                     ]
    # Now make a list of the antenna station numbers and  positions.
    antenna_info = []
    for row in an_table:
        station = row.nosta
        pos = numarray.array(row.stabxyz)
        pos[1] = pos[1] * MAGIC_MULTIPLIER
        pos = pos + array_center
        name = row.anname
        if(row.mntsta == 2):
            warnings.warn("Orbiting Station '%s'found --- setting ionosphere to 0"%name)
            pos = pos * 0.0
        antenna_info.append([station,name,pos])
    return antenna_info,subarray_info







################################################################################
# make a proper array of station positions
def make_proper_antenna_postion_array(antenna_info):
    """convert a Python antenna_info list to a numarray of positions"""
    #First, what is the maximum station number?
    max_num = 0
    for ant in antenna_info:
        if(ant[0] > max_num): max_num = ant[0]
    # Now allocate an array
    antenna_array = numarray.zeros((max_num+1,3),type='Float64')
    # Now fill things in
    for ant in antenna_info:
        antenna_array[ant[0],:] = ant[2]
    # that's it
    return antenna_array












################################################################################
# Setup AlbusIonosphere for source
def setup_ALbusIonosphere_for_source(aips_data, processing_option, source):
    """Setup AlbusIonosphere for source

    aips_data         I  the main uv dataset
    processing_option I  the text string of the processing option
    source            I  Source information [ID, NAME, RA, Dec, EPOCH]
    """
    # First, tell Albus stuff where the source is
    retval = AlbusIonosphere.set_source_position(source[2], source[3])
    if(retval < 0):
        raise RuntimeError, "Error: AlbusIonosphere.set_source_position gave %d"%retval
    # do anything else that needs to be done here, perhaps
    # based on the processing option.  For now, this is all
    return

################################################################################
# Setup AlbusIonosphere for source
def setup_ALbusIonosphere_for_station(aips_data, processing_option, station):
    """Setup AlbusIonosphere for source

    aips_data         I  the main uv dataset
    processing_option I  the text string of the processing option
    station           I  Station information [ID, NAME, [x,y,z]]

    The return value of this function depends on whether the station is a
    valid Earth-based station.  1 means Earth-based, 0 means space-based
    """
    # First, check for a strange antenna location
    pos = station[2]
    if(numarray.innerproduct(pos,pos) == 0.0):
        # strange station
        return 0
    # Next, tell Albus stuff where the station is
    retval = AlbusIonosphere.set_station_position(pos[0],pos[1],pos[2])
    if(retval < 0):
        raise RuntimeError, "Error: AlbusIonosphere.set_station_position gave %d"%retval
    # do anything else that needs to be done here, perhaps
    # based on the processing option.  For now, this is all
    return 1














################################################################################
# Add blank entry to the AI table
def add_blank_entry_to_AI(AI_table, antenna_id, source_id, subarray,
                          time, time_width):
    """Add a blank entry to the AI table, to let a spacecraft observation through

    AI_table    I  The AI table to append to
    antenna_id  I  station number
    source_id   I  which source
    subarray    I  which subarray
    time        I  time in seconds since reference date
    time_width  I  full width in seconds for which solution to be blanked
    """
    new_row = Wizardry.AIPSData.AIPSTableRow(AI_table)
    new_row.time = time
    new_row.time_interval = time_width
    new_row.source_id = source_id
    new_row.antenna_no = antenna_id
    new_row.subarray = subarray
    new_row.az = 0.0
    new_row.el = 0.0
    new_row.ion_data = [0.0,0.0,1.0]
    AI_table.append(new_row)
    











################################################################################
# process a scan of data for AlbusIonosphere information
def process_AlbusIonosphere_for_scan(AI_table, antenna_id, source_id, subarray,
                                     start_time, end_time):
    """process a scan of data for AlbusIonosphere information

    This function assumes that everything except for a specific scan has been
    set up in the AlbusIonosphere information area.  This function will
    run through all of the times the AlbusIonosphere makes data, and appends
    the information tot he AI_table

    AI_table    I  The AI table to append to
    antenna_id  I  station number
    source_id   I  which source
    subarray    I  which subarray
    start_time  I  start of the scan in seconds since the reference day
    end_time    I  end of the scan in seconds since the reference day
    """
    # Tell AlbusIonosphere about the times
    retval = AlbusIonosphere.set_scan_times(start_time, end_time)
    if(retval < 0):
        raise RuntimeError, "Error: AlbusIonosphere.set_scan_times gave %d"%retval
    # Get a blank row
    row = Wizardry.AIPSData.AIPSTableRow(AI_table)
    # Now, how many times do we process
    Num_Data = AlbusIonosphere.get_Num_Ionospheric_Predictions()
    for i in xrange(Num_Data):
        # Calculate the Ionosphere
        retval,pred_time, time_width, El, Az, STEC, SRM, VTEC_factor = \
            AlbusIonosphere.get_ionospheric_prediction(i)
        if(retval < 0):
            raise RuntimeError, "Error: AlbusIonosphere.get_ionospheric_prediction gave %d"%retval
        row.time = pred_time * DAYS_PER_SECOND
        row.time_interval = time_width * DAYS_PER_SECOND
        row.source_id = source_id
        row.antenna_no = antenna_id
        row.subarray = subarray
        row.az = Az * M_RAD2DEG
        row.el = El * M_RAD2DEG
        row.ion_data = [STEC, SRM, VTEC_factor]
        #print row
        AI_table.append(row)
        













################################################################################
# Add information from a subarray to the AI table
def add_subarray_to_AI(aips_data, processing_option, AI_table, NX_table,
                       AN_version, sources):
    """Add information from a subarray to the AI table

    aips_data         I  the main uv dataset
    processing_option I  the text string of the processing option
    AI_table          B  The (opened) AI table to add information to
    NX_table          I  The (opened) NX table to get scan info from
    AN_version        I  The subarray version number to process
    sources           I  List of sources to get Ionosphere data for
    """
    # First, get the antenna information for subarray AN_version
    antenna_info,subarray_info = \
            setup_ALbusIonosphere_for_ref_date(aips_data, AN_version)
    # Loop over the antennas
    #print antenna_info 
    for antenna in antenna_info:
        station_id = antenna[0]
        print "Working on Antenna %2d ('%s')"%(station_id,antenna[1])
        #jma_aips.pause_prog("hit key to continue")
        # Check if we need to set up any special antenna-based information
        # in the AlbusIonosphere area
        station_code = setup_ALbusIonosphere_for_station(aips_data,
                                                         processing_option,
                                                         antenna)
        # Now, work our way through the scans
        for scan in NX_table:
            # If this is not our subarray, then skip
            if(scan.subarray != AN_version): continue
            source_id = scan.source_id
            # Get the start and stop times
            mid_time = scan.time * SECONDS_PER_DAY
            Delta_time = scan.time_interval * SECONDS_PER_DAY
            start_time = mid_time - 0.5 * Delta_time
            end_time   = mid_time + 0.5 * Delta_time
            # If we are working on a normal station
            if(station_code):
                # Now do each source.  If source_id is out of range, this
                # will throw an exception, which is what I want.
                source = sources[source_id]
                if(source != None):
                    print "   Working on Source %2d ('%s')"%(source_id,source[1])
                    # Check if we need to set up something special in ALbus
                    setup_ALbusIonosphere_for_source(aips_data,
                                                     processing_option,
                                                     source)
                    process_AlbusIonosphere_for_scan(AI_table, station_id,
                                                     source_id, AN_version,
                                                     start_time, end_time)
                else:
                    # No data for this source
                    raise RuntimeError, "Error: Cannot find source %d"%source_id
                
            else:
                add_blank_entry_to_AI(AI_table, station_id, source_id,
                                      AN_version,start_time-1.0,Delta_time+2.0)
                add_blank_entry_to_AI(AI_table, station_id, source_id,
                                      AN_version,end_time+1.0,Delta_time+2.0)
            # end of station_code check
        # for scan over NX table
    # for antenna over antenna info
    return

            
        





################################################################################
# Read in the data from an AI table
def read_in_AI(aips_data, AI_version):
    """Read in the entire AI table to memory
    
    aips_data          I  an AIPS uv dataset (standard, not Wizardry)
    AI_version         I  version number of the AI table to create

    This function will return AI_header_data,AI_table_data
    where

    AI_header_data  O  the important parts of the table information
                       NO_ANT, NO_SRC, NO_TERM, IONOTYPE, DATA_SRC
    AI_table_data   O  A list of the table rows
    """
    # Ok, open the table
    AI_dataset = Wizardry.AIPSData.AIPSUVData(aips_data.name,
                                              aips_data.klass, 
                                              aips_data.disk,
                                              aips_data.seq)
    AI_table = AI_dataset.table('AIPS AI', AI_version)
    num_rows = len(AI_table)
    AI_header_data = {}
    AI_header_list = ['NO_ANT', 'NO_SRC', 'NO_TERM', 'IONOTYPE', 'DATA_SRC']
    for item in AI_header_list:
        AI_header_data[item] = AI_table.keywords[item]
    # Now read in all of the rows.  Predeclare some numarray arrays to store
    # things in
    time = numarray.zeros((num_rows), type='Float64')
    time_interval = numarray.zeros((num_rows), type='Float64')
    source_id = numarray.zeros((num_rows), type='Int32')
    antenna_no = numarray.zeros((num_rows), type='Int32')
    subarray = numarray.zeros((num_rows), type='Int32')
    az = numarray.zeros((num_rows), type='Float64')
    el = numarray.zeros((num_rows), type='Float64')
    ion_data = numarray.zeros((num_rows,AI_header_data['NO_TERM']),
                              type='Float64')
    i=0
    for row in AI_table:
        time[i] = row.time
        time_interval[i] = row.time_interval
        source_id[i] = row.source_id
        antenna_no[i] = row.antenna_no
        subarray[i] = row.subarray
        az[i] = row.az
        el[i] = row.el
        ion_data[i,:] = row.ion_data
        i = i+1
    AI_table_data = {}
    AI_table_data['time'] = time
    AI_table_data['time_interval'] = time_interval
    AI_table_data['source_id'] = source_id
    AI_table_data['antenna_no'] = antenna_no
    AI_table_data['subarray'] = subarray
    AI_table_data['az'] = az
    AI_table_data['el'] = el
    AI_table_data['ion_data'] = ion_data
    return AI_header_data,AI_table_data








################################################################################
# Find the upper and lower indices for interpolating in the AI table
def find_AI_interpolation_high_low(AI_table, subarray_req, antenna_req,
                                   source_req, time_req,
                                   low, high):
    """Find the upper and lower indices for interpolating in the AI table

    AI_table       I  The dict of columns for the AI table. Note that this is NOT
                      the AIPS table, but the dict of all columns.  The entries
                      should be sorted by time, source, antenna, so that
                      all entries for a single source are together in increasing
                      time order for a single telescope/subarray.  See
                      read_in_AI for the description of the table
    subrarray_req  I  The subarray value
    antenna_req    I  The antenna
    source_req     I  The source ID
    time _req      I  The time slot
    low            I  The starting guess for the low index   0 to initialize
    high           I  The starting guess for the high index
                      Note that if low or high are not simple integers,
                      they might get modified.
    """
    assert(low >= 0)
    time = AI_table['time']
    subarray = AI_table['subarray']
    antenna = AI_table['antenna_no']
    source = AI_table['source_id']
    size = source.size()
    if(low == 0):
        while(low < size):
            if( (source[low] != source_req)
                or (antenna[low] != antenna_req)
                or (subarray[low] != subarray_req) ):
                low = low + 1
            else:
                break
        # This is the first time for this source.  Check that it is valid
        if((time[low] > time_req) or (low >= size)):
            # Not valid
            low = -1
            high = -1
            return low,high
    if(time[low] > time_req):
        # How did this slot have a time too great?  Try again
        low = 0
        return find_AI_interpolation_high_low(AI_table, subrarray_req,
                                              antenna_req,
                                              source_req, time_req,
                                              low, high)
    # Now, working from the last valid low, find others.
    low_last = low
    low = low + 1
    while(low < size):
        if( (source[low] != source_req) or (antenna[low] != antenna_req)
            or (subarray[low] != subarray_req) ):
            # Not a valid antenna/source, so continue
            pass
        else:
            # This is a valid antenna/soure.  Is thie time ok?
            if(time[low] <= time_req):
                # This is still a valid option
                low_last = low
                low = low + 1
            else:
                # the time is too large, so stop
                break
        low = low + 1
    low = low_last
    # Now do the high part
    if(high <= low): high = low+1
    while(high < size):
        if( (time[high] <= time_req) or (source[high] != source_req)
            or (antenna[high] != antenna_req)
            or (subarray[high] != subarray_req) ):
            high = high + 1
        else: break
    if(high >= size):
        # If we have ended here, then there is no data
        low = -1
        high = -1
    # low and high found
    return  low,high













################################################################################
# Find the upper and lower indices for interpolating in the AI table
def find_AI_nearest_time_point(AI_table, subarray_req, antenna_req,
                               source_req, time_req, last_index):
    """Find the upper and lower indices for interpolating in the AI table

    AI_table       I  The dict of columns for the AI table. Note that this is NOT
                      the AIPS table, but the dict of all columns.  The entries
                      should be sorted by time, source, antenna, so that
                      all entries for a single source are together in increasing
                      time order for a single telescope/subarray.  See
                      read_in_AI for the description of the table
    subrarray_req  I  The subarray value
    antenna_req    I  The antenna
    source_req     I  The source ID
    time _req      I  The time slot
    last_index     I  The last index as to where the nearest time point was
                      found.  Note that if this is not a simple integer, it
                      might get modified.
    """
    assert(last_index >= 0)
    time = AI_table['time']
    subarray = AI_table['subarray']
    antenna = AI_table['antenna_no']
    source = AI_table['source_id']
    size = source.size()
    # make sure that our index points to a valid antenna/source
    index = last_index
    while(index < size):
        if( (source[index] != source_req)
            or (antenna[index] != antenna_req)
            or (subarray[index] != subarray_req) ):
            index = index + 1
        else:
            break
    if(index >= size):
        # if we have made it all the way through, then something is strange, and
        # start over from 0
        if(last_index != 0):
            index=0
            return find_AI_nearest_time_point(AI_table, subarray_req,
                                              antenna_req,
                                              source_req, time_req, index)
        else:
            # there were no valid points!
            return -1
    # Ok, this is a valid point.  Get the time difference
    Delta_t_min = abs(time_req - time[index])
    # Check for going down in index
    last_index = index
    index = index-1
    while(index >= 0):
        if( (source[index] == source_req)
            and (antenna[index] == antenna_req)
            and (subarray[index] == subarray_req) ):
            Delta_t = abs(time_req - time[index])
            if(Delta_t > Delta_t_min):
                break
            else:
                Delta_t_min = Delta_t
                last_index = index
        index = index-1
    # Check for increasing index
    index = last_index+1
    while(index < size):
        if( (source[index] == source_req)
            and (antenna[index] == antenna_req)
            and (subarray[index] == subarray_req) ):
            Delta_t = abs(time_req - time[index])
            if(Delta_t > Delta_t_min):
                break
            else:
                Delta_t_min = Delta_t
                last_index = index
        index = index+1
    # Ok, this is it
    return last_index

















################################################################################
# linearly interpolate ion data in STEC space
def linear_interpolate_STEC(ion_data, time_array, index1, index2, calc_time):
    """linearly interpolate ion data in STEC space

    ion_data    I  a numarray[*,*] which holds the ionospheric data as
                   STEC        = ion_data[row,0]
                   SRM         = ion_data[row,1]
                   VTEC_factor = ion_data[row,2]
    time_array  I  a numarray[*] which holds the ionosphere times as
                   time_array[row]
    index1      I  The lower time index
    index2      I  The upper time index
    calc_time   I  The desired time

    OUTPUT
    STEC,SRM

    STEC        O  The slant total electron content, in electrons m^{-2}
    SRM         0  The slant rotation measure, in electrons m^{-2} T
    """
    t1 = time_array[index1]
    t2 = time_array[index2]
    if(t1 == t2):
        # Hey, the times are the same!
        if(t1 == calc_time):
            # But they are both the needed time, so don't worry
            return ion_data[index1,0],ion_data[index1,1]
        else:
            raise RuntimeError, "Error: Both times are the same!"
    # Ok, calculate the slope factor
    factor = (calc_time - t1) / (t2-t1)
    STEC = ion_data[index1,0] + (ion_data[index2,0] - ion_data[index1,0]) *factor
    SRM  = ion_data[index1,1] + (ion_data[index2,1] - ion_data[index1,1]) *factor
    #print STEC, SRM,t1,t2,calc_time
    #print ion_data[index1,0], ion_data[index2,0]
    return STEC,SRM

################################################################################
# linearly interpolate ion data in VTEC space
def linear_interpolate_VTEC(ion_data, time_array, index1, index2, calc_time):
    """linearly interpolate ion data in STEC space

    ion_data    I  a numarray[*,*] which holds the ionospheric data as
                   STEC        = ion_data[row,0]
                   SRM         = ion_data[row,1]
                   VTEC_factor = ion_data[row,2]
    time_array  I  a numarray[*] which holds the ionosphere times as
                   time_array[row]
    index1      I  The lower time index
    index2      I  The upper time index
    calc_time   I  The desired time

    OUTPUT
    STEC,SRM

    STEC        O  The slant total electron content, in electrons m^{-2}
    SRM         0  The slant rotation measure, in electrons m^{-2} T
    """
    t1 = time_array[index1]
    t2 = time_array[index2]
    if(t1 == t2):
        # Hey, the times are the same!
        if(t1 == calc_time):
            # But they are both the needed time, so don't worry
            return ion_data[index1,0],ion_data[index1,1]
        else:
            raise RuntimeError, "Error: Both times are the same!"
    # Ok, calculate the slope factor
    factor = (calc_time - t1) / (t2-t1)
    VTEC = ion_data[index1,0]*ion_data[index1,2] \
           + (ion_data[index2,0]*ion_data[index2,2]
              - ion_data[index1,0]*ion_data[index1,2]) *factor
    VRM   = ion_data[index1,1]*ion_data[index1,2] \
            + (ion_data[index2,1]*ion_data[index2,2]
               - ion_data[index1,1]*ion_data[index1,2]) *factor
    V = ion_data[index1,2] + (ion_data[index2,2] - ion_data[index1,2]) *factor
    STEC = VTEC / V
    SRM = VRM / V
    #print V, factor, VTEC, VRM, STEC, SRM,t1,t2,calc_time
    #print ion_data[index1,0], ion_data[index2,0], ion_data[index1,2],ion_data[index2,2]
    return STEC,SRM




################################################################################
# linearly interpolate ion data in VTEC space using the actual source VTEC factor
def linear_interpolate_VTEC_source(ion_data, time_array, index1, index2,
                                   calc_time, subarray_info_list, subarray,
                                   antenna_info_array, antenna,
                                   source_info_array, source,
                                   height_ionosphere = 300E3):
    """linearly interpolate ion data in STEC space using the actual source VTEC factor

    ion_data    I  a numarray[*,*] which holds the ionospheric data as
                   STEC        = ion_data[row,0]
                   SRM         = ion_data[row,1]
                   VTEC_factor = ion_data[row,2]
    time_array  I  a numarray[*] which holds the ionosphere times as
                   time_array[row]
    index1      I  The lower time index
    index2      I  The upper time index
    calc_time   I  The desired time
    subarray_info_list  I  LIst of information about subarray stuff
    subarray            I  The current subarray to use
    antenna_info_list   I  list of numarray array of antenna positions
                           antenna_info_list[subarray][antenna][XYZ]
    antenna             I  The current antenna number
    source_info_list    I  list of source position lists
    source              I  The current source number
    height_ionosphere   I  The height of the ionosphere to get the VTEC scaling
                           factor

    OUTPUT
    STEC,SRM

    STEC        O  The slant total electron content, in electrons m^{-2}
    SRM         0  The slant rotation measure, in electrons m^{-2} T
    """
    # How much of the AlbusIonosphere stuff do we need to update?
    if(subarray != linear_interpolate_VTEC_source.last_subarray):
        # This means that everything must be updated
        linear_interpolate_VTEC_source.last_subarray = subarray
        linear_interpolate_VTEC_source.last_antenna  = -1
        linear_interpolate_VTEC_source.last_source   = -1
        s_list = subarray_info_list[subarray]
        #print 'Setting up for subarray', subarray, s_list
        retval = AlbusIonosphere.set_reference_time_AIPS(s_list[0],
                                                         s_list[1],
                                                         s_list[2],
                                                         s_list[3],
                                                         s_list[4],
                                                         s_list[5],
                                                         s_list[6],
                                                         s_list[7],
                                                         s_list[8])
        if(retval < 0):
            raise RuntimeError, "Error: AlbusIonosphere.set_reference_time_AIPS gave %d"%retval
    if(antenna != linear_interpolate_VTEC_source.last_antenna):
        linear_interpolate_VTEC_source.last_antenna = antenna
        pos = antenna_info_array[subarray]
        #print 'Setting up for antenna', antenna, pos[antenna,0], pos[antenna,1], pos[antenna,2]
        retval = AlbusIonosphere.set_station_position(pos[antenna,0],
                                                      pos[antenna,1],
                                                      pos[antenna,2])
        if(retval < 0):
            raise RuntimeError, "Error: AlbusIonosphere.set_station_position gave %d"%retval
    if(source != linear_interpolate_VTEC_source.last_source):
        # Get the soruce info.  If this is out of range, then
        # this raises an exception.  If it is None, raise my own
        source_info = source_info_array[source]
        if(source_info == None):
            raise RuntimeError, "Error: no data for source %d"%source
        linear_interpolate_VTEC_source.last_source = source
        #print 'setting up for source', source, source_info
        retval = AlbusIonosphere.set_source_position(source_info[2],
                                                     source_info[3])
        if(retval < 0):
            raise RuntimeError, "Error: AlbusIonosphere.set_source_position gave %d"%retval
    # Now start dealing with the interpolation
    t1 = time_array[index1]
    t2 = time_array[index2]
    if(t1 == t2):
        # Hey, the times are the same!
        if(t1 == calc_time):
            # But they are both the needed time, so don't worry
            return ion_data[index1,0],ion_data[index1,1]
        else:
            raise RuntimeError, "Error: Both times are the same!"
    # Get the proper VTEC_factor
    retval, El, Az, VTEC_factor = \
        AlbusIonosphere.get_source_AzElVTEC(calc_time * SECONDS_PER_DAY,
                                            height_ionosphere)
    if(retval < 0):
        raise RuntimeError, "Error: AlbusIonosphere.get_source_AzElVTEC gave %d"%retval
    # Ok, calculate the slope factor
    factor = (calc_time - t1) / (t2-t1)
    VTEC = ion_data[index1,0]*ion_data[index1,2] \
           + (ion_data[index2,0]*ion_data[index2,2]
              - ion_data[index1,0]*ion_data[index1,2]) *factor
    VRM   = ion_data[index1,1]*ion_data[index1,2] \
            + (ion_data[index2,1]*ion_data[index2,2]
               - ion_data[index1,1]*ion_data[index1,2]) *factor
    STEC = VTEC / VTEC_factor
    SRM = VRM / VTEC_factor
    #print VTEC_factor, factor, VTEC, VRM, STEC, SRM,t1,t2,calc_time
    #print ion_data[index1,0], ion_data[index2,0], ion_data[index1,2],ion_data[index2,2]
    return STEC,SRM
linear_interpolate_VTEC_source.last_subarray = -1
linear_interpolate_VTEC_source.last_antenna  = -1
linear_interpolate_VTEC_source.last_source   = -1













################################################################################
# Get some info lists
def get_subarray_antenna_source_info(aips_data):
    """Get some info lists

    Note that this function call will leave the AlbusIonosphere stuff in
    a pretty-much indetreminate state for station/reference date information.

    aips_data  I  A normal AIPSUVDAta object

    OUTPUT
    subarray_info  O  List of information about subarray stuff, each element is
                      [year,month,day,hour, minute,second,POLARX,POLARY,UT1UTC]
    antenna_info   O  list of numarray array of antenna positions
                      antenna_info_list[subarray][antenna][XYZ]
                      from center of Earth, in a right-handed coordinate system
    source_info    O  list of source position lists
                      l = source_info[source_number] gives a list
                      l[0] =  source_id number
                      l[1] = name as a string
                      l[2] = RA (radians)
                      l[3] = Dec (radians)
                      l[4] = EPOCH (years)
    """
    # Create the top-level subarray and antenna lists
    num_subarrays = aips_data.table_highver('AIPS AN')
    subarray_info = (num_subarrays+1)*[None]
    antenna_info = copy.copy(subarray_info)
    # Ok, for all subarrays, add stuff on
    for an in xrange(1,num_subarrays+1):
        # check that this exists
        if not(check_table_version_exists(aips_data, 'AIPS AN', an)): continue
        # Ok, fill in the list stuff
        ant,sub = setup_ALbusIonosphere_for_ref_date(aips_data, an)
        ant_numarray = make_proper_antenna_postion_array(ant)
        subarray_info[an] = sub
        antenna_info[an] = ant_numarray
    source_info = get_source_positions(aips_data)
    return subarray_info,antenna_info,source_info















################################################################################
# Find the upper and lower indices for interpolating in the AI table
def apply_AI_table_to_CL_table(aips_data, AI_version, gainver, gainuse,
                               interpolation_method,
                               overwrite=0):
    """Find the upper and lower indices for interpolating in the AI table

    aips_data      I  an AIPS uv dataset (standard, not Wizardry)
    AI_version     I  The version of the AI table to apply
    gainver        I  The CL table to use for input.  Must exist
    gainuse        I  The CL table version to write out.  Note that if
                      overwrite is true, then this one will be deleted.  If
                      overwrite is false, then this table must not exist yet.
    interpolation_method
                   I  The method to use for interpolating between AI_table points
                      to get CL table data.  Valid options are
                      'NearestNeighbor' -- nearest point in time for the same
                                           antenna/source
                      'LinearInterpolateSTEC' -- linearly interpolate points
                                                 in STEC space
                      'LinearInterpolateVTEC' -- linearly interpolate points
                                                 in VTEC space
                      'LinearInterpolateVTECSource' -- linearly interpolate
                                                 points in VTEC space using the
                                                 proper VTEC_factor of the
                                                 source at the time
    overwrite      I  flag to indicate whether the new CL table may be
                      overwritten.
    """
    # Typically, if gainuse is 0 then get the *next* highest
    if(gainuse == 0):
        gianuse = aips_data.table_highver('AIPS CL') + 1
    # Next, make sure the new CL table is free
    verify_table_version_free(aips_data, 'AIPS CL', gainuse, overwrite)
    # Now read in the AI table
    print 'Reading in AI table', AI_version
    print 'This may take a long time.  Try reading some e-mail.'
    start_time = systime.clock()
    AI_header,AI_table = read_in_AI(aips_data, AI_version)
    print 'Ok, I have read the AI table in %.1f seconds.  Continuing.'%(systime.clock()-start_time)
    # Get some extra information if we really need it
    if(interpolation_method == 'LinearInterpolateVTECSource'):
        subarray_info,antenna_info,source_info = \
               get_subarray_antenna_source_info(aips_data)
    # Get a Wizardy object
    W_dataset = Wizardry.AIPSData.AIPSUVData(aips_data.name,
                                              aips_data.klass, 
                                              aips_data.disk,
                                              aips_data.seq)
    # Get the old table and the new one
    CL_table_in = W_dataset.table('CL', gainver)
    CL_table_out = W_dataset.attach_table(
            'CL', gainuse,
            no_term=CL_table_in.keywords['NO_TERM'])
    # How big is the CL table?
    Num_Total_CL_Rows = len(CL_table_in)
    #no_if=CL_table_in.keywords['NO_IF']  This is automatically copied
    #no_pol=CL_table_in.keywords['NO_POL']  This is automatically copied
    CL_table_out.keywords['REVISION'] = CL_table_in.keywords['REVISION']
    CL_table_out.keywords['NO_ANT'] = CL_table_in.keywords['NO_ANT']
    CL_table_out.keywords['MGMOD'] = CL_table_in.keywords['MGMOD']
    # how many polarizations are there? If 2, then set a flag
    two_pol = 0
    if(CL_table_in.keywords['NO_POL'] > 1): two_pol = 1
    # Create a couple of placeholders to store where we are in the AI_table
    num_ant = find_antenna_highnumber(aips_data)
    low = numarray.zeros((aips_data.table_highver('AIPS AN')+1,num_ant+1),
                         type='Int32')
    high = numarray.zeros((aips_data.table_highver('AIPS AN')+1,num_ant+1),
                          type='Int32')
    # directly hold the ion data and time information
    ion_data = AI_table['ion_data']
    time_array = AI_table['time']
    # If doing the full interpolation, initialize the subroutine
    if(interpolation_method == 'LinearInterpolateVTECSource'):
        linear_interpolate_VTEC_source.last_subarray = -1
        linear_interpolate_VTEC_source.last_antenna  = -1
        linear_interpolate_VTEC_source.last_source   = -1
    # Now loop over all of the rows in the old table to update and
    # place into the new one.  Keep track of how far along I am, so that
    # I can print out some progress messages
    start_time = systime.clock()
    count = -1
    for row in CL_table_in:
        # For testing, only do first few
        count = count + 1
        #if(count > 10):
        #    warnings.warn("Ending CL table loop eary for debugging")
        #    break
        if( (count&0x3FF) == 0 ):
            print "%5.1f%% completed in %10.1f s"%\
                  ((float(count)*100.0/Num_Total_CL_Rows),
                   systime.clock()-start_time)
            #if(count > 10000):
            #    warnings.warn("Ending CL table loop eary for debugging")
            #    break
        # Need information about this row
        subarray = row.subarray
        antenna = row.antenna_no
        source = row.source_id
        time = row.time
        STEC = 0.0
        SRM = 0.0
        # Which interpolation method to use?
        if(interpolation_method == 'NearestNeighbor'):
            index = find_AI_nearest_time_point(AI_table, subarray,
                                               antenna, source, time,
                                               low[subarray,antenna])
            if(index >= 0):
                STEC = ion_data[index,0]
                SRM = ion_data[index,1]
                low[subarray,antenna] = index
            else:
                # No data, so apply a zero correction, which is already set
                # above
                low[subarray,antenna] = 0
        elif(interpolation_method == 'LinearInterpolateSTEC'):
            index1,index2 = find_AI_interpolation_high_low(AI_table, subarray,
                                                           antenna, source, time,
                                                           low[subarray,antenna],
                                                           high[subarray,antenna])
            if(index1 >= 0):
                STEC,SRM = linear_interpolate_STEC(ion_data, time_array,
                                                   index1, index2, time)
                low[subarray,antenna]  = index1
                high[subarray,antenna] = index2
            else:
                # No data, so reset the positions
                low[subarray,antenna]  = 0
                high[subarray,antenna] = 0
        elif(interpolation_method == 'LinearInterpolateVTEC'):
            index1,index2 = find_AI_interpolation_high_low(AI_table, subarray,
                                                           antenna, source, time,
                                                           low[subarray,antenna],
                                                           high[subarray,antenna])
            if(index1 >= 0):
                STEC,SRM = linear_interpolate_VTEC(ion_data, time_array,
                                                   index1, index2, time)
                low[subarray,antenna]  = index1
                high[subarray,antenna] = index2
            else:
                # No data, so reset the positions
                low[subarray,antenna]  = 0
                high[subarray,antenna] = 0
        elif(interpolation_method == 'LinearInterpolateVTECSource'):
            index1,index2 = find_AI_interpolation_high_low(AI_table, subarray,
                                                           antenna, source, time,
                                                           low[subarray,antenna],
                                                           high[subarray,antenna])
            if(index1 >= 0):
                STEC,SRM = linear_interpolate_VTEC_source(ion_data, time_array,
                                                          index1, index2,
                                                          time,
                                                          subarray_info,subarray,
                                                          antenna_info, antenna,
                                                          source_info, source)
                low[subarray,antenna]  = index1
                high[subarray,antenna] = index2
            else:
                # No data, so reset the positions
                low[subarray,antenna]  = 0
                high[subarray,antenna] = 0
        else:
            raise RuntimeError, "Error: unknown interpolation method '%s'"%interpolation_method
        #update the row
        disp = AIPS_TEC_TO_DISP * STEC
        rm = AIPS_RM_TO_RM * SRM
        row.i_far_rot = row.i_far_rot + rm
        row.disp_1 = row.disp_1  + disp
        if(two_pol):
            row.disp_2 = row.disp_2 + disp
        #print 'Final Values', disp, rm, STEC, SRM
        #print row
        CL_table_out.append(row)
    # Close the file to make sure the rows get written.
    CL_table_out.close()
    return




















################################################################################
# negate the dispersive delay value
def negate_disp_in_CL_table(aips_data, gainver, gainuse, overwrite=0):
    """negate the dispersive delay value

    aips_data      I  an AIPS uv dataset (standard, not Wizardry)
    gainver        I  The CL table to use for input.  Must exist
    gainuse        I  The CL table version to write out.  Note that if
                      overwrite is true, then this one will be deleted.  If
                      overwrite is false, then this table must not exist yet.
    overwrite      I  flag to indicate whether the new CL table may be
                      overwritten.
    """
    # Typically, if gainuse is 0 then get the *next* highest
    if(gainuse == 0):
        gianuse = aips_data.table_highver('AIPS CL') + 1
    # Next, make sure the new CL table is free
    verify_table_version_free(aips_data, 'AIPS CL', gainuse, overwrite)
    # Get a Wizardy object
    W_dataset = Wizardry.AIPSData.AIPSUVData(aips_data.name,
                                              aips_data.klass, 
                                              aips_data.disk,
                                              aips_data.seq)
    # Get the old table and the new one
    CL_table_in = W_dataset.table('CL', gainver)
    CL_table_out = W_dataset.attach_table(
            'CL', gainuse,
            no_term=CL_table_in.keywords['NO_TERM'])
    #no_if=CL_table_in.keywords['NO_IF']  This is automatically copied
    #no_pol=CL_table_in.keywords['NO_POL']  This is automatically copied
    CL_table_out.keywords['REVISION'] = CL_table_in.keywords['REVISION']
    CL_table_out.keywords['NO_ANT'] = CL_table_in.keywords['NO_ANT']
    CL_table_out.keywords['MGMOD'] = CL_table_in.keywords['MGMOD']
    # how many polarizations are there? If 2, then set a flag
    two_pol = 0
    if(CL_table_in.keywords['NO_POL'] > 1): two_pol = 1
    # Now loop over the rows in the table
    if(two_pol):
        for row in CL_table_in:
            #update the row
            row.disp_1 = -row.disp_1
            row.disp_2 = -row.disp_2
            CL_table_out.append(row)
    else:
        for row in CL_table_in:
            #update the row
            row.disp_1 = -row.disp_1
            CL_table_out.append(row)
    # Close the file to make sure the rows get written.
    CL_table_out.close()
    return





################################################################################
def ionosphere_simple_ionex(aips_data, processing_option, gainver, gainuse,
                            overwrite=0,
                            output_directory=".", aparm=[None,1,0]):
    """Apply a simple IONEX dataset using TECOR

This function will get an IONEX dataset from the web (unless it already exists)
and apply it to your dataset.

INPUTS:
    aips_data      I  an AIPS uv dataset (standard, not Wizardry)
    processing_option  I  a string defining which model to use
    gainver        I  The CL table to use for input.  Must exist
    gainuse        I  The CL table version to write out.  Note that if
                      overwrite is true, then this one will be deleted.  If
                      overwrite is false, then this table must not exist yet.
    overwrite      I  flag to indicate whether the new CL table may be
                      overwritten.
    output_directory   I  Where to put things
    aparm          I  TECOR aparm string to apply

    """
    group_name = ''
    if(processing_option == 'IO_COD'):
        group_name='cod'
    elif(processing_option == 'IO_COR'):
        group_name='cor'
    elif(processing_option == 'IO_ESA'):
        group_name='esa'
    elif(processing_option == 'IO_ESR'):
        group_name='esr'
    elif(processing_option == 'IO_IGR'):
        group_name='igr'
    elif(processing_option == 'IO_IGS'):
        group_name='igs'
    elif(processing_option == 'IO_JPL'):
        group_name='jpl'
    elif(processing_option == 'IO_JPR'):
        group_name='jpr'
    elif(processing_option == 'IO_UPC'):
        group_name='upc'
    elif(processing_option == 'IO_UPR'):
        group_name='upr'
    else:
        raise RuntimeError, "unknown ionosphere processing option " + processing_option._str__()
    IONEX_filename = group_name + 'g%3.3d0.%si'%(doy,year)
    year, month, day = get_observation_year_month_day(aips_data)
    doy = get_observation_day_of_year(aips_data)
    retval = get_IONEX_file_from_web(IONEX_filename,
                                     year, month, day, doy,
                                     output_directory, 0, overwrite)
    if(retval < 0):
        raise RuntimeError("Unable to download IONEX file '%s'"%IONEX_filename)
    filename = output_directory + '/' + IONEX_filename
    # Now run TECOR
    tecor = AIPSTask('tecor', version = jma_aips.aips_version_std)
    tecor.indata = aips_data
    tecor.infile = filename
    tecor.nfiles = get_number_days_observations(aips_data)
    tecor.gainver = gainver
    tecor.gainuse = gainuse
    tecor.aparm = jma_aips.to_aips_array(aparm)
    tecor()
    return


























################################################################################
def ionosphere_GPS_set_criteria(Max_Sat_Sky_Angle_         = 2.0*math.pi,
                                Min_Sat_Elev_              = 0.1745,     
                                Max_Rec_Dist_From_Tele_    =  600E3,     
                                Max_Iono_Pierce_Dist_      = 2000E3,     
                                Default_Iono_Height_       =  300E3,     
                                Averaging_Time_Half_Width_ = 0.5,        
                                Num_Ionosphere_Parameters_ = 25,         
                                Num_Ionosphere_Heights_    =  3,
                                Num_Time_Terms_            =  1,
                                Theo_Model_Type_           =  3,
                                Bias_Fit_Type_             =  3
                                ):
    """Set the GPS calibration satellite criteria
    
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
                                     1 IRI
                                     2 IRI_Plus
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
ionosphere_GPS_set_criteria.Max_Sat_Sky_Angle          = 2.0*math.pi
ionosphere_GPS_set_criteria.Min_Sat_Elev               = 0.1745
ionosphere_GPS_set_criteria.Max_Rec_Dist_From_Tele     =  600E3
ionosphere_GPS_set_criteria.Max_Iono_Pierce_Dist       = 2000E3
ionosphere_GPS_set_criteria.Default_Iono_Height        =  300E3
ionosphere_GPS_set_criteria.Averaging_Time_Half_Width  = 0.5
ionosphere_GPS_set_criteria.Num_Ionosphere_Parameters  = 25
ionosphere_GPS_set_criteria.Num_Ionosphere_Heights     =  3
ionosphere_GPS_set_criteria.Num_Time_Terms             =  1
ionosphere_GPS_set_criteria.Theo_Model_Type            =  3
ionosphere_GPS_set_criteria.Bias_Fit_Type              =  3






################################################################################
def ionosphere_GPS_setup_GPS(aips_data, processing_option, 
                             overwrite=0,output_directory = "."):
    """set up for a GPS fitting model

INPUTS:
aips_data          I  an AIPS uv dataset (standard, not Wizardry)
processing_option  I  a string defining which model to use
overwrite          I  flag as to whether the given table version may be
                          overwritten
output_directory   I  Where to put things


OUTPUTS: None
    """
    assert(processing_option in PROCESSING_OPTION_RINEX_GROUPS)
    # First, get a list of all telescope positions
    num_subarrays = aips_data.table_highver('AIPS AN')
    telescope_pos = []
    subarray = [None] * (num_subarrays+1)
    # Ok, for all subarrays, add stuff on
    for an in xrange(1,num_subarrays+1):
        # check that this exists
        if not(check_table_version_exists(aips_data, 'AIPS AN', an)): continue
        # Ok, fill in the list stuff
        ant,sub = setup_ALbusIonosphere_for_ref_date(aips_data, an)
        subarray[an] = jma_aips.get_MJD_hms(sub[0], sub[1], sub[2],
                                            sub[3], sub[4], sub[5])
        for a in ant:
            if((a[2][0] == 0.0)and(a[2][1] == 0.0)and(a[2][2] == 0.0)):
                continue
            telescope_pos.append(a[2])
    GPS_receivers = \
        GPS_stations.get_stations_within_distance_2( 
            telescope_pos,
            ionosphere_GPS_set_criteria.Max_Rec_Dist_From_Tele)
    if(ionosphere_GPS_set_criteria.Bias_Fit_Type>=2):
        print "Adding global stations to improve bias fitting"
        GPS_receivers = GPS_stations.add_global_stations_to_list(GPS_receivers)
    # Use the NX table to find the total duration of the observations
    Obs_Start = 1E300
    Obs_End = -1E300
    NX_table = aips_data.table('NX', 0)
    for scan in NX_table:
        # Get the start and stop times
        start_time = scan.time - 0.5 * scan.time_interval \
                     + subarray[scan.subarray]
        end_time   = scan.time + 0.5 * scan.time_interval \
                     + subarray[scan.subarray]
        if(start_time < Obs_Start):
            Obs_Start = start_time
        if(end_time > Obs_End):
            Obs_End = end_time
    # Fudge by 30 seconds each way
    Obs_Start -= 30.0 * DAYS_PER_SECOND
    Obs_End   += 30.0 * DAYS_PER_SECOND
    # Ok, start initializing the GPS data area
    if(AlbusIonosphere.clear_everything()):
        raise RuntimeError("could not clear AlbusIonosphere")
    if(AlbusIonosphere.cal_observations_init(len(GPS_receivers))):
        raise RuntimeError("could not init AlbusIonosphere")
    fit_type = int(processing_option[4:6])
    if(AlbusIonosphere.cal_observations_set_parameters(
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
        raise RuntimeError("could not set criteria for AlbusIonosphere")
    print "Starting to load in GPS receiver data"
    MJD_array = None
    sat_XYZ   = None
    receiver_count = 0
    SIG_pos = Albus_RINEX_2._DATA_POS['SIGMA']
    PL_pos = Albus_RINEX_2._DATA_POS['STECPL']
    print "Processing data from %d GPS receivers"%(len(GPS_receivers))
    for i in xrange(len(GPS_receivers)):
        GPS = GPS_receivers[i]
        print "Getting data from receiver %d/%d   '%s'"%(i,len(GPS_receivers),GPS[0])
        try:
            MJD_array, Sat_array, obs_data, sta_XYZ, sat_XYZ, sta_bias_valid, sat_block_pos = \
                Albus_RINEX_2.get_multiple_station_base_observations(Obs_Start,
                                                                     Obs_End,
                                                                     GPS[0],
                                                                     MJD_array,
                                                                     sat_XYZ,
                                                                     output_directory=output_directory,
                                                                     overwrite=overwrite,
                                                                     use_bin_files=1)
            if(receiver_count == 0):
                if(AlbusIonosphere.cal_observations_set_times(
                    len(MJD_array), MJD_array)):
                    raise RuntimeError("could not set MJDs for AlbusIonosphere")
                sh = sat_XYZ.getshape()
                if(AlbusIonosphere.cal_observations_set_sat_pos(
                    sh[0], sh[1], sh[2], sat_XYZ)):
                    raise RuntimeError("could not set sat pos for AlbusIonosphere")
            receiver_count += 1
            Sat_small = Albus_RINEX_2.convert_Sat_array_to_small_array(Sat_array,
                                                                       obs_data)
            block_small = \
                Albus_RINEX_2.convert_sat_block_pos_to_small_array(sat_block_pos,
                                                                   Sat_array,
                                                                   obs_data)
            sh = obs_data.getshape()
            # convert from TECU to m^{-2}
            STEC = obs_data[:,:,PL_pos]
            sigma = obs_data[:,:,SIG_pos]
            for t in xrange(sh[0]):
                for s in xrange(sh[1]):
                    if((STEC[t,s] != Albus_RINEX_2.BAD_DATA_CODE)
                       and(sigma[t,s] != Albus_RINEX_2.BAD_DATA_CODE)):
                        STEC[t,s] *= Albus_RINEX_2.TECU_TO_M_N2
                        sigma[t,s] *= Albus_RINEX_2.TECU_TO_M_N2
                    else:
                        STEC[t,s] = Albus_RINEX_2.BAD_DATA_CODE
                        sigma[t,s] = Albus_RINEX_2.BAD_DATA_CODE
            if(AlbusIonosphere.cal_observations_set_cal_obs(
                GPS[0], GPS[1], sh[0], sh[1], Sat_small, block_small,
                STEC, sigma, sta_bias_valid)):
                raise RuntimeError("could not set receiver data for AlbusIonosphere")
        except Albus_RINEX.No_RINEX_File_Error, error:
            # try to ignore this error for now
            print(str(error))
            print "Failed to get data for GPS receiver '%s', skipping"%(GPS[0])
        except Albus_RINEX.RINEX_Data_Barf, error:
            # ignore the stupid header problems for now
            print(str(error))
            print "Failed to get data for GPS receiver '%s' because of header/data problems, skipping"%(GPS[0])
    if(receiver_count == 0):
        # somehow we have no good GPS receivers with data.  bail
        raise RuntimeError("could not find any good GPS receivers for AlbusIonosphere")
    if(AlbusIonosphere.cal_observations_init2()):
        raise RuntimeError("could not correct bias levels in AlbusIonosphere")
    return









    
    




################################################################################
# Run a model Ionosphere to make an AI table
def ionosphere_process_model(aips_data, processing_option, tolerance, AI_version,
                             overwrite=1, output_directory="."):
    """Make an AI table from a model ionosphere, using the AlbusIonosphere stuff

    aips_data          I  an AIPS uv dataset (standard, not Wizardry)
    processing_option  I  a string defining which model to use
    tolerance          I  fractional uncertainty in total electron content to
                          ask for.  Note that this is the requested fractional
                          accuracy, not the achieved.  For L-band EVN
                          observations, you should probably ask for no worse
                          than 1E-4, and probably 1E-5.  For lower frequencies,
                          you want even better precision, probably 1E-6.
    AI_version         I  version number of the AI table to create
    overwrite          I  flag as to whether the given table version may be
                          overwritten
    output_directory   I  Where to put things
    """
    # check for an existing table version
    if(AI_version == 0):
        AI_version = aips_data.table_highver('AIPS AI') + 1
    verify_table_version_free(aips_data, 'AIPS AI', AI_version, overwrite)
    # Start up the Ionosphere model
    retval = 0
    if(processing_option == 'MO_IRI'):
        retval = AlbusIonosphere.set_ionosphere_IRI(tolerance,tolerance*100.0)
    elif(processing_option == 'MO_PIM'):
        retval = AlbusIonosphere.set_ionosphere_PIM(tolerance,tolerance*100.0)
    elif(processing_option in PROCESSING_OPTION_RINEX_GROUPS):
        ionosphere_GPS_setup_GPS(aips_data, processing_option, overwrite,
                                 output_directory)
        retval = AlbusIonosphere.set_ionosphere_GPS(tolerance,tolerance*100.0)
    else:
        raise RuntimeError, "unknown ionosphere processing option " + processing_option._str__()
    if(retval < 0):
        raise RuntimeError, "Error:  AlbusIonosphere initilization for '%s' gave %d"%(processing_option,retval)
    # Now open the table for output
    AI_dataset = Wizardry.AIPSData.AIPSUVData(aips_data.name,
                                              aips_data.klass, 
                                              aips_data.disk,
                                              aips_data.seq)
    CL_table = AI_dataset.table('CL', 0)
    AI_table = AI_dataset.attach_table('AI', AI_version, no_term=3)
    AI_table.keywords['NO_ANT'] = CL_table.keywords['NO_ANT']
    AI_table.keywords['NO_SRC'] = find_antenna_highnumber(aips_data)
    AI_table.keywords['IONOTYPE'] = 'TEST1'.ljust(8)[0:8]
    AI_table.keywords['DATA_SRC'] = processing_option.ljust(8)[0:8]
    # I need the NX table
    NX_table = AI_dataset.table('NX', 0)
    # I need the sources
    sources = get_source_positions(aips_data)
    #print sources
    # Ok, for all subarrays, add stuff on
    for an in xrange(1,aips_data.table_highver('AIPS AN')+1):
        # check that this exists
        if not(check_table_version_exists(aips_data, 'AIPS AN', an)): continue
        # Ok, fill in the table
        add_subarray_to_AI(aips_data, processing_option, AI_table, NX_table,
                           an, sources)
    AI_table.close()
    return
    
            



    
    













################################################################################
# Run a simple ionospheric model
def ionosphere_simple_model(aips_data, processing_option, tolerance, gainver,
                            gainuse, overwrite=0, output_directory="."):
    """simple function to run a new ionospheric model to generate a new CL file

    aips_data      I  an AIPS uv dataset (standard, not Wizardry)
    processing_option  I  a string defining which model to use
    tolerance      I  fractional uncertainty in total electron content to
                      ask for.  Note that this is the requested fractional
                      accuracy, not the achieved.  For L-band EVN
                      observations, you should probably ask for no worse
                      than 1E-4, and probably 1E-5.  For lower frequencies,
                      you want even better precision, probably 1E-6.
    gainver        I  The CL table to use for input.  Must exist
    gainuse        I  The CL table version to write out.  Note that if
                      overwrite is true, then this one will be deleted.  If
                      overwrite is false, then this table must not exist yet.
    overwrite      I  flag to indicate whether the new CL table may be
                      overwritten.
    output_directory   I  Where to put things
    """
    # Use the next highest AI table number
    AI_version = 0
    #AI_version = 3  # use a specific version
    # Make the new table
    ionosphere_process_model(aips_data, processing_option, tolerance, AI_version,
                             overwrite, output_directory)
    # And put it into the new CL table
    apply_AI_table_to_CL_table(aips_data, AI_version, gainver, gainuse,
                               'LinearInterpolateVTEC', overwrite)
    # That's all
    return









################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
def process_ALBUS_ionosphere(aips_data, processing_option, tolerance, gainver,
                             gainuse, overwrite=0, output_directory="."):
    """This is the main Albus_Iono ionospheric processing function.

If you want to put an ionospheric model into your AIPS dataset through ALBUS_Iono
this is the routine you probably want to call.  This will check the processing
option you have selected, and from that call the necessary routines to
model the ionospheric data for you.  If external datasets from the Internet
are necessary, they will be placed into output_directory.  If such files
already exist but are old and you want new, updated versions, set overwrite to 1.

If you are going to be using GPS data, then you should have already
set up your data selection criteria by calling ionosphere_GPS_set_criteria
BEFORE calling this function.

INPUTS:
    aips_data      I  an AIPS uv dataset (standard, not Wizardry)
    processing_option  I  a string defining which model to use
    tolerance      I  fractional uncertainty in total electron content to
                      ask for.  Note that this is the requested fractional
                      accuracy, not the achieved.  For L-band EVN
                      observations, you should probably ask for no worse
                      than 1E-4, and probably 1E-5.  For lower frequencies,
                      you want even better precision, probably 1E-6.
    gainver        I  The CL table to use for input.  Must exist
    gainuse        I  The CL table version to write out.  Note that if
                      overwrite is true, then this one will be deleted.  If
                      overwrite is false, then this table must not exist yet.
    overwrite      I  flag to indicate whether the new CL table may be
                      overwritten.
    output_directory   I  Where to put things

OUTPUTS: None
    """
    jma_aips.make_sure_directory_exists(output_directory)
    if(processing_option[:3] == 'IO_'):
        ionosphere_simple_ionex(aips_data, processing_option, gainver, gainuse,
                            overwrite, output_directory)
    elif((processing_option[:3] == 'MO_') or (processing_option[:3] == 'RI_')):
        # Model stuff
        # or GPS RINEX stuff
        Albus_Iono.ionosphere_simple_model(aips_data, processing_option,
                                           tolerance, gainver,gainuse,
                                           overwrite, output_directory)
    else:
        # don't know what to do
        raise RuntimeError, "unknown ionosphere processing option " + processing_option._str__()
    return



















################################################################################
def replace_CL_ionosphere_from_CLs(aips_data,
                                   CL_old_ionosphere,
                                   CL_new_ionosphere,
                                   CL_current_version,
                                   CL_new_version,
                                   overwrite = 0
                                   ):
    """Substitute new ionosphere values from CL tables

Suppose that you have your ionospheric delay and Faraday rotation values in
some table CL_old_ionosphere, which you used to process and FRING and so on.
But now you have a new set of ionospheric corrections in table CL_new_ionosphere,
perhaps because you are looking in a slightly different direction, or because
you want to test some other things.  But you have processed lots of things,
and don't want to give up the FRING delays and rates and so on, which are
now in CL_current_version.

This function will take the stuff from CL_current_version, subtract the
ionospheric related stuff from CL_old_ionosphere and add in the stuff from
CL_new_ionosphere, producing a new table CL_new_version.


aips_data          I  an AIPS uv dataset (standard, not Wizardry)
CL_old_ionosphere  I  An old CL table with just ionsphere info
                      This should be the CL table number (integer>0) or None
CL_new_ionosphere  I  A new CL table with just ionosphere stuff 
                      This should be the CL table number (integer>0) or None
CL_current_version I  The current CL version to change stuff in.
                      This should be the CL table number (integer), or 0
                      to get the higherst version
CL_new_version     I  The new CL table to write to.
                      This should be the CL table number (integer), or 0
                      to get the next highest available table.
overwrite          I  flag to indicate whether the new CL table may be
                      overwritten if a table already exists
    """
    assert((CL_old_ionosphere > 0) or (CL_old_ionosphere == None))
    assert((CL_new_ionosphere > 0) or (CL_new_ionosphere == None))
    assert(CL_current_version >= 0)
    assert(CL_new_version >= 0)
    assert(jma_aips.check_table_version_valid(aips_data, 'AIPS CL',
                                              CL_current_version))
    
    # Typically, if gainuse is 0 then get the *next* highest
    if(CL_current_version == 0):
        CL_current_version = aips_data.table_highver('AIPS CL')
    if(CL_new_version == 0):
        CL_new_version = aips_data.table_highver('AIPS CL') + 1
    # Next, make sure the new CL table is free
    verify_table_version_free(aips_data, 'AIPS CL', CL_new_version, overwrite)
    # Get a Wizardy object
    W_dataset = Wizardry.AIPSData.AIPSUVData(aips_data.name,
                                             aips_data.klass, 
                                             aips_data.disk,
                                             aips_data.seq)
    # Get the old table and the new one
    CL_table_in = W_dataset.table('CL', CL_current_version)
    CL_table_out = W_dataset.attach_table(
        'CL', CL_new_version,
        no_term=CL_table_in.keywords['NO_TERM'])
    # How big is the CL table?
    Num_Total_CL_Rows = len(CL_table_in)
    #no_if=CL_table_in.keywords['NO_IF']  This is automatically copied
    #no_pol=CL_table_in.keywords['NO_POL']  This is automatically copied
    CL_table_out.keywords['REVISION'] = CL_table_in.keywords['REVISION']
    CL_table_out.keywords['NO_ANT'] = CL_table_in.keywords['NO_ANT']
    CL_table_out.keywords['MGMOD'] = CL_table_in.keywords['MGMOD']
    # how many polarizations are there? If 2, then set a flag
    two_pol = 0
    if(CL_table_in.keywords['NO_POL'] > 1): two_pol = 1
    # Get the Ionosphere tables as necessary
    CL_table_old = None
    CL_table_new = None
    if(CL_old_ionosphere):
        CL_table_old = W_dataset.table('CL', CL_old_ionosphere)
    if(CL_new_ionosphere):
        CL_table_new = W_dataset.table('CL', CL_new_ionosphere)
    start_time = systime.clock()
    count = -1
    for row in CL_table_in:
        count = count + 1
        if( (count&0x3FF) == 0 ):
            print "%5.1f%% completed in %10.1f s"%\
                  ((float(count)*100.0/Num_Total_CL_Rows),
                   systime.clock()-start_time)
        #print "Original Values", row.i_far_rot, row.disp_1, row.ddisp_1, row.delay_1
        #update the row
        if(CL_old_ionosphere):
            row.i_far_rot = row.i_far_rot - CL_table_old[count].i_far_rot
            row.disp_1 = row.disp_1  - CL_table_old[count].disp_1
            row.ddisp_1 = row.ddisp_1  - CL_table_old[count].ddisp_1
            if(two_pol):
                row.disp_2 = row.disp_2 - CL_table_old[count].disp_2
                row.ddisp_2 = row.ddisp_2  - CL_table_old[count].ddisp_2
        if(CL_new_ionosphere):
            row.i_far_rot = row.i_far_rot + CL_table_new[count].i_far_rot
            row.disp_1 = row.disp_1  + CL_table_new[count].disp_1
            row.ddisp_1 = row.ddisp_1  + CL_table_new[count].ddisp_1
            if(two_pol):
                row.disp_2 = row.disp_2 + CL_table_new[count].disp_2
                row.ddisp_2 = row.ddisp_2  + CL_table_new[count].ddisp_2
        CL_table_out.append(row)
        #print "New Values", row.i_far_rot, row.disp_1, row.ddisp_1
    # Close the file to make sure the rows get written.
    CL_table_out.close()
    return
    



























################################################################################
def get_VTEC_for_AIPS_data(calc_time, subarray_info_list, subarray,
                           antenna_info_array, antenna,
                           source_info_array, source,
                           height_ionosphere = 300E3,
                           RA_alt = None, Dec_alt = None):
    """Just get a simple VTEC value for an observation point


calc_time           I  The desired time, as an offset from the subarray reference
                       time
subarray_info_list  I  LIst of information about subarray stuff
subarray            I  The current subarray to use
antenna_info_list   I  list of numarray array of antenna positions
                       antenna_info_list[subarray][antenna][XYZ]
antenna             I  The current antenna number
source_info_list    I  list of source position lists
source              I  The current source number
height_ionosphere   I  The height of the ionosphere to get the VTEC scaling
                       factor
RA_alt              I  Optional alternate Right Ascension to use instead
                       of the source position in the source list, when
                       not None.  Value in RADIANS
                       Note that the alternate RA position will only be checked
                       if the source number or subarray changes.
Dec_alt             I  Optional alternate Declination to use instead
                       of the source position in the source list, when
                       not None.  Value in RADIANS
                       Note that the alternate Dec position will only be checked
                       if the source number or subarray changes.

OUTPUT

VTEC_factor         O  The approximate conversion factor to get
                       Vertical TEC from Slant TEC.  unitless.
                       Multiply STEC by VTEC_factor to get VTEC
El                  O  Elevation angle, in radians
Az                  0  Azimuth angle, in radians
    """
    # How much of the AlbusIonosphere stuff do we need to update?
    if(subarray != get_VTEC_for_AIPS_data.last_subarray):
        # This means that everything must be updated
        get_VTEC_for_AIPS_data.last_subarray = subarray
        get_VTEC_for_AIPS_data.last_antenna  = -1
        get_VTEC_for_AIPS_data.last_source   = -1
        s_list = subarray_info_list[subarray]
        #print 'Setting up for subarray', subarray, s_list
        retval = AlbusIonosphere.set_reference_time_AIPS(s_list[0],
                                                         s_list[1],
                                                         s_list[2],
                                                         s_list[3],
                                                         s_list[4],
                                                         s_list[5],
                                                         s_list[6],
                                                         s_list[7],
                                                         s_list[8])
        if(retval < 0):
            raise RuntimeError, "Error: AlbusIonosphere.set_reference_time_AIPS gave %d"%retval
    if(antenna != get_VTEC_for_AIPS_data.last_antenna):
        get_VTEC_for_AIPS_data.last_antenna = antenna
        pos = antenna_info_array[subarray]
        #print 'Setting up for antenna', antenna, pos[antenna,0], pos[antenna,1], pos[antenna,2]
        retval = AlbusIonosphere.set_station_position(pos[antenna,0],
                                                      pos[antenna,1],
                                                      pos[antenna,2])
        if(retval < 0):
            raise RuntimeError, "Error: AlbusIonosphere.set_station_position gave %d"%retval
    if(source != get_VTEC_for_AIPS_data.last_source):
        if(source == None):
            # The user must be giving us an RA dn Dec position
            assert((RA_alt != None) and (Dec_alt != None))
            get_VTEC_for_AIPS_data.last_source = -1
            source_info = [None, None, RA_alt, Dec_alt]
        else:
            # Get the source info.  If this is out of range, then
            # this raises an exception.  If it is None, raise my own
            source_info = copy.copy(source_info_array[source])
            if(source_info == None):
                raise RuntimeError, "Error: no data for source %d"%source
            get_VTEC_for_AIPS_data.last_source = source
            if(RA_alt != None): source_info[2] = RA_alt
            if(Dec_alt != None): source_info[3] = Dec_alt
        #print 'setting up for source', source, source_info
        retval = AlbusIonosphere.set_source_position(source_info[2],
                                                     source_info[3])
        if(retval < 0):
            raise RuntimeError, "Error: AlbusIonosphere.set_source_position gave %d"%retval
    # Get the proper VTEC_factor
    retval, El, Az, VTEC_factor = \
        AlbusIonosphere.get_source_AzElVTEC(calc_time * SECONDS_PER_DAY,
                                            height_ionosphere)
    if(retval < 0):
        raise RuntimeError, "Error: AlbusIonosphere.get_source_AzElVTEC gave %d"%retval
    return VTEC_factor, El, Az
get_VTEC_for_AIPS_data.last_subarray = -1
get_VTEC_for_AIPS_data.last_antenna  = -1
get_VTEC_for_AIPS_data.last_source   = -1








################################################################################
def fudge_CL_ionosphere_by_VTEC(aips_data,
                                CL_original,
                                CL_new,
                                RA_new,
                                Dec_new,
                                RA_orig = None,
                                Dec_orig = None,
                                height_ionosphere = 300E3,
                                overwrite=0):
    """Fudge an ionospheric model in a CL table to account for a new target pos

Suppose that you have a CL table with Ionospheric information in the dispersive
delay (and the time derivative) and possibly the Faraday rotation value.  This
ionospheric information was calculated or measured for the direction given
by the position of the phase center in the SU table.  But you want to fudge
this prediction for looking at a different direction in the sky, using a simple
vertical TEC correction.  This function will take in a CL table, and fudge
the DISP, DDISP, and I_FAR_ROT values by the relative VTEC correction factors.
This is done for the new source position RA and Dec.

aips_data      I  an AIPS uv dataset (standard, not Wizardry)
CL_original    I  The CL table to use for input.  Must exist.  If 0, will
                  use the highest numbered version.
CL_new         I  The CL table version to write out.  If 0, uses the highest
                  numbered CL table +1
RA_new         I  The right ascension coordinate, in RADIANS, of the new target
Dec_new        I  The declination, RADIANS, of the new target
RA_orig        I  The right ascension coordinate, in RADIANS, of the previous
                  ionospheric prediction.  If None, use the SU table position.
Dec_orig       I  The declination, RADIANS, of the of the previous
                  ionospheric prediction.  If None, use the SU table position.
height_ionosphere I  The height of the ionosphere to get the VTEC scaling
                     factor
overwrite      I  flag to indicate whether the new CL table may be
                  overwritten if a table already exists there
    """
    assert(CL_original >= 0)
    assert(CL_new >= 0)
    assert(jma_aips.check_table_version_valid(aips_data, 'AIPS CL', CL_original))
    
    # Typically, if gainuse is 0 then get the *next* highest
    if(CL_original == 0):
        CL_original = aips_data.table_highver('AIPS CL')
    if(CL_new == 0):
        CL_new = aips_data.table_highver('AIPS CL') + 1
    # Next, make sure the new CL table is free
    verify_table_version_free(aips_data, 'AIPS CL', CL_new, overwrite)
    # Get a Wizardy object
    W_dataset = Wizardry.AIPSData.AIPSUVData(aips_data.name,
                                             aips_data.klass, 
                                             aips_data.disk,
                                             aips_data.seq)
    # Get the old table and the new one
    CL_table_in = W_dataset.table('CL', CL_original)
    CL_table_out = W_dataset.attach_table(
        'CL', CL_new,
        no_term=CL_table_in.keywords['NO_TERM'])
    # How big is the CL table?
    Num_Total_CL_Rows = len(CL_table_in)
    #no_if=CL_table_in.keywords['NO_IF']  This is automatically copied
    #no_pol=CL_table_in.keywords['NO_POL']  This is automatically copied
    CL_table_out.keywords['REVISION'] = CL_table_in.keywords['REVISION']
    CL_table_out.keywords['NO_ANT'] = CL_table_in.keywords['NO_ANT']
    CL_table_out.keywords['MGMOD'] = CL_table_in.keywords['MGMOD']
    # how many polarizations are there? If 2, then set a flag
    two_pol = 0
    if(CL_table_in.keywords['NO_POL'] > 1): two_pol = 1
    # Get information about the original source positions.
    subarray_info,antenna_info,source_info = \
          get_subarray_antenna_source_info(aips_data)
    # Initialize the subroutine
    get_VTEC_for_AIPS_data.last_subarray = -1
    get_VTEC_for_AIPS_data.last_antenna  = -1
    get_VTEC_for_AIPS_data.last_source   = -1
    # Now loop over all of the rows in the old table to update and
    # place into the new one.  Keep track of how far along I am, so that
    # I can print out some progress messages
    start_time = systime.clock()
    count = -1
    for row in CL_table_in:
        # For testing, only do first few
        count = count + 1
#         if(count > 10):
#             warnings.warn("Ending CL table loop eary for debugging")
#             break
        if( (count&0x3FF) == 0 ):
            print "%5.1f%% completed in %10.1f s"%\
                  ((float(count)*100.0/Num_Total_CL_Rows),
                   systime.clock()-start_time)
        # Need information about this row
        subarray = row.subarray
        antenna = row.antenna_no
        source = row.source_id
        time = row.time
        VTEC_factor_old, El_old, Az_old = \
                         get_VTEC_for_AIPS_data(time,
                                                subarray_info,subarray,
                                                antenna_info, antenna,
                                                source_info, source,
                                                height_ionosphere,
                                                RA_orig, Dec_orig)
        VTEC_factor_new, El_new, Az_new = \
                         get_VTEC_for_AIPS_data(time,
                                                subarray_info,subarray,
                                                antenna_info, antenna,
                                                source_info, None,
                                                height_ionosphere,
                                                RA_new, Dec_new)
        factor = 1.0
        if((El_old > 0.000001)and(El_new> 0.000001)):
            factor = VTEC_factor_old / VTEC_factor_new
        row.i_far_rot = row.i_far_rot * factor
        row.disp_1 = row.disp_1 * factor
        row.ddisp_1 = row.ddisp_1 * factor
        if(two_pol):
            row.disp_2 = row.disp_2 * factor
            row.ddisp_2 = row.ddisp_2 * factor
        #print 'Final Values', disp, rm, STEC, SRM
        #print row
        CL_table_out.append(row)
    # Close the file to make sure the rows get written.
    CL_table_out.close()
    return





















################################################################################
def update_CL_ionosphere_for_change(aips_data,
                                    CL_original,
                                    CL_new,
                                    CL_old_iono,
                                    CL_new_iono,
                                    overwrite=0):
    """Update a CL table for changes of ionospheric values

Suppose that you have done a lot of calibration, and now have some CL table
which works for you, for some direction.  You now want to cahnge the CL table,
but just the ionospheric portions, for possible changes.  This subroutine will
subtract off the ionospheric calibration supplied in CL_old_iono and add back
in the value in CL_new_iono, so that for ionospheric parameters i_far_rot,
disp, and ddisp,

CL_new = CL_original - CL_old_iono + CL_new_iono

Note that it is perfectly possible that CL_original == CL_old_iono, in which case
this will simply replace the ionospheric values with those in CL_new.  But the
other, non-ionospheric, values will remain unchanged, so the clock delays and
rates from FRINGing will not be changed, which is hopefully what you want.

aips_data      I  an AIPS uv dataset (standard, not Wizardry)
CL_original    I  The CL table to use for input.  Must exist.  If 0, will
                  use the highest numbered version.
CL_new         I  The CL table version to write out.  If 0, uses the highest
                  numbered CL table +1
CL_old_iono    I  The CL table version which contains the old ionospheric values.
                  If 0, will use the highest numbered version.
CL_new_iono    I  The CL table version which contains the new ionospheric values.
                  If 0, will use the highest numbered version.
overwrite      I  flag to indicate whether the new CL table may be
                  overwritten if a table already exists there
    """
    assert(CL_original >= 0)
    assert(CL_new >= 0)
    assert(CL_old_iono >= 0)
    assert(CL_new_iono >= 0)
    assert(jma_aips.check_table_version_valid(aips_data, 'AIPS CL', CL_original))
    assert(jma_aips.check_table_version_valid(aips_data, 'AIPS CL', CL_old_iono))
    assert(jma_aips.check_table_version_valid(aips_data, 'AIPS CL', CL_new_iono))
    
    # Typically, if gainuse is 0 then get the *next* highest
    if(CL_original == 0):
        CL_original = aips_data.table_highver('AIPS CL')
    if(CL_old_iono == 0):
        CL_old_iono = aips_data.table_highver('AIPS CL')
    if(CL_new_iono == 0):
        CL_new_iono = aips_data.table_highver('AIPS CL')
    if(CL_new == 0):
        CL_new = aips_data.table_highver('AIPS CL') + 1
    # Next, make sure the new CL table is free
    verify_table_version_free(aips_data, 'AIPS CL', CL_new, overwrite)
    # Get a Wizardy object
    W_dataset = Wizardry.AIPSData.AIPSUVData(aips_data.name,
                                             aips_data.klass, 
                                             aips_data.disk,
                                             aips_data.seq)
    # Get the old table and the new one
    CL_table_in = W_dataset.table('CL', CL_original)
    CL_table_out = W_dataset.attach_table(
        'CL', CL_new,
        no_term=CL_table_in.keywords['NO_TERM'])
    # Get the ionospheric tables
    CL_table_iono_old = W_dataset.table('CL', CL_old_iono)
    CL_table_iono_new = W_dataset.table('CL', CL_new_iono)
    # How big is the CL table?
    Num_Total_CL_Rows = len(CL_table_in)
    #no_if=CL_table_in.keywords['NO_IF']  This is automatically copied
    #no_pol=CL_table_in.keywords['NO_POL']  This is automatically copied
    CL_table_out.keywords['REVISION'] = CL_table_in.keywords['REVISION']
    CL_table_out.keywords['NO_ANT'] = CL_table_in.keywords['NO_ANT']
    CL_table_out.keywords['MGMOD'] = CL_table_in.keywords['MGMOD']
    # how many polarizations are there? If 2, then set a flag
    two_pol = 0
    if(CL_table_in.keywords['NO_POL'] > 1): two_pol = 1
    # Now loop over all of the rows in the old table to update and
    # place into the new one.  Keep track of how far along I am, so that
    # I can print out some progress messages.  Note that I am iterating over
    # three things which should all be the same length, so zip(A,B,C) seems
    # better here than map(None,A,B,C)
    start_time = systime.clock()
    for i in xrange(Num_Total_CL_Rows):
        # For testing, only do first few
#        if(i > 10):
#            warnings.warn("Ending CL table loop eary for debugging")
#            break
        if( (i&0x3FF) == 0 ):
            print "%5.1f%% completed in %10.1f s"%\
                  ((float(i)*100.0/Num_Total_CL_Rows),
                   systime.clock()-start_time)
        row = CL_table_in[i]
        row_old = CL_table_iono_old[i]
        row_new = CL_table_iono_new[i]
        row.i_far_rot = row.i_far_rot - row_old.i_far_rot + row_new.i_far_rot
        row.disp_1 = row.disp_1 - row_old.disp_1 + row_new.disp_1
        row.ddisp_1 = row.ddisp_1 - row_old.ddisp_1 + row_new.ddisp_1
        if(two_pol):
            row.disp_2 = row.disp_2 - row_old.disp_2 + row_new.disp_2
            row.ddisp_2 = row.ddisp_2 - row_old.ddisp_2 + row_new.ddisp_2
        #print row
        CL_table_out.append(row)
    # Close the file to make sure the rows get written.
    CL_table_out.close()
    return




























################################################################################
def run_TECOR_on_fudged_position(aips_data,
                                 IONEX_directory,
                                 IONEX_base_name,
                                 RA,
                                 Dec,
                                 CL_in = None,
                                 CL_out = None,
                                 overwrite=0
                                 ):
    """Use TECOR to update a CL table for the ionosphere for a fudged postion.

TECOR produces dispersive delays (and Faraday Rotation Values) for the source
positions stored in the SU table.  However, for wide-field work, you may need
to calculate the ionospheric path delay for alternate lines of sight, as the
ionospheric corrections can amount to many turns of phase difference far away
from the phase center, especially for low frequencies.

This routine will copy necessary tables to a temporary work file, (it
currently seems that this means running TASAV) and then fudge the SU
table positions for the specified RA and Dec.  It will then copy over
the input CL table file, and run TECOR using the fudged sky
coordinates.  This makes a new CL table.  Finally, the new CL table is
copied back to the original dataset, and then the scratch file
deleted.

aips_data         I  Input aips dataset
IONEX_directory   I  the directory of the IONEX files.  See the documentation for
                     jma_aips.run_tecor.  You may use '.'
IONEX_base_name   I  The base name of the IONEX files.  This is the three letter
                     code which describes the group who made the IONEX file.
                     Some typical examples are 'cod', 'esa', 'jpl', etc.  See
                     the documentation for jma_aips.run_tecor.
RA                I  The right ascension of the new target position, in RADIANS
                     May be a single floating point number, or an array of
                     positions.  If it is an array, Dec and CL_out must also be
                     arrays of the same length.  If None, will alcually use
                     the specified phase center position.
Dec               I  The declination of the new target position, in RADIANS
                     May be a single floating point number, or an array of
                     positions.  If it is an array, RA and CL_out must also be
                     arrays of the same length..  If None, will alcually use
                     the specified phase center position.
CL_in             I  The number of the input CL table, may be 0
CL_out            I  The number of the output CL table, may be 0
                     May be a single integer number, or an array of
                     numbers.  If it is an array, RA and Dec must also be
                     arrays of the same length.
overwrite         I  Is the routine allowed to overwrite an old CL table?
                     0 no, else yes.
    """
    if(CL_in == None): CL_in = 0
    assert(CL_in >= 0)
    assert(jma_aips.check_table_version_valid(aips_data, 'AIPS CL', CL_in))
    # Typically, if gainuse is 0 then get the *next* highest
    if(CL_in == 0):
        CL_in = aips_data.table_highver('AIPS CL')
    if(type(RA) == type([5.5])):
        # arrays
        assert(type(Dec) == type([5.5]))
        assert(type(CL_out) == type([5]))
        assert(len(RA) == len(Dec))
        assert(len(RA) == len(CL_out))
    else:
        # make arrays out of them
        RA = [RA]
        Dec = [Dec]
        CL_out = [CL_out]
    # Make up a fake dataset
    for i in xrange(1,256):
        scratch_data = AIPSUVData(aips_data.name, 'TEFD_S', aips_data.disk,i)
        if(not scratch_data.exists()): break
    else:     
        raise RuntimeError, "Error: cannot make scratch dataset"
    print "Got scratch ", scratch_data.__str__()
    try:
        # Ok, copy over the necessary files.  It appears that I have
        # to run a full TASAV
        jma_aips.run_tasav(aips_data, scratch_data)
        for i in xrange(len(RA)):
            if(CL_out[i] == None): CL_out[i] = 0
            assert(CL_out[i] >= 0)
            if(CL_out[i] == 0):
                CL_out[i] = scratch_data.table_highver('AIPS CL') + 1
            # Next, make sure the new CL table is free
            verify_table_version_free(scratch_data, 'AIPS CL', CL_out[i],
                                      overwrite)
            # Now process the SU table.  Get the source information in memory,
            # chenge the position, then write to the scratch file
            scratch_data.zap_table('AIPS SU', 1)
            jma_aips.run_tacop(aips_data, 'SU', 1, 1, scratch_data, 1)
            sources = get_source_positions(scratch_data,1)
            for s in xrange(len(sources)):
                if(sources[s] == None): continue
                if(RA[i]): sources[s][2] = RA[i]
                if(Dec[i]): sources[s][3] = Dec[i]
            set_source_positions(scratch_data, sources, 1, 2, scratch_data)
            # Now delete SU 1, and move SU 2 to SU 1
            scratch_data.zap_table('AIPS SU', 1)
            jma_aips.run_tacop(scratch_data, 'SU', 2, 1, scratch_data, 1)
            #jma_aips.pause_prog("Check scratch file")
            # Now run TECOR
            jma_aips.run_tecor(scratch_data,
                               IONEX_directory,
                               IONEX_base_name,
                               CL_in,
                               CL_out[i])
            #jma_aips.pause_prog("TECOR finished")
            # And move the new CL table back to the original dataset
            jma_aips.run_tacop(scratch_data, 'CL', CL_out[i], 1,
                               aips_data, CL_out[i])
    finally:
        scratch_data.zap()
    return











################################################################################
def run_simple_AI_model_on_fudged_position(aips_data,
                                           processing_option,
                                           tolerance,
                                           RA,
                                           Dec,
                                           CL_in = None,
                                           CL_out = None,
                                           data_directory = ".",
                                           overwrite=0
                                           ):
    """simple function to run AI models on fudged positions

This function will perform a simple AI model run to generate a new AI table
from an ionosphere model, and from that to generate a new CL table, all
using a fudged RA and Dec position.

This uses the Albus_Iono standard processing_option string to figure out
which model stuff to be using.  See the notes at the top of this file
(assuming I get around to it) for documentation on what string does what.

aips_data         I  Input aips dataset
processing_option I  a string defining which model to use
tolerance         I  fractional uncertainty in total electron content to
                     ask for.  Note that this is the requested fractional
                     accuracy, not the achieved.  For L-band EVN
                     observations, you should probably ask for no worse
                     than 1E-4, and probably 1E-5.  For lower frequencies,
                     you want even better precision, probably 1E-6.
RA                I  The right ascension of the new target position, in RADIANS
                     May be a single floating point number, or an array of
                     positions.  If it is an array, Dec and CL_out must also be
                     arrays of the same length..  If None, will alcually use
                     the specified phase center position.
Dec               I  The declination of the new target position, in RADIANS
                     May be a single floating point number, or an array of
                     positions.  If it is an array, RA and CL_out must also be
                     arrays of the same length..  If None, will alcually use
                     the specified phase center position.
CL_in             I  The number of the input CL table, may be 0
CL_out            I  The number of the output CL table, may be 0
                     May be a single integer number, or an array of
                     numbers.  If it is an array, RA and Dec must also be
                     arrays of the same length.
data_directory    I  a directory path where datafiles necessary for
                     computations may be stored, or are already located.
overwrite         I  Is the routine allowed to overwrite an old CL table?
                     0 no, else yes.
    """
    assert(processing_option in PROCESSING_OPTION_FULL_LIST)
    if(CL_in == None): CL_in = 0
    assert(CL_in >= 0)
    assert(jma_aips.check_table_version_valid(aips_data, 'AIPS CL', CL_in))
    # Typically, if gainuse is 0 then get the *next* highest
    if(CL_in == 0):
        CL_in = aips_data.table_highver('AIPS CL')
    if(type(RA) == type([5.5])):
        # arrays
        assert(type(Dec) == type([5.5]))
        assert(type(CL_out) == type([5]))
        assert(len(RA) == len(Dec))
        assert(len(RA) == len(CL_out))
    else:
        # make arrays out of them
        RA = [RA]
        Dec = [Dec]
        CL_out = [CL_out]
    # Make up a fake dataset
    for i in xrange(1,256):
        scratch_data = AIPSUVData(aips_data.name, 'TEFD_S', aips_data.disk,i)
        if(not scratch_data.exists()): break
    else:     
        raise RuntimeError, "Error: cannot make scratch dataset"
    print "Got scratch ", scratch_data.__str__()
    try:
        # Ok, copy over the necessary files.  It appears that I have
        # to run a full TASAV
        jma_aips.run_tasav(aips_data, scratch_data)
        for i in xrange(len(RA)):
            if(CL_out[i] == None): CL_out[i] = 0
            assert(CL_out[i] >= 0)
            if(CL_out[i] == 0):
                CL_out[i] = scratch_data.table_highver('AIPS CL') + 1
            # Next, make sure the new CL table is free
            verify_table_version_free(scratch_data, 'AIPS CL', CL_out[i],
                                      overwrite)
            # Now process the SU table.  Get the source information in memory,
            # chenge the position, then write to the scratch file
            scratch_data.zap_table('AIPS SU', 1)
            jma_aips.run_tacop(aips_data, 'SU', 1, 1, scratch_data, 1)
            sources = get_source_positions(scratch_data,1)
            for s in xrange(len(sources)):
                if(sources[s] == None): continue
                if(RA[i]): sources[s][2] = RA[i]
                if(Dec[i]): sources[s][3] = Dec[i]
            set_source_positions(scratch_data, sources, 1, 2, scratch_data)
            # Now delete SU 1, and move SU 2 to SU 1
            scratch_data.zap_table('AIPS SU', 1)
            jma_aips.run_tacop(scratch_data, 'SU', 2, 1, scratch_data, 1)
            scratch_data.zap_table('AIPS SU', 2)
            #jma_aips.pause_prog("Check scratch file")
            # Now run the ionosphere stuff
            if(processing_option in PROCESSING_OPTION_IONEX_GROUPS):
                # calling IONEX stuff, so transfer to that function
                jma_aips.run_tecor(scratch_data, data_directory,
                                   processing_option[3:].lower(),
                                   CL_in, CL_out[i])
            elif( (processing_option in PROCESSING_OPTION_MODEL_GROUPS)
                or (processing_option in PROCESSING_OPTION_RINEX_GROUPS) ):
                ionosphere_simple_model(scratch_data, processing_option,
                                        tolerance, CL_in,
                                        CL_out[i], overwrite)
                # Copy over the new AI table back to the original dataset
                jma_aips.run_tacop(scratch_data, 'AI', 0, 1, aips_data, 0)
            else:
                raise KeyError, "Unknown processing_option '%s'"%processing_option
            # And move the new CL table back to the original dataset
            jma_aips.run_tacop(scratch_data, 'CL', CL_out[i], 1,
                               aips_data, CL_out[i])
    finally:
        scratch_data.zap()
    return
