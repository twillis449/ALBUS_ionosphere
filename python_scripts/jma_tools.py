# jma_aips.py
# My standard AIPS utilities
# 2006 May 19  James M Anderson  --JIVE  start
# 2007 Jul 25  JMA  --many enhancements before now.  Today, allow
#                     self-calibration to just use point-source calib




################################################################################
# some import commands  The User should not need to change this.
################################################################################


import copy, optparse, os, sys, socket, tempfile
import re, string
import inspect
import warnings
import math








################################################################################
# Global variables
SECONDS_PER_DAY = 24.0*60.0*60.0
DAYS_PER_SECOND = 1.0/SECONDS_PER_DAY
M_DEG2RAD = math.pi/180.0
M_RAD2DEG = 180.0/math.pi





################################################################################
# Define some useful functions
################################################################################




################################################################################
# 6&*!%#@^ Python doesn't have the equivalent of __LINE__
def current_line():
    return inspect.getouterframes(inspect.currentframe())[1][2]



################################################################################
# Wait for the user to say that it is ok to move on
def pause_prog(text):
    print('#' * 78)
    junk = evalinput(text)
    return junk



################################################################################
def make_sure_directory_exists(directory):
    """check for the existence of a directory, and make it if not there
    """
    if(os.exists(directory)):
        return
    os.makedirs(directory)
    return





################################################################################
# dump a dictionary to a file in text mode
def print_dict_to_file(d, middle_name, overwrite=1, copy_to_stdout=0,
                       experiment="dict",dirname='.'):
    """prints a dict to a file based on a middle_name"""
    # generate the full file name
    filename = dirname + '/' + experiment + '.' + middle_name + '.dict'
    if(os.path.isfile(filename)):
        if(overwrite):
            warnings.warn("File '%s' already exists.  Deleting."%filename)
            os.remove(filename)
    try:
        fp = open(filename, "w")
        dk = list(d.keys())
        dk.sort()
#       for k in dk:
#           print("%30s"%k, d[k], file=fp)
#           if(copy_to_stdout): print("%30s"%k, d[k])
    finally:
        fp.close()


################################################################################
def get_MJD(year, month, day):
    """get the Modified Julian Date (JD - 2400000.5)

Note that this really only works for Gregorian dates.

This assumes that you give it a correct year, month, day.  It is
your own fault if you are out of range.

Taken from _Astronomical Algorithms_, Meeus, 1991
    """
    mm = month
    yy = year
    if(mm<= 2):
        mm += 12
        yy -= 1
    a = int(yy/100)
    b = 2 - a + int(a/4)
    MJD = int(365.25*(yy+4716)) + int(30.6001*(mm+1)) + day + b - 1524.5
    MJD -= 2400000.5
    return MJD

################################################################################
def get_MJD_from_JD(JD):
    MJD = JD - 2400000.5
    return MJD

################################################################################
def get_JD_from_MJD(MJD):
    JD = MJD + 2400000.5
    return JD



################################################################################
def get_MJD_hms(year, month, day, hour = 0, minute = 0, second = 0):
    """get the Modified Julian Date (JD - 2400000.5)
    """
    day_frac = ((hour) * 60.0 + minute) * 60.0 + second
    day_frac *= DAYS_PER_SECOND
    MJD = day_frac + get_MJD(year, month, day)
    return MJD
    
################################################################################
def get_JD_hms(year, month, day, hour = 0, minute = 0, second = 0):
    """get the Julian Date
    """
    MJD = get_MJD_hms(year, month, day, hour, minute, second)
    JD = MJD + 2400000.5
    return JD

################################################################################
def get_MJD_frac(year, month, day, day_fraction = 0):
    """get the Modified Julian Date (JD - 2400000.5)
    """
    MJD = day_fraction + get_MJD_hms(year, month, day)
    return MJD
    
################################################################################
def get_JD_frac(year, month, day, day_fraction = 0):
    """get the Julian Date
    """
    MJD = get_MJD_frac(year, month, day, day_fraction)
    JD = MJD + 2400000.5
    return JD


################################################################################
def get_ymdf_from_JD(JD):
    """get the year, month, day, day_fraction from an MJD

Taken from _Astronomical Algorithms_, Meeus, 1991
    """
    JD2 = JD + 0.5
    Z = int(JD2)
    F = JD2 - Z
    A = Z
    if(Z>= 2299161):
        alpha = int((Z-1867216.25)/36524.25)
        A = Z + 1 + alpha - int(alpha/4)
    B = A + 1524
    C = int((B-122.1)/365.25)
    D = int(365.25*C)
    E = int((B-D)/30.6001)
    day_total = B - D - int(30.6001*E) + F
    month = E - 1
    if(E >= 14): month = E - 13
    year = C - 4716
    if(month <= 2): year += 1
    day = int(day_total)
    day_fraction = day_total - day
    return year, month, day, day_fraction


################################################################################
def get_hms_from_frac(day_fraction):
    """get hours, minues, seconds from a fractional day.

Does not worry about leap seconds.
"""
    h = day_fraction * 24.0
    hour = int(h+2E-13)
    m = (h - hour) * 60.0
    minute = int(m+1E-11)
    second = (m - minute) * 60.0
    return hour, minute, second


################################################################################
def get_ymdh_from_JD(JD):
    """get hours, minues, seconds from a fractional day.
"""
    year, month, day, day_fraction = get_ymdf_from_JD(JD)
    hour, minute, second = get_hms_from_frac(day_fraction)
    return year, month, day, hour, minute, second




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


