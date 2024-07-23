# GPS_stled in read of GPS data from file
# repository for GPS station information
# 2006 Jul 27  James M Anderson  --JIVE  start

# IGS keeps a list of GPS stations at
# ftp://igs.org/pub/station/general/igs_with_former.snx
# This is one for the BKG sites
# http://igs.ifag.de/root_ftp/GREF/products/1378/bkg13787.snx.Z
# Note that one should be available for each GPS week.


import os, os.path
import math
#from string import split, strip
import Albus_Coordinates
import warnings
import time
import tempfile
import copy
import socket
if(socket.getdefaulttimeout == None):
    socket.setdefaulttimeout(120.0)
import Albus_RINEX_2
import Albus_RINEX
import string



################################################################################
# Global variables
SECONDS_PER_DAY = 24.0*60.0*60.0
DAYS_PER_SECOND = 1.0/SECONDS_PER_DAY
M_DEG2RAD = math.pi/180.0
M_RAD2DEG = 180.0/math.pi


# See JMA Notes from 2006 Jan 10 and some of BOb Campbell's notes
SPEED_OF_LIGHT = 299792458.0    # m s^{-1}
IONOSPHERE_Kp_2 = 80.6163848    # m^3 s^{-2}
IONOSPHERE_KB = 2.79924925E10   # s^{-1} T^{-1}



################################################################################
# need to keep a list of GPS stations
GPS_stations = {}
################################################################################
# need to keep a list of GPS observations which are not available
GPS_observation_missing = {}
################################################################################
# need to keep a list of global GPS stations which are used for frame reference

GPS_global_station_list = ['abmf', 'abpo', 'ac23', 'ac24', 'acrg', 'acso', 'adis', 'aggo', 'aira', 'ajac', 'albh', 'algo', 'alic', 'alrt', 'amc4', 'ank2', 'anmg', 'antc', 'antf', 'areg', 'areq', 'arht', 'artu', 'aruc', 'ascg', 'aspa', 'atru', 'auck', 'badg', 'baie', 'bake', 'bako', 'bamf', 'barh', 'baut', 'bele', 'bhr3', 'bhr4', 'bik0', 'bill', 'bjco', 'bjfs', 'bjnm', 'blyt', 'bnoa', 'boav', 'bogi', 'bogt', 'bor1', 'braz', 'brew', 'brft', 'brmg', 'brst', 'brun', 'brux', 'bshm', 'btng', 'bucu', 'bzr2', 'cags', 'cas1', 'ccj2', 'cebr', 'cedu', 'cggn', 'chan', 'chil', 'chof', 'chpg', 'chpi', 'chti', 'chum', 'chur', 'chwk', 'cibg', 'cit1', 'ckis', 'cksv', 'cmp9', 'cmum', 'cnmr', 'coco', 'cord', 'coso', 'cote', 'coyq', 'cpnm', 'cpvg', 'crfp', 'cro1', 'cuib', 'cusv', 'cut0', 'cuut', 'cyne', 'cztg', 'dae2', 'daej', 'dakr', 'darw', 'dav1', 'dear', 'dgar', 'dhlg', 'djig', 'dlf1', 'dltv', 'drag', 'drao', 'dubo', 'dumg', 'dund', 'dyng', 'ebre', 'eil3', 'eil4', 'enao', 'eprt', 'escu', 'faa1', 'fair', 'falk', 'ffmj', 'flin', 'flrs', 'frdn', 'ftna', 'func', 'gamb', 'gamg', 'ganp', 'gcgo', 'geno', 'glps', 'glsv', 'gode', 'godn', 'gods', 'godz', 'gol2', 'gold', 'gop6', 'gop7', 'gope', 'grac', 'gras', 'graz', 'guam', 'guat', 'guug', 'hal1', 'hamd', 'harb', 'hers', 'hert', 'hksl', 'hkws', 'hlfx', 'hnlc', 'hnpt', 'hnus', 'hob2', 'hofn', 'holb', 'holm', 'holp', 'hrag', 'hrao', 'hueg', 'hyde', 'ieng', 'iisc', 'iitk', 'ineg', 'invk', 'iqal', 'iqqe', 'irkj', 'irkm', 'isba', 'ishi', 'ispa', 'ista', 'izmi', 'jctw', 'jdpr', 'jfng', 'jnav', 'jog2', 'joz2', 'joze', 'jplm', 'jpre', 'karr', 'kat1', 'kerg', 'khar', 'kir0', 'kir8', 'kiri', 'kiru', 'kit3', 'kitg', 'kmnm', 'kokb', 'kokv', 'kos1', 'kost', 'kouc', 'koug', 'kour', 'krgg', 'krs1', 'ksu1', 'kuj2', 'kzn2', 'lae1', 'lama', 'laut', 'lbch', 'lck3', 'lck4', 'leij', 'lhaz', 'licc', 'llag', 'lmmf', 'lpal', 'lpgs', 'lroc', 'm0se', 'mac1', 'mad2', 'madr', 'mag0', 'maju', 'mal2', 'mana', 'mar6', 'mar7', 'mars', 'mas1', 'mat1', 'mate', 'matg', 'maui', 'maw1', 'mayg', 'mbar', 'mchl', 'mcil', 'mcm4', 'mdo1', 'mdvj', 'medi', 'meli', 'mers', 'met3', 'metg', 'mets', 'mfkg', 'mgue', 'mikl', 'mizu', 'mkea', 'mobj', 'mobk', 'mobn', 'mobs', 'moiu', 'monp', 'morp', 'mqzg', 'mrc1', 'mrl1', 'mrl2', 'mro1', 'mssa', 'mtka', 'mtv1', 'mtv2', 'nabg', 'nain', 'nano', 'naur', 'naus', 'ncku', 'nico', 'nist', 'nium', 'nklg', 'nlib', 'nnor', 'not1', 'novm', 'nrc1', 'nril', 'nrmd', 'ntus', 'nvsk', 'nya1', 'nya2', 'nyal', 'oak1', 'oak2', 'obe4', 'ohi2', 'ohi3', 'ons1', 'onsa', 'op71', 'opmt', 'orid', 'osn3', 'osn4', 'ous2', 'owmg', 'p043', 'p051', 'p053', 'p389', 'p779', 'p802', 'pado', 'palm', 'parc', 'park', 'pbr4', 'pdel', 'penc', 'pert', 'pets', 'pgen', 'picl', 'pie1', 'pimo', 'pin1', 'pngm', 'poal', 'pohn', 'pol2', 'polv', 'pots', 'pove', 'pppc', 'prds', 'pre3', 'pre4', 'ptag', 'ptbb', 'ptgg', 'ptvl', 'qaq1', 'qiki', 'qui3', 'qui4', 'quin', 'rabt', 'raeg', 'ramo', 'rbay', 'rdsd', 'redu', 'reso', 'reun', 'reyk', 'rgdg', 'riga', 'rio2', 'riop', 'roag', 'rock', 'roth', 'salu', 'samo', 'sant', 'sask', 'savo', 'sbok', 'sch2', 'scip', 'scor', 'scrz', 'sctb', 'scub', 'sejn', 'seme', 'sey2', 'seyg', 'sfdm', 'sfer', 'sgoc', 'sgpo', 'shao', 'she2', 'shlg', 'sin1', 'smst', 'sni1', 'sod3', 'sofi', 'solo', 'spk1', 'spt0', 'sptu', 'ssia', 'stfu', 'sthl', 'stj3', 'stjo', 'stk2', 'stpm', 'str1', 'str2', 'sulp', 'suth', 'sutm', 'suwn', 'svtl', 'sydn', 'syog', 'tabl', 'tana', 'tash', 'tcms', 'tdou', 'tehn', 'thtg', 'thti', 'thu2', 'tid1', 'tidb', 'tit2', 'tixi', 'tlse', 'tlsg', 'tnml', 'tong', 'topl', 'torp', 'tow2', 'trak', 'tro1', 'tsk2', 'tskb', 'tubi', 'tuva', 'twtf', 'ucal', 'uclp', 'uclu', 'ufpr', 'ulab', 'uldi', 'unb3', 'unbd', 'unbj', 'unbn', 'unsa', 'ural', 'urum', 'uscl', 'usn7', 'usn8', 'usn9', 'usp1', 'usud', 'utqi', 'uzhl', 'vacs', 'vald', 'vill', 'vis0', 'vndp', 'voim', 'wab2', 'wark', 'warn', 'wdc5', 'wdc6', 'wes2', 'wgtn', 'whc1', 'whit', 'widc', 'will', 'wind', 'wlsn', 'wroc', 'wsrt', 'wtz3', 'wtza', 'wtzr', 'wtzs', 'wtzz', 'wuh2', 'wuhn', 'wuth', 'xmis', 'yakt', 'yar2', 'yar3', 'yarr', 'yebe', 'yel2', 'yell', 'yibl', 'ykro', 'yons', 'yssk', 'zamb', 'zeck', 'zim2', 'zim3', 'zimm']


# Now, I need to keep some coordinate conversion things
# Here are some things we should need.  The algorithm and
# data here are from the _Explanatory Supplement to the Astronomical
# Almanac_, see section 4.22, and section 4.25

# Ellipsoid types
GRS80 = 0
WGS84 = 1

# Ellipsoid data
a = [
    6378137.0, # m GRS80
    6378137.0  # m WGS84
    ]
f = [
    3.352810681182318790E-03, # 1.0 / 298.257222101, # GRS80
    3.352810664747480490E-03  # 1.0 / 298.257223563  # WGS84
    ]
b_absolute = [
    # a*(1.0-f);
    6.356752314140356146E+06, # GRS80
    6.356752314245179296E+06  # WGS84
    ]
e2 = [
    # \sqrt{2f-f^2}
    8.181919104281579203E-02, # GRS80
    8.181919084262148645E-02  # WGS84
    ]



################################################################################
def ellipsoidal_coord(x, y, z, type):
    """convert Cartesian to elliptical of type

INPUTS:
x      I  x coordinate, in m
y      I  y coordinate, in m
z      I  z coordinate, in m
type   I  ellipsoid type.

OUTPUTS: longitude, latitude, height
longitude  O  longitude, in radians
latitude   O  latitude, in radians
height     O  above ellipsoid, in m


Here are some things we should need.  The algorithm and
data here are from the _Explanatory Supplement to the Astronomical
Almanac_, see section 4.22, and section 4.25


"""
    assert((type >= 0) and (type < len(a)))
    r = math.sqrt(x*x+y*y)
    if(r < 0.001):
        # we are within 1 mm of the pole, so use the simple formulas
        lambda_ = 0.0
        phi = math.pi * 0.5
        if(z < 0.0):
           phi = -phi
        h = fabs(z) - b_absolute[type]
        return lambda_,phi,h
    if(math.fabs(z) < 0.001):
        # we are within 1 mm of the equator, so use simple stuff
        h = r-a[type]
        phi = 0.0
    else:
        # we are in the general area of the planet
        # Ok, need a b with the same sign as z
        if(z >= 0.0):
            b = b_absolute[type]
        else:
            b = -b_absolute[type]
        E = (b*z-(a[type]*a[type]-b*b))/(a[type]*r)
        F = (b*z+(a[type]*a[type]-b*b))/(a[type]*r)
        P = 4.0/3.0*(E*F+1.0)
        Q = 2.0*(E*E-F*F)
        D = P*P*P+Q*Q
        Dsqrt = math.sqrt(D)
        nu = math.pow(Dsqrt-Q,1.0/3.0) - math.pow(Dsqrt+Q,1.0/3.0)
        G = 0.5*(math.sqrt(E*E+nu)+E)
        t = math.sqrt(G*G+(F-nu*G)/(2.0*G-E))-G
        phi = math.atan(a[type]*(1.0-t*t)/(2.0*b*t))
        h = (r-a[type]*t)*math.cos(phi)+(z-b)*math.sin(phi)
    # compute the longitude
    lambda_=math.atan2(y,x)
    return lambda_,phi,h



################################################################################
def cartesian_coord(longitude, latitude, height, type):
    """convert elliptical of type to Cartesian

INPUTS:
longitude  I  longitude, in radians
latitude   I  latitude, in radians
height     I  above ellipsoid, in m
type   I  ellipsoid type.

OUTPUTS: x,y,z
x      O  x coordinate, in m
y      O  y coordinate, in m
z      O  z coordinate, in m


2006 Jul 27  James M Anderson  --try a different set of equations, from
                                 National Geodetic Survey, XyzWin
                                 See http://www.ngs.noaa.gov/PC_PROD/XYZWIN/


"""
    assert((type >= 0) and (type < len(a)))
    slat = math.sin(latitude)
    clat = math.cos(latitude)
    slon = math.sin(longitude)
    clon = math.cos(longitude)
    g0 = 1.0 - f[type]
    g1 = a[type] / math.sqrt(1.0 - e2[type]*e2[type]*slat*slat)
    x = (g1 + height) *clat*clon
    y = (g1 + height) *clat*slon
    z = (g1 * g0 * g0 + height) * slat
    return x,y,z


##############################################################################
def get_global_station_list():

  temp_list = copy.deepcopy(GPS_global_station_list)
  temp_list.reverse()
  return temp_list


################################################################################
def fill_GPS_station_dict(GPS, filename):
    """add GPS station position entries to a GPS dictionary

Note: the lon,lat,height accuracy in the standard file is not great,
and is probably no better than 5 meters in many cases.  You
probably will get better accuracy from the RINEX obs files.

GPS         I  a dictionary of GPS stations, of the type
               GPS['cccc'] = [x,y,z],
               where 'cccc' is the 4-letter station code (lowercase)
               and [x,y,z] is an array of the Cartesian x,y,z position values
               in m
filename    I  The name of an SNX file to open and read to find new station
               information.  Typically, such a file should come from
               ftp://igs.org/pub/station/general/igs_with_former.snx
               or somewhere similar


OUTPUTS: GPS
GPS         O  The filled dictionary
"""
    print('input file is ', filename)
    # dunnon what encoding was used on this file but it is not UTF 8.
    # strip whatever is not ASCII further down
    fp = open(filename, "rb") 
    try:
        site_id_found = 0
        for line in fp:
            line = ''.join(map(chr, map(lambda x: x if x < 127 else ord(' '), line)))
            if line[0:1] == '#':   # a comment
              continue
            if(site_id_found == 0):
                if(line[0:8] == "+SITE/ID"):
                    site_id_found = 1
            else:
                if(line[0:8] == "-SITE/ID"):
                    print('reached end of input stations file')
                    fp.close()
                    return GPS
                else:
                  search_key = line[1:5].lower()
                  country = line[15:18]
                  name = search_key + country
                  add_data = True
                  if add_data:
                    for i in GPS.keys():
                      if i[0:4] == search_key:
                         add_data = False
                  if add_data: 
                    longitude = Albus_Coordinates.deg_str_to_rad(line[44:56])
                    latitude  = Albus_Coordinates.deg_str_to_rad(line[57:69])
                    height    = float(line[70:78])
                    x,y,z = cartesian_coord(longitude, latitude, height, WGS84)
                    GPS[name] = [x,y,z]
    except Exception as e:
      print('read failure at about line', counter)
      raise e
    return GPS


################################################################################
def fill_GPS_station_dict_new(GPS, filename):
    """add GPS station position entries to a GPS dictionary

Note: the lon,lat,height accuracy in the standard file is not great,
and is probably no better than 5 meters in many cases.  You
probably will get better accuracy from the RINEX obs files.

GPS         I  a dictionary of GPS stations, of the type
               GPS['cccc'] = [x,y,z],
               where 'cccc' is the 4-letter station code (lowercase)
               and [x,y,z] is an array of the Cartesian x,y,z position values
               in m
filename    I  The name of an SNX file to open and read to find new station
               information.  Typically, such a file should come from
               ftp://igs.org/pub/station/general/igs_with_former.snx
               or somewhere similar


OUTPUTS: GPS
GPS         O  The filled dictionary
"""
    fp = open(filename, "r")
    try:
        site_id_found = 0
        for line in fp:
            if(site_id_found == 0):
                if(line[0:8] == "+SITE/ID"):
                    site_id_found = 1
            else:
                if(line[0:8] == "-SITE/ID"):
                    break
                name = line[1:5].lower()
                if name in GPS:  
                    continue
                longitude = Albus_Coordinates.deg_str_to_rad(line[44:55])
                latitude  = Albus_Coordinates.deg_str_to_rad(line[56:67])
                height    = float(line[67:75])
                x,y,z = cartesian_coord(longitude, latitude, height, WGS84)
                GPS[name] = [x,y,z]
    finally:
        fp.close()
    return GPS



################################################################################
def _write_GPS_station_list(GPS_dict, filename):
    """write a GPS station list to a file"""
    try:
        fp = open(filename,"w")
        try:
            keys = list(GPS_dict.keys())
            keys.sort()
            for k in keys:
                XYZ = GPS_dict[k]
                line = "%4.4s %.5f %.5f %.5f\n"%(k,XYZ[0],XYZ[1],XYZ[2])
                fp.write(line)
        finally:
            fp.close()
    except:
        pass
    return


################################################################################
def _read_GPS_station_list(GPS_dict, filename):
    """read a lit of GPS stations and positions from a text file"""
    if(GPS_dict is None):
        GPS_dict = {}
    try:
        fp = open(filename, "r")
        try:
            for line in fp:
                l = line.split()
                k = l[0]
                XYZ = [float(l[1]),float(l[2]),float(l[3])]
                GPS_dict[k] = XYZ
        finally:
            fp.close()
    except:
        pass
    return GPS_dict





################################################################################
def _write_GPS_observation_missing_list(GPS_missing, missing_filename):
    """write a list of missing GPS observations to a file"""
    try:
        fp = open(missing_filename,"w")
        try:
            keys = list(GPS_missing.keys())
            keys.sort()
            for k in keys:
                t = GPS_missing[k]
                line = "%s %.5f\n"%(k,t)
                fp.write(line)
        finally:
            fp.close()
    except:
        pass
    return


################################################################################
def _append_GPS_observation_missing_list(obs_filename, obs_time, missing_filename):
    """append a missing GPS observation to a file"""
    try:
        fp = open(missing_filename,"a")
        try:
            line = "%s %.0f\n"%(obs_filename,obs_time)
            fp.write(line)
        finally:
            fp.close()
    except:
        pass
    return


################################################################################
def _read_GPS_observation_missing_list(missing_filename):
    """read a lit of GPS stations and positions from a text file"""
    GPS_dict = {}
    now = time.time()
    delay = 86400.0 * 180
    drop = 0
    try:
        fp = open(missing_filename, "r")
        try:
            for line in fp:
                l = line.split()
                t = float(l[1])
                if(now - t > delay):
                    drop = 1
                    continue  # skip if time older than about 180 days
                GPS_dict[l[0]] = t
        finally:
            fp.close()
    except:
        pass
    return GPS_dict, drop



                    






################################################################################
def fill_standard_stations():
    """get the standard station list stuff"""
    GPS_dict = {}
    # If the user has a position file in the current directory, use that file
    # and only that file
    test_file = "./gps_pos_default.snx"
    if(os.path.isfile(test_file)):
        warnings.warn("Using GPS receiver positions only from user file '%s'"%test_file)
        try:
            print('*** filling GPS_dict')
            print('using GPS data file ', test_file)
            GPS_dict = fill_GPS_station_dict(GPS_dict, test_file)
        except:
            print('***** failed in read of GPS data from file ', test_file)
            print('***** check for invalid data (maybe non-ascii character) in file')
            raise
        return GPS_dict
    # then try 'standard'file
    test_file = os.environ['HOME'] + "/albus/libdata/JMA/gps_pos_default.snx"
    if(os.path.isfile(test_file)):
        warnings.warn("Using GPS receiver positions from file '%s'"%test_file)
        try:
            print('*** filling GPS_dict')
            print('using GPS data file ', test_file)
            GPS_dict = fill_GPS_station_dict(GPS_dict, test_file)
        except:
            print('***** failed in read of GPS data from file ', test_file)
            print('***** check for invalid data (maybe non-ascii character) in file')
            raise
        return GPS_dict
    print('we are looking for standard files')
    #Otherwise, get the standard files
    month = 86400 * 30
    try:
        now_time = time.time()   # in seconds
        # find the system list
        py_path = os.environ["PYTHONPATH"]
        sys_file = None
        for d in py_path.split(os.pathsep):
            test_path = d + "/../libdata/JMA/gps_pos_default.snx"
            if(os.path.isfile(test_path)):
                sys_file = test_path
                break
            test_path = d + "/../../libdata/JMA/gps_pos_default.snx"
            if(os.path.isfile(test_path)):
                sys_file = test_path
                break
        sys_time = 0
        if(sys_file):
            sys_time = os.path.getmtime(sys_file)
            if(now_time - sys_time > 6 * month):
                warnings.warn("System default GPS station file %s is getting old.\nContact your system administrator."%sys_file)
        else:
            warnings.warn("Cannot find default GPS station file %s.\nContact your system administrator."%sys_file)
        user_file = os.environ['HOME'] + "/.ParselTongue/GPS_station_list.txt"
        user_time = 0
        if(os.path.isfile(user_file)):
            user_time = os.path.getmtime(user_file)
        need_new = 0
        write_user = 0
        if(sys_time > 0):  # Read in the system file, if available
            if(user_time > sys_time): # Read system first, then personal file
                print('&&& filling GPS_dict')
                print('using GPS data file ', sys_file)
                GPS_dict = fill_GPS_station_dict(GPS_dict, sys_file)
                print('using GPS data file ', user_file)
                GPS_dict = _read_GPS_station_list(GPS_dict, user_file)
            else:
                write_user = 1
                print('### filling GPS_dict')
                if(user_time > 0):  
                    print('using GPS data file ', user_file)
                    GPS_dict = _read_GPS_station_list(GPS_dict, user_file)
                print('using GPS data file ', sys_file)
                GPS_dict = fill_GPS_station_dict(GPS_dict, sys_file)
        else: # Try just the user's personal file
            need_new = 1
            write_user = 1
            if(user_time > 0):  
                print('### filling GPS_dict')
                print('using GPS data file ', user_file)
                GPS_dict = _read_GPS_station_list(GPS_dict, user_file)
        if((now_time - user_time > 3 * month)
           and (now_time - sys_time > 3 * month)):
            need_new = 1
            write_user = 1
        if(need_new):
            # try getting a new file from the web
            try:
                try:
                    temp_file = tempfile.NamedTemporaryFile()
                    webfile = "ftp://igs.org/pub/station/general/igs_with_former.snx"
                    try:
                        print("Downloading %s"%webfile)
                        print('%%% filling GPS_dict')
                        print('using GPS data file ', temp_file.name)
                        GPS_dict = fill_GPS_station_dict(GPS_dict, temp_file.name)
                    except IOError:
                            warnings.warn("Could not download new GPS stations list")
                finally:
                    temp_file.close()
            except:
                raise
        if(write_user):
            _write_GPS_station_list(GPS_dict, user_file)
    except:
        raise
    return GPS_dict





GPS_stations = fill_standard_stations()







################################################################################
def fill_standard_missing():
    """get the standard observation missing list stuff"""
    GPS_dict = {}
    try:
        user_file = os.environ['HOME'] + "/.ParselTongue/GPS_missing_list.txt"
        if(os.path.isfile(user_file)):
            GPS_dict, drop = _read_GPS_observation_missing_list(user_file)
            if drop:
                # the read process droped old data, so clear the file
                _write_GPS_observation_missing_list(GPS_dict, user_file)
    except:
        pass
    return GPS_dict





GPS_observation_missing = fill_standard_missing()



################################################################################
def check_for_missing(filename, age = 864000.0, GPS_missing = None):
    """check if a desired GPS datafile is in our list of missing data

    Check if we have previously been unable to download filename, within the
    past age seconds (defaults to 10 days).  Return 1 for filename in the
    missing dict, 0 if not.
    """
    global GPS_observation_missing
    if(GPS_missing == None): GPS_missing = GPS_observation_missing
    if filename in GPS_missing:
        now = time.time()
        t = GPS_missing[filename]
        if(now - t < age):
            return 1
    return 0


################################################################################
def add_to_missing(filename, GPS_missing = None):
    """Add a filename to the dict of missing GPS data"""
    global GPS_observation_missing
    if(GPS_missing == None): GPS_missing = GPS_observation_missing
    t = time.time()
    GPS_missing[filename] = t
    try:
        user_file = os.environ['HOME'] + "/.ParselTongue/GPS_missing_list.txt"
        _append_GPS_observation_missing_list(filename, t, user_file)
    except:
        pass
    return GPS_missing

   
################################################################################
def clear_missing(GPS_missing = None):
    """Clear the GPS station missing dict

This function will clear the GPS_missing dict that is passed in.  If None, the
dict is the GPS_stations global GPS_observation_missing dict, and the
user file of times is cleared as well.

WARNING: only call this if you really want to scrap all of the missing data
information.

This function is useful if you are trying to run using very recent GPS data
which may have just been uploaded to a standard download location, and you
want to clear any previous attempts to get the data in the past couple of days.

INPUTS:
GPS_missing   I  dict of missing information to clear.  If None, use the
                 global GPS_observation_missing file and clear the user file too.

OUTPUTS: None

    """
    global GPS_observation_missing
    if(GPS_missing == None):
        GPS_missing = GPS_observation_missing
        user_file = os.environ['HOME'] + "/.ParselTongue/GPS_missing_list.txt"
        fp = open(user_file,"w")
        try:
            pass
        finally:
            fp.close()
    GPS_missing.clear()
    return GPS_missing
    














################################################################################
def get_stations_by_distance(XYZ,
                             stations = None):
    """sort a dictionary of station positions by distance to some position

XYZ      I  [x,y,z] position of something, in m
stations I  dictionary of station_name : [x,y,z] pairs.  If None, uses
            the GPS_stations global

OUTPUTS: A
A        O  List of 3 element arrays [station_name, station_position, dist],
            where dist is the distance to the target point in m,
            station_name is the name of the station, and station_position
            is the [x,y,z] position of the station in m.  This list
            is sorted by distance, so element 0 is closest, and so on.

"""
    global GPS_stations
    if(stations == None): stations = GPS_stations
    A = []
    for s in stations:
        pos = stations[s]
        delta = [pos[0]-XYZ[0], pos[1]-XYZ[1], pos[2]-XYZ[2]]
        dist = math.sqrt(delta[0]*delta[0] +delta[1]*delta[1] +delta[2]*delta[2])
        A.append([s, pos, dist])
    A.sort()
    return A



################################################################################
def get_stations_within_distance(XYZ,
                                 max_dist,
                                 stations = None):
    """find a dictionary of stations within some distance to a position

XYZ      I  [x,y,z] position of something, in m
max_dist I  maximum allowable distance from the XYZ position, in m
stations I  dictionary of station_name : [x,y,z] pairs.  If None, uses
            the GPS_stations global

OUTPUTS: A
A        O  List of 3 element arrays [station_name, station_position, dist],
            where dist is the distance to the target point in m,
            station_name is the name of the station, and station_position
            is the [x,y,z] position of the station in m.  
"""
    global GPS_stations
    if(stations == None): stations = GPS_stations
    A = []
    for s in stations:
        pos = stations[s]
        delta = [pos[0]-XYZ[0], pos[1]-XYZ[1], pos[2]-XYZ[2]]
        dist = math.sqrt(delta[0]*delta[0] +delta[1]*delta[1] +delta[2]*delta[2])
        if(dist <= max_dist):
            A.append([s, pos, dist])
            print('get_stations_within_distance: potential station ',s.lower(),'at distance (km)', dist/1000.0)
    return A









################################################################################
def get_stations_within_distance_2(coord_list,
                                   max_dist,
                                   stations = None):
    """find a list of stations within some distance to many positions

Suppose that you have a list of telescope positions, and you want to find
all GPS receiver stations where the receivers are within max_dist to any
of the telescopes.  Then call this function with the telescope coordinates.

coord_list I  a list of [x,y,z] positions of some things, in m
max_dist   I  maximum allowable distance from the XYZ positions, in m
stations   I  dictionary of station_name : [x,y,z] pairs.  If None, uses
              the GPS_stations global

OUTPUTS: A
A        O  List of 3 element arrays [station_name, station_position, dist],
            where dist is the distance to the target point in m,
            station_name is the name of the station, and station_position
            is the [x,y,z] position of the station in m.  
"""
    global GPS_stations
    if(stations == None): stations = GPS_stations
    total_list = []
    for s in stations:
        pos = stations[s]
        for XYZ in coord_list:
            delta = [pos[0]-XYZ[0], pos[1]-XYZ[1], pos[2]-XYZ[2]]
            dist = math.sqrt(delta[0]*delta[0] +delta[1]*delta[1]
                             +delta[2]*delta[2])
            if(dist <= max_dist):
              print('potential GPS station',s,'at distance (km)', dist/1000.0)
              total_list.append([s, pos, dist])
              break
    return total_list



################################################################################
def add_global_stations_to_list(A,
                                stations = None):
    """add the list of global reference stations to the list of stations

Suppose that you have a list of telescope positions, and you want to find
all GPS receiver stations where the receivers are within max_dist to any
of the telescopes.  Then call this function with the telescope coordinates.

A        I  list of 3 element arrays [station_name, station_position, dist],
            where dist is the distance to the target point in m,
            station_name is the name of the station, and station_position
            is the [x,y,z] position of the station in m.
            This should have come from get_stations_within_distance or
            get_stations_within_distance_2
stations   I  dictionary of station_name : [x,y,z] pairs.  If None, uses
              the GPS_stations global

OUTPUTS: A
A        O  List of 3 element arrays [station_name, station_position, dist],
            where dist is the distance to the target point in m,
            station_name is the name of the station, and station_position
            is the [x,y,z] position of the station in m.  
"""
    global GPS_stations
    if(stations == None): stations = GPS_stations
    for name in GPS_global_station_list:
        for i in range(len(A)):
            if(A[i][0] == name):
                break
        else:
            # add this one in
            if(name in stations):
                A.append([name, stations[name], -1.0])
 
    # put global stations at the beginning - more chance of finding good
    # station data quickly when doing parallel data collection
    A.reverse()

    return A

################################################################################
def get_new_snx_information(name_list,
                            MJD_try,
                            output_directory = ".",
                            overwrite = 0,
                            stations = None):
    """find a dictionary of stations within some distance to many positions

Suppose that you have a list of telescope positions, and you want to find
all GPS receiver stations where the receivers are within max_dist to any
of the telescopes.  Then call this function with the telescope coordinates.

name_list  I  A list of two element arrays.  The first element is the 4 letter
              station ID name to try to add.  The second element is a text string
              specifying the station location
MJD_try    I  A MJD to try to download the data
output_directory  I  Where to put things
overwrite  I  May files be overwritten?  0 No, else yes
stations   B  dictionary of station_name : [x,y,z] pairs.  If None, uses
              the GPS_stations global

OUTPUTS: stations, text
stations   B  dictionary of station_name : [x,y,z] pairs.  If None, uses
              the GPS_stations global
text       O  a list of strings describing the stations in a pseudo snx format
"""
    global GPS_stations
    if(stations == None): stations = GPS_stations
    text = []
    for n in name_list:
        station_code = n[0].lower()
        if(station_code in stations):
            print("station '%s' already exists in dict"%(station_code))
            continue
        # try to get the station
        try:
            sta_MJD, Sat_array, obs_data, sta_XYZ, sta_bias_valid, sat_block_pos = \
                     Albus_RINEX_2.get_station_base_observations(MJD_try,
                                                                 MJD_try + 0.05,
                                                                 station_code,
                                                                 output_directory,
                                                                 overwrite
                                                                 )
            stations[station_code] = [sta_XYZ[0], sta_XYZ[1], sta_XYZ[2]]
            lon, lat, height = ellipsoidal_coord(sta_XYZ[0],
                                                 sta_XYZ[1],
                                                 sta_XYZ[2],
                                                 WGS84)
            if(lon < 0.0): lon += 2.0 * math.pi
            lad, lam, las = Albus_Coordinates.rad_to_dms(lat)
            lod, lom, los = Albus_Coordinates.rad_to_dms(lon)
            sign = '+'
            if(lat < 0.0): sign = '-'
            s = " %4.4s  A XXXXXMXXX P %22.22s %3d %2d %4.1f %c%2d %2d %4.1f %7.1f"%(station_code, n[1], lod, lom, los, sign, lad, lam, las, height)
            print(s)
            text.append(s)
        except Albus_RINEX.No_RINEX_File_Error as error:
            # ignore all missing stations
            print((str(error)))
        except Albus_RINEX.RINEX_Data_Barf as error:
            # ignore all stupid header problems
            print((str(error)))
    return stations, text







################################################################################
def print_receiver_positions(stations = None, fp = None):
    """find a dictionary of stations within some distance to a position


INPUTS:
stations I  dictionary of station_name : [x,y,z] pairs.  If None, uses
            the GPS_stations global
fp       I  File object to print to.  Must already be opened.  if None, will
            use sys.stdout

OUTPUTS:

"""
    global GPS_stations
    if(stations == None): stations = GPS_stations
    if(fp == None): fp = sys.stdout
    for s in stations:
        pos = stations[s]
        lon, lat, height = ellipsoidal_coord(pos[0],
                                             pos[1],
                                             pos[2],
                                             WGS84)
        if(lon < 0.0): lon += 2.0 * math.pi
        ll = "%12.6f %12.6f %12.3f\n"%(lon*M_RAD2DEG,lat*M_RAD2DEG,height)
        fp.write(ll)
    return


if __name__ == "__main__":
    pass
